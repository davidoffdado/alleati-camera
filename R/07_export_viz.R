# Esporta i dati per la visualizzazione dell'emiciclo: per ogni legislatura i blocchi di seggi
# (ordinati da sinistra a destra) e gli archi delle reazioni tra partiti diversi.
suppressMessages({library(data.table); library(jsonlite)})
setwd("C:/Users/David/Desktop/discorsi_camera/applausi")
m   <- readRDS("dati/matrici.rds")
ev  <- readRDS("dati/eventi_def.rds")
sig <- fread("tabelle/partiti.csv", encoding = "UTF-8")
sg  <- fread("tabelle/seggi.csv", encoding = "UTF-8")
dir.create("viz", showWarnings = FALSE)

# posizione in aula, da sinistra a destra (ordine indicativo, uguale per tutte le legislature);
# misto e "altri" al centro: in fondo a un lato suggerirebbero una collocazione politica
ordine <- c("DP", "PDUP", "RAD", "PSIUP", "RC", "PDCI", "PCI+PSI", "PCI", "SI", "PDS", "PROGR", "LEU", "SEL",
            "VERDI", "RETE", "DS", "ULIVO", "DEM", "MARGH", "PSI", "PSU", "PSDI", "SDI", "RNP", "PD", "IV", "IDV",
            "M5S", "ALTRI", "MISTO", "PRI", "PPI", "DC", "UDC", "FLI", "PLI", "BN", "FI", "LEGA", "AN", "MON", "DN", "MSI", "FDI")
stopifnot(all(sg$sigla %in% ordine))
grigi <- c("ALTRI", "MISTO")   # nessun arco: gruppi eterogenei

# ---------------------------------------------------------------- blocchi
b <- copy(sg)
# i partiti nati durante la legislatura tolgono i loro seggi al partito d'origine
sottr <- b[da != "", .(sottr = sum(seggi)), by = .(legislature, sigla = da)]
b <- merge(b, sottr, by = c("legislature", "sigla"), all.x = TRUE)[, seggi := seggi - fcoalesce(sottr, 0L)][, sottr := NULL]
b[, stima := fonte == "stima"]

# partiti presenti nelle matrici ma senza seggi in tabella (componenti del misto): stima = deputati
# distinti che hanno parlato con quella sigla, tolti dal misto
rilevanti <- unique(rbind(m[!chi %in% grigi, .(n = sum(interventi)), by = .(legislature, p = chi)],
                          m[, .(n = sum(interventi)), by = .(legislature, p = a_chi)])[n >= 50, .(legislature, p)])
virtuali <- data.table(legislature = 2:3, p = "PCI+PSI")   # settore "sinistra": abbraccia i blocchi PCI e PSI
mancanti <- rilevanti[!b, on = .(legislature, p = sigla)][!virtuali, on = .(legislature, p)][!p %in% c(grigi, "GOVERNO")]
dep <- unique(ev[target_chair == FALSE, .(legislature, sigla = target_partito, target_speaker)])[, .(stimati = .N), by = .(legislature, sigla)]
mancanti <- merge(mancanti, dep, by.x = c("legislature", "p"), by.y = c("legislature", "sigla"), all.x = TRUE)
cat("partiti senza seggi in tabella (stima dai deputati che parlano):\n"); print(mancanti)
mancanti <- mancanti[!is.na(stimati) & stimati >= 5]   # sotto i 5 sono codifiche spurie
b <- rbind(b, mancanti[, .(legislature, sigla = p, seggi = stimati, fonte = "stima dai dati", da = "MISTO",
                           certezza = "stima", nota = "deputati distinti che intervengono", stima = TRUE)])
tolti <- mancanti[, .(t = sum(stimati)), by = legislature]
b <- merge(b, tolti, by = "legislature", all.x = TRUE)[sigla == "MISTO", seggi := pmax(0L, seggi - fcoalesce(t, 0L))][, t := NULL]
b <- b[seggi > 0]
b[, pos := match(sigla, ordine)]
setorder(b, legislature, pos)
b[, nome := sig$nome[match(sigla, sig$sigla)]]
b[, grigio := sigla %in% grigi]

# autoapplauso e interventi con reazioni per blocco
self <- m[stesso == TRUE & chi == a_chi, .(legislature, relazione, sigla = chi, quota)]
self <- dcast(self, legislature + sigla ~ relazione, value.var = "quota")
b <- merge(b, self[, .(legislature, sigla, autoapplauso = approvazione)], by = c("legislature", "sigla"), all.x = TRUE)
ib <- unique(m[, .(legislature, sigla = a_chi, interventi = interventi_b)])
b <- merge(b, ib, by = c("legislature", "sigla"), all.x = TRUE)
setorder(b, legislature, pos)

# ---------------------------------------------------------------- archi (solo tra partiti diversi)
a <- m[stesso == FALSE & interventi >= 5 & !chi %in% grigi & !a_chi %in% grigi]
ok <- function(leg, s) paste(leg, s) %in% c(paste(b$legislature, b$sigla), paste(virtuali$legislature, virtuali$p))
a <- a[ok(legislature, chi) & ok(legislature, a_chi)]
a <- a[, .(legislature, relazione, da = chi, a = a_chi, quota = round(quota, 4), interventi, interventi_b,
           quota_da_settore)]

# ---------------------------------------------------------------- metadati delle legislature
anni <- ev[, .(inizio = min(year), fine = max(year)), by = legislature]
romani <- as.character(as.roman(1:18))
note <- function(l) c(
  if (l <= 9) "Fino al 1987 i resoconti indicano spesso chi reagisce con il settore dell'aula: l'attribuzione ai partiti è dedotta (estrema sinistra = PCI, centro = DC, destra = MSI dal 1963).",
  if (l %in% 2:3) "\"Sinistra\" indica insieme PCI e PSI: i loro archi partono da entrambi i blocchi.",
  if (l >= 11) "Dal 1992 i resoconti smettono di annotare rumori e interruzioni: l'ostilità registrata cala anche per questo.",
  if (any(b[legislature == l, stima])) "Alcuni blocchi hanno seggi stimati (partiti nati durante la legislatura o componenti del gruppo misto).")

out <- lapply(sort(unique(b$legislature)), function(l) list(
  legislatura = l, numero = romani[l], inizio = anni[legislature == l, inizio], fine = anni[legislature == l, fine],
  blocchi = b[legislature == l, .(sigla, nome, seggi, stima, grigio, autoapplauso = round(autoapplauso, 4), interventi)],
  archi = a[legislature == l, !"legislature"],
  virtuali = if (l %in% 2:3) list(list(sigla = "PCI+PSI", nome = "PCI e PSI (settore \"sinistra\")", blocchi = c("PCI", "PSI"))) else list(),
  note = note(l)))
write_json(out, "viz/emiciclo.json", auto_unbox = TRUE, pretty = FALSE, na = "null", digits = NA)
cat("\nscritto viz/emiciclo.json:", file.size("viz/emiciclo.json"), "byte\n")
# anteprima autonoma: il JSON incorporato nella pagina (fetch non funziona aprendo il file in locale)
js <- paste(readLines("viz/emiciclo.json", encoding = "UTF-8", warn = FALSE), collapse = "")
for (f in c("emiciclo", "statico")) {   # statico = immagine d'apertura (II legislatura, applausi della DC)
  tpl <- readLines(sprintf("viz/%s_template.html", f), encoding = "UTF-8", warn = FALSE)
  tpl <- sub("/*__DATI__*/null", js, tpl, fixed = TRUE)
  con <- file(sprintf("viz/%s.html", f), encoding = "UTF-8"); writeLines(enc2utf8(tpl), con, useBytes = TRUE); close(con)
}
cat("\nseggi totali per legislatura:\n"); print(dcast(b[, .(s = sum(seggi)), by = legislature], . ~ legislature, value.var = "s"))
cat("\narchi per legislatura e relazione:\n"); print(dcast(a[, .N, by = .(legislature, relazione)], relazione ~ legislature, value.var = "N"))
