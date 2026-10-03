# Matrice chi-reagisce-a-chi per FASE POLITICA (governi consecutivi della stessa legislatura con la stessa
# maggioranza), con il campo di ogni partito. Dati per la matrice a pallini dello scrollytelling.
suppressMessages({library(data.table); library(stringi); library(jsonlite)})
setwd("C:/Users/David/Desktop/discorsi_camera/applausi")
ev   <- readRDS("dati/eventi_def.rds")
gov  <- fread("tabelle/governi.csv", encoding = "UTF-8")
sig  <- fread("tabelle/partiti.csv", encoding = "UTF-8")
sosp <- readRDS("dati/righe_sospette.rds")

# stesse sigle accorpate di 06/08
alias <- data.table(legislature = c(10L, 12L, 13L), da = "PDS", a = c("PCI", "PROGR", "DS"))
for (i in seq_len(nrow(alias))) {
  ev[legislature == alias$legislature[i] & target_partito == alias$da[i], target_partito := alias$a[i]]
  ev[legislature == alias$legislature[i] & actor_party == alias$da[i], actor_party := alias$a[i]]
}
relazione <- c(applausi = "app", approvazioni = "app",
               proteste = "ost", commenti = "ost", rumori = "ost", interruzioni = "ost",
               grida = "ost", apostrofi = "ost", scontro = "ost", applausi_polemici = "ost", diniego = "ost")
esclusi <- c("PRESIDENZA", "NON ATTRIBUITO", "ALTRI", "MISTO", "GOVERNO", "POLO", "CDL", "CSX", "CDX", "UNIONE")

ev <- ev[!is.na(date) & !target_chair & !is.na(target_partito) & !target_partito %in% esclusi]
ev <- merge(ev, sosp[, .(row_id, sospetta)], by = "row_id", all.x = TRUE)[is.na(sospetta) | sospetta == FALSE]
ev[, d := as.IDate(date)]

# ---------------------------------------------------------------- fasi
gov[, inizio := as.IDate(inizio)][, fine := as.IDate(fine)]
ev <- gov[, .(governo, d = inizio, maggioranza, astensione)][ev, on = "d", roll = TRUE]
# una fase = legislatura + composizione della maggioranza (e degli astenuti), governi consecutivi
ev[, chiave := paste(legislature, maggioranza, astensione, sep = "|")]
fasi <- ev[, .(inizio = min(d), fine = max(d), governi = paste(unique(governo), collapse = ", ")), by = .(legislature, chiave, maggioranza, astensione)]
setorder(fasi, inizio)
fasi[, fase := .I]
ev <- merge(ev, fasi[, .(chiave, fase)], by = "chiave")

# ---------------------------------------------------------------- celle
den <- unique(ev[, .(fase, row_id, a_chi = target_partito)])[, .(interventi_b = .N), by = .(fase, a_chi)]
re <- ev[tipo %in% names(relazione) & !is.na(actor_party) & !actor_party %in% esclusi]
re[, rel := relazione[tipo]]
# leg. II-III: il settore "sinistra" (attore PCI+PSI) vale sia per la riga del PCI sia per quella del PSI
blocco <- re[actor_party == "PCI+PSI" & legislature %in% 2:3]
re <- rbind(re[!(actor_party == "PCI+PSI" & legislature %in% 2:3)],
            copy(blocco)[, actor_party := "PCI"], copy(blocco)[, actor_party := "PSI"])
celle <- unique(re[, .(fase, rel, chi = actor_party, a_chi = target_partito, row_id)])[, .(n = .N), by = .(fase, rel, chi, a_chi)]
celle <- dcast(celle, fase + chi + a_chi ~ rel, value.var = "n", fill = 0)
celle <- merge(celle, den, by = c("fase", "a_chi"))
celle[, `:=`(q_app = round(app / interventi_b, 4), q_ost = round(ost / interventi_b, 4))]

# partiti mostrati in una fase: almeno 40 interventi con reazioni come oratore e almeno 20 reazioni come attore
att <- re[, .(n_att = uniqueN(row_id)), by = .(fase, p = actor_party)]
ora <- den[, .(fase, p = a_chi, n_or = interventi_b)]
part <- merge(ora, att, by = c("fase", "p"), all = TRUE)[fcoalesce(n_or, 0L) >= 40 & fcoalesce(n_att, 0L) >= 20]
ordine <- c("DP", "PDUP", "RAD", "PSIUP", "RC", "PDCI", "PCI+PSI", "PCI", "SI", "PDS", "PROGR", "LEU", "SEL",
            "VERDI", "RETE", "DS", "ULIVO", "DEM", "MARGH", "PSI", "PSU", "PSDI", "SDI", "RNP", "SD", "PD", "IV", "IDV",
            "M5S", "PRI", "PPI", "DC", "UDC", "FLI", "PLI", "BN", "FI", "LEGA", "AN", "MON", "DN", "MSI", "FDI")
stopifnot(all(part$p %in% ordine))
part[, pos := match(p, ordine)]
celle <- celle[paste(fase, chi) %in% part[, paste(fase, p)] & paste(fase, a_chi) %in% part[, paste(fase, p)]]

campo <- function(s, magg, ast) fifelse(s %in% strsplit(magg, " ")[[1]], "maggioranza",
                                        fifelse(s %in% strsplit(ast, " ")[[1]], "astensione", "opposizione"))

anni <- function(a, b) if (year(a) == year(b)) as.character(year(a)) else paste0(year(a), "–", year(b))
romani <- as.character(as.roman(1:18))
out <- lapply(fasi$fase, function(f) {
  F <- fasi[fase == f]
  P <- part[fase == f][order(pos)]
  if (nrow(P) < 3) return(NULL)
  list(fase = f, legislatura = F$legislature, numero = romani[F$legislature], governi = F$governi,
       inizio = as.character(F$inizio), fine = as.character(F$fine), anni = anni(F$inizio, F$fine),
       partiti = lapply(seq_len(nrow(P)), function(i) list(sigla = P$p[i], nome = sig$nome[match(P$p[i], sig$sigla)],
                                                           campo = campo(P$p[i], F$maggioranza, F$astensione),
                                                           interventi = P$n_or[i])),
       celle = celle[fase == f, .(chi, a_chi, app, ost, q_app, q_ost, n = interventi_b)])
})
out <- Filter(Negate(is.null), out)
dir.create("viz", showWarnings = FALSE)
write_json(out, "viz/matrice.json", auto_unbox = TRUE, digits = NA, na = "null")
cat("fasi:", length(out), " dimensione:", file.size("viz/matrice.json"), "byte\n")
print(fasi[fase %in% sapply(out, `[[`, "fase"), .(fase, legislature, anni = paste(inizio, fine, sep = " → "), governi = stri_sub(governi, 1, 60), maggioranza)], nrows = 80)

# anteprima autonoma: JSON incorporato nel template
js  <- paste(readLines("viz/matrice.json", encoding = "UTF-8", warn = FALSE), collapse = "")
tpl <- readLines("viz/matrice_template.html", encoding = "UTF-8", warn = FALSE)
tpl <- sub("/*__DATI__*/null", js, tpl, fixed = TRUE)
con <- file("viz/matrice.html", encoding = "UTF-8"); writeLines(enc2utf8(tpl), con, useBytes = TRUE); close(con)
cat("scritto viz/matrice.html\n")
