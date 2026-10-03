# Anomalie rispetto allo schema maggioranza/opposizione:
#  - applausi trasversali (opposizione -> maggioranza e viceversa)
#  - applausi tra opposizioni
#  - ostilità tra alleati (maggioranza -> maggioranza, partiti diversi)
# Il campo di ogni partito dipende dal governo in carica il giorno dell'evento (tabelle/governi.csv).
suppressMessages({library(data.table); library(stringi)})
setwd("C:/Users/David/Desktop/discorsi_camera/applausi")
ev  <- readRDS("dati/eventi_def.rds")
gov <- fread("tabelle/governi.csv", encoding = "UTF-8")
sig <- fread("tabelle/partiti.csv", encoding = "UTF-8")

# stesse sigle accorpate di 06_matrici.R
alias <- data.table(legislature = c(10L, 12L, 13L), da = "PDS", a = c("PCI", "PROGR", "DS"))
for (i in seq_len(nrow(alias))) {
  ev[legislature == alias$legislature[i] & target_partito == alias$da[i], target_partito := alias$a[i]]
  ev[legislature == alias$legislature[i] & actor_party == alias$da[i], actor_party := alias$a[i]]
}

relazione <- c(applausi = "approvazione", approvazioni = "approvazione",
               proteste = "ostilita", commenti = "ostilita", rumori = "ostilita", interruzioni = "ostilita",
               grida = "ostilita", apostrofi = "ostilita", scontro = "ostilita", applausi_polemici = "ostilita",
               diniego = "ostilita")
esclusi <- c("PRESIDENZA", "NON ATTRIBUITO", "ALTRI", "MISTO", "GOVERNO", "POLO", "CDL", "CSX", "CDX", "UNIONE")   # coalizioni come attore: generiche

r <- ev[tipo %in% names(relazione) & !is.na(actor_party) & !target_chair & !is.na(target_partito) &
          !actor_party %in% esclusi & !target_partito %in% esclusi & actor_party != target_partito &
          !(actor_party == "PCI+PSI" & target_partito %in% c("PCI", "PSI"))]
r[, relazione := relazione[tipo]]
r[, d := as.IDate(date)]
r <- r[!is.na(d)]   # 33 eventi senza data nel dataset

# governo in carica: join "rolling" sulla data d'inizio
gov[, inizio := as.IDate(inizio)][, fine := as.IDate(fine)]
r <- gov[, .(governo, d = inizio, fine, maggioranza, astensione)][r, on = "d", roll = TRUE]
stopifnot(!anyNA(r$governo))
in_campo <- function(s, lista) mapply(function(x, l) x %in% strsplit(l, " ")[[1]], s, lista)
campo <- function(s) fcase(in_campo(s, r$maggioranza), "maggioranza", in_campo(s, r$astensione), "astensione", default = "opposizione")
r[, campo_chi := campo(actor_party)][, campo_a_chi := campo(target_partito)]
r[, coppia := fcase(campo_chi == "maggioranza" & campo_a_chi == "maggioranza", "tra alleati",
                    campo_chi == "opposizione" & campo_a_chi == "opposizione", "tra opposizioni",
                    campo_chi == "astensione" | campo_a_chi == "astensione", "con astenuti",
                    default = "trasversale")]

# ---------------------------------------------------------------- 0. filtri per le anomalie "pure"
# a) una reazione durante un intervento non è sempre rivolta all'oratore: se reagisce anche il partito
#    dell'oratore (es. M5S e PD protestano durante un discorso di Conte) il bersaglio è un terzo -> esclusa
# b) applausi corali (generali, "tutti i settori", 4+ partiti) sono momenti istituzionali, non anomalie
# c) "tra opposizioni" è anomalo solo tra lati opposti (PCI e MSI sì, DS e Margherita no)
lato <- c("PCI+PSI" = "sinistra", PCI = "sinistra", PSI = "sinistra", PSIUP = "sinistra", PDUP = "sinistra",
          DP = "sinistra", RC = "sinistra", PDCI = "sinistra", SI = "sinistra", PDS = "sinistra", PROGR = "sinistra",
          DS = "sinistra", VERDI = "sinistra", RETE = "sinistra", SEL = "sinistra", LEU = "sinistra", ULIVO = "sinistra",
          MARGH = "sinistra", DEM = "sinistra", PD = "sinistra", IV = "sinistra", IDV = "sinistra", SDI = "sinistra",
          RNP = "sinistra", SD = "sinistra", PSU = "sinistra",
          PSDI = "centro", PRI = "centro", PLI = "centro", DC = "centro", PPI = "centro", UDC = "centro", FLI = "centro",
          MSI = "destra", MON = "destra", BN = "destra", DN = "destra", AN = "destra", FI = "destra", LEGA = "destra",
          FDI = "destra", M5S = "M5S", RAD = "radicali")
manca_lato <- setdiff(unique(c(r$actor_party, r$target_partito)), names(lato))
if (length(manca_lato)) stop("sigle senza lato: ", paste(manca_lato, collapse = ", "))
pe <- ev[!is.na(actor_party) & !actor_party %in% c("GOVERNO", "ALTRI"), .(partiti = list(unique(actor_party)), n_part = uniqueN(actor_party)), by = ev_id]
tutti <- ev[kind == "tutti" | stri_detect_regex(stri_trans_tolower(txt), "general|tutti i settori|tutti i gruppi|ogni settore|tutta la camera|l.intera"), unique(ev_id)]
r <- merge(r, pe, by = "ev_id", all.x = TRUE)
r[, proprio := mapply(function(p, s) p %in% s, target_partito, partiti)]
r[, corale := ev_id %in% tutti | n_part >= 4]
r[, lati_diversi := lato[actor_party] != lato[target_partito] & lato[actor_party] != "radicali" & lato[target_partito] != "radicali"]
r[, anomalia := fcase(
  relazione == "approvazione" & coppia == "trasversale" & !proprio & !corale, "applauso all'avversario",
  relazione == "approvazione" & coppia == "tra opposizioni" & lati_diversi & !proprio & !corale, "applauso tra opposizioni opposte",
  relazione == "ostilita" & coppia == "tra alleati" & !proprio & !corale, "ostilità tra alleati",
  default = NA_character_)]
cat("anomalie pure (eventi):\n"); print(dcast(unique(r[!is.na(anomalia), .(ev_id, legislature, anomalia)])[, .N, by = .(legislature, anomalia)], legislature ~ anomalia, value.var = "N", fill = 0))

# ---------------------------------------------------------------- 1. quanto pesano le anomalie, per legislatura
# unità: l'intervento (row_id) per coppia di partiti, come in 06
u <- unique(r[, .(legislature, governo, relazione, coppia, chi = actor_party, a_chi = target_partito, row_id)])
quote <- u[, .N, by = .(legislature, relazione, coppia)][, quota := N / sum(N), by = .(legislature, relazione)]
options(width = 220)
cat("APPLAUSI tra partiti diversi: % per tipo di coppia\n")
print(dcast(quote[relazione == "approvazione"], legislature ~ coppia, value.var = "quota", fun.aggregate = function(v) round(100 * v), fill = 0))
cat("\nOSTILITA tra partiti diversi: % per tipo di coppia\n")
print(dcast(quote[relazione == "ostilita"], legislature ~ coppia, value.var = "quota", fun.aggregate = function(v) round(100 * v), fill = 0))

# ---------------------------------------------------------------- 2. coppie anomale più frequenti (solo anomalie pure)
ua <- unique(r[!is.na(anomalia), .(legislature, anomalia, chi = actor_party, a_chi = target_partito, row_id)])
cp <- ua[, .(interventi = .N), by = .(legislature, anomalia, chi, a_chi)][interventi >= 5][order(anomalia, -interventi)]
# peso delle anomalie: interventi con anomalia / interventi con reazioni tra partiti diversi della stessa relazione
tot <- u[, .(tot = .N), by = .(legislature, relazione)]
pes <- ua[, .(n = .N), by = .(legislature, anomalia)]
pes[, relazione := fifelse(anomalia == "ostilità tra alleati", "ostilita", "approvazione")]
pes <- merge(pes, tot, by = c("legislature", "relazione"))[, pct := round(100 * n / tot, 1)]
cat("\nPESO DELLE ANOMALIE PURE (% delle reazioni tra partiti diversi):\n")
print(dcast(pes, legislature ~ anomalia, value.var = "pct", fill = 0))
cat("\nCOPPIE ANOMALE (almeno 5 interventi):\n")
for (k in unique(cp$anomalia)) {
  cat("\n--", k, "--\n"); print(cp[anomalia == k][1:min(.N, 20), .(legislature, chi, a_chi, interventi)])
}

# ---------------------------------------------------------------- 3. episodi (anomalie pure)
ep <- unique(r[!is.na(anomalia), .(ev_id, date, legislature, governo, anomalia, tipo, intensita,
                                    chi = actor_party, a_chi = target_partito, oratore = target_speaker, txt)])
ep <- ep[, .(chi = paste(sort(unique(chi)), collapse = "+")), by = setdiff(names(ep), "chi")]
fwrite(cp, "dati/anomalie_coppie.csv")
fwrite(ep[order(date)], "dati/anomalie_episodi.csv")
saveRDS(r, "dati/reazioni_campi.rds")
cat("\nepisodi salvati:", nrow(ep), "\n")
set.seed(1)
for (k in unique(ep$anomalia)) {
  cat("\nesempi -", k, "(i più intensi prima):\n")
  x <- ep[anomalia == k][order(-intensita)][1:min(.N, 60)]
  print(x[sample(.N, min(.N, 18)), .(date, governo, chi, a_chi, oratore, txt = stri_sub(txt, 1, 85))])
}
