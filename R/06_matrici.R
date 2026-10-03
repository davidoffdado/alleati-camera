# Matrici chi-reagisce-a-chi per legislatura.
# Unità: l'intervento. Per ogni coppia (A = partito che reagisce, B = partito dell'oratore) conta in quanti
# interventi di B il partito A ha reagito almeno una volta, e lo rapporta agli interventi di B con almeno
# una reazione registrata (di qualunque tipo e attore): "A ha applaudito il x% degli interventi di B".
suppressMessages({library(data.table)})
setwd("C:/Users/David/Desktop/discorsi_camera/applausi")
ev  <- readRDS("dati/eventi_def.rds")
sig <- fread("tabelle/partiti.csv", encoding = "UTF-8")
# sigle accorpate al gruppo parlamentare in cui sedevano: i deputati PDS nella XII erano nei
# Progressisti-Federativo, nella XIII in Sinistra Democratica-L'Ulivo (DS)
alias <- data.table(legislature = c(10L, 12L, 13L), da = "PDS", a = c("PCI", "PROGR", "DS"))   # X: il PCI diventa PDS nel 1991
for (i in seq_len(nrow(alias))) {
  ev[legislature == alias$legislature[i] & target_partito == alias$da[i], target_partito := alias$a[i]]
  ev[legislature == alias$legislature[i] & actor_party == alias$da[i], actor_party := alias$a[i]]
}

relazione <- c(applausi = "approvazione", approvazioni = "approvazione",
               proteste = "ostilita", commenti = "ostilita", rumori = "ostilita", interruzioni = "ostilita",
               grida = "ostilita", apostrofi = "ostilita", scontro = "ostilita", applausi_polemici = "ostilita",
               diniego = "ostilita")
esclusi_target <- c("PRESIDENZA", "NON ATTRIBUITO", "ALTRI")
esclusi_attore <- c("GOVERNO", "ALTRI")

# denominatore: interventi di B con almeno una reazione registrata
den <- unique(ev[!target_chair & !is.na(target_partito) & !target_partito %in% esclusi_target,
                 .(legislature, row_id, target_partito)])[, .(interventi_b = .N), by = .(legislature, target_partito)]

r <- ev[tipo %in% names(relazione) & !is.na(actor_party) & !actor_party %in% esclusi_attore &
          !target_chair & !is.na(target_partito) & !target_partito %in% esclusi_target]
r[, relazione := relazione[tipo]]
# un intervento conta una volta per coppia e relazione, qualunque sia il numero di annotazioni;
# fonte = la più diretta disponibile (esplicito > deputato > settore)
r[, fonte_ord := match(party_fonte, c("esplicito", "deputato", "settore"))]
m <- r[, .(eventi = uniqueN(ev_id), fonte = c("esplicito", "deputato", "settore")[min(fonte_ord)]),
       by = .(legislature, relazione, chi = actor_party, a_chi = target_partito, row_id)]
m <- m[, .(interventi = .N, eventi = sum(eventi), quota_da_settore = round(mean(fonte == "settore"), 2)),
       by = .(legislature, relazione, chi, a_chi)]
m <- merge(m, den, by.x = c("legislature", "a_chi"), by.y = c("legislature", "target_partito"))
m[, quota := interventi / interventi_b]
# stesso partito; il blocco PCI+PSI (leg. I-III) conta come uguale ai suoi componenti
m[, stesso := chi == a_chi | (chi == "PCI+PSI" & a_chi %in% c("PCI", "PSI")) | (a_chi == "PCI+PSI" & chi %in% c("PCI", "PSI"))]
m[, chi_nome := sig$nome[match(chi, sig$sigla)]][, a_chi_nome := sig$nome[match(a_chi, sig$sigla)]]
setcolorder(m, c("legislature", "relazione", "chi", "a_chi", "interventi", "interventi_b", "quota", "eventi"))
setorder(m, legislature, relazione, -quota)
saveRDS(m, "dati/matrici.rds")
fwrite(m, "dati/matrici.csv")

# ---------------------------------------------------------------- diagnostica
options(width = 250)
cat("righe:", nrow(m), "\n\ninterventi con almeno una reazione, per legislatura:\n")
print(dcast(den[, .(n = sum(interventi_b)), by = legislature], . ~ legislature, value.var = "n"))

# partiti "principali" di una legislatura: almeno 300 interventi con reazioni
principali <- den[interventi_b >= 300, .(legislature, p = target_partito)]
mostra <- function(leg, rel) {
  p <- principali[legislature == leg, p]
  x <- m[legislature == leg & relazione == rel & chi %in% p & a_chi %in% p]
  w <- dcast(x, chi ~ a_chi, value.var = "quota", fun.aggregate = function(v) round(100 * v), fill = 0)
  cat("\n== leg", leg, "-", rel, ": % degli interventi di [colonna] con reazione di [riga] ==\n"); print(w)
}
for (leg in c(1, 5, 9, 13, 16, 18)) for (rel in c("approvazione", "ostilita")) mostra(leg, rel)

cat("\n== applausi tra partiti DIVERSI piu frequenti (quota), per legislatura ==\n")
x <- m[relazione == "approvazione" & !stesso & interventi_b >= 300 & interventi >= 20][order(-quota)]
print(x[, head(.SD, 3), by = legislature][, .(legislature, chi, a_chi, pct = round(100 * quota, 1), interventi)])
