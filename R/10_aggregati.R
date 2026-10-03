# Numeri aggregati sulle anomalie, per legislatura, escludendo le righe con attribuzione sospetta:
#  - la riga cita l'oratore registrato in terza persona ("onorevole Pastore" in un intervento di Pastore)
#  - la riga contiene il marcatore di un altro deputato ("GOMBI. ...", "SCELBA, Ministro ...")
# Serie: ostilità tra alleati, applausi all'avversario, applausi ironici/polemici.
suppressMessages({library(data.table); library(stringi)})
setwd("C:/Users/David/Desktop/discorsi_camera")
r    <- readRDS("applausi/dati/reazioni_campi.rds")
ev   <- readRDS("applausi/dati/eventi_def.rds")
orat <- readRDS("applausi/dati/oratori.rds")

norm <- function(x) stri_replace_all_regex(stri_trans_toupper(stri_trans_general(x, "Latin-ASCII")), "[^A-Z]", "")
o <- unique(orat[!is.na(speaker) & !speaker %in% c("PRESIDENTE", "Government", ""), .(legislature, speaker)])
o[, full := norm(speaker)][, cogn := norm(stri_extract_last_regex(speaker, "\\S+"))]
re_mark <- "(?<![A-Za-z])((?:[A-Z][A-Z'’]{2,}\\s?){1,4})(?:,\\s*(?:Ministro|Sottosegretario|Presidente|Relatore|Vicepresidente|Segretario)[^.]{0,80})?\\.\\s"

# ---------------------------------------------------------------- 1. righe sospette
righe <- unique(ev[!target_chair & !is.na(target_speaker) & !target_speaker %in% c("Government", "PRESIDENTE"),
                   .(legislature, row_id, target_speaker)])
sosp <- list()
for (p in c("1948-1972", "1972-1992", "1992-2006", "2006-2022")) {
  e <- new.env(); nm <- load(sprintf("camera_%s.RData", p), envir = e)
  d <- as.data.table(e[[nm[1]]])[row_id %in% righe$row_id, .(row_id, speaker, text)]; rm(e); gc()
  d <- merge(d, righe, by = "row_id")[speaker == target_speaker]
  d[, txt := stri_trans_general(text, "Latin-ASCII")][, text := NULL]
  d[, cogn := stri_trans_general(stri_extract_last_regex(target_speaker, "\\S+"), "Latin-ASCII")]
  d[, terza := stri_detect_regex(txt, paste0("(?i)\\b(onorevole|collega|il deputato) (\\p{L}+ )?", cogn, "\\b"))]
  d[, me := norm(target_speaker)]
  d[, marks := lapply(stri_extract_all_regex(txt, re_mark, omit_no_match = TRUE), function(m) unique(norm(stri_replace_all_regex(m, ",.*$", ""))))]
  oo <- split(o, by = "legislature")
  d[, altro := mapply(function(m, leg, me) {
    m <- m[m != me & !stri_detect_fixed(me, m) & m != "PRESIDENTE"]
    x <- oo[[as.character(leg)]]
    any(m %in% x$full | (nchar(m) >= 5 & m %in% x$cogn))
  }, marks, legislature, me)]
  sosp[[p]] <- unique(d[, .(row_id, sospetta = terza | altro, terza, altro)], by = "row_id")
  cat(p, ": righe", nrow(d), " sospette", round(100 * mean(sosp[[p]]$sospetta), 1), "%\n")
  rm(d); gc()
}
sosp <- rbindlist(sosp)
saveRDS(sosp, "applausi/dati/righe_sospette.rds")

# ---------------------------------------------------------------- 2. serie per legislatura
r <- merge(r, sosp[, .(row_id, sospetta)], by = "row_id", all.x = TRUE)
r[is.na(sospetta), sospetta := FALSE]
rp <- r[sospetta == FALSE]
u  <- unique(rp[, .(legislature, relazione, anomalia, chi = actor_party, a_chi = target_partito, row_id)])
tot <- unique(u[, .(legislature, relazione, chi, a_chi, row_id)])[, .(tot = .N), by = .(legislature, relazione)]
an  <- unique(u[!is.na(anomalia), .(legislature, anomalia, chi, a_chi, row_id)])[, .(n = .N), by = .(legislature, anomalia)]
an[, relazione := fifelse(anomalia == "ostilità tra alleati", "ostilita", "approvazione")]
an <- merge(an, tot, by = c("legislature", "relazione"))[, pct := round(100 * n / tot, 1)]

# applausi ironici/polemici: eventi annotati come tali, in % di tutti gli eventi di applauso (righe non sospette)
ev2 <- merge(ev, sosp[, .(row_id, sospetta)], by = "row_id", all.x = TRUE)[is.na(sospetta) | sospetta == FALSE]
ir <- unique(ev2[tipo %in% c("applausi", "applausi_polemici"), .(ev_id, legislature, tipo)])[
  , .(n = sum(tipo == "applausi_polemici"), tot = .N), by = legislature][, pct := round(100 * n / tot, 2)]
ir[, anomalia := "applausi ironici o polemici"]

serie <- rbind(an[, .(legislature, anomalia, n, tot, pct)], ir[, .(legislature, anomalia, n, tot, pct)])
anni <- ev[, .(inizio = min(year, na.rm = TRUE), fine = max(year, na.rm = TRUE)), by = legislature]   # 33 eventi senza data
serie <- merge(serie, anni, by = "legislature")[order(anomalia, legislature)]
fwrite(serie, "applausi/dati/anomalie_serie.csv")

options(width = 200)
cat("\n% per legislatura (righe non sospette):\n")
print(dcast(serie, legislature + inizio ~ anomalia, value.var = "pct", fill = 0))
cat("\nconteggi (interventi o eventi):\n")
print(dcast(serie, legislature ~ anomalia, value.var = "n", fill = 0))
