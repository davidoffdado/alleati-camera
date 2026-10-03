# Episodi candidati per lo scrollytelling: le anomalie pure più intense, distribuite nel tempo,
# con il testo originale intorno all'annotazione per verificarle a mano.
suppressMessages({library(data.table); library(stringi)})
setwd("C:/Users/David/Desktop/discorsi_camera")
r  <- readRDS("applausi/dati/reazioni_campi.rds")
an <- readRDS("applausi/dati/annotazioni_grezze.rds")[, .(ann_id, row_id, pos, ann)]
ev <- readRDS("applausi/dati/eventi_def.rds")[, .(ev_id, ann_id)]

ep <- unique(r[!is.na(anomalia), .(ev_id, date, legislature, governo, anomalia, tipo, intensita,
                                    chi = actor_party, a_chi = target_partito, oratore = target_speaker, txt)])
ep <- ep[, .(chi = paste(sort(unique(chi)), collapse = "+")), by = setdiff(names(ep), "chi")]
ep <- unique(ep, by = "ev_id")
# punteggio: intensità, reazioni più "forti" per l'ostilità, gruppi nominati esplicitamente
ep[, forza := intensita + fcase(tipo %in% c("scontro", "apostrofi", "grida"), 2, tipo == "proteste", 1, default = 0) +
              0.5 * stri_detect_regex(txt, "(?i)gruppo|gruppi|deputat")]
ep[, oratore_noto := !oratore %in% c("Government", "PRESIDENTE")]
set.seed(3)
sel <- ep[oratore_noto == TRUE][order(-forza, runif(.N))][, head(.SD, 2), by = .(legislature, anomalia)]
# episodi già individuati nell'analisi
noti <- ep[(date == as.IDate("1994-12-21") & oratore == "UMBERTO BOSSI") |
             (date == as.IDate("1970-11-17") & a_chi == "DC" & anomalia == "ostilità tra alleati") |
             (date == as.IDate("1978-12-16") & chi == "PCI" & a_chi == "PRI")]
sel <- unique(rbind(sel, noti, fill = TRUE), by = "ev_id")
sel <- merge(sel, ev, by = "ev_id")
sel <- merge(sel, an, by = "ann_id")
cat("candidati:", nrow(sel), "\n")

# testo intorno all'annotazione, un file del corpus per volta
periodo <- function(y) fcase(y <= 1972, "1948-1972", y <= 1992, "1972-1992", y <= 2006, "1992-2006", default = "2006-2022")
sel[, file := periodo(year(date))]
# le righe a cavallo tra periodi possono stare nel file precedente: si cerca il row_id in ogni file
ctx <- list()
for (p in c("1948-1972", "1972-1992", "1992-2006", "2006-2022")) {
  e <- new.env(); nm <- load(sprintf("camera_%s.RData", p), envir = e)
  d <- as.data.table(e[[nm[1]]])[row_id %in% sel$row_id, .(row_id, text)]; rm(e); gc()
  if (!nrow(d)) next
  x <- merge(sel[, .(ev_id, row_id, pos, oratore, ann)], d, by = "row_id", allow.cartesian = TRUE)   # row_id non unico nel file
  x[, c("prima", "annot", "dopo") := {
    out <- lapply(seq_len(.N), function(i) {
      loc <- stri_locate_all_regex(text[i], "\\([^()]{2,400}\\)")[[1]]
      if (pos[i] > nrow(loc)) return(list(NA_character_, NA_character_, NA_character_))
      a <- loc[pos[i], ]
      list(stri_sub(text[i], max(1, a[1] - 450), a[1] - 1), stri_sub(text[i], a[1], a[2]), stri_sub(text[i], a[2] + 1, a[2] + 250))
    })
    list(sapply(out, `[[`, 1), sapply(out, `[[`, 2), sapply(out, `[[`, 3))
  }]
  # segnali di attribuzione sospetta
  cogn <- stri_trans_general(stri_extract_last_regex(x$oratore, "\\S+"), "Latin-ASCII")
  x[, terza_persona := stri_detect_regex(stri_trans_general(text, "Latin-ASCII"), paste0("(?i)\\b(onorevole|collega|il deputato) (\\p{L}+ )?", cogn, "\\b"))]
  x[, altro_nome_prima := stri_detect_regex(stri_trans_general(prima, "Latin-ASCII"), "(?<![A-Za-z])[A-Z]{4,}(\\s[A-Z]{3,})?(,[^.]{0,60})?\\.\\s")]
  x <- x[annot == ann]   # tra le righe con lo stesso row_id, quella che contiene davvero l'annotazione
  ctx[[p]] <- unique(x[, .(ev_id, prima, annot, dopo, terza_persona, altro_nome_prima)], by = "ev_id")
}
ctx <- rbindlist(ctx)
sel <- merge(sel, ctx, by = "ev_id", all.x = TRUE)
setorder(sel, date)
sel[, id := .I]
fwrite(sel[, .(id, date, legislature, governo, anomalia, tipo, chi, a_chi, oratore, txt, terza_persona, altro_nome_prima, prima, annot, dopo)],
       "applausi/dati/candidati.csv")

# versione leggibile
con <- file("applausi/dati/candidati.txt", "w", encoding = "UTF-8")
for (i in seq_len(nrow(sel))) with(sel[i], {
  writeLines(sprintf("\n#%d  %s  [%s]  %s\n%s -> %s   oratore: %s   governo: %s%s%s", id, date, anomalia, tipo, chi, a_chi, oratore, governo,
                     if (isTRUE(terza_persona)) "   !! oratore citato in terza persona" else "",
                     if (isTRUE(altro_nome_prima)) "   !! nome in maiuscolo prima dell'annotazione" else ""), con)
  writeLines(paste0("...", stri_replace_all_regex(prima, "\\s+", " "), " >>", annot, "<< ", stri_replace_all_regex(dopo, "\\s+", " "), "..."), con)
})
close(con)
cat("scritti applausi/dati/candidati.csv e candidati.txt\n")
print(sel[, .N, by = anomalia])
