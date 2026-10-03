# Estrae dai quattro file ItaParlCorpus:
#  - tutte le annotazioni tra parentesi che iniziano con maiuscola e non contengono cifre
#  - l'anagrafica degli oratori per legislatura (serve a risolvere i nomi nelle annotazioni)
suppressMessages({library(data.table); library(stringi)})
setwd("C:/Users/David/Desktop/discorsi_camera")
periodi <- c("1948-1972", "1972-1992", "1992-2006", "2006-2022")

ann <- list(); orat <- list()
for (p in periodi) {
  e <- new.env(); nm <- load(sprintf("camera_%s.RData", p), envir = e)
  d <- as.data.table(e[[nm[1]]]); rm(e); gc()

  par <- stri_extract_all_regex(d$text, "\\([^()]{2,400}\\)", omit_no_match = TRUE)
  idx <- rep(seq_len(nrow(d)), lengths(par))
  a <- d[idx, .(row_id, date, year = year(date), legislature, speaker, pageid_wiki,
                party_name, party_family, chair, cabinet)]
  a[, pos := unlist(lapply(lengths(par), seq_len))]   # posizione dell'annotazione nell'intervento
  a[, ann := unlist(par)]
  # le cifre di solito indicano rinvii ("vedi pag. 12") e vanno escluse, ma non quelle nei nomi dei gruppi
  cifre <- stri_detect_regex(stri_replace_all_regex(a$ann, "(?i)movimento ?5 ?stelle|articolo 1\\b|10 volte meglio", ""), "\\d")
  ann[[p]] <- a[stri_detect_regex(ann, "^\\(\\s*[A-Z]") & !cifre]
  reaz <- a[cifre & stri_detect_regex(ann, "^\\(\\s*(Applaus|Comment|Protest|Rumor|Interruz|Si ride|Ilarit|Congratul)")]
  cat(p, "- annotazioni di reazione scartate per le cifre:", nrow(reaz), "\n")
  if (nrow(reaz)) print(head(reaz[, .N, by = ann][order(-N)], 8))

  orat[[p]] <- d[, .(n = .N), by = .(legislature, speaker, pageid_wiki, party_name, party_family, chair, cabinet)]
  cat(p, nrow(ann[[p]]), "annotazioni\n")
  rm(d, par, a); gc()
}
ann <- rbindlist(ann)
ann[, ann_id := .I]
saveRDS(ann, "applausi/dati/annotazioni_grezze.rds")
saveRDS(rbindlist(orat), "applausi/dati/oratori.rds")
