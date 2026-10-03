# Dati della timeline dei litigi tra alleati: ogni reazione ostile tra partiti della stessa maggioranza
# (anomalia "pura", righe non sospette), con le bande dei governi.
suppressMessages({library(data.table); library(stringi); library(jsonlite)})
setwd("C:/Users/David/Desktop/discorsi_camera/applausi")
r    <- readRDS("dati/reazioni_campi.rds")
sosp <- readRDS("dati/righe_sospette.rds")
gov  <- fread("tabelle/governi.csv", encoding = "UTF-8")
sig  <- fread("tabelle/partiti.csv", encoding = "UTF-8")

l <- merge(r[anomalia == "ostilità tra alleati"], sosp[, .(row_id, sospetta)], by = "row_id", all.x = TRUE)
l <- l[is.na(sospetta) | sospetta == FALSE]
# un punto per evento; se reagiscono più alleati insieme, li elenco
l <- l[, .(chi = paste(sort(unique(actor_party)), collapse = "+"),
           fonte = fifelse(any(party_fonte == "esplicito"), "esplicito", fifelse(any(party_fonte == "deputato"), "deputato", "settore"))),
       by = .(ev_id, date, legislature, governo, tipo, intensita, a_chi = target_partito, oratore = target_speaker, txt)]
setorder(l, date)
cat("litigi tra alleati:", nrow(l), "\n"); print(l[, .N, by = fonte])
print(l[, .N, by = .(decennio = 10 * (year(date) %/% 10), fonte)][order(decennio)] |> dcast(decennio ~ fonte, value.var = "N", fill = 0))

# bande dei governi (solo il periodo coperto dal corpus)
g <- gov[, .(governo, inizio = pmax(as.IDate(inizio), as.IDate("1948-05-08")), fine = pmin(as.IDate(fine), as.IDate("2022-10-13")), maggioranza)]
g <- g[fine > inizio]

# emiciclo: seggi a inizio legislatura + scissioni con data (la pagina applica quelle avvenute prima della scena)
# ordine in aula da sinistra a destra: lo stesso di 07_export_viz.R
ordine <- c("DP", "PDUP", "RAD", "PSIUP", "RC", "PDCI", "PCI+PSI", "PCI", "SI", "AVS", "PDS", "PROGR", "LEU", "SEL",
            "VERDI", "RETE", "DS", "ULIVO", "DEM", "MARGH", "PSI", "PSU", "PSDI", "SDI", "RNP", "PD", "IV", "AZIV", "IDV",
            "M5S", "ALTRI", "MISTO", "PRI", "PPI", "DC", "UDC", "NM", "FLI", "PLI", "BN", "FI", "LEGA", "AN", "MON", "DN", "MSI", "FDI")
sg  <- fread("tabelle/seggi.csv", encoding = "UTF-8")[da == ""]   # i partiti nati dopo vengono da scissioni.csv
sci <- fread("tabelle/scissioni.csv", encoding = "UTF-8", colClasses = list(character = "residuo"))
stopifnot(all(c(sg$sigla, sci$sigla, sci$residuo[sci$residuo != ""]) %in% ordine))
# date ufficiali di inizio delle legislature (prima seduta)
legs <- data.table(legislature = 1:18, inizio = c("1948-05-08", "1953-06-25", "1958-06-12", "1963-05-16", "1968-06-05", "1972-05-25",
  "1976-07-05", "1979-06-20", "1983-07-12", "1987-07-02", "1992-04-23", "1994-04-15", "1996-05-09", "2001-05-30", "2006-04-28",
  "2008-04-29", "2013-03-15", "2018-03-23"))
stopifnot(all(r[, min(date), by = legislature][order(legislature)]$V1 >= as.IDate(legs$inizio)))
# la Camera di oggi (XIX legislatura, fuori dal corpus): seggi delle elezioni 2022
oggi <- fread("tabelle/seggi_oggi.csv", encoding = "UTF-8")
stopifnot(all(oggi$sigla %in% ordine), sum(oggi$seggi) == 400)
legs <- rbind(legs, data.table(legislature = 19L, inizio = "2022-10-13"))
sg <- rbind(sg[, .(legislature, sigla, seggi, certezza)], oggi[, .(legislature = 19L, sigla, seggi, certezza)])
emiciclo <- list(ordine = ordine, legislature = legs,
                 seggi = sg[, .(legislature, sigla, seggi, stima = certezza != "sicuro")],
                 scissioni = sci[, .(legislature, sigla, seggi, da, data = as.character(data), residuo)],
                 nomi = setNames(as.list(sig$nome), sig$sigla))

nome <- function(s) vapply(strsplit(s, "\\+"), function(x) paste(sig$nome[match(x, sig$sigla)], collapse = " e "), "")
out <- list(
  emiciclo = emiciclo,
  governi = g[, .(governo, inizio = as.character(inizio), fine = as.character(fine), maggioranza)],
  litigi = l[, .(id = ev_id, data = as.character(date), governo, tipo, intensita, chi, chi_nome = nome(chi), a_chi, a_chi_nome = nome(a_chi),
                 oratore = stri_trans_totitle(stri_trim_both(oratore)), fonte, annotazione = txt)])
write_json(out, "viz/litigi.json", auto_unbox = TRUE, digits = NA, na = "null")
cat("scritto viz/litigi.json:", file.size("viz/litigi.json"), "byte\n")

# foto (da 12_foto.js): persona -> file locale, licenza, autore, pagina Commons
ft <- fread("tabelle/foto.csv", encoding = "UTF-8")[esito == "ok"]
ft[, locale := sub("^viz/", "", locale)]   # percorso relativo alla pagina
# autore leggibile: le foto dell'archivio di Camera e Senato hanno come "autore" un URL
ft[, autore := fcase(stri_detect_regex(autore, "camera\\.it"), "Camera dei deputati",
                     stri_detect_regex(autore, "senato\\.it"), "Senato della Repubblica",
                     default = stri_trim_both(stri_replace_all_regex(autore, "\\s*—?\\s*https?://\\S+|File:\\S+|\\S+\\.jpg:?", "")))]
ft[autore == "" | is.na(autore), autore := "autore sconosciuto"]
foto <- setNames(lapply(seq_len(nrow(ft)), function(i) as.list(ft[i, .(persona, locale, licenza, autore, pagina_commons)])), ft$persona)

# anteprima autonoma: dati e foto incorporati nel template
tpl <- readLines("viz/litigi_template.html", encoding = "UTF-8", warn = FALSE)
tpl <- sub("/*__DATI__*/null", paste(readLines("viz/litigi.json", encoding = "UTF-8", warn = FALSE), collapse = ""), tpl, fixed = TRUE)
tpl <- sub("/*__FOTO__*/null", toJSON(foto, auto_unbox = TRUE), tpl, fixed = TRUE)
# loghi dei partiti (da 12b_loghi.js), per le scene in cui reagisce un gruppo
lg <- fread("tabelle/loghi.csv", encoding = "UTF-8")[esito == "ok"]
lg[, locale := sub("^viz/", "", locale)]
loghi <- setNames(lapply(seq_len(nrow(lg)), function(i) as.list(lg[i, .(locale, licenza, autore, pagina_commons)])), lg$logo)
tpl <- sub("/*__LOGHI__*/null", toJSON(loghi, auto_unbox = TRUE), tpl, fixed = TRUE)
con <- file("viz/litigi.html", encoding = "UTF-8"); writeLines(enc2utf8(tpl), con, useBytes = TRUE); close(con)
cat("scritto viz/litigi.html con", length(foto), "foto e", length(loghi), "loghi\n")
