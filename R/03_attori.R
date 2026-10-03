# Per ogni evento individua CHI reagisce (partito/gruppo, settore dell'aula, singolo deputato, governo)
# e A CHI (l'oratore dell'intervento in cui compare l'annotazione).
suppressMessages({library(data.table); library(stringi)})
setwd("C:/Users/David/Desktop/discorsi_camera")
seg  <- readRDS("applausi/dati/segmenti.rds")
ann  <- readRDS("applausi/dati/annotazioni_grezze.rds")
orat <- readRDS("applausi/dati/oratori.rds")

ev <- seg[!is.na(tipo) & !tipo %in% c("procedura", "campanello")]
ev <- merge(ev, ann[, .(ann_id, row_id, date, year, legislature, pos, speaker, party_name, chair, cabinet)], by = "ann_id")
setorder(ev, ann_id, seg_n)
ev[, ev_id := .I]
# le parole gridate/scandite/esposte (dopo i due punti) non descrivono chi reagisce
ev[, rest := stri_replace_first_regex(low, ":.*$", "")]
# suffisso "-l'Ulivo" dei gruppi della XIII-XIV ("Democratici di sinistra-l'Ulivo", "Margherita, DL-l'Ulivo"):
# va tolto, altrimenti Ulivo diventa un secondo attore. Senza trattino (XV, "gruppo dell'Ulivo") è il gruppo vero e resta
ev[, rest := stri_replace_all_regex(rest, "(\\bdl)?\\s*-\\s*l.ulivo\\b|-l.unione\\b|-progressisti\\b", "")]
ev[, rest := paste0(" ", rest, " ")]

# ---------------------------------------------------------------- 1. deputati nominati
nomi <- ev[, .(nm = unlist(stri_extract_all_regex(
  stri_trans_general(txt, "Latin-ASCII"),
  "(?<=\\b(deput?a?t[oaie]|onorevol[ei]|[Vv]ice ?[Mm]inistro|[Mm]inistro|[Ss]ottosegretario) )([A-Z][\\p{L}'’]+(,? | e | ed )?)+",
  omit_no_match = TRUE))), by = .(ev_id, legislature)]
nomi <- nomi[, .(nm = unlist(stri_split_regex(nm, ",\\s*| e | ed "))), by = .(ev_id, legislature)]
nomi[, nm := stri_trim_both(stri_trans_toupper(stri_replace_all_regex(nm, "[^\\p{L} ]", " ")))]
# 2 lettere ammesse per cognomi come Cè (CE)
nomi <- nomi[nchar(nm) >= 2 & !nm %in% c("PRESIDENTE", "IL", "LA", "LO", "DEL", "DELLA", "DI", "DE", "DA", "E")]

# anagrafica: partito prevalente di ogni oratore in ogni legislatura
o <- orat[chair == FALSE & !speaker %in% c("PRESIDENTE", "Government", "") & !is.na(speaker),
          .(n = sum(n)), by = .(legislature, speaker, party_name)]
o <- o[order(-n)][, .(party_name = party_name[1]), by = .(legislature, speaker)]
# candidati = oratori della stessa legislatura che contengono TUTTI i token del nome:
# join per token (legislatura, token) e conteggio dei token trovati, invece di un ciclo nome per nome
tk_o <- stri_split_regex(stri_trans_toupper(stri_trans_general(o$speaker, "Latin-ASCII")), "[^A-Z]+", omit_empty = TRUE)
ot <- unique(data.table(legislature = rep(o$legislature, lengths(tk_o)), speaker = rep(o$speaker, lengths(tk_o)),
                        party_name = rep(o$party_name, lengths(tk_o)), tk = unlist(tk_o)))
un <- unique(nomi[, .(nm, legislature)])[, nm_id := .I]
tk_n <- stri_split_regex(un$nm, "\\s+", omit_empty = TRUE)
nt <- unique(data.table(nm_id = rep(un$nm_id, lengths(tk_n)), legislature = rep(un$legislature, lengths(tk_n)), tk = unlist(tk_n)))
nt[, n_tok := .N, by = nm_id]
cand <- ot[nt, on = .(legislature, tk), nomatch = NULL, allow.cartesian = TRUE][
  , .(k = .N, n_tok = n_tok[1]), by = .(nm_id, speaker, party_name)][k == n_tok]
res <- cand[, .(dep_speaker = if (.N == 1) speaker else NA_character_,
                dep_party = if (uniqueN(party_name) == 1) party_name[1] else NA_character_,
                n_cand = .N), by = nm_id]
un <- merge(un, res, by = "nm_id", all.x = TRUE)[is.na(n_cand), n_cand := 0L][, nm_id := NULL]
nomi <- merge(nomi, un, by = c("nm", "legislature"))
cat("deputati nominati:", nrow(nomi), " risolti con partito:", nomi[!is.na(dep_party), .N],
    " non trovati:", nomi[n_cand == 0, .N], " ambigui:", nomi[n_cand > 1 & is.na(dep_party), .N], "\n")
# tolgo i nomi dal testo, così non disturbano il dizionario dei partiti
ev[ev_id %in% nomi$ev_id, rest := stri_replace_all_regex(rest, "(deput?a?t[oaie]|onorevol[ei]|vice ?ministro|ministro|sottosegretario) [a-z' ,]+?(?= (che|si|esce|escono|abbandon|espon|mostr|grid|e il|e i|al|alla|dai|dal)\\b|$)", "$1 ")]

# ---------------------------------------------------------------- 2. dizionario dei partiti
# l'ordine conta: i nomi più lunghi/specifici prima, ogni corrispondenza viene "consumata"
diz <- list(
  c("FDI",          "fratelli d.?italia(.alleanza nazionale)?"),
  c("LEGA",         "lega (forza )?nord( e autonomie)?( ?- ?lega dei popoli ?- ?noi con salvini)?( per l.indipendenza della padania| padania| federazione padana)?|lega.salvini ?premier|noi con salvini|\\blega\\b|\\blnp\\b|\\blnfp\\b|\\blnip\\b"),
  c("AN",           "alleanza nazionale(.msi)?"),
  c("DN",           "costituente di de\\S?\\S?ra.?democrazia nazionale|democrazia nazionale"),
  c("MSI",          "m\\S{0,2}s\\S{0,2}[il] ?.? ?de\\S{1,2}ra nazionale|movimento so\\S{1,3}ale( italiano)?(.destra nazionale)?|\\bms[il]\\b|missin"),
  c("MON",          "partito democratico italiano di unita monarchica|monarchic|momarchic|\\bpdium\\b|\\bpnm\\b|\\bpmp\\b"),
  c("UDC",          "unione dei democratici cristiani( e dei democratici di centro)?|unione di centro( per il terzo polo)?|\\budc\\b|centro cristiano democratico|cristiani democratici uniti|\\bccd\\b|\\bcdu\\b|ccd.cdu"),
  c("FLI",          "futuro e liberta( per (il terzo polo|l.italia))?"),
  c("FI",           "forza italia( ?- ?il popolo della liberta)?( ?- ?berlusconi presidente)?|(il )?popolo della liberta( ?- ?berlusconi presidente)?|\\bpdl\\b|berlusconi presidente"),
  c("SD",           "sinistra democratica ?- ?per il socialismo europeo"),
  c("PDS",          "partito democratico della sinistra|comunista.pds|\\bpds\\b"),
  c("DS",           "democratici di sinistra|sinistra democratica|\\bds\\b"),
  c("PD",           "partito democratico|\\bpd\\b"),
  c("PROGR",        "progressisti.?federativo|alleanza dei progressisti|progressisti"),
  c("SDI",          "socialisti democratici italiani|\\bsdi\\b"),
  c("PSDI",         "socialdemocratic|so\\S{1,3}alista democra\\S{1,2}ico|\\bpsdi\\b|\\bpsd\\b|socialisti democratici"),
  c("PSU",          "\\bpsu\\b|partito socialista unitario|socialisti unificati|psi.psdi"),
  c("PSIUP",        "psiup|socialista di unita proletari\\S"),
  c("PDUP",         "\\bpdup\\b|unita proletaria per il comunismo"),
  c("DP",           "democrazia proletaria|dp.comunisti|\\bdp\\b"),
  c("RC",           "rifondazione comunista(.sinistra europea)?|\\brc\\b"),
  c("PDCI",         "comunisti italiani|\\bpdci\\b"),
  c("COM",          "\\bpci\\b|comunist"),                       # PCI fino al 1991, poi vedi sotto
  c("SI",           "sinistra indipendente"),
  # LEU prima di SEL: "Liberi e Uguali-Articolo 1-Sinistra Italiana" è un solo gruppo
  c("LEU",          "(liberi e uguali|\\bleu\\b)(.articolo 1)?(.sinistra italiana)?|articolo 1(.movimento democratico e progressista)?(.liberi e uguali)?|movimento democratico e progressista"),
  c("SEL",          "sinistra ecologia liberta(.possibile)?|sinistra italiana|\\bsel\\b"),
  c("RNP",          "rosa nel pugno"),
  c("ALTRI",        "democrazia cristiana.partito socialista|federalisti e liberaldemocratici|liberaldemocratic|coraggio italia|alternativa c.e|misto.alternativa|\\balternativa\\b|popolo e territorio|iniziativa responsabile|democrazia solidale|centro democratico|per l.italia|alleanza per l.italia|movimento per l.autonomia|movimento per le autonomie|alleati per il sud|grande sud|minoranze linguistiche|\\bsvp\\b|union valdotaine|rinnovamento italiano|\\+europa|noi con l.italia|usei|\\bmaie\\b|conservatori e riformisti|civici e innovatori|scelta civica|nuovo centrodestra|area popolare|cambiamo|popolari.udeur|\\budeur\\b|\\budr\\b|\\bpatto segni\\b"),
  c("DC",           "democrati\\S{1,2}.?cristian|democrazia cristiana|\\bdc\\b|democristian"),
  c("PPI",          "partito popolare( italiano)?|\\bppi\\b|popolari e democratici|popolari per prodi"),
  c("MARGH",        "margherita|\\bdl\\b"),
  c("ULIVO",        "l.ulivo|\\bulivo\\b"),
  c("PSI",          "so\\S{1,3}alist|\\bpsi\\b|\\bps\\b"),
  c("RAD",          "radi ?cal|federalist\\S* europe|lista pannella"),
  c("VERDI",        "\\bverd[ei]\\b|sole che ride"),
  c("PLI",          "li ?berale|\\bpli\\b"),
  c("PRI",          "repubblican|\\bpri\\b"),
  c("RETE",         "movimento per la democrazia|la rete"),
  c("IDV",          "italia dei valori|\\bidv\\b"),
  c("M5S",          "movimento 5 stelle|movimento cinque stelle|movimento ?5 ?stelle|\\bm5s\\b"),
  c("IV",           "italia viva"),
  c("DEM",          "\\b(gruppo|deputati) (de)?i democratici\\b"),   # I Democratici (Prodi, XIII leg.): DS e PPI sono già consumati
  c("MISTO",        "\\bmisto\\b"),
  c("GOVERNO",      "\\bgoverno\\b|\\bministr[oi]\\b|sottosegretari|presidente del consiglio"),
  c("MAGGIORANZA",  "maggioranza"),
  c("OPPOSIZIONE",  "opposizion[ei]"),
  c("TUTTI",        "tutti i settori|ogni settore|tutta (la camera|l.assemblea)|l.intera (camera|assemblea)|tutti i gruppi|l.assemblea|la camera\\b")
)
hits <- list()
for (d in diz) {
  i <- which(stri_detect_regex(ev$rest, d[2]))
  if (length(i)) {
    hits[[d[1]]] <- data.table(ev_id = ev$ev_id[i], kind = "gruppo", actor = d[1])
    ev[i, rest := stri_replace_all_regex(rest, d[2], " # ")]
  }
}
grp <- rbindlist(hits)
# "comunista" generico: PCI fino alla X legislatura, Rifondazione nella XI-XII, Comunisti italiani dopo
grp <- merge(grp, ev[, .(ev_id, legislature)], by = "ev_id")
grp[actor == "COM", actor := fcase(legislature <= 10, "PCI", legislature <= 12, "RC", default = "PDCI")]
grp[actor %in% c("GOVERNO", "MAGGIORANZA", "OPPOSIZIONE", "TUTTI"), kind := stri_trans_tolower(actor)]
# un evento "misto-X" ha già X: tolgo MISTO se c'è anche un altro gruppo nello stesso evento
grp <- grp[!(actor == "MISTO" & ev_id %in% grp[kind == "gruppo" & actor != "MISTO", ev_id])]

# ---------------------------------------------------------------- 3. settori dell'aula
re_set <- "(?<=\\b(a|al|alla|all'|dal|dalla|dall'|della|dell'|e|ed|sulla|sui banchi della|nei banchi della|banchi della)\\s?(l')?\\s?)(estrema sinistra|estrema destra|sinistra|destra|centro)\\b|\\b(alcentro|adestra|asinistra|alestrema sinistra)\\b"
# errori OCR frequenti (esirema, stnistra, si nistra, ceniro, desitra...)
ev[, rest2 := stri_replace_all_regex(rest, "\\b(es\\S{0,2}ema|est\\S?rema)\\b", "estrema")]
ev[, rest2 := stri_replace_all_regex(rest2, "\\bs\\S{0,2}n\\S{0,2}stra\\b|\\bsi'? nistra\\b", "sinistra")]
ev[, rest2 := stri_replace_all_regex(rest2, "\\bcen(iro|fro|'ro|ipo|ito|lro|ro|tno)\\b", "centro")]
ev[, rest2 := stri_replace_all_regex(rest2, "\\bdesitra\\b", "destra")]
ev[, rest2 := stri_replace_all_regex(rest2, "'\\s*'", "'")]
ev[, rest2 := stri_replace_all_regex(rest2, "\\b(al|all|alla|a|del|della)\\s*l?'\\s*", "$1 l'")]
ev[, rest2 := stri_replace_all_regex(rest2, "\\ball ?'? ?estrema|\\balla estrema|\\bal l'estrema|\\bal'estrema|\\bale(?=strema)", "all'estrema")]
ev[, rest2 := stri_replace_all_regex(rest2, "\\bal'centro|\\bal l'centro|\\balcentro\\b", "al centro")]
set <- ev[, .(m = unlist(stri_extract_all_regex(rest2, re_set, omit_no_match = TRUE))), by = ev_id]
set[, actor := fcase(stri_detect_regex(m, "estrema sinistra"), "ESTREMA SINISTRA",
                     stri_detect_regex(m, "estrema destra"),   "ESTREMA DESTRA",
                     stri_detect_regex(m, "sinistra"),         "SINISTRA",
                     stri_detect_regex(m, "destra"),           "DESTRA",
                     stri_detect_regex(m, "centro"),           "CENTRO")]
set <- unique(set[, .(ev_id, kind = "settore", actor)])
# settore -> partito solo dove 04_valida_settori.R mostra una corrispondenza netta (>= ~75% degli applausi):
# estrema sinistra = PCI e centro = DC sempre, destra = MSI dalla IV legislatura.
# "sinistra", "estrema destra" e la destra delle leg. I-III restano settori senza partito.
set <- merge(set, ev[, .(ev_id, legislature)], by = "ev_id")
set[, actor_party := fcase(legislature > 10, NA_character_,   # validato solo per le leg. I-X
                           # leg. I: l'estrema sinistra è il Fronte (PCI e PSI insieme);
                           # leg. II-III: "sinistra" indica tutta l'ala sinistra, ~55% PCI e ~35% PSI
                           actor == "ESTREMA SINISTRA" & legislature == 1, "PCI+PSI",
                           actor == "SINISTRA" & legislature %in% 2:3, "PCI+PSI",
                           actor == "ESTREMA SINISTRA", "PCI",
                           actor == "CENTRO", "DC",
                           actor == "DESTRA" & legislature >= 4, "MSI",
                           default = NA_character_)]
set[, legislature := NULL]

# ---------------------------------------------------------------- 4. tabella finale (una riga per attore)
dep <- unique(nomi[, .(ev_id, kind = "deputato", actor = nm, actor_party = dep_party, dep_speaker)])
att <- rbindlist(list(grp[, .(ev_id, kind, actor, actor_party = ifelse(kind == "gruppo", actor, NA_character_))],
                      set[, .(ev_id, kind, actor, actor_party)],
                      dep[, .(ev_id, kind, actor, actor_party, dep_speaker)]), fill = TRUE)
# da dove viene il partito di chi reagisce: scritto nel resoconto, dedotto dal settore, o dall'anagrafica del deputato
att[, party_fonte := fifelse(is.na(actor_party), NA_character_,
                             fcase(kind == "gruppo", "esplicito", kind == "settore", "settore", kind == "deputato", "deputato"))]
# applausi "generali" / "dell'Assemblea" senza altri attori
ev[, generale := stri_detect_regex(low, "^(vivissimi |prolungati |vivi )*(applausi|ovazione) general|general\\p{L}*,? (\\p{L}+,? )*applaus|^applausi dell.assemblea")]
att <- rbind(att, ev[generale == TRUE & !ev_id %in% att$ev_id, .(ev_id, kind = "tutti", actor = "TUTTI", actor_party = NA_character_)], fill = TRUE)

out <- merge(ev[, .(ev_id, ann_id, row_id, date, year, legislature, pos, tipo, intensita,
                    target_speaker = speaker, target_party = party_name, target_chair = chair, target_cabinet = cabinet, txt)],
             att, by = "ev_id", all.x = TRUE)
out[is.na(kind), `:=`(kind = "nessuno", actor = NA_character_)]

# ---------------------------------------------------------------- 5. sigle come chiave unica dei partiti
# il partito di oratori e deputati nominati arriva col nome lungo del dataset: lo porto alle sigle del dizionario.
# I nomi estesi per i grafici sono in tabelle/partiti.csv
pd <- fread("applausi/tabelle/partiti_dataset.csv", encoding = "UTF-8")
sig <- fread("applausi/tabelle/partiti.csv", encoding = "UTF-8")
stopifnot(all(na.omit(unique(c(out$target_party, out[kind == "deputato", actor_party]))) %in% pd$party_name),
          all(pd$sigla %in% sig$sigla))
out[, target_sigla := pd$sigla[match(target_party, pd$party_name)]]
out[kind == "deputato", actor_party := pd$sigla[match(actor_party, pd$party_name)]]
manca <- setdiff(na.omit(unique(out$actor_party)), sig$sigla)
if (length(manca)) stop("sigle senza nome esteso in tabelle/partiti.csv: ", paste(manca, collapse = ", "))
saveRDS(out, "applausi/dati/eventi.rds")

# ---------------------------------------------------------------- diagnostica
options(width = 220)
cat("\neventi:", nrow(ev), " righe evento-attore:", nrow(out), "\n")
cat("\nquota di eventi con almeno un attore, per legislatura e tipo di attore:\n")
x <- unique(out[, .(ev_id, legislature, kind)])
print(dcast(x[, .N, by = .(legislature, kind)], legislature ~ kind, value.var = "N", fill = 0))
cat("\neventi senza attore, per tipo:\n"); print(out[kind == "nessuno", .N, by = tipo][order(-N)])
cat("\nesempi di applausi con testo lungo ma senza attore riconosciuto:\n")
set.seed(4); print(out[kind == "nessuno" & nchar(txt) > 25 & tipo %in% c("applausi","proteste","commenti")][sample(.N, 30), stri_sub(txt, 1, 150)])
cat("\nresiduo non consumato nei testi con 'grupp' (top 60):\n")
r <- ev[stri_detect_regex(rest, "grupp"), .N, by = .(r = stri_trim_both(stri_replace_all_regex(rest, "\\s+", " ")))][order(-N)][1:60]
cat(paste0(r$r, " [", r$N, "]"), sep = "\n")
