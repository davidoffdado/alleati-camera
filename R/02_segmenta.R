# Divide ogni annotazione in eventi e classifica il tipo di reazione.
suppressMessages({library(data.table); library(stringi)})
setwd("C:/Users/David/Desktop/discorsi_camera")
ann <- readRDS("applausi/dati/annotazioni_grezze.rds")

norm <- function(x) {
  x <- stri_trans_general(x, "Latin-ASCII")
  x <- stri_replace_all_regex(x, "[«»<>“”\"]+", " ")
  # errori OCR ricorrenti
  x <- stri_replace_all_regex(x, "\\bap\\.?\\s?plaus", "applaus")
  x <- stri_replace_all_regex(x, "\\b(St|Sl|S1)\\s+(grida|ride)", "Si $2")
  x <- stri_replace_all_regex(x, "\\b(I|Il)[/!]\\s", "Il ")
  x <- stri_replace_all_regex(x, "\\bA/cuni", "Alcuni")
  stri_trim_both(stri_replace_all_regex(x, "\\s+", " "))
}

# 1. segmentazione: " - ", "--", "—" e ". Maiuscola" separano gli eventi
seg <- ann[, .(txt = norm(stri_sub(ann, 2, -2))), by = ann_id]
seg <- seg[, .(txt = unlist(stri_split_regex(txt, "\\s*[-–—]{2,}\\s*|\\s+[-–—]\\s*|\\s*[-–—]\\s+|\\.\\s+(?=[A-Z])|;\\s+"))), by = ann_id]
seg[, txt := stri_trim_both(stri_replace_all_regex(txt, "^[,.:;\\s]+|[,.:;\\s]+$", ""))]
seg <- seg[nchar(txt) > 1]
seg[, seg_n := seq_len(.N), by = ann_id]
seg[, low := stri_trans_tolower(txt)]

# 2. intensità: aggettivi in testa al segmento
re_int <- "^((molt[ie]|moltissim[ie]|numeros[ie]|vivi|vive|vivissim[ie]|vivac[ie]|vivacissim[ie]|prolungat[ie]|generali|ripetut[ie]|reiterat[ie]|nuov[ie]|insistent[ie]|calorosi|caloros[ie]|scroscianti|fragoros[ie]|lungh[ie]|lunghissim[ie]|forti|intens[ie]|grandi|unanimi|nutriti|alt[ie]|ironic[ie]|polemic[ie]|vivo|viva|nuovo|nuova|alcun[ie]|qualche|brev[ie]|scarsi|timidi|sommess[ie]|e|ed)[ ,]+)+"
seg[, int_txt := stri_extract_first_regex(low, re_int)]
seg[, core := stri_replace_first_regex(low, re_int, "")]
seg[, intensita := fcase(
  stri_detect_regex(int_txt, "vivissim|fragoros|scroscian|lunghissim|prolungat|ripetut|reiterat|calorosi|unanimi|generali"), 3L,
  stri_detect_regex(int_txt, "viv|vivac|insistent|forti|intens|nutriti|alt[ie]|lungh"), 2L,
  stri_detect_regex(int_txt, "alcun|qualche|brev|scarsi|timidi|sommess"), 0L,
  default = 1L)]

# 3. tipo di evento (l'ordine conta: prima i casi più specifici)
tipi <- list(
  # esiti di votazione e altre formule procedurali: non sono reazioni, servono solo a escluderle
  procedura      = "^e.? (approvat|respint|concess|appoggiat)|^non e.? (approvat|appoggiat)|^sono (approvat|respint)|^(la camera|l.assemblea) (approva|respinge|non approva)|^dopo prova|^segue (la|il|l)|^vedi\\b|^l.emendamento|^la proposta|^l.ordine del giorno|^il subemendamento|^gli emendamenti|^approvat|^respint|^cosi rimane|^i deputati segretari|^il deputato segretario|^presiedeva|^propost[ae] di (assegnazione|trasferimento)|^approvazioni in commission",
  applausi_polemici = "^a\\S{0,2}plaus\\p{L}* (polemic|ironic)|^(polemic|ironic)\\p{L}* applaus|applausi polemici|applausi ironici",
  applausi       = "^a\\S{0,3}l\\S{0,2}usi\\b|^ap plaus|^applau|^ovazion|^battimani",
  approvazioni   = "^approvazioni\\b|^consensi|^bene\\b|^benissimo|^bravo|^segni di (assenso|consentimento|generale|approvazione)|^cenni di assenso",
  diniego        = "^segni di (diniego|dissenso|disapprovazione)|^cenni di diniego|^disapprovazion|^dissensi",
  commenti       = "^c\\S?mment",
  proteste       = "^pr\\S{1,2}t?\\S{1,2}st[ae]\\b|^\\S{1,2}rotest",
  rumori         = "^rum\\S?ri",
  interruzioni   = "^\\S{0,4}err?u\\S?ion|^interromp",
  ilarita        = "^i\\S?larit|^iarit|^s\\S{0,2} ride|^risa\\b|^risate|^sorrisi|^si sorride",
  congratulazioni= "^con\\S{1,4}ula\\S{0,2}ion|^felicitaz|si congratul|^molti deputati si congratulano",
  grida          = "\\bsi grida|\\bgrid(a|ano)\\b|\\burla(no)?\\b|\\bsi urla|declama(no)?|scandisc|\\bin coro\\b|\\bcoro\\b|^fischi|\\bfischia|^voci\\b|^una voce|^voce\\b|^grida",
  apostrofi      = "apostrof|battibecc|invettiv",
  abbandono      = "abbandon\\p{L}* (l.)?aula|abbandon\\p{L}* l.emiciclo|\\besc[eo]n?o? dall.(aula|emiciclo)|\\blascia(no)? l.aula|si allontan|abbandonano i (loro )?banchi",
  scontro        = "tumult|^ag\\S{1,3}azion|^clamor|^confusion|colluttaz|vengono alle mani|venire alle mani|si avvicin|scend\\p{L}* nell.emiciclo|affollano l.emiciclo|\\baccorr|\\bcommessi (intervengono|ottemperano|si interpongono|accompagnano|allontanano)|assistenti parlamentari|si interpongono|\\bspint|^trambusto|^scompiglio|^baraonda|^parapiglia",
  cartelli       = "\\bespon(e|gono)\\b|\\bmostra(no)?\\b|cartell[oi]\\b|striscion|\\bdrapp[oi]\\b|magliett|bandier|\\bsventol",
  in_piedi       = "in piedi|si alzan|si leva(no)?\\b|osserva(no)? un minuto|minuto di silenzio|raccoglimento",
  richiamo       = "^richiam\\p{L}* del presidente|^il presidente richiama|^richiamo",
  campanello     = "campanell|scampanell"
)
seg[, tipo := NA_character_]
for (t in names(tipi)) seg[is.na(tipo) & stri_detect_regex(core, tipi[[t]]), tipo := t]

# 4. un segmento non classificato che segue un evento è quasi sempre il pezzo di un nome di gruppo
#    spezzato dal trattino ("Forza Italia - Il Popolo della Liberta - Berlusconi Presidente"): lo riattacco
setorder(seg, ann_id, seg_n)
seg[, ev := cumsum(!is.na(tipo)), by = ann_id]           # indice dell'evento a cui appartiene
seg[, attacca := is.na(tipo) & ev > 0 & !stri_detect_regex(low, "^(la camera|l.assemblea|vedi|segue|il presidente|presidenza)")]
seg <- seg[!is.na(tipo) | attacca | ev == 0]
seg[ev == 0, ev := -seq_len(.N), by = ann_id]           # segmenti isolati non classificati: restano singoli
seg <- seg[, .(txt = paste(txt, collapse = " - "), low = paste(low, collapse = " - "),
               tipo = tipo[1], intensita = intensita[1], seg_n = seg_n[1]), by = .(ann_id, ev)]
seg[, ev := NULL]

saveRDS(seg, "applausi/dati/segmenti.rds")

