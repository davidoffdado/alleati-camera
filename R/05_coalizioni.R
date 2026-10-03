# Ricodifica gli oratori che il dataset registra solo con la coalizione (Polo, Ulivo, Casa delle Libertà,
# Unione, Centrodestra, Centrosinistra) assegnando il gruppo che li applaude più spesso.
# La regola viene prima verificata sugli oratori con partito noto delle stesse legislature.
suppressMessages({library(data.table); library(stringi)})
setwd("C:/Users/David/Desktop/discorsi_camera/applausi")
ev  <- readRDS("dati/eventi.rds")
sig <- fread("tabelle/partiti.csv", encoding = "UTF-8")
coal <- sig[coalizione == TRUE, sigla]
# eccezioni: nella XV "L'Ulivo" era un gruppo parlamentare vero (DS + Margherita), non una coalizione;
# lato oratore, "Alleanza dei Progressisti" (PROGR, XII) è la coalizione elettorale del 1994 (PDS, RC, ...),
# mentre lato chi applaude PROGR è il gruppo Progressisti-Federativo e resta un partito
is_coal  <- function(s, leg) s %in% coal & !(s == "ULIVO" & leg == 15)
coal_tgt <- function(s, leg) is_coal(s, leg) | s %in% "PROGR"
MIN_EV <- 5       # applausi minimi (con gruppo esplicito) al primo gruppo
MARGINE <- 1.25   # il primo gruppo deve superare il secondo almeno del 25%

# applausi con il gruppo scritto nel resoconto, verso oratori che non sono la presidenza.
# Il conteggio è per oratore e legislatura su TUTTI i suoi interventi: lo stesso oratore può
# comparire con più sigle (es. CDX in alcune sedute, CSX in altre)
ap <- unique(ev[tipo == "applausi" & party_fonte == "esplicito" & !target_chair & !is.na(target_speaker) &
                  !actor_party %in% c("GOVERNO", "MISTO", "ALTRI") & !is_coal(actor_party, legislature),
                .(ev_id, legislature, target_speaker, actor_party)])
# solo applausi ESCLUSIVI (un solo gruppo): gli alleati applaudono spesso insieme al partito dell'oratore
# ("Applausi dei deputati dei gruppi FI, AN e Lega"), quasi mai da soli
ap[, n_gr := uniqueN(actor_party), by = ev_id]
cnt <- ap[n_gr == 1, .N, by = .(legislature, target_speaker, actor_party)][order(-N)]
top <- cnt[, .(top = actor_party[1], n1 = N[1], secondo = actor_party[2], n2 = fcoalesce(N[2], 0L)),
           by = .(legislature, target_speaker)]
top[, decidibile := n1 >= MIN_EV & n1 >= MARGINE * n2]
# sigla prevalente nel dataset per ogni oratore (per eventi)
sg <- unique(ev[!target_chair & !is.na(target_speaker), .(ev_id, legislature, target_speaker, target_sigla)])[
  , .N, by = .(legislature, target_speaker, target_sigla)][order(-N)]
top <- merge(top, sg[, .(target_sigla = target_sigla[1], n_sigle = .N), by = .(legislature, target_speaker)],
             by = c("legislature", "target_speaker"))

# ---------------------------------------------------------------- 1. verifica sugli oratori con partito noto
# stesse legislature delle coalizioni (XII-XVIII), solo oratori con una sola sigla;
# il PCI della XI-XV è in realtà PDS/RC: escluso dal test
noti <- top[legislature >= 12 & n_sigle == 1 & !coal_tgt(target_sigla, legislature) & !target_sigla %in% c("ALTRI", "PCI", "GOVERNO")]
cat("quota di oratori noti decidibili:", round(100 * noti[, mean(decidibile)], 1), "%\n")
cat("VERIFICA su", nrow(noti), "oratori con partito noto, leg. XII-XVIII\n")
cat("decidibili (>=", MIN_EV, "applausi esclusivi al primo gruppo e margine >= 25% sul secondo):", noti[decidibile == TRUE, .N], "\n")
cat("il primo gruppo coincide col partito:", round(100 * noti[decidibile == TRUE, mean(top == target_sigla)], 1), "%\n")
cat("\naccuratezza per legislatura:\n")
print(noti[decidibile == TRUE, .(n = .N, accuratezza = round(100 * mean(top == target_sigla), 1)), by = legislature][order(legislature)])
cat("\nerrori piu frequenti (partito vero -> assegnato):\n")
print(noti[decidibile == TRUE & top != target_sigla, .N, by = .(vero = target_sigla, assegnato = top)][order(-N)][1:15])

# ---------------------------------------------------------------- 2. ricodifica
# oratori con almeno un intervento registrato come coalizione
ids <- unique(ev[coal_tgt(target_sigla, legislature) & !target_chair, .(legislature, target_speaker, sigla_coal = target_sigla)])
ids <- ids[, .(sigla_coal = paste(sort(sigla_coal), collapse = "/")), by = .(legislature, target_speaker)]
rc <- merge(ids, top[, !"target_sigla"], by = c("legislature", "target_speaker"), all.x = TRUE)
rc[is.na(decidibile), decidibile := FALSE]
cat("\nRICODIFICA:", nrow(rc), "oratori con interventi registrati come coalizione;",
    rc[decidibile == TRUE, .N], "decidibili\n")
print(dcast(rc[decidibile == TRUE, .N, by = .(sigla_coal, top)], top ~ sigla_coal, value.var = "N", fill = 0))
fwrite(rc[order(legislature, sigla_coal, -n1)], "tabelle/oratori_coalizione.csv")   # da rivedere a mano se serve

# si ricodificano solo gli interventi registrati come coalizione
ev <- merge(ev, rc[decidibile == TRUE, .(legislature, target_speaker, nuovo = top)],
            by = c("legislature", "target_speaker"), all.x = TRUE)
ev[!coal_tgt(target_sigla, legislature), nuovo := NA_character_]
ev[, target_fonte := fcase(!is.na(nuovo), "applausi", coal_tgt(target_sigla, legislature), "coalizione", default = "dataset")]
ev[, target_partito := fcase(!is.na(nuovo), nuovo, coal_tgt(target_sigla, legislature), "NON ATTRIBUITO", default = target_sigla)]
ev[, nuovo := NULL]
# correzioni manuali (tabelle/correzioni_oratori.csv): valgono sugli interventi registrati come coalizione
# e prevalgono sull'assegnazione automatica; dal/al vuoti = tutta la legislatura
man <- fread("tabelle/correzioni_oratori.csv", encoding = "UTF-8", colClasses = list(character = c("dal", "al")))
stopifnot(all(man$partito %in% sig$sigla), all(man$certezza %in% c("sicuro", "probabile", "da verificare")))
# le righe "da verificare" non si usano: l'oratore resta non attribuito, anche se la regola automatica lo assegnava
man[certezza == "da verificare", partito := "NON ATTRIBUITO"]
man[, `:=`(dal = as.IDate(fifelse(dal == "", "1900-01-01", dal)), al = as.IDate(fifelse(al == "", "2100-01-01", al)),
           target_speaker = stri_trim_both(target_speaker))]
ev[, sp_trim := stri_trim_both(target_speaker)]
non_trovati <- man[!ev, on = .(legislature, target_speaker = sp_trim)]
if (nrow(non_trovati)) warning("correzioni senza oratore corrispondente: ", paste(non_trovati$target_speaker, collapse = ", "))
ev[, d := as.IDate(date)]
ev[man, on = .(legislature, sp_trim = target_speaker, d >= dal, d <= al),
   `:=`(target_partito = fifelse(coal_tgt(target_sigla, legislature), i.partito, target_partito),
        target_fonte = fifelse(coal_tgt(target_sigla, legislature), "manuale", target_fonte))]
ev[, c("sp_trim", "d") := NULL]
cat("\neventi corretti a mano:", unique(ev[target_fonte == "manuale", .(ev_id)])[, .N], "\n")

# i deputati nominati nelle annotazioni con partito = coalizione: stesso abbinamento, tramite dep_speaker
dm <- rc[decidibile == TRUE, .(legislature, dep_speaker = target_speaker, nuovo = top)]
ev <- merge(ev, dm, by = c("legislature", "dep_speaker"), all.x = TRUE)
ev[kind == "deputato" & is_coal(actor_party, legislature) & !is.na(nuovo), actor_party := nuovo]
ev[, nuovo := NULL]

# correzioni del partito valide per TUTTE le righe di un oratore (es. omonimie del dataset):
# tabelle/correzioni_partito.csv; "da verificare" -> NON ATTRIBUITO
cp <- fread("tabelle/correzioni_partito.csv", encoding = "UTF-8")
stopifnot(all(cp$partito %in% sig$sigla), all(cp$certezza %in% c("sicuro", "probabile", "da verificare")))
cp[certezza == "da verificare", partito := "NON ATTRIBUITO"]
cp[, target_speaker := stri_trim_both(target_speaker)]
ev[, sp_trim := stri_trim_both(target_speaker)]
ev[cp, on = .(legislature, sp_trim = target_speaker), `:=`(target_partito = i.partito, target_fonte = "manuale")]
ev[, dp_trim := stri_trim_both(dep_speaker)]
ev[cp[partito != "NON ATTRIBUITO"], on = .(legislature, dp_trim = target_speaker), actor_party := fifelse(kind == "deputato", i.partito, actor_party)]
ev[, c("sp_trim", "dp_trim") := NULL]
cat("\neventi con partito dell'oratore corretto (correzioni_partito):", unique(ev[cp, on = .(legislature, target_speaker), nomatch = NULL, .(ev_id)])[, .N], "\n")
setorder(ev, ev_id)

cat("\neventi verso oratori ancora con sigla di coalizione, per legislatura (prima -> dopo):\n")
print(unique(ev[, .(ev_id, legislature, target_sigla, target_partito, tc = coal_tgt(target_sigla, legislature))])[
  , .(prima = sum(tc), non_attribuiti = sum(target_partito == "NON ATTRIBUITO")), by = legislature][prima > 0][order(legislature)])
cat("\noratori con piu eventi rimasti non attribuiti:\n")
na_or <- unique(ev[target_partito == "NON ATTRIBUITO", .(ev_id, legislature, target_speaker)])[, .N, by = .(legislature, target_speaker)][order(-N)][1:15]
print(merge(na_or, rc[, .(legislature, target_speaker, sigla_coal, top, n1, secondo, n2)], by = c("legislature", "target_speaker"))[order(-N)])
saveRDS(ev, "dati/eventi_def.rds")
