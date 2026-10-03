suppressMessages({library(data.table); library(stringi)})
options(width = 250)
ev <- readRDS("C:/Users/David/Desktop/discorsi_camera/applausi/dati/eventi.rds")
sig <- c("Democrazia Cristiana" = "DC", "Partito Comunista Italiano" = "PCI", "Fronte Democratico Popolare" = "FDP(PCI+PSI)",
         "Movimento Sociale Italiano" = "MSI", "Partito Socialista Italiano" = "PSI", "Radicali" = "RAD",
         "Partito Repubblicano Italiano" = "PRI", "Partito Liberale Italiano" = "PLI",
         "Partito Socialista Democratico Italiano" = "PSDI", "Unità Socialista" = "PSDI", "PSI-PSDI Unificati" = "PSU",
         "Partito Nazionale Monarchico" = "MON", "Partito Democratico Italiano di Unità Monarchica" = "MON",
         "Partito Monarchico Popolare" = "MON", "Blocco Nazionale" = "BN(PLI+UQ)", "Democrazia Proletaria" = "DP",
         "Verdi Arcobaleno" = "VERDI", "Partito Socialista di Unità Proletaria" = "PSIUP")
d <- ev[legislature <= 10 & target_chair == FALSE & !is.na(target_party)]
d[, p := fifelse(target_party %in% names(sig), sig[target_party], "altri")]
d[, periodo := fcase(legislature <= 3, "I-III (1948-63)", legislature <= 6, "IV-VI (1963-76)", default = "VII-X (1976-92)")]
ord <- c("ESTREMA SINISTRA", "SINISTRA", "CENTRO", "DESTRA", "ESTREMA DESTRA")

# lift = quota delle reazioni del settore rivolte al partito / quota di TUTTI gli eventi (con qualunque attore) rivolti al partito
# >1: il settore reagisce a quel partito piu di quanto il partito "parli"; per gli applausi ci si aspetta lift alto verso il proprio partito
tab <- function(tp) {
  base <- unique(d[tipo %in% tp, .(ev_id, periodo, p)])[, .(b = .N), by = .(periodo, p)][, b := as.numeric(b)][, b := b / sum(b), by = periodo]
  x <- unique(d[kind == "settore" & tipo %in% tp, .(ev_id, periodo, actor, p)])[, .N, by = .(periodo, actor, p)]
  x[, quota := N / sum(N), by = .(periodo, actor)]
  x <- merge(x, base, by = c("periodo", "p"))[, lift := quota / b]
  x[, actor := factor(actor, ord)]
  x
}
for (tp in list("applausi", c("proteste", "commenti", "interruzioni", "rumori"))) {
  x <- tab(tp)
  cat("\n=====", paste(tp, collapse = "+"), "=====\n")
  for (per in sort(unique(x$periodo))) {
    y <- x[periodo == per & N >= 20]
    keep <- y[, .(tot = sum(N)), by = p][tot >= 80, p]
    cat("\n--", per, "-- quota % (lift) delle reazioni del settore, per partito dell'oratore; n per settore:",
        paste(y[, .(n = sum(N)), by = actor][order(actor), paste0(actor, "=", n)], collapse = ", "), "\n")
    w <- dcast(y[p %in% keep], p ~ actor, value.var = "lift", fun.aggregate = function(v) round(v, 1), fill = NA); print(w[
      , c("p", intersect(ord, names(w))), with = FALSE])
  }
}

# partito "dominante" per settore: quello con la quota maggiore di applausi ricevuti, per legislatura
cat("\n===== partito che riceve piu applausi da ciascun settore, per legislatura (quota %) =====\n")
a <- unique(d[kind == "settore" & tipo == "applausi", .(ev_id, legislature, actor, p)])[, .N, by = .(legislature, actor, p)]
a[, q := round(100 * N / sum(N)), by = .(legislature, actor)]
a <- a[order(-N)][, .SD[1], by = .(legislature, actor)][, cell := paste0(p, " ", q, "%")]
a[, actor := factor(actor, ord)]
print(dcast(a, legislature ~ actor, value.var = "cell", fill = ""))
