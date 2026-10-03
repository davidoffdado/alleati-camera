# àlea #1 — I litigi tra gli alleati di governo

Reazioni d'aula (applausi, proteste, rumori, interruzioni…) nei resoconti stenografici della Camera dei deputati, 1948-2022 (ItaParlCorpus, in `../`).

- `R/` — pipeline numerata: `01_estrai.R` → … → `13_litigi.R` (più `12_foto.js` e `12b_loghi.js`, in Node, per foto e loghi da Wikimedia Commons)
- `tabelle/` — tabelle scritte a mano (sigle dei partiti, governi, seggi, scissioni, correzioni, crediti di foto e loghi)
- `dati/` — risultati della pipeline (gli `.rds` non sono nel repository: si rigenerano)
- `viz/` — visualizzazioni. La pagina dell'articolo è `viz/litigi.html`

## Modificare la pagina

Si modifica **`viz/litigi_template.html`**, mai `viz/litigi.html`: quella viene riscritta da zero, con dati, foto e loghi incorporati, ogni volta che si lancia

```
"C:/Program Files/R/R-4.3.0/bin/Rscript.exe" R/13_litigi.R
```

Nei testi delle scene `==parole==` diventa evidenziatore; nell'HTML si usa `<mark class="pennarello">…</mark>`.
