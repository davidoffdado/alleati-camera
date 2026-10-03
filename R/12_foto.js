// Foto dei protagonisti delle scene, da Wikimedia Commons, solo con licenza libera.
// Per ogni voce di it.wikipedia: immagine principale -> metadati di licenza su Commons -> miniatura 400px.
// Uso: node R/12_foto.js   (dalla cartella applausi)
const fs = require("fs");
const path = require("path");
const UA = { "User-Agent": "alea-applausi/1.0 (progetto di ricerca, uso non commerciale)" };
const OUT = "viz/foto";
fs.mkdirSync(OUT, { recursive: true });

const tutte = [
  "Giovanni Roberti", "Gian Carlo Matteotti", "Attilio Piccioni", "Randolfo Pacciardi", "Angelo Castelli",
  "Loris Fortuna", "Salvatore Frasca", "Sergio Castellaneta", "Umberto Bossi", "Silvio Berlusconi",
  "Gianfranco Fini", "Alessandro Cè", "Pier Ferdinando Casini", "Renato Brunetta", "Enrico Letta",
  "Filippo Scerra", "Matteo Salvini", "Giuseppe Conte", "Mario Draghi", "Marco Pannella"
];

// Commons ospita solo file riutilizzabili: si esclude solo ciò che risultasse NC/ND; "Attribution" e "European Parliament" = libere con attribuzione
const libera = l => !!l && !/\b(nc|nd)\b|non[- ]commercial|no[- ]deriv/i.test(l);

const pausa = ms => new Promise(r => setTimeout(r, ms));
// con un nuovo tentativo se le API rispondono 429 (troppe richieste)
const get = async (url, tentativi = 4) => {
  for (let i = 0; i < tentativi; i++) {
    const r = await fetch(url, { headers: UA });
    if (r.ok) return r;
    if (r.status !== 429) throw new Error(r.status + " " + url);
    await pausa(15000 * (i + 1));
  }
  throw new Error("429 ripetuto " + url);
};
const pulisci = s => (s || "").replace(/<[^>]+>/g, "").replace(/\s+/g, " ").trim();

// node R/12_foto.js "Umberto Bossi" "Matteo Salvini" ... scarica solo quelle persone
const persone = process.argv.length > 2 ? process.argv.slice(2) : tutte;
(async () => {
  const righe = [["persona", "descrizione", "file", "licenza", "autore", "url_licenza", "pagina_commons", "locale", "esito"]];
  for (const p of persone) {
    try {
      const w = await (await get(`https://it.wikipedia.org/w/api.php?action=query&format=json&prop=pageimages%7Cdescription&piprop=name&redirects=1&titles=${encodeURIComponent(p)}`)).json();
      const pg = Object.values(w.query.pages)[0];
      if (!pg.pageimage) { righe.push([p, pg.description || "", "", "", "", "", "", "", "nessuna immagine"]); continue; }
      const file = pg.pageimage, descr = pg.description || "";
      const c = await (await get(`https://commons.wikimedia.org/w/api.php?action=query&format=json&prop=imageinfo&iiprop=url%7Cextmetadata&iiurlwidth=400&titles=File:${encodeURIComponent(file)}`)).json();
      const ii = Object.values(c.query.pages)[0].imageinfo?.[0];
      if (!ii) { righe.push([p, descr, file, "", "", "", "", "", "non su Commons (immagine locale di Wikipedia: esclusa)"]); continue; }
      const m = ii.extmetadata || {};
      const lic = pulisci(m.LicenseShortName?.value), url = m.LicenseUrl?.value || "";
      // "Unknown author" sulle foto dell'archivio della Camera: la fonte è nel campo Credit
      const aut = [pulisci(m.Artist?.value), pulisci(m.Credit?.value)].filter(x => x && !/^unknown author/i.test(x)).join(" — ") || "autore sconosciuto";
      if (!libera(lic)) { righe.push([p, descr, file, lic, aut, url, ii.descriptionurl, "", "licenza non libera: esclusa"]); continue; }
      const ext = path.extname(new URL(ii.thumburl).pathname) || ".jpg";
      const loc = path.join(OUT, p.toLowerCase().replace(/[^a-zà-ù]+/g, "_") + ext);
      fs.writeFileSync(loc, Buffer.from(await (await get(ii.thumburl)).arrayBuffer()));
      righe.push([p, descr, file, lic, aut, url, ii.descriptionurl, loc.replace(/\\/g, "/"), "ok"]);
    } catch (e) { righe.push([p, "", "", "", "", "", "", "", "errore: " + e.message]); }
    await pausa(3000);   // gentilezza verso le API
  }
  const csv = righe.map(r => r.map(x => `"${String(x).replace(/"/g, '""')}"`).join(",")).join("\n");
  // aggiunge alle righe già presenti (le persone riscaricate sostituiscono le vecchie)
  const vecchie = fs.existsSync("tabelle/foto.csv") ? fs.readFileSync("tabelle/foto.csv", "utf8").split("\n").slice(1).filter(x => !persone.some(p => x.startsWith(`"${p}"`))) : [];
  const nuove = csv.split("\n");
  fs.writeFileSync("tabelle/foto.csv", [nuove[0], ...vecchie, ...nuove.slice(1)].join("\n"));
  for (const r of righe.slice(1)) console.log(r[0].padEnd(23), r[8].padEnd(14), (r[3] + " | " + r[4]).slice(0, 60).padEnd(60), "|", r[1].slice(0, 60));
})();
