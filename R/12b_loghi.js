// Loghi dei partiti per le scene in cui chi reagisce è un gruppo e non un singolo deputato.
// File scelti a mano su Wikimedia Commons (solo licenze libere / pubblico dominio), miniatura PNG 300px.
// Uso: node R/12b_loghi.js   (dalla cartella applausi) -> viz/loghi/*.png + tabelle/loghi.csv
const fs = require("fs");
const UA = { "User-Agent": "alea-applausi/1.0 (progetto di ricerca, uso non commerciale)" };
const OUT = "viz/loghi";
fs.mkdirSync(OUT, { recursive: true });

// chiave = sigla (con periodo se il logo cambia), file su Commons
const loghi = [
  ["DC_1943", "DC Party Logo (1943-1968).svg"],
  ["DC_1968", "DC Party Logo (1968-1992).svg"],
  ["FI_1994", "Forza Italia - logo (Italy, 1993-2009).svg"],
  ["CCD", "Logo CCD Cristiano Democratici.svg"],
  ["PD", "Partito Democratico Italy.svg"],
  ["LEGA_2018", "Simbolo di Lega per Salvini Premier.svg"]
];
// Alleanza nazionale: il logo è solo su it.wikipedia come file non libero, quindi resta il riquadro con la sigla

const libera = l => !!l && !/\b(nc|nd)\b|non[- ]commercial|no[- ]deriv/i.test(l);
const pausa = ms => new Promise(r => setTimeout(r, ms));
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

(async () => {
  const righe = [["logo", "file", "licenza", "autore", "pagina_commons", "locale", "esito"]];
  for (const [k, file] of loghi) {
    try {
      const c = await (await get(`https://commons.wikimedia.org/w/api.php?action=query&format=json&prop=imageinfo&iiprop=url%7Cextmetadata&iiurlwidth=300&titles=File:${encodeURIComponent(file)}`)).json();
      const ii = Object.values(c.query.pages)[0].imageinfo?.[0];
      if (!ii) { righe.push([k, file, "", "", "", "", "non trovato"]); continue; }
      const m = ii.extmetadata || {};
      const lic = pulisci(m.LicenseShortName?.value);
      const aut = pulisci(m.Artist?.value) || "autore sconosciuto";
      if (!libera(lic)) { righe.push([k, file, lic, aut, ii.descriptionurl, "", "licenza non libera: escluso"]); continue; }
      const loc = `${OUT}/${k.toLowerCase()}.png`;
      fs.writeFileSync(loc, Buffer.from(await (await get(ii.thumburl)).arrayBuffer()));
      righe.push([k, file, lic, aut, ii.descriptionurl, loc, "ok"]);
    } catch (e) { righe.push([k, file, "", "", "", "", "errore: " + e.message]); }
    await pausa(3000);
  }
  fs.writeFileSync("tabelle/loghi.csv", righe.map(r => r.map(x => `"${String(x).replace(/"/g, '""')}"`).join(",")).join("\n"));
  for (const r of righe.slice(1)) console.log(r[0].padEnd(10), r[6].padEnd(10), r[2], "|", r[3].slice(0, 50));
})();
