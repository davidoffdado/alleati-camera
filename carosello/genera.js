// Caroselli Instagram (1080×1350) dei litigi tra alleati: un unico grafico panoramico che scorre
// di slide in slide (alla The Pudding), più la classifica dei governi in fondo. Due versioni:
//   b_pudding     caratteri e fumetti dell'articolo (Fraunces, IBM Plex)
//   b2_pudding    impaginazione della prima versione (Inter Tight, DM Mono), colori dell'articolo
//   b3_pudding    come b2, ma con una copertina in più: il titolo e le teste dei politici e dei partiti ammucchiate
//   b4_alea       come b2, nello stile comune di àlea (bianco + lime): quella pubblicata su Instagram dal 2026-10-11
// Si modifica <versione>_template.html, poi:  node carosello/genera.js [versione]
// Per ogni versione scrive <versione>.html (con i dati incorporati) e le slide in png/<versione>/,
// renderizzate con Edge headless (?s=N mostra la slide N).
const fs = require("fs"), path = require("path"), { execFileSync } = require("child_process");

const dir = __dirname;
const dati = JSON.parse(fs.readFileSync(path.join(dir, "../viz/litigi.json"), "utf8"));
// solo quello che serve alle slide
const DATI = JSON.stringify({ governi: dati.governi, litigi: dati.litigi.map(d => ({ data: d.data, governo: d.governo })) });
const EDGE = "C:/Program Files (x86)/Microsoft/Edge/Application/msedge.exe";

const versioni = fs.readdirSync(dir).filter(f => f.endsWith("_template.html")).map(f => f.replace("_template.html", ""))
  .filter(v => !process.argv[2] || v === process.argv[2]);

for (const v of versioni) {
  const pagina = fs.readFileSync(path.join(dir, `${v}_template.html`), "utf8").replace("/*__DATI__*/null", DATI);
  const html = path.join(dir, `${v}.html`);
  fs.writeFileSync(html, pagina);
  const out = path.join(dir, "png", v);
  fs.mkdirSync(out, { recursive: true });
  // numero di slide: <meta name="slide" content="N">, altrimenti le <section class="slide">
  const meta = pagina.match(/<meta name="slide" content="(\d+)">/);
  const N = meta ? +meta[1] : (pagina.match(/class="slide[^"]*" data-s=/g) || []).length;
  for (let s = 1; s <= N; s++) {
    const png = path.join(out, `litigi_${s}.png`);
    execFileSync(EDGE, ["--headless=new", "--disable-gpu", "--hide-scrollbars", "--force-device-scale-factor=1",
      "--window-size=1080,1350", "--virtual-time-budget=8000", "--allow-file-access-from-files",
      `--screenshot=${png}`, "file:///" + html.replace(/\\/g, "/") + "?s=" + s], { stdio: "ignore" });
    console.log(png);
  }
}
