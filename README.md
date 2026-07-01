# MDX2526 — A Statistical Perspective of Non-Inferiority Trials

**MSc Health Data Science · University of Galway · June 2026**  
**Áine Glynn & Filip Kłosowski · Supervised by Prof. John Newell**

---

## What is this project about?

Medical device companies often need to show that their device works well enough — but running a full randomised controlled trial with a control group isn't always practical or ethical. Instead, they compare their device against a pre-specified benchmark called a **performance goal (PG)**: a number derived from historical studies that represents the minimum acceptable level of performance.

This thesis looks at that process critically. Using a real vascular stent — the **BioMimics 3D**, designed to treat peripheral artery disease in the leg — as a case study, we examined three questions that don't get enough attention:

1. Where does the performance goal actually come from, and how uncertain is it?
2. Which statistical method should you use to test whether the device meets it?
3. How many patients do you actually need?

The short answer to all three: **it depends more than people realise**, and the choices made can change the outcome of a trial independently of how well the device actually performs.

---

## The Five Aims

| # | Aim | What we did |
|---|---|---|
| 1 | Derive evidence-based performance goals | Meta-analysis of 3 historical stent studies (116 patients) |
| 2 | Compare confidence interval methods | Simulation study across 6 CI methods for binary proportions |
| 3 | Evaluate sample size sensitivity | Benchmarked required n across CI methods and software tools |
| 4 | Build PG-Power | Open-source R Shiny app for performance goal sample size calculation |
| 5 | Explore Bayesian historical borrowing | Formally incorporate prior evidence to reduce required sample size |

---

## Key Findings

### Performance goals carry more uncertainty than they appear to

We derived two performance goals from a meta-analysis of historical bare nitinol femoropopliteal stent studies:

- **Safety:** maximum acceptable 30-day composite adverse event proportion → **≤ 12%**
- **Efficacy:** minimum acceptable 12-month Rutherford classification success proportion → **≥ 88%**

The limited historical evidence (only 3 studies, 116 patients) produced wide confidence intervals around both estimates. Performance goals are often reported as fixed numbers, but the uncertainty behind them is real and should be acknowledged.

### The choice of confidence interval method matters

We compared six methods for constructing confidence intervals for a binary proportion:

| Method | Coverage | Interval width | Verdict |
|---|---|---|---|
| Wald | ❌ Below 95% nominal | Narrow | Not suitable for small proportions |
| Wilson Score | ✅ Close to nominal | Moderate | Good general-purpose choice |
| Jeffreys | ✅ Close to nominal | Moderate | Good frequentist coverage |
| Agresti-Coull | ✅ Conservative | Wider | Reasonable |
| Clopper-Pearson | ✅ Conservative | Widest | Preferred for regulatory submissions |
| Wilson + continuity correction | ✅ Most conservative | Widest | Largest sample sizes |

### Sample size varies meaningfully between methods

Under the BioMimics 3D safety endpoint (PG = 11%, assumed true proportion = 5%, power = 90%, α = 0.025):

- **Clopper-Pearson** (primary method): **209 evaluable patients** at 89.7% achieved power
- Range across methods meeting nominal coverage: **198 (Jeffreys) to 247 (Wilson + continuity correction)**

### Bayesian borrowing can dramatically reduce sample size

Using a Beta(2, 48) prior — encoding the historical evidence that the complication proportion is around 4% — reduced the required sample size from **209 to 79 patients** (a 62% reduction). This comes with important caveats around the assumption that historical and current patients are sufficiently similar.

---

## PG-Power — the Shiny App

One of the main outputs of this thesis is **PG-Power**, an R Shiny web application that makes the sample size calculation process transparent and accessible.

**What it does:**
- Calculates required sample size for single-arm performance goal studies
- Supports all six CI methods via simulation-based binary search
- Shows interactive power vs n curves, CI diagrams, and sensitivity plots
- Compares required n across all six methods side by side
- Exports PDF or Word reports with customisable interpretation text

**Run it locally:**
```r
install.packages(c(
  "shiny", "bslib", "ggplot2", "plotly", "DT",
  "binom", "TrialSize", "thematic", "shinybusy",
  "base64enc", "officer", "pagedown", "scales"
))

shiny::runApp("PG_Power")
```

---

## Repository Structure

```
MDX2526/
│
├── CI_Analysis/          # Simulation study — coverage probability and interval
│                         # width across 6 CI methods, grids of n and p
│
├── Meta_Analysis/        # Performance goal derivation — forest plots,
│                         # pooled proportions, heterogeneity statistics
│
├── PG_Power/             # PG-Power Shiny app
│   ├── global.R
│   ├── ui.R
│   ├── server.R
│   └── www/              # app.js, custom.css, logo
│
├── Shiny_App/            # Earlier CI explorer prototype
│
├── Jun_Write_Up/
│   └── Write_Up_Markdown_Script/   # R Markdown thesis (rticles arxiv format,
│                                   # XeLaTeX, Vancouver citations, MiKTeX)
│
├── May_Presentation/     # Quarto thesis presentation slides
│
├── Feb_Poster/           # February progress poster
│
├── MDX2526.Rproj
└── .gitignore
```

---

## Tech Stack

| Tool | Use |
|---|---|
| R / RStudio | All analysis and app development |
| `binom` | CI methods for binary proportions |
| `TrialSize` | Sample size utilities |
| `shiny` + `bslib` | PG-Power web app |
| `ggplot2` + `plotly` | Plots and visualisations |
| `officer` + `pagedown` | Word and PDF report export |
| R Markdown + `rticles` | Thesis write-up |
| XeLaTeX / MiKTeX | PDF compilation |

---

## Authors

**Áine Glynn** & **Filip Kłosowski**  
MSc Health Data Science, University of Galway  
Supervised by **Prof. John Newell**

---

> **Disclaimer:** This is an academic research project. Nothing here constitutes regulatory or clinical advice. All sample size calculations should be reviewed by a qualified statistician before use in any formal study or regulatory submission.

*Claude (Anthropic) assisted with parts of the code development.*
