# PG-Power <img src="www/pg_power_logo_1.png" align="right" height="60"/>

> *design with confidence*

A Shiny web application for sample size calculation in single-arm medical device studies using performance goal (PG) methodology.

Developed by **Áine Glynn** and **Filip Kłosowski**, University of Galway, as part of a Master's thesis in Health Data Science, under the supervision of **Prof. John Newell**.

---

## Overview

Single-arm medical device studies are common when a concurrent control group is not feasible. In these studies, the device is evaluated against a pre-specified **performance goal (PG)** — a benchmark proportion derived from historical data, published literature, or prior device performance.

PG-Power calculates the minimum sample size required to demonstrate that a device meets its performance goal for a **binary endpoint**, using confidence interval (CI) simulation methods. It supports both efficacy endpoints (higher proportion is better) and safety endpoints (lower proportion is better).

---

## Features

- **Six CI methods** — Wald, Wilson Score, Agresti-Coull, Clopper-Pearson (Exact), Prop.test, Jeffreys
- **Binary search simulation** via `binom::binom.confint` to find the minimum n at the target power
- **Power vs n plot** — interactive sawtooth exact binomial power curve with required n highlighted
- **CI method comparison table** — required n and achieved power across all six methods at a glance
- **CI diagram** — visualises the confidence interval at the required n against the PG boundary
- **Sensitivity analysis** — how required n changes as the assumed device proportion varies
- **Precision toggle** — 2 d.p. slider or 3 d.p. numeric input for proportion entry
- **Dropout adjustment** — enrolment target inflated for expected dropout
- **Interpretation templates** — eight pre-written templates covering efficacy, safety, and regulatory contexts
- **Report export** — PDF (via `pagedown`) or Word (via `officer`) with configurable sections

---

## Statistical Method

PG-Power uses a **CI simulation binary search**:

1. For a candidate sample size *n*, simulate `nsim` binomial draws from the expected device proportion *p₁*
2. Construct a CI for each simulated count using the selected method
3. Estimate power as the proportion of simulations where the CI bound clears the performance goal *p₀*
4. Binary search over *n* to find the smallest value achieving the target power

**Decision rules:**

| Endpoint | Reject H₀ if |
|---|---|
| Higher is better (efficacy) | CI lower bound > PG |
| Lower is better (safety) | CI upper bound < PG |

The significance level α is one-sided; the CI is constructed at level 1 − 2α (e.g. α = 0.025 → 95% CI).

---

## CI Methods

| Method | Notes |
|---|---|
| Wald (Z-score) | Normal approximation. Fast but inaccurate at extreme proportions or small n |
| Wilson Score | Good coverage across all proportions. Recommended general-purpose method |
| Agresti-Coull | Small correction to Wilson; nearly identical in practice |
| Clopper-Pearson | Inverts the exact binomial test. Most conservative — gives the largest n. **Preferred for regulatory submissions** |
| Prop.test | Score / chi-squared interval; close to Wilson |
| Jeffreys | Equal-tailed Bayesian credible interval with a Jeffreys prior. Good frequentist coverage |

---

## Installation

### Prerequisites

- R ≥ 4.2
- The following R packages:

```r
install.packages(c(
  "shiny", "bslib", "ggplot2", "plotly", "DT",
  "binom", "TrialSize", "thematic", "shinybusy",
  "base64enc", "officer", "pagedown", "scales"
))
```

> **PDF export** requires `pagedown` and Google Chrome installed on the system.  
> **Word export** requires `officer` only.

### Running the app

```r
shiny::runApp()
```

Or clone the repository and open in RStudio, then click **Run App**.

---

## Usage

### Calculator tab

1. Set **endpoint direction** — efficacy (higher is better) or safety (lower is better)
2. Set **α**, **power**, and **CI method**
3. Enter the **performance goal** and **expected device proportion**
4. Required n is calculated automatically and displayed in the result box
5. The power vs n plot and CI comparison table update in real time

### Generate Report tab

1. Choose PDF or Word format
2. Customise the title, author, and date header
3. Select which sections to include (results table, interpretation, plots, CI comparison, definitions, calculation code)
4. Edit the interpretation text using built-in templates and insert live values using tag buttons (`{n}`, `{pg_pct}`, etc.)
5. Click Download

---

## Regulatory Context

Performance goal studies are recognised by:

- **FDA** — Non-Inferiority Clinical Trials guidance (2016) and Bayesian Statistics guidance (2010) describe PG studies as appropriate when a concurrent control is not feasible
- **ISO 14155:2020** — Requires a pre-specified primary endpoint, sample size justification, and a defined success criterion

> ⚠️ **Disclaimer:** PG-Power is an exploratory planning tool for educational and research purposes only. It does not constitute regulatory, statistical, or clinical advice. All sample size calculations must be reviewed and validated by a qualified statistician before use in any formal study protocol, regulatory submission, or clinical investigation plan.

---

## Repository Structure

```
MDX2526/
├── global.R        # Libraries, theme, shared functions, statistical methods
├── ui.R            # App layout, tabs, accordions, inputs
├── server.R        # Reactives, plots, CI comparison, report generation
├── www/
│   ├── app.js      # Accordion toggle, precision toggle, templates, reset logic
│   ├── custom.css  # Design system — typography, palette, components
│   └── pg_power_logo_1.png
└── README.md
```

---

## R Packages

`shiny` · `bslib` · `ggplot2` · `plotly` · `DT` · `binom` · `TrialSize` · `thematic` · `shinybusy` · `base64enc` · `officer` · `pagedown`

---

## Authors

**Áine Glynn** and **Filip Kłosowski**  
MSc Health Data Science, University of Galway  
Supervised by **Prof. John Newell**

Source code: [github.com/FilipMKgit/MDX2526](https://github.com/FilipMKgit/MDX2526)

---

*Claude (Anthropic) assisted with parts of the code development.*
