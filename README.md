# Adolescent Well-Being in Flux

This repository contains the analysis code and interactive web application accompanying the study by Bechtiger & Janousch (2025), examining trends in adolescent health-risk behaviours and mental distress across 44 countries using data from the Health Behaviour in School-Aged Children (HBSC) study (2002–2018).

## Scripts

### `Data_Processing.R`
Loads and merges five HBSC survey waves, constructs composite variables, and exports all processed datasets. Also contains utilities for Mplus output processing, class-selection enumeration, and multilevel analysis data preparation.

```
Data_Processing.R
├── Data loading & merging
│   ├── Read survey waves (2001, 2006, 2010, 2014, 2018)
│   ├── Harmonise column names across waves
│   └── Sequential full join into single dataset
├── Variable construction
│   ├── Reverse-coding of symptom items
│   ├── Composite scores (somatic, internalising, diet, alcohol, smoking)
│   ├── Cross-wave harmonisation (rescaling old/new item formats)
│   └── Family Affluence Scale (FAS)
├── Data export
│   ├── hbsc_raw.csv        – item-level data for webapp and alpha calculations
│   └── hbsc_variables.csv  – streamlined webapp dataset
├── Mplus output utilities
│   ├── process_mplus_output()     – extract means & class proportions from .out files
│   ├── find_mplus_files()         – locate .out files by country & class solution
│   └── batch_process_mplus_files()– batch-convert .out files to CSV
├── Enum summary batch processing  – class-selection based on size thresholds
├── Profile distribution consistency check
└── Multilevel analysis data preparation
    ├── Per-survey-year basis (HDI, Gini, GDP merge)
    └── Overall basis (HDI, Gini, GDP merge)
```

---

### `Mplus_Loop.R`
Automates Latent Profile Analysis (LPA) across all countries and survey years via MplusAutomation. Requires `hbsc_variables.csv`; run `Data_Processing.R` first. The outputs feed into several downstream scripts:
- **c_prob CSVs** → `Regression_Analysis.R` (profile assignments for all regression models) and `Data_Processing.R` (multilevel data preparation)
- **`.out` files** → `Data_Processing.R` `batch_process_mplus_files()`, which extracts profile means/proportions used by `Plot_Functions.R`, `Data_Visualization.R`, and `app.R`

```
Mplus_Loop.R
├── Helper functions
│   ├── extract_classification_probabilities() – diagonal range of classification prob. matrix
│   └── extract_class_sizes_proportions()      – class n and % per model
└── Enumeration loop → LPA/
    ├── Per country (all waves pooled): k = 1–6 classes
    │   ├── Fits MLR mixture model; saves .inp, .out, .dat files
    │   ├── Extracts c_prob CSV per solution
    │   └── Writes enum summary Excel ({country}_enum_summary.xlsx)
    └── Per country × survey year: k = 1–6 classes
        ├── Same model; saves files in LPA/{country}/{year}/
        └── Writes enum summary Excel ({country}_{year}_enum_summary.xlsx)
```

---

### `Regression_Analysis.R`
Runs all cross-sectional, longitudinal, reference-category, and sensitivity regressions. Requires `hbsc_variables.csv` and `hbsc_labels.xlsx`; run `Data_Processing.R` first.

```
Regression_Analysis.R
├── Functions
│   ├── run_regressions()            – cross-sectional models per country/year
│   │   ├── Multinomial: profile ~ age + sex + FAS
│   │   ├── Linear: outcome ~ profile (± covariates, ± profile×sex)
│   │   └── Sex-stratified models (males / females)
│   ├── run_longitudinal_regressions()
│   │   └── Linear: outcome ~ profile × survey year + covariates
│   └── run_refcat_regressions()     – cycles all profiles as reference category
├── Main regression
│   └── All countries, year-specific profiles → data/Regression/
├── Sensitivity analysis
│   └── Country-level profiles applied per year → data/Sensitivity Analysis/
└── Reference-category regressions
    ├── Main → data/Regression (ref_profile)/
    └── Sensitivity → data/Sensitivity Analysis (ref_profile)/
```

---

### `Multilevel_Analysis.R`
Runs multilevel mixed-effects models (lme4/lmerTest) examining how health-behaviour profiles predict outcomes over time, with country-level moderators (Gini, HDI). Requires `hbsc_mlvl_data_per_sy.csv`.

```
Multilevel_Analysis.R
├── Data preparation
│   └── Centering wave, squaring wave, scaling outcomes & covariates
├── Step 0 – Null models (ICC decomposition)
│   └── Outcome ~ 1 + (1 | country) + (1 | country:wave)
├── Step 1 – Main effects
│   ├── Linear:    outcome ~ HB + covariates + c_wave
│   └── Quadratic: outcome ~ HB + covariates + c_wave + c_wave²
├── Step 2 – Profile × wave interaction
│   └── LRT comparing models with and without HB × c_wave
├── Step 3 – Random slope for HB across countries
└── Country-level moderators
    ├── Gini × HB interactions
    └── HDI × HB interactions
```

---

### `Cronbach_Alpha.R`
Calculates Cronbach's alpha per country and survey year for internalising symptoms, somatic symptoms, and life satisfaction, accounting for items not available in specific years. Exports one Excel file per country with a two-level header (year → items/alpha).

```
Cronbach_Alpha.R
├── Alpha calculation loop (per country × year × variable)
│   ├── Single-item variables  – marks as N/A
│   └── Multi-item variables   – removes all-NA items; computes alpha
└── Excel export
    ├── Two-level header (year on top, Items / Alpha below)
    └── One file per country: hbsc_alpha_{country}.xlsx
```

---

### `Plot_Functions.R`
Reusable plotting and data-processing functions used by both the standalone scripts and `app.R`. Run from the project root.

```
Plot_Functions.R
├── Spatial data preparation
│   ├── Belgium split into Flemish / French regions
│   ├── UK split into England / Scotland / Wales
│   ├── Remove overseas territories (France, Netherlands)
│   └── Add Crimea to Ukraine polygon
├── generate_map()     – interactive Leaflet choropleth map (z-score per variable/year)
├── Integrated label processing
│   └── Apply human-readable labels to all item-level variables
├── create_histogram() – response-frequency bar chart for a single variable
└── LPA panel data processing
    └── Load and shape LPA CSV outputs for profile visualisations
```

---

### `Data_Visualization.R`
Produces all exploratory and publication figures for the study.

```
Data_Visualization.R
├── Response distribution plots
│   └── Raindrop / boxplot of z-standardised variables by survey year (Switzerland)
├── Spider plots
│   └── Profile means per country across all waves
├── LPA profile line plots
│   └── Faceted multi-country plots (combined survey years)
├── LPA sensitivity analysis plots
│   └── Country-level profile assignments applied per year
└── Trend plots
    ├── By sex
    └── By age category
```

---

### `app.R`
Shiny web application for interactive exploration of study findings.

```
app.R
├── Data loading & preprocessing
│   ├── LPA panel data processing
│   └── Integrated label processing
├── UI
│   ├── Home       – project overview, study team, links
│   ├── Map        – interactive choropleth map
│   ├── Descriptive Statistics – response distributions by country/year
│   ├── LPA        – latent profile visualisations
│   ├── Regression – coefficient plots per country/outcome
│   └── About      – methodology and data description
└── Server
    ├── Descriptive statistics rendering
    ├── World map rendering
    ├── LPA analysis rendering
    └── Regression analysis rendering
```

---

## Data
The HBSC data used in this project are not publicly available and are not included in this repository. Access can be requested via [www.hbsc.org](http://www.hbsc.org).
