# Adolescent Well-Being in Flux

This repository contains the analysis code and interactive web application accompanying the study by Bechtiger & Janousch (2025), examining trends in adolescent health-risk behaviours and mental distress across 45 countries using data from the Health Behaviour in School-Aged Children (HBSC) study (2002–2018).

## Scripts

### `Data_Processing.R`
Loads and merges five HBSC survey waves, constructs composite variables, runs regressions, and exports all processed datasets.

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
│   ├── hbsc_raw.csv           – item-level data for webapp
│   ├── hbsc_allrel.csv        – full processed dataset
│   ├── hbsc_allrelCH.csv      – Switzerland subset
│   └── hbsc_variables.csv     – streamlined webapp dataset
├── Regression analysis
│   ├── run_regressions()      – cross-sectional models per country/year
│   │   ├── Multinomial: profile ~ age + sex + FAS
│   │   ├── Linear: outcome ~ profile (± covariates, ± profile×sex)
│   │   └── Sex-stratified models
│   └── run_longitudinal_regressions()
│       └── Linear: outcome ~ profile × survey year + covariates
├── Sensitivity analysis
│   └── Year-level regressions using country-level profile assignments
└── Additional utilities
    ├── Mplus .out file processing
    ├── Enum summary batch processing for class selection
    ├── Profile distribution consistency check
    └── Multilevel analysis data preparation (HDI, Gini, GDP)
```

---

### `Data_Analysis.R`
Calculates Cronbach's alpha per country and survey year, accounting for items not available in specific years. Exports one Excel file per country with a two-level header (year → items/alpha).

```
Data_Analysis.R
├── Alpha calculation loop (per country × year × variable)
│   ├── Single-item variables  – marks as N/A; flags if item absent
│   └── Multi-item variables   – removes all-NA items; computes alpha
└── Excel export
    ├── Two-level header (year on top, Items / Alpha below)
    └── One file per country: hbsc_alpha_{country}.xlsx
```

---

### `Data_Visualization.R`
Produces all figures for the study and the web application.

```
Data_Visualization.R
├── Map preparation
│   ├── Remove overseas territories (France, Netherlands)
│   └── Add Crimea to Ukraine polygon
├── Response frequency histograms
├── Spider plots
├── LPA profile line plots
│   ├── Per country
│   ├── Faceted multi-country plots
│   └── Sensitivity analysis plots
├── Regression coefficient plots
└── Trend plots
    ├── By sex
    └── By age group
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
