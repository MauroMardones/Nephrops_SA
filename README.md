# **Nephrops Stock Assessment and Exploratory Data Analysis FU 30 (ICES Division 9A)**

This repository contains the full analytical workflow, data structures, and documentation used for the exploratory data analysis (EDA) and stock assessment of *Nephrops norvegicus* in ICES Division 9A (FU 30).
The project integrates fishery-dependent and fishery-independent data sources, applies standardized data processing and modelling procedures, and provides fully reproducible scripts for visualization, indicator development, and assessment model exploration.

The repository is designed to support transparency, traceability, and reproducibility, in line with current practices within ICES working groups and benchmark processes.

---

## **Repository Structure**

```
.
├── bib/        # Bibliographic databases (.bib) and citation styles used in reports
├── code/       # R scripts for data processing, EDA, modelling, diagnostics, and figures
├── data/       # Raw and processed datasets (surveys, landings, effort, biological data)
├── docs/       # Reports, R Markdown / Word / PDF documents, and supporting material
├── figs/       # All generated figures (time series, diagnostics, maps, HCRs)
├── outputs/    # Model outputs, tables,  manage outputs forecasts, and scenario-based results
├── README.md   # Project overview and documentation
└── Nephrops_SA.Rproj
```

---

## **Objectives**

This repository supports the following main objectives:

### **1. Exploratory Data Analysis (EDA)**

* Examine long-term trends in fishery-dependent and fishery-independent indices.
* Explore spatial and temporal patterns in abundance, catch, and fishing effort.
* Identify inconsistencies, gaps, and data quality issues prior to modelling.


### **2. Stock Assessment and Model Exploration**

* Prepare and evaluate SPiCT-based assessment scenarios.
* Explore alternative data configurations and prior assumptions.
* Assess uncertainty, diagnostics, and management-related outputs (HCRs, forecasts).

---

## **Repository Contents**

### **`code/`**

R scripts covering:

* Data import, cleaning, and harmonisation (using **tidyverse**, **sf**, **geosphere**, etc.).
* Processing of survey time series (ARSA, ISUNEPCA UWTV).
* CPUE standardisation workflows.
* Index normalization and uncertainty propagation.
* SPiCT model fitting, scenario definition, and diagnostics.
* Generation of figures and tables for reports.

Key assessment documents are produced from R Markdown files (e.g. `SA_Nephrops_2025.Rmd`).

The `code/` directory contains all scripts used to prepare data, configure model settings, run SPiCT models, and generate diagnostics and outputs for the assessment of *Nephrops norvegicus* in FU 30. The scripts are designed to be executed sequentially, as each step builds on objects created in previous stages.

The recommended execution order is as follows:

1. **`1_data.R`**
   Reads, cleans, and formats all input data used in the assessment, including survey indices, catch data, and fishery-dependent information. All datasets are standardized and prepared for use in the SPiCT model framework.

2. **`2_prior.R`**
   Defines the prior configurations (RUNs) explored in the assessment. This script specifies alternative prior assumptions for biological parameters, initial conditions, and process error components.

3. **`3_model.R`**
   Runs the SPiCT model for all defined scenarios and prior configurations. Model fits are stored for subsequent diagnostics and analyses.

4. **`4_diags.R`**
   Performs convergence checks and core diagnostic analyses, including likelihood components and basic model performance indicators.

5. **`5_comparision.R`**
   Produces comparative summaries across scenarios and prior configurations, focusing on key population variables and reference points.

6. **`6_retro.R`**
   Conducts retrospective analyses for converged model runs and generates retrospective diagnostics and figures.

7. **`7_hindcast.R`**
   Evaluates predictive performance through hindcast analyses, including calculation of error metrics such as MASE.

8. **`8_manage.R`**
   Generates management-relevant outputs, including estimates of biological reference points and catch advice indicators.

---

If you want, I can also add:

* a **short “Quick start” block** (3 lines),
* or a **dependency diagram** (script → outputs), which is often appreciated in WG reports.


---

### **`data/`**

Includes:

* Raw survey indices (ARSA trawl surveys, ISUNEPCA UWTV).
* Fishery-dependent datasets (landings, effort, size structure).
* Intermediate and processed datasets generated programmatically.
* `inputdata_FU30_wkbmsyspict.csv`; primary input for SPiCT assessments callled in **`1_data.R`**.

(Raw data remain unmodified; all derived datasets are generated through scripted workflows.)

---

### **`cpue/`**

Contains:

* Input data for CPUE calculations.
* Standardisation scripts and model outputs.
* Intermediate and final CPUE indices used in assessment scenarios.

---

### **`figs/`**

Stores all figures generated by the analysis:

* Time-series trends of landings, indices, and effort.
* Standardised and normalised abundance indices.
* Diagnostic plots and uncertainty visualisations.
* Spatial maps and management-related plots (e.g. HCRs).

Figures are organised to facilitate direct inclusion in reports and publications.

---

### **`outputs/`**

Includes:

* Model outputs by scenario and run.
* Tables of parameter estimates, reference points, states, and forecasts.
* CSV files and intermediate results used for reporting and review.

---

### **`bib/`**

Bibliographic resources for:

* Scientific references cited in reports and supporting documents.
* Citation styles (CSL) for reproducible reporting in R Markdown.
* LaTeX style files used in PDF outputs.

---

## **How to Use This Repository**

1. Clone the repository:

   ```bash
   git clone https://github.com/MauroMardones/Nephrops_SA
   ```

2. Open the project (`Nephrops_SA.Rproj`) in RStudio.

3. Run scripts in `code/` following the order described within each script.

4. Outputs (figures, tables, model results) will be written automatically to `figs/` and `outputs/`.

---

## **Contact**

For questions, contributions, or collaboration:

**Mauricio Mardones**
Fisheries Researcher – Marine Population Dynamics
IEO (Spain) / Chile
Mail:  [mauricio.mardones@csic.ieo.es](mauricio.mardones@csic.ieo.es)
GitHub: [https://github.com/MauroMardones](https://github.com/MauroMardones)
