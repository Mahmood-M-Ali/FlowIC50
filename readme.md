# Flow Cytometry IC50 Analyzer

[![R](https://img.shields.io/badge/R-%3E%3D4.0-blue)](https://www.r-project.org/)
[![License: CC BY-NC 4.0](https://img.shields.io/badge/License-CC%20BY--NC%204.0-lightgrey.svg)](http://creativecommons.org/licenses/by-nc/4.0/)
[![Shiny](https://img.shields.io/badge/Shiny-App-blue)](https://shiny.rstudio.com/)
[![GitHub Repo](https://img.shields.io/badge/GitHub-FlowIC50-black?logo=github)](https://github.com/Mahmood-M-Ali/FlowIC50)
[![Hugging Face](https://img.shields.io/badge/Spaces-FACS--Analysis-yellow?logo=huggingface)](https://huggingface.co/spaces/mahmood-iab/FACS-Analysis)

Automated **Shiny application** for flow cytometry-based IC50 determination using Annexin V/PI apoptosis assays with fluorescence compensation.

---

## Quick Start

**No installation required.** Access the web application directly at:  
 [https://huggingface.co/spaces/mahmood-iab/FACS-Analysis](https://huggingface.co/spaces/mahmood-iab/FACS-Analysis)

Upload FCS files and analyze immediately in your browser.

---

## Features

- Automated batch processing of FCS files  
- Fluorescence spillover compensation from single-stained controls  
- Interactive hierarchical gating (FSC/SSC, singlets, viability)  
- Robust IC50 curve fitting with multiple algorithms  
- Publication-ready plots (300 DPI) and CSV exports  
- Complete results package download (ZIP)  

---

## File Naming Convention

File names must follow this format:  
`CellLine_Treatment_ConcentrationuM_Replicate.fcs`

**Examples:**
- `Ly18_YF2_0uM_Rep1.fcs`
- `HT_DMSO_50uM_Rep2.fcs`
- `SUDHL4_Compound_10.5uM_Rep1.fcs`

---

## Workflow

1. Upload FCS files (minimum 4 concentrations per cell line)  
2. Optional: Calculate compensation matrix from single-stained controls  
3. Select control concentration (typically 0 µM)  
4. Gate control samples interactively (FSC/SSC → Singlets → Annexin V/PI)  
5. Run automated analysis  
6. Download results package (IC50 values, plots, raw data)  

---

## Compensation Controls

For fluorescence compensation, upload three controls:

- **Unstained**: No fluorescent staining  
- **Annexin V-FITC only**: Single positive control  
- **PI only**: Single positive control
- In the current version, the application is optimized for analyzing cells stained with Propidium Iodide (PI) and Annexin V. However, the workflow is flexible — you can substitute any two stains, and the analysis will still function correctly despite the namings of stains. Future updates will further generalize the app to support a wider range of staining combinations.

---

## IC50 Curve Fitting

Multiple algorithms attempted sequentially for robust fitting:

1. Four-parameter log-logistic (LL.4) with auto starting values  
2. LL.4 with manual starting values  
3. Three-parameter log-logistic (LL.3)  
4. Four-parameter Weibull (W1.4)  
5. Two-parameter Weibull (W1.2)  

IC50 values extracted with 95% confidence intervals via delta method.

---

## Output Files

ZIP package includes:

- **IC50_results.csv**: IC50 values with confidence intervals and fitting method  
- **raw_viability_data.csv**: Per-sample viability measurements  
- **dose_response_curves.png**: High-resolution dose-response plot (300 DPI)  
- **ic50_comparison.png**: Bar chart comparing IC50 values (300 DPI)  
- **file_metadata.csv**: Parsed file information  
- **analysis_settings.csv**: Analysis parameters  
- **compensation_matrix.csv**: Compensation coefficients (if calculated)  

---

## Gating Strategy

Standard hierarchical gating approach:  
**All Events → FSC/SSC (cells) → FSC-A/FSC-H (singlets) → Annexin V/PI (viability)**

Gates defined on control samples and applied uniformly to all conditions

---

## Local Installation
To run the codce locally on your own R you can take the code directly from Github and it will funciton correclty as the website.

### Prerequisites

R ≥ 4.0 and required packages:

```r
# Bioconductor
BiocManager::install("flowCore")

# CRAN packages
install.packages(c("shiny", "bslib", "ggplot2", "dplyr", "tidyr",
                   "readr", "stringr", "drc", "scales", "sp", "zip"))

