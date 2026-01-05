# FlowPath Dynamics: Precision Flow Cytometry Analysis

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.17872796.svg)](https://doi.org/10.5281/zenodo.17872796)
[![R](https://img.shields.io/badge/R-%3E%3D4.0-blue)](https://www.r-project.org/)
[![License: CC BY-NC 4.0](https://img.shields.io/badge/License-CC%20BY--NC%204.0-lightgrey.svg)](http://creativecommons.org/licenses/by-nc/4.0/)
[![Shiny](https://img.shields.io/badge/Shiny-App-blue)](https://shiny.rstudio.com/)
[![Hugging Face](https://img.shields.io/badge/Spaces-FlowPath--Dynamics-yellow?logo=huggingface)](https://huggingface.co/spaces/mahmood-iab/FACS-Analysis)

**FlowPath Dynamics** is a professional, publication-ready R Shiny application designed for automated flow cytometry dose-response profiling. It supports both specialized **Annexin V/PI Apoptosis Assays** and **Multi-Channel Phenotypic Analysis**, providing high-throughput quantification from raw FCS files to statistical inference.

---

## 🚀 Quick Start

**No installation required.** Access the full suite directly at:  
[https://huggingface.co/spaces/mahmood-iab/FACS-Analysis](https://huggingface.co/spaces/mahmood-iab/FACS-Analysis)

---

## ✨ Key Enhancements (v3.0)

- **Dual Analysis Engines:** Switch between specialized Apoptosis profiling and a new, combinatorial Multi-Channel Generic mode (up to $2^N$ populations).
- **Absolute Quantification:** Integrated volumetric cell counting using counting beads (e.g., Precision Count Beads™).
- **Dynamic Analysis Engine:** Instant results recalculation (curves, stats, plots) without re-processing raw files.
- **Interactive Data Exclusion:** Real-time replicate exclusion with visual feedback and outlier detection (Grubbs Test).
- **Advanced Compensation:** Support for per-cell-line manual matrix adjustment with real-time preview.
- **Scientific Rigor:** Statistical inference using One-Way ANOVA on Log10-potency values with Tukey HSD post-hoc testing.
- **Publication Graphics:** Automatic generation of Dumbbell plots (Therapeutic Window), violin plots (MFI), and stacked bar charts.

---

## 📁 File Naming Convention

Files must follow this standardized format for automatic metadata extraction:  
`CellLine_Treatment_ConcentrationuM_Replicate.fcs`

**Examples:**
- `Ly1_CompoundX_10uM_Rep1.fcs`
- `NUDUL1_DMSO_0uM_Rep3.fcs` (Control)

---

## 🛠 Workflow

1. **Upload:** Add batch FCS files and optional single-stained controls.
2. **Setup:** Select your analysis mode (Apoptosis vs. Generic) and enable Absolute Counting if beads were used.
3. **Compensate:** Calculate or manually fine-tune the spillover matrix.
4. **Gate:** Interactive hierarchical gating (Beads → FSC/SSC → Singlets → Marker Thresholds).
5. **Review:** Verify gates across all cell lines in the visual dashboard.
6. **Analyze:** Run the engine to fit curves and perform pairwise statistical comparisons.
7. **Export:** Download a comprehensive ZIP package including a 20+ page technical HTML report.

---

## 📊 Analytical Methodology

### Curve Fitting
Powered by the `drc` package, the app iteratively attempts:
1. **LL.4:** Four-parameter log-logistic (Standard).
2. **LL.3:** Three-parameter (Fixed upper limit).
3. **Weibull (W1.4/W2.4):** Robust alternatives for non-standard responses.

### Potency Metrics
- **IC50/LD50/EC50:** Half-maximal effective concentrations with 95% CI.
- **Therapeutic Window:** Distance-based comparison of potency vs. toxicity.
- **Absolute Survival:** Normalization to bead-based volumetric concentration to capture total cell loss.

---

## 📦 Output Package (ZIP)

- **Technical Report:** Comprehensive HTML documentation of methodology, gating, and results.
- **Data Tables:** CSV files for Raw Quadrant Data, IC50/EC50 Results, QC Metrics (CV%), and Tukey HSD Stats.
- **High-Res Figures:** 300 DPI PNG/SVG exports of all generated plots.
- **Traceability:** Exported compensation matrices and gate coordinates.

---

## 💻 Local Installation

Run locally in RStudio using the following dependencies:

```r
# Bioconductor
BiocManager::install("flowCore")

# CRAN
install.packages(c("shiny", "bslib", "ggplot2", "dplyr", "tidyr", "readr", 
                   "stringr", "drc", "scales", "sp", "zip", "svglite", 
                   "outliers", "DescTools", "gridExtra", "rmarkdown", 
                   "kableExtra", "DT", "ggrepel", "knitr", "htmltools"))
```

---

## 🎓 Developed By

**Mahmood Mohammed Ali**  
Université Grenoble Alpes | Institute for Advanced Biosciences (IAB)  
Epigenetics of Regeneration and Cancer Group  
[mahmood.mohammed-ali@univ-grenoble-alpes.fr](mailto:mahmood.mohammed-ali@univ-grenoble-alpes.fr)
