\# Flow Cytometry IC50 Analyzer



Automated Shiny application for flow cytometry-based IC50 determination using Annexin V/PI apoptosis assays with fluorescence compensation.



\[!\[R](https://img.shields.io/badge/R-%3E%3D4.0-blue)](https://www.r-project.org/)

\[!\[License: CC BY-NC 4.0](https://img.shields.io/badge/License-CC%20BY--NC%204.0-lightgrey.svg)](http://creativecommons.org/licenses/by-nc/4.0/)

\[!\[Shiny](https://img.shields.io/badge/Shiny-App-blue)](https://shiny.rstudio.com/)



---



\## Quick Start



\*\*No installation required.\*\* Access the web application directly at:



\### https://huggingface.co/spaces/mahmood-iab/FACS-Analysis\*\*



Upload FCS files and analyze immediately in your browser.



---



\## Features



\- Automated batch processing of FCS files

\- Fluorescence spillover compensation from single-stained controls

\- Interactive hierarchical gating (FSC/SSC, singlets, viability)

\- Robust IC50 curve fitting with multiple algorithms

\- Publication-ready plots (300 DPI) and CSV exports

\- Complete results package download (ZIP)



---



\## File Naming Convention



File names must follow this format: CellLine\_Treatment\_ConcentrationuM\_Replicate.fcs





\*\*Examples:\*\*

\- `Ly18_YF2_0uM_Rep1.fcs`

\- `HT_DMSO_50uM_Rep2.fcs`

\- `SUDHL4_Compound_10.5uM_Rep1.fcs`



---



\## Workflow



1\. Upload FCS files (minimum 4 concentrations per cell line)

2\. Optional: Calculate compensation matrix from single-stained controls

3\. Select control concentration (typically 0 µM)

4\. Gate control samples interactively (FSC/SSC → Singlets → Annexin V/PI)

5\. Run automated analysis

6\. Download results package (IC50 values, plots, raw data)



---



\## Compensation Controls



For fluorescence compensation, upload three controls:



\- \*\*Unstained\*\*: No fluorescent staining

\- \*\*Annexin V-FITC only\*\*: Single positive control

\- \*\*PI only\*\*: Single positive control



The application calculates a compensation matrix using classical spillover correction (Roederer, 2001) and applies it to all samples.



---



\## IC50 Curve Fitting



Multiple algorithms attempted sequentially for robust fitting:



1\. Four-parameter log-logistic (LL.4) with auto starting values

2\. LL.4 with manual starting values

3\. Three-parameter log-logistic (LL.3)

4\. Four-parameter Weibull (W1.4)

5\. Two-parameter Weibull (W1.2)



IC50 values extracted with 95% confidence intervals via delta method.



---



\## Output Files



ZIP package includes:



\- \*\*IC50\_results.csv\*\*: IC50 values with confidence intervals and fitting method

\- \*\*raw\_viability\_data.csv\*\*: Per-sample viability measurements

\- \*\*dose\_response\_curves.png\*\*: High-resolution dose-response plot (300 DPI)

\- \*\*ic50\_comparison.png\*\*: Bar chart comparing IC50 values (300 DPI)

\- \*\*file\_metadata.csv\*\*: Parsed file information

\- \*\*analysis\_settings.csv\*\*: Analysis parameters

\- \*\*compensation\_matrix.csv\*\*: Compensation coefficients (if calculated)



---



\## Gating Strategy



Standard hierarchical gating approach: All Events → FSC/SSC (cells) → FSC-A/FSC-H (singlets) → Annexin V/PI (viability)







Gates defined on control samples and applied uniformly to all conditions, following flow cytometry best practices (Rieger \& Barrientos, 2007).



---



\## Local Installation



For customization or offline use:



\### Prerequisites



R ≥ 4.0 and required packages:

\# Bioconductor



BiocManager::install("flowCore")



\# CRAN packages



install.packages(c("shiny", "bslib", "ggplot2", "dplyr", "tidyr",



"readr", "stringr", "drc", "scales", "sp", "zip"))





\### Run

git clone https://https://github.com/Mahmood-M-Ali/FlowIC50.git



shiny::runApp("FlowIC50/app.R")







---



\## Citation

Mahmood Mohammed Ali (2025). Flow Cytometry IC50 Analyzer.



GitHub: https://github.com/Mahmood-M-Ali/FlowIC50



Web App: https://huggingface.co/spaces/mahmood-iab/FACS-Analysis









---



\## Author



\*\*Mahmood Mohammed Ali\*\*



Université Grenoble Alpes | Institute for Advanced Biosciences  

Epigenetics of Regeneration and Cancer Group



Email: mahmood.mohammed-ali@univ-grenoble-alpes.fr  

GitHub: \[@Mahmood-M-Ali](https://github.com/Mahmood-M-Ali)  

LinkedIn: \[Mahmood Mohammed Ali](www.linkedin.com/in/mahmood-mohammed-ali-20334b205)



---



\## Acknowledgments



\- \*\*Code Development\*\*: Assembled with assistance from Notion AI

\- \*\*Flow Cytometry Analysis\*\*: \[flowCore](https://bioconductor.org/packages/flowCore/) Bioconductor package

\- \*\*Dose-Response Modeling\*\*: \[drc](https://CRAN.R-project.org/package=drc) package

\- \*\*Web Framework\*\*: \[Shiny](https://shiny.rstudio.com/) by Posit

\- \*\*Hosting\*\*: \[Hugging Face Spaces](https://huggingface.co/spaces)



---



\## License



\[!\[CC BY-NC 4.0](https://licensebuttons.net/l/by-nc/4.0/88x31.png)](http://creativecommons.org/licenses/by-nc/4.0/)



This work is licensed under Creative Commons Attribution-NonCommercial 4.0 International License.



---



\## References



\- Vermes et al., 1995. J Immunol Methods (Annexin V/PI methodology)

\- Roederer, 2001. Cytometry (Compensation)

\- Ritz \& Streibig, 2005. J Stat Softw (drc package)






