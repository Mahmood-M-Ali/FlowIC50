# Legal Notices, Licensing, and Compliance Report

**Project Name:** FlowPath Dynamics
**Author:** Mahmood Mohammed Ali
**Affiliation:** Université Grenoble Alpes | Institute for Advanced Biosciences
**Last Updated:** January 2026

---

## 1. Licensing and Terms of Use

### 1.1 Project License
This software application ("FlowPath Dynamics"), including its source code, documentation, and graphical user interface, is licensed under the **Creative Commons Attribution-NonCommercial 4.0 International (CC BY-NC 4.0)** license.

**Under this license, you are free to:**
*   **Share:** Copy and redistribute the material in any medium or format.
*   **Adapt:** Remix, transform, and build upon the material.

**Under the following terms:**
*   **Attribution:** You must give appropriate credit, provide a link to the license, and indicate if changes were made. You may do so in any reasonable manner, but not in any way that suggests the licensor endorses you or your use.
*   **Non-Commercial:** You may not use the material for commercial purposes (e.g., selling the software, embedding it in a paid commercial product) without explicit written permission from the author.

[View Full License Text](https://creativecommons.org/licenses/by-nc/4.0/)

### 1.2 Academic Citation
If this software contributes to published research, please cite the software version and Digital Object Identifier (DOI) archived on Zenodo:
> **Citation:** Mahmood MOHAMMED ALI. (2025). Mahmood-M-Ali/FlowIC50: v3.1 (v3.1). Zenodo. https://doi.org/10.5281/zenodo.17872796

---

## 2. Intellectual Property and Authorship

### 2.1 Code Ownership
The core logic, architectural design, and implementation of this software are the intellectual property of **Mahmood Mohammed Ali**.

### 2.2 AI Assistance Disclosure
This project was developed with the assistance of large language models (LLMs), specifically Google's Gemini.
*   **Role of AI:** The AI served as a coding assistant for syntax generation, debugging, and boilerplate creation.
*   **Legal Status:** In accordance with current terms of service from major AI providers and evolving copyright law, the human author (Mahmood Mohammed Ali) retains full ownership and responsibility for the curated, compiled, and deployed codebase. The AI provider claims no ownership over the output generated for this project.

---

## 3. Third-Party Dependencies and Licenses

This software relies on the R programming language and an ecosystem of open-source packages. All third-party libraries are used in strict accordance with their respective licenses. No proprietary or "black-box" commercial code is embedded.

### 3.1 Core R Ecosystem (MIT / GPL Compatible)
| Package | License | Purpose |
| :--- | :--- | :--- |
| **shiny** | GPL-3 | Web application framework foundation |
| **bslib** | MIT | Modern UI theming and Bootstrap integration |
| **ggplot2** | MIT | Data visualization and plot generation |
| **dplyr** | MIT | Data manipulation and processing |
| **tidyr** | MIT | Data tidying and restructuring |
| **readr** | MIT | Fast file I/O operations |
| **stringr** | MIT | String manipulation |
| **scales** | MIT | Graphical scaling and formatting |
| **rmarkdown** | GPL-3 | Dynamic report generation engine |
| **knitr** | GPL | Dynamic report weaving |
| **htmltools** | GPL | HTML generation tools |
| **DT** | GPL-3 | Interactive data tables |

### 3.2 Statistical & Scientific Libraries
| Package | License | Purpose |
| :--- | :--- | :--- |
| **drc** | GPL-2 | Dose-response curve modeling (LL.4 analysis) |
| **outliers** | GPL-2 | Statistical outlier detection (Grubbs test) |
| **DescTools** | GPL-2 \| GPL-3 | Descriptive statistics and post-hoc tests |
| **sp** | GPL-2 \| GPL-3 | Spatial data classes (used for point-in-polygon gating) |

### 3.3 Bioinformatics Dependencies (Bioconductor)
| Package | License | Purpose |
| :--- | :--- | :--- |
| **flowCore** | Artistic-2.0 | Standard infrastructure for flow cytometry data (FCS parsing) |

*Note: The **Artistic-2.0** license is an OSI-approved open-source license widely used in the bioinformatics community and is compatible with the project's distribution goals.*

### 3.4 Visualization Extensions
| Package | License | Purpose |
| :--- | :--- | :--- |
| **pheatmap** | GPL-2 | Heatmap generation |
| **gridExtra** | GPL-2 | Grid graphics arrangement |
| **ggrepel** | GPL-3 | Non-overlapping text labels in plots |
| **svglite** | GPL-2 | SVG graphics device |
| **kableExtra** | MIT | Complex table formatting in reports |

---

## 4. Data Standards and File Formats

### 4.1 Input Data: Flow Cytometry Standard (.fcs)
*   The application reads **.fcs** files.
*   **Legal Status:** The FCS format is an open standard maintained by the **International Society for the Advancement of Cytometry (ISAC)**. It is not a proprietary format owned by any single instrument manufacturer (e.g., BD, Beckman Coulter, ThermoFisher).
*   **User Rights:** Users are legally free to read, parse, and analyze their own FCS data generated by any cytometer without owing royalties to manufacturers.

### 4.2 Output Data
*   **Reports:** Generated reports (HTML/PDF) are strictly the property of the user.
*   **Images:** Plots (.png) are generated using open-source libraries (`ggplot2`) and belong to the user.
*   **Data Tables:** Processed results are exported in **.csv** (Comma Separated Values), a universal, non-proprietary text format.

---

## 5. Limitation of Liability ("AS IS" Disclaimer)

This software is provided for research and educational purposes only.

> **DISCLAIMER:** THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

**Clinical Disclaimer:** This tool is **NOT** a certified medical device (FDA/CE/IVDR). It should not be used for primary clinical diagnosis or patient management decisions. All results should be validated by standard laboratory procedures.
