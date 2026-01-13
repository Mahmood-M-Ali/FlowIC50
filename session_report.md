# Session Report & Development Plan: FACS App

**Date:** 2026-01-05

## 1. High-Level Project Goal

The primary objective is to enhance a professional, publication-ready R Shiny web application designed for flow cytometry dose-response analysis. This tool is critical for DLBCL (Diffuse Large B-cell Lymphoma) research, automating the entire workflow from raw FCS file analysis to the generation of publication-quality dose-response curves and statistical reports. The app is being developed by Mahmood Mohammed Ali for deployment on Hugging Face Spaces.

## 2. Project Directory & File Manifest

The project is located at `C:\Users\hajia\Desktop\FACS APP\FACS app working directory`. The key files and their roles are as follows:

-   `app.R`: The core file for the Shiny application. It contains all the UI and server logic, now acting as a router for different analysis workflows.
-   `facs_report.Rmd`: An R Markdown template used to generate the final downloadable HTML/PDF report from the apoptosis analysis results.
-   `generic_analysis.R`: Contains the modularized UI and server logic for the generic (multi-channel expression) analysis workflow.
-   `generic_report.Rmd`: An R Markdown template used to generate the final downloadable HTML/PDF report specifically for the generic analysis results.
-   `app_oldversion.R`: An older version of the application logic, kept as a reference.
-   `Dockerfile`: Contains instructions for building the Docker container for deployment.
-   `attune-nxt-flow-cytometer-brochure.pdf` & `attune-nxt-flow-cytometer-spec-sheet.pdf`: Technical specifications for the Attune NxT flow cytometer.
-   `FACS_IC50_Analysis_2025-12-23/`: Example output from a previous analysis run.

## 3. Detailed Development Plan (To-Do List)

Here is the full list of tasks for this development cycle, along with a detailed implementation strategy for each.

---


### ✅ 1. Investigate Codebase & User Requirements
-   **Status:** `completed`
-   **Explanation:** Initial review of all files to understand application state and user requests.

---


### ✅ 2. Implement Improved File Input
-   **Status:** `completed`
-   **Explanation:** Added `rv$file_data`, modified `observeEvent` for `input$files`, created "Remove" buttons, and updated `output$file_table`.

---


### ✅ 3. Implement Smart Axis Breaks for Plots
-   **Status:** `completed`
-   **Explanation:** Updated `get_smart_breaks` function for dynamic logarithmic breaks.

---


### ✅ 4. Expand Compensation Options
-   **Status:** `completed`
-   **Explanation:** Added `rv$comp_controls`, dynamic UI, and refactored `observeEvent` for `input$calc_comp` for compensation matrix calculation.

---


### ✅ 5. Consolidate UI Panels
-   **Status:** `completed`
-   **Explanation:** Consolidated "Quality Control" and "Advanced Metrics" into "Results" `tabPanel`.

---


### ✅ 6. Improve R Markdown Report
-   **Status:** `completed`
-   **Explanation:** Improved R Markdown report with YAML header updates and `kableExtra` styling.

---


### ✅ 7. Preserve Header and Footer
-   **Status:** `completed`
-   **Explanation:** Confirmed existing header and footer are to be kept.

---


### ✅ 8. Review and Finalize
-   **Status:** `completed`
-   **Explanation:** Final review of implemented changes.

## 4. Current Development Cycle: Implementing Generic FACS Analysis Workflow

This section details the current development cycle's objectives and the steps taken to implement a new, generic FACS analysis workflow alongside the existing apoptosis assay.

---


### ✅ 1. Add a checkbox input to the UI to switch between "Apoptosis" and "Generic" analysis modes.
-   **Status:** `completed`
-   **Explanation:** A `checkboxInput` with ID `is_apoptosis_assay` was added to the sidebar in `app.R`.

---


### ✅ 2. Create a new file `generic_analysis.R` to house the UI and server logic for the generic workflow.
-   **Status:** `completed`
-   **Explanation:** `generic_analysis.R` was created to modularize the new workflow's components.

---


### ✅ 3. Research common generic FACS analysis use cases to inform the new workflow's design.
-   **Status:** `completed`
-   **Explanation:** Research confirmed a 3-step gating process: FSC/SSC for debris, FSC-A/FSC-H for singlets, and user-defined threshold on a selected channel.

---


### ✅ 4. Implement the UI for the generic workflow in `generic_analysis.R`.
-   **Status:** `completed`
-   **Explanation:** `generic_gating_ui` was populated with UI elements for the 3-step generic gating, including reused plot outputs for FSC/SSC and Singlet, and new `selectInput` for channel, dynamic `sliderInput` for threshold, and plot/stats outputs for the target channel histogram.

---


### ✅ 5. Implement the server logic for the generic workflow in `generic_analysis.R`.
-   **Status:** `completed`
-   **Explanation:** `generic_gating_server` contains observers for navigation, dynamic slider rendering, and plot/stats. `run_generic_analysis` was implemented for full analysis, including gating, % positive, and EC50.

---


### ✅ 6. Use conditional panels in `app.R` to show/hide the appropriate UI based on the analysis mode.
-   **Status:** `completed`
-   **Explanation:** "Gating", "Results", and "Plots" `tabPanel`s in `app.R` were updated with `conditionalPanel`s to dynamically display apoptosis or generic UI based on `is_apoptosis_assay`.

---


### ✅ 7. Update the `analyze` and `download_zip` observers in `app.R` to execute the correct workflow (apoptosis or generic) based on the selected mode.
-   **Status:** `completed`
-   **Explanation:**
    -   **`analyze` observer:** Refactored to route based on `input$is_apoptosis_assay`, executing either apoptosis analysis or `run_generic_analysis`.
    -   **`download_zip` handler:** Updated with routing logic, saving appropriate CSVs and plots, and rendering `facs_report.Rmd` or `generic_report.Rmd`.
    -   **New Reactive Values:** Added `rv$generic_results`, `rv$generic_ec50_results`, `rv$generic_plot_data`, `rv$generic_predicted_curves`, and `rv$generic_plot_obj`.
    -   **`generic_report.Rmd`:** Created a new R Markdown file for the generic analysis report.

---


### **Debugging & Refinement of Generic Workflow (Previous Session)**

This section details critical bug fixes and further refinements made to the application's stability and functionality, particularly concerning the newly introduced generic workflow and dynamic UI elements.

---


### ✅ **Fix: App Crash on Deleting Compensation Files (`invalid subscript type 'list'`)**
-   **Issue:** The app would crash when dynamically added compensation control files were deleted. The error `invalid subscript type 'list'` pointed to issues in how dynamic observers were managed.
-   **Root Cause:** Nested `observe` blocks led to "observer leakage," where old, dangling observers would attempt to access invalidated data structures (`rv$comp_controls`) after elements were removed, causing runtime errors.
-   **Solution:** A comprehensive refactoring of the compensation control management was implemented. Instead of nested observers, a single `observeEvent(input$add_comp_control, ...)` now dynamically creates all necessary `observeEvent`s (for file upload, channel selection, and removal) for each new control. These observers' handles are stored in `rv$comp_observers`. The `remove` observer is configured with `once = TRUE` and explicitly `destroy()`s itself and its sibling observers upon activation. This robust pattern ensures proper observer lifecycle management, eliminating dangling observers and preventing crashes.
-   **Files Modified:** `app.R` (specifically, `rv` initialization and dynamic compensation control observers).

---


### ✅ **Fix: Missing Generic Gating Plots (FSC/SSC and Singlets)**
-   **Issue:** In generic analysis mode, the FSC/SSC and Singlet gating plots were not appearing in the UI for user review during the gating process.
-   **Root Cause:** The logic responsible for loading the control FCS file and populating `rv$current_gating_data_fsc` and `rv$current_gating_data_singlet` was initially embedded within the apoptosis-specific `output$gating_ui` `renderUI` block. When the generic UI was active, this code path was not executed, leaving the reactive values (and thus the plots) unpopulated.
-   **Solution:** The data-loading and initial data preparation logic for both FSC/SSC and Singlet gates were extracted and moved into dedicated, central `observe` blocks. These observers now trigger whenever `rv$current_cell_line_index` and `rv$gating_step` indicate the start of a new gating step, ensuring the necessary data is loaded and processed regardless of the active workflow (apoptosis or generic). The `output$gating_ui` was then refactored to rely on these centrally populated reactive values.
-   **Files Modified:** `app.R` (added two new central `observe` blocks, refactored `output$gating_ui`).

---


### ✅ **Fix: State Inconsistency After Switching Workflow Modes**
-   **Issue:** Switching between "Apoptosis" and "Generic" analysis modes (using the `is_apoptosis_assay` checkbox) after some gating progress caused the app to crash or display `non-numeric argument to mathematical function` errors in the "Gate Review" tab.
-   **Root Cause:** The app's internal state (`rv$thresholds`, `rv$gating_step`, etc.) was not being adequately reset when the workflow mode is toggled. This led to incompatible gate structures (e.g., an apoptosis UI trying to interpret a generic gate) and downstream analysis errors.
-   **Solution:** An `observeEvent` was added for `input$is_apoptosis_assay`. This observer now explicitly resets all relevant gating-related and results-related `reactiveValues` to their initial `NULL` or empty states whenever the workflow mode is toggled. A warning notification is also displayed to inform the user of the state reset.
-   **Files Modified:** `app.R` (added new `observeEvent(input$is_apoptosis_assay)`).

---


### ✅ **Fix: 'Download ZIP' Button Missing in Generic Mode**
-   **Issue:** The "Download ZIP" button did not appear after completing analysis in generic mode.
-   **Root Cause:** The `renderUI` for `output$download_zip_ui` had a `req(rv$results)` condition. In generic mode, results are stored in `rv$generic_results`, leaving `rv$results` as `NULL`.
-   **Solution:** The `req()` condition in `output$download_zip_ui` was updated to `req(!is.null(rv$results) || !is.null(rv$generic_results))`, ensuring the button appears if either set of results is available.
-   **Files Modified:** `app.R`.

---


### ✅ **Fix: `Failed to create ZIP file: object 'params' not found` in Generic Mode**
-   **Issue:** After analysis in generic mode, attempting to download the ZIP file resulted in an R Markdown rendering error: `object 'params' not found` when processing `generic_report.Rmd`.
-   **Root Cause:** The `generic_report.Rmd` file was missing the crucial `params:` block in its YAML header. Without this declaration, R Markdown does not formally recognize or prepare the `params` object from the arguments passed to `rmarkdown::render`.
-   **Solution:** The `params:` block was added to the YAML header of `generic_report.Rmd`, explicitly declaring `ec50_table`, `results_raw`, and `plot` as expected parameters. This correctly informs R Markdown to create the `params` object for the report's environment. The `envir = new.env(parent = globalenv())` argument was also confirmed to be present in the `rmarkdown::render` call for best practice, although the YAML header fix was the primary resolution.
-   **Files Modified:** `generic_report.Rmd`, `app.R` (confirmed `envir` argument).

---


### ✅ **Fix: Gate Review Plots Incomplete (Not Showing All Steps)**
-   **Issue:** In "Gate Review", only the first gating plot (FSC/SSC) was displayed in apoptosis mode, and no plots were shown in generic mode, despite individual plot objects being generated.
-   **Root Cause:** The `renderPlot` for the gate review UI was only referencing `rv$gate_plots` (which held only the FSC/SSC plot). The logic was not combining the multiple plot objects (FSC/SSC, Singlets, Annexin/PI or Histogram) into a single, cohesive view.
-   **Solution:** A new reactive value `rv$gate_review_plots` was introduced. In both `observeEvent(input$save_gate, ...)` (apoptosis) and `observeEvent(input$save_gate_generic, ...)` (generic), after the individual gate plots are created, they are now combined using `gridExtra::arrangeGrob(...)` into a single graphical object, which is then stored in `rv$gate_review_plots`. The `renderPlot` for the gate review UI was updated to render this combined object using `grid::grid.draw()`.
-   **Files Modified:** `app.R` (`rv` initialization, `observeEvent(input$save_gate, ...)`, and dynamic gate visualization `observe` block), `generic_analysis.R` (`observeEvent(input$save_gate_generic, ...)`).

---


### ✅ **Update the generic report (generic_report.Rmd) and download handler to include the new plots and data.**
-   **Status:** `completed`
-   **Explanation:**
    -   **`app.R` Download Handler:** The `output$download_zip` handler was refactored to:
        *   Correctly save new CSVs for generic analysis: `generic_population_percentages.csv`, `generic_EC50_results.csv`, `generic_raw_intensity_data.csv`.
        *   Explicitly save generated ggplot objects for generic plots as PNGs: `generic_population_breakdown.png`, `generic_fluorescence_intensity.png`, `generic_dose_response_curve.png`.
        *   Pass these plot objects (stored in `rv`) as parameters to `generic_report.Rmd`.
    -   **`app.R` `rv` Reactive Values:** Added `generic_pop_breakdown_plot_obj`, `generic_intensity_plot_obj`, `generic_dose_response_plot_obj` to `rv` to store plot objects.
    -   **`app.R` Generic `renderPlot`s:** Modified `output$generic_pop_breakdown_plot`, `output$generic_intensity_plot`, and `output$generic_dose_response_plot` to assign their generated ggplot objects to the corresponding `rv` variables.
    -   **`generic_report.Rmd` Verification:** Confirmed that `generic_report.Rmd`'s YAML `params` now correctly define `pop_breakdown_plot`, `intensity_plot`, and `dr_plot` and that the R Markdown chunks use `print(params$plot_name)` to render these passed ggplot objects.

---

## 5. Multi-Channel Generic Analysis Expansion

**Objective:** To evolve the "Generic" analysis mode from a single-channel to a multi-channel analysis, allowing users to define and quantify multiple markers simultaneously from the same FCS file. This provides more flexible and scientifically relevant visualizations.

### ✅ 1. Overhaul Generic Gating UI & Logic (in `generic_analysis.R`)
-   **Status:** `completed`
-   **Explanation:** Replaced the static "Target Channel Gate" with a dynamic UI section. Users can now "Add Channel Gate" multiple times. Each channel gate has its own `selectInput`, `sliderInput`, "Remove" button, and live histogram.
Implemented robust observer management to prevent memory leaks and crashes.

### ✅ 2. Enhance Generic Analysis Engine (in `generic_analysis.R`)
-   **Status:** `completed`
-   **Explanation:** `run_generic_analysis` now iterates through all defined gates. It classifies cells for each marker and calculates percentages for all 2^N possible population combinations (e.g., A+B+, A+B-, etc.). It also extracts raw transformed intensity data for every cell in selected channels.

### ✅ 3. Redesign Generic "Plots" Tab (in `app.R`)
-   **Status:** `completed`
-   **Explanation:** Implemented a `tabsetPanel` for Generic mode with three sub-tabs:
    -   **Population Breakdown:** Stacked bar chart showing the full phenotype distribution across samples.
    -   **Fluorescence Intensity:** Violin/box plots for comparing expression levels across samples (selectable channel).
    -   **Dose-Response (EC50):** Publication-quality curves fitted for any user-selected population combination.

### ✅ 4. Update Generic Report Template (`generic_report.Rmd`)
-   **Status:** `completed`
-   **Explanation:** Expanded the report to include the new multi-channel gating summary, population breakdown plots, individual channel intensity plots, and selectable dose-response curves.
## 6. Bug Fixes & Refinements (Session 2026-01-02)

### ✅ **Fix: Duplicated Plots in Gate Review**
-   **Issue:** In apoptosis mode, the "Gate Review" tab showed 6 plots instead of 3 due to a duplication in the `arrangeGrob` call.
-   **Solution:** Removed the duplicated plot references in `app.R`.

### ✅ **Fix: Pandoc Warnings (`unclosed Div`) in Apoptosis Report**
-   **Issue:** Generating the apoptosis report produced warnings about unclosed `<div>` tags, specifically in the gating plot sections.
-   **Solution:** Refactored `facs_report.Rmd` to move the `div class="figure"` tags inside the loops. This ensures every individual plot is correctly self-contained and closed, resolving the Pandoc warnings and improving layout reliability.
### ✅ **Enhancement: Generic Report Gating Strategy Plot Display**
-   **Issue:** The "Gating Strategy Visualization" section in `generic_report.Rmd` only displayed one plot despite being in a loop, and captions were not dynamic.
-   **Solution:** Modified the rendering loop in `generic_report.Rmd` to ensure each `gridExtra::arrangeGrob` object is properly rendered as a separate figure. This involved adding `library(grid)` and `library(gridExtra)` to the setup chunk, using `grid::grid.newpage()`, `grid::grid.draw()`, wrapping each plot in `<div class="figure">`, and adding `cat('\n\n')` to ensure knitr correctly separates figures. Dynamic captions now include the cell line name.
-   **Files Modified:** `generic_report.Rmd`.

### ✅ **Enhancement: Interactive Per-Cell-Line Compensation Adjustment**
-   **Issue:** The application only supported a single, automatically calculated compensation matrix without user-adjustable parameters or per-cell-line flexibility.
-   **Solution:** Implemented a comprehensive system for manual compensation adjustment and per-cell-line management.
    -   **Data Structures (`app.R`):** Introduced `rv$spill_matrices` and `rv$comp_matrices` to store compensation data for each cell line.
    -   **Initialization (`app.R`):** The `observeEvent(input$calc_comp)` now populates `rv$spill_matrices` and `rv$comp_matrices` for all loaded cell lines with the initially calculated matrix.
    -   **UI Overhaul (`app.R`):** Added a "Manual Adjustment" section to the "Compensation" tab, featuring:
        -   A `selectInput` to choose a cell line (or "Global Master").
        -   An editable `DT::dataTableOutput` for the spillover matrix, allowing direct user input.
        -   A real-time `plotOutput` to preview compensation effects on a sample file.
        -   Action buttons for "Apply Current Matrix to All" and "Reset to Calculated".
    -   **Server Logic (`app.R`):** Implemented observers to handle `DT` cell edits, recalculate matrices, update the preview plot, and manage the "Apply to All" and "Reset" actions.
    -   **Gating Workflow Integration (`app.R`, `generic_analysis.R`):** Modified the `annexin_pi` gating step (apoptosis) and all generic gating visualization/saving steps to dynamically retrieve the correct compensation matrix for the `current_cell_line` from `rv$comp_matrices`, with a fallback to the global `rv$comp_matrix`.
    -   **Analysis Engine Integration (`generic_analysis.R`):** The `run_generic_analysis` function's signature was updated to accept `comp_matrices`, and its internal logic modified to apply the appropriate cell-line-specific compensation matrix to each file.
    -   **`app.R` Download Handler Updates:** The call to `run_generic_analysis` was updated to pass the full `rv$comp_matrices` list.
-   **Files Modified:** `app.R`, `generic_analysis.R`.

### ✅ **Enhancement: Comprehensive Compensation Reporting**
-   **Issue:** The generated reports did not accurately reflect the per-cell-line compensation matrices when manual adjustments were made.
-   **Solution:** Updated both `facs_report.Rmd` and `generic_report.Rmd` to transparently display the compensation matrices used.
    -   **`app.R` Download Handler:** Modified the `rmarkdown::render` calls for both reports to pass the `rv$comp_matrices` list.
    -   **`facs_report.Rmd` and `generic_report.Rmd`:**
        -   Updated YAML `params` to include `comp_matrices: NULL`.
        -   Added/modified a "Fluorescence Compensation" section (now 1.3 in Apoptosis, 1.2 in Generic) that:
            -   Identifies unique compensation matrices within the passed list.
            -   For each unique matrix, displays the matrix table and clearly lists all cell lines to which that specific matrix was applied.
        -   Renumbered subsequent sections in `generic_report.Rmd` to maintain correct document hierarchy.
-   **Files Modified:** `app.R`, `facs_report.Rmd`, `generic_report.Rmd`.

### ✅ **Enhancement: Unified Automatic & Manual Compensation Visibility**
-   **Issue:** The reports initially showed the unique applied matrices but omitted the primary "Initial Automatic Inverse Compensation Matrix" and failed to render tables/plots correctly inside loops.
-   **Solution:** Updated `app.R` to explicitly pass `auto_comp_matrix` (calculated from `rv$comp_matrix`) to reports. Refactored the "Fluorescence Compensation" section in both `facs_report.Rmd` and `generic_report.Rmd` to:
    1. Display the system-calculated "Initial Automatic Inverse Compensation Matrix" table.
    2. Follow it immediately with the "Compensation Effect (Before/After)" scatter and density plots for validation.
    3. Maintain the subsequent per-matrix breakdown and per-cell-line manual adjustment professional plots.
-   **Fix: Reliable Report Table Rendering**
    -   **Issue:** Numerical compensation tables and manual adjustment plots were intermittently missing from the final HTML due to `knitr` loop rendering limitations.
    -   **Solution:** Switched from `print()` to a more robust HTML emission pattern in `asis` chunks: `cat(as.character(as.data.frame(mat) %>% render_table(...)))`. This ensures `kableExtra` objects are correctly converted to HTML strings and captured by the report engine.
-   **Files Modified:** `app.R`, `facs_report.Rmd`, `generic_report.Rmd`.

## 7. Advanced Statistical Analysis & Interactive Workflows (Session 2026-01-03)

### ✅ **Refactoring: Dynamic Analysis Engine**
-   **Goal:** Enable instant recalculation of results (curve fitting, stats, plots) without re-processing raw FCS files, laying the groundwork for interactive exclusions.
-   **Implementation:**
    -   Truncated `observeEvent(input$analyze)` to **only** perform gating and generate raw result dataframes (`rv$results`).
    -   Created two new `observe` blocks (Dynamic Analysis Engines) for Apoptosis and Generic modes. These engines listen for changes in `rv$results` or `rv$manual_exclusions` and automatically re-run the entire downstream analysis pipeline (QC -> Normalization -> Fitting -> Stats -> Plotting).
-   **Files Modified:** `app.R`, `generic_analysis.R` (fitting logic removed).

### ✅ **Enhancement: Interactive Outlier Exclusion**
-   **Issue:** Users could identify outliers via the Grubbs test but had no mechanism to exclude them from the analysis without modifying input files.
-   **Solution:** Implemented a robust "Toggle Exclusion" system for both workflows.
    -   **UI:** Added "Toggle Include/Exclude Selected" buttons and updated `qc_table` (Apoptosis) and `generic_results_table` (Generic) to support row selection.
    -   **Visuals:** Excluded rows are instantly highlighted in red with strikethrough text.
    -   **Logic:** `rv$manual_exclusions` and `rv$generic_manual_exclusions` track excluded replicate IDs. The Dynamic Analysis Engines automatically filter these out before curve fitting.
    -   **Reporting:** Added "Data Exclusion" sections to both `facs_report.Rmd` and `generic_report.Rmd` to transparently list any manually excluded replicates.
-   **Files Modified:** `app.R`, `facs_report.Rmd`, `generic_report.Rmd`.

### ✅ **Enhancement: Scientific Rigor in Statistics (Tukey HSD & Log-Transformation)**
-   **Issue:** The previous ANOVA was performed on raw IC50 values (violating normality assumptions for potency data) and lacked post-hoc testing to identify specific cell line differences.
-   **Solution:**
    -   **Log-Transformation:** Updated all statistical tests to perform ANOVA on `log10(IC50)` (or `log10(EC50)`), aligning with standard pharmacological practice ($pIC50$). 
    -   **Post-Hoc Testing:** Implemented **Tukey's Honest Significant Difference (HSD)** test.
    -   **Reporting:** Added detailed "Pairwise Comparisons (Tukey HSD)" tables to the UI ("Results" tab) and both PDF/HTML reports, showing p-values and significance levels (*, **, ***).
-   **Files Modified:** `app.R`, `facs_report.Rmd`, `generic_report.Rmd`.

### ✅ **Enhancement: Comprehensive Technical Methodology Appendix**
-   **Issue:** The reports lacked detailed explanations of the "black box" algorithms used for gating, compensation, and modeling.
-   **Solution:** Added a new **"A2. Computational & Statistical Methodology"** appendix to both reports. This section provides textbook-quality explanations of:
    -   Linear algebra for Compensation ($D = S \times C^{-1}$). 
    -   Ray-casting algorithms for Polygon Gating.
    -   Combinatorial Boolean Logic for Multi-channel Gating.
    -   Mathematical formulas for the Four-Parameter Log-Logistic (LL.4) model.
    -   Statistical justifications for Log10-ANOVA and Grubbs outlier detection.
-   **Files Modified:** `facs_report.Rmd`, `generic_report.Rmd`.

### ✅ **Fix: Generic Report Exclusion Logic**
-   **Issue:** The Generic report was incorrectly receiving the exclusion list from the Apoptosis workflow (`rv$manual_exclusions`).
-   **Solution:** Corrected the `download_zip` handler to pass `rv$generic_manual_exclusions` to `generic_report.Rmd`.
-   **Files Modified:** `app.R`.

## 8. Advanced Compensation & Visualization Logic Refinement (Session 2026-01-03 - Continued)

### ✅ **Fix: Compensation Matrix Type Mismatch**
-   **Issue:** Identified that `flowCore::compensate()` requires a **Spillover Matrix**, but the previous version was incorrectly passing the **Inverse Compensation Matrix**. This resulted in "double-inversion," effectively cancelling the compensation and leaving data with high diagonal correlation (spillover).
-   **Solution:** Refactored the gating UI, save steps, and analysis engines in both `app.R` and `generic_analysis.R` to correctly use `rv$spill_matrices` (Spillover) for the `compensate()` call.
-   **Files Modified:** `app.R`, `generic_analysis.R`.

### ✅ **Enhancement: Restored Natural "Cloud" Visualization**
-   **Issue:** Using `estimateLogicle` on highly correlated (uncompensated or poorly compensated) data created "linear line" artifacts, making gating difficult.
-   **Solution:** Reverted the Apoptosis Step 3 gating to the standard `logicleTransform()`. This restores the familiar cloud distribution. To maintain scientific consistency, the specific transformation parameters are now saved during gating and reused identically during final analysis.
-   **Files Modified:** `app.R`, `generic_analysis.R`.

### ✅ **Enhancement: Real-time Gating Diagnostics**
-   **Issue:** Users could not verify if the correct compensation matrix was being applied during the gating process.
-   **Solution:** Added a real-time debug display in the Apoptosis Step 3 gating UI that prints the exact Spillover Matrix currently in use for that specific cell line.
-   **Files Modified:** `app.R`.

### ✅ **Optimization: UI/Report Consistency & Para-Data Traceability**
-   **Issue:** Statistical summaries and QC metrics were not easily accessible for individual replicates.
-   **Solution:**
    -   Added **"Gating Summary"** tables to the Results tabs for both workflows to track cell recovery.
    -   Implemented dedicated **CSV Download Buttons** for Tukey HSD stats and QC Metrics.
    -   Standardized **Outlier Detection** in Generic mode to include population-specific p-values.
    -   Fixed **JavaScript table styling** (red strikethrough for excluded rows) by standardizing `rownames = FALSE` across all DT tables.
-   **Files Modified:** `app.R`, `facs_report.Rmd`, `generic_report.Rmd`.

## 9. UI/UX Overhaul & Advanced Statistical Reporting (Session 2026-01-03 - Evening)

### ✅ **Enhancement: Professional Branding & Identity**
-   **New Title:** **FlowPath Dynamics**
-   **New Subtitle:** **Precision Dose-Response Profiling for Apoptosis and Multi-Channel Flow Cytometry**
-   **Header Overhaul:** Implemented a high-tech scientific background image (`header_bg.png`) with a sophisticated dark linear-gradient overlay. Added a modern SVG decorative curve transition and enhanced typography with glowing text shadows.
-   **Footer Overhaul:** Implemented a matching background image (`footer_bg.png`) and inverted decorative curve to provide a cohesive "bookend" aesthetic to the application.
-   **Files Modified:** `app.R`.

### ✅ **Enhancement: Comprehensive Statistical Interpretation**
-   **Issue:** The ANOVA results were technically accurate but lacked intuitive explanations for non-statisticians.
-   **UI Integration:** Added dedicated "Statistical Hypothesis" boxes to the ANOVA tabs, explicitly stating the Null (H0) and Alternative (H1) hypotheses.
-   **Dynamic Insights:** Implemented color-coded alert boxes (`renderUI`) that analyze p-values in real-time to provide written interpretations (e.g., "Result: Statistically Significant - Reject H0").
-   **Robustness:** Fixed "argument is of length zero" crashes by adding defensive checks for cases with zero degrees of freedom (missing replicates).
-   **Report Expansion:** Updated both `facs_report.Rmd` and `generic_report.Rmd` to include these hypotheses and interpretations, ensuring that downloaded PDF/HTML reports are self-explanatory and publication-ready.
-   **Files Modified:** `app.R`, `facs_report.Rmd`, `generic_report.Rmd`.

### ✅ **Scientific Validation: Methodology Review**
-   **Verification:** Confirmed that the application's core logic strictly adheres to the gold-standard dose-response methodology described in **Ritz et al. (2015) (PLoS One)**. This includes the use of `drc` package's `drm()` with LL.4 modeling, ED-based IC50 estimation via the Delta method, and robust fallback strategies for non-standard curves.

## 10. Precision Absolute Quantification & High-Performance Gating (Session 2026-01-03 - Next Day)

**Objective:** To enable absolute cell quantification using internal counting beads and optimize the application's performance for large clinical datasets.

### ✅ 1. Absolute Cell Counting Workflow (Precision Count Beads™)
-   **New UI Module:** Added a dedicated "Absolute Cell Counting" section to the sidebar. Users can now input bead concentration (beads/µL), bead volume (µL), and sample volume (µL).
-   **Flexible Channel Selection:** Implemented dynamic channel discovery. The app scans uploaded files and populates a dropdown with all available fluorescence channels (e.g., BL1-A, YL1-A) for bead identification.
-   **Step 0: Mandatory Bead Gating:** Introduced a mandatory initial gating step if absolute counting is enabled. This ensures beads are identified and excluded from all subsequent cell-related gates (Debris, Singlets, etc.) to prevent contamination.
-   **Quantification Math:** Implemented the volumetric correction formula: $C_{cells} = (N_{cells} / N_{beads}) \times (V_{beads} / V_{sample}) \times C_{beads}$.
-   **Dual potency Reporting:** The app now calculates and fits curves for both traditional Phenotypic Viability (%) and the new Absolute Survival (%) relative to the control.

### ✅ 2. Performance & Stability Optimization
-   **Instant Gating feedback:** Refactored all gating plots and counters to use a 10,000-point sampled view. This makes adding points to polygons near-instantaneous even for files with millions of events.
-   **Atomic Data Assignment:** Fixed UI flickering and race conditions by refactoring the central data loading observer to assign gating data lists atomically.
-   **Reactivity Guards:** Added `req()` guards to stop background rendering of inactive gating steps, resolving "differing number of rows" errors during transitions.
-   **Memory Safety:** Switched from `dplyr::filter` to standard R indexing in high-frequency reactive blocks to prevent scoping errors and fatal R session crashes.

### ✅ 3. Professional Scientific Visualization Refinement
-   **Publication-Quality Axes:** Updated the global `theme_publication` to rotate X-axis labels by 45 degrees, preventing "cramped" numbering for complex concentration ranges.
-   **Pretty Scientific Notation:** Implemented a unified labeling system using `bquote()` to replace "3e-04" technical labels with mathematical notation (e.g., $3 \times 10^{-4}$ or $10^{3}$). 
-   **Robust Plot Rendering:** Added aggressive finite-value filtering (`is.finite`) to all plot functions. This prevents the "Problem while computing aesthetics" error during report generation by ensuring that artifacts like zero bead counts don't break the visualization engine.

### ✅ 4. Dataset Automation & Preparation
-   **Batch Renaming Utility:** Provided a robust PowerShell/R script to convert raw clinical nomenclatures (e.g., `Third Run..._Ly1 10 nM 1.fcs`) into the standardized `CellLine_Conc_Rep.fcs` format required by the app.
-   **Units Conversion:** Automated the conversion of `nM` labels to decimal `uM` values to ensure perfect compatibility with the app's concentration detector.
-   **Standardization:** Mapped various naming aliases (e.g., N1 vs NUDUL-1) to a single consistent identity.

- **Files Modified:** `app.R`, `generic_analysis.R`, `facs_report.Rmd`, `generic_report.Rmd`.

## 11. Full Application Polish & Robustness (Session 2026-01-04)

**Objective:** To refine the user interface, enhance scientific visualizations, and ensure complete stability across both Apoptosis and Generic workflows.

### ✅ 1. Scientific Branding & Visual Polish
-   **Color Palette Update:** Replaced pure black (`#000000`) with a professional dark grey (`#333333`) across all plots, color palettes (Okabe-Ito), and UI text elements for a softer, modern aesthetic.
-   **Compensation Matrix UI:** Replaced raw text prints with professional, styled HTML tables using `kableExtra` in the Compensation and Gating tabs.
-   **Rich Gating Feedback:** Enhanced the Annexin V / PI statistics display with color-coded alert boxes (Green for good viability, Yellow/Red for deviations).
-   **Transparency:** Added clear "Sampled View" disclaimers to the gating stats, reassuring users that the 10,000-point view is for speed while final analysis uses 100% of events.

### ✅ 2. Advanced Potency & Therapeutic Window Visuals
-   **Dumbbell Plot Implementation:** Replaced the cluttered bar charts in the "IC50 vs LD50" comparison with a sophisticated Dumbbell/Dot plot. This clearly visualizes the "Therapeutic Window" by connecting potency (IC50) and toxicity (LD50) points.
-   **Collision Prevention:** Integrated `ggrepel` to automatically move data labels apart when IC50 and LD50 values are close, ensuring text remains readable.
-   **Label Formatting:** Implemented a mathematical parser to display plot labels in standard scientific notation (e.g., $3.5 \times 10^{-3}$) instead of computer `e` notation.

### ✅ 3. Absolute Quantification (Bead QC) Expansion
-   **Generic Mode Parity:** Fully mirrored the Bead Counting workflow from Apoptosis into the Generic mode.
-   **Bead QC Dashboard:** Added "Bead Stability" (pipetting consistency) and "Count Impact" (Raw vs. Absolute) plots to the Generic Plots tab.
-   **Integrated Results:** Merged traditional IC50/LD50 and Bead-Normalized Absolute IC50/LD50 into a single, unified results table.
-   **Absolute LD50:** Added statistical modeling and curve fitting for Absolute Cell Death concentration.

### ✅ 4. Report Excellence & Formatting
-   **HTML/PDF Symbol Fix:** Resolved rendering issues where HTML entities like `&#10003;` failed to appear in exported reports; implemented direct Unicode symbol support.
-   **Strict Decimal Control:** Enforced a universal 2-decimal rounding rule for percentages and a zero-decimal rule for large absolute counts to eliminate visual "noise."
-   **Scientific Notation in Tables:** Updated results tables to use HTML superscript notation ($10^{-3}$) for professional clinical reporting.
-   **Visual Synchronization:** Ensured that the "Excluded Samples" (red/strikethrough) styling in the QC tables is perfectly mirrored in the final report.

### ✅ 5. Critical Stability & Logic Fixes
-   **Gating Loop Fix:** Corrected a bug where the app forced Step 1 (FSC/SSC) for the second cell line, skipping Step 0 (Bead Gating). The loop now correctly resets to the start for all samples.
-   **ZIP Download Resolution:** Fixed a "Row_ID not found" crash during report generation caused by a syntax error in the R Markdown template.
-   **ANOVA Robustness:** Added defensive checks to the ANOVA table formatter to prevent crashes when degrees of freedom are insufficient for p-value calculation.
-   **Axis Alignment:** Fixed a "cramped axis" issue in the nanomolar range by switching to a 90-degree label rotation for all dose-response figures.

- **Files Modified:** `app.R`, `generic_analysis.R`, `facs_report.Rmd`, `generic_report.Rmd`, `session_report.md`.

## 12. Clinical Dataset Optimization & Reporting Excellence (Session 2026-01-05)

**Objective:** To resolve critical workflow bugs in multi-sample datasets, enhance statistical formatting for publication, and ensure high-resolution report stability on cloud platforms.

### ✅ 1. Multi-Cell Line Workflow Stabilization
-   **Fix: Gating Step Reset:** Corrected a logic error where the application forced the second cell line into FSC/SSC gating, skipping the bead identification step. The gating loop now correctly resets to **Step 0: Bead Gating** for every cell line when absolute counting is enabled.
-   **Fix: Re-gating Consistency:** Updated the "Edit Gate" functionality to ensure that re-gating also correctly respects the bead gating requirement if active.

### ✅ 2. Reporting & Visualization Integrity
-   **Fix: TableGrob vs. Figure Rendering:** Resolved an issue where reports displayed text descriptions (e.g., `TableGrob (2 x 3)`) instead of plots. Switched from `print()` to **`grid::grid.draw()`** for all combined gating grobs to ensure images are correctly captured in the final HTML.
-   **Fix: High-Resolution Scaling:** Significantly increased report canvas dimensions to **14" x 12"** for faceted quality control plots. This prevents distortion and overlapping labels in large datasets.
-   **Fix: Report YAML Declarations:** Declared missing parameters (like `gate_review_plots`) in the R Markdown YAML headers to prevent rendering crashes during ZIP generation.
-   **Optimization: Granular Gating Strategy:** Replaced redundant combined grids in reports with individual, stage-by-stage loops for every gate (Beads, FSC/SSC, Singlets, and Channel Histograms). This provides a clearer audit trail while preserving the 3-column combined grid in the UI for dashboard efficiency.

### ✅ 3. Scientific Formatting & Magnitude Correction
-   **Mathematical EC50 Tables:** Implemented an HTML superscript formatter for the Generic EC50 results. Values like `4.4e-03` are now displayed as **$4.40 \times 10^{-3}$**, matching publication standards.
-   **Bead Concentration Accuracy:** Corrected the default bead stock concentration to **1,000 beads/µL** (matching BioLegend standards) and removed misleading magnitude "tips" from the sidebar.
-   **Numerical Precision:** Enforced a universal **2-decimal rounding rule** for all percentages and CV values in report tables to eliminate visual noise.

### ✅ 4. Deployment & Infrastructure
-   **Docker Caching Optimization:** Refactored the `Dockerfile` to use a global `COPY . ./` command. This maximizes build speed by caching R package installations and simplifies asset management for the `www` folder.
-   **Asset Integration:** Updated the build process to include custom scientific branding assets from the `www` directory.
-   **Documentation Overhaul:** Updated the project `readme.md` with FlowPath Dynamics branding and created a clean, non-emoji release note for version 3.1.

### ✅ 5. Data Integrity & Logic Verification
-   **Identical IC50/LD50:** Verified the mathematical consistency of the "Viability" vs "Death" curves. Confirmed that identical values are a sign of mathematical precision given the inverse set definitions ($Death = 100\% - Viability$).
-   **Appendix Name Collision:** Fixed a critical "duplicate names" error in report generation by uniquely prefixing population columns with their metric types (% vs Abs.).

- **Files Modified:** `app.R`, `generic_analysis.R`, `facs_report.Rmd`, `generic_report.Rmd`, `Dockerfile`, `readme.md`, `session_report.md`.
## 13. Compensation Overhaul & Critical UX Fixes (Session 2026-01-06)

**Objective:** To resolve critical data visualization artifacts ("corner data"), improve workflow efficiency, and stabilize reporting for absolute quantification.

### ✅ 1. Compensation Logic & Visualization Overhaul
-   **Fix: Smart Positive Detection:** Completely rewrote the compensation calculation engine. Previously, it used the median of the *entire* single-stained control file, which underestimated signal if negative events were present.
    -   **New Logic:** The app now dynamically identifies the positive population by filtering events brighter than the **99th percentile** of the unstained control. This ensures accurate spillover coefficients and prevents over-compensation.
    -   **Enhancement:** Added `clean_ff` function to remove debris (low FSC-A events) before calculation, further improving accuracy.
-   **Fix: "Squashed" Plots:** Switched the Annexin V / PI gating visualization from standard logicleTransform() to **estimateLogicle()**. This data-driven scaling properly handles compensated values (negative tails), restoring the natural "cloud" visualization.
-   **Fix: Visualization Scaling:** Applied `scales::pseudo_log_trans` to the compensation preview scatter plots to prevent visual artifacts around zero/negative values.
-   **UI Clarity:** Corrected the Compensation tab label from "Inverse Matrix" to **"Spillover Matrix"** to accurately reflect the positive off-diagonal values being displayed.

### ✅ 2. Workflow & UX Enhancements
-   **Auto-Close Polygons:** Removed the need to manually click "Close Polygon". Pressing any "Next Step" button now automatically closes open polygons and advances the workflow.
-   **Bead Gating Flexibility:** Added **SSC-A** to the bead channel dropdown. Implemented specific logic to use **Linear Scaling** for SSC channels (preserving morphology) while keeping Logicle scaling for fluorescence channels.

### ✅ 3. Reporting & Visualization Integrity
-   **Feature: Combined Gating Strategy:** Updated both `facs_report.Rmd` and `generic_report.Rmd` to include a consolidated "Gating Strategy Overview" section using robust `grid::grid.newpage()` and `grid::grid.draw()` calls. This prevents plot overlapping and ensures high-quality rendering.
-   **Fix: Nanomolar Axis Collision:** Resolved label overlapping in dose-response plots by updating the pseudo-log offset from 0.1 to **0.001** and adding padding to the "0" label. This separates the zero point from low concentration values (10^-3).

### ✅ 4. Critical Stability Fixes
-   **Crash Fix: Absolute IC50:** Resolved a fatal error in download_zip. The app now checks if curve fitting produced valid rows (`nrow > 0`) before attempting to rename columns or format text, preventing crashes when absolute quantification fails.
-   **Fix: ZIP Creation Error:** Fixed a crash caused by `NA` values in confidence intervals by hardening the `fmt_ci_report` function.

### ✅ **User Confirmation**
-   **Test Run:** User confirmed "great now it is better" after the compensation and visualization fixes. Console logs verified successful identification of positive populations (e.g., ~20% positive events) and correct handling of non-finite values.

## 14. Quality Control & Experimental Validation Features (Session 2026-01-06)

**Objective:** To provide proactive warnings for "failed experiments" where control samples show high cell death, preventing misleading normalized results.

### ✅ 1. Low Control Viability Alert
-   **Logic:** The Dynamic Analysis Engine now calculates the mean viability of the untreated control (0 µM) *before* any normalization is applied.
-   **Threshold:** If control viability falls below **70%**, the system flags the cell line as a potential experimental failure.
-   **UI Warning:** A prominent **Red Alert Box** appears in the Results tab, listing the specific cell lines and their raw viability scores (e.g., "Ly18: 20.5% Viability").
-   **Advisory:** The alert explicitly warns the user **NOT to enable normalization**, as doing so would reset these low values to 100%, masking the experimental failure in the dose-response curves.

## 15. Compensation Logic Refinement & QC (Session 2026-01-07)

**Objective:** To further refine the compensation logic and visualization to address user feedback regarding "squashed" plots and diagonal artifacts, ensuring professional-grade data presentation.

### ✅ 1. Compensation Visualization Overhaul
-   **Switch to ArcSinh:** Replaced `estimateLogicle` and `pseudo_log_trans` with the **ArcSinh (Inverse Hyperbolic Sine)** transformation (cofactor 150) for the Compensation Preview.
-   **Reason:** ArcSinh robustly handles negative values generated by compensation without the parameter sensitivity of Logicle, successfully expanding "squashed" lines into visible "clouds".
-   **Implementation:** Updated `app.R` to apply `asinh(x/150)` to visualization data and calculate axis breaks accordingly.
-   **Fix:** Resolved a scoping error (`object 'label_unstained' not found`) by correctly positioning variable definitions.

### ✅ 2. Aggressive Spillover Calculation
-   **Peak Detection:** Modified the compensation engine to calculate spillover coefficients using only the **Top 25%** of positive events (instead of the entire positive population).
-   **Reason:** This prevents "dim" or "trailing" events from artificially lowering the median signal, which was causing under-compensation (visible as diagonal "banana" shapes).
-   **Result:** More accurate spillover matrices that effectively orthogonalize highly overlapping fluorophores.

### ✅ 3. Ongoing Compensation Logic Optimization
-   **Status:** `in-progress`
-   **Context:** User feedback ("we are not finished") indicates further tuning may be required to perfect the compensation logic for all datasets.
