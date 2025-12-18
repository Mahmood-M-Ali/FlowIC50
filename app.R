# ============================================================================
# FACS IC50 ANALYSIS - ENHANCED VERSION
# ============================================================================

if (!requireNamespace("BiocManager", quietly = TRUE)) {
  install.packages("BiocManager")
}
if (!requireNamespace("flowCore", quietly = TRUE)) {
  BiocManager::install("flowCore", update = FALSE, ask = FALSE)
}

required_packages <- c(
  "shiny", "bslib", "ggplot2", "dplyr", "tidyr",
  "readr", "stringr", "drc", "scales", "sp", "zip", "svglite",
  "outliers", "DescTools", "pheatmap", "gridExtra", "rmarkdown", "kableExtra"
)
for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
}

suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(flowCore)
  library(ggplot2)
  library(dplyr)
  library(tidyr)
  library(readr)
  library(stringr)
  library(drc)
  library(scales)
  library(sp)
  library(zip)
  library(svglite)
  library(outliers)
  library(DescTools)
  library(pheatmap)
  library(gridExtra)
  library(rmarkdown)
  library(kableExtra)
})

# ============================================================================
# OKABE-ITO COLORBLIND-FRIENDLY PALETTE
# ============================================================================
okabe_ito_colors <- c(
  "#000000",  # Black
  "#E69F00",  # Orange
  "#56B4E9",  # Sky Blue
  "#009E73",  # Bluish Green
  "#F0E442",  # Yellow
  "#0072B2",  # Blue
  "#D55E00",  # Vermillion
  "#CC79A7",  # Reddish Purple
  "#999999"   # Gray
)

get_color_palette <- function(n) {
  if(n <= length(okabe_ito_colors)) {
    return(okabe_ito_colors[1:n])
  } else {
    return(colorRampPalette(okabe_ito_colors)(n))
  }
}

# ============================================================================
# PUBLICATION THEME
# ============================================================================
theme_publication <- function() {
  theme_bw(base_size = 11) +
    theme(
      panel.grid.major = element_line(color = "gray90", linewidth = 0.3),
      panel.grid.minor = element_blank(),
      axis.line = element_line(color = "black", linewidth = 0.5),
      axis.ticks = element_line(color = "black", linewidth = 0.5),
      axis.text = element_text(color = "black", size = 10),
      axis.title = element_text(face = "bold", size = 11),
      plot.title = element_text(face = "bold", hjust = 0.5, size = 12),
      plot.subtitle = element_text(hjust = 0.5, size = 10, color = "gray30"),
      legend.background = element_rect(fill = "white", color = "black"),
      legend.key = element_blank(),
      strip.background = element_rect(fill = "gray95", color = "black"),
      strip.text = element_text(face = "bold")
    )
}

# ============================================================================
# UI
# ============================================================================

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      html, body { height: 100%; margin: 0; padding: 0; }
      body { display: flex; flex-direction: column; min-height: 100vh; }
      .container-fluid { flex: 1; padding: 0 !important; max-width: 100% !important; }
      .gradient-header {
        background: linear-gradient(135deg, #1e3a8a 0%, #0f172a 100%);
        color: white; padding: 50px 20px; text-align: center; width: 100%;
        box-shadow: 0 4px 6px rgba(0,0,0,0.1);
      }
      .gradient-header h1 { font-size: 3em; font-weight: 700; margin: 0 0 15px 0; letter-spacing: 1px; }
      .gradient-header p { font-size: 1.2em; margin: 0; opacity: 0.95; font-weight: 300; }
      .content-wrapper { padding: 30px; max-width: 1800px; margin: 0 auto; width: 100%; }
      .footer {
        background: linear-gradient(135deg, #1e3a8a 0%, #0f172a 100%);
        color: white; padding: 30px 20px; text-align: center; margin-top: auto;
        box-shadow: 0 -2px 10px rgba(0,0,0,0.1); width: 100%;
      }
      .footer p { margin: 8px 0; font-size: 1em; }
      .footer .social-links {
        margin: 15px 0; display: flex; justify-content: center; gap: 20px;
        align-items: center; flex-wrap: wrap;
      }
      .footer .social-links a {
        display: inline-flex; align-items: center; gap: 8px; color: white;
        text-decoration: none; transition: all 0.3s; padding: 8px 15px;
        border-radius: 5px; background: rgba(255,255,255,0.1); font-size: 0.95em;
      }
      .footer .social-links a:hover {
        background: rgba(255,255,255,0.2); transform: translateY(-2px);
      }
      .gate-mode-switch {
        background: #f8f9fa; padding: 15px; border-radius: 8px;
        margin-bottom: 15px; border: 1px solid #dee2e6;
      }
      .alert-custom { border-radius: 8px; padding: 15px; margin-bottom: 15px; }
      .sidebar { background: #f8f9fa; border-right: 2px solid #dee2e6; padding: 20px; }
      .file-format-box {
        background: linear-gradient(135deg, #e3f2fd 0%, #bbdefb 100%);
        padding: 15px; border-radius: 8px; border-left: 5px solid #2196F3; margin: 10px 0;
      }
      .file-format-box h6 { margin: 0 0 8px 0; font-weight: 600; color: #1565c0; }
      .file-format-box p { font-size: 0.9em; margin: 5px 0; color: #424242; }
      .file-format-box code {
        background: rgba(255,255,255,0.95); padding: 6px 10px; border-radius: 4px;
        font-size: 0.85em; display: block; margin: 8px 0; color: #d32f2f;
        font-family: 'Courier New', monospace; font-weight: 600;
        word-wrap: break-word; white-space: pre-wrap; overflow-wrap: break-word;
      }
      .file-format-box .example { font-size: 0.8em; margin: 5px 0; font-style: italic; color: #616161; }
      .comp-box {
        background: linear-gradient(135deg, #fff3e0 0%, #ffe0b2 100%);
        padding: 15px; border-radius: 8px; border-left: 5px solid #ff9800; margin: 10px 0;
      }
      .comp-box h6 { margin: 0 0 8px 0; font-weight: 600; color: #e65100; }
      .comp-box p { font-size: 0.85em; margin: 5px 0; color: #424242; }
      .naming-tool-box {
        background: linear-gradient(135deg, #fff9c4 0%, #fff59d 100%);
        padding: 15px;
        border-radius: 8px;
        border-left: 5px solid #f57f17;
        margin-bottom: 20px;
      }
      .naming-tool-box h6 {
        margin: 0 0 10px 0;
        color: #f57f17;
        font-weight: 600;
      }
      .naming-tool-box p {
        font-size: 0.9em;
        margin: 5px 0;
        color: #424242;
      }
    "))
  ),
  
  div(class = "gradient-header",
      h1("Flow Cytometry Dose-Response Analyzer"),
      p("Automated apoptosis analysis with quality control and pharmacodynamic profiling")),
  
  div(class = "content-wrapper",
      fluidRow(
        column(3, class = "sidebar",
               div(style = "background: linear-gradient(135deg, #fff9c4 0%, #fff59d 100%);
                   padding: 15px; border-radius: 8px; border-left: 5px solid #f57f17;
                   margin-bottom: 20px;",
                   h6(style = "margin: 0 0 10px 0; color: #f57f17; font-weight: 600;",
                      icon("tools"), " Need to Rename Files?"),
                   p(style = "font-size: 0.9em; margin: 5px 0; color: #424242;",
                     "Use our automated file renaming tool to quickly format your FCS files to match the required naming convention."),
                   tags$a(href = "https://huggingface.co/spaces/mahmood-iab/file_naming",
                          target = "_blank",
                          class = "btn btn-warning btn-sm btn-block",
                          style = "margin-top: 10px; font-weight: 600;",
                          icon("external-link-alt"), " Open File Naming Tool")
               ),
               
               h4(icon("upload"), " Upload Files"),
               fileInput("files", "Select .fcs files:", multiple = TRUE, accept = c(".fcs", ".FCS")),
               
               hr(),
               div(class = "file-format-box",
                   h6(icon("info-circle"), " File Naming Convention"),
                   p("Files must follow this exact format:"),
                   tags$code("CellLine_Treatment_ConcentrationuM_Rep.fcs"),
                   p(class = "example", "Example: Ly18_YF2_50uM_Rep1.fcs")),
               hr(),
               h5(icon("adjust"), " Compensation (Optional)"),
               div(class = "comp-box",
                   h6("Fluorescence Compensation Controls"),
                   p("Upload single-stained controls to calculate compensation matrix:"),
                   fileInput("comp_unstained", "Unstained control:", accept = c(".fcs", ".FCS")),
                   fileInput("comp_annexin", "Annexin V-FITC only:", accept = c(".fcs", ".FCS")),
                   fileInput("comp_pi", "PI only:", accept = c(".fcs", ".FCS")),
                   actionButton("calc_comp", "Calculate Compensation Matrix",
                                class = "btn-info btn-sm btn-block", icon = icon("calculator")),
                   uiOutput("comp_status_ui")
               ),
               hr(),
               uiOutput("control_selection_ui"),
               hr(),
               div(style = "background: #e8f5e9; padding: 15px; border-radius: 8px; border-left: 5px solid #4caf50; margin: 10px 0;",
                   h6(icon("cog"), " Analysis Options", style = "margin-top: 0; color: #2e7d32;"),
                   checkboxInput("use_normalization", "Normalize viability to control (100%)", value = FALSE),
                   p(style = "font-size: 0.85em; color: #666; margin: 5px 0;",
                     "When enabled, control samples will be set to 100% viability.")
               ),
               hr(),
               uiOutput("workflow_status"),
               hr(),
               uiOutput("analyze_button_ui"),
               uiOutput("download_zip_ui")
        ),
        
        column(9,
               tabsetPanel(id = "tabs",
                           tabPanel("Files",
                                    icon = icon("table"),
                                    br(),
                                    h4("Uploaded Files"),
                                    tableOutput("file_table")),
                           
                           tabPanel("Compensation",
                                    icon = icon("adjust"),
                                    br(),
                                    uiOutput("comp_matrix_ui")),
                           
                           tabPanel("Gating",
                                    icon = icon("sliders-h"),
                                    br(),
                                    uiOutput("gating_ui")),
                           
                           tabPanel("Gate Review",
                                    icon = icon("eye"),
                                    br(),
                                    h4("Review Saved Gates"),
                                    uiOutput("gate_review_ui")),
                           
                           tabPanel("Quality Control",
                                    icon = icon("check-circle"),
                                    br(),
                                    h4("Replicate Quality Control"),
                                    uiOutput("qc_ui")),
                           
                           tabPanel("Results",
                                    icon = icon("chart-line"),
                                    br(),
                                    h4("IC50/LD50 Results with Extended Metrics"),
                                    tableOutput("ic50_table"),
                                    hr(),
                                    h5("Statistical Comparisons"),
                                    verbatimTextOutput("stats_comparison"),
                                    hr(),
                                    downloadButton("download_results", "Download CSV", class = "btn-primary")),
                           
                           tabPanel("Advanced Metrics",
                                    icon = icon("calculator"),
                                    br(),
                                    h4("Extended Dose-Response Metrics"),
                                    tableOutput("advanced_metrics_table")),
                           
                           tabPanel("Plots",
                                    icon = icon("chart-bar"),
                                    br(),
                                    h4("Viability Dose-Response Curves"),
                                    plotOutput("viability_plot", height = "600px"),
                                    hr(),
                                    h4("Cell Death Dose-Response Curves"),
                                    plotOutput("death_plot", height = "600px"),
                                    hr(),
                                    h4("IC50 vs LD50 Comparison"),
                                    plotOutput("ic50_ld50_comparison", height = "500px"),
                                    hr(),
                                    h4("Apoptosis Quadrant Breakdown"),
                                    plotOutput("quadrant_plot", height = "700px"))
               )
        )
      )
  ),
  
  div(class = "footer",
      p(strong("Developed by Mahmood Mohammed Ali")),
      p("Université Grenoble Alpes | Institute for Advanced Biosciences | Epigenetics of Regeneration and Cancer Group"),
      p("mahmood.mohammed-ali@univ-grenoble-alpes.fr"),
      div(class = "social-links",
          tags$a(href = "mailto:mahmood.mohammed-ali@univ-grenoble-alpes.fr",
                 target = "_blank",
                 icon("envelope"), " Email"),
          tags$a(href = "https://github.com/Mahmood-M-Ali",
                 target = "_blank",
                 icon("github"), " GitHub"),
          tags$a(href = "https://www.linkedin.com/in/mahmood-mohammed-ali-20334b205/",
                 target = "_blank",
                 icon("linkedin"), " LinkedIn")
      ),
      div(style = "margin-top: 20px;",
          tags$a(href = "https://creativecommons.org/licenses/by-nc/4.0/",
                 target = "_blank",
                 tags$img(src = "https://i.creativecommons.org/l/by-nc/4.0/88x31.png",
                          alt = "CC BY-NC 4.0 License",
                          style = "border: 0;"))
      ),
      div(style = "margin-top: 10px;",
          tags$a(href = "https://doi.org/10.5281/zenodo.17872796",
                 target = "_blank",
                 tags$img(src = "https://zenodo.org/badge/DOI/10.5281/zenodo.17872796.svg",
                          alt = "DOI: 10.5281/zenodo.17872796",
                          style = "border: 0;"))
      )
  )
)
# ============================================================================
# SERVER
# ============================================================================

server <- function(input, output, session) {
  
  rv <- reactiveValues(
    metadata = NULL,
    thresholds = NULL,
    results = NULL,
    ic50_results = NULL,
    ld50_results = NULL,
    viability_data = NULL,
    death_data = NULL,
    predicted_viability_curves = NULL,
    predicted_death_curves = NULL,
    current_cell_line_index = 1,
    gating_step = "fsc_ssc",
    polygon_points = data.frame(x = numeric(), y = numeric()),
    temp_fsc_ssc_gates = NULL,
    temp_singlet_gates = NULL,
    temp_gated_data_cells = NULL,
    temp_gated_data_singlets = NULL,
    control_concentration = NULL,
    comp_matrix = NULL,
    comp_calculated = FALSE,
    viability_plot_obj = NULL,
    death_plot_obj = NULL,
    comparison_plot_obj = NULL,
    quadrant_plot_obj = NULL,
    # NEW: Enhanced metrics
    qc_data = NULL,
    outlier_flags = NULL,
    cell_counts = NULL,
    advanced_metrics = NULL,
    fit_quality = NULL,
    gate_plots = list(),
    comp_before_after = NULL
  )
  
  # === COMPENSATION CALCULATION (UNCHANGED) ===
  observeEvent(input$calc_comp, {
    req(input$comp_unstained, input$comp_annexin, input$comp_pi)
    
    withProgress(message = "Calculating compensation...", {
      tryCatch({
        unstained <- read.FCS(input$comp_unstained$datapath)
        annexin_only <- read.FCS(input$comp_annexin$datapath)
        pi_only <- read.FCS(input$comp_pi$datapath)
        
        unstained_data <- exprs(unstained)
        annexin_data <- exprs(annexin_only)
        pi_data <- exprs(pi_only)
        
        bl1_unstained <- median(unstained_data[, "BL1-A"])
        bl2_unstained <- median(unstained_data[, "BL2-A"])
        
        bl1_annexin <- median(annexin_data[, "BL1-A"]) - bl1_unstained
        bl2_from_annexin <- median(annexin_data[, "BL2-A"]) - bl2_unstained
        
        bl2_pi <- median(pi_data[, "BL2-A"]) - bl2_unstained
        bl1_from_pi <- median(pi_data[, "BL1-A"]) - bl1_unstained
        
        spillover_21 <- bl2_from_annexin / bl1_annexin
        spillover_12 <- bl1_from_pi / bl2_pi
        
        spill_matrix <- matrix(c(1, spillover_12,
                                 spillover_21, 1),
                               nrow = 2, byrow = TRUE,
                               dimnames = list(c("BL1-A", "BL2-A"),
                                               c("BL1-A", "BL2-A")))
        
        rv$comp_matrix <- solve(spill_matrix)
        rv$comp_calculated <- TRUE
        
        # NEW: Store before/after data for visualization
        sample_indices <- sample(1:nrow(annexin_data), min(5000, nrow(annexin_data)))
        before_data <- annexin_data[sample_indices, c("BL1-A", "BL2-A")]
        after_data <- before_data %*% t(rv$comp_matrix)
        
        rv$comp_before_after <- list(
          before = as.data.frame(before_data),
          after = as.data.frame(after_data)
        )
        
        showNotification("Compensation matrix calculated successfully!", type = "message")
        updateTabsetPanel(session, "tabs", selected = "Compensation")
        
      }, error = function(e) {
        showNotification(paste("Compensation calculation failed:", e$message),
                         type = "error", duration = 10)
      })
    })
  })
  
  output$comp_status_ui <- renderUI({
    if(rv$comp_calculated) {
      div(class = "alert alert-success", style = "margin-top: 10px; padding: 8px; font-size: 0.9em;",
          icon("check-circle"), " Compensation matrix ready - will be applied during analysis")
    }
  })
  
  output$comp_matrix_ui <- renderUI({
    if(!rv$comp_calculated) {
      return(div(class = "alert alert-info",
                 h5(icon("info-circle"), " No Compensation Matrix"),
                 p("Upload compensation control files in the sidebar and click 'Calculate Compensation Matrix'."),
                 hr(),
                 h6("What are compensation controls?"),
                 p("Compensation corrects for spectral overlap between fluorophores:"),
                 tags$ul(
                   tags$li(strong("Unstained:"), " Cells with no fluorescent staining"),
                   tags$li(strong("Annexin V-FITC only:"), " Cells stained with Annexin V-FITC alone"),
                   tags$li(strong("PI only:"), " Cells stained with PI alone")
                 )))
    }
    
    tagList(
      div(class = "alert alert-success",
          h5(icon("check-circle"), " Compensation Matrix Calculated"),
          p(strong("Status:"), " Will be applied to all samples during analysis.")),
      h5("Inverse Compensation Matrix:"),
      verbatimTextOutput("comp_matrix_display"),
      hr(),
      # NEW: Compensation effect visualization
      h5("Compensation Effect (Before vs After)"),
      plotOutput("comp_visualization", height = "400px"),
      hr(),
      p(style = "font-size: 0.9em; color: #666;",
        icon("info-circle"), " Corrects spectral overlap between Annexin V-FITC (BL1-A) and PI (BL2-A).")
    )
  })
  
  output$comp_matrix_display <- renderPrint({
    req(rv$comp_matrix)
    print(round(rv$comp_matrix, 4))
  })
  
  # NEW: Compensation visualization
  output$comp_visualization <- renderPlot({
    req(rv$comp_before_after)
    
    before_df <- rv$comp_before_after$before
    after_df <- rv$comp_before_after$after
    colnames(before_df) <- c("BL1", "BL2")
    colnames(after_df) <- c("BL1", "BL2")
    
    before_df$Type <- "Before Compensation"
    after_df$Type <- "After Compensation"
    
    combined <- rbind(before_df, after_df)
    
    p <- ggplot(combined, aes(x = BL1, y = BL2)) +
      geom_point(alpha = 0.1, size = 0.5, color = "#2196F3") +
      facet_wrap(~ Type) +
      labs(title = "Compensation Effect on Annexin V-FITC vs PI",
           x = "BL1-A (Annexin V-FITC)", y = "BL2-A (PI)") +
      theme_publication() +
      theme(strip.text = element_text(size = 11, face = "bold"))
    rv$comp_plot_obj <- p  # store for the HTML report
    p
  })
  
  # === FILE PARSING (UNCHANGED) ===
  observeEvent(input$files, {
    req(input$files)
    
    metadata <- input$files
    metadata$cell_line <- character(nrow(metadata))
    metadata$replicate <- character(nrow(metadata))
    metadata$treatment_full <- character(nrow(metadata))
    metadata$concentration_uM <- numeric(nrow(metadata))
    
    for(i in 1:nrow(metadata)) {
      name_clean <- str_replace(metadata$name[i], "\\.[fF][cC][sS]$", "")
      parts <- str_split(name_clean, "_")[[1]]
      
      if(length(parts) >= 3) {
        metadata$cell_line[i] <- parts[1]
        metadata$replicate[i] <- parts[length(parts)]
        metadata$treatment_full[i] <- paste(parts[2:(length(parts)-1)], collapse = "_")
        
        conc <- as.numeric(str_extract(metadata$treatment_full[i], "[0-9.]+(?=[uU][mM])"))
        metadata$concentration_uM[i] <- ifelse(is.na(conc), 0, conc)
      } else {
        metadata$cell_line[i] <- NA_character_
      }
    }
    
    metadata <- metadata %>% filter(!is.na(cell_line))
    
    if(nrow(metadata) == 0) {
      showNotification("No valid files found", type = "error")
      return()
    }
    
    rv$metadata <- metadata
    rv$current_cell_line_index <- 1
    rv$gating_step <- "fsc_ssc"
    rv$thresholds <- list()
    rv$control_concentration <- NULL
    
    showNotification(paste("Loaded", nrow(metadata), "files"), type = "message")
  })
  
  # === CONTROL SELECTION UI (UNCHANGED) ===
  output$control_selection_ui <- renderUI({
    req(rv$metadata)
    
    if(is.null(rv$control_concentration)) {
      conc_options <- sort(unique(rv$metadata$concentration_uM))
      
      return(div(
        style = "background: #fff8e1; padding: 15px; border-radius: 8px; border-left: 5px solid #ff9800;",
        h6(icon("flask"), " Select Control", style = "margin-top: 0; color: #e65100;"),
        p("Which concentration represents your control samples?",
          style = "font-size: 0.9em; margin-bottom: 10px;"),
        selectInput("control_conc", "Control Concentration (µM):",
                    choices = conc_options,
                    selected = 0),
        actionButton("set_control", "Set Control & Start Gating",
                     class = "btn-primary btn-block",
                     icon = icon("check"))
      ))
    } else {
      return(div(
        style = "background: #e8f5e9; padding: 15px; border-radius: 8px; border-left: 5px solid #4caf50;",
        h6(icon("check-circle"), " Control Set", style = "margin-top: 0; color: #2e7d32;"),
        p(paste("Control:", rv$control_concentration, "µM"),
          style = "font-size: 0.9em; margin: 0;")
      ))
    }
  })
  
  observeEvent(input$set_control, {
    req(input$control_conc)
    rv$control_concentration <- as.numeric(input$control_conc)
    updateTabsetPanel(session, "tabs", selected = "Gating")
    showNotification(paste("Control set to", rv$control_concentration, "µM"), type = "message")
  })
  
  # === WORKFLOW STATUS (UNCHANGED) ===
  output$workflow_status <- renderUI({
    if(is.null(rv$metadata)) {
      return(div(class = "alert alert-info alert-custom",
                 icon("info-circle"), " Upload .fcs files to begin"))
    }
    
    if(is.null(rv$control_concentration)) {
      return(NULL)
    }
    
    cell_lines <- unique(rv$metadata$cell_line)
    n_gated <- length(rv$thresholds)
    
    if(n_gated == length(cell_lines)) {
      return(div(class = "alert alert-success alert-custom",
                 icon("check-circle"), " All cell lines gated! Ready to analyze."))
    } else {
      return(div(class = "alert alert-warning alert-custom",
                 sprintf("%s Progress: %d/%d cell lines gated",
                         icon("hourglass-half"), n_gated, length(cell_lines))))
    }
  })
  
  output$analyze_button_ui <- renderUI({
    req(rv$metadata, rv$control_concentration)
    
    cell_lines <- unique(rv$metadata$cell_line)
    n_gated <- length(rv$thresholds)
    
    if(n_gated == length(cell_lines)) {
      return(actionButton("analyze", "Start Analysis",
                          icon = icon("play"),
                          class = "btn-success btn-lg btn-block",
                          style = "font-size: 1.1em; padding: 12px;"))
    } else {
      return(actionButton("analyze_disabled", "Complete Gating First",
                          icon = icon("lock"),
                          class = "btn-secondary btn-lg btn-block",
                          disabled = TRUE,
                          style = "font-size: 1.1em; padding: 12px;"))
    }
  })
  
  output$file_table <- renderTable({
    req(rv$metadata)
    rv$metadata %>%
      group_by(cell_line, concentration_uM) %>%
      summarise(n_files = n(), .groups = "drop") %>%
      arrange(cell_line, concentration_uM)
  }, striped = TRUE, hover = TRUE, bordered = TRUE)
  
  # NEW: Gate Review UI
  output$gate_review_ui <- renderUI({
    req(rv$thresholds)
    
    if(length(rv$thresholds) == 0) {
      return(div(class = "alert alert-info",
                 h5(icon("info-circle"), " No Gates Saved Yet"),
                 p("Complete gating for at least one cell line to review gates here.")))
    }
    
    cell_lines <- names(rv$thresholds)
    
    tagList(
      p(strong("Saved gates for ", length(cell_lines), " cell line(s)")),
      lapply(cell_lines, function(line) {
        div(style = "background: #f8f9fa; padding: 15px; margin: 10px 0; border-radius: 8px; border-left: 4px solid #2196F3;",
            h5(icon("circle"), " ", line),
            p(strong("FSC/SSC Gate:"), " Polygon with ", 
              nrow(rv$thresholds[[line]]$fsc_ssc$polygon), " points"),
            p(strong("Singlet Gate:"), " Polygon with ", 
              nrow(rv$thresholds[[line]]$singlets$polygon), " points"),
            p(strong("Annexin/PI Thresholds:"), " Annexin=", 
              round(rv$thresholds[[line]]$annexin_pi$annexin, 2), 
              ", PI=", round(rv$thresholds[[line]]$annexin_pi$pi, 2)),
            actionButton(paste0("edit_gate_", line), "Re-gate This Cell Line", 
                         class = "btn-warning btn-sm", 
                         icon = icon("edit")),
            hr(style = "margin: 10px 0;"),
            # Gate visualization plots
            plotOutput(paste0("gate_vis_", line), height = "300px")
        )
      })
    )
  })
  
  # Dynamic plot outputs for gate visualization
  observe({
    req(rv$thresholds)
    cell_lines <- names(rv$thresholds)
    
    lapply(cell_lines, function(line) {
      local({
        line_local <- line
        output[[paste0("gate_vis_", line_local)]] <- renderPlot({
          req(rv$gate_plots[[line_local]])
          rv$gate_plots[[line_local]]
        })
      })
    })
  })
  
  # Re-gating functionality
  observe({
    req(rv$thresholds)
    cell_lines <- names(rv$thresholds)
    
    lapply(cell_lines, function(line) {
      local({
        line_local <- line
        observeEvent(input[[paste0("edit_gate_", line_local)]], {
          cell_lines_all <- unique(rv$metadata$cell_line)
          rv$current_cell_line_index <- which(cell_lines_all == line_local)
          rv$gating_step <- "fsc_ssc"
          rv$polygon_points <- data.frame(x = numeric(), y = numeric())
          updateTabsetPanel(session, "tabs", selected = "Gating")
          showNotification(paste("Re-gating", line_local), type = "message")
        })
      })
    })
  })
  # === GATING UI (UNCHANGED LOGIC) ===
  output$gating_ui <- renderUI({
    req(rv$metadata, rv$control_concentration)
    
    cell_lines <- unique(rv$metadata$cell_line)
    
    if(rv$current_cell_line_index > length(cell_lines)) {
      return(div(class = "alert alert-success alert-custom",
                 h4(icon("check-circle"), " Gating Complete!"),
                 p("All cell lines have been gated. Go to the Results tab and click 'Start Analysis'.")
      ))
    }
    
    current_line <- cell_lines[rv$current_cell_line_index]
    
    control_file <- rv$metadata %>%
      filter(cell_line == current_line, concentration_uM == rv$control_concentration) %>%
      slice(1)
    
    if(nrow(control_file) == 0) {
      return(div(class = "alert alert-danger alert-custom",
                 h5(icon("exclamation-triangle"), " Error"),
                 p(paste("No control file found for", current_line, "at", rv$control_concentration, "µM")),
                 actionButton("skip_line", "Skip This Cell Line", class = "btn-warning")
      ))
    }
    
    # Load control file
    fs <- read.FCS(control_file$datapath)
    data_raw <- exprs(fs)
    
    # === STEP 1: FSC/SSC (POLYGON ONLY) ===
    if(rv$gating_step == "fsc_ssc") {
      fsc_data <- data_raw[, "FSC-A"]
      ssc_data <- data_raw[, "SSC-A"]
      
      rv$current_gating_data_fsc <- list(
        fsc = fsc_data,
        ssc = ssc_data,
        cell_line = current_line,
        data_raw = data_raw
      )
      
      return(tagList(
        div(class = "alert alert-info alert-custom",
            h5(icon("circle"), " Step 1/3: FSC/SSC Gate - ", strong(current_line)),
            p("Gate cells and remove debris", style = "margin-bottom: 0;")
        ),
        
        div(style = "background: #fff3cd; padding: 10px; border-radius: 5px; margin-top: 10px;",
            p(icon("mouse-pointer"), " Click on plot to add points. Close polygon when done.",
              style = "margin: 5px 0;"),
            actionButton("clear_polygon_fsc", "Clear Points",
                         class = "btn-warning btn-sm", icon = icon("eraser")),
            actionButton("close_polygon_fsc", "Close Polygon",
                         class = "btn-success btn-sm", icon = icon("check-circle"))
        ),
        
        plotOutput("fsc_ssc_plot", height = "450px", click = "fsc_ssc_click"),
        div(class = "alert alert-secondary", verbatimTextOutput("fsc_ssc_stats")),
        hr(),
        actionButton("next_to_singlets", "Next: Singlet Gate →",
                     class = "btn-primary btn-lg btn-block", icon = icon("arrow-right"))
      ))
    }
    
    # === STEP 2: SINGLETS (POLYGON ONLY) ===
    else if(rv$gating_step == "singlets") {
      req(rv$temp_gated_data_cells)
      
      data_cells <- rv$temp_gated_data_cells
      fsc_a <- data_cells[, "FSC-A"]
      fsc_h <- data_cells[, "FSC-H"]
      
      rv$current_gating_data_singlet <- list(
        fsc_a = fsc_a,
        fsc_h = fsc_h,
        cell_line = current_line,
        data_cells = data_cells
      )
      
      return(tagList(
        div(class = "alert alert-info alert-custom",
            h5(icon("circle"), " Step 2/3: Singlet Discrimination - ", strong(current_line)),
            p("Remove doublets using FSC-A vs FSC-H", style = "margin-bottom: 0;")
        ),
        
        div(style = "background: #fff3cd; padding: 10px; border-radius: 5px; margin-top: 10px;",
            p(icon("mouse-pointer"), " Click on plot to add points. Close polygon when done.",
              style = "margin: 5px 0;"),
            actionButton("clear_polygon_singlet", "Clear Points",
                         class = "btn-warning btn-sm", icon = icon("eraser")),
            actionButton("close_polygon_singlet", "Close Polygon",
                         class = "btn-success btn-sm", icon = icon("check-circle"))
        ),
        
        plotOutput("singlet_plot", height = "450px", click = "singlet_click"),
        div(class = "alert alert-secondary", verbatimTextOutput("singlet_stats")),
        hr(),
        fluidRow(
          column(6, actionButton("back_to_fsc", "← Back: FSC/SSC",
                                 class = "btn-secondary btn-lg btn-block", icon = icon("arrow-left"))),
          column(6, actionButton("next_to_annexin", "Next: Annexin/PI →",
                                 class = "btn-primary btn-lg btn-block", icon = icon("arrow-right")))
        )
      ))
    }
    
    # === STEP 3: ANNEXIN/PI (RECTANGLE ONLY) ===
    else if(rv$gating_step == "annexin_pi") {
      req(rv$temp_gated_data_singlets)
      
      data_singlets <- rv$temp_gated_data_singlets
      
      # Apply compensation if available
      trans <- logicleTransform()
      if(rv$comp_calculated) {
        fluor_data <- data_singlets[, c("BL1-A", "BL2-A")]
        comp_data <- fluor_data %*% t(rv$comp_matrix)
        annexin <- trans(comp_data[, "BL1-A"])
        pi <- trans(comp_data[, "BL2-A"])
      } else {
        annexin <- trans(data_singlets[, "BL1-A"])
        pi <- trans(data_singlets[, "BL2-A"])
      }
      
      annexin_median <- median(annexin)
      annexin_q95 <- quantile(annexin, 0.95)
      pi_median <- median(pi)
      pi_q95 <- quantile(pi, 0.95)
      
      start_annexin <- mean(c(annexin_median, annexin_q95))
      start_pi <- mean(c(pi_median, pi_q95))
      
      rv$current_gating_data <- list(
        annexin = annexin,
        pi = pi,
        cell_line = current_line
      )
      
      return(tagList(
        div(class = "alert alert-info alert-custom",
            h5(icon("circle"), " Step 3/3: Annexin V / PI - ", strong(current_line)),
            p("Set viability/apoptosis thresholds", style = "margin-bottom: 0;")
        ),
        
        fluidRow(
          column(6,
                 sliderInput("annexin_threshold", "Annexin V Threshold:",
                             min = floor(min(annexin)), max = ceiling(max(annexin)),
                             value = start_annexin, step = 0.05)
          ),
          column(6,
                 sliderInput("pi_threshold", "PI Threshold:",
                             min = floor(min(pi)), max = ceiling(max(pi)),
                             value = start_pi, step = 0.05)
          )
        ),
        
        plotOutput("annexin_plot", height = "450px"),
        div(class = "alert alert-secondary", verbatimTextOutput("annexin_stats")),
        hr(),
        fluidRow(
          column(6, actionButton("back_to_singlet", "← Back: Singlets",
                                 class = "btn-secondary btn-lg btn-block", icon = icon("arrow-left"))),
          column(6, actionButton("save_gate", "Save & Next Cell Line ✓",
                                 class = "btn-success btn-lg btn-block", icon = icon("check")))
        )
      ))
    }
  })
  
  # === PLOT OUTPUTS (UNCHANGED) ===
  
  output$fsc_ssc_plot <- renderPlot({
    req(rv$current_gating_data_fsc)
    
    df <- data.frame(
      FSC = rv$current_gating_data_fsc$fsc,
      SSC = rv$current_gating_data_fsc$ssc
    )
    if(nrow(df) > 10000) df <- df[sample(nrow(df), 10000), ]
    
    p <- ggplot(df, aes(x = FSC, y = SSC)) +
      stat_density_2d(aes(fill = after_stat(level)), geom = "polygon", alpha = 0.6) +
      geom_point(alpha = 0.1, size = 0.3, color = "black") +
      scale_fill_viridis_c(option = "viridis") +
      labs(title = paste("FSC/SSC Gate -", rv$current_gating_data_fsc$cell_line),
           x = "FSC-A", y = "SSC-A") +
      theme_bw(base_size = 14) +
      theme(legend.position = "none", plot.title = element_text(face = "bold"))
    
    # Only draw polygon if we have at least 2 points
    if(nrow(rv$polygon_points) >= 1) {
      p <- p + geom_path(data = rv$polygon_points, aes(x = x, y = y),
                         color = "red", linewidth = 2) +
        geom_point(data = rv$polygon_points, aes(x = x, y = y),
                   color = "red", size = 4, shape = 21, fill = "white", stroke = 2)
    }
    p
  })
  
  output$singlet_plot <- renderPlot({
    req(rv$current_gating_data_singlet)
    
    df <- data.frame(
      FSC_A = rv$current_gating_data_singlet$fsc_a,
      FSC_H = rv$current_gating_data_singlet$fsc_h
    )
    if(nrow(df) > 10000) df <- df[sample(nrow(df), 10000), ]
    
    p <- ggplot(df, aes(x = FSC_A, y = FSC_H)) +
      stat_density_2d(aes(fill = after_stat(level)), geom = "polygon", alpha = 0.6) +
      geom_point(alpha = 0.1, size = 0.3, color = "black") +
      geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray50", linewidth = 1) +
      scale_fill_viridis_c(option = "plasma") +
      labs(title = paste("Singlet Gate -", rv$current_gating_data_singlet$cell_line),
           subtitle = "Diagonal = singlets; above = doublets",
           x = "FSC-A", y = "FSC-H") +
      theme_bw(base_size = 14) +
      theme(legend.position = "none", plot.title = element_text(face = "bold"))
    
    # Only draw polygon if we have at least 2 points
    if(nrow(rv$polygon_points) >= 1) {
      p <- p + geom_path(data = rv$polygon_points, aes(x = x, y = y),
                         color = "red", linewidth = 2) +
        geom_point(data = rv$polygon_points, aes(x = x, y = y),
                   color = "red", size = 4, shape = 21, fill = "white", stroke = 2)
    }
    p
  })
  
  output$annexin_plot <- renderPlot({
    req(rv$current_gating_data)
    
    df <- data.frame(
      Annexin = rv$current_gating_data$annexin,
      PI = rv$current_gating_data$pi
    )
    if(nrow(df) > 10000) df <- df[sample(nrow(df), 10000), ]
    
    p <- ggplot(df, aes(x = Annexin, y = PI)) +
      stat_density_2d(aes(fill = after_stat(level)), geom = "polygon", alpha = 0.6) +
      geom_point(alpha = 0.1, size = 0.5, color = "black") +
      scale_fill_viridis_c(option = "magma") +
      labs(title = paste("Annexin V / PI Gate -", rv$current_gating_data$cell_line),
           subtitle = "Lower-left = viable",
           x = "Annexin V-FITC (transformed)", y = "PI (transformed)") +
      theme_bw(base_size = 14) +
      theme(legend.position = "none", plot.title = element_text(face = "bold"))
    
    p <- p +
      geom_vline(xintercept = input$annexin_threshold, color = "red", linewidth = 2) +
      geom_hline(yintercept = input$pi_threshold, color = "red", linewidth = 2)
    
    p
  })
  
  # === STATS OUTPUTS (UNCHANGED) ===
  
  output$fsc_ssc_stats <- renderText({
    req(rv$current_gating_data_fsc)
    total <- length(rv$current_gating_data_fsc$fsc)
    
    if(nrow(rv$polygon_points) >= 3) {
      in_gate <- point.in.polygon(rv$current_gating_data_fsc$fsc,
                                  rv$current_gating_data_fsc$ssc,
                                  rv$polygon_points$x, rv$polygon_points$y) > 0
      kept <- sum(in_gate)
    } else {
      kept <- 0
    }
    
    pct <- round(kept/total*100, 1)
    paste0("Events kept: ", format(kept, big.mark = ","), " / ",
           format(total, big.mark = ","), " (", pct, "%)")
  })
  
  output$singlet_stats <- renderText({
    req(rv$current_gating_data_singlet)
    total <- length(rv$current_gating_data_singlet$fsc_a)
    
    if(nrow(rv$polygon_points) >= 3) {
      in_gate <- point.in.polygon(rv$current_gating_data_singlet$fsc_a,
                                  rv$current_gating_data_singlet$fsc_h,
                                  rv$polygon_points$x, rv$polygon_points$y) > 0
      kept <- sum(in_gate)
    } else {
      kept <- 0
    }
    
    pct <- round(kept/total*100, 1)
    paste0("Singlets: ", format(kept, big.mark = ","), " / ",
           format(total, big.mark = ","), " (", pct, "%)\nExpected: 85-95%")
  })
  
  output$annexin_stats <- renderText({
    req(rv$current_gating_data)
    annexin <- rv$current_gating_data$annexin
    pi <- rv$current_gating_data$pi
    total <- length(annexin)
    
    viable <- sum(annexin < input$annexin_threshold & pi < input$pi_threshold)
    
    viable_pct <- round(viable/total*100, 1)
    status <- if(viable_pct >= 70 && viable_pct <= 95) "✓ Good"
    else if(viable_pct > 95) "⚠ Too high"
    else "⚠ Too low"
    
    paste0("Viable cells: ", viable_pct, "% ", status, "\nExpected for control: 70-95%")
  })
  
  # === CLICK HANDLERS (UNCHANGED) ===
  
  observeEvent(input$fsc_ssc_click, {
    click <- input$fsc_ssc_click
    rv$polygon_points <- rbind(rv$polygon_points, data.frame(x = click$x, y = click$y))
  })
  
  observeEvent(input$singlet_click, {
    click <- input$singlet_click
    rv$polygon_points <- rbind(rv$polygon_points, data.frame(x = click$x, y = click$y))
  })
  
  observeEvent(input$clear_polygon_fsc, {
    rv$polygon_points <- data.frame(x = numeric(), y = numeric())
  })
  
  observeEvent(input$clear_polygon_singlet, {
    rv$polygon_points <- data.frame(x = numeric(), y = numeric())
  })
  
  observeEvent(input$close_polygon_fsc, {
    if(nrow(rv$polygon_points) >= 3) {
      rv$polygon_points <- rbind(rv$polygon_points, rv$polygon_points[1, ])
      showNotification("Polygon closed!", type = "message")
    }
  })
  
  observeEvent(input$close_polygon_singlet, {
    if(nrow(rv$polygon_points) >= 3) {
      rv$polygon_points <- rbind(rv$polygon_points, rv$polygon_points[1, ])
      showNotification("Polygon closed!", type = "message")
    }
  })
  
  # === NAVIGATION BETWEEN STEPS (UNCHANGED) ===
  
  observeEvent(input$next_to_singlets, {
    req(rv$current_gating_data_fsc)
    
    if(nrow(rv$polygon_points) < 3) {
      showNotification("Draw a polygon first (at least 3 points)", type = "error")
      return()
    }
    rv$temp_fsc_ssc_gates <- list(type = "polygon", polygon = rv$polygon_points)
    in_gate <- point.in.polygon(rv$current_gating_data_fsc$fsc,
                                rv$current_gating_data_fsc$ssc,
                                rv$polygon_points$x, rv$polygon_points$y) > 0
    rv$temp_gated_data_cells <- rv$current_gating_data_fsc$data_raw[in_gate, ]
    
    if(nrow(rv$temp_gated_data_cells) == 0) {
      showNotification("No cells in gate! Adjust gate.", type = "error")
      return()
    }
    
    rv$polygon_points <- data.frame(x = numeric(), y = numeric())
    rv$gating_step <- "singlets"
  })
  
  observeEvent(input$next_to_annexin, {
    req(rv$current_gating_data_singlet)
    
    if(nrow(rv$polygon_points) < 3) {
      showNotification("Draw a polygon first (at least 3 points)", type = "error")
      return()
    }
    rv$temp_singlet_gates <- list(type = "polygon", polygon = rv$polygon_points)
    in_gate <- point.in.polygon(rv$current_gating_data_singlet$fsc_a,
                                rv$current_gating_data_singlet$fsc_h,
                                rv$polygon_points$x, rv$polygon_points$y) > 0
    rv$temp_gated_data_singlets <- rv$current_gating_data_singlet$data_cells[in_gate, ]
    
    if(nrow(rv$temp_gated_data_singlets) == 0) {
      showNotification("No singlets in gate! Adjust gate.", type = "error")
      return()
    }
    
    rv$polygon_points <- data.frame(x = numeric(), y = numeric())
    rv$gating_step <- "annexin_pi"
  })
  
  observeEvent(input$back_to_fsc, {
    rv$gating_step <- "fsc_ssc"
    rv$polygon_points <- data.frame(x = numeric(), y = numeric())
  })
  
  observeEvent(input$back_to_singlet, {
    rv$gating_step <- "singlets"
    rv$polygon_points <- data.frame(x = numeric(), y = numeric())
  })
  
  observeEvent(input$save_gate, {
    req(rv$current_gating_data)
    cell_line <- rv$current_gating_data$cell_line
    
    rv$thresholds[[cell_line]] <- list(
      fsc_ssc   = rv$temp_fsc_ssc_gates,
      singlets  = rv$temp_singlet_gates,
      annexin_pi = list(
        type    = "rectangle",
        annexin = input$annexin_threshold,
        pi      = input$pi_threshold
      )
    )
    
    # ---- NEW: store a real FSC/SSC gate plot for Gate Review ----
    # Rebuild a small data frame for FSC/SSC from the control gate
    if (!is.null(rv$temp_gated_data_cells) && !is.null(rv$temp_fsc_ssc_gates)) {
      gate_df <- data.frame(
        FSC = rv$temp_gated_data_cells[, "FSC-A"],
        SSC = rv$temp_gated_data_cells[, "SSC-A"]
      )
      poly_df <- rv$temp_fsc_ssc_gates$polygon
      
      rv$gate_plots[[cell_line]] <- ggplot(gate_df, aes(x = FSC, y = SSC)) +
        geom_point(alpha = 0.2, size = 0.5) +
        geom_path(data = poly_df, aes(x = x, y = y),
                  color = "red", linewidth = 1) +
        labs(
          title = paste("FSC/SSC Gate -", cell_line),
          x = "FSC-A",
          y = "SSC-A"
        ) +
        theme_bw(base_size = 12)
    } else {
      # Fallback: at least show something instead of NULL
      rv$gate_plots[[cell_line]] <- ggplot() +
        annotate(
          "text",
          x = 0.5, y = 0.5,
          label = paste("Gates saved for", cell_line),
          size = 6
        ) +
        xlim(0, 1) + ylim(0, 1) +
        theme_void()
    }
    # -------------------------------------------------------------
    
    showNotification(paste("Gates saved for", cell_line), type = "message")
    
    rv$current_cell_line_index <- rv$current_cell_line_index + 1
    rv$gating_step             <- "fsc_ssc"
    rv$polygon_points          <- data.frame(x = numeric(), y = numeric())
    rv$current_gating_data     <- NULL
    rv$current_gating_data_fsc <- NULL
    rv$current_gating_data_singlet <- NULL
    rv$temp_fsc_ssc_gates      <- NULL
    rv$temp_singlet_gates      <- NULL
    rv$temp_gated_data_cells   <- NULL
    rv$temp_gated_data_singlets <- NULL
  })
  # === ANALYSIS WITH ENHANCED METRICS ===
  observeEvent(input$analyze, {
    req(rv$metadata, rv$thresholds)
    
    if(length(rv$thresholds) == 0) {
      showNotification("No gates set", type = "error")
      return()
    }
    
    updateTabsetPanel(session, "tabs", selected = "Results")
    
    withProgress(message = "Analyzing samples...", value = 0, {
      trans <- logicleTransform()
      results <- data.frame()
      errors <- character()
      cell_counts_data <- data.frame()  # NEW: Track cell counts
      
      for(i in 1:nrow(rv$metadata)) {
        row <- rv$metadata[i, ]
        cell_line <- row$cell_line
        
        if(is.na(cell_line) || !cell_line %in% names(rv$thresholds)) {
          incProgress(1/nrow(rv$metadata))
          next
        }
        
        thresholds <- rv$thresholds[[cell_line]]
        
        tryCatch({
          fs <- read.FCS(row$datapath)
          data_raw <- exprs(fs)
          total_events <- nrow(data_raw)  # NEW: Track starting count
          
          # Gate 1: FSC/SSC
          fsc_ssc_gate <- thresholds$fsc_ssc
          poly <- fsc_ssc_gate$polygon
          in_gate1 <- point.in.polygon(data_raw[, "FSC-A"], data_raw[, "SSC-A"],
                                       poly$x, poly$y) > 0
          data_cells <- data_raw[in_gate1, ]
          cells_after_fsc <- nrow(data_cells)  # NEW
          if(nrow(data_cells) == 0) next
          
          # Gate 2: Singlets
          singlet_gate <- thresholds$singlets
          poly <- singlet_gate$polygon
          in_gate2 <- point.in.polygon(data_cells[, "FSC-A"], data_cells[, "FSC-H"],
                                       poly$x, poly$y) > 0
          data_singlets <- data_cells[in_gate2, ]
          singlets_count <- nrow(data_singlets)  # NEW
          if(nrow(data_singlets) == 0) next
          
          # Gate 3: Annexin/PI with compensation
          if(rv$comp_calculated) {
            fluor_data <- data_singlets[, c("BL1-A", "BL2-A")]
            comp_data <- fluor_data %*% t(rv$comp_matrix)
            annexin <- trans(comp_data[, "BL1-A"])
            pi <- trans(comp_data[, "BL2-A"])
          } else {
            annexin <- trans(data_singlets[, "BL1-A"])
            pi <- trans(data_singlets[, "BL2-A"])
          }
          
          annexin_pi_gate <- thresholds$annexin_pi
          ann_thresh <- annexin_pi_gate$annexin
          pi_thresh <- annexin_pi_gate$pi
          
          # 4-QUADRANT CLASSIFICATION
          viable <- sum(annexin < ann_thresh & pi < pi_thresh)
          early_apoptotic <- sum(annexin >= ann_thresh & pi < pi_thresh)
          late_apoptotic <- sum(annexin >= ann_thresh & pi >= pi_thresh)
          necrotic <- sum(annexin < ann_thresh & pi >= pi_thresh)
          total <- length(annexin)
          
          cell_death <- early_apoptotic + late_apoptotic + necrotic
          
          results <- rbind(results, data.frame(
            cell_line = cell_line,
            concentration_uM = row$concentration_uM,
            replicate = row$replicate,
            pct_viable = (viable/total)*100,
            pct_early_apoptotic = (early_apoptotic/total)*100,
            pct_late_apoptotic = (late_apoptotic/total)*100,
            pct_necrotic = (necrotic/total)*100,
            pct_cell_death = (cell_death/total)*100,
            stringsAsFactors = FALSE
          ))
          
          # NEW: Store cell counts at each gating step
          cell_counts_data <- rbind(cell_counts_data, data.frame(
            cell_line = cell_line,
            concentration_uM = row$concentration_uM,
            replicate = row$replicate,
            total_events = total_events,
            after_fsc_ssc = cells_after_fsc,
            after_singlets = singlets_count,
            viable_count = viable,
            dead_count = cell_death,
            stringsAsFactors = FALSE
          ))
          
        }, error = function(e) {
          errors <<- c(errors, paste(row$name, ":", e$message))
        })
        
        incProgress(1/nrow(rv$metadata))
      }
      
      if(nrow(results) == 0) {
        msg <- "Analysis failed - no valid results"
        if(length(errors) > 0) msg <- paste0(msg, "\n\nErrors:\n", paste(errors, collapse = "\n"))
        showNotification(msg, type = "error", duration = NULL)
        return()
      }
      
      rv$results <- results
      rv$cell_counts <- cell_counts_data  # NEW
      
      # NEW: Calculate CV% and detect outliers
      qc_data <- results %>%
        group_by(cell_line, concentration_uM) %>%
        summarise(
          mean_viable = mean(pct_viable, na.rm = TRUE),
          sd_viable = sd(pct_viable, na.rm = TRUE),
          cv_pct = (sd_viable / mean_viable) * 100,
          n = n(),
          .groups = "drop"
        )
      
      # Grubbs test for outliers (if n >= 3)
      outlier_flags <- data.frame()
      for(i in 1:nrow(results)) {
        group_data <- results %>%
          filter(cell_line == results$cell_line[i],
                 concentration_uM == results$concentration_uM[i])
        
        if(nrow(group_data) >= 3) {
          grubbs_result <- tryCatch({
            grubbs.test(group_data$pct_viable)
          }, error = function(e) NULL)
          
          if(!is.null(grubbs_result) && grubbs_result$p.value < 0.05) {
            outlier_idx <- which.max(abs(group_data$pct_viable - mean(group_data$pct_viable)))
            outlier_flags <- rbind(outlier_flags, data.frame(
              cell_line = results$cell_line[i],
              concentration_uM = results$concentration_uM[i],
              replicate = group_data$replicate[outlier_idx],
              is_outlier = TRUE,
              p_value = grubbs_result$p.value,
              stringsAsFactors = FALSE
            ))
          }
        }
      }
      rv$qc_data <- qc_data
      rv$outlier_flags <- outlier_flags
      
      # === NORMALIZATION (if enabled) ===
      if(input$use_normalization) {
        control_mean <- results %>%
          filter(concentration_uM == rv$control_concentration) %>%
          group_by(cell_line) %>%
          summarise(control_viable = mean(pct_viable, na.rm = TRUE), .groups = "drop")
        
        results <- results %>%
          left_join(control_mean, by = "cell_line") %>%
          mutate(pct_viable = (pct_viable / control_viable) * 100)
        
        # Remove helper column without using select()
        results$control_viable <- NULL
        
        rv$results <- results
      }
      
      # === VIABILITY DATA ===
      viability_data <- results %>%
        group_by(cell_line, concentration_uM) %>%
        summarise(mean_viable = mean(pct_viable, na.rm = TRUE),
                  sd_viable = sd(pct_viable, na.rm = TRUE),
                  n = n(),
                  se_viable = sd_viable / sqrt(n),
                  .groups = "drop") %>%
        mutate(concentration_uM = as.numeric(as.character(concentration_uM)))
      
      # === CELL DEATH DATA ===
      death_data <- results %>%
        group_by(cell_line, concentration_uM) %>%
        summarise(mean_death = mean(pct_cell_death, na.rm = TRUE),
                  sd_death = sd(pct_cell_death, na.rm = TRUE),
                  n = n(),
                  se_death = sd_death / sqrt(n),
                  .groups = "drop") %>%
        mutate(concentration_uM = as.numeric(as.character(concentration_uM)))
      
      # === IC50 & LD50 FITTING WITH EXTENDED METRICS ===
      ic50_results <- data.frame()
      ld50_results <- data.frame()
      predicted_viability_curves <- data.frame()
      predicted_death_curves <- data.frame()
      advanced_metrics <- data.frame()  # NEW
      fit_quality <- data.frame()  # NEW
      
      for(line in unique(viability_data$cell_line)) {
        if(is.na(line)) next
        
        line_viab <- viability_data %>% filter(cell_line == line)
        line_death <- death_data %>% filter(cell_line == line)
        
        if(nrow(line_viab) < 4) {
          message("Skipping IC50 for ", line, ": insufficient data points (", nrow(line_viab), " < 4)")
          next
        }
        
        line_viab$dose <- line_viab$concentration_uM + 0.1
        line_death$dose <- line_death$concentration_uM + 0.1
        
        # FIT VIABILITY (IC50)
        fit_viab <- NULL
        fit_method_viab <- "none"
        
        if(is.null(fit_viab)) {
          fit_viab <- tryCatch({
            drm(mean_viable ~ dose, data = line_viab, fct = LL.4())
          }, error = function(e) NULL)
          if(!is.null(fit_viab)) fit_method_viab <- "LL.4 auto"
        }
        
        if(is.null(fit_viab)) {
          fit_viab <- tryCatch({
            min_v <- min(line_viab$mean_viable, na.rm = TRUE)
            max_v <- max(line_viab$mean_viable, na.rm = TRUE)
            mid_dose <- median(line_viab$dose)
            drm(mean_viable ~ dose, data = line_viab,
                fct = LL.4(names = c("Slope", "Lower", "Upper", "ED50")),
                start = c(1, min_v, max_v, mid_dose))
          }, error = function(e) NULL)
          if(!is.null(fit_viab)) fit_method_viab <- "LL.4 manual"
        }
        
        if(is.null(fit_viab)) {
          fit_viab <- tryCatch({
            drm(mean_viable ~ dose, data = line_viab, fct = LL.3())
          }, error = function(e) NULL)
          if(!is.null(fit_viab)) fit_method_viab <- "LL.3"
        }
        
        if(is.null(fit_viab)) {
          fit_viab <- tryCatch({
            drm(mean_viable ~ dose, data = line_viab, fct = W1.4())
          }, error = function(e) NULL)
          if(!is.null(fit_viab)) fit_method_viab <- "Weibull"
        }
        
        if(is.null(fit_viab)) {
          fit_viab <- tryCatch({
            drm(mean_viable ~ dose, data = line_viab, fct = W1.2())
          }, error = function(e) NULL)
          if(!is.null(fit_viab)) fit_method_viab <- "Weibull-2"
        }
        
        if(is.null(fit_viab)) {
          message("IC50 fit failed for ", line, ": all strategies exhausted")
        } else {
          message("IC50 fit succeeded for ", line, " using ", fit_method_viab)
          
          tryCatch({
            # IC50
            ic50_result <- suppressWarnings(ED(fit_viab, 50, interval = "delta", display = FALSE))
            ec50_val <- ic50_result[1]
            ec50_lower <- ic50_result[3]
            ec50_upper <- ic50_result[4]
            
            # NEW: Extract extended metrics
            coefs <- coef(fit_viab)
            hill_slope <- ifelse(length(coefs) >= 1, abs(coefs[1]), NA)
            top_plateau <- ifelse(length(coefs) >= 3, coefs[3], NA)
            bottom_plateau <- ifelse(length(coefs) >= 2, coefs[2], NA)
            
            # NEW: EC25, EC75, EC90
            ec25 <- tryCatch(ED(fit_viab, 25, interval = "delta", display = FALSE)[1] - 0.1, error = function(e) NA)
            ec75 <- tryCatch(ED(fit_viab, 75, interval = "delta", display = FALSE)[1] - 0.1, error = function(e) NA)
            ec90 <- tryCatch(ED(fit_viab, 90, interval = "delta", display = FALSE)[1] - 0.1, error = function(e) NA)
            
            # NEW: R² calculation
            predicted_vals <- predict(fit_viab)
            observed_vals <- line_viab$mean_viable
            ss_res <- sum((observed_vals - predicted_vals)^2)
            ss_tot <- sum((observed_vals - mean(observed_vals))^2)
            r_squared <- 1 - (ss_res / ss_tot)
            
            # NEW: AUC calculation
            dose_range <- seq(min(line_viab$concentration_uM), max(line_viab$concentration_uM), length.out = 1000)
            pred_range <- predict(fit_viab, newdata = data.frame(dose = dose_range + 0.1))
            auc_val <- AUC(dose_range, pred_range, method = "trapezoid")
            
            ic50_results <- rbind(ic50_results, data.frame(
              cell_line = line,
              IC50_uM = ec50_val - 0.1,
              IC50_lower = ec50_lower - 0.1,
              IC50_upper = ec50_upper - 0.1,
              fit_method = fit_method_viab,
              stringsAsFactors = FALSE
            ))
            
            # NEW: Store advanced metrics
            advanced_metrics <- rbind(advanced_metrics, data.frame(
              cell_line = line,
              metric_type = "Viability",
              EC25_uM = ec25,
              EC75_uM = ec75,
              EC90_uM = ec90,
              Hill_slope = hill_slope,
              Top_plateau = top_plateau,
              Bottom_plateau = bottom_plateau,
              AUC = auc_val,
              R_squared = r_squared,
              stringsAsFactors = FALSE
            ))
            
            pred_doses <- seq(min(line_viab$dose), max(line_viab$dose), length.out = 200)
            pred_data <- data.frame(dose = pred_doses)
            pred_data$predicted_viable <- predict(fit_viab, newdata = pred_data)
            pred_data$concentration_uM <- pred_doses - 0.1
            pred_data$cell_line <- line
            
            predicted_viability_curves <- rbind(predicted_viability_curves, pred_data)
            
          }, error = function(e) {
            message("IC50 extraction failed for ", line, ": ", e$message)
          })
        }
        
        # FIT DEATH (LD50) - Similar structure
        fit_death <- NULL
        fit_method_death <- "none"
        
        if(is.null(fit_death)) {
          fit_death <- tryCatch({
            drm(mean_death ~ dose, data = line_death, fct = LL.4())
          }, error = function(e) NULL)
          if(!is.null(fit_death)) fit_method_death <- "LL.4 auto"
        }
        
        if(is.null(fit_death)) {
          fit_death <- tryCatch({
            min_v <- min(line_death$mean_death, na.rm = TRUE)
            max_v <- max(line_death$mean_death, na.rm = TRUE)
            mid_dose <- median(line_death$dose)
            drm(mean_death ~ dose, data = line_death,
                fct = LL.4(names = c("Slope", "Lower", "Upper", "ED50")),
                start = c(1, min_v, max_v, mid_dose))
          }, error = function(e) NULL)
          if(!is.null(fit_death)) fit_method_death <- "LL.4 manual"
        }
        
        if(is.null(fit_death)) {
          fit_death <- tryCatch({
            drm(mean_death ~ dose, data = line_death, fct = LL.3())
          }, error = function(e) NULL)
          if(!is.null(fit_death)) fit_method_death <- "LL.3"
        }
        
        if(is.null(fit_death)) {
          fit_death <- tryCatch({
            drm(mean_death ~ dose, data = line_death, fct = W1.4())
          }, error = function(e) NULL)
          if(!is.null(fit_death)) fit_method_death <- "Weibull"
        }
        
        if(is.null(fit_death)) {
          fit_death <- tryCatch({
            drm(mean_death ~ dose, data = line_death, fct = W1.2())
          }, error = function(e) NULL)
          if(!is.null(fit_death)) fit_method_death <- "Weibull-2"
        }
        
        if(!is.null(fit_death)) {
          tryCatch({
            ld50_result <- suppressWarnings(ED(fit_death, 50, interval = "delta", display = FALSE))
            ed50_val <- ld50_result[1]
            ed50_lower <- ld50_result[3]
            ed50_upper <- ld50_result[4]
            
            ld50_results <- rbind(ld50_results, data.frame(
              cell_line = line,
              LD50_uM = ed50_val - 0.1,
              LD50_lower = ed50_lower - 0.1,
              LD50_upper = ed50_upper - 0.1,
              fit_method = fit_method_death,
              stringsAsFactors = FALSE
            ))
            
            pred_doses <- seq(min(line_death$dose), max(line_death$dose), length.out = 200)
            pred_data <- data.frame(dose = pred_doses)
            pred_data$predicted_death <- predict(fit_death, newdata = pred_data)
            pred_data$concentration_uM <- pred_doses - 0.1
            pred_data$cell_line <- line
            
            predicted_death_curves <- rbind(predicted_death_curves, pred_data)
            
          }, error = function(e) {
            message("LD50 extraction failed for ", line, ": ", e$message)
          })
        }
      }
      
      rv$ic50_results <- ic50_results
      rv$ld50_results <- ld50_results
      rv$viability_data <- viability_data
      rv$death_data <- death_data
      rv$predicted_viability_curves <- predicted_viability_curves
      rv$predicted_death_curves <- predicted_death_curves
      rv$advanced_metrics <- advanced_metrics  # NEW
      
      if(nrow(ic50_results) == 0 && nrow(ld50_results) == 0) {
        showNotification("Analysis complete, but IC50/LD50 fitting failed for all cell lines. Check data quality.",
                         type = "warning", duration = 10)
      } else {
        showNotification("Analysis complete!", type = "message")
      }
    })
  })
  
  # NEW: QC UI Output
  output$qc_ui <- renderUI({
    req(rv$qc_data)
    
    tagList(
      h5("Coefficient of Variation (CV%) by Condition"),
      p("CV% < 15%: Good | 15-25%: Acceptable | >25%: High variability"),
      tableOutput("qc_table"),
      hr(),
      h5("Outlier Detection (Grubbs Test, α=0.05)"),
      if(!is.null(rv$outlier_flags) && nrow(rv$outlier_flags) > 0) {
        tagList(
          p(style = "color: #d32f2f;", icon("exclamation-triangle"), 
            " ", nrow(rv$outlier_flags), " potential outlier(s) detected:"),
          tableOutput("outlier_table"),
          p(style = "font-size: 0.9em; color: #666;",
            "Note: Outliers are not automatically removed. Review and decide if exclusion is warranted.")
        )
      } else {
        div(class = "alert alert-success",
            icon("check-circle"), " No significant outliers detected")
      }
    )
  })
  
  output$qc_table <- renderTable({
    req(rv$qc_data)
    rv$qc_data %>%
      mutate(
        QC_Status = case_when(
          cv_pct < 15 ~ "✓ Good",
          cv_pct < 25 ~ "⚠ Acceptable",
          TRUE ~ "✗ High Variability"
        )
      ) %>%
      dplyr::select(`Cell Line` = cell_line,   # <-- ADD dplyr::
                    `Concentration (µM)` = concentration_uM,
                    `Mean Viability (%)` = mean_viable,
                    `CV (%)` = cv_pct,
                    `n` = n,
                    `Status` = QC_Status)
  }, striped = TRUE, hover = TRUE, bordered = TRUE, digits = 2)
  
  output$outlier_table <- renderTable({
    req(rv$outlier_flags)
    rv$outlier_flags %>%
      dplyr::select(`Cell Line` = cell_line,   # <-- ADD dplyr::
                    `Concentration (µM)` = concentration_uM,
                    `Replicate` = replicate,
                    `p-value` = p_value)
  }, striped = TRUE, hover = TRUE, bordered = TRUE, digits = 4)
  
  # NEW: Statistical comparison output
  output$stats_comparison <- renderPrint({
    req(rv$ic50_results)
    
    if(nrow(rv$ic50_results) < 2) {
      cat("Statistical comparison requires at least 2 cell lines.\n")
      return()
    }
    
    cat("=== STATISTICAL COMPARISON OF IC50 VALUES ===\n\n")
    cat("Cell Line IC50 Summary:\n")
    print(rv$ic50_results %>% dplyr::select(cell_line, IC50_uM))
    cat("\n")
    
    # ANOVA test
    cat("ANOVA Test:\n")
    cat("H0: All cell lines have equal IC50 values\n")
    anova_result <- tryCatch({
      aov(IC50_uM ~ cell_line, data = rv$ic50_results)
    }, error = function(e) NULL)
    
    if(!is.null(anova_result)) {
      print(summary(anova_result))
    } else {
      cat("ANOVA could not be performed.\n")
    }
  })
  
  # NEW: Advanced metrics table
  output$advanced_metrics_table <- renderTable({
    req(rv$advanced_metrics)
    rv$advanced_metrics %>%
      dplyr::select(`Cell Line` = cell_line,   # <-- ADD dplyr::
                    `Type` = metric_type,
                    `EC25 (µM)` = EC25_uM,
                    `EC75 (µM)` = EC75_uM,
                    `EC90 (µM)` = EC90_uM,
                    `Hill Slope` = Hill_slope,
                    `Top (%)` = Top_plateau,
                    `Bottom (%)` = Bottom_plateau,
                    `AUC` = AUC,
                    `R²` = R_squared)
  }, striped = TRUE, hover = TRUE, bordered = TRUE, digits = 3)
  
  # === SMART X-AXIS BREAKS (UNCHANGED) ===
  get_smart_breaks <- function(conc_range) {
    min_c <- min(conc_range)
    max_c <- max(conc_range)
    
    if(max_c <= 1) {
      return(c(0, 0.1, 0.5, 1))
    } else if(max_c <= 10) {
      return(c(0, 0.1, 0.5, 1, 5, 10))
    } else if(max_c <= 100) {
      return(c(0, 0.1, 0.5, 1, 10, 50, 100))
    } else {
      return(c(0, 1, 10, 100, 500, 1000))
    }
  }
  
  # === OUTPUT TABLES (UNCHANGED STRUCTURE) ===
  output$ic50_table <- renderTable({
    req(rv$ic50_results, rv$ld50_results)
    
    combined <- merge(rv$ic50_results, rv$ld50_results, by = "cell_line", all = TRUE)
    combined <- combined %>%
      mutate(
        `IC50 [95% CI] µM` = ifelse(is.na(IC50_uM), "N/A",
                                    sprintf("%.2f [%.2f - %.2f]", IC50_uM, IC50_lower, IC50_upper)),
        `LD50 [95% CI] µM` = ifelse(is.na(LD50_uM), "N/A",
                                    sprintf("%.2f [%.2f - %.2f]", LD50_uM, LD50_lower, LD50_upper))
      )
    
    combined <- combined[, c("cell_line", "IC50 [95% CI] µM", "LD50 [95% CI] µM")]
    colnames(combined)[1] <- "Cell Line"
    
    combined
  }, striped = TRUE, hover = TRUE, bordered = TRUE)
  
  # === VIABILITY PLOT (UNCHANGED) ===
  output$viability_plot <- renderPlot({
    req(rv$viability_data)
    
    cell_lines <- unique(rv$viability_data$cell_line)
    colors <- get_color_palette(length(cell_lines))
    names(colors) <- cell_lines
    
    conc_range <- rv$viability_data$concentration_uM
    breaks <- get_smart_breaks(conc_range)
    
    p <- ggplot() +
      geom_point(data = rv$viability_data, aes(x = concentration_uM, y = mean_viable, color = cell_line),
                 size = 4, shape = 21, fill = "white", stroke = 1.5) +
      geom_errorbar(data = rv$viability_data,
                    aes(x = concentration_uM, ymin = mean_viable - se_viable,
                        ymax = mean_viable + se_viable, color = cell_line),
                    width = 0.15, linewidth = 1)
    
    if(!is.null(rv$predicted_viability_curves) && nrow(rv$predicted_viability_curves) > 0) {
      p <- p + geom_line(data = rv$predicted_viability_curves,
                         aes(x = concentration_uM, y = predicted_viable, color = cell_line),
                         linewidth = 1.5, alpha = 0.9)
    }
    
    p <- p + scale_color_manual(values = colors) +
      scale_x_continuous(trans = pseudo_log_trans(base = 10), breaks = breaks) +
      scale_y_continuous(limits = c(0, 110), breaks = seq(0, 100, 20)) +
      labs(title = "Dose-Response Curves with IC50 Determination",
           subtitle = "Multiple fitting strategies attempted for optimal curve fit",
           x = "Concentration (µM)", y = "Cell Viability (%)", color = "Cell Line") +
      theme_publication()
    
    rv$viability_plot_obj <- p
    p
  })
  
  # === DEATH PLOT (UNCHANGED) ===
  output$death_plot <- renderPlot({
    req(rv$death_data)
    
    cell_lines <- unique(rv$death_data$cell_line)
    colors <- get_color_palette(length(cell_lines))
    names(colors) <- cell_lines
    
    conc_range <- rv$death_data$concentration_uM
    breaks <- get_smart_breaks(conc_range)
    
    p <- ggplot() +
      geom_point(data = rv$death_data, aes(x = concentration_uM, y = mean_death, color = cell_line),
                 size = 4, shape = 21, fill = "white", stroke = 1.5) +
      geom_errorbar(data = rv$death_data,
                    aes(x = concentration_uM, ymin = mean_death - se_death,
                        ymax = mean_death + se_death, color = cell_line),
                    width = 0.15, linewidth = 1)
    
    if(!is.null(rv$predicted_death_curves) && nrow(rv$predicted_death_curves) > 0) {
      p <- p + geom_line(data = rv$predicted_death_curves,
                         aes(x = concentration_uM, y = predicted_death, color = cell_line),
                         linewidth = 1.5, alpha = 0.9)
    }
    
    p <- p + scale_color_manual(values = colors) +
      scale_x_continuous(trans = pseudo_log_trans(base = 10), breaks = breaks) +
      scale_y_continuous(limits = c(0, 110), breaks = seq(0, 100, 20)) +
      labs(title = "Cell Death Dose-Response Curves with LD50",
           subtitle = "Cell death = Early Apoptotic + Late Apoptotic + Necrotic",
           x = "Concentration (µM)", y = "Cell Death (%)", color = "Cell Line") +
      theme_publication()
    
    rv$death_plot_obj <- p
    p
  })
  
  # === IC50 vs LD50 COMPARISON (UNCHANGED) ===
  output$ic50_ld50_comparison <- renderPlot({
    req(rv$ic50_results, rv$ld50_results)
    
    combined <- merge(rv$ic50_results, rv$ld50_results, by = "cell_line", all = TRUE)
    combined_long <- combined %>%
      pivot_longer(cols = c(IC50_uM, LD50_uM), names_to = "Metric", values_to = "Value") %>%
      filter(!is.na(Value)) %>%
      mutate(Metric = ifelse(Metric == "IC50_uM", "IC50 (Viability)", "LD50 (Death)"))
    
    cell_lines <- unique(combined_long$cell_line)
    colors <- get_color_palette(length(cell_lines))
    names(colors) <- cell_lines
    
    p <- ggplot(combined_long, aes(x = reorder(cell_line, Value), y = Value, fill = cell_line)) +
      geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7, color = "black", linewidth = 0.5) +
      geom_text(aes(label = sprintf("%.2f", Value)), position = position_dodge(width = 0.8),
                vjust = -0.5, size = 3.5, fontface = "bold") +
      facet_wrap(~ Metric, scales = "free_y") +
      scale_fill_manual(values = colors) +
      labs(title = "IC50 vs LD50 Comparison Across Cell Lines",
           subtitle = "IC50: Half-maximal inhibitory concentration | LD50: Lethal dose 50%",
           x = "Cell Line", y = "Concentration (µM)") +
      theme_publication() +
      theme(legend.position = "none",
            axis.text.x = element_text(angle = 45, hjust = 1),
            strip.text = element_text(size = 11, face = "bold"))
    
    rv$comparison_plot_obj <- p
    p
  })
  
  # === QUADRANT BREAKDOWN (UNCHANGED) ===
  output$quadrant_plot <- renderPlot({
    req(rv$results)
    
    quadrant_data <- rv$results %>%
      group_by(cell_line, concentration_uM) %>%
      summarise(
        Viable = mean(pct_viable),
        `Early Apoptotic` = mean(pct_early_apoptotic),
        `Late Apoptotic` = mean(pct_late_apoptotic),
        Necrotic = mean(pct_necrotic),
        .groups = "drop"
      ) %>%
      pivot_longer(cols = c(Viable, `Early Apoptotic`, `Late Apoptotic`, Necrotic),
                   names_to = "Quadrant", values_to = "Percentage")
    
    quadrant_data$Quadrant <- factor(quadrant_data$Quadrant,
                                     levels = c("Viable", "Early Apoptotic", "Late Apoptotic", "Necrotic"))
    
    p <- ggplot(quadrant_data, aes(x = as.factor(concentration_uM), y = Percentage, fill = Quadrant)) +
      geom_bar(stat = "identity", position = "stack", color = "black", linewidth = 0.3) +
      scale_fill_manual(values = c("Viable" = "#009E73",
                                   "Early Apoptotic" = "#F0E442",
                                   "Late Apoptotic" = "#D55E00",
                                   "Necrotic" = "#CC79A7")) +
      facet_wrap(~ cell_line, scales = "free_x") +
      labs(title = "Apoptosis Quadrant Breakdown by Concentration",
           subtitle = "Annexin V/PI classification: Viable (−/−), Early Apoptotic (+/−), Late Apoptotic (+/+), Necrotic (−/+)",
           x = "Concentration (µM)", y = "Percentage (%)", fill = "Cell State") +
      theme_publication() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            strip.text = element_text(size = 10, face = "bold"))
    
    rv$quadrant_plot_obj <- p
    p
  })
  
  output$download_results <- downloadHandler(
    filename = function() { paste0("FACS_results_", Sys.Date(), ".csv") },
    content = function(file) {
      req(rv$results)
      write_csv(rv$results, file)
    }
  )
  
  # === ENHANCED ZIP DOWNLOAD ===
  output$download_zip_ui <- renderUI({
    req(rv$results)
    downloadButton("download_zip", "📦 Download All Results (ZIP)",
                   class = "btn-info btn-lg btn-block",
                   style = "margin-top: 10px; font-size: 1.1em; padding: 12px;")
  })
  
  output$download_zip <- downloadHandler(
    filename = function() {
      paste0("FACS_IC50_Analysis_", Sys.Date(), ".zip")
    },
    content = function(file) {
      temp_dir <- tempdir()
      
      # Save raw viability data CSV (always available)
      write_csv(rv$results, file.path(temp_dir, "raw_quadrant_data.csv"))
      
      # Save IC50 results CSV (if available)
      if(!is.null(rv$ic50_results) && nrow(rv$ic50_results) > 0) {
        write_csv(rv$ic50_results, file.path(temp_dir, "IC50_results.csv"))
      }
      
      # Save LD50 results CSV (if available)
      if(!is.null(rv$ld50_results) && nrow(rv$ld50_results) > 0) {
        write_csv(rv$ld50_results, file.path(temp_dir, "LD50_results.csv"))
      }
      
      # NEW: Save QC data
      if(!is.null(rv$qc_data) && nrow(rv$qc_data) > 0) {
        write_csv(rv$qc_data, file.path(temp_dir, "QC_CV_analysis.csv"))
      }
      
      # NEW: Save outlier flags
      if(!is.null(rv$outlier_flags) && nrow(rv$outlier_flags) > 0) {
        write_csv(rv$outlier_flags, file.path(temp_dir, "outlier_detection.csv"))
      }
      
      # NEW: Save cell counts
      if(!is.null(rv$cell_counts) && nrow(rv$cell_counts) > 0) {
        write_csv(rv$cell_counts, file.path(temp_dir, "cell_counts_per_gate.csv"))
      }
      
      # NEW: Save advanced metrics
      if(!is.null(rv$advanced_metrics) && nrow(rv$advanced_metrics) > 0) {
        write_csv(rv$advanced_metrics, file.path(temp_dir, "advanced_metrics_EC_AUC_Hill.csv"))
      }
      
      # Save metadata summary
      metadata_summary <- rv$metadata[, c("name", "cell_line", "treatment_full", "concentration_uM", "replicate")]
      colnames(metadata_summary)[1] <- "filename"
      write_csv(metadata_summary, file.path(temp_dir, "file_metadata.csv"))
      
      # Save control summary
      control_summary <- data.frame(
        control_concentration_uM = rv$control_concentration,
        compensation_applied = rv$comp_calculated,
        normalization_applied = input$use_normalization
      )
      write_csv(control_summary, file.path(temp_dir, "analysis_settings.csv"))
      
      # Save compensation matrix if calculated
      if(rv$comp_calculated) {
        comp_df <- as.data.frame(rv$comp_matrix)
        comp_df <- cbind(Channel = rownames(comp_df), comp_df)
        write_csv(comp_df, file.path(temp_dir, "compensation_matrix.csv"))
      }
      
      # NEW: Save gate coordinates
      if(length(rv$thresholds) > 0) {
        gate_coords <- data.frame()
        for(line in names(rv$thresholds)) {
          fsc_poly <- rv$thresholds[[line]]$fsc_ssc$polygon
          fsc_poly$cell_line <- line
          fsc_poly$gate_type <- "FSC_SSC"
          gate_coords <- rbind(gate_coords, fsc_poly)
          
          sing_poly <- rv$thresholds[[line]]$singlets$polygon
          sing_poly$cell_line <- line
          sing_poly$gate_type <- "Singlets"
          gate_coords <- rbind(gate_coords, sing_poly)
        }
        write_csv(gate_coords, file.path(temp_dir, "gate_coordinates.csv"))
      }
      
      # --- Build gate summary table for the report ---
      gate_summary <- NULL
      if (length(rv$thresholds) > 0) {
        gate_summary <- data.frame()
        
        for (line in names(rv$thresholds)) {
          th <- rv$thresholds[[line]]
          
          fsc_npts  <- nrow(th$fsc_ssc$polygon)
          sing_npts <- nrow(th$singlets$polygon)
          ann_th    <- th$annexin_pi$annexin
          pi_th     <- th$annexin_pi$pi
          
          gate_summary <- rbind(
            gate_summary,
            data.frame(
              cell_line         = line,
              fsc_ssc_points    = fsc_npts,
              singlet_points    = sing_npts,
              annexin_threshold = ann_th,
              pi_threshold      = pi_th,
              stringsAsFactors  = FALSE
            )
          )
        }
      }
      
      # --- Rebuild plots for report (independent of UI tabs) ---
      
      # 1) Viability plot
      plot_viability <- NULL
      if (!is.null(rv$viability_data) && nrow(rv$viability_data) > 0) {
        cell_lines <- unique(rv$viability_data$cell_line)
        colors <- get_color_palette(length(cell_lines))
        names(colors) <- cell_lines
        
        conc_range <- rv$viability_data$concentration_uM
        breaks <- get_smart_breaks(conc_range)
        
        p <- ggplot() +
          geom_point(
            data = rv$viability_data,
            aes(x = concentration_uM, y = mean_viable, color = cell_line),
            size = 4, shape = 21, fill = "white", stroke = 1.5
          ) +
          geom_errorbar(
            data = rv$viability_data,
            aes(
              x = concentration_uM,
              ymin = mean_viable - se_viable,
              ymax = mean_viable + se_viable,
              color = cell_line
            ),
            width = 0.15, linewidth = 1
          )
        
        if (!is.null(rv$predicted_viability_curves) &&
            nrow(rv$predicted_viability_curves) > 0) {
          p <- p + geom_line(
            data = rv$predicted_viability_curves,
            aes(x = concentration_uM, y = predicted_viable, color = cell_line),
            linewidth = 1.5, alpha = 0.9
          )
        }
        
        p <- p +
          scale_color_manual(values = colors) +
          scale_x_continuous(trans = pseudo_log_trans(base = 10), breaks = breaks) +
          scale_y_continuous(limits = c(0, 110), breaks = seq(0, 100, 20)) +
          labs(
            title    = "Dose-Response Curves with IC50 Determination",
            subtitle = "Multiple fitting strategies attempted for optimal curve fit",
            x        = "Concentration (µM)",
            y        = "Cell Viability (%)",
            color    = "Cell Line"
          ) +
          theme_publication()
        
        plot_viability <- p
      }
      
      # 2) Death plot
      plot_death <- NULL
      if (!is.null(rv$death_data) && nrow(rv$death_data) > 0) {
        cell_lines <- unique(rv$death_data$cell_line)
        colors <- get_color_palette(length(cell_lines))
        names(colors) <- cell_lines
        
        conc_range <- rv$death_data$concentration_uM
        breaks <- get_smart_breaks(conc_range)
        
        p <- ggplot() +
          geom_point(
            data = rv$death_data,
            aes(x = concentration_uM, y = mean_death, color = cell_line),
            size = 4, shape = 21, fill = "white", stroke = 1.5
          ) +
          geom_errorbar(
            data = rv$death_data,
            aes(
              x = concentration_uM,
              ymin = mean_death - se_death,
              ymax = mean_death + se_death,
              color = cell_line
            ),
            width = 0.15, linewidth = 1
          )
        
        if (!is.null(rv$predicted_death_curves) &&
            nrow(rv$predicted_death_curves) > 0) {
          p <- p + geom_line(
            data = rv$predicted_death_curves,
            aes(x = concentration_uM, y = predicted_death, color = cell_line),
            linewidth = 1.5, alpha = 0.9
          )
        }
        
        p <- p +
          scale_color_manual(values = colors) +
          scale_x_continuous(trans = pseudo_log_trans(base = 10), breaks = breaks) +
          scale_y_continuous(limits = c(0, 110), breaks = seq(0, 100, 20)) +
          labs(
            title    = "Cell Death Dose-Response Curves with LD50",
            subtitle = "Cell death = Early Apoptotic + Late Apoptotic + Necrotic",
            x        = "Concentration (µM)",
            y        = "Cell Death (%)",
            color    = "Cell Line"
          ) +
          theme_publication()
        
        plot_death <- p
      }
      
      # 3) IC50 / LD50 comparison plot
      plot_ic50_ld50 <- NULL
      if (!is.null(rv$ic50_results) && !is.null(rv$ld50_results) &&
          nrow(rv$ic50_results) > 0 && nrow(rv$ld50_results) > 0) {
        
        combined <- merge(rv$ic50_results, rv$ld50_results,
                          by = "cell_line", all = TRUE)
        
        combined_long <- combined %>%
          pivot_longer(
            cols      = c(IC50_uM, LD50_uM),
            names_to  = "Metric",
            values_to = "Value"
          ) %>%
          filter(!is.na(Value)) %>%
          mutate(Metric = ifelse(
            Metric == "IC50_uM", "IC50 (Viability)", "LD50 (Death)"
          ))
        
        cell_lines <- unique(combined_long$cell_line)
        colors <- get_color_palette(length(cell_lines))
        names(colors) <- cell_lines
        
        p <- ggplot(
          combined_long,
          aes(x = reorder(cell_line, Value), y = Value, fill = cell_line)
        ) +
          geom_bar(
            stat     = "identity",
            position = position_dodge(width = 0.8),
            width    = 0.7,
            color    = "black",
            linewidth = 0.5
          ) +
          geom_text(
            aes(label = sprintf("%.2f", Value)),
            position = position_dodge(width = 0.8),
            vjust    = -0.5,
            size     = 3.5,
            fontface = "bold"
          ) +
          facet_wrap(~ Metric, scales = "free_y") +
          scale_fill_manual(values = colors) +
          labs(
            title    = "IC50 vs LD50 Comparison Across Cell Lines",
            subtitle = "IC50: Half-maximal inhibitory concentration | LD50: Lethal dose 50%",
            x        = "Cell Line",
            y        = "Concentration (µM)"
          ) +
          theme_publication() +
          theme(
            legend.position = "none",
            axis.text.x     = element_text(angle = 45, hjust = 1),
            strip.text      = element_text(size = 11, face = "bold")
          )
        
        plot_ic50_ld50 <- p
      }
      
      # 4) Quadrant breakdown plot
      plot_quadrant <- NULL
      if (!is.null(rv$results) && nrow(rv$results) > 0) {
        quadrant_data <- rv$results %>%
          group_by(cell_line, concentration_uM) %>%
          summarise(
            Viable            = mean(pct_viable),
            `Early Apoptotic` = mean(pct_early_apoptotic),
            `Late Apoptotic`  = mean(pct_late_apoptotic),
            Necrotic          = mean(pct_necrotic),
            .groups           = "drop"
          ) %>%
          pivot_longer(
            cols      = c(Viable, `Early Apoptotic`, `Late Apoptotic`, Necrotic),
            names_to  = "Quadrant",
            values_to = "Percentage"
          )
        
        quadrant_data$Quadrant <- factor(
          quadrant_data$Quadrant,
          levels = c("Viable", "Early Apoptotic", "Late Apoptotic", "Necrotic")
        )
        
        p <- ggplot(
          quadrant_data,
          aes(x = as.factor(concentration_uM), y = Percentage, fill = Quadrant)
        ) +
          geom_bar(
            stat     = "identity",
            position = "stack",
            color    = "black",
            linewidth = 0.3
          ) +
          scale_fill_manual(values = c(
            "Viable"          = "#009E73",
            "Early Apoptotic" = "#F0E442",
            "Late Apoptotic"  = "#D55E00",
            "Necrotic"        = "#CC79A7"
          )) +
          facet_wrap(~ cell_line, scales = "free_x") +
          labs(
            title    = "Apoptosis Quadrant Breakdown by Concentration",
            subtitle = "Annexin V/PI: Viable (−/−), Early (+/−), Late (+/+), Necrotic (−/+)",
            x        = "Concentration (µM)",
            y        = "Percentage (%)",
            fill     = "Cell State"
          ) +
          theme_publication() +
          theme(
            axis.text.x = element_text(angle = 45, hjust = 1),
            strip.text  = element_text(size = 10, face = "bold")
          )
        
        plot_quadrant <- p
      }
      
      # Save viability plot (PNG, SVG, PDF)
      if (!is.null(plot_viability)) {
        ggsave(file.path(temp_dir, "viability_curves.png"),
               plot = plot_viability,
               width = 12, height = 8, dpi = 300)
        ggsave(file.path(temp_dir, "viability_curves.svg"),
               plot = plot_viability,
               width = 12, height = 8)
        ggsave(file.path(temp_dir, "viability_curves.pdf"),
               plot = plot_viability,
               width = 12, height = 8)
      }
      
      # Save death plot (PNG, SVG, PDF)
      if (!is.null(plot_death)) {
        ggsave(file.path(temp_dir, "death_curves.png"),
               plot = plot_death,
               width = 12, height = 8, dpi = 300)
        ggsave(file.path(temp_dir, "death_curves.svg"),
               plot = plot_death,
               width = 12, height = 8)
        ggsave(file.path(temp_dir, "death_curves.pdf"),
               plot = plot_death,
               width = 12, height = 8)
      }
      
      # Save IC50/LD50 comparison plot (PNG, SVG, PDF)
      if (!is.null(plot_ic50_ld50)) {
        ggsave(file.path(temp_dir, "ic50_ld50_comparison.png"),
               plot = plot_ic50_ld50,
               width = 10, height = 6, dpi = 300)
        ggsave(file.path(temp_dir, "ic50_ld50_comparison.svg"),
               plot = plot_ic50_ld50,
               width = 10, height = 6)
        ggsave(file.path(temp_dir, "ic50_ld50_comparison.pdf"),
               plot = plot_ic50_ld50,
               width = 10, height = 6)
      }
      
      # Save quadrant breakdown plot (PNG, SVG, PDF)
      if (!is.null(plot_quadrant)) {
        ggsave(file.path(temp_dir, "quadrant_breakdown.png"),
               plot = plot_quadrant,
               width = 14, height = 10, dpi = 300)
        ggsave(file.path(temp_dir, "quadrant_breakdown.svg"),
               plot = plot_quadrant,
               width = 14, height = 10)
        ggsave(file.path(temp_dir, "quadrant_breakdown.pdf"),
               plot = plot_quadrant,
               width = 14, height = 10)
      }
      
      # --- Build IC50/LD50 display table like the Results tab ---
      ic50_display <- NULL
      if (!is.null(rv$ic50_results) && !is.null(rv$ld50_results)) {
        combined <- merge(rv$ic50_results, rv$ld50_results, by = "cell_line", all = TRUE)
        combined <- combined %>%
          mutate(
            `IC50 [95% CI] µM` = ifelse(
              is.na(IC50_uM), "N/A",
              sprintf("%.2f [%.2f - %.2f]", IC50_uM, IC50_lower, IC50_upper)
            ),
            `LD50 [95% CI] µM` = ifelse(
              is.na(LD50_uM), "N/A",
              sprintf("%.2f [%.2f - %.2f]", LD50_uM, LD50_lower, LD50_upper)
            )
          )
        ic50_display <- combined[, c("cell_line", "IC50 [95% CI] µM", "LD50 [95% CI] µM")]
        colnames(ic50_display)[1] <- "Cell Line"
      }
      
      # --- Capture stats_comparison text into a string ---
      ic50_stats_text <- NULL
      if (!is.null(rv$ic50_results) && nrow(rv$ic50_results) >= 2) {
        ic50_stats_text <- paste(
          capture.output({
            cat("=== STATISTICAL COMPARISON OF IC50 VALUES ===\n\n")
            cat("Cell Line IC50 Summary:\n")
            print(rv$ic50_results %>% dplyr::select(cell_line, IC50_uM))
            cat("\nANOVA Test:\n")
            cat("H0: All cell lines have equal IC50 values\n")
            anova_result <- tryCatch(
              aov(IC50_uM ~ cell_line, data = rv$ic50_results),
              error = function(e) NULL
            )
            if (!is.null(anova_result)) {
              print(summary(anova_result))
            } else {
              cat("ANOVA could not be performed.\n")
            }
          }),
          collapse = "\n"
        )
      }
      
      
      # --- Prepare parameters for the R Markdown report ---
      analysis_settings <- data.frame(
        control_concentration_uM = rv$control_concentration,
        compensation_applied      = rv$comp_calculated,
        normalization_applied     = input$use_normalization
      )
      
      file_metadata <- rv$metadata[, c("name", "cell_line", "treatment_full", "concentration_uM", "replicate")]
      colnames(file_metadata)[1] <- "filename"
      
      comp_df <- NULL
      if (rv$comp_calculated) {
        comp_df <- as.data.frame(rv$comp_matrix)
        comp_df <- cbind(Channel = rownames(comp_df), comp_df)
      }
      
      # --- Render the HTML report with rmarkdown (fail-safe) ---
      report_file <- file.path(temp_dir, "FACS_IC50_report.html")

      tryCatch(
        {
          rmarkdown::render(
            input       = normalizePath("facs_report.Rmd"),      # same directory as app.R
            output_file = report_file,
            params = list(
              analysis_settings = analysis_settings,
              ic50_table        = ic50_display,
              ld50_table        = rv$ld50_results,
              qc_table          = rv$qc_data,
              advanced_metrics  = rv$advanced_metrics,
              results_raw       = rv$results,
              cell_counts       = rv$cell_counts,
              comp_matrix       = comp_df,
              file_metadata     = file_metadata,
              ic50_stats        = ic50_stats_text,
              viability_plot    = plot_viability,
              death_plot        = plot_death,
              comp_plot         = rv$comp_plot_obj,      # compensation plot still comes from earlier
              ic50_ld50_plot    = plot_ic50_ld50,
              quadrant_plot     = plot_quadrant,
              gate_summary      = gate_summary,
              gate_plots        = rv$gate_plots
            ),
            envir = new.env(parent = globalenv()),
            quiet = TRUE
          )
        },
        error = function(e) {
          # optional: show a notification inside the app
          showNotification(
            paste("Report generation failed:", e$message),
            type    = "error",
            duration = 10
          )
        }
      )

      if (!file.exists(report_file)) {
        stop("FACS_IC50_report.html was not generated — check pandoc or Rmd path")
      }

      # Create ZIP file
      files_to_zip <- list.files(temp_dir, full.names = TRUE)
      files_to_zip <- files_to_zip[grep(
        "IC50_|LD50_|raw_|file_|analysis_|compensation_|viability_|death_|ic50_ld50_|quadrant_|QC_|outlier_|cell_counts_|advanced_|gate_|FACS_IC50_report\\\\.html",
        basename(files_to_zip)
      )]

      zip::zip(zipfile = file, files = files_to_zip, mode = "cherry-pick")
    },
    contentType = "application/zip"
  )
}  # end of server()

# ============================================================================
# LAUNCH APP
# ============================================================================

shinyApp(ui = ui, server = server)
