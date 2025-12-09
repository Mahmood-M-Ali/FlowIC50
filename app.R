# ============================================================================
# FACS IC50 ANALYSIS - COMPLETE FUNCTIONAL VERSION
# With compensation, control selection, and ZIP download
# ============================================================================

if (!requireNamespace("BiocManager", quietly = TRUE)) {
  install.packages("BiocManager")
}
if (!requireNamespace("flowCore", quietly = TRUE)) {
  BiocManager::install("flowCore")
}

required_packages <- c("shiny", "bslib", "ggplot2", "dplyr", "tidyr", 
                       "readr", "stringr", "drc", "scales", "sp", "zip")
for(pkg in required_packages) {
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
})

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
      .comp-box h6 { margin: 0 0 8px 0; font-weight: 600; color: #e65100; }
      .comp-box p { font-size: 0.85em; margin: 5px 0; color: #424242; 
      }
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
      h1("Flow Cytometry IC50 Analyzer"),
      p("Annexin V/PI apoptosis assay with gating, compensation, and IC50 determination")),
  
  div(class = "content-wrapper",
      fluidRow(
        column(3, class = "sidebar",
                div(style = "background: linear-gradient(135deg, #fff9c4 0%, #fff59d 100%); 
               padding: 15px; border-radius: 8px; border-left: 5px solid #f57f17; 
               margin-bottom: 20px;",
                    h6(style = "margin: 0 0 10px 0; color: #f57f17; font-weight: 600;",
                       icon("tools"), " Need to Rename Files?"),
                    p(style = "font-size: 0.9em; margin: 5px 0; color: #424242;",
                      "Use our automated file renaming tool to quickly format your FCS files 
         to match the required naming convention."),
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
                           
                           tabPanel("Results",
                                    icon = icon("chart-line"),
                                    br(),
                                    h4("IC50 Results"),
                                    tableOutput("ic50_table"),
                                    hr(),
                                    downloadButton("download_results", "Download CSV", class = "btn-primary")),
                           
                           tabPanel("Plots",
                                    icon = icon("chart-bar"),
                                    br(),
                                    h4("Dose-Response Curves"),
                                    plotOutput("dose_response_plot", height = "600px"),
                                    hr(),
                                    h4("IC50 Comparison"),
                                    plotOutput("ic50_comparison_plot", height = "500px"))
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
                 tags$img(src = "https://licensebuttons.net/l/by-nc/4.0/88x31.png",
                          alt = "CC BY-NC 4.0 License",
                          style = "border: 0;")))
  )
)

# ===== END OF PART 1 =====
# Paste PART 2 immediately below this line
# ============================================================================
# SERVER
# ============================================================================

server <- function(input, output, session) {
  
  rv <- reactiveValues(
    metadata = NULL,
    thresholds = NULL,
    results = NULL,
    ic50_results = NULL,
    ic50_data = NULL,
    predicted_curves = NULL,
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
    dose_response_plot_obj = NULL,
    ic50_comparison_plot_obj = NULL
  )
  
  # === COMPENSATION CALCULATION ===
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
      p(style = "font-size: 0.9em; color: #666;",
        icon("info-circle"), " Corrects spectral overlap between Annexin V-FITC (BL1-A) and PI (BL2-A).")
    )
  })
  
  output$comp_matrix_display <- renderPrint({
    req(rv$comp_matrix)
    print(round(rv$comp_matrix, 4))
  })
  
  # === FILE PARSING - FIXED ===
  observeEvent(input$files, {
    req(input$files)
    
    metadata <- input$files
    metadata$cell_line <- character(nrow(metadata))
    metadata$replicate <- character(nrow(metadata))
    metadata$treatment_full <- character(nrow(metadata))
    metadata$concentration_uM <- numeric(nrow(metadata))
    
    for(i in 1:nrow(metadata)) {
      name_clean <- str_replace(metadata$name[i], "\\.[fF][cC][sS]$", "")
      parts <- str_split(name_clean, "_")[[1]]  # FIXED: underscore not asterisk
      
      if(length(parts) >= 3) {
        metadata$cell_line[i] <- parts[1]
        metadata$replicate[i] <- parts[length(parts)]
        metadata$treatment_full[i] <- paste(parts[2:(length(parts)-1)], collapse = "_")  # FIXED
        
        conc <- as.numeric(str_extract(metadata$treatment_full[i], "[0-9.]+(?=[uU][mM])"))  # FIXED: decimal support
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
  
  # === CONTROL SELECTION UI ===
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
  
  # === WORKFLOW STATUS ===
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
  
  # ===== END OF PART 2 =====
  # Paste PART 3 immediately below this line
  # === GATING UI ===
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
    
    # === STEP 1: FSC/SSC ===
    if(rv$gating_step == "fsc_ssc") {
      fsc_data <- data_raw[, "FSC-A"]
      ssc_data <- data_raw[, "SSC-A"]
      
      fsc_q10 <- quantile(fsc_data, 0.10, na.rm = TRUE)
      fsc_q95 <- quantile(fsc_data, 0.95, na.rm = TRUE)
      ssc_q10 <- quantile(ssc_data, 0.10, na.rm = TRUE)
      ssc_q95 <- quantile(ssc_data, 0.95, na.rm = TRUE)
      
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
        
        div(class = "gate-mode-switch",
            radioButtons("gate_mode_fsc", "Gating Mode:",
                         choices = c("Rectangle (Sliders)" = "rectangle",
                                     "Polygon (Click)" = "polygon"),
                         selected = "rectangle", inline = TRUE),
            conditionalPanel(
              condition = "input.gate_mode_fsc == 'polygon'",
              div(style = "background: #fff3cd; padding: 10px; border-radius: 5px; margin-top: 10px;",
                  p(icon("mouse-pointer"), " Click on plot to add points. Close polygon when done.", 
                    style = "margin: 5px 0;"),
                  actionButton("clear_polygon_fsc", "Clear Points", 
                               class = "btn-warning btn-sm", icon = icon("eraser")),
                  actionButton("close_polygon_fsc", "Close Polygon", 
                               class = "btn-success btn-sm", icon = icon("check-circle"))
              )
            )
        ),
        
        conditionalPanel(
          condition = "input.gate_mode_fsc == 'rectangle'",
          fluidRow(
            column(6,
                   sliderInput("fsc_min", "FSC-A Min:", 
                               min = 0, max = max(fsc_data),
                               value = fsc_q10, step = 1000),
                   sliderInput("fsc_max", "FSC-A Max:", 
                               min = 0, max = max(fsc_data),
                               value = fsc_q95, step = 1000)
            ),
            column(6,
                   sliderInput("ssc_min", "SSC-A Min:", 
                               min = 0, max = max(ssc_data),
                               value = ssc_q10, step = 1000),
                   sliderInput("ssc_max", "SSC-A Max:", 
                               min = 0, max = max(ssc_data),
                               value = ssc_q95, step = 1000)
            )
          )
        ),
        
        plotOutput("fsc_ssc_plot", height = "500px", click = "fsc_ssc_click"),
        div(class = "alert alert-secondary", verbatimTextOutput("fsc_ssc_stats")),
        hr(),
        actionButton("next_to_singlets", "Next: Singlet Gate →", 
                     class = "btn-primary btn-lg btn-block", icon = icon("arrow-right"))
      ))
    }
    
    # === STEP 2: SINGLETS ===
    else if(rv$gating_step == "singlets") {
      req(rv$temp_gated_data_cells)
      
      data_cells <- rv$temp_gated_data_cells
      fsc_a <- data_cells[, "FSC-A"]
      fsc_h <- data_cells[, "FSC-H"]
      
      fsc_a_q10 <- quantile(fsc_a, 0.10, na.rm = TRUE)
      fsc_a_q95 <- quantile(fsc_a, 0.95, na.rm = TRUE)
      fsc_h_q10 <- quantile(fsc_h, 0.10, na.rm = TRUE)
      fsc_h_q95 <- quantile(fsc_h, 0.95, na.rm = TRUE)
      
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
        
        div(class = "gate-mode-switch",
            radioButtons("gate_mode_singlet", "Gating Mode:",
                         choices = c("Rectangle (Sliders)" = "rectangle",
                                     "Polygon (Click)" = "polygon"),
                         selected = "rectangle", inline = TRUE),
            conditionalPanel(
              condition = "input.gate_mode_singlet == 'polygon'",
              div(style = "background: #fff3cd; padding: 10px; border-radius: 5px; margin-top: 10px;",
                  p(icon("mouse-pointer"), " Click on plot to add points. Close polygon when done.", 
                    style = "margin: 5px 0;"),
                  actionButton("clear_polygon_singlet", "Clear Points", 
                               class = "btn-warning btn-sm", icon = icon("eraser")),
                  actionButton("close_polygon_singlet", "Close Polygon", 
                               class = "btn-success btn-sm", icon = icon("check-circle"))
              )
            )
        ),
        
        conditionalPanel(
          condition = "input.gate_mode_singlet == 'rectangle'",
          fluidRow(
            column(6,
                   sliderInput("fsc_a_min", "FSC-A Min:", 
                               min = 0, max = max(fsc_a),
                               value = fsc_a_q10, step = 1000),
                   sliderInput("fsc_a_max", "FSC-A Max:", 
                               min = 0, max = max(fsc_a),
                               value = fsc_a_q95, step = 1000)
            ),
            column(6,
                   sliderInput("fsc_h_min", "FSC-H Min:", 
                               min = 0, max = max(fsc_h),
                               value = fsc_h_q10, step = 1000),
                   sliderInput("fsc_h_max", "FSC-H Max:", 
                               min = 0, max = max(fsc_h),
                               value = fsc_h_q95, step = 1000)
            )
          )
        ),
        
        plotOutput("singlet_plot", height = "500px", click = "singlet_click"),
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
    
    # === STEP 3: ANNEXIN/PI ===
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
        
        div(class = "gate-mode-switch",
            radioButtons("gate_mode_annexin", "Gating Mode:",
                         choices = c("Rectangle (Sliders)" = "rectangle",
                                     "Polygon (Click)" = "polygon"),
                         selected = "rectangle", inline = TRUE),
            conditionalPanel(
              condition = "input.gate_mode_annexin == 'polygon'",
              div(style = "background: #fff3cd; padding: 10px; border-radius: 5px; margin-top: 10px;",
                  p(icon("mouse-pointer"), " Click to define VIABLE region (lower-left quadrant).", 
                    style = "margin: 5px 0;"),
                  actionButton("clear_polygon_annexin", "Clear Points", 
                               class = "btn-warning btn-sm", icon = icon("eraser")),
                  actionButton("close_polygon_annexin", "Close Polygon", 
                               class = "btn-success btn-sm", icon = icon("check-circle"))
              )
            )
        ),
        
        conditionalPanel(
          condition = "input.gate_mode_annexin == 'rectangle'",
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
          )
        ),
        
        plotOutput("annexin_plot", height = "500px", click = "annexin_click"),
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
  
  # === PLOT OUTPUTS ===
  
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
    
    if(!is.null(input$gate_mode_fsc) && input$gate_mode_fsc == "rectangle") {
      p <- p + geom_rect(aes(xmin = input$fsc_min, xmax = input$fsc_max,
                             ymin = input$ssc_min, ymax = input$ssc_max),
                         fill = NA, color = "red", linewidth = 2)
    } else if(nrow(rv$polygon_points) > 0) {
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
    
    if(!is.null(input$gate_mode_singlet) && input$gate_mode_singlet == "rectangle") {
      p <- p + geom_rect(aes(xmin = input$fsc_a_min, xmax = input$fsc_a_max,
                             ymin = input$fsc_h_min, ymax = input$fsc_h_max),
                         fill = NA, color = "red", linewidth = 2)
    } else if(nrow(rv$polygon_points) > 0) {
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
    
    if(!is.null(input$gate_mode_annexin) && input$gate_mode_annexin == "rectangle") {
      p <- p + 
        geom_vline(xintercept = input$annexin_threshold, color = "red", linewidth = 2) +
        geom_hline(yintercept = input$pi_threshold, color = "red", linewidth = 2)
    } else if(nrow(rv$polygon_points) > 0) {
      p <- p + geom_path(data = rv$polygon_points, aes(x = x, y = y),
                         color = "red", linewidth = 2) +
        geom_point(data = rv$polygon_points, aes(x = x, y = y),
                   color = "red", size = 4, shape = 21, fill = "white", stroke = 2)
    }
    p
  })
  
  # === STATS OUTPUTS ===
  
  output$fsc_ssc_stats <- renderText({
    req(rv$current_gating_data_fsc)
    total <- length(rv$current_gating_data_fsc$fsc)
    
    if(!is.null(input$gate_mode_fsc) && input$gate_mode_fsc == "rectangle") {
      in_gate <- rv$current_gating_data_fsc$fsc >= input$fsc_min &
        rv$current_gating_data_fsc$fsc <= input$fsc_max &
        rv$current_gating_data_fsc$ssc >= input$ssc_min &
        rv$current_gating_data_fsc$ssc <= input$ssc_max
      kept <- sum(in_gate)
    } else if(nrow(rv$polygon_points) >= 3) {
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
    
    if(!is.null(input$gate_mode_singlet) && input$gate_mode_singlet == "rectangle") {
      in_gate <- rv$current_gating_data_singlet$fsc_a >= input$fsc_a_min &
        rv$current_gating_data_singlet$fsc_a <= input$fsc_a_max &
        rv$current_gating_data_singlet$fsc_h >= input$fsc_h_min &
        rv$current_gating_data_singlet$fsc_h <= input$fsc_h_max
      kept <- sum(in_gate)
    } else if(nrow(rv$polygon_points) >= 3) {
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
    
    if(!is.null(input$gate_mode_annexin) && input$gate_mode_annexin == "rectangle") {
      viable <- sum(annexin < input$annexin_threshold & pi < input$pi_threshold)
    } else if(nrow(rv$polygon_points) >= 3) {
      viable <- sum(point.in.polygon(annexin, pi, 
                                     rv$polygon_points$x, rv$polygon_points$y) > 0)
    } else {
      viable <- 0
    }
    
    viable_pct <- round(viable/total*100, 1)
    status <- if(viable_pct >= 70 && viable_pct <= 95) "✓ Good" 
    else if(viable_pct > 95) "⚠ Too high" 
    else "⚠ Too low"
    
    paste0("Viable cells: ", viable_pct, "% ", status, "\nExpected for control: 70-95%")
  })
  
  # === CLICK HANDLERS ===
  
  observeEvent(input$fsc_ssc_click, {
    if(!is.null(input$gate_mode_fsc) && input$gate_mode_fsc == "polygon") {
      click <- input$fsc_ssc_click
      rv$polygon_points <- rbind(rv$polygon_points, data.frame(x = click$x, y = click$y))
    }
  })
  
  observeEvent(input$singlet_click, {
    if(!is.null(input$gate_mode_singlet) && input$gate_mode_singlet == "polygon") {
      click <- input$singlet_click
      rv$polygon_points <- rbind(rv$polygon_points, data.frame(x = click$x, y = click$y))
    }
  })
  
  observeEvent(input$annexin_click, {
    if(!is.null(input$gate_mode_annexin) && input$gate_mode_annexin == "polygon") {
      click <- input$annexin_click
      rv$polygon_points <- rbind(rv$polygon_points, data.frame(x = click$x, y = click$y))
    }
  })
  
  # Clear polygon buttons
  observeEvent(input$clear_polygon_fsc, {
    rv$polygon_points <- data.frame(x = numeric(), y = numeric())
  })
  
  observeEvent(input$clear_polygon_singlet, {
    rv$polygon_points <- data.frame(x = numeric(), y = numeric())
  })
  
  observeEvent(input$clear_polygon_annexin, {
    rv$polygon_points <- data.frame(x = numeric(), y = numeric())
  })
  
  # Close polygon buttons
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
  
  observeEvent(input$close_polygon_annexin, {
    if(nrow(rv$polygon_points) >= 3) {
      rv$polygon_points <- rbind(rv$polygon_points, rv$polygon_points[1, ])
      showNotification("Polygon closed!", type = "message")
    }
  })
  
  # === NAVIGATION BETWEEN STEPS ===
  
  observeEvent(input$next_to_singlets, {
    req(rv$current_gating_data_fsc)
    
    if(!is.null(input$gate_mode_fsc) && input$gate_mode_fsc == "polygon") {
      if(nrow(rv$polygon_points) < 3) {
        showNotification("Draw a polygon first (at least 3 points)", type = "error")
        return()
      }
      rv$temp_fsc_ssc_gates <- list(type = "polygon", polygon = rv$polygon_points)
      in_gate <- point.in.polygon(rv$current_gating_data_fsc$fsc,
                                  rv$current_gating_data_fsc$ssc,
                                  rv$polygon_points$x, rv$polygon_points$y) > 0
      rv$temp_gated_data_cells <- rv$current_gating_data_fsc$data_raw[in_gate, ]
    } else {
      rv$temp_fsc_ssc_gates <- list(type = "rectangle",
                                    fsc_min = input$fsc_min, fsc_max = input$fsc_max,
                                    ssc_min = input$ssc_min, ssc_max = input$ssc_max)
      in_gate <- rv$current_gating_data_fsc$fsc >= input$fsc_min &
        rv$current_gating_data_fsc$fsc <= input$fsc_max &
        rv$current_gating_data_fsc$ssc >= input$ssc_min &
        rv$current_gating_data_fsc$ssc <= input$ssc_max
      rv$temp_gated_data_cells <- rv$current_gating_data_fsc$data_raw[in_gate, ]
    }
    
    if(nrow(rv$temp_gated_data_cells) == 0) {
      showNotification("No cells in gate! Adjust gate.", type = "error")
      return()
    }
    
    rv$polygon_points <- data.frame(x = numeric(), y = numeric())
    rv$gating_step <- "singlets"
  })
  
  observeEvent(input$next_to_annexin, {
    req(rv$current_gating_data_singlet)
    
    if(!is.null(input$gate_mode_singlet) && input$gate_mode_singlet == "polygon") {
      if(nrow(rv$polygon_points) < 3) {
        showNotification("Draw a polygon first (at least 3 points)", type = "error")
        return()
      }
      rv$temp_singlet_gates <- list(type = "polygon", polygon = rv$polygon_points)
      in_gate <- point.in.polygon(rv$current_gating_data_singlet$fsc_a,
                                  rv$current_gating_data_singlet$fsc_h,
                                  rv$polygon_points$x, rv$polygon_points$y) > 0
      rv$temp_gated_data_singlets <- rv$current_gating_data_singlet$data_cells[in_gate, ]
    } else {
      rv$temp_singlet_gates <- list(type = "rectangle",
                                    fsc_a_min = input$fsc_a_min, fsc_a_max = input$fsc_a_max,
                                    fsc_h_min = input$fsc_h_min, fsc_h_max = input$fsc_h_max)
      in_gate <- rv$current_gating_data_singlet$fsc_a >= input$fsc_a_min &
        rv$current_gating_data_singlet$fsc_a <= input$fsc_a_max &
        rv$current_gating_data_singlet$fsc_h >= input$fsc_h_min &
        rv$current_gating_data_singlet$fsc_h <= input$fsc_h_max
      rv$temp_gated_data_singlets <- rv$current_gating_data_singlet$data_cells[in_gate, ]
    }
    
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
    
    if(!is.null(input$gate_mode_annexin) && input$gate_mode_annexin == "polygon") {
      if(nrow(rv$polygon_points) < 3) {
        showNotification("Draw a polygon first (at least 3 points)", type = "error")
        return()
      }
      annexin_pi_gate <- list(type = "polygon", polygon = rv$polygon_points)
    } else {
      annexin_pi_gate <- list(type = "rectangle",
                              annexin = input$annexin_threshold, pi = input$pi_threshold)
    }
    
    rv$thresholds[[cell_line]] <- list(
      fsc_ssc = rv$temp_fsc_ssc_gates,
      singlets = rv$temp_singlet_gates,
      annexin_pi = annexin_pi_gate
    )
    
    showNotification(paste("Gates saved for", cell_line), type = "message")
    
    rv$current_cell_line_index <- rv$current_cell_line_index + 1
    rv$gating_step <- "fsc_ssc"
    rv$polygon_points <- data.frame(x = numeric(), y = numeric())
    rv$current_gating_data <- NULL
    rv$current_gating_data_fsc <- NULL
    rv$current_gating_data_singlet <- NULL
    rv$temp_fsc_ssc_gates <- NULL
    rv$temp_singlet_gates <- NULL
    rv$temp_gated_data_cells <- NULL
    rv$temp_gated_data_singlets <- NULL
  })
  
  observeEvent(input$skip_line, {
    rv$current_cell_line_index <- rv$current_cell_line_index + 1
    rv$gating_step <- "fsc_ssc"
    rv$polygon_points <- data.frame(x = numeric(), y = numeric())
  })
  
  # ===== END OF PART 3 =====
  # Paste PART 4 immediately below this line
  # === ANALYSIS WITH COMPENSATION SUPPORT ===
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
          
          # Gate 1: FSC/SSC
          fsc_ssc_gate <- thresholds$fsc_ssc
          if(fsc_ssc_gate$type == "polygon") {
            poly <- fsc_ssc_gate$polygon
            in_gate1 <- point.in.polygon(data_raw[, "FSC-A"], data_raw[, "SSC-A"],
                                         poly$x, poly$y) > 0
          } else {
            in_gate1 <- data_raw[, "FSC-A"] >= fsc_ssc_gate$fsc_min &
              data_raw[, "FSC-A"] <= fsc_ssc_gate$fsc_max &
              data_raw[, "SSC-A"] >= fsc_ssc_gate$ssc_min &
              data_raw[, "SSC-A"] <= fsc_ssc_gate$ssc_max
          }
          data_cells <- data_raw[in_gate1, ]
          if(nrow(data_cells) == 0) next
          
          # Gate 2: Singlets
          singlet_gate <- thresholds$singlets
          if(singlet_gate$type == "polygon") {
            poly <- singlet_gate$polygon
            in_gate2 <- point.in.polygon(data_cells[, "FSC-A"], data_cells[, "FSC-H"],
                                         poly$x, poly$y) > 0
          } else {
            in_gate2 <- data_cells[, "FSC-A"] >= singlet_gate$fsc_a_min &
              data_cells[, "FSC-A"] <= singlet_gate$fsc_a_max &
              data_cells[, "FSC-H"] >= singlet_gate$fsc_h_min &
              data_cells[, "FSC-H"] <= singlet_gate$fsc_h_max
          }
          data_singlets <- data_cells[in_gate2, ]
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
          if(annexin_pi_gate$type == "polygon") {
            poly <- annexin_pi_gate$polygon
            viable <- sum(point.in.polygon(annexin, pi, poly$x, poly$y) > 0)
          } else {
            viable <- sum(annexin < annexin_pi_gate$annexin & pi < annexin_pi_gate$pi)
          }
          
          total <- length(annexin)
          
          results <- rbind(results, data.frame(
            cell_line = cell_line,
            concentration_uM = row$concentration_uM,
            replicate = row$replicate,
            pct_viable = (viable/total)*100,
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
      
      # IC50 CALCULATION WITH ROBUST FITTING
      ic50_data <- results %>%
        group_by(cell_line, concentration_uM) %>%
        summarise(mean_viable = mean(pct_viable, na.rm = TRUE),
                  sd_viable = sd(pct_viable, na.rm = TRUE),
                  n = n(),
                  se_viable = sd_viable / sqrt(n),
                  .groups = "drop") %>%
        mutate(concentration_uM = as.numeric(as.character(concentration_uM)))
      
      ic50_results <- data.frame()
      predicted_curves <- data.frame()
      
      for(line in unique(ic50_data$cell_line)) {
        if(is.na(line)) next
        
        line_data <- ic50_data %>% filter(cell_line == line)
        if(nrow(line_data) < 4) {
          message("Skipping IC50 for ", line, ": insufficient data points (", nrow(line_data), " < 4)")
          next
        }
        
        line_data$dose <- line_data$concentration_uM + 0.1
        
        # MULTI-STRATEGY FITTING APPROACH
        fit <- NULL
        fit_method <- "none"
        
        # Strategy 1: Standard LL.4 with auto starting values
        if(is.null(fit)) {
          fit <- tryCatch({
            drm(mean_viable ~ dose, data = line_data, fct = LL.4())
          }, error = function(e) NULL)
          if(!is.null(fit)) fit_method <- "LL.4 auto"
        }
        
        # Strategy 2: LL.4 with manual starting values based on data
        if(is.null(fit)) {
          fit <- tryCatch({
            min_v <- min(line_data$mean_viable, na.rm = TRUE)
            max_v <- max(line_data$mean_viable, na.rm = TRUE)
            mid_dose <- median(line_data$dose)
            drm(mean_viable ~ dose, data = line_data, 
                fct = LL.4(names = c("Slope", "Lower", "Upper", "ED50")),
                start = c(1, min_v, max_v, mid_dose))
          }, error = function(e) NULL)
          if(!is.null(fit)) fit_method <- "LL.4 manual"
        }
        
        # Strategy 3: 3-parameter log-logistic (simpler, more robust)
        if(is.null(fit)) {
          fit <- tryCatch({
            drm(mean_viable ~ dose, data = line_data, fct = LL.3())
          }, error = function(e) NULL)
          if(!is.null(fit)) fit_method <- "LL.3"
        }
        
        # Strategy 4: Weibull model
        if(is.null(fit)) {
          fit <- tryCatch({
            drm(mean_viable ~ dose, data = line_data, fct = W1.4())
          }, error = function(e) NULL)
          if(!is.null(fit)) fit_method <- "Weibull"
        }
        
        # Strategy 5: 2-parameter Weibull (most robust)
        if(is.null(fit)) {
          fit <- tryCatch({
            drm(mean_viable ~ dose, data = line_data, fct = W1.2())
          }, error = function(e) NULL)
          if(!is.null(fit)) fit_method <- "Weibull-2"
        }
        
        # If all strategies failed
        if(is.null(fit)) {
          message("IC50 fit failed for ", line, ": all strategies exhausted")
          next
        }
        
        message("IC50 fit succeeded for ", line, " using ", fit_method)
        
        # Extract IC50 (ED50 = EC50)
        tryCatch({
          # Use ED function to get IC50
          ic50_result <- ED(fit, 50, interval = "delta", display = FALSE)
          ec50_val <- ic50_result[1]
          ec50_lower <- ic50_result[3]
          ec50_upper <- ic50_result[4]
          
          ic50_results <- rbind(ic50_results, data.frame(
            cell_line = line,
            IC50_uM = ec50_val - 0.1,
            IC50_lower = ec50_lower - 0.1,
            IC50_upper = ec50_upper - 0.1,
            fit_method = fit_method,
            stringsAsFactors = FALSE
          ))
          
          # Generate predicted curve
          pred_doses <- seq(min(line_data$dose), max(line_data$dose), length.out = 200)
          pred_data <- data.frame(dose = pred_doses)
          pred_data$predicted_viable <- predict(fit, newdata = pred_data)
          pred_data$concentration_uM <- pred_doses - 0.1
          pred_data$cell_line <- line
          
          predicted_curves <- rbind(predicted_curves, pred_data)
          
        }, error = function(e) {
          message("IC50 extraction failed for ", line, ": ", e$message)
        })
      }
      
      # Store results
      rv$ic50_results <- ic50_results
      rv$ic50_data <- ic50_data
      rv$predicted_curves <- predicted_curves
      
      if(nrow(ic50_results) == 0) {
        showNotification("Analysis complete, but IC50 fitting failed for all cell lines. Check that you have sufficient concentration range and dose-response.", 
                         type = "warning", duration = 10)
      } else if(nrow(ic50_results) < length(unique(ic50_data$cell_line))) {
        showNotification(paste("Analysis complete! IC50 calculated for", nrow(ic50_results), "of", 
                               length(unique(ic50_data$cell_line)), "cell lines."), 
                         type = "message")
      } else {
        showNotification("Analysis complete!", type = "message")
      }
    })
  })
  
  # === OUTPUT TABLES & PLOTS ===
  
  output$ic50_table <- renderTable({
    req(rv$ic50_results)
    
    if(nrow(rv$ic50_results) == 0) {
      return(data.frame(Message = "No IC50 values could be calculated. Check data quality and concentration range."))
    }
    
    rv$ic50_results %>%
      mutate(`IC50 [95% CI]` = sprintf("%.2f [%.2f - %.2f] µM", 
                                       IC50_uM, IC50_lower, IC50_upper),
             Method = fit_method) %>%
      dplyr::select(`Cell Line` = cell_line, `IC50 [95% CI]`, `Fit Method` = Method)
  }, striped = TRUE, hover = TRUE, bordered = TRUE)
  
  output$dose_response_plot <- renderPlot({
    req(rv$ic50_data)
    
    # If no predicted curves, just plot the data points
    if(is.null(rv$predicted_curves) || nrow(rv$predicted_curves) == 0) {
      p <- ggplot(rv$ic50_data, aes(x = concentration_uM, y = mean_viable, color = cell_line)) +
        geom_point(size = 4, shape = 21, fill = "white", stroke = 1.5) +
        geom_errorbar(aes(ymin = mean_viable - se_viable, 
                          ymax = mean_viable + se_viable),
                      width = 0.15, linewidth = 1) +
        scale_x_continuous(trans = scales::pseudo_log_trans(base = 10),
                           breaks = c(0, 1, 10, 20, 50, 100)) +
        scale_y_continuous(limits = c(0, 110), breaks = seq(0, 100, 20)) +
        labs(title = "Dose-Response Data",
             subtitle = "IC50 fitting failed - check data quality",
             x = "Concentration (µM)", y = "Cell Viability (%)", color = "Cell Line") +
        theme_bw(base_size = 16) +
        theme(legend.position = "right",
              plot.title = element_text(face = "bold", hjust = 0.5, size = 18),
              plot.subtitle = element_text(hjust = 0.5, size = 12, color = "red"),
              panel.grid.minor = element_blank())
      
      rv$dose_response_plot_obj <- p
      return(p)
    }
    
    # Plot with curves
    p <- ggplot() +
      geom_line(data = rv$predicted_curves,
                aes(x = concentration_uM, y = predicted_viable, color = cell_line),
                linewidth = 1.5, alpha = 0.9) +
      geom_point(data = rv$ic50_data,
                 aes(x = concentration_uM, y = mean_viable, color = cell_line),
                 size = 4, shape = 21, fill = "white", stroke = 1.5) +
      geom_errorbar(data = rv$ic50_data,
                    aes(x = concentration_uM, 
                        ymin = mean_viable - se_viable, 
                        ymax = mean_viable + se_viable,
                        color = cell_line),
                    width = 0.15, linewidth = 1) +
      scale_x_continuous(trans = scales::pseudo_log_trans(base = 10),
                         breaks = c(0, 1, 10, 20, 50, 100)) +
      scale_y_continuous(limits = c(0, 110), breaks = seq(0, 100, 20)) +
      labs(title = "Dose-Response Curves with IC50 Determination",
           subtitle = "Multiple fitting strategies attempted for optimal curve fit",
           x = "Concentration (µM)", y = "Cell Viability (%)", color = "Cell Line") +
      theme_bw(base_size = 16) +
      theme(legend.position = "right",
            plot.title = element_text(face = "bold", hjust = 0.5, size = 18),
            plot.subtitle = element_text(hjust = 0.5, size = 12, color = "gray30"),
            panel.grid.minor = element_blank())
    
    rv$dose_response_plot_obj <- p
    p
  })
  
  output$ic50_comparison_plot <- renderPlot({
    req(rv$ic50_results)
    
    if(nrow(rv$ic50_results) == 0) {
      return(ggplot() + 
               annotate("text", x = 0.5, y = 0.5, 
                        label = "No IC50 values to display\nCheck data quality and concentration range",
                        size = 8, color = "red") +
               theme_void())
    }
    
    p <- ggplot(rv$ic50_results, aes(x = reorder(cell_line, IC50_uM), 
                                     y = IC50_uM, fill = cell_line)) +
      geom_bar(stat = "identity", width = 0.6, color = "black", linewidth = 0.8) +
      geom_errorbar(aes(ymin = IC50_lower, ymax = IC50_upper), 
                    width = 0.25, linewidth = 1) +
      geom_text(aes(label = sprintf("%.2f µM", IC50_uM)), 
                vjust = -0.5, size = 5, fontface = "bold") +
      labs(title = "IC50 Comparison Across Cell Lines",
           subtitle = "Error bars represent 95% confidence intervals",
           x = "Cell Line", y = "IC50 (µM)") +
      theme_bw(base_size = 16) +
      theme(legend.position = "none",
            plot.title = element_text(face = "bold", hjust = 0.5, size = 18),
            plot.subtitle = element_text(hjust = 0.5, size = 12, color = "gray30"),
            axis.text.x = element_text(angle = 0))
    
    rv$ic50_comparison_plot_obj <- p
    p
  })
  
  output$download_results <- downloadHandler(
    filename = function() { paste0("IC50_results_", Sys.Date(), ".csv") },
    content = function(file) { 
      req(rv$ic50_results)
      write_csv(rv$ic50_results, file) 
    }
  )
  
  # === ZIP DOWNLOAD WITH ALL RESULTS ===
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
      write_csv(rv$results, file.path(temp_dir, "raw_viability_data.csv"))
      
      # Save IC50 results CSV (if available)
      if(!is.null(rv$ic50_results) && nrow(rv$ic50_results) > 0) {
        write_csv(rv$ic50_results, file.path(temp_dir, "IC50_results.csv"))
      }
      
      # Save metadata summary
      metadata_summary <- rv$metadata %>%
        dplyr::select(filename = name, cell_line, treatment_full, concentration_uM, replicate)
      write_csv(metadata_summary, file.path(temp_dir, "file_metadata.csv"))
      
      # Save control summary
      control_summary <- data.frame(
        control_concentration_uM = rv$control_concentration,
        compensation_applied = rv$comp_calculated
      )
      write_csv(control_summary, file.path(temp_dir, "analysis_settings.csv"))
      
      # Save compensation matrix if calculated
      if(rv$comp_calculated) {
        comp_df <- as.data.frame(rv$comp_matrix)
        comp_df <- cbind(Channel = rownames(comp_df), comp_df)
        write_csv(comp_df, file.path(temp_dir, "compensation_matrix.csv"))
      }
      
      # Save dose-response plot
      if(!is.null(rv$dose_response_plot_obj)) {
        ggsave(file.path(temp_dir, "dose_response_curves.png"),
               plot = rv$dose_response_plot_obj,
               width = 12, height = 8, dpi = 300)
      }
      
      # Save IC50 comparison plot (if available)
      if(!is.null(rv$ic50_comparison_plot_obj) && !is.null(rv$ic50_results) && nrow(rv$ic50_results) > 0) {
        ggsave(file.path(temp_dir, "ic50_comparison.png"),
               plot = rv$ic50_comparison_plot_obj,
               width = 10, height = 6, dpi = 300)
      }
      
      # Create ZIP file
      files_to_zip <- list.files(temp_dir, full.names = TRUE)
      files_to_zip <- files_to_zip[grep("IC50_|raw_|file_|analysis_|compensation_|dose_|ic50_", 
                                        basename(files_to_zip))]
      
      zip::zip(zipfile = file, files = files_to_zip, mode = "cherry-pick")
    },
    contentType = "application/zip"
  )
}

# ============================================================================
# LAUNCH APP
# ============================================================================

shinyApp(ui = ui, server = server)

# ===== END OF PART 4 =====
# This is the end of the complete app.R file!
