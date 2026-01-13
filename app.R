# ============================================================================
# ============================================================================
# FACS IC50 ANALYSIS - ENHANCED VERSION
# ============================================================================
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
  "outliers", "DescTools", "pheatmap", "gridExtra", "rmarkdown", "kableExtra", "DT",
  "ggrepel", "knitr", "htmltools"
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
  library(DT)
  library(ggrepel)
})

# Helper for NULL defaults
`%||%` <- function(a, b) if (!is.null(a)) a else b

# ============================================================================
# ============================================================================
# OKABE-ITO COLORBLIND-FRIENDLY PALETTE
# ============================================================================
# ============================================================================

okabe_ito_colors <- c(
  "#333333", # Dark Grey (Replacing Black)
  "#E69F00", # Orange
  "#56B4E9", # Sky Blue
  "#009E73", # Bluish Green
  "#F0E442", # Yellow
  "#0072B2", # Blue
  "#D55E00", # Vermillion
  "#CC79A7", # Reddish Purple
  "#999999" # Gray
)

get_color_palette <- function(n) {
  if (n <= length(okabe_ito_colors)) {
    return(okabe_ito_colors[1:n])
  } else {
    return(colorRampPalette(okabe_ito_colors)(n))
  }
}

# ============================================================================
# ============================================================================
# PUBLICATION THEME
# ============================================================================
# ============================================================================

theme_publication <- function() {
  theme_bw(base_size = 11) +
    theme(
      panel.grid.major = element_line(color = "gray90", linewidth = 0.3),
      panel.grid.minor = element_blank(),
      axis.line = element_line(color = "#333333", linewidth = 0.5),
      axis.ticks = element_line(color = "#333333", linewidth = 0.5),
      axis.text.x = element_text(color = "#333333", size = 10, angle = 45, vjust = 1, hjust = 1),
      axis.text.y = element_text(color = "#333333", size = 10),
      axis.title = element_text(face = "bold", size = 11, color = "#333333"),
      plot.title = element_text(face = "bold", hjust = 0.5, size = 12, color = "#333333"),
      plot.subtitle = element_text(hjust = 0.5, size = 10, color = "gray30"),
      legend.background = element_rect(fill = "white", color = "#333333"),
      legend.key = element_blank(),
      strip.background = element_rect(fill = "gray95", color = "#333333"),
      strip.text = element_text(face = "bold", color = "#333333"),
      plot.margin = margin(10, 10, 10, 20)
    )
}

pseudo_log_trans_for_breaks <- function() {
  scales::trans_new(
    "pseudo_log_for_breaks",
    function(x) log10(x + 0.001), # Offset by 0.1 as used in the plot
    function(x) 10^x - 0.001,
    domain = c(0, Inf)
  )
}

# Helper for custom log labels (0, 1, 10, 100, 10^3...)
custom_log_labels <- function(breaks) {
  lapply(breaks, function(x) {
    if (is.na(x) || !is.finite(x)) return("")
    if (x == 0) return("0   ")
    
    # Standard pretty range (0.01 to 1000)
    if (abs(x) >= 0.01 && abs(x) < 1000) {
       # Check if integer
       if (abs(x - round(x)) < 1e-10) return(as.character(round(x)))
       # Else return as is, trimming trailing zeros
       s <- format(x, scientific = FALSE, trim = TRUE)
       if (grepl("\\.", s)) s <- sub("0+$", "", s)
       return(sub("\\.$", "", s))
    }
    
    # Pretty scientific notation for very large or very small numbers
    log_x <- log10(abs(x))
    exponent <- floor(log_x)
    coefficient <- x / (10^exponent)
    
    if (abs(coefficient - 1) < 1e-10) {
      return(bquote(10^.(exponent)))
    } else {
      # Use 1 decimal place for coefficient if needed, otherwise clean
      coeff_formatted <- sub("\\.0+$", "", sprintf("%.1f", coefficient))
      return(bquote(.(coeff_formatted) %*% 10^.(exponent)))
    }
  })
}

get_smart_breaks <- function(conc_range) {
  if (length(conc_range) == 0 || all(is.na(conc_range))) {
    return(c(0, 1, 10))
  }

  min_c <- min(conc_range, na.rm = TRUE)
  max_c <- max(conc_range, na.rm = TRUE)

  # Strategy 1: If we have a discrete set of concentrations (typical for dose-response),
  # use them directly. This ensures 0, 1, 10, 20, 50, 100 are all shown.
  unique_vals <- sort(unique(conc_range))
  if (length(unique_vals) <= 15) {
    return(unique_vals)
  }

  # Identify positive values
  pos_vals <- conc_range[conc_range > 1e-9]

  breaks <- numeric()

  if (length(pos_vals) > 0) {
    min_pos <- min(pos_vals)
    max_pos <- max(pos_vals)

    # Generate powers of 10 covering the range
    log_min <- floor(log10(min_pos))
    log_max <- ceiling(log10(max_pos))

    # Basic powers of 10
    breaks <- 10^(seq(log_min, log_max))

    # If the range is narrow (e.g. within one decade), add intermediate values
    if (length(breaks) < 3) {
      # Add 1, 2, 5 sequence
      bases <- c(1, 2, 5)
      exponents <- seq(log_min - 1, log_max + 1)
      detailed_breaks <- as.vector(outer(bases, 10^exponents, "*"))

      # Filter to relevant range
      breaks <- detailed_breaks[detailed_breaks >= min_pos * 0.5 & detailed_breaks <= max_pos * 1.5]
    }
  }

  # Always include 0 if it's in the data (or close to it)
  if (min_c <= 1e-9) {
    breaks <- sort(unique(c(0, breaks)))
  } else {
    breaks <- sort(unique(breaks))
  }

  return(breaks)
}

format_conc_labels <- function(x) {
  # Redirect concentration labels to the same pretty formatter for consistency
  return(custom_log_labels(x))
}
    
    # --- Plot Generator Functions ---
    
    generate_viability_plot <- function(viability_data, predicted_curves) {
      if (is.null(viability_data) || nrow(viability_data) == 0) return(NULL)
      
      # Clean data: Remove non-finite values
      viability_data <- viability_data %>%
        filter(is.finite(mean_viable), is.finite(concentration_uM))
      
      if (nrow(viability_data) == 0) return(NULL)

      cell_lines <- unique(viability_data$cell_line)
      colors <- get_color_palette(length(cell_lines))
      names(colors) <- cell_lines
    
      conc_range <- viability_data$concentration_uM
      breaks <- get_smart_breaks(conc_range)
    
      p <- ggplot() +
        geom_point(data = viability_data, aes(x = concentration_uM, y = mean_viable, color = cell_line), size = 4, shape = 21, fill = "white", stroke = 1.5)
        
      if ("se_viable" %in% names(viability_data) && any(is.finite(viability_data$se_viable))) {
        p <- p + geom_errorbar(data = viability_data %>% filter(is.finite(se_viable)), 
                               aes(x = concentration_uM, ymin = mean_viable - se_viable, ymax = mean_viable + se_viable, color = cell_line), width = 0.15, linewidth = 1)
      }
    
      if (!is.null(predicted_curves) && nrow(predicted_curves) > 0) {
        predicted_curves <- predicted_curves %>% filter(is.finite(predicted_viable), is.finite(concentration_uM))
        if (nrow(predicted_curves) > 0) {
          p <- p + geom_line(data = predicted_curves, aes(x = concentration_uM, y = predicted_viable, color = cell_line), linewidth = 1.5, alpha = 0.9)
        }
      }
    
      # Check if concentration range is nanomolar (max < 1 uM)
      max_conc <- max(conc_range, na.rm = TRUE)
      
      # Define label formatter
      conc_labeller <- format_conc_labels
      x_title <- "Concentration (µM)"
      
      p <- p + scale_color_manual(values = colors) +
        scale_x_continuous(trans = pseudo_log_trans_for_breaks(), breaks = breaks, labels = conc_labeller) +
        scale_y_continuous(limits = c(0, 110), breaks = seq(0, 100, 20)) +
        labs(title = "Dose-Response Curves with IC50 Determination", 
             subtitle = "Scientific Note: IC50 and LD50 are identical by definition (Death = 100% - Viability)", 
             x = x_title, y = "Cell Viability (%)", color = "Cell Line") +
        theme_publication() +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) 
      return(p)
    }
    
    generate_death_plot <- function(death_data, predicted_curves) {
      if (is.null(death_data) || nrow(death_data) == 0) return(NULL)
      
      # Clean data
      death_data <- death_data %>%
        filter(is.finite(mean_death), is.finite(concentration_uM))
        
      if (nrow(death_data) == 0) return(NULL)

      cell_lines <- unique(death_data$cell_line)
      colors <- get_color_palette(length(cell_lines))
      names(colors) <- cell_lines
    
      conc_range <- death_data$concentration_uM
      breaks <- get_smart_breaks(conc_range)
    
      p <- ggplot() +
        geom_point(data = death_data, aes(x = concentration_uM, y = mean_death, color = cell_line), size = 4, shape = 21, fill = "white", stroke = 1.5)
        
      if ("se_death" %in% names(death_data) && any(is.finite(death_data$se_death))) {
        p <- p + geom_errorbar(data = death_data %>% filter(is.finite(se_death)), 
                               aes(x = concentration_uM, ymin = mean_death - se_death, ymax = mean_death + se_death, color = cell_line), width = 0.15, linewidth = 1)
      }
    
      if (!is.null(predicted_curves) && nrow(predicted_curves) > 0) {
        predicted_curves <- predicted_curves %>% filter(is.finite(predicted_death), is.finite(concentration_uM))
        if (nrow(predicted_curves) > 0) {
          p <- p + geom_line(data = predicted_curves, aes(x = concentration_uM, y = predicted_death, color = cell_line), linewidth = 1.5, alpha = 0.9)
        }
      }
    
      p <- p + scale_color_manual(values = colors) +
        scale_x_continuous(trans = pseudo_log_trans_for_breaks(), breaks = breaks, labels = format_conc_labels) +
        scale_y_continuous(limits = c(0, 110), breaks = seq(0, 100, 20)) +
        labs(title = "Cell Death Dose-Response Curves with LD50", 
             subtitle = "Scientific Note: LD50 and IC50 are identical by definition (Death = 100% - Viability)", 
             x = "Concentration (µM)", y = "Cell Death (%)", color = "Cell Line") +
        theme_publication() +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
      return(p)
    }
    
    # Helper for plot labels (expression)
    pretty_sci_plot <- function(x) {
       lapply(x, function(val) {
         if (is.na(val) || !is.finite(val)) return("")
         if (abs(val) < 0.01 || abs(val) > 1000) {
            exponent <- floor(log10(abs(val)))
            base <- val / (10^exponent)
            # Return as character for geom_text, using plotmath format if needed
            # geom_text with parse=TRUE needs 'base %*% 10^exponent'
            return(sprintf("%.2f %%*%% 10^%d", base, exponent))
         }
         return(sprintf("%.4f", val))
       })
    }

    generate_comparison_plot <- function(ic50_results, ld50_results) {
      combined <- merge(ic50_results, ld50_results, by = "cell_line", all = TRUE)
      
      # Prepare Long Format
      plot_df <- combined %>%
        dplyr::select(cell_line, IC50_uM, LD50_uM) %>%
        pivot_longer(cols = c(IC50_uM, LD50_uM), names_to = "Metric_Type", values_to = "Concentration") %>%
        filter(!is.na(Concentration), is.finite(Concentration), Concentration > 1e-9) %>% # Filter out "Not Reached"
        mutate(
          Metric_Label = ifelse(Metric_Type == "IC50_uM", "IC50 (Viability)", "LD50 (Death)"),
          cell_line = as.factor(cell_line),
          Label_Text = unlist(pretty_sci_plot(Concentration)) # Generate parseable labels
        )
      
      if (nrow(plot_df) == 0) return(NULL)
      
      # Dumbbell Plot Construction
      p <- ggplot(plot_df, aes(x = Concentration, y = cell_line)) +
        # Connection Line (only if both exist for a cell line)
        geom_line(aes(group = cell_line), color = "gray60", linewidth = 1) +
        # Points
        geom_point(aes(color = Metric_Label), size = 5) +
        # Labels - Use ggrepel to avoid collision
        ggrepel::geom_text_repel(aes(label = Label_Text, color = Metric_Label), 
                  size = 3.5, fontface = "bold", show.legend = FALSE, parse = TRUE,
                  nudge_y = 0.3, direction = "x") +
        # Scales
        scale_x_log10(labels = custom_log_labels) +
        scale_color_manual(values = c("IC50 (Viability)" = "#009E73", "LD50 (Death)" = "#D55E00")) +
        labs(
          title = "Therapeutic Window: IC50 vs LD50", 
          subtitle = "Comparison of Potency (IC50) vs Toxicity (LD50). Distance indicates safety margin.",
          x = "Concentration (µM) [Log Scale]", 
          y = "Cell Line",
          color = "Metric"
        ) +
        theme_publication() +
        theme(
          legend.position = "top",
          panel.grid.major.y = element_line(color = "gray90", linetype = "dashed"),
          axis.text.x = element_text(angle = 0, hjust = 0.5) # Reset x-axis to normal since it's log scale now
        )
      
      return(p)
    }
    
    generate_quadrant_plot <- function(results) {
      if (is.null(results) || nrow(results) == 0) return(NULL)
      
      quadrant_data <- results %>%
        group_by(cell_line, concentration_uM) %>%
        summarise(
          Viable = mean(pct_viable, na.rm = TRUE),
          `Early Apoptotic` = mean(pct_early_apoptotic, na.rm = TRUE),
          `Late Apoptotic` = mean(pct_late_apoptotic, na.rm = TRUE),
          Necrotic = mean(pct_necrotic, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        pivot_longer(cols = c(Viable, `Early Apoptotic`, `Late Apoptotic`, Necrotic), names_to = "Quadrant", values_to = "Percentage")
    
      # Remove non-finite
      quadrant_data <- quadrant_data %>% filter(is.finite(Percentage))
      if (nrow(quadrant_data) == 0) return(NULL)

      quadrant_data$Quadrant <- factor(quadrant_data$Quadrant, levels = c("Viable", "Early Apoptotic", "Late Apoptotic", "Necrotic"))
    
      p <- ggplot(quadrant_data, aes(x = as.factor(concentration_uM), y = Percentage, fill = Quadrant)) +
        geom_bar(stat = "identity", position = "stack", color = "black", linewidth = 0.3) +
        scale_fill_manual(values = c("Viable" = "#009E73", "Early Apoptotic" = "#F0E442", "Late Apoptotic" = "#D55E00", "Necrotic" = "#CC79A7")) +
        facet_wrap(~cell_line, scales = "free_x") +
        labs(title = "Apoptosis Quadrant Breakdown by Concentration", subtitle = "Annexin V/PI classification: Viable (−/−), Early Apoptotic (+/−), Late Apoptotic (+/+), Necrotic (−/+)", x = "Concentration (µM)", y = "Percentage (%)", fill = "Cell State") +
        theme_publication() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1), strip.text = element_text(size = 10, face = "bold"))
      return(p)
    }
    
    generate_absolute_survival_plot <- function(abs_survival_summary, predicted_curves) {
      if (is.null(abs_survival_summary) || nrow(abs_survival_summary) == 0) return(NULL)
      
      # Clean data: Remove Inf or NA which can break aesthetics
      abs_survival_summary <- abs_survival_summary %>%
        filter(is.finite(mean_survival))
        
      if (nrow(abs_survival_summary) == 0) return(NULL)

      cell_lines <- unique(abs_survival_summary$cell_line)
      colors <- get_color_palette(length(cell_lines))
      names(colors) <- cell_lines
    
      conc_range <- abs_survival_summary$concentration_uM
      breaks <- get_smart_breaks(conc_range)
    
      p <- ggplot() +
        geom_point(data = abs_survival_summary, aes(x = concentration_uM, y = mean_survival, color = cell_line), size = 4, shape = 21, fill = "white", stroke = 1.5)
        
      # Only add error bars if se_survival is valid and not all NA
      if ("se_survival" %in% names(abs_survival_summary) && any(!is.na(abs_survival_summary$se_survival))) {
        p <- p + geom_errorbar(data = abs_survival_summary, aes(x = concentration_uM, ymin = mean_survival - se_survival, ymax = mean_survival + se_survival, color = cell_line), width = 0.15, linewidth = 1)
      }
    
      if (!is.null(predicted_curves) && nrow(predicted_curves) > 0) {
        # Ensure predicted curves are also finite
        predicted_curves <- predicted_curves %>% filter(is.finite(predicted_survival))
        if (nrow(predicted_curves) > 0) {
          p <- p + geom_line(data = predicted_curves, aes(x = concentration_uM, y = predicted_survival, color = cell_line), linewidth = 1.5, alpha = 0.9)
        }
      }
    
      p <- p + scale_color_manual(values = colors) +
        scale_x_continuous(trans = pseudo_log_trans_for_breaks(), breaks = breaks, labels = format_conc_labels) +
        scale_y_continuous(limits = c(0, 110), breaks = seq(0, 100, 20)) +
        labs(title = "Absolute Survival Dose-Response Curves (Bead-Normalized)", 
             subtitle = "Corrects for total cell lysis; survival relative to control concentration", 
             x = "Concentration (µM)", y = "Relative Absolute Survival (%)", color = "Cell Line") +
        theme_publication() +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
      return(p)
    }
    
    # ============================================================================
# ============================================================================
# UI
# ============================================================================
# ============================================================================

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      html, body { height: 100%; margin: 0; padding: 0; }
      body { display: flex; flex-direction: column; min-height: 100vh; }
      .container-fluid { flex: 1; padding: 0 !important; max-width: 100% !important; }
      .gradient-header {
        background: 
          linear-gradient(rgba(15, 23, 42, 0.7), rgba(15, 23, 42, 0.9)),
          url('header_bg.png');
        background-size: cover;
        background-position: center;
        color: white; padding: 100px 20px; text-align: center; width: 100%;
        box-shadow: 0 4px 20px rgba(0,0,0,0.3);
        position: relative;
        overflow: hidden;
      }
      .gradient-header h1 { 
        font-size: 4.5em; 
        font-weight: 800; 
        margin: -15px 0 30px 0; 
        letter-spacing: -0.03em; 
        text-shadow: 0 4px 15px rgba(0,0,0,0.8), 0 0 30px rgba(59, 130, 246, 0.4);
        color: #ffffff;
      }
      .gradient-header p { 
        font-size: 1.5em; 
        margin: 0 auto; 
        max-width: 900px;
        opacity: 1; 
        font-weight: 500; 
        letter-spacing: 0.02em;
        line-height: 1.4;
        text-shadow: 0 2px 10px rgba(0,0,0,0.9);
        color: #f8fafc;
      }
      .header-curve {
        position: absolute;
        bottom: -1px;
        left: 0;
        width: 100%;
        height: 60px;
        fill: #f8f9fa; /* Matches the sidebar/content background */
      }
      .content-wrapper { padding: 30px; max-width: 1800px; margin: 0 auto; width: 100%; }
      .footer {
        background: 
          linear-gradient(rgba(15, 23, 42, 0.8), rgba(15, 23, 42, 0.95)),
          url('footer_bg.png');
        background-size: cover;
        background-position: center;
        color: white; padding: 80px 20px 40px 20px; text-align: center; margin-top: auto;
        box-shadow: 0 -2px 10px rgba(0,0,0,0.1); width: 100%;
        position: relative;
        overflow: hidden;
      }
      .footer-curve {
        position: absolute;
        top: -1px;
        left: 0;
        width: 100%;
        height: 60px;
        fill: #f8f9fa;
        transform: rotate(180deg);
      }
      .footer p { margin: 8px 0; font-size: 1.1em; text-shadow: 0 1px 3px rgba(0,0,0,0.5); }
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
  div(
    class = "gradient-header",
    h1("FlowPath Dynamics"),
    p("Precision Dose-Response Profiling for Apoptosis and Multi-Channel Flow Cytometry"),
    HTML('<svg class="header-curve" viewBox="0 0 1440 320" preserveAspectRatio="none">
            <path d="M0,160L48,176C96,192,192,224,288,213.3C384,203,480,149,576,149.3C672,149,768,203,864,213.3C960,224,1056,192,1152,181.3C1248,171,1344,181,1392,186.7L1440,192L1440,320L1392,320C1344,320,1248,320,1152,320C1056,320,960,320,864,320C768,320,672,320,576,320C480,320,384,320,288,320C192,320,96,320,48,320L0,320Z"></path>
          </svg>')
  ),
  div(
    class = "content-wrapper",
    fluidRow(
      column(3,
        class = "sidebar",
        div(
          style = "background: linear-gradient(135deg, #fff9c4 0%, #fff59d 100%);
                   padding: 15px; border-radius: 8px; border-left: 5px solid #f57f17;
                   margin-bottom: 20px;",
          h6(
            style = "margin: 0 0 10px 0; color: #f57f17; font-weight: 600;",
            icon("tools"), " Need to Rename Files?"
          ),
          p(
            style = "font-size: 0.9em; margin: 5px 0; color: #424242;",
            "Use our automated file renaming tool to quickly format your FCS files to match the required naming convention."
          ),
          tags$a(
            href = "https://huggingface.co/spaces/mahmood-iab/file_naming",
            target = "_blank",
            class = "btn btn-warning btn-sm btn-block",
            style = "margin-top: 10px; font-weight: 600;",
            icon("external-link-alt"), " Open File Naming Tool"
          )
        ),
        h4(icon("upload"), " Upload Files"),
        fileInput("files", "Add .fcs files:", multiple = TRUE, accept = c(".fcs", ".FCS")),
        hr(),
        div(
          style = "background: #e9f5ff; padding: 15px; border-radius: 8px; border-left: 5px solid #1976d2; margin: 10px 0;",
          h6(icon("compass"), " Select Analysis Workflow", style = "margin-top: 0; color: #1976d2;"),
          checkboxInput("is_apoptosis_assay", "This is an Annexin V / PI apoptosis assay", value = TRUE),
          p(
            style = "font-size: 0.85em; color: #666; margin: 5px 0;",
            "Uncheck for generic single-channel analysis (e.g., % positive cells)."
          )
        ),
        div(
          style = "background: #fdf2f2; padding: 15px; border-radius: 8px; border-left: 5px solid #e53935; margin: 10px 0;",
          h6(icon("bullseye"), " Absolute Cell Counting", style = "margin-top: 0; color: #e53935;"),
          checkboxInput("is_absolute_counting", "Perform Absolute Cell Counting (if you used Precision Count Beads)", value = FALSE),
          conditionalPanel(
            condition = "input.is_absolute_counting == true",
            numericInput("bead_conc", "Bead Stock Concentration (beads/µL):", value = 1000, min = 0),
            numericInput("bead_vol", "Volume of Beads added (µL):", value = 50, min = 0),
            numericInput("sample_vol", "Initial Sample Volume (µL):", value = 150, min = 0),
            selectInput("bead_gate_channel", "Channel for Bead Identification:", choices = NULL),
            p(
              style = "font-size: 0.8em; color: #666; font-style: italic; margin-top: 5px;",
              "Note: The volume of beads, the initial sample volume, and the bead concentration are assumed to be identical for all files in this run."
            )
          )
        ),
        hr(),
        h5(icon("adjust"), " Compensation (Optional)"),
        div(
          class = "comp-box",
          h6("Fluorescence Compensation Controls"),
          p("Upload single-stained controls to calculate compensation matrix:"),
          fileInput("comp_unstained", "Unstained control:", accept = c(".fcs", ".FCS")),

          # Dynamic UI for compensation controls
          uiOutput("dynamic_comp_controls"),
          actionButton("add_comp_control", "Add Compensation Control", icon = icon("plus"), class = "btn-default btn-sm btn-block"),
          hr(),
          actionButton("calc_comp", "Calculate Compensation Matrix",
            class = "btn-info btn-sm btn-block", icon = icon("calculator")
          ),
          uiOutput("comp_status_ui")
        ),
        hr(),
        uiOutput("control_selection_ui"),
        hr(),
        div(
          style = "background: #e8f5e9; padding: 15px; border-radius: 8px; border-left: 5px solid #4caf50; margin: 10px 0;",
          h6(icon("cog"), " Analysis Options", style = "margin-top: 0; color: #2e7d32;"),
          checkboxInput("use_normalization", "Normalize viability to control (100%)", value = FALSE),
          p(
            style = "font-size: 0.85em; color: #666; margin: 5px 0;",
            "When enabled, control samples will be set to 100% viability."
          )
        ),
        hr(),
        uiOutput("workflow_status"),
        hr(),
        uiOutput("analyze_button_ui"),
        uiOutput("download_zip_ui")
      ),
      column(
        9,
        tabsetPanel(
          id = "tabs",
          tabPanel("Files",
            icon = icon("table"),
            br(),
            h4("File Management"),
            p("Use the 'Upload Files' button in the sidebar to add more files. Duplicates will be ignored."),
            hr(),
            actionButton("remove_all_files", "Remove All", icon = icon("trash-alt"), class = "btn-danger btn-sm"),
            HTML("&nbsp;"),
            actionButton("remove_selected_files", "Remove Selected", icon = icon("minus-circle"), class = "btn-warning btn-sm"),
            hr(),
            DT::dataTableOutput("file_table")
          ),
          tabPanel("Compensation",
            icon = icon("adjust"),
            br(),
            uiOutput("comp_matrix_ui")
          ),
          tabPanel("Gating",
            icon = icon("sliders-h"),
            br(),
            conditionalPanel(
              condition = "input.is_apoptosis_assay == true",
              uiOutput("gating_ui")
            ),
            conditionalPanel(
              condition = "input.is_apoptosis_assay == false",
              uiOutput("generic_gating_ui_output")
            )
          ),
          tabPanel("Gate Review",
            icon = icon("eye"),
            br(),
            h4("Review Saved Gates"),
            uiOutput("gate_review_ui")
          ),
          tabPanel("Results",
            icon = icon("chart-line"),
            br(),
            # Apoptosis-specific results
            conditionalPanel(
              condition = "input.is_apoptosis_assay == true",
              uiOutput("control_quality_alert_ui"),
              h4("IC50/LD50 Results"),
              tableOutput("ic50_table"),
              p(style = "font-size: 0.85em; color: #555; font-style: italic; margin-top: -10px;",
                icon("info-circle"), " 'Not Reached' indicates the value is below the detection limit (< 1nM) or the curve did not cross the 50% threshold (e.g. flat response)."
              ),
              hr(),
              h4(icon("list-ol"), " Gating Summary (Cell Recovery)"),
              p("Events remaining after each gating step. High loss (>30%) at FSC/SSC or Singlets may indicate poor sample quality."),
              DT::dataTableOutput("cell_counts_table"),
              hr(),
              h4(icon("check-circle"), " Quality Control"),
              p("Select replicates in the table below and click 'Toggle Status' to exclude/include them from analysis."),
              actionButton("toggle_exclusion", "Toggle Include/Exclude Selected", icon = icon("power-off"), class = "btn-warning btn-sm"),
              p(style="margin-top: 10px; font-size: 0.9em; color: #666;", 
                tags$b("CV (Coefficient of Variation)"), " measures relative variability: ",
                tags$code("CV(%) = (Standard Deviation / Mean) × 100"), 
                ". Use this to identify inconsistent replicates."),
              br(), br(),
              DT::dataTableOutput("qc_table"),
              hr(),
              h4(icon("calculator"), " Advanced Dose-Response Metrics"),
              p("Extended pharmacodynamic parameters derived from the curve fitting."),
              DT::dataTableOutput("advanced_metrics_table"),
              hr(),
              h4(icon("flask"), " Statistical Analysis"),
              p("Comparison of IC50 values between cell lines using One-Way ANOVA and Tukey HSD Post-Hoc tests."),
              tabsetPanel(
                tabPanel("ANOVA", 
                  br(),
                  div(style = "background: #f1f8ff; padding: 15px; border-radius: 8px; border-left: 5px solid #0366d6; margin-bottom: 20px;",
                    h5(icon("info-circle"), " Statistical Hypothesis"),
                    tags$ul(
                      tags$li(strong("Null Hypothesis (H0):"), " There is no significant difference in mean Log10(IC50) values across the selected cell lines."),
                      tags$li(strong("Alternative Hypothesis (H1):"), " At least one cell line has a significantly different mean Log10(IC50).")
                    ),
                    uiOutput("anova_interpretation")
                  ),
                  DT::dataTableOutput("stats_comparison_table")
                ),
                tabPanel("Tukey Post-Hoc", 
                  br(), 
                  div(style = "background: #fff9db; padding: 15px; border-radius: 8px; border-left: 5px solid #f59f00; margin-bottom: 20px;",
                    h5(icon("vial"), " About Tukey's HSD"),
                    p("Tukey's Honest Significant Difference (HSD) test is a post-hoc test performed only if the ANOVA is significant. It compares all possible pairs of cell lines to identify exactly which ones differ while controlling for multiple testing errors.")
                  ),
                  DT::dataTableOutput("posthoc_table"),
                  br(),
                  downloadButton("download_tukey", "Download Tukey Stats (CSV)", class = "btn-outline-primary btn-sm")
                )
              )
            ),
            # Generic-specific results
            conditionalPanel(
              condition = "input.is_apoptosis_assay == false",
              h4("EC50 Results"),
              tableOutput("generic_ec50_table"),
              hr(),
              h4(icon("list-ol"), " Gating Summary (Cell Recovery)"),
              p("Events remaining after each gating step."),
              DT::dataTableOutput("generic_cell_counts_table"),
              hr(),
              h4("Statistical Analysis"),
              tabsetPanel(
                 tabPanel("ANOVA", 
                   br(),
                   div(style = "background: #f1f8ff; padding: 15px; border-radius: 8px; border-left: 5px solid #0366d6; margin-bottom: 20px;",
                     h5(icon("info-circle"), " Statistical Hypothesis"),
                     tags$ul(
                       tags$li(strong("Null Hypothesis (H0):"), " There is no significant difference in mean Log10(EC50) values across the cell lines for the selected populations."),
                       tags$li(strong("Alternative Hypothesis (H1):"), " At least one cell line has a significantly different mean Log10(EC50).")
                     ),
                     uiOutput("generic_anova_interpretation")
                   ),
                   DT::dataTableOutput("generic_anova_table")
                 ),
                 tabPanel("Tukey Post-Hoc", 
                   br(), 
                   div(style = "background: #fff9db; padding: 15px; border-radius: 8px; border-left: 5px solid #f59f00; margin-bottom: 20px;",
                     h5(icon("vial"), " About Tukey's HSD"),
                     p("Tukey's HSD identifies specific pairwise differences in potency (Log10 EC50) between cell lines for each defined population.")
                   ),
                   DT::dataTableOutput("generic_posthoc_table"),
                   br(),
                   downloadButton("download_generic_tukey", "Download Tukey Stats (CSV)", class = "btn-outline-primary btn-sm")
                 )
              ),
              hr(),
              h4("Raw Percent Positive Results (Interactive Exclusion)"),
              p("Select replicates below and click 'Toggle Status' to exclude/include them from analysis."),
              actionButton("toggle_generic_exclusion", "Toggle Include/Exclude Selected", icon = icon("power-off"), class = "btn-warning btn-sm"),
              p(style="margin-top: 10px; font-size: 0.9em; color: #666;", 
                tags$b("CV (Coefficient of Variation)"), " measures relative variability: ",
                tags$code("CV(%) = (Standard Deviation / Mean) × 100"), 
                ". Use this to identify inconsistent replicates."),
              br(), br(),
              DT::dataTableOutput("generic_results_table")
            ),
            hr(),
            div(
              style = "display: flex; gap: 10px;",
              downloadButton("download_results", "Download Raw Results (CSV)", class = "btn-primary"),
              downloadButton("download_qc", "Download QC Metrics (CSV)", class = "btn-outline-secondary")
            )
          ),
          tabPanel("Plots",
            icon = icon("chart-bar"),
            br(),
            # Apoptosis-specific plots
            conditionalPanel(
              condition = "input.is_apoptosis_assay == true",
              tagList(
                h4("Viability Dose-Response Curves"),
                plotOutput("viability_plot", height = "600px"),
                hr(),
                h4("Cell Death Dose-Response Curves"),
                plotOutput("death_plot", height = "600px"),
                conditionalPanel(
                  condition = "input.is_absolute_counting == true",
                  hr(),
                  h4("Absolute Survival Dose-Response (Bead-Normalized)"),
                  plotOutput("abs_survival_plot", height = "600px")
                ),
                hr(),
                h4("IC50 vs LD50 Comparison"),
                plotOutput("ic50_ld50_comparison", height = "500px"),
                hr(),
                h4("Apoptosis Quadrant Breakdown"),
                plotOutput("quadrant_plot", height = "700px")
              ),
              conditionalPanel(
                condition = "input.is_absolute_counting == true",
                hr(),
                h4(icon("bullseye"), " Bead Counting QC & Impact"),
                p("Assess the consistency of bead pipetting and the impact of absolute quantification."),
                tabsetPanel(
                  tabPanel("Bead Stability", 
                    br(),
                    p("Consistency of bead counts across samples. High variability indicates potential pipetting errors."),
                    plotOutput("bead_qc_plot", height = "500px")
                  ),
                  tabPanel("Count Impact", 
                    br(),
                    p("Comparison of Raw Events (Machine Count) vs. Absolute Cell Concentration (Calculated)."),
                    plotOutput("bead_impact_plot", height = "600px")
                  )
                )
              )
            ),
            # Generic-specific plot
            conditionalPanel(
              condition = "input.is_apoptosis_assay == false",
              tabsetPanel(
                id = "generic_plots_tabs",
                tabPanel("Population Breakdown",
                  value = "generic_pop_breakdown",
                  h4("Population Breakdown Across Samples"),
                  p("Visualizes the percentage of each defined cell population across all samples."),
                  plotOutput("generic_pop_breakdown_plot", height = "600px")
                ),
                tabPanel("Fluorescence Intensity",
                  value = "generic_fluor_intensity",
                  h4("Fluorescence Intensity Distribution"),
                  p("Compare the distribution of fluorescence intensity for a selected channel across samples."),
                  selectInput("generic_intensity_channel_select", "Select Channel:", choices = NULL),
                  plotOutput("generic_intensity_plot", height = "600px")
                ),
                tabPanel("Dose-Response (EC50)",
                  value = "generic_dose_response",
                  h4("Dose-Response Curves with EC50 Determination"),
                  p("Generate dose-response curves for a selected cell population."),
                  selectInput("generic_dr_population_select", "Select Population:", choices = NULL),
                  plotOutput("generic_dose_response_plot", height = "600px")
                ),
                tabPanel("Bead QC & Impact", 
                  value = "generic_bead_qc",
                  conditionalPanel(
                    condition = "input.is_absolute_counting == true",
                    h4(icon("bullseye"), " Bead Counting QC & Impact"),
                    p("Assess the consistency of bead pipetting and the impact of absolute quantification."),
                    tabsetPanel(
                      tabPanel("Bead Stability", 
                        br(),
                        p("Consistency of bead counts across samples."),
                        plotOutput("generic_bead_qc_plot", height = "500px")
                      ),
                      tabPanel("Count Impact", 
                        br(),
                        p("Comparison of Raw Events vs. Absolute Cell Concentration."),
                        plotOutput("generic_bead_impact_plot", height = "600px")
                      )
                    )
                  ),
                  conditionalPanel(
                    condition = "input.is_absolute_counting == false",
                    div(class="alert alert-info", "Absolute cell counting is not enabled.")
                  )
                )
              )
            )
          )
        )
      )
    )
  ),
  div(
    class = "footer",
    HTML('<svg class="footer-curve" viewBox="0 0 1440 320" preserveAspectRatio="none">
            <path d="M0,160L48,176C96,192,192,224,288,213.3C384,203,480,149,576,149.3C672,149,768,203,864,213.3C960,224,1056,192,1152,181.3C1248,171,1344,181,1392,186.7L1440,192L1440,320L1392,320C1344,320,1248,320,1152,320C1056,320,960,320,864,320C768,320,672,320,576,320C480,320,384,320,288,320C192,320,96,320,48,320L0,320Z"></path>
          </svg>'),
    p(strong("Developed by Mahmood Mohammed Ali")),
    p("Université Grenoble Alpes | Institute for Advanced Biosciences | Epigenetics of Regeneration and Cancer Group"),
    p("mahmood.mohammed-ali@univ-grenoble-alpes.fr"),
    div(
      class = "social-links",
      tags$a(
        href = "mailto:mahmood.mohammed-ali@univ-grenoble-alpes.fr",
        target = "_blank",
        icon("envelope"), " Email"
      ),
      tags$a(
        href = "https://github.com/Mahmood-M-Ali",
        target = "_blank",
        icon("github"), " GitHub"
      ),
      tags$a(
        href = "https://www.linkedin.com/in/mahmood-mohammed-ali-20334b205/",
        target = "_blank",
        icon("linkedin"), " LinkedIn"
      )
    ),
    div(
      style = "margin-top: 20px;",
      tags$a(
        href = "https://creativecommons.org/licenses/by-nc/4.0/",
        target = "_blank",
        tags$img(
          src = "https://i.creativecommons.org/l/by-nc/4.0/88x31.png",
          alt = "CC BY-NC 4.0 License",
          style = "border: 0;"
        )
      )
    ),
    div(
      style = "margin-top: 10px;",
      tags$a(
        href = "https://doi.org/10.5281/zenodo.17872796",
        target = "_blank",
        tags$img(
          src = "https://zenodo.org/badge/DOI/10.5281/zenodo.17872796.svg",
          alt = "DOI: 10.5281/zenodo.17872796",
          style = "border: 0;"
        )
      )
    )
  )
)
# ============================================================================
# ============================================================================
# SERVER
# ============================================================================
# ============================================================================

server <- function(input, output, session) {
  source("generic_analysis.R", local = TRUE)

  # Render the UI for the generic gating workflow
  output$generic_gating_ui_output <- renderUI({
    # We pass rv and available_fluor_channels to the function from generic_analysis.R
    generic_gating_ui(rv, available_fluor_channels)
  })


  # Activate the server-side logic for the generic gating workflow
  generic_gating_server(input, output, session, rv, available_fluor_channels)


  # Define available fluorescence channels
  # Based on ThermoFisher Attune NxT Flow Cytometer specifications
  available_fluor_channels <- c(
    "BL1-A", "BL2-A", "BL3-A", "BL4-A", # Blue Laser
    "VL1-A", "VL2-A", "VL3-A", "VL4-A", # Violet Laser
    "YL1-A", "YL2-A", "YL3-A", "YL4-A", # Yellow Laser
    "RL1-A", "RL2-A", "RL3-A" # Red Laser
  )

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
    spill_matrix = NULL,
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
    singlet_gate_plots = list(), # NEW
    annexin_gate_plots = list(), # NEW
    gate_review_plots = list(), # NEW: For combined review plots
    comp_before_after = NULL,
    file_data = NULL,
    comp_controls = list(), # Stores dynamic compensation controls
    comp_observers = list(), # NEW: Stores observer handles
    
    # NEW: Per-Cell-Line Compensation Support
    spill_matrices = list(), # List of Spillover Matrices (User Editable)
    comp_matrices = list(),  # List of Compensation Matrices (Calculated Inverse)


    # NEW: Reactive values for the generic analysis workflow
    generic_results = NULL,
    generic_ec50_results = NULL,


    # NEW: For dynamic multi-channel generic gating
    generic_gate_defs = list(),
    generic_gate_counter = 0,
    generic_observers = list(),
    generic_histogram_plots = list(), # NEW: Store individual histograms per cell line


    # NEW: Comprehensive results from multi-channel generic analysis
    generic_raw_intensity_data = list(),
    generic_plot_data_dr = NULL, # New DR plot data
    generic_predicted_curves_dr = NULL, # New DR predicted curves


    # NEW: Reactive values for generic plot objects
    generic_pop_breakdown_plot_obj = NULL,
    generic_intensity_plot_obj = NULL,
    generic_dose_response_plot_obj = NULL,
    
    # NEW: For Manual Compensation Preview Plots in Reports
    manual_comp_preview_snapshots = list(),
    comp_plot_obj_current = NULL, # NEW: Store the currently rendered comp preview plot
    
    # NEW: Data Exclusion and Statistics
    manual_exclusions = character(0), # Stores IDs of excluded replicates (Apoptosis)
    posthoc_results = NULL,           # Stores Tukey results (Apoptosis)
    
    generic_manual_exclusions = character(0), # Stores IDs of excluded replicates (Generic)
    generic_posthoc_results = NULL,           # Stores Tukey results (Generic)
    generic_anova_results = NULL,             # Stores ANOVA results (Generic)
    
    # NEW: Absolute Cell Counting Support
    bead_counts = list(),                     # Stores bead counts per file
    temp_bead_gates = list(),                 # Stores polygon gates for beads
    bead_gate_plots = list(),                 # Stores plots for bead gating
    absolute_survival_results = NULL,         # Stores IC50 based on absolute survival
    predicted_absolute_survival_curves = NULL # Stores curves for absolute survival
  )

  # Counter for dynamic compensation controls
  rv$comp_control_counter <- 0

  # Render dynamic compensation control UI
  output$dynamic_comp_controls <- renderUI({
    if (length(rv$comp_controls) == 0) {
      return(NULL)
    }

    comp_control_uis <- lapply(seq_along(rv$comp_controls), function(i) {
      control_id <- rv$comp_controls[[i]]$id

      div(
        id = paste0("comp_control_box_", control_id),
        style = "border: 1px solid #ccc; padding: 10px; margin-bottom: 10px; border-radius: 5px; background-color: #f9f9f9;",
        fluidRow(
          column(
            6,
            fileInput(paste0("comp_file_", control_id),
              label = paste0("Control ", i, ":FCS File"),
              accept = c(".fcs", ".FCS"),
              width = "100%"
            )
          ),
          column(
            6,
            selectInput(paste0("comp_channel_", control_id),
              label = paste0("Control ", i, ": Channel"),
              choices = available_fluor_channels,
              selected = rv$comp_controls[[i]]$channel,
              width = "100%"
            ),
            actionButton(paste0("remove_comp_control_", control_id),
              "Remove",
              icon = icon("trash-alt"),
              class = "btn-danger btn-xs"
            )
          )
        )
      )
    })

    do.call(tagList, comp_control_uis)
  })

  # Add new compensation control and set up its observers
  observeEvent(input$add_comp_control, {
    # 1. Increment counter and create new control data structure
    rv$comp_control_counter <- rv$comp_control_counter + 1
    new_id <- rv$comp_control_counter
    new_control <- list(id = new_id, file = NULL, channel = NULL)


    # 2. Add the control to the main list
    rv$comp_controls <- c(rv$comp_controls, list(new_control))


    # 3. Create the observers for the new control's inputs

    # Observer for file changes
    file_obs <- observeEvent(input[[paste0("comp_file_", new_id)]],
      {
        idx <- which(sapply(isolate(rv$comp_controls), function(x) isTRUE(x$id == new_id)))
        if (length(idx) == 1) {
          # Use a temporary list to make the modification
          temp_controls <- isolate(rv$comp_controls)
          temp_controls[[idx]]$file <- input[[paste0("comp_file_", new_id)]]
          rv$comp_controls <- temp_controls
        }
      },
      ignoreNULL = TRUE,
      ignoreInit = TRUE,
      suspended = TRUE
    ) # Create suspended

    # Observer for channel changes
    channel_obs <- observeEvent(input[[paste0("comp_channel_", new_id)]],
      {
        idx <- which(sapply(isolate(rv$comp_controls), function(x) isTRUE(x$id == new_id)))
        if (length(idx) == 1) {
          temp_controls <- isolate(rv$comp_controls)
          temp_controls[[idx]]$channel <- input[[paste0("comp_channel_", new_id)]]
          rv$comp_controls <- temp_controls
        }
      },
      ignoreNULL = TRUE,
      ignoreInit = TRUE,
      suspended = TRUE
    ) # Create suspended

    # Observer for the remove button
    remove_obs <- observeEvent(input[[paste0("remove_comp_control_", new_id)]],
      {
        # Destroy this control's observers
        obs_to_remove <- isolate(rv$comp_observers)[[as.character(new_id)]]
        if (!is.null(obs_to_remove)) {
          obs_to_remove$file_obs$destroy()
          obs_to_remove$channel_obs$destroy()
          obs_to_remove$remove_obs$destroy()
        }

        # Remove observers from the tracking list
        temp_observers <- isolate(rv$comp_observers)
        temp_observers[[as.character(new_id)]] <- NULL
        rv$comp_observers <- temp_observers

        # Remove control data from the main list
        temp_controls <- isolate(rv$comp_controls)
        temp_controls <- temp_controls[sapply(temp_controls, function(x) !isTRUE(x$id == new_id))]
        rv$comp_controls <- temp_controls

        showNotification(paste("Removed compensation control", new_id), type = "message")
      },
      ignoreNULL = TRUE,
      ignoreInit = TRUE,
      once = TRUE
    ) # Destroy itself after one click

    # 4. Store the observer handles in the tracking list
    temp_observers <- isolate(rv$comp_observers)
    temp_observers[[as.character(new_id)]] <- list(
      file_obs = file_obs,
      channel_obs = channel_obs,
      remove_obs = remove_obs
    )
    rv$comp_observers <- temp_observers


    # 5. NOW resume the observers
    file_obs$resume()
    channel_obs$resume()
  })

  # === COMPENSATION CALCULATION (UNCHANGED) ===
  observeEvent(input$calc_comp, {
    req(input$comp_unstained, length(rv$comp_controls) > 0)

    # Filter out controls without an uploaded file or a selected channel
    valid_controls <- Filter(function(x) !is.null(x$file) && !is.null(x$channel), rv$comp_controls)

    if (length(valid_controls) < 2) {
      showNotification("Please upload at least two valid single-stained controls (unstained + 2 stained) to calculate compensation.",
        type = "error", duration = 10
      )
      return()
    }

    withProgress(message = "Calculating compensation...", {
      tryCatch(
        {
          # Load unstained control
          unstained_ff <- read.FCS(input$comp_unstained$datapath)
          if (is.null(unstained_ff)) {
            showNotification("Failed to load unstained control file.", type = "error", duration = 10)
            return()
          }

          # Load stained controls and store them in a list
          ff_controls <- list()
          control_channels <- character()

          for (i in seq_along(valid_controls)) {
            control <- valid_controls[[i]]
            ff <- read.FCS(control$file$datapath)
            if (is.null(ff)) {
              showNotification(paste("Failed to load stained control file:", control$file$name), type = "error", duration = 10)
              return()
            }
            ff_controls[[control$channel]] <- ff
            control_channels <- c(control_channels, control$channel)
          }

          # Ensure unique channels (should be handled by UI, but as a safeguard)
          control_channels <- unique(control_channels)

          # Prepare for spillover calculation
          # Combine unstained with stained controls
          all_controls_list <- c(list(`_UNSTAINED_` = unstained_ff), ff_controls)

          # Debug prints
          print("Content of all_controls_list:")
          print(names(all_controls_list))
          print("Class of flowSet(all_controls_list):")
          print(class(flowSet(all_controls_list)))

          # The channels used in spillover must match the channels in the flowFrames
          # Get common channels across all files for spillover matrix calculation
          common_channels <- Reduce(intersect, lapply(all_controls_list, function(x) colnames(exprs(x))))

          # Filter control_channels to only include those present in all FCS files
          comp_channels_for_spill <- intersect(control_channels, common_channels)

          if (length(comp_channels_for_spill) < 2) {
            showNotification("Insufficient common fluorescent channels for compensation calculation.",
              type = "error", duration = 10
            )
            return()
          }

          # Compute spillover matrix manually to avoid method dispatch errors
          # Initialize spillover matrix
          n_ch <- length(comp_channels_for_spill)
          spill_matrix <- diag(n_ch)
          rownames(spill_matrix) <- comp_channels_for_spill
          colnames(spill_matrix) <- comp_channels_for_spill

          # --- HELPER: Clean Debris & Saturation ---
          clean_ff <- function(ff) {
            exprs_data <- exprs(ff)
            
            # 1. Filter Debris: FSC-A
            # Increased to 20% to be very aggressive against noise/dead cells in controls
            fsc_limit <- quantile(exprs_data[,"FSC-A"], 0.20, na.rm=TRUE)
            mask <- exprs_data[,"FSC-A"] > fsc_limit
            
            # 2. Filter Saturation: Remove events at max dynamic range for fluorescent channels
            # We look at the $PnR value for each channel in the parameters.
            p_meta <- parameters(ff)@data
            for (i in seq_len(nrow(p_meta))) {
               ch_name <- p_meta$name[i]
               # Skip FSC/SSC and Time
               if (grepl("FSC|SSC|Time", ch_name, ignore.case = TRUE)) next
               
               # Get max range from metadata ($PnR)
               max_val <- as.numeric(description(ff)[[paste0("$P", i, "R")]])
               if (is.na(max_val)) max_val <- 1048576 # Default for many 20-bit systems
               
               # Threshold for saturation (events exactly at max or within 0.1% of it)
               # Most cytometers hard-cap at the max value.
               mask <- mask & (exprs_data[, ch_name] < (max_val * 0.999))
            }
            
            ff[mask, ]
          }

          # Get unstained stats (CLEANED)
          unstained_ff_clean <- clean_ff(unstained_ff)
          uns_exprs <- exprs(unstained_ff_clean)
          uns_medians <- apply(uns_exprs[, comp_channels_for_spill, drop = FALSE], 2, median, na.rm = TRUE)

          # Loop over stained controls
          for (ch in comp_channels_for_spill) {
            if (!ch %in% names(ff_controls)) next

            ff <- ff_controls[[ch]]
            ff_clean <- clean_ff(ff) # Use CLEANED data
            dat <- exprs(ff_clean)

            # --- SMART POSITIVE DETECTION (ROBUST) ---
            # 1. Define Negative Limit using Robust Stats (Median + 3*MAD)
            # This avoids outliers/debris inflating the negative baseline.
            uns_values <- uns_exprs[, ch]
            uns_med <- median(uns_values, na.rm = TRUE)
            uns_mad <- mad(uns_values, constant = 1.4826, na.rm = TRUE) # constant makes it consistent with SD
            neg_limit <- uns_med + 3 * uns_mad
            
            # Safety: If MAD is 0 (unlikely but possible with digitized data), fallback to quantile
            if (uns_mad == 0) neg_limit <- quantile(uns_values, 0.98, na.rm = TRUE)

            # 2. Identify Positive Events
            stained_values <- dat[, ch]
            is_positive <- stained_values > neg_limit
            
            # 3. Check for sufficient events
            num_pos <- sum(is_positive, na.rm = TRUE)
            total_events <- length(stained_values)
            pct_pos <- (num_pos / total_events) * 100
            
            print(sprintf("Compensation Debug [%s]: Neg Limit (Robust)=%.2f, %% Pos=%.2f%%", ch, neg_limit, pct_pos))
            
            if (num_pos < 100 || pct_pos < 1.0) {
                 # Fallback for weak controls: use top 5%
                 print(sprintf("  -> Fallback triggered for %s (Top 5%%)", ch))
                 top_cutoff <- quantile(stained_values, 0.95, na.rm = TRUE)
                 is_positive <- stained_values >= top_cutoff
            } else {
                 # --- REFINED POSITIVE POPULATION (Center-Mass) ---
                 # Target the linear "Sweet Spot" (50th-80th percentile).
                 # This ignores the dim smear AND the saturated/non-linear tip.
                 pos_vals <- stained_values[is_positive]
                 p50 <- quantile(pos_vals, 0.50, na.rm = TRUE)
                 p80 <- quantile(pos_vals, 0.80, na.rm = TRUE)
                 
                 print(sprintf("  -> Refined Range [%s]: P50=%.2f, P80=%.2f (Avoiding Tip)", ch, p50, p80))
                 
                 is_positive <- is_positive & (stained_values >= p50) & (stained_values <= p80)
            }

            # 4. Calculate Medians (The FlowJo/FCS Express Standard)
            # We use the Median Ratio on the refined populations.
            
            # Positive Medians (on refined high-intensity population)
            pos_medians <- apply(dat[is_positive, comp_channels_for_spill, drop = FALSE], 2, median, na.rm = TRUE)
            
            # Negative Medians (on truly negative population from same tube)
            # Use events below the neg_limit to account for tube autofluorescence
            is_negative <- stained_values <= neg_limit
            # Ensure we have enough negative events, else fallback to unstained
            if (sum(is_negative, na.rm=TRUE) > 100) {
               neg_medians <- apply(dat[is_negative, comp_channels_for_spill, drop = FALSE], 2, median, na.rm = TRUE)
            } else {
               neg_medians <- uns_medians 
            }
            
            print(sprintf("  -> Medians [%s]: Pos=%.2f, Neg=%.2f", ch, pos_medians[ch], neg_medians[ch]))
            
            # Primary Signal (MFI_Pos - MFI_Neg in the primary channel)
            primary_signal <- pos_medians[ch] - neg_medians[ch]
            
            # Loop through all channels to calculate spillover
            for (detect_ch in comp_channels_for_spill) {
               if (detect_ch == ch) {
                   spill_matrix[detect_ch, ch] <- 1.0
               } else {
                   # Spillover Signal
                   spill_signal <- pos_medians[detect_ch] - neg_medians[detect_ch]
                   
                   # Slope = Spillover / Primary
                   if (primary_signal > 1) {
                      slope <- spill_signal / primary_signal
                   } else {
                      slope <- 0
                   }
                   
                   print(sprintf("     -> Spillover to %s: Signal=%.2f, Slope=%.4f", detect_ch, spill_signal, slope))
                   
                   # Physics check
                   if (slope < 0) slope <- 0
                   
                   # --- SANITY CAP ---
                   # Cap spillover at 80% to prevent over-compensation from dead cell contamination
                   if (slope > 0.8) {
                      slope <- 0.8
                      showNotification(paste0("Warning: High spillover detected (", round(slope*100, 1), "%) from ", ch, " into ", detect_ch, ". Capped at 80% to prevent artifacts."), type = "warning", duration = 15)
                   }
                   
                   spill_matrix[detect_ch, ch] <- slope
               }
            }
          }

          rv$comp_matrix <- solve(spill_matrix)
          rv$spill_matrix <- spill_matrix
          rv$comp_calculated <- TRUE
          
          # NEW: Initialize Per-Cell-Line Matrices
          # Copy the calculated master matrix to all cell lines currently in metadata
          if (!is.null(rv$metadata)) {
            cell_lines <- unique(rv$metadata$cell_line)
            for (line in cell_lines) {
              rv$spill_matrices[[line]] <- rv$spill_matrix
              rv$comp_matrices[[line]] <- rv$comp_matrix
            }
          }

          # Visualization data: use the first two specified channels for before/after comparison
          if (length(comp_channels_for_spill) >= 2) {
            vis_channel1 <- comp_channels_for_spill[1]
            vis_channel2 <- comp_channels_for_spill[2]

            # Use data from the first stained control for visualization
            sample_ff_for_vis <- ff_controls[[vis_channel1]]
            sample_indices <- sample(1:nrow(exprs(sample_ff_for_vis)), min(5000, nrow(exprs(sample_ff_for_vis))))

            before_data_vis <- exprs(sample_ff_for_vis)[sample_indices, c(vis_channel1, vis_channel2)]

            # Apply compensation to the relevant channels
            # Ensure comp_matrix has dimnames matching comp_channels_for_spill
            # And order before_data_vis columns to match comp_matrix rows

            # Create a minimal compensated flowFrame to get compensated values
            # This is more robust than direct matrix multiplication for selecting channels
            compensated_ff <- compensate(sample_ff_for_vis, rv$spill_matrix)
            after_data_vis <- exprs(compensated_ff)[sample_indices, c(vis_channel1, vis_channel2)]

            rv$comp_before_after <- list(
              before = as.data.frame(before_data_vis),
              after = as.data.frame(after_data_vis),
              channels = c(vis_channel1, vis_channel2) # Store channels used for visualization
            )

            # === GENERATE COMPENSATION PLOT OBJECT (ArcSinh Scaled) ===

            # 1. Use ArcSinh Transformation (Standard for CyTOF/FACS visualization)
            # This is robust, handles negatives linearly, and creates good "clouds"
            # Cofactor 150 is standard for flow cytometry (Logicle approx)
            asinh_cofactor <- 150
            asinh_trans <- function(x) asinh(x / asinh_cofactor)
            
            # Helper to apply transform
            apply_trans <- function(data) {
               asinh_trans(as.matrix(data))
            }

            # 2. Transform all datasets
            # Unstained
            uns_indices <- sample(1:nrow(exprs(unstained_ff)), min(5000, nrow(exprs(unstained_ff))))
            uns_data_raw <- exprs(unstained_ff)[uns_indices, c(vis_channel1, vis_channel2)]
            uns_data_vis <- apply_trans(uns_data_raw)
            
            # Before (Uncompensated)
            before_data_vis_trans <- apply_trans(before_data_vis)
            
            # After (Compensated)
            after_data_vis_trans <- apply_trans(after_data_vis)

            # 3. Calculate Smart Axis Limits & Breaks
            # Include negative breaks to visualize over-compensated "corner" data
            raw_breaks <- c(-10000, -1000, -100, 0, 100, 1000, 10000, 100000, 1000000)
            trans_breaks <- asinh_trans(raw_breaks)
            
            # Filter breaks that are within the data range (with some padding)
            all_vals <- c(before_data_vis_trans, after_data_vis_trans)
            data_range <- range(all_vals, na.rm=TRUE)
            valid_indices <- trans_breaks >= (data_range[1] - 1) & trans_breaks <= (data_range[2] + 1)
            
            # Ensure we always include at least 0 and some positive/negative markers if they exist
            if (!any(raw_breaks[valid_indices] == 0)) {
               # Add 0 if it's within range but was filtered
               if (0 >= (data_range[1]-1) && 0 <= (data_range[2]+1)) {
                  valid_indices[raw_breaks == 0] <- TRUE
               }
            }

            final_breaks <- trans_breaks[valid_indices]
            final_labels <- custom_log_labels(raw_breaks[valid_indices])

            # 4. Scatter Plots (Top Row)
            df_b <- as.data.frame(before_data_vis_trans)
            colnames(df_b) <- c("x", "y")
            df_a <- as.data.frame(after_data_vis_trans)
            colnames(df_a) <- c("x", "y")

            p_scat_b <- ggplot(df_b, aes(x, y)) +
              geom_point(alpha = 0.2, size = 0.5, color = "#D55E00") +
              scale_x_continuous(breaks = final_breaks, labels = final_labels) +
              scale_y_continuous(breaks = final_breaks, labels = final_labels) +
              labs(title = "Before Compensation", subtitle = "Scatter (ArcSinh Scale)", x = vis_channel1, y = vis_channel2) +
              theme_publication() +
              theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.5))

            p_scat_a <- ggplot(df_a, aes(x, y)) +
              geom_point(alpha = 0.2, size = 0.5, color = "#009E73") +
              scale_x_continuous(breaks = final_breaks, labels = final_labels) +
              scale_y_continuous(breaks = final_breaks, labels = final_labels) +
              labs(title = "After Compensation", subtitle = "Scatter (ArcSinh Scale)", x = vis_channel1, y = vis_channel2) +
              theme_publication() +
              theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.5))

            # 5. Histograms (Bottom Row)
            # Helper to build histogram data
            label_unstained <- "Unstained"
            label_primary <- paste0(vis_channel1, " (Primary)")
            label_spillover <- paste0(vis_channel2, " (Spillover)")

            make_hist_df <- function(uns, samp, tag) {
              rbind(
                data.frame(Val = uns[, 2], Type = label_unstained, stringsAsFactors = FALSE),
                data.frame(Val = samp[, 1], Type = label_primary, stringsAsFactors = FALSE),
                data.frame(Val = samp[, 2], Type = label_spillover, stringsAsFactors = FALSE)
              )
            }

            df_hist_b <- make_hist_df(uns_data_vis, before_data_vis_trans, "Before")
            df_hist_b$Type <- factor(df_hist_b$Type, levels = c(label_unstained, label_primary, label_spillover))

            df_hist_a <- make_hist_df(uns_data_vis, after_data_vis_trans, "After")
            df_hist_a$Type <- factor(df_hist_a$Type, levels = c(label_unstained, label_primary, label_spillover))

            # Common theme for histograms
            hist_theme <- list(
              scale_x_continuous(breaks = final_breaks, labels = final_labels),
              scale_fill_manual(values = c("#999999", "#0072B2", "#D55E00")), # Grey, Blue, Red
              labs(x = "ArcSinh Intensity", y = "Density"),
              theme_publication(),
              theme(legend.position = "bottom", legend.title = element_blank(), legend.text = element_text(size = 8),
                    axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.5))
            )

            p_hist_b <- ggplot(df_hist_b, aes(x = Val, fill = Type)) +
              geom_density(alpha = 0.5) +
              labs(title = "Before: Channel Overlay") +
              hist_theme

            p_hist_a <- ggplot(df_hist_a, aes(x = Val, fill = Type)) +
              geom_density(alpha = 0.5) +
              labs(title = "After: Channel Overlay") +
              hist_theme

            rv$comp_plot_obj <- gridExtra::arrangeGrob(p_scat_b, p_scat_a, p_hist_b, p_hist_a, ncol = 2)
          } else {
            rv$comp_before_after <- NULL # Cannot visualize with less than 2 channels
            rv$comp_plot_obj <- NULL
          }

          showNotification("Compensation matrix calculated successfully!", type = "message")
          updateTabsetPanel(session, "tabs", selected = "Compensation")
        },
        error = function(e) {
          showNotification(paste("Compensation calculation failed:", e$message),
            type = "error", duration = 10
          )
        }
      )
    })
  })

  output$comp_status_ui <- renderUI({
    if (rv$comp_calculated) {
      div(
        class = "alert alert-success", style = "margin-top: 10px; padding: 8px; font-size: 0.9em;",
        icon("check-circle"), " Compensation matrix ready - will be applied during analysis"
      )
    }
  })

  output$comp_matrix_ui <- renderUI({
    if (!rv$comp_calculated) {
      return(div(
        class = "alert alert-info",
        h5(icon("info-circle"), " No Compensation Matrix"),
        p("Upload compensation control files in the sidebar and click 'Calculate Compensation Matrix'."),
        hr(),
        h6("What are compensation controls?"),
        p("Compensation corrects for spectral overlap between fluorophores. You need:"),
        tags$ul(
          tags$li(strong("Unstained:"), " Cells with no fluorescent staining in any channel."),
          tags$li(strong("Single-stained controls:"), " For each fluorophore, a sample stained with only that fluorophore.")
        )
      ))
    }

    tagList(
      div(
        class = "alert alert-success",
        h5(icon("check-circle"), " Compensation Matrix Calculated"),
        p(strong("Status:"), " Matrix ready. You can now manually adjust coefficients below.")
      ),
      h5(icon("table"), " Automatic Inverse Compensation Matrix:"),
      uiOutput("comp_matrix_display_styled"),
      hr(),
      # Compensation effect visualization
      h5("Compensation Effect (Before vs After)"),
      plotOutput("comp_visualization", height = "800px"),
      hr(),
      p(
        style = "font-size: 0.9em; color = #666;",
        icon("info-circle"), " Compensation applied to selected fluorescent channels."
      ),
      
      # NEW: Manual Adjustment UI
      hr(),
      div(
        style = "background: #f1f8e9; padding: 20px; border-radius: 8px; border: 1px solid #c5e1a5;",
        h4(icon("sliders-h"), " Manual Matrix Adjustment"),
        p("Here you can fine-tune the compensation for specific cell lines or apply a global correction. The table below displays the **Spillover Matrix** (not the inverse)."),
        
        fluidRow(
          column(4, 
                 selectInput("comp_edit_cell_line", "Select Cell Line to Edit:", 
                             choices = c("Global Master", if(!is.null(rv$metadata)) unique(rv$metadata$cell_line) else NULL))
          ),
          column(4,
                 actionButton("apply_comp_to_all", "Apply Current Matrix to All", icon = icon("copy"), class = "btn-warning btn-block", style = "margin-top: 25px;")
          ),
          column(4,
                 actionButton("reset_comp", "Reset to Calculated", icon = icon("undo"), class = "btn-secondary btn-block", style = "margin-top: 25px;")
          ),
          column(4,
                 actionButton("save_manual_comp", "Save Manual Adjustments", icon = icon("save"), class = "btn-success btn-block", style = "margin-top: 25px;")
          )
        ),
        br(),
        h5("Spillover Matrix (Editable)"),
        p(style = "font-size: 0.9em; color: #666;", "Click on a cell to edit the spillover coefficient (ratio 0-1). Rows = Fluorophores, Columns = Detectors."),
        DT::dataTableOutput("spill_matrix_editor"),
        br(),
        h5("Real-Time Preview"),
        p(style = "font-size: 0.9em; color: #666;", "Visualizing a random sample from the selected cell line with the current matrix applied."),
        plotOutput("comp_preview_plot", height = "400px")
      )
    )
  })

  # === MANUAL COMPENSATION ADJUSTMENT LOGIC ===

  # 1. Render the Editable Spillover Matrix
  output$spill_matrix_editor <- DT::renderDataTable({
    req(rv$comp_calculated, input$comp_edit_cell_line)
    
    selected_line <- input$comp_edit_cell_line
    
    # Determine which matrix to show
    mat_to_show <- if (selected_line == "Global Master") {
      rv$spill_matrix
    } else {
      rv$spill_matrices[[selected_line]]
    }
    
    # If cell line specific matrix doesn't exist yet (e.g. new file), fallback to global
    if (is.null(mat_to_show)) mat_to_show <- rv$spill_matrix
    
    DT::datatable(
      round(mat_to_show, 4), 
      editable = TRUE, 
      selection = "none",
      options = list(dom = 't', paging = FALSE, ordering = FALSE)
    )
  })

  # 2. Handle Cell Edits in the Matrix
  observeEvent(input$spill_matrix_editor_cell_edit, {
    info <- input$spill_matrix_editor_cell_edit
    selected_line <- input$comp_edit_cell_line
    
    # Get current matrix
    current_mat <- if (selected_line == "Global Master") {
      rv$spill_matrix
    } else {
      rv$spill_matrices[[selected_line]]
    }
    if (is.null(current_mat)) current_mat <- rv$spill_matrix

    # Update value (DT uses row/col indices starting at 1)
    new_val <- as.numeric(info$value)
    if (is.na(new_val)) return()
    
    current_mat[info$row, info$col] <- new_val
    
    # Calculate new Inverse Matrix
    tryCatch({
      new_inverse <- solve(current_mat)
      
      # Save back to RVs
      if (selected_line == "Global Master") {
        rv$spill_matrix <- current_mat
        rv$comp_matrix <- new_inverse
      } else {
        rv$spill_matrices[[selected_line]] <- current_mat
        rv$comp_matrices[[selected_line]] <- new_inverse
      }
    }, error = function(e) {
      showNotification("Invalid Matrix (singular)", type = "error")
    })
  })

  # 3. Preview Plot Logic
  output$comp_preview_plot <- renderPlot({
    req(rv$comp_calculated, input$comp_edit_cell_line)
    
    selected_line <- input$comp_edit_cell_line
    
    # Identify a file to visualize
    file_to_plot <- NULL
    
    if (selected_line == "Global Master") {
      # Pick the first available file from metadata
      if (!is.null(rv$metadata) && nrow(rv$metadata) > 0) {
        file_to_plot <- rv$metadata[1, ]
      }
    } else {
      # Pick first file for this cell line
      if (!is.null(rv$metadata)) {
        files <- rv$metadata[rv$metadata$cell_line == selected_line, ]
        if (nrow(files) > 0) file_to_plot <- files[1, ]
      }
    }
    
    if (is.null(file_to_plot)) return(NULL)
    
    # Load Data
    fs <- read.FCS(file_to_plot$datapath)
    
    # Get the Compensation Matrix to use
    comp_mat_use <- if (selected_line == "Global Master") {
      rv$comp_matrix
    } else {
      rv$comp_matrices[[selected_line]]
    }
    
    # Check if comp matrix is valid
    if (is.null(comp_mat_use)) comp_mat_use <- rv$comp_matrix
    
    # Compensate
    fs_comp <- tryCatch(compensate(fs, comp_mat_use), error = function(e) fs)
    
    # Transform using Manual Logicle (w=1.0) for consistency with Gating Plot
    trans <- logicleTransform(w = 0.5, t = 1000000, m = 4.5, a = 0)
    data_raw <- exprs(fs_comp)
    
    # Pick 2 channels to plot
    ch_cols <- colnames(comp_mat_use)
    if (length(ch_cols) < 2) return(NULL)
    
    ch1 <- ch_cols[1]
    ch2 <- ch_cols[2]
    
    # Apply transformation
    df_plot <- data.frame(
      x = trans(data_raw[, ch1]),
      y = trans(data_raw[, ch2])
    )
    
    # Subsample for speed
    if (nrow(df_plot) > 5000) df_plot <- df_plot[sample(nrow(df_plot), 5000), ]
    
    p <- ggplot(df_plot, aes(x = x, y = y)) +
      geom_point(alpha = 0.3, size = 0.5, color = "#2196F3") +
      stat_density_2d(color = "black", alpha = 0.3) +
      labs(
        title = paste("Preview:", file_to_plot$name),
        subtitle = paste("Matrix:", selected_line),
        x = ch1, y = ch2
      ) +
      theme_publication() +
      theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.5))
    rv$comp_plot_obj_current <- p # Store the current plot object
    p # Return the plot
  })

  # 4. Apply to All
  observeEvent(input$apply_comp_to_all, {
    req(rv$comp_calculated, input$comp_edit_cell_line)
    selected_line <- input$comp_edit_cell_line
    
    # Get the source matrix
    source_spill <- if (selected_line == "Global Master") rv$spill_matrix else rv$spill_matrices[[selected_line]]
    source_comp <- if (selected_line == "Global Master") rv$comp_matrix else rv$comp_matrices[[selected_line]]
    
    if (is.null(source_spill)) return()
    
    # Apply to all lines
    if (!is.null(rv$metadata)) {
      lines <- unique(rv$metadata$cell_line)
      for (line in lines) {
        rv$spill_matrices[[line]] <- source_spill
        rv$comp_matrices[[line]] <- source_comp
      }
    }
    
    # Also update global master
    rv$spill_matrix <- source_spill
    rv$comp_matrix <- source_comp
    
    showNotification("Current matrix applied to ALL cell lines.", type = "message")
  })

  # 5. Reset to Calculated
  observeEvent(input$reset_comp, {
    req(rv$comp_calculated, input$comp_edit_cell_line)
    selected_line <- input$comp_edit_cell_line
    
    # Recalculate global master logic (or just revert)
    # Simplest way: Re-run the core calculation logic or store a "Backup"
    # For now, let's just grab the Global Master and apply it to the selected line (Reset to Global)
    
    if (selected_line != "Global Master") {
      rv$spill_matrices[[selected_line]] <- rv$spill_matrix
      rv$comp_matrices[[selected_line]] <- rv$comp_matrix
      showNotification(paste("Reset", selected_line, "to Global Master."), type = "message")
    } else {
       showNotification("To reset Global Master, please click 'Calculate Compensation Matrix' again.", type = "warning")
    }
  })

  # 6. Save Manual Adjustments & Capture Plot Object for Report
  observeEvent(input$save_manual_comp, {
    req(input$comp_edit_cell_line, !is.null(rv$comp_plot_obj_current))
    
    selected_line <- input$comp_edit_cell_line
    
    # Store the CURRENT professional plot object for the report
    rv$manual_comp_preview_snapshots[[selected_line]] <- rv$comp_plot_obj_current
    
    showNotification(paste("Compensation settings and preview plot saved for", selected_line), type = "message")
  })

  output$comp_matrix_display_styled <- renderUI({
    req(rv$comp_matrix)
    HTML(kableExtra::kable(round(rv$comp_matrix, 4), format = "html") %>% 
         kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "condensed"), 
                                   full_width = FALSE, position = "left"))
  })


  # NEW: Compensation visualization
  output$comp_visualization <- renderPlot({
    req(rv$comp_plot_obj)
    grid::grid.draw(rv$comp_plot_obj)
  })

  # === FILE INPUT HANDLING ===
  observeEvent(input$files, {
    req(input$files)

    new_files <- input$files

    if (is.null(rv$file_data) || nrow(rv$file_data) == 0) {
      rv$file_data <- new_files
    } else {
      # Identify new files not already in rv$file_data based on datapath
      existing_datapaths <- rv$file_data$datapath
      new_unique_files <- new_files[!new_files$datapath %in% existing_datapaths, ]

      if (nrow(new_unique_files) > 0) {
        rv$file_data <- rbind(rv$file_data, new_unique_files)
        showNotification(paste("Added", nrow(new_unique_files), "new files."), type = "message")
      } else {
        showNotification("No new unique files to add.", type = "warning")
      }
    }
  })

  # Reactive expression to process file metadata from rv$file_data
  processed_metadata <- reactive({
    req(rv$file_data)

    metadata <- rv$file_data
    metadata$cell_line <- character(nrow(metadata))
    metadata$replicate <- character(nrow(metadata))
    metadata$treatment_full <- character(nrow(metadata))
    metadata$concentration_uM <- numeric(nrow(metadata))

    for (i in 1:nrow(metadata)) {
      name_clean <- str_replace(metadata$name[i], "\\.[fF][cC][sS]$", "")
      parts <- str_split(name_clean, "_")[[1]]

      if (length(parts) >= 3) {
        metadata$cell_line[i] <- parts[1]
        metadata$replicate[i] <- parts[length(parts)]
        metadata$treatment_full[i] <- paste(parts[2:(length(parts) - 1)], collapse = "_")

        conc <- as.numeric(str_extract(metadata$treatment_full[i], "[0-9\\.]+(?=[uU][mM])"))
        metadata$concentration_uM[i] <- ifelse(is.na(conc), 0, conc)
      } else {
        metadata$cell_line[i] <- NA_character_
      }
    }

    metadata <- metadata %>% filter(!is.na(cell_line))

    if (nrow(metadata) == 0) {
      showNotification("No valid files found after parsing.", type = "error")
      return(NULL)
    }

    metadata
  })

  # Observer to update rv$metadata and reset state when processed_metadata changes
  observeEvent(processed_metadata(), {
    current_metadata <- processed_metadata()
    if (!is.null(current_metadata) && nrow(current_metadata) > 0) {
      rv$metadata <- current_metadata
      rv$current_cell_line_index <- 1
      rv$gating_step <- if (input$is_absolute_counting) "bead_gate" else "fsc_ssc"
      rv$thresholds <- list()
      rv$control_concentration <- NULL
      showNotification(paste("Successfully processed", nrow(current_metadata), "valid files."), type = "message")
      
      # Populate bead channel choices
      tryCatch({
        fs_temp <- read.FCS(current_metadata$datapath[1], truncate_max_range = FALSE)
        # Use colnames(fs) directly which is more robust than colnames(exprs(fs))
        all_chans <- colnames(fs_temp)
        
        # Filter for fluorescence channels (remove FSC, SSC, Time)
        # Also remove Width (W) and Height (H) to keep it simple, or keep them if preferred.
        # Scientific standard: primarily Area (A) is used for gating.
        fluor_chans <- all_chans[!grepl("FSC|SSC|Time", all_chans, ignore.case = TRUE)]
        
        # Include SSC-A as an option for bead gating
        ssc_chan <- all_chans[grepl("SSC-A", all_chans, ignore.case = TRUE)]
        final_bead_choices <- unique(c(ssc_chan, fluor_chans))

        # Provide clean labels for the dropdown
        updateSelectInput(session, "bead_gate_channel", 
                         choices = final_bead_choices, 
                         selected = if("BL1-A" %in% final_bead_choices) "BL1-A" else final_bead_choices[1])
      }, error = function(e) {
          message("Bead channel extraction error: ", e$message)
      })
      
    } else {
      rv$metadata <- NULL
      rv$current_cell_line_index <- 1
      rv$gating_step <- if (input$is_absolute_counting) "bead_gate" else "fsc_ssc"
      rv$thresholds <- list()
      rv$control_concentration <- NULL
      showNotification("No valid files available for analysis.", type = "warning")
    }
  })

  # NEW: Observer to reset state when absolute counting is switched
  observeEvent(input$is_absolute_counting, {
      showNotification("Absolute counting mode changed. Resetting gating progress.", type = "warning")
      rv$current_cell_line_index <- 1
      rv$gating_step <- if (input$is_absolute_counting) "bead_gate" else "fsc_ssc"
      rv$polygon_points <- data.frame(x = numeric(), y = numeric())
      rv$temp_bead_gates <- list()
      rv$thresholds <- list()
  }, ignoreInit = TRUE)

  # Remove All Files
  observeEvent(input$remove_all_files, {
    rv$file_data <- NULL
    showNotification("All files removed.", type = "message")
  })

  # Remove Selected Files
  observeEvent(input$remove_selected_files, {
    req(input$file_table_rows_selected)
    if (!is.null(rv$file_data) && length(input$file_table_rows_selected) > 0) {
      rv$file_data <- rv$file_data[-input$file_table_rows_selected, ]
      showNotification(paste(length(input$file_table_rows_selected), "selected files removed."), type = "message")
      if (nrow(rv$file_data) == 0) {
        rv$file_data <- NULL # Ensure it's truly NULL if empty
      }
    } else {
      showNotification("No files selected for removal.", type = "warning")
    }
  })


  # === CONTROL SELECTION UI (UNCHANGED) ===
  output$control_selection_ui <- renderUI({
    req(rv$metadata)

    if (is.null(rv$control_concentration)) {
      conc_options <- sort(unique(rv$metadata$concentration_uM))

      return(div(
        style = "background: #fff8e1; padding: 15px; border-radius: 8px; border-left: 5px solid #ff9800;",
        h6(icon("flask"), " Select Control", style = "margin-top: 0; color: #e65100;"),
        p("Which concentration represents your control samples?",
          style = "font-size: 0.9em; margin-bottom: 10px;"
        ),
        selectInput("control_conc", "Control Concentration (µM):",
          choices = conc_options,
          selected = 0
        ),
        actionButton("set_control", "Set Control & Start Gating",
          class = "btn-primary btn-block",
          icon = icon("check")
        )
      ))
    } else {
      return(div(
        style = "background: #e8f5e9; padding: 15px; border-radius: 8px; border-left: 5px solid #4caf50;",
        h6(icon("check-circle"), " Control Set", style = "margin-top: 0; color: #2e7d32;"),
        p(paste("Control:", rv$control_concentration, "µM"),
          style = "font-size: 0.9em; margin: 0;"
        )
      ))
    }
  })

  observeEvent(input$set_control, {
    req(input$control_conc)
    rv$control_concentration <- as.numeric(input$control_conc)
    rv$gating_step <- if (input$is_absolute_counting) "bead_gate" else "fsc_ssc"
    updateTabsetPanel(session, "tabs", selected = "Gating")
    showNotification(paste("Control set to", rv$control_concentration, "µM"), type = "message")
  })

  # NEW: Observer to reset state when workflow is switched
  observeEvent(input$is_apoptosis_assay,
    {
      showNotification("Workflow mode changed. Resetting gating progress and results.", type = "warning")

      # Reset all gating and results data
      rv$thresholds <- list()
      rv$results <- NULL
      rv$ic50_results <- NULL
      rv$ld50_results <- NULL
      rv$viability_data <- NULL
      rv$death_data <- NULL
      rv$predicted_viability_curves <- NULL
      rv$predicted_death_curves <- NULL
      rv$current_cell_line_index <- 1
      rv$gating_step <- if (input$is_absolute_counting) "bead_gate" else "fsc_ssc"
      rv$polygon_points <- data.frame(x = numeric(), y = numeric())
      rv$temp_fsc_ssc_gates <- NULL
      rv$temp_singlet_gates <- NULL
      rv$temp_gated_data_cells <- NULL
      rv$temp_gated_data_singlets <- NULL
      rv$qc_data <- NULL
      rv$outlier_flags <- NULL
      rv$cell_counts <- NULL
      rv$advanced_metrics <- NULL

      # Reset generic results as well
      rv$generic_results <- NULL
      rv$generic_ec50_results <- NULL

      # Reset generic gating definitions
      rv$generic_gate_defs <- list()
      rv$generic_gate_counter <- 0
    },
    ignoreInit = TRUE
  ) # ignoreInit = TRUE is crucial to not fire on app startup


  # === WORKFLOW STATUS (UNCHANGED) ===
  output$workflow_status <- renderUI({
    if (is.null(rv$metadata)) {
      return(div(
        class = "alert alert-info alert-custom",
        icon("info-circle"), " Upload .fcs files to begin"
      ))
    }

    if (is.null(rv$control_concentration)) {
      return(NULL)
    }

    cell_lines <- unique(rv$metadata$cell_line)
    n_gated <- length(rv$thresholds)

    if (n_gated == length(cell_lines)) {
      return(div(
        class = "alert alert-success alert-custom",
        icon("check-circle"), " All cell lines gated! Ready to analyze."
      ))
    } else {
      return(div(
        class = "alert alert-warning alert-custom",
        HTML(sprintf(
          "%s Progress: %d/%d cell lines gated",
          icon("hourglass-half"), n_gated, length(cell_lines)
        ))
      ))
    }
  })

  output$analyze_button_ui <- renderUI({
    req(rv$metadata, rv$control_concentration)

    cell_lines <- unique(rv$metadata$cell_line)
    n_gated <- length(rv$thresholds)

    if (n_gated == length(cell_lines)) {
      return(actionButton("analyze", "Start Analysis",
        icon = icon("play"),
        class = "btn-success btn-lg btn-block",
        style = "font-size: 1.1em; padding: 12px;"
      ))
    } else {
      return(actionButton("analyze_disabled", "Complete Gating First",
        icon = icon("lock"),
        class = "btn-secondary btn-lg btn-block",
        disabled = TRUE,
        style = "font-size: 1.1em; padding: 12px;"
      ))
    }
  })

  output$file_table <- DT::renderDataTable({
    req(rv$metadata)
    display_data <- rv$metadata %>%
      dplyr::select(
        Name = name, `Cell Line` = cell_line,
        Treatment = treatment_full, `Concentration (uM)` = concentration_uM,
        Replicate = replicate
      ) %>%
      arrange(`Cell Line`, `Concentration (uM)`, Replicate)

    DT::datatable(
      display_data,
      selection = "multiple", # Enable row selection
      options = list(
        paging = TRUE,
        pageLength = 10,
        dom = "tip", # Table, Info, Paging
        autoWidth = TRUE,
        scrollX = TRUE
      )
    )
  })

  # NEW: Gate Review UI
  output$gate_review_ui <- renderUI({
    req(rv$thresholds)

    if (length(rv$thresholds) == 0) {
      return(div(
        class = "alert alert-info",
        h5(icon("info-circle"), " No Gates Saved Yet"),
        p("Complete gating for at least one cell line to review gates here.")
      ))
    }

    cell_lines <- names(rv$thresholds)

    tagList(
      p(strong("Saved gates for ", length(cell_lines), " cell line(s)")),
      lapply(cell_lines, function(line) {
        gate_info <- rv$thresholds[[line]]


        # Determine which type of gate summary to display
        gate_summary_ui <- if (!is.null(gate_info$annexin_pi)) {
          # Apoptosis Gate Summary
          tagList(
            p(strong("Analysis Type:"), " Apoptosis (Annexin V / PI)"),
            p(
              strong("Annexin/PI Thresholds:"), " Annexin=",
              round(gate_info$annexin_pi$annexin, 2),
              ", PI=", round(gate_info$annexin_pi$pi, 2)
            )
          )
        } else if (!is.null(gate_info$generic_gate_list)) {
          # Multi-Channel Generic Gate Summary
          gates_str <- sapply(gate_info$generic_gate_list, function(g) {
            paste0(g$channel, " (> ", round(as.numeric(g$threshold %||% 0), 2), ")")
          })
          tagList(
            p(strong("Analysis Type:"), " Generic Analysis (Multi-Channel)"),
            p(strong("Thresholds:"), paste(gates_str, collapse = "; "))
          )
        } else if (!is.null(gate_info$generic_gate)) {
          # Legacy Single-Channel Generic Gate Summary
          tagList(
            p(strong("Analysis Type:"), " Generic Analysis (Legacy)"),
            p(
              strong("Target Channel Gate:"), " Channel=",
              gate_info$generic_gate$channel,
              ", Threshold=", round(as.numeric(gate_info$generic_gate$threshold %||% 0), 2)
            )
          )
        } else {
          # Fallback for unknown gate type
          p(strong("Gate Type:"), " Unknown or Incomplete")
        }


        div(
          style = "background: #f8f9fa; padding: 15px; margin: 10px 0; border-radius: 8px; border-left: 4px solid #2196F3;",
          h5(icon("circle"), " ", line),
          p(
            strong("FSC/SSC Gate:"), " Polygon with ",
            nrow(gate_info$fsc_ssc$polygon), " points"
          ),
          p(
            strong("Singlet Gate:"), " Polygon with ",
            nrow(gate_info$singlets$polygon), " points"
          ),
          gate_summary_ui, # Insert the correct summary UI
          actionButton(paste0("edit_gate_", line), "Re-gate This Cell Line",
            class = "btn-warning btn-sm",
            icon = icon("edit")
          ),
          hr(style = "margin: 10px 0;"),
          # Gate visualization plots (this part might need adjustment in the future)
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
          # Render the combined plot object
          req(rv$gate_review_plots[[line_local]])
          grid::grid.draw(rv$gate_review_plots[[line_local]])
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
          rv$gating_step <- if (input$is_absolute_counting) "bead_gate" else "fsc_ssc"
          rv$polygon_points <- data.frame(x = numeric(), y = numeric())
          updateTabsetPanel(session, "tabs", selected = "Gating")
          showNotification(paste("Re-gating", line_local), type = "message")
        })
      })
    })
  })
  # Central observer to load data for gating (Bead Gate or FSC/SSC)
  observe({
    # Trigger when we are at the start of a gating process
    req(
      rv$gating_step %in% c("bead_gate", "fsc_ssc"),
      rv$metadata,
      rv$control_concentration,
      rv$current_cell_line_index <= length(unique(rv$metadata$cell_line))
    )

    cell_lines <- unique(rv$metadata$cell_line)
    current_line <- cell_lines[rv$current_cell_line_index]

    control_file <- rv$metadata %>%
      filter(cell_line == current_line, concentration_uM == rv$control_concentration) %>%
      slice(1)

    if (nrow(control_file) > 0) {
      fs <- read.FCS(control_file$datapath, truncate_max_range = FALSE)
      data_raw <- exprs(fs)
      
      # Handle Bead Exclusion for FSC/SSC plot if applicable
      data_for_fsc_ssc <- data_raw
      if (input$is_absolute_counting && !is.null(rv$temp_bead_gates[[current_line]]) && !is.null(input$bead_gate_channel)) {
        gate <- rv$temp_bead_gates[[current_line]]
        bead_chan <- input$bead_gate_channel
        if (gate$type == "polygon") {
          trans_temp <- logicleTransform(w = 0.5, t = 1000000, m = 4.5, a = 0)
          in_bead_gate <- point.in.polygon(
            data_raw[, "FSC-A"],
            trans_temp(data_raw[, bead_chan]),
            gate$polygon$x, gate$polygon$y
          ) > 0
          data_for_fsc_ssc <- data_raw[!in_bead_gate, ]
        }
      }

      # Pre-calculate transformed fluorescence for the SELECTED channel
      fl_trans <- NULL
      if (!is.null(input$bead_gate_channel)) {
          bead_chan <- input$bead_gate_channel
          if (grepl("SSC", bead_chan, ignore.case = TRUE)) {
             # Use linear scaling for SSC
             fl_trans <- data_raw[, bead_chan]
          } else {
             # Use logicle transformation for fluorescence
             trans_obj <- logicleTransform(w = 0.5, t = 1000000, m = 4.5, a = 0)
             fl_trans <- trans_obj(data_raw[, bead_chan])
          }
      }

      # PRE-SAMPLE for UI responsiveness
      # Store sampled indices so plots and stats use the SAME subset
      sample_size <- 10000
      total_ev <- nrow(data_for_fsc_ssc)
      idx_sample <- if(total_ev > sample_size) sample(seq_len(total_ev), sample_size) else seq_len(total_ev)
      
      # For Bead Gating, we need the original data (including beads)
      total_orig <- nrow(data_raw)
      idx_sample_orig <- if(total_orig > sample_size) sample(seq_len(total_orig), sample_size) else seq_len(total_orig)

      # Prepare the lists locally first to ensure ATOMIC assignment to reactiveValues
      new_gating_data <- list(
        cell_line = current_line,
        data_raw = data_for_fsc_ssc,
        # Sampled data specifically for fast UI rendering
        fsc = data_for_fsc_ssc[idx_sample, "FSC-A"],
        ssc = data_for_fsc_ssc[idx_sample, "SSC-A"],
        fsc_full = data_for_fsc_ssc[, "FSC-A"], # Keep for point.in.polygon accuracy later
        ssc_full = data_for_fsc_ssc[, "SSC-A"]
      )
      
      if (rv$gating_step == "bead_gate") {
          new_gating_data$data_raw_original <- data_raw
          new_gating_data$fsc = data_raw[idx_sample_orig, "FSC-A"]
          new_gating_data$ssc = data_raw[idx_sample_orig, "SSC-A"]
          new_gating_data$fl_trans = fl_trans[idx_sample_orig] # Sampled transformed FL
          new_gating_data$fl_trans_full = fl_trans # Full transformed FL
          new_gating_data$data_raw <- data_raw
      }

      # Atomic assignment
      rv$current_gating_data_fsc <- new_gating_data

    } else {
      # Invalidate the data if no control file is found
      rv$current_gating_data_fsc <- NULL
      showNotification(paste("No control file found for", current_line, "at", rv$control_concentration, "µM"), type = "error")
    }
  })


  # Central observer to prepare data for singlet gating
  observe({
    req(rv$gating_step == "singlets", rv$temp_gated_data_cells)


    data_cells <- rv$temp_gated_data_cells
    current_line <- unique(rv$metadata$cell_line)[rv$current_cell_line_index]


    rv$current_gating_data_singlet <- list(
      fsc_a = data_cells[, "FSC-A"],
      fsc_h = data_cells[, "FSC-H"],
      cell_line = current_line,
      data_cells = data_cells
    )
  })


  # === GATING UI (UNCHANGED LOGIC) ===
  output$gating_ui <- renderUI({
    req(rv$metadata, rv$control_concentration)

    cell_lines <- unique(rv$metadata$cell_line)

    if (rv$current_cell_line_index > length(cell_lines)) {
      return(div(
        class = "alert alert-success alert-custom",
        h4(icon("check-circle"), " Gating Complete!"),
        p("All cell lines have been gated. Go to the Results tab and click 'Start Analysis'.")
      ))
    }


    current_line <- cell_lines[rv$current_cell_line_index]


    # Check if data is loaded by the central observer
    if (is.null(rv$current_gating_data_fsc)) {
      return(div(
        class = "alert alert-danger alert-custom",
        h5(icon("exclamation-triangle"), " Error"),
        p(paste("No control file found for", current_line, "at", rv$control_concentration, "µM")),
        actionButton("skip_line", "Skip This Cell Line", class = "btn-warning")
      ))
    }

    # === STEP 0: BEAD GATE (POLYGON ONLY) ===
    if (rv$gating_step == "bead_gate") {
      return(tagList(
        div(
          class = "alert alert-danger alert-custom",
          h5(icon("bullseye"), " Step 0: Bead Gating - ", strong(current_line)),
          p("Circle the high-fluorescence beads. These will be EXCLUDED from cell counts.", style = "margin-bottom: 0;")
        ),
        div(
          style = "background: #fff3cd; padding: 10px; border-radius: 5px; margin-top: 10px;",
          p(icon("mouse-pointer"), " Click on plot to add points. Close polygon when done.",
            style = "margin: 5px 0;"
          ),
          actionButton("clear_polygon_bead", "Clear Points",
            class = "btn-warning btn-sm", icon = icon("eraser")
          ),
          actionButton("close_polygon_bead", "Close Polygon",
            class = "btn-success btn-sm", icon = icon("check-circle")
          )
        ),
        plotOutput("bead_plot", height = "450px", click = "bead_click"),
        div(class = "alert alert-secondary", verbatimTextOutput("bead_stats")),
        hr(),
        actionButton("next_to_fsc", "Next: FSC/SSC Gate →",
          class = "btn-primary btn-lg btn-block", icon = icon("arrow-right")
        )
      ))
    }

    # === STEP 1: FSC/SSC (POLYGON ONLY) ===
    if (rv$gating_step == "fsc_ssc") {
      return(tagList(
        div(
          class = "alert alert-info alert-custom",
          h5(icon("circle"), " Step 1/3: FSC/SSC Gate - ", strong(current_line)),
          p("Gate cells and remove debris", style = "margin-bottom: 0;")
        ),
        div(
          style = "background: #fff3cd; padding: 10px; border-radius: 5px; margin-top: 10px;",
          p(icon("mouse-pointer"), " Click on plot to add points. Close polygon when done.",
            style = "margin: 5px 0;"
          ),
          actionButton("clear_polygon_fsc", "Clear Points",
            class = "btn-warning btn-sm", icon = icon("eraser")
          ),
          actionButton("close_polygon_fsc", "Close Polygon",
            class = "btn-success btn-sm", icon = icon("check-circle")
          )
        ),
        plotOutput("fsc_ssc_plot", height = "450px", click = "fsc_ssc_click"),
        div(class = "alert alert-secondary", verbatimTextOutput("fsc_ssc_stats")),
        hr(),
        actionButton("next_to_singlets", "Next: Singlet Gate →",
          class = "btn-primary btn-lg btn-block", icon = icon("arrow-right")
        )
      ))
    }

    # === STEP 2: SINGLETS (POLYGON ONLY) ===
    else if (rv$gating_step == "singlets") {
      req(rv$temp_gated_data_cells, rv$current_gating_data_singlet)

      return(tagList(
        div(
          class = "alert alert-info alert-custom",
          h5(icon("circle"), " Step 2/3: Singlet Discrimination - ", strong(current_line)),
          p("Remove doublets using FSC-A vs FSC-H", style = "margin-bottom: 0;")
        ),
        div(
          style = "background: #fff3cd; padding: 10px; border-radius: 5px; margin-top: 10px;",
          p(icon("mouse-pointer"), " Click on plot to add points. Close polygon when done.",
            style = "margin: 5px 0;"
          ),
          actionButton("clear_polygon_singlet", "Clear Points",
            class = "btn-warning btn-sm", icon = icon("eraser")
          ),
          actionButton("close_polygon_singlet", "Close Polygon",
            class = "btn-success btn-sm", icon = icon("check-circle")
          )
        ),
        plotOutput("singlet_plot", height = "450px", click = "singlet_click"),
        div(class = "alert alert-secondary", verbatimTextOutput("singlet_stats")),
        hr(),
        fluidRow(
          column(6, actionButton("back_to_fsc", "← Back: FSC/SSC",
            class = "btn-secondary btn-lg btn-block", icon = icon("arrow-left")
          )),
          column(6, actionButton("next_to_annexin", "Next: Annexin/PI →",
            class = "btn-primary btn-lg btn-block", icon = icon("arrow-right")
          ))
        )
      ))
    }

    # === STEP 3: ANNEXIN/PI (RECTANGLE ONLY) ===
    else if (rv$gating_step == "annexin_pi") {
      req(rv$temp_gated_data_singlets)

      data_singlets <- rv$temp_gated_data_singlets

      # Apply compensation if available
      trans <- logicleTransform(w = 0.5, t = 1000000, m = 4.5, a = 0)

      compensated_fs <- NULL
      
                    # Determine correct SPILLOVER matrix for this cell line
                    # flowCore::compensate expects the SPILLOVER matrix (it inverts it internally)
                    comp_mat_to_use <- NULL
                    if (rv$comp_calculated) {
                       if (!is.null(rv$spill_matrices[[current_line]])) {
                         comp_mat_to_use <- rv$spill_matrices[[current_line]]
                       } else {
                         comp_mat_to_use <- rv$spill_matrix
                       }
                    }
      
                    if (!is.null(comp_mat_to_use)) {
                      # Create flowFrame directly - flowCore handles parameters automatically
                      ff_singlets <- flowFrame(exprs = data_singlets)
      
                      # Use the specific matrix for compensate()
                      compensated_fs <- compensate(ff_singlets, comp_mat_to_use)                
                # --- REFINED VISUALIZATION (Manual Logicle) ---
                # Instead of estimateLogicle (which fails on non-linear artifacts),
                # we use a standard logicleTransform with a wider linear region.
                # This 'absorbs' the negative values from compensation into a natural cluster.
                trans_func <- logicleTransform(w = 0.5, t = 1000000, m = 4.5, a = 0)
                
                # Apply directly to matrix to avoid transform() scoping issues
                exprs_data <- exprs(compensated_fs)
                annexin <- trans_func(exprs_data[, "BL1-A"])
                pi <- trans_func(exprs_data[, "BL2-A"])
                
                rv$current_gating_data_transform <- trans_func
                
              } else {
        annexin <- trans(data_singlets[, "BL1-A"])
        pi <- trans(data_singlets[, "BL2-A"])
        rv$current_gating_data_transform <- NULL
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
        div(
          class = "alert alert-info alert-custom",
          h5(icon("circle"), " Step 3/3: Annexin V / PI - ", strong(current_line)),
          p("Set viability/apoptosis thresholds", style = "margin-bottom: 0;")
        ),
        # Diagnostic: Show applied matrix
        if (!is.null(comp_mat_to_use)) {
           div(style="margin-bottom: 15px; border: 1px solid #ddd; padding: 10px; border-radius: 5px; background: #f9f9f9;",
               h6(icon("table"), " Applied Spillover Matrix", style="margin-top:0; color: #555;"),
               HTML(kableExtra::kable(round(comp_mat_to_use, 4), format = "html") %>% 
                    kableExtra::kable_styling(bootstrap_options = "condensed", full_width = FALSE, position = "left"))
           )
        } else {
           div(class="alert alert-warning", icon("exclamation-circle"), " No Compensation Matrix Applied")
        },
        fluidRow(
          column(
            6,
            sliderInput("annexin_threshold", "Annexin V Threshold:",
              min = floor(min(annexin)), max = ceiling(max(annexin)),
              value = start_annexin, step = 0.05
            )
          ),
          column(
            6,
            sliderInput("pi_threshold", "PI Threshold:",
              min = floor(min(pi)), max = ceiling(max(pi)),
              value = start_pi, step = 0.05
            )
          )
        ),
        plotOutput("annexin_plot", height = "450px"),
        uiOutput("annexin_stats_ui"),
        hr(),
        fluidRow(
          column(6, actionButton("back_to_singlet", "← Back: Singlets",
            class = "btn-secondary btn-lg btn-block", icon = icon("arrow-left")
          )),
          column(6, actionButton("save_gate", "Save & Next Cell Line ✓",
            class = "btn-success btn-lg btn-block", icon = icon("check")
          ))
        )
      ))
    }
  })
  
  # Debug output for matrix
  output$debug_comp_matrix <- renderPrint({
    req(rv$comp_calculated)
    cell_line <- unique(rv$metadata$cell_line)[rv$current_cell_line_index]
    if (!is.null(rv$comp_matrices[[cell_line]])) {
      print(rv$comp_matrices[[cell_line]])
    } else {
      print(rv$comp_matrix)
    }
  })

  # === PLOT OUTPUTS (UNCHANGED) ===

  output$bead_plot <- renderPlot({
    req(rv$current_gating_data_fsc, input$bead_gate_channel)
    req(rv$gating_step == "bead_gate") # Reactivity Guard

    # Use the pre-calculated sampled data for speed
    bead_chan <- input$bead_gate_channel
    
    # Extra safety check for data existence
    if (is.null(rv$current_gating_data_fsc$fsc) || is.null(rv$current_gating_data_fsc$fl_trans)) return(NULL)
    
    df <- data.frame(
      FSC = rv$current_gating_data_fsc$fsc,
      FL  = rv$current_gating_data_fsc$fl_trans
    )

    p <- ggplot(df, aes(x = FSC, y = FL)) +
      geom_point(alpha = 0.2, size = 0.5, color = "black") +
      scale_x_continuous(labels = custom_log_labels) +
      scale_y_continuous(labels = custom_log_labels) +
      labs(
        title = paste("Bead Gate -", rv$current_gating_data_fsc$cell_line),
        subtitle = paste("Identify the high-fluorescence bead population using", bead_chan),
        x = "FSC-A", y = paste("Fluorescence (", bead_chan, ", transformed)")
      ) +
      theme_bw(base_size = 14) +
      theme(legend.position = "none", plot.title = element_text(face = "bold"))

    # Only draw polygon if we have at least 2 points
    if (nrow(rv$polygon_points) >= 1) {
      if (nrow(rv$polygon_points) >= 2) {
        p <- p + geom_path(
          data = rv$polygon_points, aes(x = x, y = y),
          color = "red", linewidth = 0.5
        )
      }
      p <- p + geom_point(
        data = rv$polygon_points, aes(x = x, y = y),
        color = "red", size = 1, shape = 21, fill = "white", stroke = 0.5
      )
    }
    p
  })

  output$fsc_ssc_plot <- renderPlot({
    req(rv$current_gating_data_fsc)

    # Efficiently extract only needed columns
    fsc_data <- rv$current_gating_data_fsc$fsc
    ssc_data <- rv$current_gating_data_fsc$ssc
    
    # Subsample if too large
    if (length(fsc_data) > 10000) {
        idx <- sample(seq_along(fsc_data), 10000)
        fsc_data <- fsc_data[idx]
        ssc_data <- ssc_data[idx]
    }
    
    df <- data.frame(FSC = fsc_data, SSC = ssc_data)

    p <- ggplot(df, aes(x = FSC, y = SSC)) +
      stat_density_2d(aes(fill = after_stat(level)), geom = "polygon", alpha = 0.6) +
      geom_point(alpha = 0.1, size = 0.3, color = "black") +
      scale_fill_viridis_c(option = "viridis") +
      scale_x_continuous(labels = custom_log_labels) +
      scale_y_continuous(labels = custom_log_labels) +
      labs(
        title = paste("FSC/SSC Gate -", rv$current_gating_data_fsc$cell_line),
        x = "FSC-A", y = "SSC-A"
      ) +
      theme_bw(base_size = 14) +
      theme(legend.position = "none", plot.title = element_text(face = "bold"))

    # Only draw polygon if we have at least 2 points
    if (nrow(rv$polygon_points) >= 1) {
      if (nrow(rv$polygon_points) >= 2) {
        p <- p + geom_path(
          data = rv$polygon_points, aes(x = x, y = y),
          color = "red", linewidth = 0.5
        )
      }
      p <- p + geom_point(
        data = rv$polygon_points, aes(x = x, y = y),
        color = "red", size = 1, shape = 21, fill = "white", stroke = 0.5
      )
    }
    p
  })

  output$singlet_plot <- renderPlot({
    req(rv$current_gating_data_singlet)

    df <- data.frame(
      FSC_A = rv$current_gating_data_singlet$fsc_a,
      FSC_H = rv$current_gating_data_singlet$fsc_h
    )
    if (nrow(df) > 10000) df <- df[sample(nrow(df), 10000), ]

    p <- ggplot(df, aes(x = FSC_A, y = FSC_H)) +
      stat_density_2d(aes(fill = after_stat(level)), geom = "polygon", alpha = 0.6) +
      geom_point(alpha = 0.1, size = 0.3, color = "black") +
      geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray50", linewidth = 1) +
      scale_fill_viridis_c(option = "plasma") +
      scale_x_continuous(labels = custom_log_labels) +
      scale_y_continuous(labels = custom_log_labels) +
      labs(
        title = paste("Singlet Gate -", rv$current_gating_data_singlet$cell_line),
        subtitle = "Diagonal = singlets; above = doublets",
        x = "FSC-A", y = "FSC-H"
      ) +
      theme_bw(base_size = 14) +
      theme(legend.position = "none", plot.title = element_text(face = "bold"))

    # Only draw polygon if we have at least 2 points
    if (nrow(rv$polygon_points) >= 1) {
      if (nrow(rv$polygon_points) >= 2) {
        p <- p + geom_path(
          data = rv$polygon_points, aes(x = x, y = y),
          color = "red", linewidth = 0.5
        )
      }
      p <- p + geom_point(
        data = rv$polygon_points, aes(x = x, y = y),
        color = "red", size = 1, shape = 21, fill = "white", stroke = 0.5
      )
    }
    p
  })

  output$annexin_plot <- renderPlot({
    req(rv$current_gating_data)

    df <- data.frame(
      Annexin = rv$current_gating_data$annexin,
      PI = rv$current_gating_data$pi
    )
    if (nrow(df) > 10000) df <- df[sample(nrow(df), 10000), ]

    p <- ggplot(df, aes(x = Annexin, y = PI)) +
      stat_density_2d(aes(fill = after_stat(level)), geom = "polygon", alpha = 0.6) +
      geom_point(alpha = 0.1, size = 0.5, color = "black") +
      scale_fill_viridis_c(option = "magma") +
      labs(
        title = paste("Annexin V / PI Gate -", rv$current_gating_data$cell_line),
        subtitle = "Lower-left = viable",
        x = "Annexin V-FITC (transformed)", y = "PI (transformed)"
      ) +
      theme_bw(base_size = 14) +
      theme(legend.position = "none", plot.title = element_text(face = "bold"))

    p <- p +
      geom_vline(xintercept = input$annexin_threshold, color = "red", linewidth = 0.5) +
      geom_hline(yintercept = input$pi_threshold, color = "red", linewidth = 0.5)

    p
  })

  # === STATS OUTPUTS (STYLED) ===

  output$bead_stats <- renderText({
    req(rv$current_gating_data_fsc, input$bead_gate_channel)
    req(rv$gating_step == "bead_gate")
    
    fsc_vals <- rv$current_gating_data_fsc$fsc
    fl_vals  <- rv$current_gating_data_fsc$fl_trans
    
    if (is.null(fsc_vals) || is.null(fl_vals)) return("Loading data...")
    
    total_sampled <- length(fsc_vals)

    if (nrow(rv$polygon_points) >= 3) {
      in_gate <- point.in.polygon(
        fsc_vals, fl_vals,
        rv$polygon_points$x, rv$polygon_points$y
      ) > 0
      beads_sampled <- sum(in_gate)
    } else {
      beads_sampled <- 0
    }

    pct <- round(beads_sampled / total_sampled * 100, 2)
    paste0(
      "Beads identified: ", format(beads_sampled, big.mark = ","), " / ",
      format(total_sampled, big.mark = ","), " (", pct, "%)\n",
      "Note: Display shows 10,000 sampled events for speed. Final analysis uses all events."
    )
  })

  output$fsc_ssc_stats <- renderText({
    req(rv$current_gating_data_fsc)
    
    fsc_vals <- rv$current_gating_data_fsc$fsc
    ssc_vals <- rv$current_gating_data_fsc$ssc
    total_sampled <- length(fsc_vals)

    if (nrow(rv$polygon_points) >= 3) {
      in_gate <- point.in.polygon(
        fsc_vals, ssc_vals,
        rv$polygon_points$x, rv$polygon_points$y
      ) > 0
      kept_sampled <- sum(in_gate)
    } else {
      kept_sampled <- 0
    }

    pct <- round(kept_sampled / total_sampled * 100, 1)
    paste0(
      "Events kept: ", format(kept_sampled, big.mark = ","), " / ",
      format(total_sampled, big.mark = ","), " (", pct, "%)\n",
      "Note: Display shows 10,000 sampled events for speed. Final analysis uses all events."
    )
  })

  output$singlet_stats <- renderText({
    req(rv$current_gating_data_singlet)
    
    data_cells <- rv$current_gating_data_singlet$data_cells
    total_cells <- nrow(data_cells)
    
    sample_size <- 10000
    if (total_cells > sample_size) {
        idx <- sample(seq_len(total_cells), sample_size)
        fsc_a_sampled <- data_cells[idx, "FSC-A"]
        fsc_h_sampled <- data_cells[idx, "FSC-H"]
        total_view <- sample_size
    } else {
        fsc_a_sampled <- data_cells[, "FSC-A"]
        fsc_h_sampled <- data_cells[, "FSC-H"]
        total_view <- total_cells
    }

    if (nrow(rv$polygon_points) >= 3) {
      in_gate <- point.in.polygon(
        fsc_a_sampled, fsc_h_sampled,
        rv$polygon_points$x, rv$polygon_points$y
      ) > 0
      kept_sampled <- sum(in_gate)
    } else {
      kept_sampled <- 0
    }

    pct <- round(kept_sampled / total_view * 100, 1)
    paste0(
      "Singlets: ", format(kept_sampled, big.mark = ","), " / ",
      format(total_view, big.mark = ","), " (", pct, "%)\n",
      "Note: Display shows 10,000 sampled events for speed. Final analysis uses all events."
    )
  })

  output$annexin_stats_ui <- renderUI({
    req(rv$current_gating_data)
    annexin <- rv$current_gating_data$annexin
    pi <- rv$current_gating_data$pi
    total <- length(annexin)

    viable <- sum(annexin < input$annexin_threshold & pi < input$pi_threshold)
    viable_pct <- round(viable / total * 100, 1)
    
    status_color <- if (viable_pct >= 70 && viable_pct <= 95) "#28a745" else if (viable_pct > 95) "#ffc107" else "#dc3545"
    status_icon <- if (viable_pct >= 70 && viable_pct <= 95) icon("check-circle") else icon("exclamation-triangle")
    status_text <- if (viable_pct >= 70 && viable_pct <= 95) "Good" else if (viable_pct > 95) "Too high" else "Too low"

    div(
      class = "alert", 
      style = "margin-top: 10px; border: 1px solid #ddd; background: #f8f9fa;",
      h4(style = paste0("color: ", status_color, "; margin-top: 0;"),
         status_icon, " Viable cells: ", viable_pct, "% (", status_text, ")"
      ),
      p("Expected for control: 70-95%. Adjust thresholds if necessary.")
    )
  })

  # === CLICK HANDLERS (UNCHANGED) ===

  observeEvent(input$bead_click, {
    click <- input$bead_click
    rv$polygon_points <- rbind(rv$polygon_points, data.frame(x = click$x, y = click$y))
  })

  observeEvent(input$fsc_ssc_click, {
    click <- input$fsc_ssc_click
    rv$polygon_points <- rbind(rv$polygon_points, data.frame(x = click$x, y = click$y))
  })

  observeEvent(input$singlet_click, {
    click <- input$singlet_click
    rv$polygon_points <- rbind(rv$polygon_points, data.frame(x = click$x, y = click$y))
  })

  observeEvent(input$clear_polygon_bead, {
    rv$polygon_points <- data.frame(x = numeric(), y = numeric())
  })

  observeEvent(input$clear_polygon_fsc, {
    rv$polygon_points <- data.frame(x = numeric(), y = numeric())
  })

  observeEvent(input$clear_polygon_singlet, {
    rv$polygon_points <- data.frame(x = numeric(), y = numeric())
  })

  observeEvent(input$close_polygon_bead, {
    if (nrow(rv$polygon_points) >= 3) {
      rv$polygon_points <- rbind(rv$polygon_points, rv$polygon_points[1, ])
      showNotification("Polygon closed!", type = "message")
    }
  })

  observeEvent(input$close_polygon_fsc, {
    if (nrow(rv$polygon_points) >= 3) {
      rv$polygon_points <- rbind(rv$polygon_points, rv$polygon_points[1, ])
      showNotification("Polygon closed!", type = "message")
    }
  })

  observeEvent(input$close_polygon_singlet, {
    if (nrow(rv$polygon_points) >= 3) {
      rv$polygon_points <- rbind(rv$polygon_points, rv$polygon_points[1, ])
      showNotification("Polygon closed!", type = "message")
    }
  })

  # === NAVIGATION BETWEEN STEPS (UNCHANGED) ===

  observeEvent(input$next_to_fsc, {
    req(rv$current_gating_data_fsc)

    # Auto-close polygon if needed
    if (nrow(rv$polygon_points) >= 3) {
       first_pt <- rv$polygon_points[1, ]
       last_pt <- rv$polygon_points[nrow(rv$polygon_points), ]
       if (!identical(as.numeric(first_pt), as.numeric(last_pt))) {
           rv$polygon_points <- rbind(rv$polygon_points, first_pt)
       }
    }

    if (nrow(rv$polygon_points) < 3) {
      showNotification("Draw a polygon first (at least 3 points)", type = "error")
      return()
    }
    
    current_line <- unique(rv$current_gating_data_fsc$cell_line)
    rv$temp_bead_gates[[current_line]] <- list(type = "polygon", polygon = rv$polygon_points)
    
    # ---- SAVE BEAD PLOT IMMEDIATELY ----
    if (!is.null(input$bead_gate_channel)) {
      bead_chan <- input$bead_gate_channel
      data_full <- rv$current_gating_data_fsc$data_raw_original
      fl_full <- rv$current_gating_data_fsc$fl_trans_full
      
      if (!is.null(data_full) && !is.null(fl_full)) {
          bead_df_full <- data.frame(FSC = data_full[, "FSC-A"], FL = fl_full)
          bead_df <- bead_df_full[is.finite(bead_df_full$FSC) & is.finite(bead_df_full$FL), ]
          if (nrow(bead_df) > 5000) bead_df <- bead_df[sample(nrow(bead_df), 5000), ]
          
          poly_bead <- rv$polygon_points
          
          rv$bead_gate_plots[[current_line]] <- ggplot(bead_df, aes(x = FSC, y = FL)) +
            geom_point(alpha = 0.2, size = 0.5, color = "black") +
            geom_path(data = poly_bead, aes(x = x, y = y), color = "red", linewidth = 0.5) +
            geom_point(data = poly_bead, aes(x = x, y = y), color = "red", size = 1, shape = 21, fill = "white") +
            scale_x_continuous(labels = custom_log_labels) +
            scale_y_continuous(labels = custom_log_labels) +
            labs(title = paste("Bead Gate -", current_line), x = "FSC-A", y = paste("Fluorescence (", bead_chan, ")")) +
            theme_publication()
      }
    }

    # Accurate bead subtraction using FULL dataset
    data_full <- rv$current_gating_data_fsc$data_raw_original
    fl_full <- rv$current_gating_data_fsc$fl_trans_full
    in_bead_gate <- point.in.polygon(
        data_full[, "FSC-A"], fl_full,
        rv$polygon_points$x, rv$polygon_points$y
    ) > 0
    
    # Prepare non-bead data for next step
    # Note: We update rv$gating_step which will trigger the central observer to reload correctly
    rv$polygon_points <- data.frame(x = numeric(), y = numeric())
    rv$gating_step <- "fsc_ssc"
  })

  observeEvent(input$next_to_singlets, {
    req(rv$current_gating_data_fsc)

    # Auto-close polygon if needed
    if (nrow(rv$polygon_points) >= 3) {
       first_pt <- rv$polygon_points[1, ]
       last_pt <- rv$polygon_points[nrow(rv$polygon_points), ]
       if (!identical(as.numeric(first_pt), as.numeric(last_pt))) {
           rv$polygon_points <- rbind(rv$polygon_points, first_pt)
       }
    }

    if (nrow(rv$polygon_points) < 3) {
      showNotification("Draw a polygon first (at least 3 points)", type = "error")
      return()
    }
    rv$temp_fsc_ssc_gates <- list(type = "polygon", polygon = rv$polygon_points)
    
    # EXTRACT USING FULL DATASET
    in_gate <- point.in.polygon(
      rv$current_gating_data_fsc$fsc_full,
      rv$current_gating_data_fsc$ssc_full,
      rv$polygon_points$x, rv$polygon_points$y
    ) > 0
    rv$temp_gated_data_cells <- rv$current_gating_data_fsc$data_raw[in_gate, ]

    if (nrow(rv$temp_gated_data_cells) == 0) {
      showNotification("No cells in gate! Adjust gate.", type = "error")
      return()
    }

    rv$polygon_points <- data.frame(x = numeric(), y = numeric())
    rv$gating_step <- "singlets"
  })

  observeEvent(input$next_to_annexin, {
    req(rv$current_gating_data_singlet)

    # Auto-close polygon if needed
    if (nrow(rv$polygon_points) >= 3) {
       first_pt <- rv$polygon_points[1, ]
       last_pt <- rv$polygon_points[nrow(rv$polygon_points), ]
       if (!identical(as.numeric(first_pt), as.numeric(last_pt))) {
           rv$polygon_points <- rbind(rv$polygon_points, first_pt)
       }
    }

    if (nrow(rv$polygon_points) < 3) {
      showNotification("Draw a polygon first (at least 3 points)", type = "error")
      return()
    }
    rv$temp_singlet_gates <- list(type = "polygon", polygon = rv$polygon_points)
    in_gate <- point.in.polygon(
      rv$current_gating_data_singlet$fsc_a,
      rv$current_gating_data_singlet$fsc_h,
      rv$polygon_points$x, rv$polygon_points$y
    ) > 0
    rv$temp_gated_data_singlets <- rv$current_gating_data_singlet$data_cells[in_gate, ]

    if (nrow(rv$temp_gated_data_singlets) == 0) {
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

  # ============================================================================
  # ============================================================================
  # MODIFY observeEvent(input$save_gate) - around line 774
  # ============================================================================
  # ============================================================================

  observeEvent(input$save_gate, {
    req(rv$current_gating_data)
    cell_line <- rv$current_gating_data$cell_line
    
    # Store Bead Gate in thresholds if active
    bead_gate_to_store <- NULL
    if (input$is_absolute_counting) {
      bead_gate_to_store <- rv$temp_bead_gates[[cell_line]]
    }

    rv$thresholds[[cell_line]] <- list(
      bead_gate = bead_gate_to_store,
      fsc_ssc = rv$temp_fsc_ssc_gates,
      singlets = rv$temp_singlet_gates,
      annexin_pi = list(
        type    = "rectangle",
        annexin = input$annexin_threshold,
        pi      = input$pi_threshold
      ),
      logicle_transform = rv$current_gating_data_transform # NEW: Save the smart transform
    )
    
    # ---- NEW: Store Bead gate plot ----
    if (input$is_absolute_counting && !is.null(rv$temp_bead_gates[[cell_line]]) && !is.null(input$bead_gate_channel)) {
      bead_chan <- input$bead_gate_channel
      
      # Safety check for raw original data
      if (!is.null(rv$current_gating_data_fsc$data_raw_original)) {
          bead_raw_fsc <- rv$current_gating_data_fsc$data_raw_original[, "FSC-A"]
          bead_raw_fl  <- logicleTransform(w = 0.5, t = 1000000, m = 4.5, a = 0)(rv$current_gating_data_fsc$data_raw_original[, bead_chan])
          
          bead_df_full <- data.frame(FSC = bead_raw_fsc, FL = bead_raw_fl)
          bead_df <- bead_df_full[is.finite(bead_df_full$FSC) & is.finite(bead_df_full$FL), ]
            
          if (nrow(bead_df) > 5000) bead_df <- bead_df[sample(nrow(bead_df), 5000), ]
          
          poly_bead <- rv$temp_bead_gates[[cell_line]]$polygon
          
          rv$bead_gate_plots[[cell_line]] <- ggplot(bead_df, aes(x = FSC, y = FL)) +
            geom_point(alpha = 0.2, size = 0.5, color = "black") +
            geom_path(data = poly_bead, aes(x = x, y = y), color = "red", linewidth = 0.5) +
            geom_point(data = poly_bead, aes(x = x, y = y), color = "red", size = 1, shape = 21, fill = "white") +
            scale_x_continuous(labels = custom_log_labels) +
            scale_y_continuous(labels = custom_log_labels) +
            labs(title = paste("Bead Gate -", cell_line), x = "FSC-A", y = paste("Fluorescence (", bead_chan, ")")) +
            theme_publication()
      }
    }

    # ---- EXISTING: Store FSC/SSC gate plot ----
    if (!is.null(rv$temp_gated_data_cells) && !is.null(rv$temp_fsc_ssc_gates) && !is.null(rv$current_gating_data_fsc)) {
      
      fsc_full <- rv$current_gating_data_fsc$fsc_full
      ssc_full <- rv$current_gating_data_fsc$ssc_full
      
      if (!is.null(fsc_full) && !is.null(ssc_full)) {
          gate_df_full <- data.frame(FSC = fsc_full, SSC = ssc_full)
          gate_df <- gate_df_full[is.finite(gate_df_full$FSC) & is.finite(gate_df_full$SSC), ]
          
          if (nrow(gate_df) > 5000) gate_df <- gate_df[sample(nrow(gate_df), 5000), ]
          
          poly_df <- rv$temp_fsc_ssc_gates$polygon
    
          rv$gate_plots[[cell_line]] <- ggplot(gate_df, aes(x = FSC, y = SSC)) +
            stat_density_2d(aes(fill = after_stat(level)), geom = "polygon", alpha = 0.6) +
            geom_point(alpha = 0.1, size = 0.3, color = "black") +
            geom_path(data = poly_df, aes(x = x, y = y), color = "red", linewidth = 0.5) +
            geom_point(
              data = poly_df, aes(x = x, y = y),
              color = "red", size = 1, shape = 21, fill = "white", stroke = 0.5
            ) +
            scale_fill_viridis_c(option = "viridis") +
            scale_x_continuous(labels = custom_log_labels) +
            scale_y_continuous(labels = custom_log_labels) +
            labs(
              title = paste("FSC/SSC Gate -", cell_line),
              x = "FSC-A", y = "SSC-A"
            ) +
            theme_publication() +
            theme(legend.position = "none")
      }
    }

    # ---- NEW: Store Singlet gate plot ----
    if (!is.null(rv$temp_gated_data_singlets) && !is.null(rv$temp_singlet_gates) && !is.null(rv$current_gating_data_singlet)) {
      
      fsc_a <- rv$current_gating_data_singlet$fsc_a
      fsc_h <- rv$current_gating_data_singlet$fsc_h
      
      if (!is.null(fsc_a) && !is.null(fsc_h)) {
          singlet_df_full <- data.frame(FSC_A = fsc_a, FSC_H = fsc_h)
          singlet_df <- singlet_df_full[is.finite(singlet_df_full$FSC_A) & is.finite(singlet_df_full$FSC_H), ]
          
          if (nrow(singlet_df) > 5000) singlet_df <- singlet_df[sample(nrow(singlet_df), 5000), ]
          
          singlet_poly <- rv$temp_singlet_gates$polygon
    
          rv$singlet_gate_plots[[cell_line]] <- ggplot(singlet_df, aes(x = FSC_A, y = FSC_H)) +
            stat_density_2d(aes(fill = after_stat(level)), geom = "polygon", alpha = 0.6) +
            geom_point(alpha = 0.1, size = 0.3, color = "black") +
            geom_abline(
              slope = 1, intercept = 0, linetype = "dashed",
              color = "gray50", linewidth = 1
            ) +
            geom_path(
              data = singlet_poly, aes(x = x, y = y),
              color = "red", linewidth = 0.5
            ) +
            geom_point(
              data = singlet_poly, aes(x = x, y = y),
              color = "red", size = 1, shape = 21, fill = "white", stroke = 0.5
            ) +
            scale_fill_viridis_c(option = "plasma") +
            scale_x_continuous(labels = custom_log_labels) +
            scale_y_continuous(labels = custom_log_labels) +
            labs(
              title = paste("Singlet Gate -", cell_line),
              subtitle = "Diagonal = singlets; above = doublets",
              x = "FSC-A", y = "FSC-H"
            ) +
            theme_publication() +
            theme(legend.position = "none")
      }
    }

    # ---- NEW: Store Annexin/PI gate plot ----
    if (!is.null(rv$current_gating_data)) {
      
      annexin_raw <- rv$current_gating_data$annexin
      pi_raw <- rv$current_gating_data$pi
      
      if (!is.null(annexin_raw) && !is.null(pi_raw)) {
          annexin_df_full <- data.frame(Annexin = annexin_raw, PI = pi_raw)
          annexin_df <- annexin_df_full[is.finite(annexin_df_full$Annexin) & is.finite(annexin_df_full$PI), ]
          
          if (nrow(annexin_df) > 5000) annexin_df <- annexin_df[sample(nrow(annexin_df), 5000), ]
    
          ann_thresh <- input$annexin_threshold
          pi_thresh <- input$pi_threshold
    
          rv$annexin_gate_plots[[cell_line]] <- ggplot(annexin_df, aes(x = Annexin, y = PI)) +
            stat_density_2d(aes(fill = after_stat(level)), geom = "polygon", alpha = 0.6) +
            geom_point(alpha = 0.1, size = 0.5, color = "black") +
            geom_vline(xintercept = ann_thresh, color = "red", linewidth = 0.5) +
            geom_hline(yintercept = pi_thresh, color = "red", linewidth = 0.5) +
            annotate("text",
              x = -Inf, y = Inf, label = "Necrotic",
              hjust = -0.1, vjust = 1.5, color = "black", fontface = "bold", size = 3
            ) +
            annotate("text",
              x = Inf, y = Inf, label = "Late Apoptotic",
              hjust = 1.1, vjust = 1.5, color = "black", fontface = "bold", size = 3
            ) +
            annotate("text",
              x = Inf, y = -Inf, label = "Early Apoptotic",
              hjust = 1.1, vjust = -0.5, color = "black", fontface = "bold", size = 3
            ) +
            annotate("text",
              x = -Inf, y = -Inf, label = "Viable",
              hjust = -0.1, vjust = -0.5, color = "black", fontface = "bold", size = 3
            ) +
            scale_fill_viridis_c(option = "magma") +
            scale_x_continuous(labels = custom_log_labels) +
            scale_y_continuous(labels = custom_log_labels) +
            labs(
              title = paste("Apoptosis Gate -", cell_line),
              x = "Annexin V (Transformed)", y = "PI (Transformed)"
            ) +
            theme_publication() +
            theme(legend.position = "none")
      }
    }

    # ---- Store a combined plot for the review tab ----
    plots_to_arrange <- list()
    
    # 0. Use the bead plot saved during navigation
    if (!is.null(rv$bead_gate_plots[[cell_line]])) {
       plots_to_arrange <- c(plots_to_arrange, list(rv$bead_gate_plots[[cell_line]] + theme(plot.title = element_text(size = 10))))
    }
    
    # 1. Morphological Gates
    if (!is.null(rv$gate_plots[[cell_line]]))      plots_to_arrange <- c(plots_to_arrange, list(rv$gate_plots[[cell_line]] + theme(plot.title = element_text(size = 10))))
    if (!is.null(rv$singlet_gate_plots[[cell_line]])) plots_to_arrange <- c(plots_to_arrange, list(rv$singlet_gate_plots[[cell_line]] + theme(plot.title = element_text(size = 10))))
    
    # 2. Functional Gate (Annexin/PI)
    if (!is.null(rv$annexin_gate_plots[[cell_line]])) plots_to_arrange <- c(plots_to_arrange, list(rv$annexin_gate_plots[[cell_line]] + theme(plot.title = element_text(size = 10))))

    if (length(plots_to_arrange) > 0) {
      rv$gate_review_plots[[cell_line]] <- gridExtra::arrangeGrob(
        grobs = plots_to_arrange,
        ncol = 3
      )
    }

    showNotification(paste("Gates saved for", cell_line), type = "message")

    rv$current_cell_line_index <- rv$current_cell_line_index + 1
    # Check if we should go to bead_gate or fsc_ssc for the next line
    rv$gating_step <- if (input$is_absolute_counting) "bead_gate" else "fsc_ssc"
    rv$polygon_points <- data.frame(x = numeric(), y = numeric())
    rv$current_gating_data <- NULL
    rv$current_gating_data_fsc <- NULL
    rv$current_gating_data_singlet <- NULL
    rv$temp_fsc_ssc_gates <- NULL
    rv$temp_singlet_gates <- NULL
    rv$temp_gated_data_cells <- NULL
    rv$temp_gated_data_singlets <- NULL
  })
  # === ANALYSIS WITH ENHANCED METRICS ===
  observeEvent(input$analyze, {
    req(rv$metadata, rv$thresholds)

    if (length(rv$thresholds) == 0) {
      showNotification("No gates set", type = "error")
      return()
    }

    updateTabsetPanel(session, "tabs", selected = "Results")

    if (input$is_apoptosis_assay) {
      withProgress(message = "Analyzing samples...", value = 0, {
        trans <- logicleTransform(w = 0.5, t = 1000000, m = 4.5, a = 0)
        results <- data.frame()
        errors <- character()
        cell_counts_data <- data.frame() # NEW: Track cell counts

        for (i in 1:nrow(rv$metadata)) {
          row <- rv$metadata[i, ]
          cell_line <- row$cell_line

          if (is.na(cell_line) || !cell_line %in% names(rv$thresholds)) {
            incProgress(1 / nrow(rv$metadata))
            next
          }

          thresholds <- rv$thresholds[[cell_line]]

          tryCatch(
            {
              fs <- read.FCS(row$datapath)
              data_raw <- exprs(fs)
              total_events <- nrow(data_raw)
              
              # --- NEW: Step 0: Bead Gating (if enabled) ---
              bead_count <- NA
              if (input$is_absolute_counting && !is.null(input$bead_gate_channel)) {
                bead_gate <- rv$temp_bead_gates[[cell_line]]
                bead_chan <- input$bead_gate_channel
                if (!is.null(bead_gate)) {
                  trans_bead <- logicleTransform(w = 0.5, t = 1000000, m = 4.5, a = 0)
                  in_bead_gate <- point.in.polygon(
                    data_raw[, "FSC-A"],
                    trans_bead(data_raw[, bead_chan]),
                    bead_gate$polygon$x, bead_gate$polygon$y
                  ) > 0
                  bead_count <- sum(in_bead_gate)
                  # Exclude beads from further cell analysis
                  data_raw <- data_raw[!in_bead_gate, ]
                }
              }

              # Gate 1: FSC/SSC
              fsc_ssc_gate <- thresholds$fsc_ssc
              poly <- fsc_ssc_gate$polygon
              in_gate1 <- point.in.polygon(
                data_raw[, "FSC-A"], data_raw[, "SSC-A"],
                poly$x, poly$y
              ) > 0
              data_cells <- data_raw[in_gate1, ]
              cells_after_fsc <- nrow(data_cells)
              if (nrow(data_cells) == 0) next

              # Gate 2: Singlets
              singlet_gate <- thresholds$singlets
              poly <- singlet_gate$polygon
              in_gate2 <- point.in.polygon(
                data_cells[, "FSC-A"], data_cells[, "FSC-H"],
                poly$x, poly$y
              ) > 0
              data_singlets <- data_cells[in_gate2, ]
              singlets_count <- nrow(data_singlets)
              if (nrow(data_singlets) == 0) next
              
              # Calculation Factor for Absolute Counting
              abs_factor <- NA
              if (input$is_absolute_counting && !is.na(bead_count) && bead_count > 0) {
                 # Formula: (Cell Count / Bead Count) * (Bead Vol / Sample Vol) * Bead Conc
                 abs_factor <- (input$bead_vol / input$sample_vol) * input$bead_conc / bead_count
              }

              # Gate 3: Annexin/PI with compensation - NOW PER CELL LINE
              compensated_fs <- NULL
              
              # Determine correct SPILLOVER matrix
              comp_mat_use <- NULL
              if (rv$comp_calculated) {
                if (!is.null(rv$spill_matrices[[cell_line]])) {
                  comp_mat_use <- rv$spill_matrices[[cell_line]]
                } else {
                  comp_mat_use <- rv$spill_matrix
                }
              }

              if (!is.null(comp_mat_use)) {
                # Create flowFrame directly - flowCore handles parameters automatically
                ff_singlets <- flowFrame(exprs = data_singlets)

                # Use the specific matrix for compensate()
                compensated_fs <- compensate(ff_singlets, comp_mat_use)
                
                # REUSE the stored transform from gating for this cell line
                trans_obj <- thresholds$logicle_transform
                
                if (!is.null(trans_obj) && inherits(trans_obj, "transformList")) {
                  transformed_fs <- transform(compensated_fs, trans_obj)
                  annexin <- exprs(transformed_fs)[, "BL1-A"]
                  pi <- exprs(transformed_fs)[, "BL2-A"]
                } else {
                  # Fallback to default if no stored transform
                  annexin <- trans(exprs(compensated_fs)[, "BL1-A"])
                  pi <- trans(exprs(compensated_fs)[, "BL2-A"])
                }
              } else {
                # Uncompensated fallback (use default logicle)
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
              
              # Calculate Absolute Concentrations
              abs_viable <- if (!is.na(abs_factor)) viable * abs_factor else NA
              abs_total_cells <- if (!is.na(abs_factor)) total * abs_factor else NA
              abs_death <- if (!is.na(abs_factor)) cell_death * abs_factor else NA

              results <- rbind(results, data.frame(
                cell_line = cell_line,
                concentration_uM = row$concentration_uM,
                replicate = row$replicate,
                pct_viable = (viable / total) * 100,
                pct_early_apoptotic = (early_apoptotic / total) * 100,
                pct_late_apoptotic = (late_apoptotic / total) * 100,
                pct_necrotic = (necrotic / total) * 100,
                pct_cell_death = (cell_death / total) * 100,
                abs_conc_viable = abs_viable,
                abs_conc_total = abs_total_cells,
                abs_conc_death = abs_death,
                bead_count = bead_count,
                stringsAsFactors = FALSE
              ))

              # NEW: Store cell counts at each gating step
              cell_counts_data <- rbind(cell_counts_data, data.frame(
                cell_line = cell_line,
                concentration_uM = row$concentration_uM,
                replicate = row$replicate,
                total_events = total_events,
                bead_count = bead_count,
                after_fsc_ssc = cells_after_fsc,
                after_singlets = singlets_count,
                viable_count = viable,
                dead_count = cell_death,
                abs_conc_viable = abs_viable,
                abs_conc_total = abs_total_cells, # Fixed: Added missing column
                stringsAsFactors = FALSE
              ))
            },
            error = function(e) {
              errors <<- c(errors, paste(row$name, ":", e$message))
            }
          )

          incProgress(1 / nrow(rv$metadata))
        }

        if (nrow(results) == 0) {
          msg <- "Analysis failed - no valid results"
          if (length(errors) > 0) msg <- paste0(msg, "\n\nErrors:\n", paste(errors, collapse = "\n"))
          showNotification(msg, type = "error", duration = NULL)
          return()
        }

        rv$results <- results
        rv$cell_counts <- cell_counts_data # NEW
        
        showNotification("Gating complete. Proceeding to statistical analysis...", type = "message")
        
        # (Deprecated logic removed)
        NULL
      })
    } else {
      # WORKFLOW 2: Generic Analysis
      generic_output <- run_generic_analysis(
        rv$metadata,
        rv$thresholds,
        rv$control_concentration,
        rv$spill_matrices, 
        rv$spill_matrix, 
        rv$comp_calculated,
        is_absolute_counting = input$is_absolute_counting,
        bead_conc = input$bead_conc,
        bead_vol = input$bead_vol,
        sample_vol = input$sample_vol,
        temp_bead_gates = rv$temp_bead_gates,
        bead_gate_channel = input$bead_gate_channel
      )
      showNotification("Analysis complete!", type = "message")

      if (!is.null(generic_output)) {
        rv$generic_results <- generic_output$results
        rv$generic_ec50_results <- generic_output$ec50_results
        rv$generic_plot_data_dr <- generic_output$plot_data_dr
        rv$generic_predicted_curves_dr <- generic_output$predicted_curves_dr
        rv$generic_raw_intensity_data <- generic_output$raw_intensity_data
        rv$cell_counts <- generic_output$cell_counts # NEW
        showNotification("Generic analysis complete!", type = "message")
      } else {
        showNotification("Generic analysis failed to return results.", type = "error")
      }
    }
  })
  # NEW: QC UI Output
  output$qc_ui <- renderUI({
    req(rv$qc_data)

    tagList(
      h5("Coefficient of Variation (CV%) by Condition"),
      p("CV% < 15%: Good | 15-25%: Acceptable | >25%: High variability"),
      DT::dataTableOutput("qc_table"),
      hr(),
      h5("Outlier Detection (Grubbs Test, α=0.05)"),
      if (!is.null(rv$outlier_flags) && nrow(rv$outlier_flags) > 0) {
        tagList(
          p(
            style = "color: #d32f2f;", icon("exclamation-triangle"),
            " ", nrow(rv$outlier_flags), " potential outlier(s) detected:"
          ),
          DT::dataTableOutput("outlier_table"),
          p(
            style = "font-size: 0.9em; color: #666;",
            "Note: Outliers are not automatically removed. Review and decide if exclusion is warranted."
          )
        )
      } else {
        div(
          class = "alert alert-success",
          icon("check-circle"), " No significant outliers detected"
        )
      }
    )
  })

  # === QC TABLE & EXCLUSION LOGIC ===
  
  # Helper to generate unique IDs
  get_row_id <- function(df) {
    paste(df$cell_line, df$concentration_uM, df$replicate, sep = "_")
  }

  output$qc_table <- DT::renderDataTable({
    req(rv$qc_data)
    
    # Create display data
    display_df <- rv$qc_data %>%
      mutate(
        Row_ID = get_row_id(.),
        Status_Flag = ifelse(Row_ID %in% rv$manual_exclusions, "Excluded", "Included"),
        QC_Status = case_when(
          Row_ID %in% rv$manual_exclusions ~ "⛔ Excluded",
          cv_pct < 15 ~ "✅ Good",
          cv_pct < 25 ~ "⚠️ Acceptable",
          TRUE ~ "❌ High Variability"
        )
      )
    
    # We want to show specific columns
    
    dt_out <- display_df %>%
      dplyr::select(
        `Cell Line` = cell_line,
        `Conc (µM)` = concentration_uM,
        `Rep` = replicate,
        `Viability (%)` = pct_viable,
        `Mean (%)` = mean_viable,
        `CV (%)` = cv_pct,
        `n` = n,
        `Status` = QC_Status
      )

    DT::datatable(
      dt_out,
      selection = "multiple",
      rownames = FALSE,
      class = "compact hover cell-border stripe",
      options = list(
        dom = "t", 
        paging = FALSE,
        columnDefs = list(list(className = 'dt-center', targets = "_all")),
        rowCallback = DT::JS(
          "function(row, data) {",
          "  // Excluded row styling",
          "  if (data[7] && data[7].includes('Excluded')) {",
          "    $('td', row).css('background-color', '#f8d7da');",
          "    $('td', row).css('color', '#721c24');",
          "    $('td', row).css('text-decoration', 'line-through');",
          "  }",
          "  // Status color coding (Column index 7)",
          "  var status = data[7];",
          "  var color = '';",
          "  if (status.includes('Good')) color = '#d4edda';",
          "  else if (status.includes('Acceptable')) color = '#fff3cd';",
          "  else if (status.includes('High')) color = '#f8d7da';",
          "  if (color && !status.includes('Excluded')) {",
          "    $('td:eq(7)', row).css('background-color', color);",
          "    $('td:eq(7)', row).css('font-weight', 'bold');",
          "  }",
          "}"
        )
      ), 
      caption = "Select replicates below and click 'Toggle Status' to exclude/include them from analysis."
    ) %>%
    DT::formatRound(columns = c(4, 5, 6), digits = 2)
  })

  # Toggle Exclusion Observer
  observeEvent(input$toggle_exclusion, {
    req(rv$qc_data)
    selected_indices <- input$qc_table_rows_selected
    if (is.null(selected_indices) || length(selected_indices) == 0) return()
    
    # Reconstruct IDs for selected rows
    # Note: DT indices are 1-based, matching the dataframe order if not sorted/filtered differently
    # Since we disable sorting/paging in options, order should match rv$qc_data
    
    current_ids <- get_row_id(rv$qc_data)
    selected_ids <- current_ids[selected_indices]
    
    # Toggle logic
    current_exclusions <- rv$manual_exclusions
    
    for (id in selected_ids) {
      if (id %in% current_exclusions) {
        # If already excluded, remove from list (Include)
        current_exclusions <- setdiff(current_exclusions, id)
      } else {
        # If included, add to list (Exclude)
        current_exclusions <- c(current_exclusions, id)
      }
    }
    
    rv$manual_exclusions <- unique(current_exclusions)
    showNotification(paste("Updated exclusion status for", length(selected_ids), "replicates."), type = "message")
  })

  output$outlier_table <- DT::renderDataTable({
    req(rv$outlier_flags)
    rv$outlier_flags %>%
      dplyr::select(
        `Cell Line` = cell_line,
        `Conc (µM)` = concentration_uM,
        `Rep` = replicate,
        `p-value` = p_value
      ) %>%
      DT::datatable(
        rownames = FALSE,
        class = "compact hover cell-border",
        options = list(
          dom = "t", 
          paging = FALSE,
          columnDefs = list(list(className = 'dt-center', targets = "_all"))
        ), 
        caption = "Outlier Detection (Grubbs Test α=0.05)"
      ) %>%
      DT::formatSignif(columns = 4, digits = 3)
  })

  # NEW: Statistical comparison output
  output$stats_comparison_table <- DT::renderDataTable({
    req(rv$ic50_results)

    if (nrow(rv$ic50_results) < 2) {
      return(DT::datatable(data.frame(Status = "Statistical comparison requires at least 2 cell lines.")))
    }

    anova_result <- tryCatch(
      {
        # Scientific Standard: Use Log10-transformed IC50 for statistical testing
        # This handles the log-normal distribution of potency values
        data_for_stats <- rv$ic50_results
        data_for_stats$LogIC50 <- log10(data_for_stats$IC50_uM)
        
        aov(LogIC50 ~ cell_line, data = data_for_stats)
      },
      error = function(e) NULL
    )

    if (!is.null(anova_result)) {
      anova_summary <- summary(anova_result)
      anova_df <- as.data.frame(anova_summary[[1]])
      anova_df <- tibble::rownames_to_column(anova_df, "Source")
      
      # Rename 'cell_line' to 'Between Groups' for clarity
      anova_df$Source[anova_df$Source == "cell_line"] <- "Between Groups"
      
      # Render styled table
      dt_anova <- DT::datatable(anova_df, 
                    rownames = FALSE,
                    class = "compact hover cell-border",
                    options = list(dom = "t", paging = FALSE), 
                    caption = "ANOVA Results for Log10(IC50) Values")
      
      # Conditionally format columns if they exist
      cols_to_round <- intersect(names(anova_df), c("Sum Sq", "Mean Sq", "F value"))
      if (length(cols_to_round) > 0) {
        dt_anova <- dt_anova %>% DT::formatRound(columns = cols_to_round, digits = 3)
      }
      
      if ("Pr(>F)" %in% names(anova_df)) {
        dt_anova <- dt_anova %>% DT::formatSignif(columns = "Pr(>F)", digits = 3)
      }
      
      dt_anova
    } else {
      DT::datatable(data.frame(Status = "ANOVA could not be performed."))
    }
  })

  output$anova_interpretation <- renderUI({
    req(rv$ic50_results)
    if (nrow(rv$ic50_results) < 2) return(NULL)
    
    # Run a quick internal ANOVA to get the p-value
    data_for_stats <- rv$ic50_results
    data_for_stats$LogIC50 <- log10(data_for_stats$IC50_uM)
    anova_res <- tryCatch(aov(LogIC50 ~ cell_line, data = data_for_stats), error = function(e) NULL)
    
    if (is.null(anova_res)) return(p("Interpretation unavailable."))
    
    p_val <- summary(anova_res)[[1]][["Pr(>F)"]][1]
    
    # Check if p_val is valid (not length 0 and not NA)
    if (length(p_val) == 0 || is.na(p_val)) {
      return(div(class = "alert alert-info", style = "margin-top: 10px;",
        strong(icon("info-circle"), " Note on Statistical Power:"),
        p("ANOVA requires at least 2 observations per cell line to estimate within-group variance. If you only have one IC50 value per cell line, the p-value cannot be computed. However, you can still observe trends in the dose-response curves and the Tukey comparison of point-estimates.")
      ))
    }
    
    if (p_val < 0.05) {
      return(div(class = "alert alert-success", style = "margin-top: 10px;",
        strong(icon("check-circle"), sprintf(" Result: Statistically Significant (p = %s)", format.pval(p_val, digits = 3))),
        p("The p-value is less than 0.05. We reject the null hypothesis. There are significant differences in sensitivity (LogIC50) between the tested cell lines. Proceed to Tukey HSD to identify specific differences.")
      ))
    } else {
      return(div(class = "alert alert-warning", style = "margin-top: 10px;",
        strong(icon("exclamation-triangle"), sprintf(" Result: Not Significant (p = %s)", format.pval(p_val, digits = 3))),
        p("The p-value is greater than 0.05. We fail to reject the null hypothesis. There is no evidence of significant differences in sensitivity (LogIC50) between the cell lines at this alpha level.")
      ))
    }
  })


  # NEW: Advanced metrics table
  output$advanced_metrics_table <- DT::renderDataTable({
    req(rv$advanced_metrics)
    
    dt_display <- rv$advanced_metrics %>%
      dplyr::select(
        `Cell Line` = cell_line,
        `Metric` = metric_type,
        `Hill Slope` = Hill_slope,
        `AUC` = AUC,
        `R²` = R_squared,
        everything()
      )
    
    # Rename EC columns
    colnames(dt_display) <- gsub("_uM", " (µM)", colnames(dt_display))
    colnames(dt_display) <- gsub("_plateau", " Plateau", colnames(dt_display))

    num_cols <- which(sapply(dt_display, is.numeric))

    DT::datatable(
      dt_display, 
      rownames = FALSE,
      class = "compact hover cell-border stripe",
      options = list(
        dom = "t", 
        paging = FALSE,
        scrollX = TRUE,
        columnDefs = list(list(className = 'dt-center', targets = "_all"))
      ), 
      caption = "Extended Pharmacodynamic Parameters (Advanced Metrics)"
    ) %>%
    DT::formatRound(columns = num_cols, digits = 3)
  })

  # === SMART X-AXIS BREAKS ===

  # === OUTPUT TABLES (UNCHANGED STRUCTURE) ===
  
  output$control_quality_alert_ui <- renderUI({
    req(rv$control_quality_alert)
    
    lines <- rv$control_quality_alert
    
    div(
      class = "alert alert-danger",
      style = "border-left: 5px solid #d32f2f; margin-bottom: 20px;",
      h5(icon("exclamation-triangle"), " Experimental Failure Warning: Low Control Viability"),
      p("The following cell lines show < 70% viability in the untreated control (0 µM). This indicates a potential experimental failure or poor cell health."),
      tags$ul(
        lapply(1:nrow(lines), function(i) {
          tags$li(sprintf("%s: %.1f%% Viability", lines$cell_line[i], lines$control_viable[i]))
        })
      ),
      p(strong("Recommendation:"), " Do NOT enable normalization, as it will mask this failure by resetting these low values to 100%. Check raw data plots.")
    )
  })

  output$ic50_table <- renderTable(
    {
      req(rv$ic50_results, rv$ld50_results)

      # Merge basic IC50 and LD50
      combined <- merge(rv$ic50_results, rv$ld50_results, by = "cell_line", all = TRUE)
      
      # Merge Absolute Survival if available
      if (input$is_absolute_counting && !is.null(rv$absolute_survival_results) && nrow(rv$absolute_survival_results) > 0) {
         # Rename columns before merge to avoid conflict
         abs_res <- rv$absolute_survival_results %>%
           dplyr::rename(Abs_IC50_uM = IC50_uM, Abs_IC50_lower = IC50_lower, Abs_IC50_upper = IC50_upper)
         combined <- merge(combined, abs_res, by = "cell_line", all = TRUE)
      } else {
         # Add empty cols
         combined$Abs_IC50_uM <- NA
         combined$Abs_IC50_lower <- NA
         combined$Abs_IC50_upper <- NA
      }
      
      if (input$is_absolute_counting && !is.null(rv$absolute_ld50_results) && nrow(rv$absolute_ld50_results) > 0) {
        abs_ld_res <- rv$absolute_ld50_results %>%
          dplyr::rename(Abs_LD50_uM = LD50_uM, Abs_LD50_lower = LD50_lower, Abs_LD50_upper = LD50_upper)
        combined <- merge(combined, abs_ld_res, by = "cell_line", all = TRUE)
      } else {
         combined$Abs_LD50_uM <- NA
         combined$Abs_LD50_lower <- NA
         combined$Abs_LD50_upper <- NA
      }

            # Helper for HTML scientific notation

            pretty_sci_html <- function(x) {

              if (is.na(x) || is.nan(x)) return("Fit Failed")

              if (x < 1e-9) return("Not Reached")

              

              # Logic: if < 0.01 or > 1000, use sci notation

              if (abs(x) < 0.01 || abs(x) > 1000) {

                 exponent <- floor(log10(abs(x)))

                 base <- x / (10^exponent)

                 return(sprintf("%.2f &times; 10<sup>%d</sup>", base, exponent))

              }

              return(sprintf("%.4f", x))

            }

      

            # Helper for formatting with CI

            fmt_ci <- function(val, low, upp) {

              val_str <- pretty_sci_html(val)

              if (val_str %in% c("Fit Failed", "Not Reached")) return(val_str)

              

              low_str <- pretty_sci_html(low)

              upp_str <- pretty_sci_html(upp)

              

              paste0(val_str, " [", low_str, " - ", upp_str, "]")

            }

      

                  # Construct column list safely

                  cols_to_show <- c("Cell Line", "IC50 (Viability) [95% CI]", "LD50 (Death) [95% CI]")

                  if (input$is_absolute_counting) {

                     cols_to_show <- c(cols_to_show, "Abs. IC50 (Survival) [95% CI]", "Abs. LD50 (Absolute Death) [95% CI]")

                  }

      

                  combined_display <- combined %>% 

                    rowwise() %>% 

                    mutate(

                      `IC50 (Viability) [95% CI]` = fmt_ci(IC50_uM, IC50_lower, IC50_upper),

                      `LD50 (Death) [95% CI]` = fmt_ci(LD50_uM, LD50_lower, LD50_upper),

                      `Abs. IC50 (Survival) [95% CI]` = if(input$is_absolute_counting) fmt_ci(Abs_IC50_uM, Abs_IC50_lower, Abs_IC50_upper) else "N/A",

                      `Abs. LD50 (Absolute Death) [95% CI]` = if(input$is_absolute_counting) fmt_ci(Abs_LD50_uM, Abs_LD50_lower, Abs_LD50_upper) else "N/A"

                    ) %>% 

                    dplyr::rename(`Cell Line` = cell_line) %>% 

                    dplyr::select(all_of(cols_to_show))

      

                  combined_display

                },

          striped = TRUE,

          hover = TRUE,

          bordered = TRUE,

          align = 'c',

          sanitize.text.function = function(x) x # Allow HTML

        )
  
  output$abs_survival_table <- renderTable({
    NULL
  })

  # --- NEW: Generic Workflow Outputs ---
  output$generic_ec50_table <- renderTable(
    {
      req(rv$generic_ec50_results)
      df <- rv$generic_ec50_results
      
      # Clean Target Population names
      df$Target_Population <- gsub("pct_", "", df$Target_Population)
      df$Target_Population <- gsub("_", " / ", df$Target_Population)
      
      # Format EC50 using the same pretty scientific logic as Apoptosis table
      pretty_sci_html <- function(x) {
        if (is.na(x) || is.nan(x)) return("Fit Failed")
        if (x < 1e-9 && x > 0) return("Not Reached")
        if (x == 0) return("0   ")
        
        if (abs(x) < 0.01 || abs(x) > 1000) {
           exponent <- floor(log10(abs(x)))
           base <- x / (10^exponent)
           return(sprintf("%.2f &times; 10<sup>%d</sup>", base, exponent))
        }
        return(sprintf("%.4f", x))
      }

      df$EC50_uM <- sapply(df$EC50_uM, pretty_sci_html)
      
      # Rename Columns
      colnames(df) <- c("Cell Line", "EC50 (µM)", "Target Population")
      df
    },
    striped = TRUE,
    hover = TRUE,
    bordered = TRUE,
    align = 'c',
    sanitize.text.function = function(x) x # Allow HTML superscripts
  )

  output$generic_results_table <- DT::renderDataTable({
    req(rv$generic_results)
    
    # Create display df with IDs
    display_df <- rv$generic_results %>%
      mutate(
        Row_ID = paste(cell_line, concentration_uM, replicate, sep = "_"),
        QC_Status = ifelse(Row_ID %in% rv$generic_manual_exclusions, "⛔ Excluded", "✅ Included")
      )
      
    # Identify population columns dynamically
    pop_cols_pct <- names(display_df)[startsWith(names(display_df), "pct_")]
    pop_cols_abs <- names(display_df)[startsWith(names(display_df), "abs_")]
    
    # We want to keep Row_ID for logic but hide it from display if possible, 
    # or just select display columns
    dt_display <- display_df %>%
      dplyr::select(
        `Cell Line` = cell_line,
        `Conc (µM)` = concentration_uM,
        `Rep` = replicate,
        everything(),
        -Row_ID
      )
    
    # Rename pct_ columns to be cleaner
    colnames(dt_display) <- gsub("pct_", "", colnames(dt_display))
    # Rename abs_ columns
    colnames(dt_display) <- gsub("abs_", "", colnames(dt_display))
    
    # Improve Population Names (replace underscores with spaces or slashes)
    # E.g. BL1-A+_BL2-A+ -> BL1-A+ / BL2-A+
    clean_pop_name <- function(x) {
       x <- gsub("_", " / ", x)
       return(x)
    }
    
    # Add suffixes and clean names
    for(col in pop_cols_pct) {
       clean_name <- gsub("pct_", "", col)
       clean_name_display <- clean_pop_name(clean_name)
       idx <- which(colnames(dt_display) == clean_name)[1]
       colnames(dt_display)[idx] <- paste0(clean_name_display, " (%)")
    }
    for(col in pop_cols_abs) {
       clean_name <- gsub("abs_", "", col)
       # The column name in dt_display is just the clean_name now because of the gsub above
       # But wait, I ran gsub on ALL columns above. 
       # So 'abs_BL1_BL2' became 'BL1_BL2'. 
       # And 'pct_BL1_BL2' became 'BL1_BL2'.
       # This causes duplicate names! 
       
       # Better approach: Don't do the global gsub first.
    }
    
    # --- Corrected Renaming Logic ---
    dt_display <- display_df %>%
      dplyr::select(
        `Cell Line` = cell_line,
        `Conc (µM)` = concentration_uM,
        `Rep` = replicate,
        everything(),
        -Row_ID
      )
      
    # Iterate and rename specific columns
    curr_names <- colnames(dt_display)
    new_names <- curr_names
    
    for(i in seq_along(curr_names)) {
       col <- curr_names[i]
       if(startsWith(col, "pct_")) {
          base <- gsub("pct_", "", col)
          new_names[i] <- paste0(clean_pop_name(base), " (%)")
       } else if(startsWith(col, "abs_")) {
          base <- gsub("abs_", "", col)
          new_names[i] <- paste0(clean_pop_name(base), " (Cells/µL)")
       }
    }
    colnames(dt_display) <- new_names
    
    colnames(dt_display)[ncol(dt_display)] <- "Status"

    # Find numeric indices for formatting
    num_cols <- which(sapply(dt_display, is.numeric))

    DT::datatable(
      dt_display,
      selection = "multiple",
      rownames = FALSE,
      class = "compact hover cell-border stripe",
      options = list(
        pageLength = 25, 
        dom = "tip",
        columnDefs = list(list(className = 'dt-center', targets = "_all")),
        initComplete = DT::JS(
          "function(settings, json) {",
          "$(this.api().table().header()).css({'font-size': '80%'});",
          "$(this.api().table().body()).css({'font-size': '80%'});",
          "}"
        ),
        rowCallback = DT::JS(
          "function(row, data) {",
          "  // Check the last column for status",
          "  var status = data[data.length - 1];",
          "  if (status && status.includes('Excluded')) {",
          "    $('td', row).css('background-color', '#f8d7da');",
          "    $('td', row).css('color', '#721c24');",
          "    $('td', row).css('text-decoration', 'line-through');",
          "  } else {",
          "    // Color included green",
          "    $('td:last', row).css('background-color', '#d4edda');",
          "    $('td:last', row).css('font-weight', 'bold');",
          "  }",
          "}"
        )
      ),
      caption = "Percent Positive Results (Interactive Exclusion)"
    ) %>%
    DT::formatRound(columns = num_cols, digits = 2)
  })
  
  observeEvent(input$toggle_generic_exclusion, {
    req(rv$generic_results)
    selected_indices <- input$generic_results_table_rows_selected
    if (is.null(selected_indices) || length(selected_indices) == 0) return()
    
    current_ids <- paste(rv$generic_results$cell_line, rv$generic_results$concentration_uM, rv$generic_results$replicate, sep = "_")
    selected_ids <- current_ids[selected_indices]
    
    current_exclusions <- rv$generic_manual_exclusions
    
    for (id in selected_ids) {
      if (id %in% current_exclusions) {
        current_exclusions <- setdiff(current_exclusions, id)
      } else {
        current_exclusions <- c(current_exclusions, id)
      }
    }
    
    rv$generic_manual_exclusions <- unique(current_exclusions)
    showNotification(paste("Updated generic exclusion status for", length(selected_ids), "replicates."), type = "message")
  })


  # --- NEW GENERIC PLOTS ---


  # 1. Population Breakdown Plot (Stacked Bar)
  output$generic_pop_breakdown_plot <- renderPlot({
    req(rv$generic_results)


    # Identify population columns dynamically
    pop_cols <- names(rv$generic_results)[startsWith(names(rv$generic_results), "pct_")]


    # Reshape data to long format for stacked bar plot
    plot_data <- rv$generic_results %>%
      pivot_longer(
        cols = all_of(pop_cols),
        names_to = "Population",
        values_to = "Percentage"
      ) %>%
      mutate(
        Sample_ID = paste(cell_line, concentration_uM, replicate, sep = "_"),
        Population = gsub("pct_", "", Population) # Clean name for legend
      )

    # Correct Sorting for X-Axis (Sample_ID)
    unique_samples <- plot_data %>%
      dplyr::select(Sample_ID, cell_line, concentration_uM, replicate) %>%
      distinct() %>%
      arrange(cell_line, concentration_uM, replicate)

    plot_data$Sample_ID <- factor(plot_data$Sample_ID, levels = unique_samples$Sample_ID)


    # Assign color-blind friendly palette
    pop_colors <- get_color_palette(length(unique(plot_data$Population)))
    names(pop_colors) <- unique(plot_data$Population)


    p <- ggplot(plot_data, aes(x = Sample_ID, y = Percentage, fill = Population)) +
      geom_bar(stat = "identity", position = "stack", color = "black", linewidth = 0.3) +
      scale_fill_manual(values = pop_colors) +
      labs(
        title = "Population Breakdown Across Samples",
        x = "Sample (Cell Line_Concentration_Replicate)",
        y = "Percentage (%)",
        fill = "Population"
      ) +
      theme_publication() +
      theme(plot.title = element_text(size = 14, face = "bold"))

    rv$generic_pop_breakdown_plot_obj <- p # Store the plot object
    p # Return the plot
  })


  # Observer to populate channel choices for Fluorescence Intensity plot
  observe({
    req(rv$generic_raw_intensity_data)


    # Extract all unique channel names from the raw intensity data
    # rv$generic_raw_intensity_data is a list of data.frames
    all_channels <- unique(do.call(rbind, rv$generic_raw_intensity_data)$Channel)


    updateSelectInput(session, "generic_intensity_channel_select",
      choices = all_channels,
      selected = all_channels[1]
    )
  })


  # 2. Fluorescence Intensity Plot (Violin/Box Plot)
  output$generic_intensity_plot <- renderPlot({
    req(input$generic_intensity_channel_select, rv$generic_raw_intensity_data)


    selected_channel <- input$generic_intensity_channel_select


    # Combine all raw intensity data into a single data frame
    plot_data <- do.call(rbind, rv$generic_raw_intensity_data) %>%
      filter(Channel == selected_channel) %>%
      mutate(Sample_ID = paste(CellLine, Concentration, Replicate, sep = "_"))

    req(nrow(plot_data) > 0)

    # Correct Sorting for X-Axis (Sample_ID)
    unique_samples <- plot_data %>%
      dplyr::select(Sample_ID, CellLine, Concentration, Replicate) %>%
      distinct() %>%
      arrange(CellLine, Concentration, Replicate)

    plot_data$Sample_ID <- factor(plot_data$Sample_ID, levels = unique_samples$Sample_ID)


    # Use global Okabe-Ito colors or a derived palette
    cell_line_colors <- get_color_palette(length(unique(plot_data$CellLine)))
    names(cell_line_colors) <- unique(plot_data$CellLine)


    p <- ggplot(plot_data, aes(x = Sample_ID, y = Intensity, fill = CellLine)) +
      geom_violin(trim = FALSE, alpha = 0.7) +
      geom_boxplot(width = 0.1, fill = "white", color = "black", outlier.shape = NA) +
      scale_fill_manual(values = cell_line_colors) +
      scale_y_continuous(labels = custom_log_labels) +
      labs(
        title = paste("Fluorescence Intensity for", selected_channel),
        x = "Sample (Cell Line_Concentration_Replicate)",
        y = paste(selected_channel, "Intensity (Transformed)"),
        fill = "Cell Line"
      ) +
      theme_publication() +
      theme(
        plot.title = element_text(size = 14, face = "bold"),
        legend.position = "right"
      )

    rv$generic_intensity_plot_obj <- p # Store the plot object
    p # Return the plot
  })


  # Observer to populate population choices for Dose-Response plot
  observe({
    req(rv$generic_results)


    # Get all population columns (those starting with "pct_")
    population_cols <- names(rv$generic_results)[startsWith(names(rv$generic_results), "pct_")]


    # Clean names for display
    display_names <- gsub("pct_", "", population_cols)


    updateSelectInput(session, "generic_dr_population_select",
      choices = setNames(population_cols, display_names), # Use raw names as values, clean names as labels
      selected = population_cols[1]
    )
  })


  # 3. Dose-Response (EC50) Plot (Selectable Population)
  output$generic_dose_response_plot <- renderPlot({
    req(input$generic_dr_population_select, rv$generic_plot_data_dr, rv$generic_predicted_curves_dr)


    selected_pop_col <- input$generic_dr_population_select


    plot_data <- rv$generic_plot_data_dr %>%
      filter(Population == selected_pop_col)


    predicted_curves <- rv$generic_predicted_curves_dr %>%
      filter(Population == selected_pop_col)


    req(nrow(plot_data) > 0) # Ensure data exists after filtering


    cell_lines <- unique(plot_data$cell_line)
    colors <- get_color_palette(length(cell_lines))
    names(colors) <- cell_lines


    conc_range <- plot_data$concentration_uM
    breaks <- get_smart_breaks(conc_range)


    # Clean population name for title
    pop_display_name <- gsub("pct_", "", selected_pop_col)


    p <- ggplot() +
      geom_point(
        data = plot_data, aes(x = concentration_uM, y = mean_pop, color = cell_line),
        size = 4, shape = 21, fill = "white", stroke = 1.5
      ) +
      geom_errorbar(
        data = plot_data,
        aes(x = concentration_uM, ymin = mean_pop - se_pop, ymax = mean_pop + se_pop, color = cell_line),
        width = 0.15, linewidth = 1
      ) +
      geom_line(
        data = predicted_curves,
        aes(x = concentration_uM, y = predicted_pop, color = cell_line),
        linewidth = 1.5, alpha = 0.9
      ) +
      scale_color_manual(values = colors) +
      scale_x_continuous(trans = pseudo_log_trans_for_breaks(), breaks = breaks, labels = format_conc_labels) +
      labs(
        title = paste("Dose-Response Curve for Population:", pop_display_name),
        subtitle = "EC50 determined from selected population percentage",
        x = "Concentration (µM)", y = "Population Percentage (%)", color = "Cell Line"
      ) +
      theme_publication() +
      theme(plot.title = element_text(size = 14, face = "bold"), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))


    rv$generic_dose_response_plot_obj <- p # Store the plot object
    p # Return the plot
  })


  # === GENERIC BEAD QC PLOTS ===
  output$generic_bead_qc_plot <- renderPlot({
    req(rv$cell_counts, input$is_absolute_counting)
    if (!"bead_count" %in% names(rv$cell_counts)) return(NULL)
    
    bead_stats <- rv$cell_counts %>%
      summarise(mean_beads = mean(bead_count, na.rm = TRUE), sd_beads = sd(bead_count, na.rm = TRUE), cv_beads = (sd_beads / mean_beads) * 100)
    
    p <- ggplot(rv$cell_counts, aes(x = replicate, y = bead_count, fill = cell_line)) +
      geom_bar(stat = "identity", position = position_dodge(), color = "black") +
      facet_wrap(~ cell_line + concentration_uM, scales = "free_x") +
      geom_hline(yintercept = bead_stats$mean_beads, linetype = "dashed", color = "red") +
      labs(title = "Bead Count Stability Check", subtitle = paste0("Global CV: ", round(bead_stats$cv_beads, 2), "%"), x = "Replicate", y = "Bead Count") +
      theme_publication() + theme(axis.text.x = element_text(angle = 0, hjust = 0.5))
      
    rv$generic_bead_qc_plot_obj <- p
    p
  })
  
  output$generic_bead_impact_plot <- renderPlot({
    req(rv$cell_counts, input$is_absolute_counting)
    # Generic usually has total events as "after_singlets" and no "abs_conc_total" unless calculated
    # But run_generic_analysis adds abs_pop columns. We need a summary total.
    # Let's use the columns already in cell_counts
    
    # Check if we have abs_conc_total or similar. run_generic_analysis adds it?
    # I need to check run_generic_analysis return values.
    # It returns cell_counts_data with `bead_count`.
    # It does NOT seem to calculate a "total absolute count" column by default in the cell_counts table, 
    # but it calculates abs_pop for each population in results.
    
    # We can approximate "Total Absolute Cells" by summing the abs_ populations or just re-calculating using the factor.
    # Factor = (BeadVol/SampleVol) * BeadConc / BeadCount
    # AbsTotal = AfterSinglets * Factor
    
    df_long <- rv$cell_counts %>%
      mutate(
        abs_factor = (input$bead_vol / input$sample_vol) * input$bead_conc / bead_count,
        abs_conc_total = after_singlets * abs_factor
      ) %>%
      dplyr::select(cell_line, concentration_uM, replicate, after_singlets, abs_conc_total) %>%
      pivot_longer(cols = c(after_singlets, abs_conc_total), names_to = "Metric", values_to = "Value") %>%
      mutate(Metric_Label = ifelse(Metric == "after_singlets", "Raw Count", "Absolute Conc."))
      
    p <- ggplot(df_long, aes(x = as.factor(concentration_uM), y = Value, fill = cell_line)) +
      geom_boxplot(alpha = 0.7, outlier.shape = NA) +
      geom_point(position = position_jitterdodge(), size = 1) +
      facet_grid(Metric_Label ~ cell_line, scales = "free_y") +
      scale_y_continuous(labels = scales::label_number(big.mark = ",")) + # Better for counts than log labels usually, unless huge range
      labs(title = "Impact of Absolute Quantification", x = "Concentration (µM)", y = "Value") +
      theme_publication() +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) # Fix X axis angle
      
    rv$generic_bead_impact_plot_obj <- p
    p
  })

  # === VIABILITY PLOT ===
  output$viability_plot <- renderPlot({
    req(rv$viability_plot_obj)
    rv$viability_plot_obj
  })

  # === DEATH PLOT ===
  output$death_plot <- renderPlot({
    req(rv$death_plot_obj)
    rv$death_plot_obj
  })

  # === ABSOLUTE SURVIVAL PLOT ===
  output$abs_survival_plot <- renderPlot({
    req(rv$absolute_survival_plot_obj)
    rv$absolute_survival_plot_obj
  })

  # === IC50 vs LD50 COMPARISON ===
  output$ic50_ld50_comparison <- renderPlot({
    req(rv$comparison_plot_obj)
    rv$comparison_plot_obj
  })

  # === QUADRANT BREAKDOWN ===
  output$quadrant_plot <- renderPlot({
    req(rv$quadrant_plot_obj)
    rv$quadrant_plot_obj
  })
  
  # === BEAD QC PLOTS ===
  output$bead_qc_plot <- renderPlot({
    req(rv$cell_counts, input$is_absolute_counting)
    if (!"bead_count" %in% names(rv$cell_counts)) return(NULL)
    
    bead_stats <- rv$cell_counts %>%
      summarise(mean_beads = mean(bead_count, na.rm = TRUE), sd_beads = sd(bead_count, na.rm = TRUE), cv_beads = (sd_beads / mean_beads) * 100)
    
    p <- ggplot(rv$cell_counts, aes(x = replicate, y = bead_count, fill = cell_line)) +
      geom_bar(stat = "identity", position = position_dodge(), color = "black", width = 0.7) +
      facet_wrap(~ cell_line + concentration_uM, scales = "free_x") +
      geom_hline(yintercept = bead_stats$mean_beads, linetype = "dashed", color = "red") +
      labs(title = "Bead Count Stability Check", 
           subtitle = paste0("Global CV: ", round(bead_stats$cv_beads, 2), "% (Target < 10%)"), 
           x = "Replicate", y = "Bead Count (Events)") +
      theme_publication() + 
      theme(axis.text.x = element_text(angle = 0, hjust = 0.5, size = 8),
            strip.text = element_text(size = 8))
      
    rv$bead_qc_plot_obj <- p
    p
  })
  
  output$bead_impact_plot <- renderPlot({
    req(rv$cell_counts, input$is_absolute_counting)
    
    df_long <- rv$cell_counts %>%
      dplyr::select(cell_line, concentration_uM, replicate, after_singlets, abs_conc_total) %>% 
      pivot_longer(cols = c(after_singlets, abs_conc_total), names_to = "Metric", values_to = "Value") %>%
      mutate(
        Metric_Label = ifelse(Metric == "after_singlets", "Raw Count (Events)", "Absolute Conc. (Cells/µL)")
      )
      
    p <- ggplot(df_long, aes(x = as.factor(concentration_uM), y = Value, fill = cell_line)) +
      geom_boxplot(alpha = 0.7, outlier.shape = NA) +
      geom_point(position = position_jitterdodge(jitter.width = 0.2), size = 1.2, alpha = 0.8) +
      facet_wrap(~ Metric_Label, scales = "free_y", ncol = 1) +
      scale_y_continuous(labels = scales::label_number(big.mark = ",")) +
      labs(
        title = "Impact of Absolute Quantification",
        subtitle = "Comparison of Raw Machine Events vs. Volumetric Absolute Concentration",
        x = "Concentration (µM)", y = "Value"
      ) +
      theme_publication() +
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 9),
            strip.text = element_text(size = 10, face = "bold"))
      
    rv$bead_impact_plot_obj <- p
    p
  })

  # ============================================================================
  # === DYNAMIC ANALYSIS ENGINE (Fitting, Stats, Plots) ===
  # ============================================================================
  
  observe({
    req(rv$results, rv$control_concentration) # Ensure control is set
    
    # 1. FILTER DATA based on Exclusions
    # ... (rest of logic) ...
    data_current <- rv$results
    data_current$Row_ID <- paste(data_current$cell_line, data_current$concentration_uM, data_current$replicate, sep = "_")
    
    # Filter out excluded rows
    data_active <- data_current[!data_current$Row_ID %in% rv$manual_exclusions, ]
    
    if (nrow(data_active) == 0) return() # Safety check

    # 2. RE-CALCULATE QC METRICS
    # -------------------------------------------
    qc_summary <- rv$results %>%
      group_by(cell_line, concentration_uM) %>%
      summarise(
        mean_viable = mean(pct_viable, na.rm = TRUE),
        sd_viable = sd(pct_viable, na.rm = TRUE),
        cv_pct = (sd_viable / mean_viable) * 100,
        n = n(),
        .groups = "drop"
      )
      
    rv$qc_data <- rv$results %>%
      left_join(qc_summary, by = c("cell_line", "concentration_uM"))
    
    # Outlier Detection
    outlier_flags <- data.frame()
    for (i in 1:nrow(rv$results)) {
       group_data <- rv$results %>%
         filter(cell_line == rv$results$cell_line[i], concentration_uM == rv$results$concentration_uM[i])
       if (nrow(group_data) >= 3) {
         grubbs_res <- tryCatch(grubbs.test(group_data$pct_viable), error=function(e) NULL)
         if (!is.null(grubbs_res) && grubbs_res$p.value < 0.05) {
             mean_val <- mean(group_data$pct_viable)
             diffs <- abs(group_data$pct_viable - mean_val)
             outlier_idx <- which.max(diffs)
             outlier_flags <- rbind(outlier_flags, data.frame(
               cell_line = rv$results$cell_line[i],
               concentration_uM = rv$results$concentration_uM[i],
               replicate = group_data$replicate[outlier_idx],
               is_outlier = TRUE,
               p_value = grubbs_res$p.value,
               stringsAsFactors = FALSE
             ))
         }
       }
    }
    rv$outlier_flags <- unique(outlier_flags)

    # 3. NORMALIZATION & CONTROL QC
    # ---------------------------------
    results_proc <- data_active
    
    # Always calculate control stats for QC
    control_mean <- results_proc %>%
         filter(concentration_uM == rv$control_concentration) %>%
         group_by(cell_line) %>%
         summarise(control_viable = mean(pct_viable, na.rm=TRUE), .groups="drop")

    # QC Check for Control Viability (< 70%)
    low_quality_controls <- control_mean %>% filter(control_viable < 70)
    
    if (nrow(low_quality_controls) > 0) {
        rv$control_quality_alert <- low_quality_controls
    } else {
        rv$control_quality_alert <- NULL
    }
    
    if (input$use_normalization) {
       results_proc <- results_proc %>%
         left_join(control_mean, by="cell_line") %>%
         mutate(pct_viable = (pct_viable / control_viable) * 100)
    }
    
    # 3.5 ABSOLUTE SURVIVAL NORMALIZATION
    if (input$is_absolute_counting) {
       control_abs <- results_proc %>%
         filter(concentration_uM == rv$control_concentration) %>%
         group_by(cell_line) %>%
         summarise(control_abs_conc = mean(abs_conc_total, na.rm=TRUE), .groups="drop")
         
       results_proc <- results_proc %>%
         left_join(control_abs, by="cell_line") %>%
         mutate(pct_absolute_survival = (abs_conc_total / control_abs_conc) * 100)
    }

    # 4. SUMMARIZE DATA
    # -------------------------------------
    viability_data <- results_proc %>%
      group_by(cell_line, concentration_uM) %>%
      summarise(
        mean_viable = mean(pct_viable, na.rm = TRUE),
        sd_viable = sd(pct_viable, na.rm = TRUE),
        n = n(),
        se_viable = sd_viable / sqrt(n),
        .groups = "drop"
      ) %>%
      mutate(concentration_uM = as.numeric(as.character(concentration_uM)))

    death_data <- results_proc %>%
      group_by(cell_line, concentration_uM) %>%
      summarise(
        mean_death = mean(pct_cell_death, na.rm = TRUE),
        sd_death = sd(pct_cell_death, na.rm = TRUE),
        n = n(),
        se_death = sd_death / sqrt(n),
        .groups = "drop"
      ) %>%
      mutate(concentration_uM = as.numeric(as.character(concentration_uM)))
      
    abs_survival_summary <- NULL
    if (input$is_absolute_counting) {
       abs_survival_summary <- results_proc %>%
         group_by(cell_line, concentration_uM) %>%
         summarise(
           mean_survival = mean(pct_absolute_survival, na.rm = TRUE),
           sd_survival = sd(pct_absolute_survival, na.rm = TRUE),
           n = n(),
           se_survival = sd_survival / sqrt(n),
           
           # Also summarize absolute death concentration
           mean_abs_death = mean(abs_conc_death, na.rm = TRUE),
           sd_abs_death = sd(abs_conc_death, na.rm = TRUE),
           se_abs_death = sd_abs_death / sqrt(n),
           .groups = "drop"
         ) %>%
         mutate(concentration_uM = as.numeric(as.character(concentration_uM)))
    }

    # 5. CURVE FITTING
    # ------------------------------
    ic50_results <- data.frame()
    ld50_results <- data.frame()
    abs_survival_results <- data.frame()
    abs_ld50_results <- data.frame() # NEW
    predicted_viability_curves <- data.frame()
    predicted_death_curves <- data.frame()
    predicted_abs_survival_curves <- data.frame()
    advanced_metrics <- data.frame()

    for (line in unique(viability_data$cell_line)) {
       line_viab <- viability_data %>% filter(cell_line == line)
       line_death <- death_data %>% filter(cell_line == line)
       
       # --- Absolute Survival Fit ---
       if (input$is_absolute_counting && !is.null(abs_survival_summary)) {
          line_abs <- abs_survival_summary %>% filter(cell_line == line)
          if (nrow(line_abs) >= 4) {
             line_abs$dose <- line_abs$concentration_uM + 0.001
             fit_abs <- tryCatch(drm(mean_survival ~ dose, data = line_abs, fct = LL.4()), error = function(e) NULL)
             
             if (!is.null(fit_abs)) {
                res <- tryCatch(ED(fit_abs, 50, interval="delta", display=FALSE), error=function(e) NULL)
                if (!is.null(res)) {
                   abs_survival_results <- rbind(abs_survival_results, data.frame(
                     cell_line = line,
                     IC50_uM = res[1] - 0.001,
                     IC50_lower = res[3] - 0.001,
                     IC50_upper = res[4] - 0.001,
                     stringsAsFactors = FALSE
                   ))
                   
                   pred_doses <- seq(min(line_abs$dose), max(line_abs$dose), length.out = 200)
                   pred_curve <- data.frame(
                      dose = pred_doses,
                      predicted_survival = predict(fit_abs, newdata=data.frame(dose=pred_doses)),
                      concentration_uM = pred_doses - 0.001,
                      cell_line = line
                   )
                   predicted_abs_survival_curves <- rbind(predicted_abs_survival_curves, pred_curve)
                }
             }
          }
          
          # --- NEW: Absolute LD50 Fit (using absolute death concentration) ---
          if ("mean_abs_death" %in% names(line_abs) && nrow(line_abs) >= 4) {
             fit_abs_ld <- tryCatch(drm(mean_abs_death ~ dose, data = line_abs, fct = LL.4()), error = function(e) NULL)
             if (!is.null(fit_abs_ld)) {
                res_ld <- tryCatch(ED(fit_abs_ld, 50, interval="delta", display=FALSE), error=function(e) NULL)
                if (!is.null(res_ld)) {
                   abs_ld50_results <- rbind(abs_ld50_results, data.frame(
                     cell_line = line,
                     LD50_uM = res_ld[1] - 0.1,
                     LD50_lower = res_ld[3] - 0.1,
                     LD50_upper = res_ld[4] - 0.1,
                     stringsAsFactors = FALSE
                   ))
                }
             }
          }
       }

       # --- Viability Fit ---
       if (nrow(line_viab) >= 4) {
         line_viab$dose <- line_viab$concentration_uM + 0.001
         fit_viab <- tryCatch(drm(mean_viable ~ dose, data = line_viab, fct = LL.4()), error = function(e) NULL)
         if (is.null(fit_viab)) fit_viab <- tryCatch(drm(mean_viable ~ dose, data=line_viab, fct=LL.3()), error=function(e) NULL)
         if (is.null(fit_viab)) fit_viab <- tryCatch(drm(mean_viable ~ dose, data=line_viab, fct=W1.4()), error=function(e) NULL)
         
         if (!is.null(fit_viab)) {
            res <- tryCatch(ED(fit_viab, 50, interval="delta", display=FALSE), error=function(e) NULL)
            if (!is.null(res)) {
               ic50_results <- rbind(ic50_results, data.frame(
                 cell_line = line,
                 IC50_uM = res[1] - 0.001,
                 IC50_lower = res[3] - 0.001,
                 IC50_upper = res[4] - 0.001,
                 fit_method = "Robust",
                 stringsAsFactors = FALSE
               ))
               
               # Metrics & Curves
               pred_doses <- seq(min(line_viab$dose), max(line_viab$dose), length.out = 200)
               pred_curve <- data.frame(
                  dose = pred_doses,
                  predicted_viable = predict(fit_viab, newdata=data.frame(dose=pred_doses)),
                  concentration_uM = pred_doses - 0.001,
                  cell_line = line
               )
               predicted_viability_curves <- rbind(predicted_viability_curves, pred_curve)
               
               obs <- line_viab$mean_viable
               pred <- predict(fit_viab)
               r2 <- 1 - (sum((obs-pred)^2) / sum((obs-mean(obs))^2))
               coefs <- coef(fit_viab)
               hill <- if(length(coefs)>=1) abs(coefs[1]) else NA
               dose_seq <- seq(min(line_viab$concentration_uM), max(line_viab$concentration_uM), length.out=1000)
               pred_seq <- predict(fit_viab, newdata=data.frame(dose=dose_seq+0.1))
               auc <- tryCatch(AUC(dose_seq, pred_seq, method="trapezoid"), error=function(e) NA)

               advanced_metrics <- rbind(advanced_metrics, data.frame(
                 cell_line = line, metric_type="Viability", Hill_slope=hill, AUC=auc, R_squared=r2,
                 EC25_uM=NA, EC75_uM=NA, EC90_uM=NA, Top_plateau=NA, Bottom_plateau=NA
               ))
            }
         }
       }
       
       # --- Death Fit ---
       if (nrow(line_death) >= 4) {
          line_death$dose <- line_death$concentration_uM + 0.001
          fit_death <- tryCatch(drm(mean_death ~ dose, data = line_death, fct = LL.4()), error = function(e) NULL)
          if (!is.null(fit_death)) {
             res <- tryCatch(ED(fit_death, 50, interval="delta", display=FALSE), error=function(e) NULL)
             if (!is.null(res)) {
                ld50_results <- rbind(ld50_results, data.frame(
                  cell_line = line,
                  LD50_uM = res[1] - 0.001,
                  LD50_lower = res[3] - 0.001,
                  LD50_upper = res[4] - 0.001,
                  stringsAsFactors = FALSE
                ))
                pred_doses <- seq(min(line_death$dose), max(line_death$dose), length.out = 200)
                pred_curve <- data.frame(
                   dose = pred_doses,
                   predicted_death = predict(fit_death, newdata=data.frame(dose=pred_doses)),
                   concentration_uM = pred_doses - 0.001,
                   cell_line = line
                )
                predicted_death_curves <- rbind(predicted_death_curves, pred_curve)
             }
          }
       }
    }
    
    # 6. UPDATE REACTIVE VALUES
    rv$ic50_results <- ic50_results
    rv$ld50_results <- ld50_results
    rv$absolute_survival_results <- abs_survival_results
    rv$absolute_ld50_results <- abs_ld50_results # NEW
    rv$viability_data <- viability_data
    rv$death_data <- death_data
    rv$predicted_viability_curves <- predicted_viability_curves
    rv$predicted_death_curves <- predicted_death_curves
    rv$predicted_absolute_survival_curves <- predicted_abs_survival_curves
    rv$advanced_metrics <- advanced_metrics
    
    if (nrow(viability_data) > 0) rv$viability_plot_obj <- generate_viability_plot(viability_data, predicted_viability_curves)
    if (nrow(death_data) > 0) rv$death_plot_obj <- generate_death_plot(death_data, predicted_death_curves)
    if (input$is_absolute_counting && !is.null(abs_survival_summary)) {
        rv$absolute_survival_plot_obj <- generate_absolute_survival_plot(abs_survival_summary, predicted_abs_survival_curves)
    }
    if (nrow(ic50_results) > 0 || nrow(ld50_results) > 0) rv$comparison_plot_obj <- generate_comparison_plot(ic50_results, ld50_results)
    if (nrow(results_proc) > 0) rv$quadrant_plot_obj <- generate_quadrant_plot(results_proc)

    # 7. STATISTICAL ANALYSIS (TUKEY)
    # -------------------------------
    rv$posthoc_results <- NULL
    
    if (nrow(ic50_results) >= 2) {
       stats_data <- ic50_results
       stats_data$LogIC50 <- log10(stats_data$IC50_uM)
       aov_res <- tryCatch(aov(LogIC50 ~ cell_line, data = stats_data), error=function(e) NULL)
       if (!is.null(aov_res)) {
          tukey_res <- tryCatch(TukeyHSD(aov_res), error=function(e) NULL)
          if (!is.null(tukey_res)) {
             tukey_df <- as.data.frame(tukey_res$cell_line)
             tukey_df <- tibble::rownames_to_column(tukey_df, "Comparison")
             rv$posthoc_results <- tukey_df
          }
       }
    }
  })
  
  # Output for Tukey Table
  output$posthoc_table <- DT::renderDataTable({
    req(rv$posthoc_results)
    DT::datatable(
      rv$posthoc_results, 
      rownames = FALSE,
      class = "compact hover cell-border stripe",
      options = list(
        dom = "t", 
        paging = FALSE,
        columnDefs = list(list(className = 'dt-center', targets = "_all"))
      ), 
      caption = "Tukey's Honest Significant Difference (HSD) Post-Hoc Test (Log10 IC50)"
    ) %>%
    DT::formatRound(columns = 2:5, digits = 4) %>%
    DT::formatStyle(
      'p adj',
      backgroundColor = DT::styleInterval(c(0.001, 0.01, 0.05), c('#d4edda', '#e2f3e5', '#f3faf4', 'white')),
      fontWeight = DT::styleInterval(0.05, c('bold', 'normal'))
    )
  })

  # ============================================================================
  # === GENERIC DYNAMIC ANALYSIS ENGINE ===
  # ============================================================================
  
  observe({
    req(rv$generic_results)
    
    # 1. Filter Data
    data_current <- rv$generic_results
    data_current$Row_ID <- paste(data_current$cell_line, data_current$concentration_uM, data_current$replicate, sep = "_")
    data_active <- data_current[!data_current$Row_ID %in% rv$generic_manual_exclusions, ]
    
    # 2. Outlier Detection (Grubbs) for Generic
    # Iterate over all population columns
    outlier_flags_generic <- data.frame()
    pop_cols <- names(data_current)[startsWith(names(data_current), "pct_")]
    
    for (pop in pop_cols) {
       for (i in 1:nrow(data_current)) {
         # Group by Cell Line and Concentration
         group_data <- data_current %>%
           filter(cell_line == data_current$cell_line[i], concentration_uM == data_current$concentration_uM[i])
           
         if (nrow(group_data) >= 3) {
           vals <- group_data[[pop]]
           if (var(vals) == 0) next # Skip if no variance
           
           grubbs_res <- tryCatch(grubbs.test(vals), error=function(e) NULL)
           if (!is.null(grubbs_res) && grubbs_res$p.value < 0.05) {
               mean_val <- mean(vals)
               diffs <- abs(vals - mean_val)
               outlier_idx <- which.max(diffs)
               
               # Check if this outlier is already recorded for this pop (to avoid duplicates from loop)
               # But we are looping rows... inefficient but safe if we unique() later.
               
               outlier_flags_generic <- rbind(outlier_flags_generic, data.frame(
                 cell_line = data_current$cell_line[i],
                 concentration_uM = data_current$concentration_uM[i],
                 replicate = group_data$replicate[outlier_idx],
                 Target_Population = gsub("pct_", "", pop),
                 p_value = grubbs_res$p.value,
                 stringsAsFactors = FALSE
               ))
           }
         }
       }
    }
    rv$outlier_flags <- unique(outlier_flags_generic)
    
    # 3. Curve Fitting
    population_cols <- names(data_active)[startsWith(names(data_active), "pct_")]
    ec50_results <- data.frame()
    all_predicted_curves_dr <- data.frame()
    all_plot_data_dr <- data.frame()

    for (pop_col in population_cols) {
      plot_data_for_fitting <- data_active %>%
        filter(!is.na(.data[[pop_col]])) %>%
        group_by(cell_line, concentration_uM) %>%
        summarise(
          mean_pop = mean(.data[[pop_col]], na.rm = TRUE),
          sd_pop = sd(.data[[pop_col]], na.rm = TRUE),
          n = n(),
          se_pop = sd_pop / sqrt(n),
          .groups = "drop"
        ) %>%
        mutate(concentration_uM = as.numeric(as.character(concentration_uM)))

      if (nrow(plot_data_for_fitting) == 0) next
      
      plot_data_for_fitting$Population <- pop_col
      all_plot_data_dr <- rbind(all_plot_data_dr, plot_data_for_fitting)

      for (line in unique(plot_data_for_fitting$cell_line)) {
        line_data <- plot_data_for_fitting %>% filter(cell_line == line)
        if (nrow(line_data) < 4) next

        line_data$dose <- line_data$concentration_uM + 0.001
        fit <- tryCatch(drm(mean_pop ~ dose, data = line_data, fct = LL.4()), error = function(e) NULL)

        if (!is.null(fit)) {
          ec50_val <- tryCatch(ED(fit, 50, interval = "delta", display = FALSE)[1, 1], error = function(e) NA)
          ec50_results <- rbind(ec50_results, data.frame(
            cell_line = line,
            EC50_uM = ec50_val - 0.001,
            Target_Population = pop_col,
            stringsAsFactors = FALSE
          ))

          pred_doses <- seq(min(line_data$dose), max(line_data$dose), length.out = 100)
          pred_data <- data.frame(dose = pred_doses)
          pred_data$predicted_pop <- predict(fit, newdata = pred_data)
          pred_data$concentration_uM <- pred_doses - 0.001
          pred_data$cell_line <- line
          pred_data$Population <- pop_col
          all_predicted_curves_dr <- rbind(all_predicted_curves_dr, pred_data)
        }
      }
    }
    
    rv$generic_ec50_results <- ec50_results
    rv$generic_plot_data_dr <- all_plot_data_dr
    rv$generic_predicted_curves_dr <- all_predicted_curves_dr
    
    # 3. Statistical Analysis (ANOVA + Tukey) for Generic Mode
    rv$generic_posthoc_results <- NULL
    rv$generic_anova_results <- NULL
    
    if (nrow(ec50_results) > 0) {
       # Perform stats per population
       stats_list <- list()
       anova_list <- list()
       
       for (pop in unique(ec50_results$Target_Population)) {
          pop_data <- ec50_results %>% filter(Target_Population == pop)
          if (length(unique(pop_data$cell_line)) >= 2) {
             pop_data$LogEC50 <- log10(pop_data$EC50_uM)
             aov_res <- tryCatch(aov(LogEC50 ~ cell_line, data = pop_data), error=function(e) NULL)
             
             if (!is.null(aov_res)) {
                # Store ANOVA summary
                sum_aov <- summary(aov_res)
                df_aov <- as.data.frame(sum_aov[[1]])
                df_aov$Population <- pop
                anova_list[[pop]] <- tibble::rownames_to_column(df_aov, "Source")

                tukey_res <- tryCatch(TukeyHSD(aov_res), error=function(e) NULL)
                if (!is.null(tukey_res)) {
                   df <- as.data.frame(tukey_res$cell_line)
                   df <- tibble::rownames_to_column(df, "Comparison")
                   df$Population <- pop
                   stats_list[[pop]] <- df
                }
             }
          }
       }
       if (length(stats_list) > 0) {
          rv$generic_posthoc_results <- do.call(rbind, stats_list)
       }
       if (length(anova_list) > 0) {
          rv$generic_anova_results <- do.call(rbind, anova_list)
       }
    }
  })
  
  # Generic ANOVA Table
  output$generic_anova_table <- DT::renderDataTable({
    req(rv$generic_anova_results)
    
    df <- rv$generic_anova_results
    if ("Population" %in% names(df)) {
      df$Population <- gsub("pct_", "", df$Population)
    }
    
    # Replace NaN with NA for better DT handling
    df[] <- lapply(df, function(x) { if(is.numeric(x)) { x[is.nan(x)] <- NA; x } else { x } })

    DT::datatable(
      df, 
      rownames = FALSE,
      class = "compact hover cell-border stripe",
      options = list(
        pageLength = 10,
        dom = "tip",
        search = list(search = ""), # Clear any stray search
        columnDefs = list(list(className = 'dt-center', targets = "_all"))
      ), 
      caption = "Analysis of Variance (ANOVA) Results (Log10 EC50)"
    ) %>%
    DT::formatRound(columns = which(sapply(df, is.numeric)), digits = 4)
  })

  output$generic_anova_interpretation <- renderUI({
    req(rv$generic_anova_results)
    df <- rv$generic_anova_results
    
    # Check if there are any non-NA p-values to interpret
    if (all(is.na(df$`Pr(>F)`))) {
       return(div(class = "alert alert-info", style = "margin-top: 10px;",
        strong(icon("info-circle"), " Note on Statistical Power:"),
        p("ANOVA p-values could not be calculated. This typically occurs when there is only one observation (EC50) per cell line, providing zero degrees of freedom for the residuals. Replicate-level data is required for ANOVA-based significance testing.")
      ))
    }

    # Filter for the 'cell_line' source to get p-values for inter-group differences
    anova_sig <- df %>% 
      filter(Source == "cell_line") %>%
      mutate(Significant = !is.na(`Pr(>F)`) & `Pr(>F)` < 0.05)
    
    sig_pops <- anova_sig %>% filter(Significant) %>% pull(Population)
    sig_pops <- gsub("pct_", "", sig_pops)
    
    if (length(sig_pops) > 0) {
      return(div(class = "alert alert-success", style = "margin-top: 10px;",
        strong(icon("check-circle"), " Statistical Summary:"),
        p(sprintf("Significant differences (p < 0.05) in EC50 were found for the following populations: %s.", paste(sig_pops, collapse = ", "))),
        p("For these populations, at least one cell line differs significantly from the others. Check the Tukey Post-Hoc tab for details.")
      ))
    } else {
      return(div(class = "alert alert-warning", style = "margin-top: 10px;",
        strong(icon("exclamation-triangle"), " Statistical Summary:"),
        p("No statistically significant differences in EC50 were found between cell lines for any of the analyzed populations at the α=0.05 level.")
      ))
    }
  })
  
  # Generic Tukey Table
  output$generic_posthoc_table <- DT::renderDataTable({
    req(rv$generic_posthoc_results)
    
    df <- rv$generic_posthoc_results
    if ("Population" %in% names(df)) {
      df$Population <- gsub("pct_", "", df$Population)
    }
    
    # Replace NaN with NA
    df[] <- lapply(df, function(x) { if(is.numeric(x)) { x[is.nan(x)] <- NA; x } else { x } })

    DT::datatable(
      df, 
      rownames = FALSE,
      class = "compact hover cell-border stripe",
      options = list(
        pageLength = 10,
        dom = "tip",
        search = list(search = ""), # Clear any stray search
        columnDefs = list(list(className = 'dt-center', targets = "_all"))
      ), 
      caption = "Tukey's HSD Post-Hoc Test (Log10 EC50)"
    ) %>%
    DT::formatRound(columns = which(sapply(df, is.numeric)), digits = 4) %>%
    DT::formatStyle(
      'p adj',
      backgroundColor = DT::styleInterval(c(0.001, 0.01, 0.05), c('#d4edda', '#e2f3e5', '#f3faf4', 'white')),
      fontWeight = DT::styleInterval(0.05, c('bold', 'normal'))
    )
  })

  # --- NEW: Gating Summary Table ---
  output$cell_counts_table <- DT::renderDataTable({
    req(rv$cell_counts)
    
    display_df <- rv$cell_counts %>%
      mutate(
        pct_fsc_ssc = round(after_fsc_ssc / total_events * 100, 1),
        pct_singlets = round(after_singlets / after_fsc_ssc * 100, 1)
      )
      
    if (input$is_absolute_counting) {
       display_df <- display_df %>%
         dplyr::select(
           `Cell Line` = cell_line,
           `Conc (µM)` = concentration_uM,
           `Rep` = replicate,
           `Beads` = bead_count,
           `Total Events` = total_events,
           `After FSC/SSC` = after_fsc_ssc,
           `Singlets` = after_singlets,
           `Abs. Viable (Cells/µL)` = abs_conc_viable
         )
    } else {
       display_df <- display_df %>%
         dplyr::select(
           `Cell Line` = cell_line,
           `Conc (µM)` = concentration_uM,
           `Rep` = replicate,
           `Total Events` = total_events,
           `After FSC/SSC` = after_fsc_ssc,
           `% Recovery` = pct_fsc_ssc,
           `Singlets` = after_singlets,
           `% Singlets` = pct_singlets
         )
    }
      
    dt_obj <- DT::datatable(
      display_df, 
      rownames = FALSE,
      class = "compact hover cell-border stripe",
      options = list(
        pageLength = 10, 
        dom = "tip",
        columnDefs = list(list(className = 'dt-center', targets = "_all"))
      ), 
      caption = "Cell Recovery and Absolute Quantification Summary"
    )
    
    # Apply formatting if absolute column exists
    if (input$is_absolute_counting) {
       dt_obj <- dt_obj %>% DT::formatRound(columns = "Abs. Viable (Cells/µL)", digits = 0, mark = ",")
    }
    
    dt_obj
  })

  output$generic_cell_counts_table <- DT::renderDataTable({
    req(rv$cell_counts)
    
    display_df <- rv$cell_counts %>%
      mutate(
        pct_fsc_ssc = round(after_fsc_ssc / total_events * 100, 1),
        pct_singlets = round(after_singlets / after_fsc_ssc * 100, 1)
      )
      
    if (input$is_absolute_counting) {
       display_df <- display_df %>%
         dplyr::select(
           `Cell Line` = cell_line,
           `Conc (µM)` = concentration_uM,
           `Rep` = replicate,
           `Beads` = bead_count,
           `Total Events` = total_events,
           `After FSC/SSC` = after_fsc_ssc,
           `Singlets` = after_singlets
         )
    } else {
       display_df <- display_df %>%
         dplyr::select(
           `Cell Line` = cell_line,
           `Conc (µM)` = concentration_uM,
           `Rep` = replicate,
           `Total Events` = total_events,
           `After FSC/SSC` = after_fsc_ssc,
           `% Recovery` = pct_fsc_ssc,
           `Singlets` = after_singlets,
           `% Singlets` = pct_singlets
         )
    }
      
    DT::datatable(
      display_df, 
      rownames = FALSE,
      class = "compact hover cell-border stripe",
      options = list(
        pageLength = 10, 
        dom = "tip",
        columnDefs = list(list(className = 'dt-center', targets = "_all"))
      ), 
      caption = "Cell Recovery and Bead Counting Summary"
    )
  })

  # --- NEW: Download Handlers ---
  output$download_tukey <- downloadHandler(
    filename = function() { paste0("Tukey_HSD_Apoptosis_", Sys.Date(), ".csv") },
    content = function(file) {
      req(rv$posthoc_results)
      write_csv(rv$posthoc_results, file)
    }
  )

  output$download_generic_tukey <- downloadHandler(
    filename = function() { paste0("Tukey_HSD_Generic_", Sys.Date(), ".csv") },
    content = function(file) {
      req(rv$generic_posthoc_results)
      write_csv(rv$generic_posthoc_results, file)
    }
  )

  output$download_qc <- downloadHandler(
    filename = function() { paste0("QC_Metrics_", Sys.Date(), ".csv") },
    content = function(file) {
      req(rv$qc_data)
      write_csv(rv$qc_data, file)
    }
  )

  output$download_results <- downloadHandler(
    filename = function() {
      paste0("FACS_results_", Sys.Date(), ".csv")
    },
    content = function(file) {
      req(rv$results)
      write_csv(rv$results, file)
    }
  )

  # === ENHANCED ZIP DOWNLOAD ===
  output$download_zip_ui <- renderUI({
    # Show the download button if either apoptosis or generic results are available
    req(!is.null(rv$results) || !is.null(rv$generic_results))
    downloadButton("download_zip", "📦 Download All Results (ZIP)",
      class = "btn-info btn-lg btn-block",
      style = "margin-top: 10px; font-size: 1.1em; padding: 12px;"
    )
  })

  output$download_zip <- downloadHandler(
    filename = function() {
      paste0("FACS_Analysis_", Sys.Date(), ".zip")
    },
    content = function(file) {
      shiny::withProgress(message = "Generating Report Package", value = 0, {
        tryCatch(
          {
            # Create a unique subdirectory for this export to ensure clean zip files

            temp_dir <- file.path(tempdir(), paste0("export_", as.numeric(Sys.time()), "_", paste(sample(letters, 5), collapse = "")))

            dir.create(temp_dir)

            # --- SYNCHRONIZE COMPENSATION MATRICES ---
            # Ensure every cell line has an entry in the matrices list passed to the report.
            # If a cell line was added after calculation and has no specific entry, assign the global master.
            
            final_comp_matrices <- list()
            final_spill_matrices <- list()
            
            if (!is.null(rv$metadata)) {
              all_lines <- unique(rv$metadata$cell_line)
              
              for (line in all_lines) {
                # Inverse Matrix
                if (!is.null(rv$comp_matrices[[line]])) {
                  final_comp_matrices[[line]] <- rv$comp_matrices[[line]]
                } else {
                  final_comp_matrices[[line]] <- rv$comp_matrix
                }
                
                # Spillover Matrix
                if (!is.null(rv$spill_matrices[[line]])) {
                  final_spill_matrices[[line]] <- rv$spill_matrices[[line]]
                } else {
                  final_spill_matrices[[line]] <- rv$spill_matrix
                }
              }
            }


            # --- ROUTE TO CORRECT WORKFLOW ---

            if (input$is_apoptosis_assay) {
              # --- WORKFLOW 1: Apoptosis Report ---

              incProgress(0.1, detail = "Saving apoptosis data...")


              if (!is.null(rv$results) && nrow(rv$results) > 0) {
                write_csv(rv$results, file.path(temp_dir, "raw_quadrant_data.csv"))
              }

              if (!is.null(rv$ic50_results) && nrow(rv$ic50_results) > 0) {
                write_csv(rv$ic50_results, file.path(temp_dir, "IC50_results.csv"))
              }

              if (!is.null(rv$ld50_results) && nrow(rv$ld50_results) > 0) {
                write_csv(rv$ld50_results, file.path(temp_dir, "LD50_results.csv"))
              }

              if (!is.null(rv$qc_data) && nrow(rv$qc_data) > 0) {
                write_csv(rv$qc_data, file.path(temp_dir, "QC_CV_analysis.csv"))
              }

              if (!is.null(rv$outlier_flags) && nrow(rv$outlier_flags) > 0) {
                write_csv(rv$outlier_flags, file.path(temp_dir, "outlier_detection.csv"))
              }

              if (!is.null(rv$cell_counts) && nrow(rv$cell_counts) > 0) {
                write_csv(rv$cell_counts, file.path(temp_dir, "cell_counts_per_gate.csv"))
              }

              if (!is.null(rv$advanced_metrics) && nrow(rv$advanced_metrics) > 0) {
                write_csv(rv$advanced_metrics, file.path(temp_dir, "advanced_metrics_EC_AUC_Hill.csv"))
              }


              incProgress(0.2, detail = "Saving metadata...")

              if (!is.null(rv$metadata) && nrow(rv$metadata) > 0) {
                metadata_summary <- rv$metadata %>%
                  dplyr::select(name, cell_line, treatment_full, concentration_uM, replicate)

                colnames(metadata_summary)[1] <- "filename"

                write_csv(metadata_summary, file.path(temp_dir, "file_metadata.csv"))
              }

              control_summary <- data.frame(
                control_concentration_uM = rv$control_concentration,
                compensation_applied = rv$comp_calculated,
                normalization_applied = input$use_normalization
              )

              write_csv(control_summary, file.path(temp_dir, "analysis_settings.csv"))

              if (rv$comp_calculated && length(rv$comp_matrices) > 0) {
                for (line in names(rv$comp_matrices)) {
                  comp_df <- as.data.frame(rv$comp_matrices[[line]])
                  comp_df <- cbind(Channel = rownames(comp_df), comp_df)
                  write_csv(comp_df, file.path(temp_dir, paste0("compensation_matrix_", line, ".csv")))
                }
              }

              if (length(rv$thresholds) > 0) {
                gate_coords <- data.frame()

                for (line in names(rv$thresholds)) {
                  th <- rv$thresholds[[line]]

                  fsc_poly <- th$fsc_ssc$polygon

                  if (!is.null(fsc_poly) && nrow(fsc_poly) > 0) {
                    fsc_poly$cell_line <- line

                    fsc_poly$gate_type <- "FSC_SSC"

                    gate_coords <- rbind(gate_coords, fsc_poly)
                  }

                  sing_poly <- th$singlets$polygon

                  if (!is.null(sing_poly) && nrow(sing_poly) > 0) {
                    sing_poly$cell_line <- line

                    sing_poly$gate_type <- "Singlets"

                    gate_coords <- rbind(gate_coords, sing_poly)
                  }
                }

                if (nrow(gate_coords) > 0) {
                  write_csv(gate_coords, file.path(temp_dir, "gate_coordinates.csv"))
                }
              }
              
              # Save Bead Plots
              if (input$is_absolute_counting && length(rv$bead_gate_plots) > 0) {
                for (line in names(rv$bead_gate_plots)) {
                  ggsave(file.path(temp_dir, paste0("bead_gate_", line, ".png")), plot = rv$bead_gate_plots[[line]], width = 8, height = 6, dpi = 300)
                }
              }

              # --- Prepare RMarkdown parameters ---

              report_filename <- paste0("FACS_Analysis_Report_", Sys.Date(), ".html")

              ic50_ld50_combined_report <- if (!is.null(rv$ic50_results) && !is.null(rv$ld50_results)) {
                # Helper for report table CI formatting (HTML Style)
                fmt_ci_report <- function(val, low, upp) {
                  if (is.na(val) || is.nan(val)) return("Fit Failed")
                  if (val < 1e-9) return("Not Reached") 
                  
                  # Formatter
                  to_sci <- function(x) {
                    if (is.na(x) || is.nan(x)) return("NA")
                    if (abs(x) < 0.01 || abs(x) > 1000) {
                       exponent <- floor(log10(abs(x)))
                       base <- x / (10^exponent)
                       return(sprintf("%.2f &times; 10<sup>%d</sup>", base, exponent))
                    }
                    sprintf("%.4f", x)
                  }
                  
                  val_s <- to_sci(val)
                  low_s <- to_sci(low)
                  upp_s <- to_sci(upp)
                  
                  paste0(val_s, " [", low_s, " - ", upp_s, "]")
                }

                combined <- merge(rv$ic50_results, rv$ld50_results, by = "cell_line", all = TRUE)
                
                # Initialize optional columns as NA to guarantee existence
                combined$Abs_IC50_uM <- NA
                combined$Abs_IC50_lower <- NA
                combined$Abs_IC50_upper <- NA
                combined$Abs_LD50_uM <- NA
                combined$Abs_LD50_lower <- NA
                combined$Abs_LD50_upper <- NA
                
                if (input$is_absolute_counting && !is.null(rv$absolute_survival_results) && nrow(rv$absolute_survival_results) > 0) {
                   abs_res <- rv$absolute_survival_results %>%
                     dplyr::rename(Abs_IC50_uM = IC50_uM, Abs_IC50_lower = IC50_lower, Abs_IC50_upper = IC50_upper)
                   
                   # Update combined with matching columns
                   rows_match <- match(combined$cell_line, abs_res$cell_line)
                   combined$Abs_IC50_uM <- abs_res$Abs_IC50_uM[rows_match]
                   combined$Abs_IC50_lower <- abs_res$Abs_IC50_lower[rows_match]
                   combined$Abs_IC50_upper <- abs_res$Abs_IC50_upper[rows_match]
                }
                   
                if (input$is_absolute_counting && !is.null(rv$absolute_ld50_results) && nrow(rv$absolute_ld50_results) > 0) {
                      abs_ld_res <- rv$absolute_ld50_results %>%
                        dplyr::rename(Abs_LD50_uM = LD50_uM, Abs_LD50_lower = LD50_lower, Abs_LD50_upper = LD50_upper)
                        
                      rows_match_ld <- match(combined$cell_line, abs_ld_res$cell_line)
                      combined$Abs_LD50_uM <- abs_ld_res$Abs_LD50_uM[rows_match_ld]
                      combined$Abs_LD50_lower <- abs_ld_res$Abs_LD50_lower[rows_match_ld]
                      combined$Abs_LD50_upper <- abs_ld_res$Abs_LD50_upper[rows_match_ld]
                }
                
                # Build list of final columns to keep
                cols_to_select <- c("Cell Line", "IC50 (Viability)", "LD50 (Death)")
                if (input$is_absolute_counting) {
                   cols_to_select <- c(cols_to_select, "Abs. IC50 (Survival)", "Abs. LD50 (Death)")
                }

                combined %>%
                  rowwise() %>%
                  mutate(
                    `IC50 (Viability)` = fmt_ci_report(IC50_uM, IC50_lower, IC50_upper),
                    `LD50 (Death)` = fmt_ci_report(LD50_uM, LD50_lower, LD50_upper),
                    `Abs. IC50 (Survival)` = if(input$is_absolute_counting) fmt_ci_report(Abs_IC50_uM, Abs_IC50_lower, Abs_IC50_upper) else "N/A",
                    `Abs. LD50 (Death)` = if(input$is_absolute_counting) fmt_ci_report(Abs_LD50_uM, Abs_LD50_lower, Abs_LD50_upper) else "N/A"
                  ) %>%
                  dplyr::rename(`Cell Line` = cell_line) %>%
                  dplyr::select(all_of(cols_to_select))
              } else {
                NULL
              }

              ic50_anova_text <- if (!is.null(rv$ic50_results) && nrow(rv$ic50_results) >= 2) {
                anova_result <- tryCatch(
                  {
                    data_for_stats <- rv$ic50_results
                    data_for_stats$LogIC50 <- log10(data_for_stats$IC50_uM)
                    capture.output(summary(aov(LogIC50 ~ cell_line, data = data_for_stats)))
                  },
                  error = function(e) paste("ANOVA could not be performed:", e$message)
                )

                paste(anova_result, collapse = "\n")
              } else {
                "Statistical comparison requires at least 2 cell lines."
              }


              gate_summary_report <- NULL

              if (length(rv$thresholds) > 0) {
                gate_summary_report <- data.frame()

                for (line in names(rv$thresholds)) {
                  th <- rv$thresholds[[line]]

                  fsc_npts <- if (!is.null(th$fsc_ssc$polygon)) nrow(th$fsc_ssc$polygon) else 0

                  sing_npts <- if (!is.null(th$singlets$polygon)) nrow(th$singlets$polygon) else 0

                  ann_th <- th$annexin_pi$annexin

                  pi_th <- th$annexin_pi$pi

                  gate_summary_report <- rbind(gate_summary_report, data.frame(
                    cell_line = line,
                    fsc_ssc_points = fsc_npts,
                    singlet_points = sing_npts,
                    annexin_threshold = ann_th,
                    pi_threshold = pi_th,
                    stringsAsFactors = FALSE
                  ))
                }
              }


              # --- Rebuild plots for report and export ---

              incProgress(0.4, detail = "Generating plots...")

              plot_viability <- NULL

              if (!is.null(rv$viability_plot_obj)) {
                plot_viability <- rv$viability_plot_obj

                ggsave(file.path(temp_dir, "viability_curves.png"), plot = plot_viability, width = 12, height = 8, dpi = 300)

                ggsave(file.path(temp_dir, "viability_curves.svg"), plot = plot_viability, width = 12, height = 8)

                ggsave(file.path(temp_dir, "viability_curves.jpg"), plot = plot_viability, width = 12, height = 8, dpi = 300)
              }

              plot_death <- NULL

              if (!is.null(rv$death_plot_obj)) {
                plot_death <- rv$death_plot_obj

                ggsave(file.path(temp_dir, "death_curves.png"), plot = plot_death, width = 12, height = 8, dpi = 300)

                ggsave(file.path(temp_dir, "death_curves.svg"), plot = plot_death, width = 12, height = 8)

                ggsave(file.path(temp_dir, "death_curves.jpg"), plot = plot_death, width = 12, height = 8, dpi = 300)
              }

              plot_ic50_ld50 <- NULL

              if (!is.null(rv$comparison_plot_obj)) {
                plot_ic50_ld50 <- rv$comparison_plot_obj

                ggsave(file.path(temp_dir, "ic50_ld50_comparison.png"), plot = plot_ic50_ld50, width = 10, height = 6, dpi = 300)

                ggsave(file.path(temp_dir, "ic50_ld50_comparison.svg"), plot = plot_ic50_ld50, width = 10, height = 6)

                ggsave(file.path(temp_dir, "ic50_ld50_comparison.jpg"), plot = plot_ic50_ld50, width = 10, height = 6, dpi = 300)
              }

              plot_quadrant <- NULL

              if (!is.null(rv$quadrant_plot_obj)) {
                plot_quadrant <- rv$quadrant_plot_obj

                ggsave(file.path(temp_dir, "quadrant_breakdown.png"), plot = plot_quadrant, width = 14, height = 10, dpi = 300)

                ggsave(file.path(temp_dir, "quadrant_breakdown.svg"), plot = plot_quadrant, width = 14, height = 10)

                ggsave(file.path(temp_dir, "quadrant_breakdown.jpg"), plot = plot_quadrant, width = 14, height = 10, dpi = 300)
              }
              
              plot_abs_survival <- NULL
              if (input$is_absolute_counting && !is.null(rv$absolute_survival_plot_obj)) {
                plot_abs_survival <- rv$absolute_survival_plot_obj
                ggsave(file.path(temp_dir, "absolute_survival_curves.png"), plot = plot_abs_survival, width = 12, height = 8, dpi = 300)
                
                # --- NEW: Save Bead QC Plots ---
                if (!is.null(rv$cell_counts) && "bead_count" %in% names(rv$cell_counts)) {
                   # Regenerate locally to ensure existence
                   bead_stats <- rv$cell_counts %>%
                      summarise(mean_beads = mean(bead_count, na.rm = TRUE), sd_beads = sd(bead_count, na.rm = TRUE), cv_beads = (sd_beads / mean_beads) * 100)
                   
                   p_bead_qc <- ggplot(rv$cell_counts, aes(x = replicate, y = bead_count, fill = cell_line)) +
                      geom_bar(stat = "identity", position = position_dodge(), color = "black") +
                      facet_wrap(~ cell_line + concentration_uM, scales = "free_x") +
                      geom_hline(yintercept = bead_stats$mean_beads, linetype = "dashed", color = "red") +
                      labs(title = "Bead Count Stability Check", subtitle = paste0("Global CV: ", round(bead_stats$cv_beads, 2), "%"), x = "Replicate", y = "Bead Count") +
                      theme_publication()
                   
                   df_long <- rv$cell_counts %>%
                      dplyr::select(cell_line, concentration_uM, replicate, after_singlets, abs_conc_total) %>%
                      pivot_longer(cols = c(after_singlets, abs_conc_total), names_to = "Metric", values_to = "Value") %>%
                      mutate(Metric_Label = ifelse(Metric == "after_singlets", "Raw Count", "Absolute Conc."))
                      
                   p_bead_impact <- ggplot(df_long, aes(x = as.factor(concentration_uM), y = Value, fill = cell_line)) +
                      geom_boxplot(alpha = 0.7) + geom_point(position = position_jitterdodge(), size = 1) +
                      facet_grid(Metric_Label ~ cell_line, scales = "free_y") +
                      labs(title = "Impact of Absolute Quantification", x = "Concentration (µM)", y = "Value") +
                      theme_publication()
                      
                   ggsave(file.path(temp_dir, "bead_qc_stability.png"), plot = p_bead_qc, width = 10, height = 6, dpi = 300)
                   ggsave(file.path(temp_dir, "bead_count_impact.png"), plot = p_bead_impact, width = 10, height = 8, dpi = 300)
                   
                   # Store for report
                   rv$bead_qc_plot_obj <- p_bead_qc
                   rv$bead_impact_plot_obj <- p_bead_impact
                }
              }

              # --- Render RMarkdown report ---

              incProgress(0.7, detail = "Rendering apoptosis report...")
              
              abs_ic50_df_report <- NULL
              if (input$is_absolute_counting && !is.null(rv$absolute_survival_results) && nrow(rv$absolute_survival_results) > 0) {
                abs_ic50_df_report <- rv$absolute_survival_results %>%
                  mutate(`Abs. IC50 [95% CI] µM` = sprintf("%.2f [%.2f - %.2f]", IC50_uM, IC50_lower, IC50_upper)) %>%
                  dplyr::select(cell_line, `Abs. IC50 [95% CI] µM`)
              }

              rmarkdown_output_file <- file.path(temp_dir, report_filename)


              rmarkdown::render(
                input = "facs_report.Rmd",
                output_file = rmarkdown_output_file,
                output_dir = temp_dir, # Ensure output goes to temp_dir

                params = list(
                  analysis_settings = control_summary,
                  ic50_table = ic50_ld50_combined_report, # Use combined table
                  abs_ic50_table = abs_ic50_df_report, 

                  ld50_table = NULL, # Deprecated, now in combined table

                  qc_table = rv$qc_data,
                  advanced_metrics = rv$advanced_metrics,
                  results_raw = rv$results,
                  cell_counts = rv$cell_counts,
                  file_metadata = metadata_summary,
                                    ic50_stats = ic50_anova_text,
                                    viability_plot = plot_viability, # Pass locally generated plots
                                    death_plot = plot_death,
                                    abs_survival_plot = plot_abs_survival, # NEW
                                    comp_matrices = final_comp_matrices, # Pass the synchronized list
                                    spill_matrices = final_spill_matrices, # Pass the synchronized spillover list
                                    auto_comp_matrix = rv$comp_matrix, # Pass automatic inverse matrix
                                    auto_spill_matrix = rv$spill_matrix, # Pass automatic spillover matrix
                                    comp_plot = rv$comp_plot_obj, # NEW: Pass the initial comp visualization
                                    ic50_ld50_plot = plot_ic50_ld50,
                                    quadrant_plot = plot_quadrant,
                                    gate_summary = gate_summary_report,
                                    gate_plots = rv$gate_plots,
                                    gate_review_plots = rv$gate_review_plots, # NEW: Pass combined plots
                  singlet_plots = rv$singlet_gate_plots,
                  annexin_plots = rv$annexin_gate_plots,
                  bead_gate_plots = rv$bead_gate_plots, # NEW
                  bead_qc_plot = rv$bead_qc_plot_obj, # NEW
                  bead_impact_plot = rv$bead_impact_plot_obj, # NEW
                  manual_comp_preview_plots = rv$manual_comp_preview_snapshots, # NEW: Pass captured manual comp preview plots
                  outlier_flags = rv$outlier_flags, # NEW: Pass detected outliers
                  posthoc_results = rv$posthoc_results, # NEW: Pass stats
                  excluded_data = rv$manual_exclusions, # NEW: Pass exclusion list
                  is_absolute_counting = input$is_absolute_counting # NEW
                ),
                envir = new.env(parent = globalenv())
              )
            } else {
              # --- WORKFLOW 2: Generic Report ---

              incProgress(0.1, detail = "Saving generic data tables...")


              if (!is.null(rv$generic_results) && nrow(rv$generic_results) > 0) {
                write_csv(rv$generic_results, file.path(temp_dir, "generic_population_percentages.csv"))
              }

              if (!is.null(rv$generic_ec50_results) && nrow(rv$generic_ec50_results) > 0) {
                write_csv(rv$generic_ec50_results, file.path(temp_dir, "generic_EC50_results.csv"))
              }

              if (length(rv$generic_raw_intensity_data) > 0) {
                # Combine list of data.frames into one and save

                raw_intensity_df <- do.call(rbind, rv$generic_raw_intensity_data)

                write_csv(raw_intensity_df, file.path(temp_dir, "generic_raw_intensity_data.csv"))
              }


              # Prepare Metadata

              metadata_summary <- NULL

              if (!is.null(rv$metadata) && nrow(rv$metadata) > 0) {
                metadata_summary <- rv$metadata %>%
                  dplyr::select(name, cell_line, treatment_full, concentration_uM, replicate)

                colnames(metadata_summary)[1] <- "filename"

                write_csv(metadata_summary, file.path(temp_dir, "file_metadata.csv"))
              }


              # Prepare Gate Summary

              generic_gate_summary_report <- NULL

              if (length(rv$thresholds) > 0) {
                generic_gate_summary_report <- data.frame()

                for (line in names(rv$thresholds)) {
                  th <- rv$thresholds[[line]]

                  if (!is.null(th$generic_gate_list)) {
                    # Format: "Channel1 (>100), Channel2 (>500)"

                    gates_str <- paste(sapply(th$generic_gate_list, function(g) paste0(g$channel, " (> ", g$threshold, ")")), collapse = "; ")

                    generic_gate_summary_report <- rbind(generic_gate_summary_report, data.frame(
                      cell_line = line,
                      defined_gates = gates_str,
                      stringsAsFactors = FALSE
                    ))
                  }
                }

                if (!is.null(generic_gate_summary_report) && nrow(generic_gate_summary_report) > 0) {
                  write_csv(generic_gate_summary_report, file.path(temp_dir, "gate_summary.csv"))
                }
              }
              
              # Save Bead Plots
              if (input$is_absolute_counting && length(rv$bead_gate_plots) > 0) {
                for (line in names(rv$bead_gate_plots)) {
                  ggsave(file.path(temp_dir, paste0("bead_gate_", line, ".png")), plot = rv$bead_gate_plots[[line]], width = 8, height = 6, dpi = 300)
                }
              }


              incProgress(0.4, detail = "Generating generic plots...")


              # --- 1. Population Breakdown Plot (All) ---


              pop_breakdown_plot <- NULL


              if (!is.null(rv$generic_results)) {
                # Re-generate plot to ensure correct sorting and clean environment


                pop_cols <- names(rv$generic_results)[startsWith(names(rv$generic_results), "pct_")]


                plot_data <- rv$generic_results %>%
                  pivot_longer(cols = all_of(pop_cols), names_to = "Population", values_to = "Percentage") %>%
                  mutate(Sample_ID = paste(cell_line, concentration_uM, replicate, sep = "_"), Population = gsub("pct_", "", Population))


                # Sort X-axis


                unique_samples <- plot_data %>%
                  dplyr::select(Sample_ID, cell_line, concentration_uM, replicate) %>%
                  distinct() %>%
                  arrange(cell_line, concentration_uM, replicate)


                plot_data$Sample_ID <- factor(plot_data$Sample_ID, levels = unique_samples$Sample_ID)


                pop_colors <- get_color_palette(length(unique(plot_data$Population)))


                names(pop_colors) <- unique(plot_data$Population)


                pop_breakdown_plot <- ggplot(plot_data, aes(x = Sample_ID, y = Percentage, fill = Population)) +
                  geom_bar(stat = "identity", position = "stack", color = "black", linewidth = 0.3) +
                  scale_fill_manual(values = pop_colors) +
                  labs(title = "Population Breakdown", x = "Sample", y = "Percentage (%)", fill = "Population") +
                  theme_publication() +
                  theme(plot.title = element_text(size = 14, face = "bold"))


                ggsave(file.path(temp_dir, "generic_population_breakdown.png"), plot = pop_breakdown_plot, width = 12, height = 8, dpi = 300)


                ggsave(file.path(temp_dir, "generic_population_breakdown.svg"), plot = pop_breakdown_plot, width = 12, height = 8)
              }


              # --- 2. Fluorescence Intensity Plots (Loop all channels) ---


              intensity_plots_list <- list()


              if (length(rv$generic_raw_intensity_data) > 0) {
                # Combine and filter for valid channels only


                all_raw_data <- do.call(rbind, rv$generic_raw_intensity_data)


                unique_channels <- unique(all_raw_data$Channel)


                for (ch in unique_channels) {
                  ch_data <- all_raw_data %>%
                    filter(Channel == ch) %>%
                    mutate(Sample_ID = paste(CellLine, Concentration, Replicate, sep = "_"))


                  if (nrow(ch_data) == 0) next


                  # Sort X-axis


                  unique_samples_ch <- ch_data %>%
                    dplyr::select(Sample_ID, CellLine, Concentration, Replicate) %>%
                    distinct() %>%
                    arrange(CellLine, Concentration, Replicate)


                  ch_data$Sample_ID <- factor(ch_data$Sample_ID, levels = unique_samples_ch$Sample_ID)


                  cell_colors <- get_color_palette(length(unique(ch_data$CellLine)))


                  names(cell_colors) <- unique(ch_data$CellLine)


                  p_int <- ggplot(ch_data, aes(x = Sample_ID, y = Intensity, fill = CellLine)) +
                    geom_violin(trim = FALSE, alpha = 0.7) +
                    geom_boxplot(width = 0.1, fill = "white", color = "black", outlier.shape = NA) +
                    scale_fill_manual(values = cell_colors) +
                    scale_y_continuous(labels = custom_log_labels) +
                    labs(title = paste("Intensity:", ch), x = "Sample", y = "Intensity", fill = "Cell Line") +
                    theme_publication() +
                    theme(legend.position = "right")


                  # Save


                  safe_ch_name <- gsub("[^a-zA-Z0-9]", "_", ch)


                  ggsave(file.path(temp_dir, paste0("generic_intensity_", safe_ch_name, ".png")), plot = p_int, width = 12, height = 8, dpi = 300)


                  ggsave(file.path(temp_dir, paste0("generic_intensity_", safe_ch_name, ".svg")), plot = p_int, width = 12, height = 8)


                  intensity_plots_list[[ch]] <- p_int
                }
              }


              # --- 3. Dose-Response Plots (Loop all populations) ---


              dr_plots_list <- list()


              if (!is.null(rv$generic_plot_data_dr)) {
                unique_pops <- unique(rv$generic_plot_data_dr$Population)


                for (pop in unique_pops) {
                  pop_data <- rv$generic_plot_data_dr %>% filter(Population == pop)
                  
                  # Ensure pop_data is finite
                  pop_data <- pop_data %>% filter(is.finite(mean_pop))

                  pred_data <- if(!is.null(rv$generic_predicted_curves_dr)) {
                      rv$generic_predicted_curves_dr %>% filter(Population == pop)
                  } else {
                      data.frame()
                  }


                  if (nrow(pop_data) == 0) next


                  cell_lines <- unique(pop_data$cell_line)


                  line_colors <- get_color_palette(length(cell_lines))


                  names(line_colors) <- cell_lines


                  conc_range <- pop_data$concentration_uM


                  breaks <- get_smart_breaks(conc_range)


                  pop_clean <- gsub("pct_", "", pop)


                  p_dr <- ggplot() +
                    geom_point(data = pop_data, aes(x = concentration_uM, y = mean_pop, color = cell_line), size = 4, shape = 21, fill = "white", stroke = 1.5)
                    
                  if ("se_pop" %in% names(pop_data) && any(!is.na(pop_data$se_pop))) {
                    p_dr <- p_dr + geom_errorbar(data = pop_data, aes(x = concentration_uM, ymin = mean_pop - se_pop, ymax = mean_pop + se_pop, color = cell_line), width = 0.15, linewidth = 1)
                  }
                  
                  if (!is.null(pred_data) && nrow(pred_data) > 0) {
                    p_dr <- p_dr + geom_line(data = pred_data, aes(x = concentration_uM, y = predicted_pop, color = cell_line), linewidth = 1.5, alpha = 0.9)
                  }
                  
                  p_dr <- p_dr +
                    scale_color_manual(values = line_colors) +
                    scale_x_continuous(trans = pseudo_log_trans_for_breaks(), breaks = breaks, labels = format_conc_labels) +
                    labs(title = paste("DR Curve:", pop_clean), x = "Concentration (µM)", y = "% Positive", color = "Cell Line") +
                    theme_publication() +
                    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))


                  # Save


                  safe_pop_name <- gsub("[^a-zA-Z0-9]", "_", pop_clean)


                  ggsave(file.path(temp_dir, paste0("generic_dr_", safe_pop_name, ".png")), plot = p_dr, width = 12, height = 8, dpi = 300)


                  ggsave(file.path(temp_dir, paste0("generic_dr_", safe_pop_name, ".svg")), plot = p_dr, width = 12, height = 8)


                  dr_plots_list[[pop_clean]] <- p_dr
                }
              }
              
              # --- 4. Bead QC Plots ---
              if (input$is_absolute_counting) {
                 if (!is.null(rv$generic_bead_qc_plot_obj)) {
                    ggsave(file.path(temp_dir, "generic_bead_qc.png"), plot = rv$generic_bead_qc_plot_obj, width = 10, height = 6, dpi = 300)
                 }
                 if (!is.null(rv$generic_bead_impact_plot_obj)) {
                    ggsave(file.path(temp_dir, "generic_bead_impact.png"), plot = rv$generic_bead_impact_plot_obj, width = 10, height = 8, dpi = 300)
                 }
              }


              incProgress(0.7, detail = "Rendering generic report...")


              # Use analysis settings for the generic report


              control_settings <- data.frame(
                control_concentration_uM = rv$control_concentration,
                compensation_applied = rv$comp_calculated,
                normalization_applied = input$use_normalization
              )


              rmarkdown::render(
                input = "generic_report.Rmd",
                output_file = file.path(temp_dir, paste0("Generic_Analysis_Report_", Sys.Date(), ".html")),
                params = list(
                  ec50_table = rv$generic_ec50_results,
                  results_raw = rv$generic_results,
                  pop_breakdown_plot = pop_breakdown_plot,
                  intensity_plots = intensity_plots_list, # Pass list


                  dr_plots = dr_plots_list, # Pass list


                  analysis_settings = control_settings,
                  file_metadata = metadata_summary,
                  gate_summary = generic_gate_summary_report,
                  gate_plots = rv$gate_plots, # Pass individual FSC/SSC plots
                  singlet_plots = rv$singlet_gate_plots, # Pass individual Singlet plots
                  marker_plots = rv$generic_histogram_plots, # Pass individual marker histograms
                  gate_review_plots = rv$gate_review_plots, # Keep for UI parity if needed
                  bead_gate_plots = rv$bead_gate_plots, # NEW
                  bead_qc_plot = rv$bead_qc_plot_obj, # NEW
                  bead_impact_plot = rv$bead_impact_plot_obj, # NEW
                  comp_matrices = final_comp_matrices, # Pass the synchronized list
                  spill_matrices = final_spill_matrices, # Pass the synchronized spillover list
                  auto_comp_matrix = rv$comp_matrix, # Pass automatic inverse matrix
                  auto_spill_matrix = rv$spill_matrix, # Pass automatic spillover matrix
                  comp_plot = rv$comp_plot_obj, # NEW: Pass the initial comp visualization
                  manual_comp_preview_plots = rv$manual_comp_preview_snapshots, # NEW: Pass captured manual comp preview plots
                  posthoc_results = rv$generic_posthoc_results, # NEW: Pass Tukey results (Generic)
                  excluded_data = rv$generic_manual_exclusions, # NEW: Use generic-specific exclusions
                  outlier_flags = rv$outlier_flags, # NEW: Pass outliers
                  cell_counts = rv$cell_counts, # NEW: Pass cell counts
                  is_absolute_counting = input$is_absolute_counting # NEW
                ),
                envir = new.env(parent = globalenv())
              )
            }


            # Create ZIP file from all files in the export directory

            incProgress(0.9, detail = "Creating ZIP archive...")

            zip::zip(zipfile = file, files = list.files(temp_dir), root = temp_dir)


            # Cleanup

            unlink(temp_dir, recursive = TRUE)
          },
          error = function(e) {
            showNotification(paste("Failed to create ZIP file:", e$message), type = "error", duration = 15)

            message("ZIP creation error:", e$message)
          }
        )
      })
    },
    contentType = "application/zip"
  )
}

# ============================================================================
# ============================================================================
# LAUNCH APP
# ============================================================================
# ============================================================================

shinyApp(ui = ui, server = server)

