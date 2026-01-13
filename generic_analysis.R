# ============================================================================
# GENERIC MULTI-CHANNEL ANALYSIS WORKFLOW MODULE
# ============================================================================

# --- UI Function ---
# Returns the UI components for the generic gating tab.
generic_gating_ui <- function(rv, available_fluor_channels) {
  if (is.null(rv$metadata) || is.null(rv$control_concentration)) {
    return(NULL)
  }

  cell_lines <- unique(rv$metadata$cell_line)
  if (rv$current_cell_line_index > length(cell_lines)) {
    return(div(
      class = "alert alert-success alert-custom",
      h4(icon("check-circle"), " Gating Complete!"),
      p("All cell lines have been gated. Go to the Results tab and click 'Start Analysis'."),
      if (rv$current_cell_line_index > 1) {
        actionButton("prev_cell_line_generic", "← Back to Last Cell Line", class = "btn-secondary btn-sm")
      }
    ))
  }

  current_line <- cell_lines[rv$current_cell_line_index]

  # --- Top Navigation (Back to Previous Cell Line) ---
  top_nav <- if (rv$current_cell_line_index > 1) {
    div(
      style = "margin-bottom: 15px;",
      actionButton("prev_cell_line_generic", "← Back to Previous Cell Line",
        class = "btn-secondary btn-sm", icon = icon("chevron-left")
      )
    )
  } else {
    NULL
  }

  # --- UI for Gating Steps ---

  # Step 0: Bead Gate
  if (rv$gating_step == "bead_gate") {
    return(tagList(
      top_nav,
      div(class = "alert alert-danger alert-custom", h5(icon("bullseye"), " Step 0: Bead Gating - ", strong(current_line))),
      p("Circle the high-fluorescence beads. These will be EXCLUDED from cell counts.", style = "margin-bottom: 0;"),
      div(
        style = "background: #fff3cd; padding: 10px; border-radius: 5px; margin-top: 10px;",
        p(icon("mouse-pointer"), " Click on plot to add points. Close polygon when done."),
        actionButton("clear_polygon_bead", "Clear Points", class = "btn-warning btn-sm", icon = icon("eraser")),
        actionButton("close_polygon_bead", "Close Polygon", class = "btn-success btn-sm", icon = icon("check-circle"))
      ),
      plotOutput("bead_plot", height = "450px", click = "bead_click"),
      div(class = "alert alert-secondary", verbatimTextOutput("bead_stats")),
      hr(),
      actionButton("next_to_fsc", "Next: FSC/SSC Gate →", class = "btn-primary btn-lg btn-block", icon = icon("arrow-right"))
    ))
  }

  # Step 1: FSC/SSC
  if (rv$gating_step == "fsc_ssc") {
    return(tagList(
      top_nav,
      div(class = "alert alert-info", h5(icon("circle"), " Step 1/3: FSC/SSC Gate - ", strong(current_line))),
      div(
        style = "background: #fff3cd; padding: 10px; border-radius: 5px; margin-top: 10px;",
        p(icon("mouse-pointer"), " Click on plot to add points. Close polygon when done."),
        actionButton("clear_polygon_fsc", "Clear Points", class = "btn-warning btn-sm", icon = icon("eraser")),
        actionButton("close_polygon_fsc", "Close Polygon", class = "btn-success btn-sm", icon = icon("check-circle"))
      ),
      plotOutput("fsc_ssc_plot", height = "450px", click = "fsc_ssc_click"),
      div(class = "alert alert-secondary", verbatimTextOutput("fsc_ssc_stats")),
      hr(),
      actionButton("next_to_singlets", "Next: Singlet Gate →", class = "btn-primary btn-lg btn-block", icon = icon("arrow-right"))
    ))
  }

  # Step 2: Singlets
  else if (rv$gating_step == "singlets") {
    return(tagList(
      top_nav,
      div(class = "alert alert-info", h5(icon("circle"), " Step 2/3: Singlet Discrimination - ", strong(current_line))),
      div(
        style = "background: #fff3cd; padding: 10px; border-radius: 5px; margin-top: 10px;",
        p(icon("mouse-pointer"), " Click on plot to add points. Close polygon when done."),
        actionButton("clear_polygon_singlet", "Clear Points", class = "btn-warning btn-sm", icon = icon("eraser")),
        actionButton("close_polygon_singlet", "Close Polygon", class = "btn-success btn-sm", icon = icon("check-circle"))
      ),
      plotOutput("singlet_plot", height = "450px", click = "singlet_click"),
      div(class = "alert alert-secondary", verbatimTextOutput("singlet_stats")),
      hr(),
      fluidRow(
        column(6, actionButton("back_to_fsc", "← Back: FSC/SSC", class = "btn-secondary btn-lg btn-block", icon = icon("arrow-left"))),
        column(6, actionButton("next_to_target_channel", "Next: Target Channels →", class = "btn-primary btn-lg btn-block", icon = icon("arrow-right")))
      )
    ))
  }

  # Step 3: Multi-Channel Gating
  else if (rv$gating_step == "target_channel") {
    return(tagList(
      top_nav,
      div(class = "alert alert-info", h5(icon("circle"), " Step 3/3: Define Target Channel Gates - ", strong(current_line))),
      p("Add one or more channels to define your populations. For each channel, a histogram will appear and you can set a threshold for positivity."),

      # Dynamic UI for each channel will be rendered here
      uiOutput("generic_gate_channels_ui"),
      hr(style = "margin-top: 20px;"),
      actionButton("add_generic_gate_channel", "Add Channel Gate", icon = icon("plus"), class = "btn-info btn-sm"),
      hr(style = "margin-bottom: 20px;"),
      fluidRow(
        column(6, actionButton("back_to_singlet_generic", "← Back: Singlets", class = "btn-secondary btn-lg btn-block", icon = icon("arrow-left"))),
        column(6, actionButton("save_gate_generic", "Save & Next Cell Line ✓", class = "btn-success btn-lg btn-block", icon = icon("check")))
      )
    ))
  }
}

# --- Server Function ---
generic_gating_server <- function(input, output, session, rv, available_fluor_channels) {
  # Navigation back to previous cell line
  observeEvent(input$prev_cell_line_generic, {
    req(rv$current_cell_line_index > 1)

    # 1. Clear current cell line's generic observers to prevent leaks
    for (obs_set in rv$generic_observers) {
      if (!is.null(obs_set$remove_obs)) obs_set$remove_obs$destroy()
      if (!is.null(obs_set$channel_obs)) obs_set$channel_obs$destroy()
      if (!is.null(obs_set$threshold_obs)) obs_set$threshold_obs$destroy()
    }
    rv$generic_observers <- list()

    # 2. Move back index
    rv$current_cell_line_index <- rv$current_cell_line_index - 1
    cell_lines <- unique(rv$metadata$cell_line)
    prev_line <- cell_lines[rv$current_cell_line_index]

    # 3. Load previous thresholds if they exist
    prev_gate_info <- rv$thresholds[[prev_line]]
    if (!is.null(prev_gate_info)) {
      rv$temp_fsc_ssc_gates <- prev_gate_info$fsc_ssc
      rv$temp_singlet_gates <- prev_gate_info$singlets
      rv$generic_gate_defs <- prev_gate_info$generic_gate_list %||% list()

      # Re-initialize the gate counter based on existing IDs to avoid collisions
      if (length(rv$generic_gate_defs) > 0) {
        max_id <- max(sapply(rv$generic_gate_defs, function(g) g$id))
        rv$generic_gate_counter <- max_id
      }

      # Start at Step 3 (channels) since FSC/SSC and Singlets are likely fine
      rv$gating_step <- "target_channel"

      # Re-create observers for the loaded gates
      lapply(rv$generic_gate_defs, function(gate_def) {
        create_generic_gate_observers(gate_def$id)
      })
    } else {
      rv$gating_step <- "fsc_ssc"
      rv$generic_gate_defs <- list()
    }

    rv$polygon_points <- data.frame(x = numeric(), y = numeric())
    showNotification(paste("Returning to:", prev_line), type = "message")
  })

  # Navigation from Singlets to Target Channel
  observeEvent(input$next_to_target_channel, {
    req(!input$is_apoptosis_assay, rv$current_gating_data_singlet)

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
    in_gate <- point.in.polygon(rv$current_gating_data_singlet$fsc_a, rv$current_gating_data_singlet$fsc_h, rv$polygon_points$x, rv$polygon_points$y) > 0
    rv$temp_gated_data_singlets <- rv$current_gating_data_singlet$data_cells[in_gate, ]
    if (nrow(rv$temp_gated_data_singlets) == 0) {
      showNotification("No singlets in gate! Adjust gate.", type = "error")
      return()
    }
    rv$polygon_points <- data.frame(x = numeric(), y = numeric())
    rv$gating_step <- "target_channel"
  })

  # Dynamic UI for the channel gates
  output$generic_gate_channels_ui <- renderUI({
    req(rv$gating_step == "target_channel", rv$temp_gated_data_singlets)

    if (length(rv$generic_gate_defs) == 0) {
      return(p("No channels defined. Click 'Add Channel Gate' to begin.", style = "color: #666; font-style: italic;"))
    }

    # Generate the UI for each defined gate
    lapply(rv$generic_gate_defs, function(gate_def) {
      gate_id <- gate_def$id

      # Dynamically get data for the selected channel
      trans <- logicleTransform(w = 0.5, t = 1000000, m = 4.5, a = 0)
      data_singlets <- rv$temp_gated_data_singlets
      target_channel_data <- tryCatch(
        {
          if (rv$comp_calculated && !is.null(rv$spill_matrix)) {
            ff_singlets <- flowFrame(exprs = data_singlets)
            compensated_fs <- compensate(ff_singlets, rv$spill_matrix)
            trans(compensated_fs@exprs[, gate_def$channel])
          } else {
            trans(data_singlets[, gate_def$channel])
          }
        },
        error = function(e) {
          NULL
        }
      )

      # Determine slider range
      if (!is.null(target_channel_data)) {
        min_val <- floor(min(target_channel_data, na.rm = TRUE))
        max_val <- ceiling(max(target_channel_data, na.rm = TRUE))
        start_val <- gate_def$threshold %||% median(target_channel_data, na.rm = T)
      } else {
        min_val <- 0
        max_val <- 5
        start_val <- 2.5
      }

      # Build the UI element
      div(
        style = "border: 1px solid #ddd; padding: 15px; margin-top: 10px; border-radius: 5px;",
        fluidRow(
          column(
            4,
            selectInput(paste0("generic_channel_select_", gate_id), "Channel:",
              choices = available_fluor_channels, selected = gate_def$channel
            ),
            actionButton(paste0("remove_generic_gate_", gate_id), "Remove", icon = icon("trash-alt"), class = "btn-danger btn-xs")
          ),
          column(
            8,
            plotOutput(paste0("generic_hist_plot_", gate_id), height = "200px"),
            sliderInput(paste0("generic_threshold_slider_", gate_id), "Positivity Threshold:",
              min = min_val, max = max_val, value = start_val, step = 0.05
            )
          )
        )
      )
    })
  })

  # --- FUNCTION TO CREATE GATE OBSERVERS (The safe pattern) ---
  create_generic_gate_observers <- function(gate_id) {
    # 1. Removal
    remove_obs <- observeEvent(input[[paste0("remove_generic_gate_", gate_id)]],
      {
        # Destroy self and siblings
        obs_set <- isolate(rv$generic_observers)[[as.character(gate_id)]]
        if (!is.null(obs_set)) {
          obs_set$remove_obs$destroy()
          obs_set$channel_obs$destroy()
          obs_set$threshold_obs$destroy()
        }

        # Update Defs
        current_defs <- isolate(rv$generic_gate_defs)
        rv$generic_gate_defs <- current_defs[sapply(current_defs, function(d) !isTRUE(d$id == gate_id))]

        # Update Observer List
        temp_obs <- isolate(rv$generic_observers)
        temp_obs[[as.character(gate_id)]] <- NULL
        rv$generic_observers <- temp_obs
      },
      ignoreNULL = TRUE,
      ignoreInit = TRUE,
      once = TRUE
    )

    # 2. Channel Selection Change
    channel_obs <- observeEvent(input[[paste0("generic_channel_select_", gate_id)]],
      {
        idx <- which(sapply(isolate(rv$generic_gate_defs), function(d) isTRUE(d$id == gate_id)))
        if (length(idx) == 1) {
          temp_defs <- isolate(rv$generic_gate_defs)
          temp_defs[[idx]]$channel <- input[[paste0("generic_channel_select_", gate_id)]]
          rv$generic_gate_defs <- temp_defs
        }
      },
      ignoreNULL = TRUE,
      ignoreInit = TRUE
    )

    # 3. Threshold Slider Change
    threshold_obs <- observeEvent(input[[paste0("generic_threshold_slider_", gate_id)]],
      {
        idx <- which(sapply(isolate(rv$generic_gate_defs), function(d) isTRUE(d$id == gate_id)))
        if (length(idx) == 1) {
          temp_defs <- isolate(rv$generic_gate_defs)
          temp_defs[[idx]]$threshold <- input[[paste0("generic_threshold_slider_", gate_id)]]
          rv$generic_gate_defs <- temp_defs
        }
      },
      ignoreNULL = TRUE,
      ignoreInit = TRUE
    )

    # Store in tracking list
    temp_obs <- isolate(rv$generic_observers)
    temp_obs[[as.character(gate_id)]] <- list(
      remove_obs = remove_obs,
      channel_obs = channel_obs,
      threshold_obs = threshold_obs
    )
    rv$generic_observers <- temp_obs

    # 4. Histogram Plot Rendering (Static name, but depends on inputs)
    output[[paste0("generic_hist_plot_", gate_id)]] <- renderPlot({
      # Use reactive inputs or fall back to defs
      selected_channel <- input[[paste0("generic_channel_select_", gate_id)]]
      current_threshold <- input[[paste0("generic_threshold_slider_", gate_id)]]

      if (is.null(selected_channel)) {
        idx <- which(sapply(isolate(rv$generic_gate_defs), function(d) isTRUE(d$id == gate_id)))
        if (length(idx) == 1) selected_channel <- rv$generic_gate_defs[[idx]]$channel
      }

      req(selected_channel)

      target_channel_data <- tryCatch(
        {
          current_line <- unique(rv$metadata$cell_line)[rv$current_cell_line_index]
          comp_mat_to_use <- NULL
          if (rv$comp_calculated) {
             if (!is.null(rv$spill_matrices[[current_line]])) {
               comp_mat_to_use <- rv$spill_matrices[[current_line]]
             } else {
               comp_mat_to_use <- rv$spill_matrix # Fallback to global
             }
          }
          
          if (!is.null(comp_mat_to_use)) {
            ff <- flowFrame(exprs = rv$temp_gated_data_singlets)
            cf <- compensate(ff, comp_mat_to_use)
            
            # Use manual robust transform for the live plot
            trans_obj <- logicleTransform(w = 0.5, t = 1000000, m = 4.5, a = 0)
            trans_obj(exprs(cf)[, selected_channel])
          } else {
            # Try to estimate even on uncompensated
            trans_obj <- logicleTransform(w = 0.5, t = 1000000, m = 4.5, a = 0)
            trans_obj(rv$temp_gated_data_singlets[, selected_channel])
          }
        },
        error = function(e) {
          return(NULL)
        }
      )

      req(target_channel_data)

      ggplot(data.frame(Intensity = target_channel_data), aes(x = Intensity)) +
        geom_histogram(aes(y = after_stat(density)), bins = 200, fill = "#56B4E9", color = "black", alpha = 0.7) +
        geom_density(color = "#0072B2", linewidth = 1) +
        list(if (!is.null(current_threshold)) geom_vline(xintercept = current_threshold, color = "#D55E00", linetype = "dashed", linewidth = 1.5)) +
        labs(x = "Fluorescence Intensity", y = "Density") +
        theme_publication() +
        theme(plot.title = element_text(size = 10))
    }) # This closing brace now correctly ends the renderPlot function
  }

  # Handle adding a new channel gate
  observeEvent(input$add_generic_gate_channel, {
    rv$generic_gate_counter <- rv$generic_gate_counter + 1
    new_id <- rv$generic_gate_counter

    new_gate <- list(id = new_id, channel = available_fluor_channels[1], threshold = NULL)
    rv$generic_gate_defs <- c(rv$generic_gate_defs, list(new_gate))

    # Create observers for the new gate
    create_generic_gate_observers(new_id)
  })

  # Navigation back from Target Channel to Singlets
  observeEvent(input$back_to_singlet_generic, {
    req(!input$is_apoptosis_assay)
    rv$gating_step <- "singlets"
    rv$polygon_points <- data.frame(x = numeric(), y = numeric())
  })

  # Save the list of generic gates
  observeEvent(input$save_gate_generic, {
    req(!input$is_apoptosis_assay, length(rv$generic_gate_defs) > 0)

    # Validate that all thresholds are set
    all_thresholds_set <- all(sapply(rv$generic_gate_defs, function(g) !is.null(g$threshold)))
    if (!all_thresholds_set) {
      showNotification("A threshold has not been set for all channels. Please adjust the sliders.", type = "error")
      return()
    }

    cell_line <- unique(rv$metadata$cell_line)[rv$current_cell_line_index]
    
    # Store Bead Gate in thresholds if active
    bead_gate_to_store <- NULL
    if (input$is_absolute_counting) {
      bead_gate_to_store <- rv$temp_bead_gates[[cell_line]]
    }

    # --- NEW: Smart Data-Driven Transformation (estimateLogicle) ---
    # We estimate the transform for all defined channels at once for this cell line
    defined_channels <- unique(sapply(rv$generic_gate_defs, function(g) g$channel))
    trans_list <- NULL
    
    if (length(defined_channels) > 0 && !is.null(rv$temp_gated_data_singlets)) {
      ff_for_trans <- flowFrame(exprs = rv$temp_gated_data_singlets)
      
      # Apply compensation to the estimate frame if needed
      comp_mat_to_use <- NULL
      if (rv$comp_calculated) {
         if (!is.null(rv$spill_matrices[[cell_line]])) {
           comp_mat_to_use <- rv$spill_matrices[[cell_line]]
         } else {
           comp_mat_to_use <- rv$spill_matrix
         }
      }
      
      if (!is.null(comp_mat_to_use)) {
        ff_for_trans <- compensate(ff_for_trans, comp_mat_to_use)
      }

      # Standard robust transform
      trans_obj <- logicleTransform(w = 0.5, t = 1000000, m = 4.5, a = 0)
      
      # Create a transformList manually for all defined channels
      t_list <- list()
      for(ch in defined_channels) t_list[[ch]] <- trans_obj
      trans_list <- transformList(defined_channels, t_list)
    }
    
    rv$thresholds[[cell_line]] <- list(
      bead_gate = bead_gate_to_store,
      fsc_ssc = rv$temp_fsc_ssc_gates,
      singlets = rv$temp_singlet_gates,
      generic_gate_list = rv$generic_gate_defs,
      logicle_transform = trans_list # Save the transform object
    )

    # --- Create a combined plot for the review tab ---
    gate_plots_list <- list()
    
    # 0. Bead Gate Plot (Saved during navigation)
    if (input$is_absolute_counting && !is.null(rv$bead_gate_plots[[cell_line]])) {
      gate_plots_list <- c(gate_plots_list, list(rv$bead_gate_plots[[cell_line]] + theme(plot.title = element_text(size = 10))))
    }

    # 1. FSC/SSC Plot
    if (!is.null(rv$current_gating_data_fsc$fsc_full) && !is.null(rv$current_gating_data_fsc$ssc_full)) {
        fsc_full <- rv$current_gating_data_fsc$fsc_full
        ssc_full <- rv$current_gating_data_fsc$ssc_full
        
        fsc_df_full <- data.frame(FSC = fsc_full, SSC = ssc_full)
        fsc_data_for_plot <- fsc_df_full[is.finite(fsc_df_full$FSC) & is.finite(fsc_df_full$SSC), ]
        
        if (nrow(fsc_data_for_plot) > 5000) fsc_data_for_plot <- fsc_data_for_plot[sample(nrow(fsc_data_for_plot), 5000), ]
        
        fsc_plot <- ggplot(fsc_data_for_plot, aes(x = FSC, y = SSC)) +
          geom_point(alpha = 0.1, size = 0.3) +
          geom_path(data = rv$temp_fsc_ssc_gates$polygon, aes(x = x, y = y), color = "red", linewidth = 0.5) +
          scale_x_continuous(labels = custom_log_labels) +
          scale_y_continuous(labels = custom_log_labels) +
          labs(title = paste("FSC/SSC -", cell_line)) +
          theme_publication() +
          theme(legend.position = "none", plot.title = element_text(size = 10))
        gate_plots_list <- c(gate_plots_list, list(fsc_plot))
    }

    # 2. Singlet Plot
    if (!is.null(rv$current_gating_data_singlet)) {
        sing_a <- rv$current_gating_data_singlet$fsc_a
        sing_h <- rv$current_gating_data_singlet$fsc_h
        
        if (!is.null(sing_a) && !is.null(sing_h)) {
            sing_df_full <- data.frame(FSC_A = sing_a, FSC_H = sing_h)
            singlet_data_for_plot <- sing_df_full[is.finite(sing_df_full$FSC_A) & is.finite(sing_df_full$FSC_H), ]
            
            if (nrow(singlet_data_for_plot) > 5000) singlet_data_for_plot <- singlet_data_for_plot[sample(nrow(singlet_data_for_plot), 5000), ]
            
            singlet_plot <- ggplot(singlet_data_for_plot, aes(x = FSC_A, y = FSC_H)) +
              geom_point(alpha = 0.1, size = 0.3) +
              geom_path(data = rv$temp_singlet_gates$polygon, aes(x = x, y = y), color = "red", linewidth = 0.5) +
              scale_x_continuous(labels = custom_log_labels) +
              scale_y_continuous(labels = custom_log_labels) +
              labs(title = paste("Singlets -", cell_line)) +
              theme_publication() +
              theme(legend.position = "none", plot.title = element_text(size = 10))
            gate_plots_list <- c(gate_plots_list, list(singlet_plot))
        }
    }

    # 3. Histogram Plots for each generic gate
    trans <- logicleTransform(w = 0.5, t = 1000000, m = 4.5, a = 0)
    for (gate_def in rv$generic_gate_defs) {
      selected_channel <- gate_def$channel
      current_threshold <- gate_def$threshold

      target_channel_data <- tryCatch(
        {
          current_line <- unique(rv$metadata$cell_line)[rv$current_cell_line_index]
          comp_mat_to_use <- NULL
          if (rv$comp_calculated) {
             if (!is.null(rv$spill_matrices[[current_line]])) {
               comp_mat_to_use <- rv$spill_matrices[[current_line]]
             } else {
               comp_mat_to_use <- rv$spill_matrix # Fallback to global
             }
          }
          
          raw_data_target <- rv$temp_gated_data_singlets[, selected_channel]
          
          if (!is.null(comp_mat_to_use)) {
            ff <- flowFrame(exprs = rv$temp_gated_data_singlets)
            cf <- compensate(ff, comp_mat_to_use)
            trans(exprs(cf)[, selected_channel])
          } else {
            trans(raw_data_target)
          }
        },
        error = function(e) {
          return(NULL)
        }
      )

      if (!is.null(target_channel_data)) {
        hist_df <- data.frame(Intensity = target_channel_data) %>%
          filter(is.finite(Intensity))
          
        if (nrow(hist_df) > 0) {
          hist_plot <- ggplot(hist_df, aes(x = Intensity)) +
            geom_histogram(aes(y = after_stat(density)), bins = 200, fill = "#56B4E9", color = "black", alpha = 0.7) +
            geom_density(color = "#0072B2", linewidth = 1) +
            (if (!is.null(current_threshold)) geom_vline(xintercept = current_threshold, color = "#D55E00", linetype = "dashed", linewidth = 1.5)) +
            labs(title = paste(selected_channel, "-", cell_line)) +
            theme_publication() +
            theme(legend.position = "none", plot.title = element_text(size = 10))
          gate_plots_list <- c(gate_plots_list, list(hist_plot))
          
          # Also store in individual list for report
          hist_key <- paste0(cell_line, "_", selected_channel)
          rv$generic_histogram_plots[[hist_key]] <- hist_plot
        }
      }
    }

    # Arrange all generated plots into a single grob
    if (length(gate_plots_list) > 0) {
      rv$gate_review_plots[[cell_line]] <- gridExtra::arrangeGrob(grobs = gate_plots_list, ncol = 3)
    }

    # Clear observers for the cell line we just finished
    for (obs_set in rv$generic_observers) {
      if (!is.null(obs_set$remove_obs)) obs_set$remove_obs$destroy()
      if (!is.null(obs_set$channel_obs)) obs_set$channel_obs$destroy()
      if (!is.null(obs_set$threshold_obs)) obs_set$threshold_obs$destroy()
    }
    rv$generic_observers <- list()

    # Advance to the next cell line and reset state
    rv$current_cell_line_index <- rv$current_cell_line_index + 1
    rv$gating_step <- if (input$is_absolute_counting) "bead_gate" else "fsc_ssc"
    rv$polygon_points <- data.frame(x = numeric(), y = numeric())
    rv$temp_fsc_ssc_gates <- NULL
    rv$temp_singlet_gates <- NULL
    rv$temp_gated_data_cells <- NULL
    rv$temp_gated_data_singlets <- NULL
    rv$generic_gate_defs <- list() # Reset for next cell line
  })
}

# --- Analysis Function ---
run_generic_analysis <- function(metadata, thresholds, control_concentration, comp_matrices, spill_matrix, comp_calculated, 
                                 is_absolute_counting = FALSE, bead_conc = 1000, bead_vol = 100, sample_vol = 1000, 
                                 temp_bead_gates = list(), bead_gate_channel = "BL1-A") {
  withProgress(message = "Running Multi-channel Generic Analysis...", value = 0, {
    trans <- logicleTransform(w = 0.5, t = 1000000, m = 4.5, a = 0)
    all_results_df <- data.frame()
    all_raw_intensity_data <- list() # To store raw intensity data for violin plots
    cell_counts_data <- data.frame() # NEW: Track cell counts
    errors <- character()

    for (i in 1:nrow(metadata)) {
      row <- metadata[i, ]
      cell_line <- row$cell_line

      incProgress(1 / nrow(metadata), detail = paste("Processing", basename(row$name)))

      if (is.na(cell_line) || !cell_line %in% names(thresholds)) {
        errors <<- c(errors, paste(row$name, ": No thresholds found for cell line. Skipping."))
        next
      }

      current_thresholds <- thresholds[[cell_line]]

      # Check if the gate is of the correct type (generic multi-channel)
      if (is.null(current_thresholds$generic_gate_list) || length(current_thresholds$generic_gate_list) == 0) {
        errors <<- c(errors, paste(row$name, ": No multi-channel generic gates defined. Please re-gate in the correct mode."))
        next
      }

      tryCatch(
        {
          fs <- read.FCS(row$datapath)
          data_raw <- exprs(fs)
          total_events <- nrow(data_raw)
          
          # --- NEW: Step 0: Bead Gating (if enabled) ---
          bead_count <- NA
          if (is_absolute_counting && !is.null(bead_gate_channel)) {
            bead_gate <- temp_bead_gates[[cell_line]]
            if (!is.null(bead_gate)) {
              trans_bead <- logicleTransform(w = 0.5, t = 1000000, m = 4.5, a = 0)
              in_bead_gate <- point.in.polygon(
                data_raw[, "FSC-A"],
                trans_bead(data_raw[, bead_gate_channel]),
                bead_gate$polygon$x, bead_gate$polygon$y
              ) > 0
              bead_count <- sum(in_bead_gate)
              # Exclude beads from further cell analysis
              data_raw <- data_raw[!in_bead_gate, ]
            }
          }

          # Gate 1: FSC/SSC
          poly_fsc <- current_thresholds$fsc_ssc$polygon
          in_gate1 <- point.in.polygon(data_raw[, "FSC-A"], data_raw[, "SSC-A"], poly_fsc$x, poly_fsc$y) > 0
          data_cells <- data_raw[in_gate1, ]
          cells_after_fsc <- nrow(data_cells)
          if (nrow(data_cells) == 0) {
            errors <<- c(errors, paste(row$name, ": No cells after FSC/SSC gate."))
            next
          }

          # Gate 2: Singlets
          poly_singlet <- current_thresholds$singlets$polygon
          in_gate2 <- point.in.polygon(data_cells[, "FSC-A"], data_cells[, "FSC-H"], poly_singlet$x, poly_singlet$y) > 0
          data_singlets <- data_cells[in_gate2, ]
          singlets_count <- nrow(data_singlets)
          if (nrow(data_singlets) == 0) {
            errors <<- c(errors, paste(row$name, ": No singlets after singlet gate."))
            next
          }
          
          # Calculation Factor for Absolute Counting
          abs_factor <- NA
          if (is_absolute_counting && !is.na(bead_count) && bead_count > 0) {
             # Formula: (Cell Count / Bead Count) * (Bead Vol / Sample Vol) * Bead Conc
             abs_factor <- (bead_vol / sample_vol) * bead_conc / bead_count
          }

          # NEW: Store cell counts at each gating step
          cell_counts_data <- rbind(cell_counts_data, data.frame(
            cell_line = cell_line,
            concentration_uM = row$concentration_uM,
            replicate = row$replicate,
            total_events = total_events,
            bead_count = bead_count,
            after_fsc_ssc = cells_after_fsc,
            after_singlets = singlets_count,
            viable_count = NA, # Not applicable in generic mode
            dead_count = NA,   # Not applicable in generic mode
            stringsAsFactors = FALSE
          ))

          # Apply compensation if enabled - NOW PER CELL LINE
          compensated_data_singlets <- data_singlets
          if (comp_calculated) {
            # Determine correct matrix
            comp_mat_use <- NULL
            if (!is.null(comp_matrices) && !is.null(comp_matrices[[cell_line]])) {
              comp_mat_use <- comp_matrices[[cell_line]]
            }
            
            if (!is.null(comp_mat_use)) {
               ff_singlets <- flowFrame(exprs = data_singlets)
               compensated_fs <- compensate(ff_singlets, comp_mat_use)
               compensated_data_singlets <- exprs(compensated_fs)
            }
          }

          # Ensure all defined channels exist in the data
          defined_channels <- sapply(current_thresholds$generic_gate_list, `[[`, "channel")
          if (!all(defined_channels %in% colnames(compensated_data_singlets))) {
            missing_channels <- defined_channels[!defined_channels %in% colnames(compensated_data_singlets)]
            errors <<- c(errors, paste(row$name, ": Missing channels in FCS file:", paste(missing_channels, collapse = ", ")))
            next
          }

          # Step 3: Multi-Channel Gating & Population Calculation
          population_flags <- data.frame(row.names = 1:nrow(compensated_data_singlets))
          gate_names <- character()
          
          # REUSE the stored transform from gating
          trans_list <- current_thresholds$logicle_transform

          for (gate_def in current_thresholds$generic_gate_list) {
            channel <- gate_def$channel
            threshold <- gate_def$threshold

            # Get transformed channel data
            # If we have a transformList, apply it. Else fallback to default.
            if (!is.null(trans_list) && inherits(trans_list, "transformList")) {
               # We need to transform the compensated flowFrame or just the channel
               # Using flowFrame is more robust for transformList
               ff_comp <- flowFrame(exprs = compensated_data_singlets)
               ff_trans <- transform(ff_comp, trans_list)
               channel_data <- exprs(ff_trans)[, channel]
            } else {
               channel_data <- trans(compensated_data_singlets[, channel])
            }

            # Classify cells as positive or negative for this channel
            population_flags[[paste0(channel, "_pos")]] <- channel_data > threshold
            gate_names <- c(gate_names, channel)
          }

          # Generate all 2^N population combinations
          num_channels <- length(gate_names)
          if (num_channels == 0) {
            errors <<- c(errors, paste(row$name, ": No channels defined for generic gate."))
            next
          }

          # Initialize an empty data frame for this sample's results
          sample_population_results <- data.frame(
            cell_line = cell_line,
            concentration_uM = row$concentration_uM,
            replicate = row$replicate
          )

          # Iterate through all combinations of positive/negative for N channels
          # If num_channels is 1, `expand.grid` just returns a single column of TRUE/FALSE
          combinations <- do.call(expand.grid, rep(list(c(TRUE, FALSE)), num_channels))
          colnames(combinations) <- paste0(gate_names, "_pos_flag") # Name them by channel_pos_flag

          total_singlets <- nrow(compensated_data_singlets)
          if (total_singlets == 0) {
            errors <<- c(errors, paste(row$name, ": No cells after singlet gate and compensation."))
            next
          }

          for (k in 1:nrow(combinations)) {
            combo <- combinations[k, , drop = FALSE]

            # Build the filter condition for this combination
            filter_condition <- rep(TRUE, total_singlets)
            pop_label_parts <- character()

            for (j in 1:num_channels) {
              channel_name <- gate_names[j]
              is_pos <- combo[[paste0(channel_name, "_pos_flag")]]

              # Use the actual population_flags
              current_flag_col <- population_flags[[paste0(channel_name, "_pos")]]

              if (is_pos) {
                filter_condition <- filter_condition & current_flag_col
                pop_label_parts <- c(pop_label_parts, paste0(channel_name, "+"))
              } else {
                filter_condition <- filter_condition & !current_flag_col
                pop_label_parts <- c(pop_label_parts, paste0(channel_name, "-"))
              }
            }

            pop_name <- paste(pop_label_parts, collapse = "_")
            count_population <- sum(filter_condition, na.rm = TRUE)
            pct_population <- (count_population / total_singlets) * 100

            sample_population_results[[paste0("pct_", pop_name)]] <- pct_population
            
            # Calculate Absolute Concentration for this population
            if (!is.na(abs_factor)) {
               sample_population_results[[paste0("abs_", pop_name)]] <- count_population * abs_factor
            }
          }

          all_results_df <- dplyr::bind_rows(all_results_df, sample_population_results)

          # --- Store Raw Intensity Data for Plots ---        # For each channel that was gated, store all transformed intensities for cells that passed singlet gate
          # This will be a data frame with Intensity, Channel, Sample, CellLine, etc.
          sample_intensity_df <- data.frame(
            Intensity = numeric(),
            Channel = character(),
            Sample = character(),
            CellLine = character(),
            Concentration = numeric(),
            Replicate = character(),
            stringsAsFactors = FALSE
          )

          for (channel_name in defined_channels) {
            if (channel_name %in% colnames(compensated_data_singlets)) {
              channel_intensity <- data.frame(
                Intensity = trans(compensated_data_singlets[, channel_name]),
                Channel = channel_name,
                Sample = row$name,
                CellLine = cell_line,
                Concentration = row$concentration_uM,
                Replicate = row$replicate,
                stringsAsFactors = FALSE
              )
              sample_intensity_df <- rbind(sample_intensity_df, channel_intensity)
            }
          }
          all_raw_intensity_data[[row$name]] <- sample_intensity_df
        },
        error = function(e) {
          errors <<- c(errors, paste(row$name, ":", e$message))
        }
      )
    } # END loop through metadata

    if (length(errors) > 0) {
      showNotification(paste("Some files failed during analysis:", paste(errors, collapse = "\n")), type = "warning", duration = 15)
    }

    if (nrow(all_results_df) == 0) {
      showNotification("Generic analysis failed to produce results.", type = "error")
      return(NULL)
    }

    return(list(
      results = all_results_df,
      raw_intensity_data = all_raw_intensity_data,
      cell_counts = cell_counts_data
    ))
  }) # END withProgress
} # END run_generic_analysis function

