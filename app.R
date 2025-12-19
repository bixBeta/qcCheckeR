# RNA-Seq Editor — Fully Integrated with Reusable Modules
# With Session Storage and Email Notifications

# Environment setup with package installation
required_packages <- c(
  "shiny",
  "bslib",
  "jsonlite",
  "reactable",
  "dplyr",
  "tidyr",
  "tibble",
  "shinycssloaders",
  "blastula",
  "glue",
  "shinyjs",
  "plotly",
  "ggplot2",
  "DESeq2",
  "matrixStats",
  "shinyjqui",
  "viridisLite",
  "webshot2",
  "colourpicker"
)

# Install missing packages
for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    if (pkg == "DESeq2") {
      # DESeq2 is from Bioconductor
      if (!requireNamespace("BiocManager", quietly = TRUE)) {
        install.packages("BiocManager")
      }
      BiocManager::install("DESeq2", update = FALSE, ask = FALSE)
      library(pkg, character.only = TRUE)
    } else {
      install.packages(pkg)
      library(pkg, character.only = TRUE)
    }
  }
}

# =================================================
# SOURCE THE REUSABLE MODULES
# =================================================
source("module_session_storage.R")
source("module_email_notifications.R")

# =================================================
# SESSION CONFIGURATION
# =================================================
options(shiny.autoreload = FALSE)
if (interactive()) {
  options(shiny.launch.browser = TRUE)
}

# =================================================
# UI MODULES (Your existing table modules)
# =================================================
editableTableUI <- function(id) {
  ns <- NS(id)
  card(
    card_header("Editable Metadata Table"),
    card_body(
      withSpinner(
        reactableOutput(ns("table")),
        type = 4,
        color = "#0dcaf0",
        size = 1
      ),
      br(),
      div(
        style = "margin-top: 15px; display: flex; gap: 10px; align-items: center;",
        actionButton(
          ns("reset"),
          "Reset to Original",
          class = "btn-outline-secondary"
        ),
        uiOutput(ns("undo_ui")),
        downloadButton(
          ns("download"),
          "Download CSV",
          class = "btn-outline-primary"
        )
      )
    )
  )
}

linkedTableUI <- function(id, title = "Count Matrix") {
  ns <- NS(id)
  card(
    card_header(title),
    card_body(
      navset_card_tab(
        id = ns("counts_tabs"),
        nav_panel(
          "Raw Counts",
          plotOutput(ns("boxplot_raw"), height = "500px")
        ),
        nav_panel(
          "VST Counts",
          uiOutput(ns("boxplot_vst_ui"))
        ),
        nav_panel(
          "Comparison (Raw vs VST)",
          uiOutput(ns("comparison_ui"))
        ),
        nav_panel(
          "Count Table",
          withSpinner(
            reactableOutput(ns("table")),
            type = 4,
            color = "#0dcaf0",
            size = 1
          ),
          br(),
          div(
            downloadButton(
              ns("download"),
              "Download CSV",
              class = "btn-outline-primary"
            )
          )
        )
      )
    )
  )
}

summaryUI <- function(id) {
  ns <- NS(id)
  card(
    card_header("Data Summary"),
    card_body(
      withSpinner(
        uiOutput(ns("summary_text")),
        type = 4,
        color = "#0dcaf0",
        size = 0.8
      )
    )
  )
}

# =================================================
# EDITABLE SERVER (Modified to use modules)
# =================================================
editableTableServer <- function(id, data_rv, counts_rv, storage, email) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    current_data <- reactiveVal()
    deleted_stack <- reactiveVal(list())

    observe({
      req(data_rv$meta)
      if (is.null(current_data())) {
        current_data(data_rv$meta)
      }
    })

    output$table <- renderReactable({
      req(current_data())
      reactable(
        current_data(),
        highlight = TRUE,
        bordered = TRUE,
        striped = TRUE,
        compact = TRUE,
        resizable = TRUE,
        defaultPageSize = 10,
        onClick = "select",
        selection = "single",
        theme = reactableTheme(
          borderColor = "#ddd",
          stripedColor = "#f8f9fa",
          highlightColor = "#f5f5f5"
        )
      )
    })

    observeEvent(input$table__reactable__selected, {
      sel <- input$table__reactable__selected
      req(sel)
      row <- current_data()[sel, , drop = FALSE]

      group_col <- as.character(current_data()$group)
      group_choices <- sort(unique(group_col[
        !is.na(group_col) & group_col != ""
      ]))

      input_fields <- lapply(colnames(current_data()), function(col) {
        if (col == "group") {
          column(
            6,
            selectInput(
              ns(paste0("edit_", col)),
              label = col,
              choices = c(group_choices, "Add New..." = "__new__"),
              selected = as.character(row[[col]])
            )
          )
        } else {
          column(
            6,
            textInput(
              ns(paste0("edit_", col)),
              label = col,
              value = as.character(row[[col]])
            )
          )
        }
      })

      showModal(modalDialog(
        title = paste("Edit Sample:", row$label),
        fluidRow(do.call(tagList, input_fields)),
        p(
          "Note: Changing label will update the column headers in the linked table below.",
          style = "font-size: 0.85em; color: #0066cc; font-weight: bold;"
        ),
        p(
          "You can select from existing Group or add a new one.",
          style = "font-size: 0.85em; color: #666; font-style: italic;"
        ),
        conditionalPanel(
          condition = paste0("input['", ns("edit_group"), "'] == '__new__'"),
          textInput(ns("new_group_name"), "New Group Name:", "")
        ),
        footer = tagList(
          actionButton(
            ns("delete_modal"),
            "Delete Row",
            class = "btn-outline-danger me-2",
            icon = icon("trash")
          ),
          modalButton("Cancel"),
          actionButton(ns("save_edit"), "Save Changes", class = "btn-primary")
        ),
        size = "l"
      ))
    })

    observeEvent(input$save_edit, {
      sel <- input$table__reactable__selected
      req(sel)
      updated <- current_data()
      old_row <- updated[sel, , drop = FALSE]
      old_label <- old_row$label

      group_value <- input$edit_group
      if (group_value == "__new__" && nzchar(input$new_group_name)) {
        group_value <- input$new_group_name
      }

      # Track changes before updating
      for (col in colnames(updated)) {
        input_id <- paste0("edit_", col)
        if (col == "group") {
          old_val <- as.character(old_row[[col]])
          new_val <- group_value
          if (old_val != new_val) {
            # Use email module to track change
            email$track_change(
              type = "Group Change",
              details = glue::glue("Changed group for sample '{old_label}'"),
              sample_label = old_label,
              old_value = old_val,
              new_value = new_val
            )
          }
          updated[sel, col] <- group_value
        } else if (input_id %in% names(input)) {
          old_val <- as.character(old_row[[col]])
          new_val <- input[[input_id]]
          if (old_val != new_val) {
            # Use email module to track change
            email$track_change(
              type = "Field Update",
              details = glue::glue("Updated {col} for sample '{old_label}'"),
              sample_label = old_label,
              old_value = old_val,
              new_value = new_val
            )
          }
          updated[sel, col] <- input[[input_id]]
        }
      }

      current_data(updated)

      # Sync count matrix if label changed
      if (old_label %in% colnames(counts_rv$counts)) {
        new_label <- updated[sel, "label"]
        if (old_label != new_label) {
          colnames(counts_rv$counts)[
            colnames(counts_rv$counts) == old_label
          ] <- new_label

          # Track label change
          email$track_change(
            type = "Label Change",
            details = glue::glue(
              "Renamed sample from '{old_label}' to '{new_label}'"
            ),
            sample_label = old_label,
            old_value = old_label,
            new_value = new_label
          )
        }
      }

      # Auto-save disabled - use manual "Save Now" button
      # current_edits <- list(
      #   metadata = current_data(),
      #   counts = counts_rv$counts
      # )
      # storage$update_edits(current_edits)

      removeModal()
      showNotification("Updated! Count matrix synced.", type = "message")
    })

    observeEvent(input$delete_modal, {
      sel <- input$table__reactable__selected
      req(sel)
      label <- current_data()[sel, "label"]
      row_to_del <- current_data()[sel, , drop = FALSE]
      current_data(current_data()[-sel, ])
      deleted_stack(append(
        deleted_stack(),
        list(list(row = row_to_del, label = label))
      ))

      # Track deletion
      email$track_change(
        type = "Row Deletion",
        details = glue::glue("Deleted sample '{label}'"),
        sample_label = label
      )

      if (label %in% colnames(counts_rv$counts)) {
        counts_rv$counts <- counts_rv$counts[,
          !colnames(counts_rv$counts) == label,
          drop = FALSE
        ]
      }

      # Auto-save disabled - use manual "Save Now" button
      # current_edits <- list(
      #   metadata = current_data(),
      #   counts = counts_rv$counts
      # )
      # storage$update_edits(current_edits)

      showNotification("Deleted. Undo available.", type = "warning")
      removeModal()
    })

    output$undo_ui <- renderUI({
      if (length(deleted_stack()) == 0) {
        return(NULL)
      }
      actionButton(
        ns("undo_delete"),
        "Undo Delete",
        class = "btn-outline-success",
        icon = icon("history")
      )
    })

    observeEvent(input$undo_delete, {
      req(length(deleted_stack()) > 0)
      last <- deleted_stack()[[length(deleted_stack())]]
      deleted_stack(deleted_stack()[-length(deleted_stack())])
      current_data(rbind(current_data(), last$row))

      # Track undo
      email$track_change(
        type = "Undo Operation",
        details = glue::glue("Restored deleted sample '{last$label}'"),
        sample_label = last$label
      )

      if (last$label %in% colnames(counts_rv$orig_counts)) {
        counts_rv$counts[[last$label]] <- counts_rv$orig_counts[[last$label]]
      }

      # Auto-save disabled - use manual "Save Now" button
      # current_edits <- list(
      #   metadata = current_data(),
      #   counts = counts_rv$counts
      # )
      # storage$update_edits(current_edits)

      showNotification("Undo: restored.", type = "message")
    })

    observeEvent(input$reset, {
      old_sample_count <- nrow(current_data())
      current_data(data_rv$orig_meta)
      counts_rv$counts <- counts_rv$orig_counts  # Use counts_rv$orig_counts for consistency
      deleted_stack(list())

      # Track reset
      email$track_change(
        type = "Dataset Reset",
        details = glue::glue(
          "Reset dataset to original state ({old_sample_count} → {nrow(current_data())} samples)"
        )
      )

      # Auto-save disabled - use manual "Save Now" button
      # current_edits <- list(
      #   metadata = data_rv$orig_meta,
      #   counts = data_rv$orig_counts
      # )
      # storage$update_edits(current_edits)

      showNotification("Reset to original uploaded data!", type = "warning")
    })

    output$download <- downloadHandler(
      filename = function() paste("metadata_", Sys.Date(), ".csv"),
      content = function(file) {
        write.csv(current_data(), file, row.names = FALSE)
      }
    )

    return(list(data = current_data, deleted_stack = deleted_stack))
  })
}

summaryServer <- function(id, current_data) {
  moduleServer(id, function(input, output, session) {
    output$summary_text <- renderUI({
      req(current_data())

      data <- current_data()
      n_rows <- nrow(data)

      if ("group" %in% colnames(data)) {
        groups <- table(data$group)
        group_summary <- paste(names(groups), ":", groups, collapse = ", ")
      } else {
        group_summary <- "No group column found"
      }

      tagList(
        h5("Dataset Overview", class = "mb-3"),
        p(
          strong("Total Samples: "),
          span(n_rows, class = "badge bg-primary ms-1")
        ),
        hr(),
        h6("Group Distribution:", class = "mb-2"),
        p(
          group_summary,
          style = "font-size: 0.9em; color: #666;"
        )
      )
    })
  })
}

linkedTableServer <- function(id, counts_rv, meta_data, pca_rv = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    dynamic_data <- reactive({
      data <- counts_rv$counts
      cur <- meta_data()
      if (!is.null(cur) && nrow(cur) > 0 && "label" %in% colnames(cur)) {
        available_cols <- intersect(cur$label, colnames(data))
        if (length(available_cols) > 0) {
          data <- data %>% select(all_of(available_cols))
        }
      }
      data
    })
    
    # Extended SARTools-style color palette (12 base colors for better interpolation)
    # These are carefully chosen to be distinct and colorblind-friendly
    sartools_base_colors <- c(
      "#6BAED6",  # Light blue
      "#FC8D62",  # Orange
      "#66C2A5",  # Teal/Green
      "#E78AC3",  # Pink
      "#A6D854",  # Lime green
      "#FFD92F",  # Yellow
      "#E5C494",  # Tan
      "#B3B3B3",  # Gray
      "#8DA0CB",  # Periwinkle
      "#E41A1C",  # Red
      "#377EB8",  # Blue
      "#4DAF4A"   # Green
    )
    
    # Helper function to generate colors for any number of groups
    get_group_colors <- function(n_groups, group_names) {
      if (n_groups <= length(sartools_base_colors)) {
        # Use base colors directly if we have enough
        colors <- sartools_base_colors[1:n_groups]
      } else {
        # Interpolate to create more colors
        colors <- colorRampPalette(sartools_base_colors)(n_groups)
      }
      names(colors) <- group_names
      colors
    }
    
    # Custom theme with lighter background
    theme_qc <- function(base_size = 12) {
      theme_bw(base_size = base_size) +
        theme(
          panel.background = element_rect(fill = "#fafafa", color = NA),
          plot.background = element_rect(fill = "white", color = NA),
          panel.grid.major = element_line(color = "#e0e0e0", linewidth = 0.3),
          panel.grid.minor = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
          plot.title = element_text(hjust = 0.5, face = "bold"),
          legend.position = "bottom",
          legend.background = element_rect(fill = "white"),
          strip.background = element_rect(fill = "#f0f0f0", color = "#cccccc"),
          strip.text = element_text(face = "bold", size = 11)
        )
    }
    
    # Helper function to create box plot data frame
    create_boxplot_df <- function(count_matrix, metadata) {
      # Convert to long format for ggplot
      df_long <- count_matrix %>%
        tibble::rownames_to_column("gene") %>%
        tidyr::pivot_longer(
          cols = -gene,
          names_to = "sample",
          values_to = "count"
        ) %>%
        mutate(log2_count = log2(count + 1))
      
      # Add group information from metadata
      if (!is.null(metadata) && "label" %in% colnames(metadata) && "group" %in% colnames(metadata)) {
        df_long <- df_long %>%
          left_join(metadata %>% select(label, group), by = c("sample" = "label"))
      } else {
        df_long$group <- "Sample"
      }
      
      # Order samples by group then by name
      sample_order <- metadata %>% 
        arrange(group, label) %>% 
        pull(label)
      df_long$sample <- factor(df_long$sample, levels = sample_order)
      
      df_long
    }
    
    # Raw counts violin + box plot - static ggplot2 (SARTools style)
    output$boxplot_raw <- renderPlot({
      req(dynamic_data(), meta_data())
      
      counts_data <- dynamic_data()
      cur_meta <- meta_data()
      
      # Create long format data
      plot_df <- create_boxplot_df(counts_data, cur_meta)
      
      # Get unique groups for coloring (preserve order from metadata)
      unique_groups <- unique(cur_meta$group)
      n_groups <- length(unique_groups)
      group_colors <- get_group_colors(n_groups, unique_groups)
      
      # Create violin + box plot overlay
      ggplot(plot_df, aes(x = sample, y = log2_count, fill = group)) +
        geom_violin(trim = FALSE, scale = "width", alpha = 0.7) +
        geom_boxplot(width = 0.15, fill = "white", alpha = 0.8, 
                     outlier.size = 0.5, outlier.alpha = 0.3) +
        scale_fill_manual(values = group_colors) +
        labs(
          title = "Raw Counts Distribution",
          x = "Sample",
          y = expression(log[2](count + 1)),
          fill = "Group"
        ) +
        theme_qc()
    }, res = 96)
    
    # VST box plot UI - conditional on PCA being computed
    output$boxplot_vst_ui <- renderUI({
      if (is.null(pca_rv) || !isTRUE(pca_rv$computed) || is.null(pca_rv$pca_result$vst)) {
        tags$div(
          style = "text-align: center; padding: 50px; color: #666;",
          tags$i(class = "fa fa-info-circle", style = "font-size: 2em; margin-bottom: 10px;"),
          tags$p("VST counts not available yet."),
          tags$p("Run PCA analysis first to generate variance-stabilized counts.")
        )
      } else {
        plotOutput(ns("boxplot_vst"), height = "500px")
      }
    })
    
    # VST counts violin + box plot - static ggplot2
    output$boxplot_vst <- renderPlot({
      req(pca_rv$computed, pca_rv$pca_result$vst)
      
      # Extract VST counts
      vst_counts <- SummarizedExperiment::assay(pca_rv$pca_result$vst)
      cur_meta <- meta_data()
      
      # Filter to current samples
      if (!is.null(cur_meta) && nrow(cur_meta) > 0 && "label" %in% colnames(cur_meta)) {
        available_cols <- intersect(cur_meta$label, colnames(vst_counts))
        if (length(available_cols) > 0) {
          vst_counts <- vst_counts[, available_cols, drop = FALSE]
          cur_meta <- cur_meta %>% filter(label %in% available_cols)
        }
      }
      
      # Convert to data frame and long format
      vst_df <- as.data.frame(vst_counts)
      
      df_long <- vst_df %>%
        tibble::rownames_to_column("gene") %>%
        tidyr::pivot_longer(
          cols = -gene,
          names_to = "sample",
          values_to = "vst_count"
        )
      
      # Add group information
      if (!is.null(cur_meta) && "label" %in% colnames(cur_meta) && "group" %in% colnames(cur_meta)) {
        df_long <- df_long %>%
          left_join(cur_meta %>% select(label, group), by = c("sample" = "label"))
      } else {
        df_long$group <- "Sample"
      }
      
      # Order samples
      sample_order <- cur_meta %>% arrange(group, label) %>% pull(label)
      df_long$sample <- factor(df_long$sample, levels = sample_order)
      
      # Get unique groups for coloring
      unique_groups <- unique(cur_meta$group)
      n_groups <- length(unique_groups)
      group_colors <- get_group_colors(n_groups, unique_groups)
      
      # Create violin + box plot overlay
      ggplot(df_long, aes(x = sample, y = vst_count, fill = group)) +
        geom_violin(trim = FALSE, scale = "width", alpha = 0.7) +
        geom_boxplot(width = 0.15, fill = "white", alpha = 0.8,
                     outlier.size = 0.5, outlier.alpha = 0.3) +
        scale_fill_manual(values = group_colors) +
        labs(
          title = "VST Normalized Counts Distribution",
          x = "Sample",
          y = "VST counts",
          fill = "Group"
        ) +
        theme_qc()
    }, res = 96)
    
    # Side-by-side comparison UI (SARTools style - raw vs normalized)
    output$comparison_ui <- renderUI({
      if (is.null(pca_rv) || !isTRUE(pca_rv$computed) || is.null(pca_rv$pca_result$vst)) {
        tags$div(
          style = "text-align: center; padding: 50px; color: #666;",
          tags$i(class = "fa fa-info-circle", style = "font-size: 2em; margin-bottom: 10px;"),
          tags$p("Comparison not available yet."),
          tags$p("Run PCA analysis first to compare raw vs VST-normalized counts.")
        )
      } else {
        plotOutput(ns("comparison_plot"), height = "500px")
      }
    })
    
    # Side-by-side comparison plot (like SARTools countsBoxplots.png)
    output$comparison_plot <- renderPlot({
      req(pca_rv$computed, pca_rv$pca_result$vst, dynamic_data(), meta_data())
      
      counts_data <- dynamic_data()
      cur_meta <- meta_data()
      
      # Extract VST counts
      vst_counts <- SummarizedExperiment::assay(pca_rv$pca_result$vst)
      
      # Filter to current samples
      available_cols <- intersect(cur_meta$label, colnames(vst_counts))
      if (length(available_cols) > 0) {
        vst_counts <- vst_counts[, available_cols, drop = FALSE]
        counts_data <- counts_data %>% select(all_of(available_cols))
        cur_meta <- cur_meta %>% filter(label %in% available_cols)
      }
      
      # Create raw counts long format
      raw_df <- counts_data %>%
        tibble::rownames_to_column("gene") %>%
        tidyr::pivot_longer(cols = -gene, names_to = "sample", values_to = "count") %>%
        mutate(value = log2(count + 1), type = "Raw (log2 + 1)") %>%
        select(gene, sample, value, type)
      
      # Create VST counts long format
      vst_df <- as.data.frame(vst_counts) %>%
        tibble::rownames_to_column("gene") %>%
        tidyr::pivot_longer(cols = -gene, names_to = "sample", values_to = "count") %>%
        mutate(value = count, type = "VST Normalized") %>%
        select(gene, sample, value, type)
      
      # Combine
      plot_df <- bind_rows(raw_df, vst_df)
      
      # Add group information
      plot_df <- plot_df %>%
        left_join(cur_meta %>% select(label, group), by = c("sample" = "label"))
      
      # Order samples and types
      sample_order <- cur_meta %>% arrange(group, label) %>% pull(label)
      plot_df$sample <- factor(plot_df$sample, levels = sample_order)
      plot_df$type <- factor(plot_df$type, levels = c("Raw (log2 + 1)", "VST Normalized"))
      
      # Get colors
      unique_groups <- unique(cur_meta$group)
      n_groups <- length(unique_groups)
      group_colors <- get_group_colors(n_groups, unique_groups)
      
      # Create faceted violin + box plot comparison
      ggplot(plot_df, aes(x = sample, y = value, fill = group)) +
        geom_violin(trim = FALSE, scale = "width", alpha = 0.7) +
        geom_boxplot(width = 0.15, fill = "white", alpha = 0.8,
                     outlier.size = 0.3, outlier.alpha = 0.2) +
        facet_wrap(~type, scales = "free_y", ncol = 2) +
        scale_fill_manual(values = group_colors) +
        labs(
          title = "Comparison: Raw vs VST Normalized Counts",
          x = "Sample",
          y = "Expression",
          fill = "Group"
        ) +
        theme_qc(base_size = 11) +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1, size = 7)
        )
    }, res = 96)

    output$table <- renderReactable({
      req(dynamic_data())
      reactable(
        dynamic_data(),
        resizable = TRUE,
        compact = TRUE,
        defaultPageSize = 15,
        theme = reactableTheme(stripedColor = "#f8f9fa")
      )
    })

    output$download <- downloadHandler(
      filename = function() paste("counts_", Sys.Date(), ".csv"),
      content = function(file) write.csv(dynamic_data(), file, row.names = TRUE)
    )

    return(dynamic_data)
  })
}

# =================================================
# MAIN UI
# =================================================
ui <- function(request) {
  navbarPage(
    title = "QC CHECKER",
    id = "main_tabs",
    header = tags$head(
      shinyjs::useShinyjs(),
      tags$link(href = "https://fonts.googleapis.com/css2?family=Stack+Sans+Headline:wght@200..700&display=swap", rel = "stylesheet"),
      tags$script(HTML("
        $(document).ready(function() {
          // Create save button
          var saveBtn = $('<button>', {
            id: 'save_now_global',
            class: 'btn btn-success btn-sm',
            style: 'position: absolute; top: 8px; right: 15px; z-index: 1050;',
            html: '<i class=\"fa fa-save\"></i> Save Now'
          });
          
          // Add to navbar
          $('.navbar').append(saveBtn);
          
          // Bind click handler
          saveBtn.on('click', function() {
            Shiny.setInputValue('save_now_global', Math.random());
          });
          
          // Handle plotly resize when jqui resizable is used
          $(document).on('resizestop', '.jqui-resizable', function(event, ui) {
            var plotDiv = $(this).find('.js-plotly-plot')[0];
            if (plotDiv && window.Plotly) {
              setTimeout(function() {
                window.Plotly.Plots.resize(plotDiv);
              }, 100);
            }
          });
        });
      ")),
      tags$style(HTML(
        "
        .session-id-status {
          background-color: #e8f5e8;
          border: 1px solid #c3e6c3;
          border-radius: 5px;
          padding: 10px;
          margin: 10px 0;
          color: #155724;
        }
        .file-status {
          background-color: #e8f4fd;
          border: 1px solid #b6e0fe;
          border-radius: 5px;
          padding: 8px;
          margin: 5px 0;
          color: #0c5460;
          font-size: 0.85em;
        }
        .notification-status {
          font-size: 0.85em;
          color: #28a745;
          margin-top: 5px;
        }
        .batch-status {
          background-color: #e8f4fd;
          border: 1px solid #b6e0fe;
          border-radius: 5px;
          padding: 10px;
          margin: 10px 0;
          color: #0c5460;
        }
        .navbar-header {
         font-family: 'Stack Sans Headline', sans-serif;
       }
        .navbar-brand {
        font-family: 'Stack Sans Headline', sans-serif;
       }
        .save-button-header {
          position: absolute;
          top: 8px;
          right: 15px;
          z-index: 1050;
        }
        
        /* Resizable card styling - subtle gray handles */
        .ui-resizable-handle {
          background-color: #dee2e6;
          opacity: 0.5;
          transition: opacity 0.2s;
        }
        .ui-resizable-handle:hover {
          opacity: 0.8;
        }
        .ui-resizable-se {
          width: 12px;
          height: 12px;
          right: 1px;
          bottom: 1px;
          background-color: #adb5bd;
          border-radius: 0 0 4px 0;
        }
        .ui-resizable-s {
          height: 8px;
          bottom: 1px;
        }
        .ui-resizable-e {
          width: 8px;
          right: 1px;
        }
        
        /* Make cards resizable and plots fill them */
        .jqui-resizable {
          height: 650px;  /* Initial height */
          width: 100%;
          min-width: 400px;
          min-height: 400px;
        }
        
        .jqui-resizable .card {
          height: 100%;
          width: 100%;
          display: flex;
          flex-direction: column;
        }
        
        .jqui-resizable .card-body {
          flex: 1;
          display: flex;
          flex-direction: column;
          padding: 0.5rem;
          min-height: 0;  /* Important for flexbox */
        }
        
        .jqui-resizable .card-body > div {
          flex: 1;
          display: flex;
          flex-direction: column;
          min-height: 0;  /* Important for flexbox */
        }
        
        .jqui-resizable .card-body .shiny-spinner-output-container {
          flex: 1;
          display: flex;
          flex-direction: column;
          min-height: 0;  /* Important for flexbox */
        }
        
        .jqui-resizable .card-body .plotly,
        .jqui-resizable .card-body .js-plotly-plot {
          flex: 1;
          min-height: 0 !important;  /* Important for flexbox */
          width: 100% !important;
          height: 100% !important;
        }
        "
      ))
    ),
    tabPanel(
      "Start",
      value = "start",
      icon = icon("play"),
      fluidPage(
        br(),
        h4("Welcome to QC-CheckeR"),
        card(
          card_header("Load Existing Session or Start New"),
          card_body(
            textInput(
              "session_id_input",
              "Enter Session ID to resume previous work:",
              placeholder = "Paste your session ID here...",
              width = "35%"
            ),
            actionButton(
              "load_session",
              "Load Session",
              icon = icon("folder-open"),
              class = "btn-outline-primary",
              width = "220px"
            ),
            hr(),
            p(
              "Don't have a session ID?",
              tags$br(),
              "Upload a new .RData file on the next tab to get started."
            ),
            actionButton(
              "start_new",
              "Start New Session",
              icon = icon("plus"),
              class = "btn-success",
              width = "250px"
            ),
            hr(),
            p(
              tags$strong("New to QC-CheckeR?"),
              tags$br(),
              "Try the app with our example RNA-Seq dataset (20 samples, 4 groups)."
            ),
            actionButton(
              "load_example",
              "Load Example Data",
              icon = icon("flask"),
              class = "btn-outline-primary",
              width = "250px"
            )
          )
        ),
        br(),
        h5("How it works:"),
        tags$ul(
          tags$li(
            "Enter a Session ID to resume your previous work with all edits preserved"
          ),
          tags$li("Or start fresh by uploading a new .RData file"),
          tags$li(
            "Your data is automatically saved (debounced - 3 seconds after last edit)"
          ),
          tags$li("Use the Session ID to return to your work anytime"),
          tags$li("All changes can be undone with the undo functionality"),
          tags$li(
            "Enable email notifications to receive batched change summaries"
          )
        )
      )
    ),
    tabPanel(
      "Help",
      value = "help",
      icon = icon("circle-info"),
      fluidPage(
        br(),
        # Important Session ID Warning Card
        card(
          card_header(
            class = "bg-warning",
            tags$span(icon("triangle-exclamation"), " Important: Save Your Session ID")
          ),
          card_body(
            tags$p(
              style = "font-size: 1.1em;",
              tags$strong("Before closing this app, copy your Session ID!"),
              " Your Session ID is displayed in the sidebar of the Upload/Rename tab. ",
              "You will need this ID to resume your work later. Without it, your progress cannot be recovered."
            ),
            tags$p(
              style = "margin-bottom: 0;",
              icon("lightbulb"), " ",
              tags$em("Tip: Save your Session ID somewhere safe (e.g., notes, email to yourself, or bookmark).")
            )
          )
        ),
        br(),
        
        # App Overview
        h4(icon("info-circle"), " About QC-CheckeR"),
        tags$p(
          "QC-CheckeR is an interactive Shiny application for RNA-Seq quality control and exploratory data analysis. ",
          "It allows you to upload count matrices, edit sample metadata, visualize count distributions, ",
          "and perform PCA analysis using DESeq2's variance-stabilizing transformation (VST)."
        ),
        hr(),
        
        # Quick Start Guide
        h4(icon("rocket"), " Quick Start Guide"),
        tags$ol(
          tags$li(
            tags$strong("Upload Data: "),
            "Go to the ", tags$em("Upload/Rename"), " tab and upload an ", tags$code(".RData"), 
            " file containing a ", tags$code("counts"), " matrix and ", tags$code("metadata"), " data frame."
          ),
          tags$li(
            tags$strong("Review & Edit Metadata: "),
            "Click any row in the metadata table to edit sample labels and group assignments. ",
            "You can also delete samples (with undo support)."
          ),
          tags$li(
            tags$strong("Check Count Distributions: "),
            "View violin plots of raw counts (log2 + 1) in the ", tags$em("Raw Counts Violin"), " tab. ",
            "This helps identify outlier samples before normalization."
          ),
          tags$li(
            tags$strong("Run PCA Analysis: "),
            "Go to the ", tags$em("PCA Analysis"), " tab, configure VST parameters, and click ", 
            tags$em("Run DESeq2 PCA"), ". The plot will update with your samples colored by group."
          ),
          tags$li(
            tags$strong("Compare Distributions: "),
            "After PCA, view VST-normalized distributions and use the ", tags$em("Test (Side-by-Side)"), 
            " tab to compare raw vs. normalized counts."
          ),
          tags$li(
            tags$strong("Export Results: "),
            "Download PCA scores, edited metadata, and count matrices using the export buttons."
          )
        ),
        hr(),
        
        # Data Requirements
        h4(icon("database"), " Data Requirements"),
        tags$p("Your ", tags$code(".RData"), " file must contain:"),
        tags$ul(
          tags$li(
            tags$code("counts"), " — A count matrix (genes × samples). Column names must match ", 
            tags$code("metadata$label"), "."
          ),
          tags$li(
            tags$code("metadata"), " — A data frame with at least two columns:",
            tags$ul(
              tags$li(tags$code("label"), " — Sample identifiers (must match count matrix column names)"),
              tags$li(tags$code("group"), " — Group/condition assignments for coloring PCA plots")
            )
          )
        ),
        hr(),
        
        # Features Overview
        h4(icon("list-check"), " Features"),
        tags$div(
          class = "row",
          tags$div(
            class = "col-md-6",
            tags$h5("Metadata Editing"),
            tags$ul(
              tags$li("Click a row → edit all fields in a modal"),
              tags$li("Group dropdown allows adding new groups"),
              tags$li("Delete samples with undo support"),
              tags$li("Reset → restores original uploaded data"),
              tags$li("Changes sync live with count matrix and plots")
            )
          ),
          tags$div(
            class = "col-md-6",
            tags$h5("Visualization"),
            tags$ul(
              tags$li("Violin plots with box plots and scatter points"),
              tags$li("Raw counts (log2 + 1) and VST-normalized views"),
              tags$li("Side-by-side comparison for QC"),
              tags$li("Interactive PCA scatter plots (Plotly)"),
              tags$li("Customizable colors, point size, and opacity")
            )
          )
        ),
        tags$div(
          class = "row",
          tags$div(
            class = "col-md-6",
            tags$h5("PCA Analysis"),
            tags$ul(
              tags$li("DESeq2 variance-stabilizing transformation"),
              tags$li("Configurable: blind mode, fit type, top N genes"),
              tags$li("View any PC combination (PC1-PC10+)"),
              tags$li("Color by any metadata column"),
              tags$li("Export PCA scores as CSV")
            )
          ),
          tags$div(
            class = "col-md-6",
            tags$h5("Session Management"),
            tags$ul(
              tags$li("Auto-save every 3 seconds after edits"),
              tags$li("Unique Session ID for each session"),
              tags$li(tags$strong("Use your Session ID to resume work later")),
              tags$li("Email notifications for change summaries"),
              tags$li("Start new session clears all data")
            )
          )
        ),
        hr(),
        
        # Tips
        h4(icon("lightbulb"), " Tips"),
        tags$ul(
          tags$li(
            tags$strong("Outlier Detection: "),
            "Look for samples with unusual violin shapes or positions. These may indicate technical issues."
          ),
          tags$li(
            tags$strong("Batch Effects: "),
            "If VST normalization doesn't bring samples together by group, consider batch correction."
          ),
          tags$li(
            tags$strong("Large Datasets: "),
            "For many groups (40+), the 3-color gradient system scales automatically."
          ),
          tags$li(
            tags$strong("Sample Removal: "),
            "If you identify a problematic sample, delete it from metadata and re-run PCA to see the effect."
          )
        ),
        br()
      )
    ),
    tabPanel(
      "Upload/Rename",
      value = "editor",
      icon = icon("table"),
      page_sidebar(
        sidebar = sidebar(
          width = 400,
          h4("Session Management"),
          # Use the storage module UI
          sessionStorageUI("storage"),
          hr(),
          h4("Upload .RData"),
          fileInput(
            "file",
            "Choose .RData (metadata + counts)",
            accept = ".RData"
          ),
          tags$p(
            "Note: metadata must have",
            tags$code("label"),
            "and",
            tags$code("group"),
            "columns"
          ),
          hr(),
          conditionalPanel(
            condition = "output.file_uploaded",
            h5("Summary"),
            summaryUI("summary"),
            hr(),
            h5("Notifications"),
            # Use the email module UI
            emailNotificationsUI("email")
          )
        ),
        mainPanel(
          withSpinner(
            uiOutput("tables_ui"),
            type = 4,
            color = "#0dcaf0",
            size = 1
          )
        )
      )
    ),
    tabPanel(
      "PCA Analysis",
      value = "pca",
      icon = icon("project-diagram"),
      page_sidebar(
        sidebar = sidebar(
          width = 350,
          h4("PCA Parameters"),
          p("Using DESeq2 PCA with Variance Stabilizing Transformation", 
            style = "font-size: 0.9em; color: #666;"),
          hr(),
          
          # DESeq2 VST parameters
          checkboxInput(
            "vst_blind",
            "Blind to experimental design",
            value = TRUE
          ),
          helpText("Ignore sample information for exploratory PCA."),
          
          selectInput(
            "vst_fitType",
            "Dispersion fit type",
            choices = c("parametric", "local", "mean"),
            selected = "parametric"
          ),
          helpText("Method for fitting dispersion-mean relationship."),
          
          numericInput(
            "pca_ntop",
            "Top variable genes",
            value = 500,
            min = 100,
            step = 100
          ),
          helpText("Number of top genes by variance to use for PCA."),
          
          hr(),
          
          selectInput(
            "pca_pc_x",
            "X-axis PC:",
            choices = c("PC1" = 1, "PC2" = 2, "PC3" = 3, "PC4" = 4, "PC5" = 5),
            selected = 1
          ),
          
          selectInput(
            "pca_pc_y",
            "Y-axis PC:",
            choices = c("PC1" = 1, "PC2" = 2, "PC3" = 3, "PC4" = 4, "PC5" = 5),
            selected = 2
          ),
          
          uiOutput("pca_color_ui"),
          
          hr(),
          actionButton(
            "run_pca",
            "Run PCA",
            icon = icon("play"),
            class = "btn-outline-primary btn-lg w-100"
          )
        ),
        
        # Main content area
        conditionalPanel(
          condition = "output.pca_computed",
          card(
              full_screen = TRUE,
              card_header("PCA Results"),
              layout_sidebar(
                sidebar = sidebar(
                  id = "plot_customization_sidebar",
                  position = "right",
                  width = 280,
                  open = TRUE,
                  h4("Plot Options"),
                  
                  textInput("plot_title", "Title:", value = "PCA Score Plot"),
                  sliderInput("plot_point_size", "Point Size:", min = 10, max = 50, value = 20, step = 1),
                  sliderInput("plot_point_opacity", "Opacity:", min = 0.1, max = 1, value = 1, step = 0.1),
                  
                  # 3-color gradient picker
                  tags$label("Gradient Colors:", style = "font-weight: 500;"),
                  tags$div(
                    style = "display: flex; gap: 5px; margin-bottom: 10px;",
                    tags$div(
                      style = "flex: 1; text-align: center;",
                      colourpicker::colourInput("gradient_color1", NULL, "#636EFA", 
                        showColour = "background", palette = "square"),
                      tags$small("Start", style = "color: #888; font-size: 0.7em;")
                    ),
                    tags$div(
                      style = "flex: 1; text-align: center;",
                      colourpicker::colourInput("gradient_color2", NULL, "#EF553B", 
                        showColour = "background", palette = "square"),
                      tags$small("Mid", style = "color: #888; font-size: 0.7em;")
                    ),
                    tags$div(
                      style = "flex: 1; text-align: center;",
                      colourpicker::colourInput("gradient_color3", NULL, "#00CC96", 
                        showColour = "background", palette = "square"),
                      tags$small("End", style = "color: #888; font-size: 0.7em;")
                    )
                  ),
                  
                  checkboxInput("plot_show_grid", "Show Grid", value = TRUE),
                  checkboxInput("plot_show_legend", "Show Legend", value = TRUE),
                  selectInput("plot_bg_color", "Background:",
                    choices = c("Light Gray" = "#f8f9fa", "White" = "white",
                               "Light Blue" = "#f0f8ff", "Light Yellow" = "#fffef0",
                               "Light Green" = "#f0fff0"),
                    selected = "#fffef0"
                  ),
                  hr(),
                  actionButton("open_download_modal", "Download Plot", icon = icon("download"),
                             class = "btn-success w-100 btn-sm"),
                  downloadButton("download_eigenvalues", "Export Eigenvalues", 
                             class = "btn-outline-primary w-100 btn-sm", style = "margin-top: 5px;"),
                  downloadButton("download_pca_scores", "Export PCA Scores", 
                             class = "btn-outline-primary w-100 btn-sm", style = "margin-top: 5px;"),
                  actionButton("reset_plot_options", "Reset Options", icon = icon("undo"),
                             class = "btn-secondary w-100 btn-sm", style = "margin-top: 5px;")
                ),
                
                # Tabbed content
                navset_card_tab(
                  id = "pca_tabs",
                  nav_panel(
                    "PCA Score Plot",
                    withSpinner(
                      plotlyOutput("pca_plot", width = "100%", height = "700px"),
                      type = 4,
                      color = "#0dcaf0"
                    )
                  ),
                  nav_panel(
                    "Variance Explained",
                    withSpinner(
                      plotlyOutput("pca_scree", width = "100%", height = "700px"),
                      type = 4,
                      color = "#0dcaf0"
                    )
                  ),
                  nav_panel(
                    "PCA Summary",
                    withSpinner(
                      verbatimTextOutput("pca_summary"),
                      type = 4,
                      color = "#0dcaf0"
                    )
                  )
                )
              )
            )
          ),
          conditionalPanel(
            condition = "!output.pca_computed",
            card(
              card_body(
                div(
                  style = "text-align: center; padding: 100px 20px; color: #999;",
                  icon("project-diagram", style = "font-size: 72px;"),
                  h3("No PCA Results Yet"),
                  p("Upload data in the Upload/Rename tab, configure parameters, and click 'Run PCA'")
                )
              )
            )
          )
      )  # Close page_sidebar
    )  # Close tabPanel (PCA Analysis)
  )  # Close navbarPage
}

# =================================================
# DOWNLOAD MODAL
# =================================================
download_modal <- modalDialog(
  title = "Download PCA Plot",
  size = "m",
  
  textInput(
    "plot_download_filename",
    "Filename:",
    value = "pca_plot",
    placeholder = "Enter filename (no extension)"
  ),
  
  selectInput(
    "plot_download_format",
    "File Format:",
    choices = c(
      "PNG (recommended)" = "png",
      "HTML (interactive)" = "html"
    ),
    selected = "png"
  ),
  
  numericInput(
    "plot_download_width",
    "Width (pixels):",
    value = 1200,
    min = 400,
    max = 4000,
    step = 100
  ),
  
  numericInput(
    "plot_download_height",
    "Height (pixels):",
    value = 800,
    min = 400,
    max = 4000,
    step = 100
  ),
  
  footer = tagList(
    modalButton("Cancel"),
    downloadButton("download_pca_plot", "Download", class = "btn-success")
  )
)

# =================================================
# SERVER
# =================================================
server <- function(input, output, session) {
  # Helper function - null coalesce operator
  `%||%` <- function(a, b) if (is.null(a)) b else a
  
  # Initialize meta_out
  meta_out <- reactiveValues(data = NULL)

  # =================================================
  # GLOBAL SAVE BUTTON - MANUAL SAVE ONLY
  # =================================================

  observeEvent(input$save_now_global, {
    if (!is.null(data_rv$meta)) {
      # Create progress bar
      progress <- Progress$new()
      on.exit(progress$close())
      
      progress$set(message = "Saving session", value = 0)
      
      tryCatch(
        {
          progress$set(value = 0.1, detail = "Preparing data...")
          
          # Get current metadata (including any edits) using helper
          current_meta <- get_current_metadata()
          if (is.null(current_meta)) {
            current_meta <- data_rv$meta
          }
          
          progress$set(value = 0.2, detail = "Collecting metadata and counts...")
          
          # Prepare data to save
          save_data <- list(
            metadata = current_meta,
            counts = counts_rv$counts
          )
          
          # Add DDS object if it exists (from PCA computation)
          if (!is.null(pca_rv$pca_result) && !is.null(pca_rv$pca_result$dds)) {
            progress$set(value = 0.4, detail = "Including DESeq2 dataset...")
            save_data$dds <- pca_rv$pca_result$dds
          }
          
          # Add VST-transformed counts if they exist
          if (!is.null(pca_rv$pca_result) && !is.null(pca_rv$pca_result$vst)) {
            progress$set(value = 0.5, detail = "Including VST-normalized counts...")
            save_data$vst_counts <- SummarizedExperiment::assay(pca_rv$pca_result$vst)
            save_data$vst_result <- pca_rv$pca_result$vst  # Save full VST object for restoration
          }
          
          # Add PCA results if they exist
          if (!is.null(pca_rv$pca_result)) {
            progress$set(value = 0.6, detail = "Including PCA results...")
            save_data$pca_result <- list(
              pca = pca_rv$pca_result$pca,
              pca_data = pca_rv$pca_data,
              var_explained = pca_rv$pca_result$var_explained,
              ntop = pca_rv$pca_result$ntop,
              params_used = pca_rv$pca_result$params_used,  # Save parameters used
              plot_settings = list(  # Save plot customization settings
                title = input$plot_title,
                point_size = input$plot_point_size,
                point_opacity = input$plot_point_opacity,
                gradient_color1 = input$gradient_color1,
                gradient_color2 = input$gradient_color2,
                gradient_color3 = input$gradient_color3,
                show_grid = input$plot_show_grid,
                show_legend = input$plot_show_legend,
                bg_color = input$plot_bg_color
              )
            )
          }
          
          progress$set(value = 0.7, detail = "Creating session file...")
          
          # Save using custom save function
          session_id <- isolate(storage$session_id())
          if (is.null(session_id)) {
            session_id <- storage$create_session()
          }
          
          # Create session file
          session_file <- file.path("saved_sessions", paste0(session_id, ".rds"))
          
          progress$set(value = 0.8, detail = "Packaging data...")
          
          session_data <- list(
            session_id = session_id,
            file_data = list(
              metadata = data_rv$orig_meta,
              counts = data_rv$orig_counts
            ),
            current_edits = save_data,
            file_name = isolate(storage$file_data()$file_name) %||% "data.RData",
            timestamp = Sys.time(),
            last_accessed = Sys.time()
          )
          
          progress$set(value = 0.9, detail = "Writing to disk...")
          saveRDS(session_data, session_file)
          
          progress$set(value = 1, detail = "Complete!")
          Sys.sleep(0.3)  # Brief pause to show completion
          
          showNotification(
            "Session saved successfully! All data, PCA, and DDS objects saved.",
            type = "message",
            duration = 3
          )
          cat("Manual save completed at:", format(Sys.time(), "%H:%M:%S"), "\n")
          cat("  - Metadata:", nrow(save_data$metadata), "samples\n")
          cat("  - Counts:", ncol(save_data$counts), "samples x", nrow(save_data$counts), "features\n")
          if (!is.null(save_data$dds)) cat("  - DDS object included\n")
          if (!is.null(save_data$vst_counts)) cat("  - VST counts included\n")
          if (!is.null(save_data$pca_result)) cat("  - PCA results included\n")
        },
        error = function(e) {
          showNotification(
            paste("Save failed:", e$message),
            type = "error",
            duration = 5
          )
          cat("Save error:", e$message, "\n")
        }
      )
    } else {
      showNotification("No data to save yet", type = "warning")
    }
  })

  # =================================================
  # SESSION END HANDLING
  # =================================================

  # Session end save disabled - manual save only
  # session$onSessionEnded(function() {
  #   cat("Session ended - forcing final save\n")
  #   # Force immediate save (bypasses debounce)
  #   if (!is.null(isolate(data_rv$meta))) {
  #     storage$force_save()
  #     cat("Final save completed\n")
  #   }
  #   stopApp()
  # })

  # Periodic backup disabled - manual save only
  # observe({
  #   invalidateLater(30000, session) # 30 seconds
  #
  #   if (!is.null(data_rv$meta)) {
  #     cat("Periodic backup save at:", format(Sys.time(), "%H:%M:%S"), "\n")
  #     storage$force_save()
  #   }
  # })

  output$file_uploaded <- reactive({
    !is.null(data_rv$meta) || !is.null(input$file)
  })
  outputOptions(output, "file_uploaded", suspendWhenHidden = FALSE)

  data_rv <- reactiveValues(
    meta = NULL,
    orig_meta = NULL,
    counts = NULL,
    orig_counts = NULL
  )

  # Initialize counts_rv at server level
  counts_rv <- reactiveValues(
    counts = NULL,
    orig_counts = NULL
  )
  
  # Initialize PCA reactive values
  pca_rv <- reactiveValues(
    pca_result = NULL,
    pca_data = NULL,
    computed = FALSE
  )

  # Update counts_rv when data changes
  observe({
    req(data_rv$counts)
    if (is.null(counts_rv$counts)) {
      counts_rv$counts <- data_rv$counts
      counts_rv$orig_counts <- data_rv$orig_counts
    }
  })

  # =================================================
  # INITIALIZE MODULES
  # =================================================

  # Initialize storage module with configuration
  storage <- sessionStorageServer(
    "storage",
    storage_config = list(
      file_storage_enabled = TRUE,
      storage_dir = "saved_sessions",
      db_storage_enabled = FALSE,
      cleanup_days = 7
    ),
    debounce_seconds = 3 # Wait 3 seconds after last change
  )


# cat("\n========================================\n")
# cat("DEBUG: Checking email credentials\n")
# cat("========================================\n")

# creds_path <- "/root/.shiny_app_credentials/gmail_creds"
# cat("Path:", creds_path, "\n")
# cat("File exists:", file.exists(creds_path), "\n")

# if (file.exists(creds_path)) {
#   cat("File size:", file.info(creds_path)$size, "bytes\n")
#   cat("Readable:", file.access(creds_path, 4) == 0, "\n")
  
#   # Try to load it
#   tryCatch({
#     library(blastula)
#     test_creds <- creds_file(creds_path)
#     cat("✅ Credentials loaded successfully!\n")
#     cat("User:", test_creds$user, "\n")
#   }, error = function(e) {
#     cat("❌ Error loading credentials:", e$message, "\n")
#   })
# } else {
#   cat("❌ File does not exist!\n")
#   cat("Directory contents:\n")
#   print(list.files("/root/.shiny_app_credentials/", full.names = TRUE))
# }

# cat("========================================\n\n")


  # Initialize email module with configuration
  email <- emailNotificationsServer(
    "email",
    email_config = list(
      enabled = TRUE,
      sender_email = "rshiny.trex@gmail.com", # Will be overridden by creds
      creds_file = "/srv/shiny-server/QC_CHECKER/.credentials/gmail_creds",
      batch_delay_seconds = 120,
      min_changes_for_email = 1
    ),
    get_summary_func = reactive({
      # Use helper function to get current metadata
      current_meta <- get_current_metadata()

      if (!is.null(current_meta) && nrow(current_meta) > 0) {
        groups <- if ("group" %in% colnames(current_meta)) {
          paste(unique(current_meta$group), collapse = ", ")
        } else {
          "No group column"
        }

        session_id <- storage$session_id()

        list(
          info = glue::glue(
            "Total Samples: {nrow(current_meta)}\n",
            "Groups: {groups}\n",
            "Session ID: {ifelse(is.null(session_id), 'None', session_id)}"
          )
        )
      } else {
        list(info = "No data")
      }
    })
  )

  # =================================================
  # SESSION MANAGEMENT
  # =================================================

  # Load session from ID
  observeEvent(input$load_session, {
    req(input$session_id_input)
    session_id <- trimws(input$session_id_input)

    if (nzchar(session_id)) {
      # Create progress bar
      progress <- Progress$new()
      on.exit(progress$close())
      
      progress$set(message = "Loading session", value = 0)
      progress$set(value = 0.2, detail = "Reading session file...")

      # Use storage module to load session
      session_data <- storage$load_session(session_id)

      if (!is.null(session_data)) {
        progress$set(value = 0.4, detail = "Restoring metadata...")
        
        # Restore the edited data
        data_rv$meta <- session_data$current_edits$metadata
        data_rv$orig_meta <- session_data$file_data$metadata
        data_rv$counts <- session_data$current_edits$counts
        data_rv$orig_counts <- session_data$file_data$counts
        
        progress$set(value = 0.6, detail = "Restoring count data...")
        
        # Restore PCA results if they exist
        if (!is.null(session_data$current_edits$pca_result)) {
          progress$set(value = 0.8, detail = "Restoring PCA results...")
          
          pca_rv$pca_result <- list(
            pca = session_data$current_edits$pca_result$pca,
            vst = session_data$current_edits$vst_result,
            dds = session_data$current_edits$dds,
            pca_data = session_data$current_edits$pca_result$pca_data,
            var_explained = session_data$current_edits$pca_result$var_explained,
            ntop = session_data$current_edits$pca_result$ntop,
            params_used = session_data$current_edits$pca_result$params_used  # Restore parameters
          )
          pca_rv$pca_data <- as.data.frame(session_data$current_edits$pca_result$pca_data)
          pca_rv$computed <- TRUE
          
          # Restore plot settings if they exist
          if (!is.null(session_data$current_edits$pca_result$plot_settings)) {
            plot_settings <- session_data$current_edits$pca_result$plot_settings
            updateTextInput(session, "plot_title", value = plot_settings$title %||% "PCA Score Plot")
            updateSliderInput(session, "plot_point_size", value = plot_settings$point_size %||% 20)
            updateSliderInput(session, "plot_point_opacity", value = plot_settings$point_opacity %||% 1)
            # Backward compatible: check new names first, then fall back to old names
            colourpicker::updateColourInput(session, "gradient_color1", 
              value = plot_settings$gradient_color1 %||% plot_settings$custom_color1 %||% "#636EFA")
            colourpicker::updateColourInput(session, "gradient_color2", 
              value = plot_settings$gradient_color2 %||% plot_settings$custom_color2 %||% "#EF553B")
            colourpicker::updateColourInput(session, "gradient_color3", 
              value = plot_settings$gradient_color3 %||% plot_settings$custom_color3 %||% "#00CC96")
            updateCheckboxInput(session, "plot_show_grid", value = plot_settings$show_grid %||% TRUE)
            updateCheckboxInput(session, "plot_show_legend", value = plot_settings$show_legend %||% TRUE)
            updateSelectInput(session, "plot_bg_color", selected = plot_settings$bg_color %||% "#fffef0")
          }
          
          showNotification(
            "PCA results restored!",
            type = "message",
            duration = 3
          )
        } else {
          # No PCA in saved session
          pca_rv$pca_result <- NULL
          pca_rv$pca_data <- NULL
          pca_rv$computed <- FALSE
        }

        progress$set(value = 1, detail = "Complete!")
        Sys.sleep(0.3)  # Brief pause to show completion
        
        showNotification(
          "Session loaded successfully! All edits preserved.",
          type = "message"
        )

        # Switch to editor tab
        shinyjs::runjs('$("a[data-value=\'editor\']").click();')

        # Track session load
        email$track_change(
          type = "Session Loaded",
          details = glue::glue(
            "Loaded existing session with {nrow(data_rv$meta)} samples"
          )
        )
      } else {
        showNotification(
          "Session not found. Please check the Session ID or start a new session.",
          type = "error"
        )
      }
    }
  })

  # Start new session
 observeEvent(input$start_new, {
    # Show confirmation modal first
    showModal(modalDialog(
      title = "Start New Session?",
      p("This will reset the entire application and clear all current data, including:"),
      tags$ul(
        tags$li("Uploaded metadata and counts"),
        tags$li("All edits and changes"),
        tags$li("PCA analysis results"),
        tags$li("Current session ID")
      ),
      p(strong("This action cannot be undone."), style = "color: #dc3545;"),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_new_session", "Yes, Start Fresh", class = "btn-danger")
      ),
      size = "m"
    ))
  })
  
  # Confirmed new session - reset everything
  observeEvent(input$confirm_new_session, {
    removeModal()
    
    # Reset all data reactive values
    data_rv$meta <- NULL
    data_rv$orig_meta <- NULL
    data_rv$counts <- NULL
    data_rv$orig_counts <- NULL
    
    # Reset counts reactive values
    counts_rv$counts <- NULL
    counts_rv$orig_counts <- NULL
    
    # Reset PCA reactive values
    pca_rv$pca_result <- NULL
    pca_rv$pca_data <- NULL
    pca_rv$computed <- FALSE
    
    # Reset meta_out
    meta_out$data <- NULL
    
    # Reset file input
    shinyjs::reset("file")
    
    # Create new session ID
    new_session_id <- storage$create_session()
    
    # Reset plot options to defaults
    updateTextInput(session, "plot_title", value = "PCA Score Plot")
    updateSliderInput(session, "plot_point_size", value = 20)
    updateSliderInput(session, "plot_point_opacity", value = 1)
    colourpicker::updateColourInput(session, "gradient_color1", value = "#636EFA")
    colourpicker::updateColourInput(session, "gradient_color2", value = "#EF553B")
    colourpicker::updateColourInput(session, "gradient_color3", value = "#00CC96")
    updateCheckboxInput(session, "plot_show_grid", value = TRUE)
    updateCheckboxInput(session, "plot_show_legend", value = TRUE)
    updateSelectInput(session, "plot_bg_color", selected = "#fffef0")
    
    # Reset PCA parameters to defaults
    updateCheckboxInput(session, "vst_blind", value = TRUE)
    updateSelectInput(session, "vst_fitType", selected = "parametric")
    updateNumericInput(session, "pca_ntop", value = 500)
    updateSelectInput(session, "pca_pc_x", selected = 1)
    updateSelectInput(session, "pca_pc_y", selected = 2)
    
    showNotification(
      glue::glue("New session started: {new_session_id}\nAll data has been cleared."),
      type = "message",
      duration = 5
    )
    
    # Switch to editor tab
    shinyjs::runjs('$("a[data-value=\'editor\']").click();')
  })
  
  # =================================================
  # LOAD EXAMPLE DATA
  # =================================================
  observeEvent(input$load_example, {
    # Path to example data file (server-side) - RData format
    example_file <- "Example_Data.RData"
    
    # Check if file exists
    if (!file.exists(example_file)) {
      showNotification(
        "Example data file not found. Please contact administrator.",
        type = "error",
        duration = 5
      )
      return()
    }
    
    showNotification(
      "Loading example data...",
      type = "message",
      duration = NULL,
      id = "loading_example"
    )
    
    tryCatch({
      # Load RData file into environment
      env <- new.env()
      load(example_file, envir = env)
      
      # Check for required objects
      if (!all(c("metadata", "counts") %in% ls(env))) {
        showNotification("Example data is corrupted (missing metadata/counts)", type = "error")
        removeNotification("loading_example")
        return()
      }
      
      # Extract metadata and counts
      meta <- as.data.frame(env$metadata)
      meta$group <- as.character(meta$group)
      cnt <- as.data.frame(env$counts)
      
      # Validate data
      if (!all(c("label", "group") %in% colnames(meta))) {
        showNotification("Example data is corrupted (missing label/group)", type = "error")
        removeNotification("loading_example")
        return()
      }
      
      common <- intersect(meta$label, colnames(cnt))
      if (length(common) == 0) {
        showNotification("Example data is corrupted (no matching labels)", type = "error")
        removeNotification("loading_example")
        return()
      }
      
      meta <- meta[meta$label %in% common, ]
      cnt <- cnt %>%
        select(all_of(common)) %>%
        mutate(across(where(is.numeric), ~ as.integer(round(.x))))
      
      # Store data in reactive values
      data_rv$meta <- meta
      data_rv$orig_meta <- meta
      data_rv$counts <- cnt
      data_rv$orig_counts <- cnt
      
      # Reset PCA
      pca_rv$pca_result <- NULL
      pca_rv$pca_data <- NULL
      pca_rv$computed <- FALSE
      
      # Use storage module to save initial data
      file_data <- list(
        metadata = meta,
        counts = cnt
      )
      storage$save_initial_data(file_data, "Example_Data.RData")
      
      # Track example data load
      email$track_change(
        type = "Example Data Loaded",
        details = glue::glue("Loaded example dataset with {nrow(meta)} samples, {nrow(cnt)} genes")
      )
      
      removeNotification("loading_example")
      showNotification(
        glue::glue("Example data loaded successfully!\n{nrow(meta)} samples, {nrow(cnt)} genes, {length(unique(meta$group))} groups"),
        type = "message",
        duration = 5
      )
      
      # Switch to editor tab
      shinyjs::runjs('$("a[data-value=\'editor\']").click();')
      
    }, error = function(e) {
      removeNotification("loading_example")
      showNotification(
        paste("Error loading example data:", e$message),
        type = "error",
        duration = 5
      )
    })
  })

  # =================================================
  # FILE UPLOAD
  # =================================================

  observeEvent(input$file, {
    req(input$file)

    showNotification(
      "Processing file...",
      type = "message",
      duration = NULL,
      id = "processing"
    )

    env <- new.env()
    tryCatch(
      {
        load(input$file$datapath, envir = env)
        if (!all(c("metadata", "counts") %in% ls(env))) {
          showNotification("Need 'metadata' and 'counts'", type = "error")
          return()
        }
        meta <- as.data.frame(env$metadata)
        meta$group <- as.character(meta$group)
        cnt <- as.data.frame(env$counts)

        if (!all(c("label", "group") %in% colnames(meta))) {
          showNotification("metadata needs 'label' and 'group'", type = "error")
          return()
        }

        common <- intersect(meta$label, colnames(cnt))
        if (length(common) == 0) {
          showNotification("No matching labels", type = "error")
          return()
        }

        meta <- meta[meta$label %in% common, ]
        cnt <- cnt %>%
          select(all_of(common)) %>%
          mutate(across(where(is.numeric), ~ as.integer(round(.x))))

        # Store data in reactive values
        data_rv$meta <- meta
        data_rv$orig_meta <- meta
        data_rv$counts <- cnt
        data_rv$orig_counts <- cnt
        
        # Reset PCA on new file upload
        pca_rv$pca_result <- NULL
        pca_rv$pca_data <- NULL
        pca_rv$computed <- FALSE

        # Use storage module to save initial data
        file_data <- list(
          metadata = meta,
          counts = cnt
        )
        storage$save_initial_data(file_data, input$file$name)

        # Track file upload
        email$track_change(
          type = "File Upload",
          details = glue::glue("Uploaded new dataset with {nrow(meta)} samples")
        )

        removeNotification("processing")
        showNotification(
          "File Uploaded Successfully! Data saved with session ID.",
          type = "message"
        )
      },
      error = function(e) {
        removeNotification("processing")
        showNotification(paste("Error:", e$message), type = "error")
      }
    )
  })

  # =================================================
  # INITIALIZE TABLE MODULES (OUTSIDE RENDERUI)
  # =================================================
  
  # Initialize editable table server once at server level
  # This returns a list with $data (reactiveVal) and $deleted_stack (reactiveVal)
  meta_module_out <- editableTableServer(
    "meta",
    data_rv,
    counts_rv,
    storage,
    email
  )
  
  # =================================================
  # HELPER: Get current metadata (edited version)
  # =================================================
  # This reactive DIRECTLY calls the module's data function
  # It will re-run whenever the module's current_data changes
  get_current_metadata <- reactive({
    # Directly call the module's data reactiveVal
    # This creates a proper reactive dependency
    if (!is.null(meta_module_out) && !is.null(meta_module_out$data)) {
      result <- meta_module_out$data()
      if (!is.null(result) && is.data.frame(result) && nrow(result) > 0) {
        return(result)
      }
    }
    
    # Fallback to data_rv$meta (original data)
    return(data_rv$meta)
  })
  
  # Keep meta_out for backward compatibility
  observe({
    meta_out$data <- meta_module_out$data
  })
  
  # Initialize summary server - pass the reactive directly
  summaryServer("summary", get_current_metadata)
  
  # Initialize linked table server - pass the reactive directly and pca_rv for VST plots
  linkedTableServer("counts", counts_rv, get_current_metadata, pca_rv)

  # =================================================
  # RENDER TABLES
  # =================================================

  output$tables_ui <- renderUI({
    req(data_rv$meta, data_rv$counts)

    Sys.sleep(0.5)

    # Just return the UI components - servers are already initialized above
    tagList(
      editableTableUI("meta"),
      linkedTableUI("counts", "Count Matrix (Live)")
    )
  })
  
  # =================================================
  # PCA ANALYSIS
  # =================================================
  
  # Dynamic color variable dropdown
  output$pca_color_ui <- renderUI({
    # Get current metadata using helper function
    current_meta <- get_current_metadata()
    req(current_meta)
    
    # Get all column names from metadata
    color_choices <- colnames(current_meta)
    
    selectInput(
      "pca_color_by",
      "Color by:",
      choices = color_choices,
      selected = if("group" %in% color_choices) "group" else color_choices[1]
    )
  })
  
  # Run PCA analysis
  observeEvent(input$run_pca, {
    req(counts_rv$counts)
    
    showNotification(
      "Running DESeq2 PCA with VST...",
      id = "pca_running",
      duration = NULL,
      type = "message"
    )
    
    tryCatch({
      # Get CURRENT count matrix (reflects latest edits)
      count_matrix <- as.matrix(counts_rv$counts)
      
      # Get current metadata using helper function
      current_meta <- get_current_metadata()
      
      if (is.null(current_meta) || nrow(current_meta) == 0) {
        showNotification(
          "Error: No metadata available. Please upload data first.",
          type = "error",
          duration = 5
        )
        return()
      }
      
      # CRITICAL: Validate that metadata matches count matrix
      sample_labels_counts <- colnames(count_matrix)
      sample_labels_meta <- current_meta$label
      
      # Check for mismatches
      missing_in_meta <- setdiff(sample_labels_counts, sample_labels_meta)
      missing_in_counts <- setdiff(sample_labels_meta, sample_labels_counts)
      
      if (length(missing_in_meta) > 0) {
        showNotification(
          paste("Warning: Samples in counts but not in metadata:", 
                paste(missing_in_meta, collapse = ", ")),
          type = "warning",
          duration = 5
        )
      }
      
      if (length(missing_in_counts) > 0) {
        # Filter metadata to only include samples that exist in count matrix
        current_meta <- current_meta[current_meta$label %in% sample_labels_counts, , drop = FALSE]
      }
      
      # Also filter count matrix to only include samples in metadata
      common_samples <- intersect(sample_labels_counts, current_meta$label)
      if (length(common_samples) == 0) {
        showNotification(
          "Error: No matching samples between metadata and count matrix!",
          type = "error",
          duration = 10
        )
        return()
      }
      
      count_matrix <- count_matrix[, common_samples, drop = FALSE]
      current_meta <- current_meta[current_meta$label %in% common_samples, , drop = FALSE]
      
      cat("PCA using", ncol(count_matrix), "samples\n")
      cat("Metadata has", nrow(current_meta), "samples\n")
      cat("Sample labels:", paste(head(colnames(count_matrix)), collapse = ", "), "...\n")
      
      # Ensure count matrix is integer (DESeq2 requirement)
      count_matrix <- round(count_matrix)
      
      # Handle NA/Inf values
      if (any(is.na(count_matrix)) || any(is.infinite(count_matrix))) {
        showNotification(
          "Warning: Replacing NA/Inf values with 0",
          type = "warning",
          duration = 3
        )
        count_matrix[is.na(count_matrix)] <- 0
        count_matrix[is.infinite(count_matrix)] <- 0
      }
      
      # Ensure all values are non-negative (DESeq2 requirement)
      if (any(count_matrix < 0)) {
        showNotification(
          "Warning: Negative values found, converting to 0",
          type = "warning",
          duration = 3
        )
        count_matrix[count_matrix < 0] <- 0
      }
      
      showNotification(
        "Creating DESeq2 dataset...",
        id = "pca_running",
        duration = NULL,
        type = "message"
      )
      
      # Create a minimal colData (sample information) matching count matrix columns
      # Ensure metadata is in the same order as count matrix columns
      sample_labels <- colnames(count_matrix)
      colData_df <- current_meta[match(sample_labels, current_meta$label), , drop = FALSE]
      rownames(colData_df) <- colData_df$label
      
      # Create DESeqDataSet
      dds <- DESeq2::DESeqDataSetFromMatrix(
        countData = count_matrix,
        colData = colData_df,
        design = ~ 1  # No design needed for VST
      )
      
      showNotification(
        "Estimating size factors and dispersions...",
        id = "pca_running",
        duration = NULL,
        type = "message"
      )
      
      # Estimate size factors (needed for VST)
      dds <- DESeq2::estimateSizeFactors(dds)
      
      # Estimate dispersions with user parameters
      dds <- DESeq2::estimateDispersions(
        dds, 
        fitType = input$vst_fitType,
        maxit = 100
      )
      
      showNotification(
        "Running Variance Stabilizing Transformation...",
        id = "pca_running",
        duration = NULL,
        type = "message"
      )
      
      # Apply Variance Stabilizing Transformation
      # Note: VST only takes 'blind' parameter, not nsub or fitType
      vst_result <- DESeq2::varianceStabilizingTransformation(
        dds,
        blind = input$vst_blind
      )
      
      showNotification(
        "Computing PCA...",
        id = "pca_running",
        duration = NULL,
        type = "message"
      )
      
      # Run PCA using DESeq2's plotPCA function data
      pca_data <- DESeq2::plotPCA(
        vst_result,
        intgroup = "label",  # Just use label, we'll color by user choice later
        returnData = TRUE,
        ntop = input$pca_ntop
      )
      
      # Get the full PCA object for variance explained
      vst_mat <- SummarizedExperiment::assay(vst_result)
      
      # Select top genes by variance
      rv <- matrixStats::rowVars(vst_mat)
      select <- order(rv, decreasing = TRUE)[seq_len(min(input$pca_ntop, length(rv)))]
      
      # Run prcomp on the selected genes
      pca_full <- prcomp(t(vst_mat[select, ]))
      
      # Extract PC coordinates (from plotPCA result, which are already computed)
      pca_coords <- pca_data[, c("PC1", "PC2")]
      
      # Add all other PCs from the full PCA
      if (ncol(pca_full$x) > 2) {
        pca_coords <- cbind(pca_coords, pca_full$x[, 3:ncol(pca_full$x)])
      }
      
      # Store results (includes DDS and VST for later saving)
      pca_rv$pca_result <- list(
        pca = pca_full,
        vst = vst_result,
        dds = dds,  # Store DDS object
        pca_data = pca_coords,
        var_explained = pca_full$sdev^2 / sum(pca_full$sdev^2) * 100,
        ntop = input$pca_ntop,
        params_used = list(  # Store actual parameters used
          blind = input$vst_blind,
          fitType = input$vst_fitType,
          ntop = input$pca_ntop
        )
      )
      pca_rv$pca_data <- as.data.frame(pca_coords)
      pca_rv$computed <- TRUE
      
      removeNotification("pca_running")
      showNotification(
        "DESeq2 PCA analysis complete!",
        type = "message",
        duration = 3
      )
      
      # Track PCA run in email system with ALL parameters
      pca_params_text <- paste(
        sprintf("method=DESeq2_VST"),
        sprintf("blind=%s", input$vst_blind),
        sprintf("fitType=%s", input$vst_fitType),
        sprintf("ntop=%d", input$pca_ntop),
        sprintf("samples=%d", ncol(count_matrix)),
        sprintf("total_features=%d", nrow(count_matrix)),
        sprintf("features_used=%d", length(select)),
        sprintf("PCs_computed=%d", ncol(pca_full$x)),
        sep=", "
      )
      
      email$track_change(
        type = "PCA Analysis (DESeq2)",
        details = glue::glue(
          "Ran DESeq2 PCA with VST on current data\nParameters: {pca_params_text}"
        )
      )
      
    }, error = function(e) {
      removeNotification("pca_running")
      showNotification(
        paste("PCA failed:", e$message),
        type = "error",
        duration = 10
      )
      cat("PCA Error Details:\n", e$message, "\n")
      pca_rv$computed <- FALSE
    })
  })
  
  # Output flag for conditional panel
  output$pca_computed <- reactive({
    pca_rv$computed
  })
  outputOptions(output, "pca_computed", suspendWhenHidden = FALSE)
  
  # Show download modal
  observeEvent(input$open_download_modal, {
    showModal(download_modal)
  })
  
  # Download PCA plot handler
  output$download_pca_plot <- downloadHandler(
    filename = function() {
      filename <- if (!is.null(input$plot_download_filename) && input$plot_download_filename != "") {
        input$plot_download_filename
      } else {
        "pca_plot"
      }
      format <- if (!is.null(input$plot_download_format)) {
        input$plot_download_format
      } else {
        "png"
      }
      paste0(filename, ".", format)
    },
    content = function(file) {
      req(pca_rv$pca_result, input$pca_color_by)
      
      # Get current metadata using helper function
      current_meta <- get_current_metadata()
      req(current_meta)
      
      # Get PC indices
      pc_x <- as.integer(input$pca_pc_x)
      pc_y <- as.integer(input$pca_pc_y)
      
      # Get PCA data
      pca_data <- pca_rv$pca_data
      pca_data$sample <- rownames(pca_data)
      
      # Merge with metadata
      if (input$pca_color_by == "label") {
        plot_data <- pca_data
        plot_data$label <- plot_data$sample
      } else {
        plot_data <- merge(
          pca_data,
          current_meta,
          by.x = "sample",
          by.y = "label",
          all.x = TRUE
        )
      }
      
      # Get color variable
      color_var <- plot_data[[input$pca_color_by]]
      
      # Get variance explained
      var_explained <- pca_rv$pca_result$var_explained
      
      # Get customization options
      plot_title <- if (!is.null(input$plot_title)) input$plot_title else "PCA Score Plot"
      plot_xlabel <- sprintf("PC%d (%.1f%% variance)", pc_x, var_explained[pc_x])
      plot_ylabel <- sprintf("PC%d (%.1f%% variance)", pc_y, var_explained[pc_y])
      point_size <- if (!is.null(input$plot_point_size)) input$plot_point_size else 20
      point_opacity <- if (!is.null(input$plot_point_opacity)) input$plot_point_opacity else 1
      show_grid <- if (!is.null(input$plot_show_grid)) input$plot_show_grid else TRUE
      show_legend <- if (!is.null(input$plot_show_legend)) input$plot_show_legend else TRUE
      bg_color <- if (!is.null(input$plot_bg_color)) input$plot_bg_color else "#fffef0"
      
      # Get number of unique groups for color generation
      n_colors <- length(unique(color_var))
      
      # 3-color gradient interpolation - works for any number of groups
      c1 <- if (!is.null(input$gradient_color1)) input$gradient_color1 else "#636EFA"
      c2 <- if (!is.null(input$gradient_color2)) input$gradient_color2 else "#EF553B"
      c3 <- if (!is.null(input$gradient_color3)) input$gradient_color3 else "#00CC96"
      color_values <- colorRampPalette(c(c1, c2, c3))(n_colors)
      
      # Create plot
      pc_x_col <- paste0("PC", pc_x)
      pc_y_col <- paste0("PC", pc_y)
      
      p <- plot_ly(
        plot_data,
        x = ~get(pc_x_col),
        y = ~get(pc_y_col),
        color = color_var,
        colors = color_values,
        type = "scatter",
        mode = "markers",
        marker = list(
          size = point_size, 
          opacity = point_opacity,
          line = list(color = "white", width = 1)
        ),
        text = ~paste(
          "Sample:", sample,
          "<br>", input$pca_color_by, ":", color_var
        ),
        hoverinfo = "text",
        width = input$plot_download_width %||% 1200,
        height = input$plot_download_height %||% 800
      ) %>%
        layout(
          title = list(
            text = plot_title,
            y = 0.98,
            x = 0.5,
            xanchor = "center",
            yanchor = "top",
            font = list(size = 18)
          ),
          xaxis = list(
            title = plot_xlabel,
            zeroline = TRUE,
            showgrid = show_grid
          ),
          yaxis = list(
            title = plot_ylabel,
            zeroline = TRUE,
            showgrid = show_grid
          ),
          hovermode = "closest",
          plot_bgcolor = bg_color,
          paper_bgcolor = "white",
          showlegend = show_legend,
          margin = list(
            t = 100,
            b = 80,
            l = 80,
            r = 60,
            pad = 10
          )
        )
      
      # Get download dimensions and format
      width <- if (!is.null(input$plot_download_width)) {
        input$plot_download_width
      } else {
        1200
      }
      
      height <- if (!is.null(input$plot_download_height)) {
        input$plot_download_height
      } else {
        800
      }
      
      format <- if (!is.null(input$plot_download_format)) {
        input$plot_download_format
      } else {
        "png"
      }
      
      # Get format
      format <- input$plot_download_format %||% "png"
      
      # Set plot dimensions
      p <- p %>% 
        plotly::config(
          toImageButtonOptions = list(
            width = input$plot_download_width %||% 1200,
            height = input$plot_download_height %||% 800
          )
        )
      
      # Export based on format
      if (format == "html") {
        # HTML - always works, interactive
        htmlwidgets::saveWidget(plotly::as_widget(p), file, selfcontained = TRUE)
        removeModal()
        showNotification("Plot downloaded as interactive HTML", type = "message", duration = 3)
      } else {
        # PNG - use webshot2 for reliable screenshot
        if (!requireNamespace("webshot2", quietly = TRUE)) {
          # Fallback to HTML if webshot2 not available
          html_file <- paste0(tools::file_path_sans_ext(file), ".html")
          htmlwidgets::saveWidget(plotly::as_widget(p), html_file, selfcontained = TRUE)
          file.copy(html_file, file)
          file.remove(html_file)
          removeModal()
          showNotification(
            "webshot2 package not installed. Saved as HTML. Install with: install.packages('webshot2')",
            type = "warning",
            duration = 8
          )
        } else {
          # Use webshot2 to capture PNG
          temp_html <- tempfile(fileext = ".html")
          htmlwidgets::saveWidget(plotly::as_widget(p), temp_html, selfcontained = TRUE)
          
          webshot2::webshot(
            temp_html, 
            file = file,
            vwidth = input$plot_download_width %||% 1200,
            vheight = input$plot_download_height %||% 800,
            delay = 1
          )
          
          file.remove(temp_html)
          removeModal()
          showNotification("Plot downloaded as PNG", type = "message", duration = 3)
        }
      }
    }
  )
  
  # Reset plot options to defaults
  observeEvent(input$reset_plot_options, {
    updateTextInput(session, "plot_title", value = "PCA Score Plot")
    updateSliderInput(session, "plot_point_size", value = 20)
    updateSliderInput(session, "plot_point_opacity", value = 1)
    colourpicker::updateColourInput(session, "gradient_color1", value = "#636EFA")
    colourpicker::updateColourInput(session, "gradient_color2", value = "#EF553B")
    colourpicker::updateColourInput(session, "gradient_color3", value = "#00CC96")
    updateCheckboxInput(session, "plot_show_grid", value = TRUE)
    updateCheckboxInput(session, "plot_show_legend", value = TRUE)
    updateSelectInput(session, "plot_bg_color", selected = "#fffef0")
    
    showNotification("Plot options reset to defaults", type = "message", duration = 2)
  })
  
  # =================================================
  # PCA DATA EXPORT HANDLERS
  # =================================================
  
  # Download eigenvalues/variance explained
  output$download_eigenvalues <- downloadHandler(
    filename = function() {
      paste0("pca_eigenvalues_", Sys.Date(), ".csv")
    },
    content = function(file) {
      req(pca_rv$pca_result)
      
      # Get PCA object
      pca_obj <- pca_rv$pca_result$pca
      var_explained <- pca_rv$pca_result$var_explained
      
      # Create comprehensive eigenvalue table
      eigenvalue_data <- data.frame(
        PC = paste0("PC", seq_along(pca_obj$sdev)),
        Eigenvalue = pca_obj$sdev^2,
        Standard_Deviation = pca_obj$sdev,
        Variance_Percent = round(var_explained, 4),
        Cumulative_Variance_Percent = round(cumsum(var_explained), 4)
      )
      
      write.csv(eigenvalue_data, file, row.names = FALSE)
      
      showNotification(
        paste("Exported", nrow(eigenvalue_data), "principal components"),
        type = "message",
        duration = 3
      )
    }
  )
  
  # Download PCA scores joined with metadata
  output$download_pca_scores <- downloadHandler(
    filename = function() {
      paste0("pca_scores_with_metadata_", Sys.Date(), ".csv")
    },
    content = function(file) {
      req(pca_rv$pca_result, pca_rv$pca_data)
      
      # Get current metadata using helper function
      current_meta <- get_current_metadata()
      req(current_meta)
      
      # Get PCA scores
      pca_scores <- pca_rv$pca_data
      pca_scores$sample <- rownames(pca_scores)
      
      # Join PCA scores with metadata
      # Metadata label column matches PCA sample rownames
      export_data <- merge(
        current_meta,
        pca_scores,
        by.x = "label",
        by.y = "sample",
        all.x = FALSE,  # Only keep samples that have PCA scores
        all.y = TRUE    # Keep all PCA samples
      )
      
      # Reorder columns: metadata first, then PC columns
      pc_cols <- grep("^PC[0-9]+$", colnames(export_data), value = TRUE)
      meta_cols <- setdiff(colnames(export_data), pc_cols)
      export_data <- export_data[, c(meta_cols, pc_cols)]
      
      write.csv(export_data, file, row.names = FALSE)
      
      showNotification(
        paste("Exported", nrow(export_data), "samples with", length(pc_cols), "PCs and metadata"),
        type = "message",
        duration = 3
      )
    }
  )
  
  # PCA Score Plot
  output$pca_plot <- renderPlotly({
    req(pca_rv$pca_result, input$pca_color_by)
    
    # Get current metadata using helper function
    current_meta <- get_current_metadata()
    req(current_meta)
    
    # Get PC indices
    pc_x <- as.integer(input$pca_pc_x)
    pc_y <- as.integer(input$pca_pc_y)
    
    # Get PCA data from the stored result
    pca_data <- pca_rv$pca_data
    
    # Ensure we have enough PCs
    max_pc <- max(pc_x, pc_y)
    if (ncol(pca_data) < max_pc) {
      return(plotly_empty() %>% 
        layout(title = "Not enough principal components available"))
    }
    
    # Match samples with metadata
    pca_data$sample <- rownames(pca_data)
    
    # Merge with metadata - handle when color_by is "label"
    if (input$pca_color_by == "label") {
      # If coloring by label, just add it from sample column
      plot_data <- pca_data
      plot_data$label <- plot_data$sample
    } else {
      # Merge with metadata for other columns
      plot_data <- merge(
        pca_data,
        current_meta,
        by.x = "sample",
        by.y = "label",
        all.x = TRUE
      )
    }
    
    # Get color variable
    color_var <- plot_data[[input$pca_color_by]]
    
    # Get variance explained from stored result
    var_explained <- pca_rv$pca_result$var_explained
    
    # Get customization options (with defaults if not set)
    plot_title <- if (!is.null(input$plot_title)) input$plot_title else "PCA Score Plot"
    # Always show variance percentage on axes
    plot_xlabel <- sprintf("PC%d (%.1f%% variance)", pc_x, var_explained[pc_x])
    plot_ylabel <- sprintf("PC%d (%.1f%% variance)", pc_y, var_explained[pc_y])
    point_size <- if (!is.null(input$plot_point_size)) input$plot_point_size else 20
    point_opacity <- if (!is.null(input$plot_point_opacity)) input$plot_point_opacity else 1
    show_grid <- if (!is.null(input$plot_show_grid)) input$plot_show_grid else TRUE
    show_legend <- if (!is.null(input$plot_show_legend)) input$plot_show_legend else TRUE
    bg_color <- if (!is.null(input$plot_bg_color)) input$plot_bg_color else "#fffef0"
    
    # Get number of unique groups for color generation
    n_colors <- length(unique(color_var))
    
    # 3-color gradient interpolation - works for any number of groups
    c1 <- if (!is.null(input$gradient_color1)) input$gradient_color1 else "#636EFA"
    c2 <- if (!is.null(input$gradient_color2)) input$gradient_color2 else "#EF553B"
    c3 <- if (!is.null(input$gradient_color3)) input$gradient_color3 else "#00CC96"
    color_values <- colorRampPalette(c(c1, c2, c3))(n_colors)
    
    # Create plot
    pc_x_col <- paste0("PC", pc_x)
    pc_y_col <- paste0("PC", pc_y)
    
    p <- plot_ly(
      plot_data,
      x = ~get(pc_x_col),
      y = ~get(pc_y_col),
      color = color_var,
      colors = color_values,
      type = "scatter",
      mode = "markers",
      marker = list(
        size = point_size, 
        opacity = point_opacity,
        line = list(color = "white", width = 1)
      ),
      text = ~paste(
        "Sample:", sample,
        "<br>", input$pca_color_by, ":", color_var
      ),
      hoverinfo = "text"
    ) %>%
      layout(
        title = list(
          text = plot_title,
          y = 0.98,
          x = 0.5,
          xanchor = "center",
          yanchor = "top",
          font = list(size = 16)
        ),
        xaxis = list(
          title = plot_xlabel,
          zeroline = TRUE,
          showgrid = show_grid
        ),
        yaxis = list(
          title = plot_ylabel,
          zeroline = TRUE,
          showgrid = show_grid
        ),
        hovermode = "closest",
        plot_bgcolor = bg_color,
        paper_bgcolor = "white",
        showlegend = show_legend,
        autosize = TRUE,
        margin = list(
          t = 80,
          b = 60,
          l = 60,
          r = 40,
          pad = 10
        )
      ) %>%
      config(
        displayModeBar = FALSE
      )
    
    p
  })
  
  # Scree Plot
  output$pca_scree <- renderPlotly({
    req(pca_rv$pca_result)
    
    # Get variance explained from stored result
    var_explained <- pca_rv$pca_result$var_explained
    
    # Create data frame
    scree_data <- data.frame(
      PC = 1:length(var_explained),
      Variance = var_explained,
      Cumulative = cumsum(var_explained)
    )
    
    # Limit to first 20 PCs for readability
    if (nrow(scree_data) > 20) {
      scree_data <- scree_data[1:20, ]
    }
    
    p <- plot_ly(scree_data) %>%
      add_bars(
        x = ~PC,
        y = ~Variance,
        name = "Individual",
        marker = list(color = "#0dcaf0")
      ) %>%
      add_lines(
        x = ~PC,
        y = ~Cumulative,
        name = "Cumulative",
        yaxis = "y2",
        line = list(color = "#dc3545", width = 3)
      ) %>%
      layout(
        title = list(
          text = "Variance Explained by Principal Components (DESeq2 VST)",
          y = 0.98,
          x = 0.5,
          xanchor = "center",
          yanchor = "top",
          font = list(size = 14)
        ),
        xaxis = list(title = "Principal Component", dtick = 1),
        yaxis = list(
          title = "Variance Explained (%)",
          side = "left"
        ),
        yaxis2 = list(
          title = "Cumulative Variance (%)",
          overlaying = "y",
          side = "right"
        ),
        hovermode = "x unified",
        plot_bgcolor = "#f8f9fa",
        paper_bgcolor = "white",
        showlegend = TRUE,
        autosize = TRUE,
        margin = list(
          t = 60,
          b = 50,
          l = 60,
          r = 60,
          pad = 5
        )
      ) %>%
      config(
        displayModeBar = TRUE,
        displaylogo = FALSE,
        modeBarButtonsToRemove = c("select2d", "lasso2d", "autoScale2d"),
        toImageButtonOptions = list(
          format = "png",
          filename = "scree_plot",
          height = 600,
          width = 1200
        )
      )
    
    p
  })
  
  # PCA Summary
  output$pca_summary <- renderPrint({
    req(pca_rv$pca_result)
    
    cat("DESeq2 PCA Summary\n")
    cat("==================\n\n")
    
    cat("Method: Variance Stabilizing Transformation (VST)\n\n")
    
    # Get variance info
    var_explained <- pca_rv$pca_result$var_explained
    pca_obj <- pca_rv$pca_result$pca
    
    cat("Data Dimensions:\n")
    cat("  Number of samples:", nrow(pca_obj$x), "\n")
    cat("  Top genes used:", pca_rv$pca_result$ntop, "\n")
    cat("  Number of PCs computed:", ncol(pca_obj$x), "\n\n")
    
    cat("Variance Explained (%):\n")
    n_show <- min(10, length(var_explained))
    for (i in 1:n_show) {
      cat(sprintf("  PC%d: %.2f%%\n", i, var_explained[i]))
    }
    
    if (length(var_explained) > n_show) {
      cat(sprintf("  ... and %d more PCs\n", length(var_explained) - n_show))
    }
    
    cat(sprintf("\nTotal variance explained by first %d PCs: %.2f%%\n",
                n_show, sum(var_explained[1:n_show])))
    
    cat("\n")
    cat("=", rep("=", 40), "\n", sep = "")
    cat("Parameters Used for This PCA:\n")
    cat("=" , rep("=", 40), "\n", sep = "")
    
    # Check if this is a restored PCA (no current input values match)
    is_restored <- is.null(input$vst_blind) || 
                   is.null(input$vst_fitType) || 
                   is.null(input$pca_ntop)
    
    if (is_restored) {
      cat("\n⚠️  RESTORED FROM SAVED SESSION\n")
      cat("The PCA below was computed in a previous session.\n")
      cat("Current sidebar settings may differ from what was used.\n\n")
    }
    
    cat("\nVST Transformation:\n")
    cat("  Blind to design:", 
        if(!is.null(pca_rv$pca_result$params_used$blind)) 
          pca_rv$pca_result$params_used$blind 
        else "TRUE (assumed)", 
        "\n")
    cat("  Fit type:", 
        if(!is.null(pca_rv$pca_result$params_used$fitType)) 
          pca_rv$pca_result$params_used$fitType 
        else "parametric (assumed)", 
        "\n")
    
    cat("\nGene Selection:\n")
    cat("  Top variable genes (ntop):", pca_rv$pca_result$ntop, "\n")
    
    cat("\nPlot Display:\n")
    cat("  X-axis: PC", input$pca_pc_x, "\n")
    cat("  Y-axis: PC", input$pca_pc_y, "\n")
    cat("  Color by:", input$pca_color_by, "\n")
    
    cat("\n")
    cat("-", rep("-", 40), "\n", sep = "")
    cat("Default VST Parameters:\n")
    cat("-", rep("-", 40), "\n", sep = "")
    cat("Current sidebar settings (for next PCA run):\n")
    cat("  Blind:", if(!is.null(input$vst_blind)) input$vst_blind else "TRUE", "\n")
    cat("  Fit type:", if(!is.null(input$vst_fitType)) input$vst_fitType else "parametric", "\n")
    cat("  Top genes:", if(!is.null(input$pca_ntop)) input$pca_ntop else "500", "\n")
  })
}

shinyApp(ui, server)