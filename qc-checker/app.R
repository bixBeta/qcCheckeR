# RNA-Seq Editor — Fully Integrated with Reusable Modules
# With Session Storage and Email Notifications
# Version: 2.1.0

# Environment setup with package installation
required_packages <- c(
  "shiny",
  "bslib",
  "jsonlite",
  "reactable",
  "dplyr",
  "tidyr",
  "tibble",
  "rlang",
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
  "colourpicker",
  "heatmaply",
  "DT",
  "RColorBrewer",
  "gprofiler2",
  "digest"
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

# Set maximum upload size to 3GB (for large GTF files)
options(shiny.maxRequestSize = 3 * 1024 * 1024^2) # 3GB in bytes

# =================================================
# SOURCE THE REUSABLE MODULES
# =================================================
# Use shared modules from the pipeline's shared directory
# Adjust path based on your deployment structure
SHARED_PATH <- Sys.getenv("PIPELINE_SHARED_PATH", "../shared")

source(file.path(SHARED_PATH, "module_user_sessions.R"))
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
          plotOutput(ns("boxplot_raw"), height = "700px")
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
editableTableServer <- function(id, data_rv, counts_rv, storage = NULL, email) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    current_data <- reactiveVal()
    deleted_stack <- reactiveVal(list())
    last_data_id <- reactiveVal(NULL) # Track data identity to detect changes
    render_counter <- reactiveVal(0) # Force re-render on data change

    # React to data_rv$meta changes - this is critical for loading new data
    observe({
      meta <- data_rv$meta

      # Handle NULL case (data was reset)
      if (is.null(meta)) {
        if (!is.null(current_data())) {
          current_data(NULL)
          deleted_stack(list())
          last_data_id(NULL)
          render_counter(render_counter() + 1)
        }
        return()
      }

      # Create a robust identity hash including actual data content
      new_data_id <- paste(
        nrow(meta),
        ncol(meta),
        paste(head(meta$label, 5), collapse = ","),
        paste(tail(meta$label, 2), collapse = ","),
        digest::digest(meta, algo = "xxhash32"),
        sep = "_"
      )

      # Update current_data if:
      # 1. It's NULL (first load)
      # 2. The underlying data has changed (new file/session loaded)
      if (
        is.null(current_data()) ||
          is.null(last_data_id()) ||
          new_data_id != last_data_id()
      ) {
        current_data(meta)
        deleted_stack(list()) # Clear undo stack when data changes
        last_data_id(new_data_id)
        render_counter(render_counter() + 1) # Force table re-render
      }
    })

    output$table <- renderReactable({
      req(current_data())
      # Depend on render_counter to force re-render
      render_counter()

      reactable(
        current_data(),
        highlight = TRUE,
        bordered = TRUE,
        striped = TRUE,
        compact = TRUE,
        resizable = TRUE,
        defaultPageSize = 10,
        showPageSizeOptions = TRUE,
        pageSizeOptions = c(10, 25, 50),
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
      counts_rv$counts <- counts_rv$orig_counts # Use counts_rv$orig_counts for consistency
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
      "#6BAED6", # Light blue
      "#FC8D62", # Orange
      "#66C2A5", # Teal/Green
      "#E78AC3", # Pink
      "#A6D854", # Lime green
      "#FFD92F", # Yellow
      "#E5C494", # Tan
      "#B3B3B3", # Gray
      "#8DA0CB", # Periwinkle
      "#E41A1C", # Red
      "#377EB8", # Blue
      "#4DAF4A" # Green
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
      if (
        !is.null(metadata) &&
          "label" %in% colnames(metadata) &&
          "group" %in% colnames(metadata)
      ) {
        df_long <- df_long %>%
          left_join(
            metadata %>% select(label, group),
            by = c("sample" = "label")
          )
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
    output$boxplot_raw <- renderPlot(
      {
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
          geom_boxplot(
            width = 0.15,
            fill = "white",
            alpha = 0.8,
            outlier.size = 0.5,
            outlier.alpha = 0.3
          ) +
          scale_fill_manual(values = group_colors) +
          labs(
            title = "Raw Counts Distribution",
            x = "Sample",
            y = expression(log[2](count + 1)),
            fill = "Group"
          ) +
          theme_qc()
      },
      res = 96
    )

    # VST box plot UI - conditional on PCA being computed
    output$boxplot_vst_ui <- renderUI({
      if (
        is.null(pca_rv) ||
          !isTRUE(pca_rv$computed) ||
          is.null(pca_rv$pca_result$vst)
      ) {
        tags$div(
          style = "text-align: center; padding: 50px; color: #666;",
          tags$i(
            class = "fa fa-info-circle",
            style = "font-size: 2em; margin-bottom: 10px;"
          ),
          tags$p("VST counts not available yet."),
          tags$p(
            "Run PCA analysis first to generate variance-stabilized counts."
          )
        )
      } else {
        plotOutput(ns("boxplot_vst"), height = "700px")
      }
    })

    # VST counts violin + box plot - static ggplot2
    output$boxplot_vst <- renderPlot(
      {
        req(pca_rv$computed, pca_rv$pca_result$vst)

        # Extract VST counts
        vst_counts <- SummarizedExperiment::assay(pca_rv$pca_result$vst)
        cur_meta <- meta_data()

        # Filter to current samples
        if (
          !is.null(cur_meta) &&
            nrow(cur_meta) > 0 &&
            "label" %in% colnames(cur_meta)
        ) {
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
        if (
          !is.null(cur_meta) &&
            "label" %in% colnames(cur_meta) &&
            "group" %in% colnames(cur_meta)
        ) {
          df_long <- df_long %>%
            left_join(
              cur_meta %>% select(label, group),
              by = c("sample" = "label")
            )
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
          geom_boxplot(
            width = 0.15,
            fill = "white",
            alpha = 0.8,
            outlier.size = 0.5,
            outlier.alpha = 0.3
          ) +
          scale_fill_manual(values = group_colors) +
          labs(
            title = "VST Normalized Counts Distribution",
            x = "Sample",
            y = "VST counts",
            fill = "Group"
          ) +
          theme_qc()
      },
      res = 96
    )

    # Side-by-side comparison UI (SARTools style - raw vs normalized)
    output$comparison_ui <- renderUI({
      if (
        is.null(pca_rv) ||
          !isTRUE(pca_rv$computed) ||
          is.null(pca_rv$pca_result$vst)
      ) {
        tags$div(
          style = "text-align: center; padding: 50px; color: #666;",
          tags$i(
            class = "fa fa-info-circle",
            style = "font-size: 2em; margin-bottom: 10px;"
          ),
          tags$p("Comparison not available yet."),
          tags$p(
            "Run PCA analysis first to compare raw vs VST-normalized counts."
          )
        )
      } else {
        plotOutput(ns("comparison_plot"), height = "700px")
      }
    })

    # Side-by-side comparison plot (like SARTools countsBoxplots.png)
    output$comparison_plot <- renderPlot(
      {
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
          tidyr::pivot_longer(
            cols = -gene,
            names_to = "sample",
            values_to = "count"
          ) %>%
          mutate(value = log2(count + 1), type = "Raw (log2 + 1)") %>%
          select(gene, sample, value, type)

        # Create VST counts long format
        vst_df <- as.data.frame(vst_counts) %>%
          tibble::rownames_to_column("gene") %>%
          tidyr::pivot_longer(
            cols = -gene,
            names_to = "sample",
            values_to = "count"
          ) %>%
          mutate(value = count, type = "VST Normalized") %>%
          select(gene, sample, value, type)

        # Combine
        plot_df <- bind_rows(raw_df, vst_df)

        # Add group information
        plot_df <- plot_df %>%
          left_join(
            cur_meta %>% select(label, group),
            by = c("sample" = "label")
          )

        # Order samples and types
        sample_order <- cur_meta %>% arrange(group, label) %>% pull(label)
        plot_df$sample <- factor(plot_df$sample, levels = sample_order)
        plot_df$type <- factor(
          plot_df$type,
          levels = c("Raw (log2 + 1)", "VST Normalized")
        )

        # Get colors
        unique_groups <- unique(cur_meta$group)
        n_groups <- length(unique_groups)
        group_colors <- get_group_colors(n_groups, unique_groups)

        # Create faceted violin + box plot comparison
        ggplot(plot_df, aes(x = sample, y = value, fill = group)) +
          geom_violin(trim = FALSE, scale = "width", alpha = 0.7) +
          geom_boxplot(
            width = 0.15,
            fill = "white",
            alpha = 0.8,
            outlier.size = 0.3,
            outlier.alpha = 0.2
          ) +
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
      },
      res = 96
    )

    output$table <- renderReactable({
      req(dynamic_data())
      reactable(
        dynamic_data(),
        resizable = TRUE,
        compact = TRUE,
        defaultPageSize = 15,
        showPageSizeOptions = TRUE,
        pageSizeOptions = c(15, 50, 100),
        searchable = TRUE,
        filterable = FALSE,
        striped = TRUE,
        highlight = TRUE,
        bordered = TRUE,
        theme = reactableTheme(
          stripedColor = "#f8f9fa",
          searchInputStyle = list(width = "200px")
        )
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
      tags$link(
        href = "https://fonts.googleapis.com/css2?family=Stack+Sans+Headline:wght@200..700&display=swap",
        rel = "stylesheet"
      ),
      tags$script(HTML(
        "
        $(document).ready(function() {
          // Track unsaved changes
          window.hasUnsavedChanges = false;
          
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
          
          // Warn user before leaving if there are unsaved changes
          $(window).on('beforeunload', function(e) {
            if (window.hasUnsavedChanges) {
              var message = 'You have unsaved changes. Are you sure you want to leave?';
              e.returnValue = message;
              return message;
            }
          });
          
          // Listen for Shiny to tell us about unsaved changes
          Shiny.addCustomMessageHandler('setUnsavedChanges', function(hasChanges) {
            window.hasUnsavedChanges = hasChanges;
            // Update save button appearance
            if (hasChanges) {
              $('#save_now_global').removeClass('btn-success').addClass('btn-warning');
              $('#save_now_global').html('<i class=\"fa fa-save\"></i> Save Now *');
            } else {
              $('#save_now_global').removeClass('btn-warning').addClass('btn-success');
              $('#save_now_global').html('<i class=\"fa fa-save\"></i> Save Now');
            }
          });
          
          // Force UI refresh handler - fixes scroll freezing issues
          Shiny.addCustomMessageHandler('forceUIRefresh', function(msg) {
            console.log('Forcing UI refresh...');
            
            // Reset scroll position on all scrollable elements
            $('html, body').scrollTop(0);
            $('.tab-content').scrollTop(0);
            $('.card-body').scrollTop(0);
            $('[style*=\"overflow\"]').scrollTop(0);
            
            // Force reflow/repaint
            document.body.style.display = 'none';
            document.body.offsetHeight; // Trigger reflow
            document.body.style.display = '';
            
            // Clear any stuck Plotly plots
            if (window.Plotly) {
              $('.js-plotly-plot').each(function() {
                try {
                  Plotly.purge(this);
                } catch(e) {}
              });
            }
            
            // Rebind Shiny inputs/outputs after a short delay
            setTimeout(function() {
              Shiny.bindAll(document.body);
            }, 100);
            
            console.log('UI refresh complete');
          });
        });
      "
      )),
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

        # Step 1: User Identification
        card(
          card_header(
            tagList(
              span(class = "badge bg-primary me-2", "1"),
              "Identify Yourself"
            )
          ),
          card_body(
            p(
              "Enter your email to access your saved sessions across all pipeline apps."
            ),
            fluidRow(
              column(
                6,
                textInput(
                  "user_email",
                  label = tagList(icon("envelope"), "Your Email"),
                  placeholder = "you@institution.edu",
                  width = "100%"
                ),
                uiOutput("email_validation_status")
              ),
              column(
                6,
                div(
                  class = "mt-4",
                  style = "padding: 10px; background: #f8f9fa; border-radius: 6px;",
                  icon("shield-halved", class = "text-info"),
                  span(
                    class = "text-muted small",
                    " Your sessions are protected with a PIN. Only you can access your data."
                  )
                )
              )
            ),

            # PIN Entry (for existing users)
            conditionalPanel(
              condition = "output.needs_pin_entry",
              hr(),
              div(
                style = "background: #e8f4fd; border: 1px solid #b8daff; border-radius: 8px; padding: 20px;",
                h5(icon("lock"), " Enter Your PIN"),
                fluidRow(
                  column(
                    6,
                    passwordInput(
                      "pin_entry",
                      label = "PIN (4-6 digits)",
                      placeholder = "••••",
                      width = "100%"
                    )
                  ),
                  column(
                    6,
                    div(
                      style = "margin-top: 32px;",
                      actionButton(
                        "verify_pin",
                        "Unlock",
                        icon = icon("unlock"),
                        class = "btn-primary",
                        width = "100%"
                      )
                    )
                  )
                ),
                tags$small(
                  class = "text-muted",
                  icon("info-circle"),
                  " 5 failed attempts will lock your account for 30 minutes."
                )
              )
            ),

            # PIN Creation (for new users or users without PIN)
            conditionalPanel(
              condition = "output.needs_pin_creation",
              hr(),
              div(
                style = "background: #e8f5e9; border: 1px solid #c3e6cb; border-radius: 8px; padding: 20px;",
                h5(icon("key"), " Create Your PIN"),
                p(
                  class = "text-muted small",
                  "Choose a 4-6 digit PIN to secure your sessions."
                ),
                fluidRow(
                  column(
                    4,
                    passwordInput(
                      "pin_create",
                      label = "Create PIN",
                      placeholder = "••••",
                      width = "100%"
                    )
                  ),
                  column(
                    4,
                    passwordInput(
                      "pin_confirm",
                      label = "Confirm PIN",
                      placeholder = "••••",
                      width = "100%"
                    )
                  ),
                  column(
                    4,
                    div(
                      style = "margin-top: 32px;",
                      actionButton(
                        "create_pin",
                        "Create PIN",
                        icon = icon("check"),
                        class = "btn-success",
                        width = "100%"
                      )
                    )
                  )
                ),
                tags$small(
                  class = "text-muted",
                  icon("info-circle"),
                  " Remember your PIN! You'll need it to access your sessions."
                )
              )
            )
          )
        ),

        br(),

        # Step 2: Session Selection (shown after PIN verified)
        conditionalPanel(
          condition = "output.user_authenticated",
          card(
            card_header(
              tagList(
                span(class = "badge bg-primary me-2", "2"),
                "Load or Start Session"
              )
            ),
            card_body(
              fluidRow(
                column(
                  6,
                  h6(icon("folder-open"), " Load Existing Session"),
                  uiOutput("existing_sessions_ui"),
                  br(),
                  actionButton(
                    "refresh_sessions",
                    "Refresh List",
                    icon = icon("sync"),
                    class = "btn-outline-secondary btn-sm"
                  )
                ),
                column(
                  6,
                  h6(icon("plus"), " Start Fresh"),
                  p(
                    class = "text-muted small",
                    "Upload a new .RData file to begin analysis"
                  ),
                  actionButton(
                    "start_new",
                    "New Session",
                    icon = icon("plus"),
                    class = "btn-success",
                    width = "100%"
                  )
                )
              )
            )
          ),

          br(),

          # Current Session Info
          uiOutput("current_session_card")
        ),

        br(),

        # Try Example Data - Always visible (no authentication required)
        card(
          card_header(
            class = "bg-light",
            tagList(
              icon("flask", class = "text-primary"),
              " Try Without Signing Up"
            )
          ),
          card_body(
            h6(icon("flask"), " Load Example Data"),
            p(
              class = "text-muted small",
              "20 samples, 4 groups - explore the app without an account"
            ),
            actionButton(
              "load_example",
              "Load Example",
              icon = icon("flask"),
              class = "btn-secondary",
              width = "20%"
            ),
            br(),
            br(),
            tags$small(
              class = "text-muted",
              icon("info-circle"),
              " Note: Example data won't be saved. Sign up to save your work."
            )
          )
        ),

        br(),

        # Help section
        card(
          card_header("How it works"),
          card_body(
            class = "small",
            tags$ul(
              tags$li(
                tags$strong("PIN-protected sessions: "),
                "Your sessions are secured with a 4-6 digit PIN that only you know"
              ),
              tags$li(
                tags$strong("Email-based access: "),
                "Your email links all your sessions across the RNA-Seq pipeline"
              ),
              tags$li(
                tags$strong("Manual save: "),
                "Click 'Save Now' to save your session (auto-save disabled)"
              ),
              tags$li(
                tags$strong("Cross-app workflow: "),
                "After QC, send your data directly to DEG-Explorer"
              ),
              tags$li(
                tags$strong("Undo support: "),
                "All changes can be undone with the undo functionality"
              ),
              tags$li(
                tags$strong("Email notifications: "),
                "Enable to receive batched change summaries"
              )
            )
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
        # Email-Based Sessions Info Card
        card(
          card_header(
            class = "bg-info text-white",
            tags$span(icon("user-circle"), " Email-Based Session Management")
          ),
          card_body(
            tags$p(
              style = "font-size: 1.1em;",
              tags$strong("Your sessions are linked to your email address. "),
              "Enter your email on the Start tab to access all your saved sessions. ",
              "Click 'Save Now' to save your work - the button turns ",
              tags$span(style = "color: #ffc107; font-weight: bold;", "yellow"),
              " when you have unsaved changes."
            ),
            tags$p(
              style = "margin-bottom: 0;",
              icon("triangle-exclamation"),
              " ",
              tags$em(
                "Warning: If you close the browser with unsaved changes, you'll see a confirmation dialog."
              )
            )
          )
        ),
        br(),

        # App Overview
        h4(icon("info-circle"), " About QC-CheckeR"),
        tags$p(
          "QC-CheckeR is an interactive Shiny application for RNA-Seq quality control and exploratory data analysis. ",
          "It allows you to upload count matrices, edit sample metadata, visualize count distributions, ",
          "and perform PCA analysis using DESeq2's variance-stabilizing transformation (VST). ",
          "Part of the RNA-Seq Pipeline suite - your QC'd data can be sent directly to DEG-Explorer for differential expression analysis."
        ),
        hr(),

        # Quick Start Guide
        h4(icon("rocket"), " Quick Start Guide"),
        tags$ol(
          tags$li(
            tags$strong("Try Without Signing Up (Optional): "),
            "Click 'Load Example Data' on the Start tab to explore the app with sample RNA-Seq data."
          ),
          tags$li(
            tags$strong("Enter Your Email & PIN: "),
            "To save your work, enter your email and create a 4-6 digit PIN. ",
            "Returning users enter their existing PIN."
          ),
          tags$li(
            tags$strong("Upload Data: "),
            "Upload an ",
            tags$code(".RData"),
            " file containing a ",
            tags$code("counts"),
            " matrix and ",
            tags$code("metadata"),
            " data frame."
          ),
          tags$li(
            tags$strong("Save Your Session: "),
            "Click the green 'Save Now' button to save. The button turns yellow when you have unsaved changes."
          ),
          tags$li(
            tags$strong("Review & Edit Metadata: "),
            "Click any row in the metadata table to edit sample labels and group assignments. ",
            "You can also delete samples (with undo support)."
          ),
          tags$li(
            tags$strong("Annotate Genes (Optional): "),
            "Go to the ",
            tags$em("Annotate Counts"),
            " tab to add gene names and descriptions. ",
            "Use g:Profiler (online) or upload a GTF file for local annotation."
          ),
          tags$li(
            tags$strong("Check Count Distributions: "),
            "View violin plots of raw counts (log2 + 1) in the ",
            tags$em("Raw Counts Violin"),
            " tab. ",
            "This helps identify outlier samples before normalization."
          ),
          tags$li(
            tags$strong("Run PCA Analysis: "),
            "Go to the ",
            tags$em("PCA Analysis"),
            " tab, configure VST parameters, and click ",
            tags$em("Run DESeq2 PCA"),
            ". The plot will update with your samples colored by group."
          ),
          tags$li(
            tags$strong("Send to DEG-Explorer: "),
            "After running PCA, click 'Send to DEG-Explorer' to continue with differential expression analysis."
          )
        ),
        hr(),

        # Data Requirements
        h4(icon("database"), " Data Requirements"),
        tags$p("Your ", tags$code(".RData"), " file must contain:"),
        tags$ul(
          tags$li(
            tags$code("counts"),
            " — A count matrix (genes × samples). Column names must match ",
            tags$code("metadata$label"),
            "."
          ),
          tags$li(
            tags$code("metadata"),
            " — A data frame with at least two columns:",
            tags$ul(
              tags$li(
                tags$code("label"),
                " — Sample identifiers (must match count matrix column names)"
              ),
              tags$li(
                tags$code("group"),
                " — Group/condition assignments for coloring PCA plots"
              )
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
              tags$li("Email-based user identification"),
              tags$li("All your sessions linked to your email"),
              tags$li("Manual save with 'Save Now' button"),
              tags$li("Yellow button = unsaved changes"),
              tags$li("Browser warning before losing unsaved work"),
              tags$li("Session activity logging"),
              tags$li("Cross-app workflow to DEG-Explorer")
            )
          )
        ),
        tags$div(
          class = "row",
          tags$div(
            class = "col-md-6",
            tags$h5("Gene Annotation"),
            tags$ul(
              tags$li("Two methods: g:Profiler (online) or GTF file upload"),
              tags$li("g:Profiler: 7 organisms, multiple ID types"),
              tags$li("GTF: Parse local .gtf/.gff files"),
              tags$li("Annotations flow to loading table and heatmap"),
              tags$li("Export annotated counts as CSV")
            )
          ),
          tags$div(
            class = "col-md-6",
            tags$h5("Expression Heatmap"),
            tags$ul(
              tags$li("Top loading genes from PCA"),
              tags$li("Customizable clustering (Pearson, Ward.D2)"),
              tags$li("Z-score scaling option"),
              tags$li("Color by any metadata column"),
              tags$li("Multiple color palettes available")
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
          # Current session display
          uiOutput("sidebar_session_info"),
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
            emailNotificationsUI("email"),
            hr(),
            # Send to next app button
            uiOutput("send_to_deg_explorer_ui")
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
      "Annotate Counts",
      value = "annotate",
      icon = icon("tags"),
      fluidPage(
        br(),
        h4("Gene Annotation"),
        p(
          "Add gene names and descriptions to your count matrix. Choose between g:Profiler (online) or a local GTF file.",
          style = "color: #666;"
        ),

        # Method selection
        card(
          card_header("Annotation Method"),
          card_body(
            radioButtons(
              "annotation_method",
              "Select annotation source:",
              choices = c(
                "g:Profiler (Online API)" = "gprofiler",
                "GTF File (Local Upload)" = "gtf"
              ),
              selected = "gprofiler",
              inline = TRUE
            )
          )
        ),

        br(),

        # g:Profiler settings (conditional)
        conditionalPanel(
          condition = "input.annotation_method == 'gprofiler'",
          div(
            style = "overflow: visible;",
            card(
              card_header("g:Profiler Settings"),
              card_body(
                style = "padding: 20px; min-height: 180px; overflow: visible;",
                fluidRow(
                  column(
                    5,
                    selectInput(
                      "annotation_organism",
                      "Organism:",
                      choices = c(
                        "Human" = "hsapiens",
                        "Mouse" = "mmusculus",
                        "Rat" = "rnorvegicus",
                        "Zebrafish" = "drerio",
                        "Fly" = "dmelanogaster",
                        "Worm" = "celegans",
                        "Yeast" = "scerevisiae"
                      ),
                      selected = "hsapiens",
                      width = "100%"
                    )
                  ),
                  column(
                    5,
                    selectInput(
                      "annotation_source",
                      "ID Type:",
                      choices = c(
                        "Ensembl Gene" = "ENSG",
                        "Ensembl Transcript" = "ENST",
                        "Entrez Gene ID" = "ENTREZGENE_ACC",
                        "Gene Symbol" = "HGNC"
                      ),
                      selected = "ENSG",
                      width = "100%"
                    )
                  ),
                  column(
                    2,
                    br(),
                    actionButton(
                      "run_annotation",
                      "Annotate",
                      icon = icon("cloud-download"),
                      class = "btn-primary btn-lg",
                      style = "width: 100%;"
                    )
                  )
                )
              )
            )
          )
        ),

        # GTF settings (conditional)
        conditionalPanel(
          condition = "input.annotation_method == 'gtf'",
          card(
            card_header("GTF File Upload"),
            card_body(
              style = "padding: 20px; min-height: 180px;",
              fluidRow(
                column(
                  6,
                  fileInput(
                    "gtf_file",
                    "Upload GTF/GFF file:",
                    accept = c(
                      ".gtf",
                      ".gff",
                      ".gff3",
                      ".gtf.gz",
                      ".gff.gz",
                      ".gff3.gz"
                    ),
                    width = "100%"
                  ),
                  p(
                    "Supports .gtf, .gff, .gff3 (and gzipped versions)",
                    style = "font-size: 0.85em; color: #666; margin-top: -10px;"
                  )
                ),
                column(
                  4,
                  selectInput(
                    "gtf_gene_id_attr",
                    "Gene ID attribute:",
                    choices = c(
                      "gene_id" = "gene_id",
                      "ID" = "ID",
                      "Name" = "Name"
                    ),
                    selected = "gene_id",
                    width = "100%"
                  )
                ),
                column(
                  2,
                  br(),
                  actionButton(
                    "run_gtf_annotation",
                    "Parse GTF",
                    icon = icon("file-code"),
                    class = "btn-primary btn-lg",
                    style = "width: 100%;"
                  )
                )
              ),
              p(
                icon("info-circle"),
                " GTF parsing extracts gene_id, gene_name, and gene_biotype (as description) from gene features.",
                style = "color: #666; font-size: 0.9em; margin-top: 10px;"
              )
            )
          )
        ),

        br(),

        # Results panel (shown for either method)
        conditionalPanel(
          condition = "output.annotation_ready",
          card(
            card_header(
              div(
                style = "display: flex; justify-content: space-between; align-items: center;",
                span("Annotated Genes"),
                uiOutput("annotation_source_badge")
              )
            ),
            card_body(
              withSpinner(
                reactableOutput("annotation_table"),
                type = 4,
                color = "#0dcaf0"
              ),
              br(),
              fluidRow(
                column(
                  6,
                  downloadButton(
                    "download_annotations",
                    "Download Annotations",
                    class = "btn-outline-primary"
                  )
                ),
                column(
                  6,
                  actionButton(
                    "clear_annotations",
                    "Clear Annotations",
                    icon = icon("trash"),
                    class = "btn-outline-danger"
                  )
                )
              )
            )
          )
        ),

        conditionalPanel(
          condition = "!output.annotation_ready",
          card(
            card_body(
              div(
                style = "text-align: center; padding: 50px 20px; color: #999;",
                icon("tags", style = "font-size: 48px;"),
                h4("No Annotations Yet"),
                p(
                  "Upload data in the Upload/Rename tab, then use g:Profiler or upload a GTF file to add gene names and descriptions."
                )
              )
            )
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
          p(
            "Using DESeq2 PCA with Variance Stabilizing Transformation",
            style = "font-size: 0.9em; color: #666;"
          ),
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
            card_body(
              # Tabbed content - each tab manages its own layout
              navset_card_tab(
                id = "pca_tabs",
                nav_panel(
                  "PCA Score Plot",
                  page_sidebar(
                    sidebar = sidebar(
                      position = "right",
                      width = 280,
                      open = TRUE,
                      title = "Plot Options",

                      textInput(
                        "plot_title",
                        "Title:",
                        value = "PCA Score Plot"
                      ),
                      sliderInput(
                        "plot_point_size",
                        "Point Size:",
                        min = 10,
                        max = 50,
                        value = 20,
                        step = 1
                      ),
                      sliderInput(
                        "plot_point_opacity",
                        "Opacity:",
                        min = 0.1,
                        max = 1,
                        value = 1,
                        step = 0.1
                      ),

                      # 3-color gradient picker
                      tags$label(
                        "Gradient Colors:",
                        style = "font-weight: 500;"
                      ),
                      tags$div(
                        style = "display: flex; gap: 5px; margin-bottom: 10px;",
                        tags$div(
                          style = "flex: 1; text-align: center;",
                          colourpicker::colourInput(
                            "gradient_color1",
                            NULL,
                            "#636EFA",
                            showColour = "background",
                            palette = "square"
                          ),
                          tags$small(
                            "Start",
                            style = "color: #888; font-size: 0.7em;"
                          )
                        ),
                        tags$div(
                          style = "flex: 1; text-align: center;",
                          colourpicker::colourInput(
                            "gradient_color2",
                            NULL,
                            "#EF553B",
                            showColour = "background",
                            palette = "square"
                          ),
                          tags$small(
                            "Mid",
                            style = "color: #888; font-size: 0.7em;"
                          )
                        ),
                        tags$div(
                          style = "flex: 1; text-align: center;",
                          colourpicker::colourInput(
                            "gradient_color3",
                            NULL,
                            "#00CC96",
                            showColour = "background",
                            palette = "square"
                          ),
                          tags$small(
                            "End",
                            style = "color: #888; font-size: 0.7em;"
                          )
                        )
                      ),

                      checkboxInput(
                        "plot_show_grid",
                        "Show Grid",
                        value = TRUE
                      ),
                      checkboxInput(
                        "plot_show_legend",
                        "Show Legend",
                        value = TRUE
                      ),
                      selectInput(
                        "plot_bg_color",
                        "Background:",
                        choices = c(
                          "Light Gray" = "#f8f9fa",
                          "White" = "white",
                          "Light Blue" = "#f0f8ff",
                          "Light Yellow" = "#fffef0",
                          "Light Green" = "#f0fff0"
                        ),
                        selected = "#fffef0"
                      ),
                      hr(),
                      actionButton(
                        "open_download_modal",
                        "Download Plot",
                        icon = icon("download"),
                        class = "btn-success w-100 btn-sm"
                      ),
                      downloadButton(
                        "download_eigenvalues",
                        "Export Eigenvalues",
                        class = "btn-outline-primary w-100 btn-sm",
                        style = "margin-top: 5px;"
                      ),
                      downloadButton(
                        "download_pca_scores",
                        "Export PCA Scores",
                        class = "btn-outline-primary w-100 btn-sm",
                        style = "margin-top: 5px;"
                      ),
                      actionButton(
                        "reset_plot_options",
                        "Reset Options",
                        icon = icon("undo"),
                        class = "btn-secondary w-100 btn-sm",
                        style = "margin-top: 5px;"
                      )
                    ),
                    # PCA plot content
                    withSpinner(
                      plotlyOutput(
                        "pca_plot",
                        width = "100%",
                        height = "700px"
                      ),
                      type = 4,
                      color = "#0dcaf0"
                    )
                  )
                ),
                nav_panel(
                  "Variance Explained",
                  br(),
                  withSpinner(
                    plotlyOutput("pca_scree", width = "100%", height = "650px"),
                    type = 4,
                    color = "#0dcaf0"
                  )
                ),
                nav_panel(
                  "Top Loading Genes",
                  icon = icon("list-ol"),
                  page_sidebar(
                    sidebar = sidebar(
                      width = 260,
                      title = "Gene Selection",

                      selectInput(
                        "topgenes_pc",
                        "Select PC for ranking:",
                        choices = c("PC1" = 1, "PC2" = 2),
                        selected = 1
                      ),
                      sliderInput(
                        "topgenes_n",
                        "Number of top genes:",
                        min = 10,
                        max = 100,
                        value = 30,
                        step = 5
                      ),
                      radioButtons(
                        "topgenes_direction",
                        "Loading direction:",
                        choices = c(
                          "Both (absolute)" = "both",
                          "Positive only" = "positive",
                          "Negative only" = "negative"
                        ),
                        selected = "both"
                      ),

                      hr(),
                      downloadButton(
                        "download_top_genes",
                        "Export Top Genes",
                        class = "btn-outline-primary w-100"
                      )
                    ),

                    # Main content
                    div(
                      p(
                        "Genes ranked by their contribution (loading) to the selected principal component. ",
                        "Higher absolute loading = stronger influence on that PC.",
                        style = "color: #666; font-style: italic; margin-bottom: 15px;"
                      ),
                      withSpinner(
                        DT::dataTableOutput("loading_table"),
                        type = 4,
                        color = "#0dcaf0"
                      )
                    )
                  )
                ),
                nav_panel(
                  "Expression Heatmap",
                  icon = icon("grip"),
                  page_sidebar(
                    sidebar = sidebar(
                      width = 280,
                      title = "Heatmap Options",

                      h6("Gene Selection", style = "font-weight: bold;"),
                      selectInput(
                        "heatmap_pc",
                        "Select PC for ranking:",
                        choices = c("PC1" = 1, "PC2" = 2),
                        selected = 1
                      ),
                      sliderInput(
                        "heatmap_n_genes",
                        "Number of top genes:",
                        min = 10,
                        max = 100,
                        value = 30,
                        step = 5
                      ),

                      hr(),
                      h6("Sample Annotation", style = "font-weight: bold;"),
                      uiOutput("heatmap_color_ui"),

                      hr(),
                      h6("Clustering", style = "font-weight: bold;"),

                      checkboxInput(
                        "heatmap_cluster_rows",
                        "Cluster rows (genes)",
                        value = FALSE
                      ),
                      checkboxInput(
                        "heatmap_cluster_cols",
                        "Cluster columns (samples)",
                        value = FALSE
                      ),
                      selectInput(
                        "heatmap_dist_method",
                        "Distance metric:",
                        choices = c(
                          "Pearson correlation" = "pearson",
                          "Spearman correlation" = "spearman",
                          "Euclidean" = "euclidean",
                          "Manhattan" = "manhattan"
                        ),
                        selected = "pearson"
                      ),
                      selectInput(
                        "heatmap_clust_method",
                        "Linkage method:",
                        choices = c(
                          "Ward.D2" = "ward.D2",
                          "Complete" = "complete",
                          "Average" = "average",
                          "Single" = "single"
                        ),
                        selected = "ward.D2"
                      ),
                      checkboxInput(
                        "heatmap_show_dendro",
                        "Show dendrograms",
                        value = TRUE
                      ),

                      hr(),
                      h6("Display", style = "font-weight: bold;"),

                      checkboxInput(
                        "heatmap_scale_rows",
                        "Scale rows (z-score)",
                        value = TRUE
                      ),
                      checkboxInput(
                        "heatmap_show_rownames",
                        "Show gene names",
                        value = TRUE
                      ),
                      checkboxInput(
                        "heatmap_show_colnames",
                        "Show sample names",
                        value = TRUE
                      ),
                      selectInput(
                        "heatmap_colors",
                        "Color palette:",
                        choices = c(
                          "Blue-White-Red" = "RdBu",
                          "Viridis" = "viridis",
                          "Purple-Green" = "PRGn",
                          "Blue-Yellow-Red" = "RdYlBu",
                          "Plasma" = "plasma"
                        ),
                        selected = "RdBu"
                      )
                    ),

                    # Main content - simple fixed dimensions
                    div(
                      p(
                        "VST-normalized expression of top loading genes across samples. ",
                        "Rows are genes, columns are samples colored by group.",
                        style = "color: #666; font-style: italic; margin-bottom: 15px;"
                      ),
                      withSpinner(
                        plotlyOutput(
                          "topgenes_heatmap",
                          height = "1200px",
                          width = "840px"
                        ),
                        type = 4,
                        color = "#0dcaf0"
                      )
                    )
                  )
                ),
                nav_panel(
                  "PCA Summary",
                  br(),
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
                p(
                  "Upload data in the Upload/Rename tab, configure parameters, and click 'Run PCA'"
                )
              )
            )
          )
        )
      ) # Close page_sidebar
    ) # Close tabPanel (PCA Analysis)
  ) # Close navbarPage
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
      "PNG (image)" = "png",
      "PDF (vector)" = "pdf",
      "SVG (vector)" = "svg",
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
  # HELPER: Reset All Data (call before loading new data)
  # =================================================
  # This function completely purges all reactive data to ensure

  # a clean slate before loading new data (example, session, or upload)

  # Reset token to force UI components to re-render
  reset_token <- reactiveVal(0)

  reset_all_data <- function(reset_session_id = FALSE, reset_ui = TRUE) {
    # Increment reset token to invalidate any cached UI state
    reset_token(reset_token() + 1)

    # Force JavaScript UI refresh to fix scroll issues
    session$sendCustomMessage("forceUIRefresh", list(token = reset_token()))

    # Reset data reactive values
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

    # Reset annotation reactive values
    annotation_rv$annotated_data <- NULL
    annotation_rv$ready <- FALSE
    annotation_rv$method <- NULL

    # Reset meta_out (editable table module data)
    meta_out$data <- NULL

    # Reset session tracking if requested
    if (reset_session_id) {
      user_rv$current_session_id <- NULL
      user_rv$current_file_name <- NULL
    }

    # Reset unsaved changes tracking
    user_rv$has_unsaved_changes <- FALSE
    user_rv$last_save_time <- NULL
    session$sendCustomMessage("setUnsavedChanges", FALSE)

    # Reset file input
    shinyjs::reset("file")

    # Reset UI inputs to defaults if requested
    if (reset_ui) {
      # Reset plot options
      updateTextInput(session, "plot_title", value = "PCA Score Plot")
      updateSliderInput(session, "plot_point_size", value = 20)
      updateSliderInput(session, "plot_point_opacity", value = 1)
      tryCatch(
        {
          colourpicker::updateColourInput(
            session,
            "gradient_color1",
            value = "#636EFA"
          )
          colourpicker::updateColourInput(
            session,
            "gradient_color2",
            value = "#EF553B"
          )
          colourpicker::updateColourInput(
            session,
            "gradient_color3",
            value = "#00CC96"
          )
        },
        error = function(e) NULL
      )
      updateCheckboxInput(session, "plot_show_grid", value = TRUE)
      updateCheckboxInput(session, "plot_show_legend", value = TRUE)
      updateSelectInput(session, "plot_bg_color", selected = "#fffef0")

      # Reset PCA parameters
      updateCheckboxInput(session, "vst_blind", value = TRUE)
      updateSelectInput(session, "vst_fitType", selected = "parametric")
      updateNumericInput(session, "pca_ntop", value = 500)
      updateSelectInput(session, "pca_pc_x", selected = 1)
      updateSelectInput(session, "pca_pc_y", selected = 2)
    }
  }

  # =================================================
  # GLOBAL SAVE BUTTON - Uses new user session system
  # =================================================
  # Note: save_session_now handler is defined in the
  # INITIALIZE MODULES section above

  # =================================================
  # SESSION END HANDLING & UNSAVED CHANGES TRACKING
  # =================================================

  # Track data changes to mark as unsaved
  observe({
    # These triggers indicate data has changed
    meta_changed <- data_rv$meta
    pca_changed <- pca_rv$computed
    anno_changed <- annotation_rv$ready

    # Only mark as unsaved if we have data and a session
    # Use isolate to prevent reading these values from triggering the observer
    isolate({
      if (!is.null(meta_changed) && user_rv$authenticated) {
        # Don't mark as unsaved on initial load
        if (!is.null(user_rv$last_save_time) && !user_rv$has_unsaved_changes) {
          user_rv$has_unsaved_changes <- TRUE
          session$sendCustomMessage("setUnsavedChanges", TRUE)
        }
      }
    })
  })

  # When session is loaded, don't mark as unsaved initially
  observeEvent(
    user_rv$current_session_id,
    {
      # Reset unsaved flag when loading a session
      user_rv$has_unsaved_changes <- FALSE
      session$sendCustomMessage("setUnsavedChanges", FALSE)
    },
    ignoreInit = TRUE
  )

  # Session cleanup on browser close/refresh
  session$onSessionEnded(function() {
    cat("\n=== SESSION ENDED ===\n")
    cat("Time:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")

    # Log session end if user was authenticated
    if (isolate(user_rv$authenticated)) {
      cat("User:", isolate(user_rv$email), "\n")
      cat("Session ID:", isolate(user_rv$current_session_id), "\n")
      cat("Had unsaved changes:", isolate(user_rv$has_unsaved_changes), "\n")

      # Log to activity file
      tryCatch(
        {
          log_session_activity(
            base_path = SESSIONS_BASE_PATH,
            email = isolate(user_rv$email),
            session_id = isolate(user_rv$current_session_id) %||% "none",
            app_name = APP_NAME,
            action = "session_ended",
            file_name = isolate(user_rv$current_file_name),
            samples = if (!is.null(isolate(data_rv$meta))) {
              nrow(isolate(data_rv$meta))
            } else {
              NA
            },
            has_pca = isolate(pca_rv$computed),
            has_annotations = isolate(isTRUE(annotation_rv$ready))
          )
        },
        error = function(e) {
          cat("Failed to log session end:", e$message, "\n")
        }
      )
    }

    cat("=== END ===\n\n")
  })

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

  # Update counts_rv when data changes - only if not already set
  observe(
    {
      req(data_rv$counts)
      # Only update if counts_rv is empty AND data_rv has data
      # This prevents the observer from running during normal edits
      if (is.null(counts_rv$counts) && !is.null(data_rv$counts)) {
        counts_rv$counts <- data_rv$counts
        counts_rv$orig_counts <- data_rv$orig_counts
      }
    },
    priority = -1
  ) # Lower priority to run after other observers

  # =================================================
  # INITIALIZE MODULES
  # =================================================

  # Configuration for session storage
  SESSIONS_BASE_PATH <- Sys.getenv("PIPELINE_SESSIONS_PATH", "shared/sessions")
  APP_NAME <- "qc-checker"

  # User session reactive values
  user_rv <- reactiveValues(
    email = NULL,
    email_hash = NULL,
    email_valid = FALSE, # Email format is valid
    user_exists = FALSE, # User exists in registry
    needs_pin_creation = FALSE, # New user needs to create PIN
    needs_pin_entry = FALSE, # Existing user needs to enter PIN
    pin_verified = FALSE, # PIN has been verified
    authenticated = FALSE, # Fully authenticated (email + PIN)
    current_session_id = NULL,
    current_file_name = NULL,
    sessions_list = list(),
    has_unsaved_changes = FALSE,
    last_save_time = NULL
  )

  # Email validation (debounced)
  email_debounced <- debounce(reactive(input$user_email), 500)

  observe({
    email <- email_debounced()

    if (is_valid_email(email)) {
      user_rv$email <- tolower(trimws(email))
      user_rv$email_hash <- hash_email(user_rv$email)
      user_rv$email_valid <- TRUE

      # Check if user exists and has PIN
      if (user_exists(user_rv$email, SESSIONS_BASE_PATH)) {
        user_rv$user_exists <- TRUE
        if (user_has_pin(user_rv$email, SESSIONS_BASE_PATH)) {
          # Existing user with PIN - needs to enter it
          user_rv$needs_pin_entry <- TRUE
          user_rv$needs_pin_creation <- FALSE
        } else {
          # Existing user without PIN - needs to create one
          user_rv$needs_pin_creation <- TRUE
          user_rv$needs_pin_entry <- FALSE
        }
      } else {
        # New user - needs to create PIN
        user_rv$user_exists <- FALSE
        user_rv$needs_pin_creation <- TRUE
        user_rv$needs_pin_entry <- FALSE
      }

      # Not authenticated until PIN is verified
      user_rv$authenticated <- user_rv$pin_verified
    } else {
      user_rv$email_valid <- FALSE
      user_rv$user_exists <- FALSE
      user_rv$needs_pin_creation <- FALSE
      user_rv$needs_pin_entry <- FALSE
      user_rv$pin_verified <- FALSE
      user_rv$authenticated <- FALSE
    }
  })

  # Output for conditional panels
  output$user_authenticated <- reactive({
    user_rv$authenticated
  })
  outputOptions(output, "user_authenticated", suspendWhenHidden = FALSE)

  # Output for PIN entry panel
  output$needs_pin_entry <- reactive({
    user_rv$email_valid && user_rv$needs_pin_entry && !user_rv$pin_verified
  })
  outputOptions(output, "needs_pin_entry", suspendWhenHidden = FALSE)

  # Output for PIN creation panel
  output$needs_pin_creation <- reactive({
    user_rv$email_valid && user_rv$needs_pin_creation && !user_rv$pin_verified
  })
  outputOptions(output, "needs_pin_creation", suspendWhenHidden = FALSE)

  # Email validation status
  output$email_validation_status <- renderUI({
    email <- input$user_email

    if (is.null(email) || email == "") {
      return(NULL)
    }

    if (is_valid_email(email)) {
      if (user_rv$pin_verified) {
        span(
          class = "text-success small",
          icon("check-circle"),
          " Authenticated"
        )
      } else if (user_rv$user_exists) {
        span(
          class = "text-info small",
          icon("user"),
          " Welcome back! Enter your PIN below."
        )
      } else {
        span(
          class = "text-info small",
          icon("user-plus"),
          " New user - create a PIN below."
        )
      }
    } else {
      span(
        class = "text-danger small",
        icon("exclamation-circle"),
        " Please enter a valid email"
      )
    }
  })

  # PIN Entry Handler
  observeEvent(input$verify_pin, {
    req(user_rv$email, input$pin_entry)

    pin <- input$pin_entry

    result <- verify_user_pin(user_rv$email, pin, SESSIONS_BASE_PATH)

    if (result$success) {
      # PIN verified - complete authentication
      user_rv$pin_verified <- TRUE
      user_rv$authenticated <- TRUE
      user_rv$needs_pin_entry <- FALSE

      # Ensure user directory exists
      user_dir <- get_user_session_dir(
        SESSIONS_BASE_PATH,
        user_rv$email,
        APP_NAME
      )
      if (!dir.exists(user_dir)) {
        dir.create(user_dir, recursive = TRUE)
      }

      # Load user's session list
      user_rv$sessions_list <- get_user_sessions(
        user_rv$email,
        APP_NAME,
        SESSIONS_BASE_PATH
      )

      # Log activity
      log_session_activity(
        base_path = SESSIONS_BASE_PATH,
        email = user_rv$email,
        session_id = "login",
        app_name = APP_NAME,
        action = "login_success"
      )

      showNotification(
        paste("Welcome back,", user_rv$email),
        type = "message",
        duration = 3
      )

      # Clear PIN input
      updateTextInput(session, "pin_entry", value = "")
    } else {
      # PIN verification failed
      if (result$locked) {
        showNotification(result$error, type = "error", duration = 10)
      } else {
        showNotification(result$error, type = "warning", duration = 5)
      }

      # Log failed attempt
      log_session_activity(
        base_path = SESSIONS_BASE_PATH,
        email = user_rv$email,
        session_id = "login",
        app_name = APP_NAME,
        action = "login_failed"
      )
    }
  })

  # PIN Creation Handler
  observeEvent(input$create_pin, {
    req(user_rv$email, input$pin_create, input$pin_confirm)

    pin1 <- input$pin_create
    pin2 <- input$pin_confirm

    # Validate PIN format
    if (!is_valid_pin(pin1)) {
      showNotification("PIN must be 4-6 digits", type = "error")
      return()
    }

    # Check PINs match
    if (pin1 != pin2) {
      showNotification("PINs do not match", type = "error")
      return()
    }

    # Set the PIN
    if (set_user_pin(user_rv$email, pin1, SESSIONS_BASE_PATH)) {
      # PIN created - complete authentication
      user_rv$pin_verified <- TRUE
      user_rv$authenticated <- TRUE
      user_rv$needs_pin_creation <- FALSE
      user_rv$user_exists <- TRUE

      # Register user if new
      register_user(user_rv$email, SESSIONS_BASE_PATH)

      # Ensure user directory exists
      user_dir <- get_user_session_dir(
        SESSIONS_BASE_PATH,
        user_rv$email,
        APP_NAME
      )
      if (!dir.exists(user_dir)) {
        dir.create(user_dir, recursive = TRUE)
      }

      # Load user's session list
      user_rv$sessions_list <- get_user_sessions(
        user_rv$email,
        APP_NAME,
        SESSIONS_BASE_PATH
      )

      # Log activity
      log_session_activity(
        base_path = SESSIONS_BASE_PATH,
        email = user_rv$email,
        session_id = "registration",
        app_name = APP_NAME,
        action = "pin_created"
      )

      showNotification(
        "PIN created! You can now access your sessions.",
        type = "message",
        duration = 3
      )

      # Clear PIN inputs
      updateTextInput(session, "pin_create", value = "")
      updateTextInput(session, "pin_confirm", value = "")
    } else {
      showNotification(
        "Failed to create PIN. Please try again.",
        type = "error"
      )
    }
  })

  # Existing sessions UI
  output$existing_sessions_ui <- renderUI({
    req(user_rv$authenticated)

    sessions <- user_rv$sessions_list
    session_count <- length(sessions)

    tagList(
      # Session count indicator
      div(
        class = "d-flex justify-content-between align-items-center mb-2",
        tags$small(
          class = if (session_count >= MAX_SESSIONS_PER_USER) {
            "text-warning"
          } else {
            "text-muted"
          },
          icon(
            if (session_count >= MAX_SESSIONS_PER_USER) {
              "exclamation-triangle"
            } else {
              "database"
            }
          ),
          paste0(" ", session_count, "/", MAX_SESSIONS_PER_USER, " sessions")
        )
      ),

      if (session_count == 0) {
        div(
          class = "text-muted small p-2",
          icon("folder-open"),
          " No saved sessions yet"
        )
      } else {
        div(
          style = "max-height: 300px; overflow-y: auto;",
          lapply(seq_along(sessions), function(i) {
            sess <- sessions[[i]]
            file_display <- sess$file_name %||% "Unnamed"
            div(
              class = "card mb-2 session-card",
              style = "cursor: pointer;",
              onclick = sprintf(
                "Shiny.setInputValue('select_session', '%s', {priority: 'event'})",
                sess$session_id
              ),
              div(
                class = "card-body py-2 px-3",
                div(
                  strong(class = "small", file_display),
                  br(),
                  tags$code(
                    class = "small",
                    style = "font-size: 0.7em; word-break: break-all;",
                    sess$session_id
                  ),
                  br(),
                  div(
                    class = "d-flex justify-content-between align-items-center mt-1",
                    tags$small(
                      class = "text-muted",
                      icon("clock"),
                      {
                        # Parse ISO format timestamp
                        time_str <- sess$updated %||% sess$created
                        parsed_time <- tryCatch(
                          as.POSIXct(
                            time_str,
                            format = "%Y-%m-%dT%H:%M:%SZ",
                            tz = "UTC"
                          ),
                          error = function(e) as.POSIXct(time_str)
                        )
                        format(parsed_time, "%m/%d %H:%M")
                      }
                    ),
                    div(
                      if (isTRUE(sess$has_pca)) {
                        span(
                          class = "badge bg-success me-1",
                          style = "font-size: 0.65em;",
                          "PCA"
                        )
                      },
                      if (isTRUE(sess$has_annotations)) {
                        span(
                          class = "badge bg-info",
                          style = "font-size: 0.65em;",
                          "Anno"
                        )
                      }
                    )
                  )
                )
              )
            )
          })
        )
      }
    )
  })

  # Current session card
  output$current_session_card <- renderUI({
    req(user_rv$authenticated)

    # Show card if we have data, even without session_id yet
    if (is.null(data_rv$meta) && is.null(user_rv$current_session_id)) {
      return(NULL)
    }

    card(
      card_header(
        tagList(
          icon("id-card", class = "text-success"),
          " Current Session"
        )
      ),
      card_body(
        if (!is.null(user_rv$current_session_id)) {
          tagList(
            p(
              strong("Session ID: "),
              tags$code(
                user_rv$current_session_id,
                style = "font-size: 0.85em;"
              )
            ),
            if (!is.null(data_rv$meta)) {
              p(
                class = "small text-muted",
                icon("table"),
                " ",
                nrow(data_rv$meta),
                " samples loaded"
              )
            }
          )
        } else {
          p(
            class = "text-muted",
            icon("info-circle"),
            " Data loaded but not saved yet. Click 'Save Now' to create a session."
          )
        },
        div(
          class = "d-flex gap-2 mt-2",
          if (!is.null(user_rv$current_session_id)) {
            actionButton(
              "copy_session_id",
              "Copy ID",
              icon = icon("copy"),
              class = "btn-outline-secondary btn-sm"
            )
          },
          actionButton(
            "save_session_now",
            "Save Now",
            icon = icon("save"),
            class = "btn-success btn-sm"
          )
        )
      )
    )
  })

  # Sidebar session info
  output$sidebar_session_info <- renderUI({
    if (!user_rv$authenticated) {
      div(
        class = "alert alert-info py-2 px-3",
        style = "font-size: 0.85em;",
        icon("info-circle"),
        " Enter your email on the Start tab to enable session management."
      )
    } else if (is.null(user_rv$current_session_id)) {
      div(
        class = "alert alert-warning py-2 px-3",
        style = "font-size: 0.85em;",
        icon("exclamation-triangle"),
        " No active session. Upload data to start."
      )
    } else {
      div(
        class = "session-info-panel",
        style = "background: #e8f5e8; border: 1px solid #c3e6c3; border-radius: 6px; padding: 10px; margin-bottom: 10px;",
        div(
          icon("user", class = "text-success"),
          strong(" ", user_rv$email)
        ),
        hr(style = "margin: 8px 0;"),
        div(
          icon("id-card"),
          " Session:"
        ),
        tags$code(
          user_rv$current_session_id,
          style = "font-size: 0.75em; word-break: break-all; display: block; margin: 4px 0;"
        ),
        if (!is.null(user_rv$current_file_name)) {
          div(
            class = "small text-muted",
            icon("file"),
            " ",
            user_rv$current_file_name
          )
        },
        div(
          class = "mt-2",
          actionButton(
            "copy_session_id_sidebar",
            "Copy ID",
            icon = icon("copy"),
            class = "btn-outline-primary btn-sm"
          )
        )
      )
    }
  })

  # Copy session ID handlers
  observeEvent(input$copy_session_id, {
    req(user_rv$current_session_id)
    shinyjs::runjs(sprintf(
      "navigator.clipboard.writeText('%s');",
      user_rv$current_session_id
    ))
    showNotification(
      "Session ID copied to clipboard!",
      type = "message",
      duration = 2
    )
  })

  observeEvent(input$copy_session_id_sidebar, {
    req(user_rv$current_session_id)
    shinyjs::runjs(sprintf(
      "navigator.clipboard.writeText('%s');",
      user_rv$current_session_id
    ))
    showNotification(
      "Session ID copied to clipboard!",
      type = "message",
      duration = 2
    )
  })

  # Refresh sessions list
  observeEvent(input$refresh_sessions, {
    req(user_rv$authenticated)
    user_rv$sessions_list <- get_user_sessions(
      user_rv$email,
      APP_NAME,
      SESSIONS_BASE_PATH
    )
    showNotification("Sessions list refreshed", type = "message", duration = 2)
  })

  # Send to DEG-Explorer UI
  output$send_to_deg_explorer_ui <- renderUI({
    req(user_rv$current_session_id)
    req(pca_rv$computed) # Only show if PCA is done

    deg_explorer_url <- Sys.getenv("DEG_EXPLORER_URL", "../deg-explorer/")

    div(
      class = "mt-3 p-3",
      style = "background: #fff3cd; border: 1px solid #ffc107; border-radius: 6px;",
      h6(icon("arrow-right"), " Ready for Differential Expression?"),
      p(
        class = "small text-muted mb-2",
        "Your QC'd data can be sent directly to DEG-Explorer."
      ),
      tags$a(
        href = paste0(
          deg_explorer_url,
          "?session=",
          user_rv$current_session_id,
          "&email=",
          URLencode(user_rv$email)
        ),
        target = "_blank",
        class = "btn btn-warning btn-sm",
        icon("external-link-alt"),
        " Open in DEG-Explorer"
      )
    )
  })

  # Helper function to generate session ID
  generate_session_id <- function() {
    paste0(
      "qc_",
      format(Sys.time(), "%Y%m%d%H%M%S"),
      "_",
      substr(digest::digest(runif(1)), 1, 8)
    )
  }

  # Reusable save session function with progress bar
  do_save_session <- function() {
    # Check prerequisites with user feedback
    if (!user_rv$authenticated) {
      showNotification(
        "Please enter your email on the Start tab first",
        type = "warning",
        duration = 4
      )
      return(FALSE)
    }

    if (is.null(data_rv$meta)) {
      showNotification(
        "No data to save. Please upload a file first.",
        type = "warning",
        duration = 4
      )
      return(FALSE)
    }

    # Check session limit if this is a NEW session (not updating existing)
    is_new_session <- is.null(user_rv$current_session_id) ||
      !any(sapply(user_rv$sessions_list, function(s) {
        s$session_id == user_rv$current_session_id
      }))

    if (
      is_new_session && length(user_rv$sessions_list) >= MAX_SESSIONS_PER_USER
    ) {
      # Show limit reached modal
      showModal(modalDialog(
        title = tagList(icon("exclamation-triangle"), " Session Limit Reached"),
        div(
          style = "text-align: center;",
          p(
            style = "font-size: 1.1em;",
            "You have reached the maximum of ",
            tags$strong(MAX_SESSIONS_PER_USER),
            " saved sessions."
          ),
          hr(),
          p("Please delete an existing session before saving a new one."),
          p(
            class = "text-muted small",
            "Click on a session in the 'Load Existing Session' list to delete it."
          )
        ),
        footer = modalButton("OK"),
        size = "m",
        easyClose = TRUE
      ))
      return(FALSE)
    }

    # Create session ID if none exists
    if (is.null(user_rv$current_session_id)) {
      user_rv$current_session_id <- generate_session_id()
    }

    # Create progress bar
    progress <- Progress$new()
    on.exit(progress$close())

    progress$set(message = "Saving session...", value = 0)

    tryCatch(
      {
        progress$set(value = 0.1, detail = "Preparing metadata...")

        # Get current metadata
        current_meta <- if (
          !is.null(meta_module_out) && !is.null(meta_module_out$data)
        ) {
          meta_module_out$data()
        } else {
          data_rv$meta
        }

        progress$set(value = 0.2, detail = "Building session data...")

        # Build session data
        session_data <- list(
          session_id = user_rv$current_session_id,
          user_email = user_rv$email,
          app_name = APP_NAME,
          created = Sys.time(),
          updated = Sys.time(),
          file_name = user_rv$current_file_name %||%
            isolate(input$file$name) %||%
            "data.RData",
          file_data = list(
            metadata = data_rv$orig_meta,
            counts = data_rv$orig_counts
          ),
          current_edits = list(
            metadata = current_meta,
            counts = counts_rv$counts %||% data_rv$counts
          )
        )

        progress$set(value = 0.4, detail = "Adding PCA results...")

        # Add PCA results if available
        if (pca_rv$computed && !is.null(pca_rv$pca_result)) {
          session_data$current_edits$pca_result <- pca_rv$pca_result
          session_data$current_edits$vst_result <- pca_rv$pca_result$vst
          session_data$current_edits$dds <- pca_rv$pca_result$dds

          # Save plot settings
          session_data$current_edits$pca_result$plot_settings <- list(
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
        }

        progress$set(value = 0.5, detail = "Adding annotations...")

        # Add annotation data if available
        if (
          isTRUE(annotation_rv$ready) && !is.null(annotation_rv$annotated_data)
        ) {
          session_data$current_edits$annotation <- list(
            annotated_data = annotation_rv$annotated_data,
            method = annotation_rv$method,
            organism = input$annotation_organism,
            source = input$annotation_source
          )
        }

        progress$set(value = 0.6, detail = "Creating directories...")

        # Save to file
        session_dir <- get_user_session_dir(
          SESSIONS_BASE_PATH,
          user_rv$email,
          APP_NAME
        )
        if (!dir.exists(session_dir)) {
          dir.create(session_dir, recursive = TRUE)
        }

        progress$set(value = 0.7, detail = "Writing session file...")

        session_file <- file.path(
          session_dir,
          paste0(user_rv$current_session_id, ".rds")
        )
        saveRDS(session_data, session_file)

        progress$set(value = 0.85, detail = "Updating registry...")

        # Update registry
        update_session_registry(
          email = user_rv$email,
          app_name = APP_NAME,
          session_info = list(
            session_id = user_rv$current_session_id,
            created = format(session_data$created, "%Y-%m-%dT%H:%M:%SZ"),
            updated = format(session_data$updated, "%Y-%m-%dT%H:%M:%SZ"),
            file_name = session_data$file_name,
            samples = nrow(current_meta),
            groups = unique(as.character(current_meta$group)),
            has_pca = pca_rv$computed,
            has_annotations = isTRUE(annotation_rv$ready),
            status = "complete"
          ),
          base_path = SESSIONS_BASE_PATH
        )

        # Log activity
        log_session_activity(
          base_path = SESSIONS_BASE_PATH,
          email = user_rv$email,
          session_id = user_rv$current_session_id,
          app_name = APP_NAME,
          action = "saved",
          file_name = session_data$file_name,
          samples = nrow(current_meta),
          has_pca = pca_rv$computed,
          has_annotations = isTRUE(annotation_rv$ready)
        )

        progress$set(value = 0.95, detail = "Refreshing session list...")

        # Refresh sessions list
        user_rv$sessions_list <- get_user_sessions(
          user_rv$email,
          APP_NAME,
          SESSIONS_BASE_PATH
        )

        progress$set(value = 1, detail = "Complete!")
        Sys.sleep(0.3)

        showNotification(
          paste("Session saved successfully!", user_rv$current_session_id),
          type = "message",
          duration = 3
        )

        # Mark changes as saved
        user_rv$has_unsaved_changes <- FALSE
        user_rv$last_save_time <- Sys.time()
        session$sendCustomMessage("setUnsavedChanges", FALSE)

        cat("Session saved:", user_rv$current_session_id, "\n")
        cat("  - Samples:", nrow(current_meta), "\n")
        cat("  - PCA:", pca_rv$computed, "\n")
        cat("  - Annotations:", isTRUE(annotation_rv$ready), "\n")

        return(TRUE)
      },
      error = function(e) {
        showNotification(
          paste("Error saving session:", e$message),
          type = "error",
          duration = 5
        )
        cat("Save error:", e$message, "\n")
        return(FALSE)
      }
    )
  }

  # Handler for card button
  observeEvent(input$save_session_now, {
    do_save_session()
  })

  # Handler for global navbar button (Ctrl+S also triggers this)
  observeEvent(input$save_now_global, {
    do_save_session()
  })

  # Auto-save DISABLED - manual save only via "Save Now" button
  # observe({
  #   # Trigger on data changes
  #   data_rv$meta
  #   pca_rv$computed
  #   annotation_rv$ready
  #
  #   # Only auto-save if we have an active session with data
  #   if (user_rv$authenticated &&
  #       !is.null(user_rv$current_session_id) &&
  #       !is.null(data_rv$meta)) {
  #     invalidateLater(3000)  # 3 second debounce
  #     save_current_session(show_notification = FALSE)
  #   }
  # })

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
  # Uses .credentials folder local to app directory (Docker-friendly)
  email <- emailNotificationsServer(
    "email",
    email_config = list(
      enabled = TRUE,
      sender_email = "rshiny.trex@gmail.com", # Will be overridden by creds
      creds_file = ".credentials/gmail_creds", # Local to app directory
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

        session_id <- user_rv$current_session_id

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
    }),
    # NEW: Pass session ID so email module can auto-clear changes on session change
    session_id_func = reactive({
      user_rv$current_session_id
    })
  )

  # =================================================
  # SESSION MANAGEMENT
  # =================================================

  # Maximum sessions per user
  MAX_SESSIONS_PER_USER <- 5

  # Store selected session for modal actions
  selected_session_rv <- reactiveVal(NULL)

  # Show session options modal when user clicks on a session card
  observeEvent(input$select_session, {
    req(user_rv$authenticated)
    session_id <- input$select_session

    # Find session info from list
    session_info <- NULL
    for (sess in user_rv$sessions_list) {
      if (sess$session_id == session_id) {
        session_info <- sess
        break
      }
    }

    if (is.null(session_info)) {
      showNotification("Session not found", type = "error")
      return()
    }

    # Store selected session
    selected_session_rv(session_id)

    # Parse timestamp
    time_str <- session_info$updated %||% session_info$created
    parsed_time <- tryCatch(
      format(
        as.POSIXct(time_str, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
        "%Y-%m-%d %H:%M"
      ),
      error = function(e) time_str
    )

    # Build info display
    file_name <- session_info$file_name %||% "Unknown"
    samples <- session_info$samples %||% "?"
    has_pca <- isTRUE(session_info$has_pca)
    has_anno <- isTRUE(session_info$has_annotations)

    showModal(modalDialog(
      title = tagList(icon("folder-open"), " Session Options"),

      # Session info card
      div(
        style = "background: #f8f9fa; border-radius: 8px; padding: 15px; margin-bottom: 15px;",
        h5(file_name, style = "margin-bottom: 10px;"),
        tags$table(
          style = "width: 100%;",
          tags$tr(
            tags$td(
              tags$strong("Session ID:"),
              style = "padding: 5px 10px 5px 0;"
            ),
            tags$td(tags$code(session_id, style = "font-size: 0.85em;"))
          ),
          tags$tr(
            tags$td(
              tags$strong("Last Updated:"),
              style = "padding: 5px 10px 5px 0;"
            ),
            tags$td(parsed_time)
          ),
          tags$tr(
            tags$td(
              tags$strong("Samples:"),
              style = "padding: 5px 10px 5px 0;"
            ),
            tags$td(samples)
          ),
          tags$tr(
            tags$td(
              tags$strong("Analysis:"),
              style = "padding: 5px 10px 5px 0;"
            ),
            tags$td(
              if (has_pca) span(class = "badge bg-success me-1", "PCA"),
              if (has_anno) span(class = "badge bg-info", "Annotated"),
              if (!has_pca && !has_anno) span(class = "text-muted", "None yet")
            )
          )
        )
      ),

      # Action buttons
      div(
        style = "display: flex; gap: 10px;",
        actionButton(
          "modal_load_session",
          "Load Session",
          icon = icon("folder-open"),
          class = "btn-primary flex-grow-1"
        ),
        actionButton(
          "modal_delete_session",
          "Delete",
          icon = icon("trash"),
          class = "btn-outline-danger"
        )
      ),

      footer = modalButton("Cancel"),
      size = "m",
      easyClose = TRUE
    ))
  })

  # Handle Load Session from modal
  observeEvent(input$modal_load_session, {
    req(selected_session_rv())
    session_id <- selected_session_rv()
    removeModal()

    tryCatch(
      {
        # Create progress bar
        progress <- Progress$new()
        on.exit(progress$close())

        progress$set(message = "Loading session", value = 0)
        progress$set(value = 0.1, detail = "Preparing...")

        # IMPORTANT: Reset all data before loading new session
        reset_all_data(reset_session_id = TRUE, reset_ui = FALSE)

        progress$set(value = 0.2, detail = "Reading session file...")

        # Load from user's session directory
        session_file <- file.path(
          get_user_session_dir(SESSIONS_BASE_PATH, user_rv$email, APP_NAME),
          paste0(session_id, ".rds")
        )

        if (!file.exists(session_file)) {
          showNotification(
            "Session file not found",
            type = "error",
            duration = 5
          )
          return()
        }

        session_data <- readRDS(session_file)

        # Set current session ID and file name
        user_rv$current_session_id <- session_id
        user_rv$current_file_name <- session_data$file_name

        progress$set(value = 0.4, detail = "Restoring metadata...")

        # Restore the edited data
        data_rv$meta <- session_data$current_edits$metadata
        data_rv$orig_meta <- session_data$file_data$metadata
        data_rv$counts <- session_data$current_edits$counts
        data_rv$orig_counts <- session_data$file_data$counts
        counts_rv$counts <- session_data$current_edits$counts
        counts_rv$orig_counts <- session_data$file_data$counts

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
            params_used = session_data$current_edits$pca_result$params_used
          )
          pca_rv$pca_data <- as.data.frame(
            session_data$current_edits$pca_result$pca_data
          )
          pca_rv$computed <- TRUE

          # Restore plot settings if they exist
          if (!is.null(session_data$current_edits$pca_result$plot_settings)) {
            plot_settings <- session_data$current_edits$pca_result$plot_settings
            updateTextInput(
              session,
              "plot_title",
              value = plot_settings$title %||% "PCA Score Plot"
            )
            updateSliderInput(
              session,
              "plot_point_size",
              value = plot_settings$point_size %||% 20
            )
            updateSliderInput(
              session,
              "plot_point_opacity",
              value = plot_settings$point_opacity %||% 1
            )
            colourpicker::updateColourInput(
              session,
              "gradient_color1",
              value = plot_settings$gradient_color1 %||%
                plot_settings$custom_color1 %||%
                "#636EFA"
            )
            colourpicker::updateColourInput(
              session,
              "gradient_color2",
              value = plot_settings$gradient_color2 %||%
                plot_settings$custom_color2 %||%
                "#EF553B"
            )
            colourpicker::updateColourInput(
              session,
              "gradient_color3",
              value = plot_settings$gradient_color3 %||%
                plot_settings$custom_color3 %||%
                "#00CC96"
            )
            updateCheckboxInput(
              session,
              "plot_show_grid",
              value = plot_settings$show_grid %||% TRUE
            )
            updateCheckboxInput(
              session,
              "plot_show_legend",
              value = plot_settings$show_legend %||% TRUE
            )
            updateSelectInput(
              session,
              "plot_bg_color",
              selected = plot_settings$bg_color %||% "#fffef0"
            )
          }

          showNotification(
            "PCA results restored!",
            type = "message",
            duration = 3
          )
        } else {
          pca_rv$pca_result <- NULL
          pca_rv$pca_data <- NULL
          pca_rv$computed <- FALSE
        }

        # Restore annotation data if it exists
        if (!is.null(session_data$current_edits$annotation)) {
          progress$set(value = 0.9, detail = "Restoring gene annotations...")

          annotation_rv$annotated_data <- session_data$current_edits$annotation$annotated_data
          annotation_rv$ready <- TRUE
          annotation_rv$method <- session_data$current_edits$annotation$method %||%
            "gprofiler"

          if (!is.null(session_data$current_edits$annotation$organism)) {
            updateSelectInput(
              session,
              "annotation_organism",
              selected = session_data$current_edits$annotation$organism
            )
          }
          if (!is.null(session_data$current_edits$annotation$source)) {
            updateSelectInput(
              session,
              "annotation_source",
              selected = session_data$current_edits$annotation$source
            )
          }
          if (!is.null(session_data$current_edits$annotation$method)) {
            updateRadioButtons(
              session,
              "annotation_method",
              selected = session_data$current_edits$annotation$method
            )
          }

          showNotification(
            "Gene annotations restored!",
            type = "message",
            duration = 3
          )
        } else {
          annotation_rv$annotated_data <- NULL
          annotation_rv$ready <- FALSE
          annotation_rv$method <- NULL
        }

        progress$set(value = 1, detail = "Complete!")
        Sys.sleep(0.3)

        # Force UI refresh to fix scroll issues
        session$sendCustomMessage("forceUIRefresh", list(token = Sys.time()))

        showNotification(
          "Session loaded successfully! All edits preserved.",
          type = "message"
        )

        # Mark as saved (no unsaved changes after load)
        user_rv$has_unsaved_changes <- FALSE
        user_rv$last_save_time <- Sys.time()
        session$sendCustomMessage("setUnsavedChanges", FALSE)

        # Log activity
        log_session_activity(
          base_path = SESSIONS_BASE_PATH,
          email = user_rv$email,
          session_id = session_id,
          app_name = APP_NAME,
          action = "loaded",
          file_name = session_data$file_name,
          samples = nrow(data_rv$meta),
          has_pca = pca_rv$computed,
          has_annotations = isTRUE(annotation_rv$ready)
        )

        # Switch to editor tab
        shinyjs::runjs('$("a[data-value=\'editor\']").click();')

        # Track session load
        tryCatch(
          {
            email$track_change(
              type = "Session Loaded",
              details = glue::glue(
                "Loaded existing session with {nrow(data_rv$meta)} samples"
              )
            )
          },
          error = function(e) NULL
        )
      },
      error = function(e) {
        showNotification(
          paste("Error loading session:", e$message),
          type = "error",
          duration = 5
        )
      }
    )
  })

  # Handle Delete Session from modal
  observeEvent(input$modal_delete_session, {
    req(selected_session_rv())
    session_id <- selected_session_rv()

    # Show confirmation dialog
    showModal(modalDialog(
      title = tagList(icon("trash"), " Delete Session?"),
      div(
        style = "text-align: center;",
        p("Are you sure you want to delete this session?"),
        tags$code(session_id, style = "font-size: 1.1em;"),
        br(),
        br(),
        p(
          style = "color: #dc3545; font-weight: bold;",
          icon("exclamation-triangle"),
          " This action cannot be undone!"
        )
      ),
      footer = tagList(
        modalButton("Cancel"),
        actionButton(
          "confirm_delete_session",
          "Yes, Delete",
          class = "btn-danger"
        )
      ),
      size = "m",
      easyClose = TRUE
    ))
  })

  # Confirm delete session
  observeEvent(input$confirm_delete_session, {
    req(selected_session_rv())
    session_id <- selected_session_rv()
    removeModal()

    tryCatch(
      {
        # Delete the session file
        session_file <- file.path(
          get_user_session_dir(SESSIONS_BASE_PATH, user_rv$email, APP_NAME),
          paste0(session_id, ".rds")
        )

        if (file.exists(session_file)) {
          file.remove(session_file)
        }

        # Remove from registry
        delete_session(user_rv$email, APP_NAME, session_id, SESSIONS_BASE_PATH)

        # Log deletion
        log_session_activity(
          base_path = SESSIONS_BASE_PATH,
          email = user_rv$email,
          session_id = session_id,
          app_name = APP_NAME,
          action = "deleted"
        )

        # Refresh sessions list
        user_rv$sessions_list <- get_user_sessions(
          user_rv$email,
          APP_NAME,
          SESSIONS_BASE_PATH
        )

        # Clear selection
        selected_session_rv(NULL)

        showNotification(
          paste("Session deleted:", session_id),
          type = "message",
          duration = 3
        )
      },
      error = function(e) {
        showNotification(
          paste("Error deleting session:", e$message),
          type = "error",
          duration = 5
        )
      }
    )
  })

  # Start new session
  observeEvent(input$start_new, {
    # Show confirmation modal first
    showModal(modalDialog(
      title = "Start New Session?",
      p(
        "This will reset the entire application and clear all current data, including:"
      ),
      tags$ul(
        tags$li("Uploaded metadata and counts"),
        tags$li("All edits and changes"),
        tags$li("PCA analysis results"),
        tags$li("Current session ID")
      ),
      p(strong("This action cannot be undone."), style = "color: #dc3545;"),
      footer = tagList(
        modalButton("Cancel"),
        actionButton(
          "confirm_new_session",
          "Yes, Start Fresh",
          class = "btn-danger"
        )
      ),
      size = "m"
    ))
  })

  # Confirmed new session - reset everything
  observeEvent(input$confirm_new_session, {
    removeModal()

    # Complete reset of all data
    reset_all_data(reset_session_id = TRUE, reset_ui = TRUE)

    # Create new session ID
    new_session_id <- generate_session_id()
    user_rv$current_session_id <- new_session_id

    showNotification(
      glue::glue(
        "New session started: {new_session_id}\nAll data has been cleared."
      ),
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

    tryCatch(
      {
        # IMPORTANT: Reset all data before loading new data
        reset_all_data(reset_session_id = TRUE, reset_ui = TRUE)

        # Load RData file into environment
        env <- new.env()
        load(example_file, envir = env)

        # Check for required objects
        if (!all(c("metadata", "counts") %in% ls(env))) {
          showNotification(
            "Example data is corrupted (missing metadata/counts)",
            type = "error"
          )
          removeNotification("loading_example")
          return()
        }

        # Extract metadata and counts
        meta <- as.data.frame(env$metadata)
        meta$group <- as.character(meta$group)
        cnt <- as.data.frame(env$counts)

        # Validate data
        if (!all(c("label", "group") %in% colnames(meta))) {
          showNotification(
            "Example data is corrupted (missing label/group)",
            type = "error"
          )
          removeNotification("loading_example")
          return()
        }

        common <- intersect(meta$label, colnames(cnt))
        if (length(common) == 0) {
          showNotification(
            "Example data is corrupted (no matching labels)",
            type = "error"
          )
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
        counts_rv$counts <- cnt
        counts_rv$orig_counts <- cnt

        # Create new session if authenticated
        if (user_rv$authenticated) {
          user_rv$current_session_id <- generate_session_id()
          user_rv$current_file_name <- "Example_Data.RData"
          user_rv$has_unsaved_changes <- TRUE
          user_rv$last_save_time <- Sys.time()
          session$sendCustomMessage("setUnsavedChanges", TRUE)

          showNotification(
            glue::glue(
              "Example data loaded! Click 'Save Now' to save.\n{nrow(meta)} samples, {nrow(cnt)} genes"
            ),
            type = "message",
            duration = 5
          )
        } else {
          # Not authenticated - show different message
          showNotification(
            glue::glue(
              "Example data loaded! {nrow(meta)} samples, {nrow(cnt)} genes\nSign up to save your work."
            ),
            type = "message",
            duration = 5
          )
        }

        # Track example data load (only if email module is active)
        tryCatch(
          {
            email$track_change(
              type = "Example Data Loaded",
              details = glue::glue(
                "Loaded example dataset with {nrow(meta)} samples, {nrow(cnt)} genes"
              )
            )
          },
          error = function(e) NULL
        )

        removeNotification("loading_example")

        # Force UI refresh to fix scroll issues
        session$sendCustomMessage("forceUIRefresh", list(token = Sys.time()))

        # Switch to editor tab
        shinyjs::runjs('$("a[data-value=\'editor\']").click();')
      },
      error = function(e) {
        removeNotification("loading_example")
        showNotification(
          paste("Error loading example data:", e$message),
          type = "error",
          duration = 5
        )
      }
    )
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

        # IMPORTANT: Reset all data before storing new data
        reset_all_data(reset_session_id = TRUE, reset_ui = TRUE)

        # Store data in reactive values
        data_rv$meta <- meta
        data_rv$orig_meta <- meta
        data_rv$counts <- cnt
        data_rv$orig_counts <- cnt
        counts_rv$counts <- cnt
        counts_rv$orig_counts <- cnt

        # Create new session if authenticated
        if (user_rv$authenticated) {
          user_rv$current_session_id <- generate_session_id()
          user_rv$current_file_name <- input$file$name
          user_rv$has_unsaved_changes <- TRUE
          user_rv$last_save_time <- Sys.time()
          session$sendCustomMessage("setUnsavedChanges", TRUE)
        }

        # Track file upload
        tryCatch(
          {
            email$track_change(
              type = "File Upload",
              details = glue::glue(
                "Uploaded new dataset with {nrow(meta)} samples"
              )
            )
          },
          error = function(e) NULL
        )

        removeNotification("processing")

        # Force UI refresh to fix scroll issues
        session$sendCustomMessage("forceUIRefresh", list(token = Sys.time()))

        showNotification(
          "File Uploaded! Click 'Save Now' to save your session.",
          type = "message",
          duration = 5
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
    storage = NULL, # Storage now handled by user sessions
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

  # Keep meta_out for backward compatibility - call the reactive to get the value
  observe({
    if (!is.null(meta_module_out) && !is.null(meta_module_out$data)) {
      meta_out$data <- meta_module_out$data()
    }
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

    # Just return the UI components - servers are already initialized above
    tagList(
      editableTableUI("meta"),
      linkedTableUI("counts", "Count Matrix (Live)")
    )
  })

  # =================================================
  # GENE ANNOTATION
  # =================================================

  # Reactive values for annotation
  annotation_rv <- reactiveValues(
    annotated_data = NULL,
    ready = FALSE,
    method = NULL # "gprofiler" or "gtf"
  )

  # Output flag for conditional panel
  output$annotation_ready <- reactive({
    annotation_rv$ready
  })
  outputOptions(output, "annotation_ready", suspendWhenHidden = FALSE)

  # Badge showing annotation source
  output$annotation_source_badge <- renderUI({
    req(annotation_rv$ready)

    if (annotation_rv$method == "gprofiler") {
      span(
        class = "badge bg-primary",
        style = "font-size: 0.85em;",
        icon("cloud"),
        " g:Profiler"
      )
    } else if (annotation_rv$method == "gtf") {
      span(
        class = "badge bg-success",
        style = "font-size: 0.85em;",
        icon("file-code"),
        " GTF File"
      )
    }
  })

  # Clear annotations button
  observeEvent(input$clear_annotations, {
    annotation_rv$annotated_data <- NULL
    annotation_rv$ready <- FALSE
    annotation_rv$method <- NULL

    showNotification(
      "Annotations cleared.",
      type = "message",
      duration = 3
    )
  })

  # Run g:Profiler annotation
  observeEvent(input$run_annotation, {
    req(counts_rv$counts)

    # Create progress bar
    progress <- Progress$new()
    on.exit(progress$close())

    progress$set(message = "Gene Annotation (g:Profiler)", value = 0)

    tryCatch(
      {
        progress$set(value = 0.1, detail = "Preparing gene IDs...")

        # Get gene IDs from rownames of count matrix
        gene_ids <- rownames(counts_rv$counts)

        if (is.null(gene_ids) || length(gene_ids) == 0) {
          showNotification(
            "Error: No gene IDs found in count matrix rownames",
            type = "error",
            duration = 5
          )
          return()
        }

        cat("Annotating", length(gene_ids), "genes...\n")

        progress$set(
          value = 0.2,
          detail = paste0(
            "Querying g:Profiler for ",
            length(gene_ids),
            " genes..."
          )
        )

        # Use gconvert to get gene names and descriptions
        annotation_result <- gprofiler2::gconvert(
          query = gene_ids,
          organism = input$annotation_organism,
          target = "ENSG", # Always convert to Ensembl for consistent output
          filter_na = FALSE # Keep genes that couldn't be mapped
        )

        progress$set(value = 0.6, detail = "Processing results...")

        if (is.null(annotation_result) || nrow(annotation_result) == 0) {
          showNotification(
            "Warning: No annotations found. Check organism and ID type settings.",
            type = "warning",
            duration = 5
          )
          return()
        }

        progress$set(value = 0.7, detail = "Cleaning annotation data...")

        # Create a clean annotation table
        # Keep only relevant columns and rename them
        annotation_clean <- annotation_result %>%
          select(
            original_id = input,
            ensembl_id = target,
            gene_name = name,
            description = description
          ) %>%
          distinct(original_id, .keep_all = TRUE) # Keep first match per input

        progress$set(value = 0.8, detail = "Merging with count data...")

        # Merge with count data
        counts_with_anno <- counts_rv$counts %>%
          tibble::rownames_to_column("gene_id") %>%
          left_join(annotation_clean, by = c("gene_id" = "original_id")) %>%
          select(gene_id, ensembl_id, gene_name, description, everything())

        progress$set(value = 0.9, detail = "Finalizing...")

        annotation_rv$annotated_data <- counts_with_anno
        annotation_rv$ready <- TRUE
        annotation_rv$method <- "gprofiler"

        n_annotated <- sum(!is.na(counts_with_anno$gene_name))

        progress$set(value = 1, detail = "Complete!")
        Sys.sleep(0.3) # Brief pause to show completion

        showNotification(
          paste0(
            "Annotation complete! ",
            n_annotated,
            " of ",
            nrow(counts_with_anno),
            " genes annotated."
          ),
          type = "message",
          duration = 5
        )

        # Track annotation in email system
        email$track_change(
          type = "Gene Annotation",
          details = glue::glue(
            "Ran gene annotation with g:Profiler\n",
            "Organism: {input$annotation_organism}, ",
            "ID type: {input$annotation_source}\n",
            "Annotated: {n_annotated} of {nrow(counts_with_anno)} genes"
          )
        )
      },
      error = function(e) {
        showNotification(
          paste("Annotation failed:", e$message),
          type = "error",
          duration = 10
        )
        cat("Annotation Error:", e$message, "\n")
      }
    )
  })

  # Run GTF file annotation
  observeEvent(input$run_gtf_annotation, {
    req(counts_rv$counts)
    req(input$gtf_file)

    # Create progress bar
    progress <- Progress$new()
    on.exit(progress$close())

    progress$set(message = "Gene Annotation (GTF)", value = 0)

    tryCatch(
      {
        progress$set(value = 0.1, detail = "Reading GTF file...")

        # Read GTF file
        gtf_path <- input$gtf_file$datapath
        gtf_name <- input$gtf_file$name

        # Check if gzipped
        if (grepl("\\.gz$", gtf_name)) {
          gtf_lines <- readLines(gzfile(gtf_path))
        } else {
          gtf_lines <- readLines(gtf_path)
        }

        progress$set(value = 0.3, detail = "Parsing GTF attributes...")

        # Filter for gene features only (skip comments)
        gtf_lines <- gtf_lines[!grepl("^#", gtf_lines)]
        gene_lines <- gtf_lines[grepl("\tgene\t", gtf_lines)]

        # If no "gene" features, try all lines
        if (length(gene_lines) == 0) {
          gene_lines <- gtf_lines
        }

        progress$set(
          value = 0.5,
          detail = paste0("Processing ", length(gene_lines), " features...")
        )

        # Parse attributes from GTF
        parse_gtf_attr <- function(attr_string, attr_name) {
          # Try standard GTF format: attr_name "value"
          pattern <- paste0(attr_name, '\\s*[="]([^";\n]+)')
          match <- regmatches(attr_string, regexec(pattern, attr_string))[[1]]
          if (length(match) > 1) {
            return(gsub('"', '', match[2]))
          }

          # Try GFF3 format: attr_name=value
          pattern2 <- paste0(attr_name, '=([^;]+)')
          match2 <- regmatches(attr_string, regexec(pattern2, attr_string))[[1]]
          if (length(match2) > 1) {
            return(match2[2])
          }

          return(NA_character_)
        }

        # Extract gene info
        gene_id_attr <- input$gtf_gene_id_attr %||% "gene_id"

        gtf_data <- data.frame(
          gene_id = sapply(gene_lines, function(x) {
            attrs <- strsplit(x, "\t")[[1]][9]
            parse_gtf_attr(attrs, gene_id_attr)
          }),
          gene_name = sapply(gene_lines, function(x) {
            attrs <- strsplit(x, "\t")[[1]][9]
            parse_gtf_attr(attrs, "gene_name")
          }),
          description = sapply(gene_lines, function(x) {
            attrs <- strsplit(x, "\t")[[1]][9]
            # Try gene_biotype first, then description
            biotype <- parse_gtf_attr(attrs, "gene_biotype")
            if (is.na(biotype)) {
              biotype <- parse_gtf_attr(attrs, "description")
            }
            biotype
          }),
          # Extract genomic coordinates (columns 1, 4, 5)
          chromosome = sapply(gene_lines, function(x) {
            strsplit(x, "\t")[[1]][1]
          }),
          start = as.integer(sapply(gene_lines, function(x) {
            strsplit(x, "\t")[[1]][4]
          })),
          end = as.integer(sapply(gene_lines, function(x) {
            strsplit(x, "\t")[[1]][5]
          })),
          stringsAsFactors = FALSE
        )

        # Remove rows with NA gene_id
        gtf_data <- gtf_data[!is.na(gtf_data$gene_id), ]

        # Handle 1-to-many relationships: collapse multiple gene names per gene_id
        # Group by gene_id and concatenate unique non-NA values with comma
        # For coordinates: take first chromosome, min start, max end (full gene span)
        gtf_data <- gtf_data %>%
          group_by(gene_id) %>%
          summarise(
            gene_name = paste(unique(na.omit(gene_name)), collapse = ", "),
            description = paste(unique(na.omit(description)), collapse = ", "),
            chromosome = chromosome[1],
            start = min(start, na.rm = TRUE),
            end = max(end, na.rm = TRUE),
            .groups = "drop"
          ) %>%
          mutate(
            # Replace empty strings with NA
            gene_name = ifelse(gene_name == "", NA_character_, gene_name),
            description = ifelse(description == "", NA_character_, description),
            # Create coordinates column in chr:start:end format
            coordinates = paste0(chromosome, ":", start, ":", end)
          ) %>%
          select(-chromosome, -start, -end) # Remove individual columns, keep coordinates

        progress$set(
          value = 0.7,
          detail = paste0("Found ", nrow(gtf_data), " unique genes...")
        )

        if (nrow(gtf_data) == 0) {
          showNotification(
            "Error: Could not parse any genes from GTF file. Check the file format.",
            type = "error",
            duration = 5
          )
          return()
        }

        progress$set(value = 0.8, detail = "Merging with count data...")

        # Get gene IDs from count matrix
        count_gene_ids <- rownames(counts_rv$counts)

        # Clean gene IDs (remove version numbers like .1, .2 for matching)
        clean_count_ids <- gsub("\\.[0-9]+$", "", count_gene_ids)
        clean_gtf_ids <- gsub("\\.[0-9]+$", "", gtf_data$gene_id)

        # Create lookup with cleaned IDs
        gtf_lookup <- gtf_data
        gtf_lookup$clean_id <- clean_gtf_ids

        # Merge with count data
        counts_with_anno <- counts_rv$counts %>%
          tibble::rownames_to_column("gene_id") %>%
          mutate(clean_id = gsub("\\.[0-9]+$", "", gene_id)) %>%
          left_join(
            gtf_lookup %>%
              select(clean_id, gene_name, description, coordinates),
            by = "clean_id"
          ) %>%
          mutate(ensembl_id = gene_id) %>% # Use gene_id as ensembl_id for GTF
          select(
            gene_id,
            ensembl_id,
            gene_name,
            description,
            coordinates,
            everything(),
            -clean_id
          )

        progress$set(value = 0.9, detail = "Finalizing...")

        annotation_rv$annotated_data <- counts_with_anno
        annotation_rv$ready <- TRUE
        annotation_rv$method <- "gtf"

        n_annotated <- sum(!is.na(counts_with_anno$gene_name))

        progress$set(value = 1, detail = "Complete!")
        Sys.sleep(0.3)

        showNotification(
          paste0(
            "GTF annotation complete! ",
            n_annotated,
            " of ",
            nrow(counts_with_anno),
            " genes annotated."
          ),
          type = "message",
          duration = 5
        )

        # Track in email
        email$track_change(
          type = "Gene Annotation (GTF)",
          details = glue::glue(
            "Parsed GTF file: {gtf_name}\n",
            "Annotated: {n_annotated} of {nrow(counts_with_anno)} genes"
          )
        )
      },
      error = function(e) {
        showNotification(
          paste("GTF parsing failed:", e$message),
          type = "error",
          duration = 10
        )
        cat("GTF Error:", e$message, "\n")
      }
    )
  })

  # Render annotation table
  output$annotation_table <- renderReactable({
    req(annotation_rv$annotated_data)

    # Show annotation columns for preview
    # Include coordinates if available (GTF method)
    cols_to_show <- c("gene_id", "ensembl_id", "gene_name", "description")

    has_coords <- "coordinates" %in% colnames(annotation_rv$annotated_data)
    if (has_coords) {
      cols_to_show <- c(cols_to_show, "coordinates")
    }

    display_data <- annotation_rv$annotated_data %>%
      select(any_of(cols_to_show))

    # Build column definitions
    col_defs <- list(
      gene_id = colDef(
        name = "Gene ID",
        minWidth = 180,
        filterable = TRUE
      ),
      ensembl_id = colDef(
        name = "Ensembl ID",
        minWidth = 180,
        filterable = TRUE
      ),
      gene_name = colDef(
        name = "Gene Name",
        minWidth = 100,
        filterable = TRUE
      ),
      description = colDef(
        name = "Description",
        minWidth = 250,
        filterable = TRUE
      )
    )

    # Add coordinates column if available - use JS for server-side compatibility
    if (has_coords) {
      col_defs$coordinates <- colDef(
        name = "Coordinates",
        minWidth = 180,
        filterable = TRUE,
        # Use JavaScript cell renderer for server-side compatibility
        cell = JS(
          "function(cellInfo) {
          var value = cellInfo.value;
          if (!value || value === '') return '';
          var parts = value.split(':');
          if (parts.length === 3) {
            var chr = parts[0];
            var start = parseInt(parts[1]).toLocaleString();
            var end = parseInt(parts[2]).toLocaleString();
            return chr + ':' + start + '-' + end;
          }
          return value;
        }"
        )
      )
    }

    reactable(
      display_data,
      columns = col_defs,
      filterable = TRUE,
      searchable = TRUE,
      striped = TRUE,
      highlight = TRUE,
      bordered = TRUE,
      compact = TRUE,
      defaultPageSize = 15,
      showPageSizeOptions = TRUE,
      pageSizeOptions = c(10, 15, 25, 50, 100),
      theme = reactableTheme(
        borderColor = "#ddd",
        stripedColor = "#f8f9fa",
        highlightColor = "#fff3cd",
        headerStyle = list(
          borderBottom = "2px solid #dee2e6",
          fontWeight = "600"
        ),
        filterInputStyle = list(
          paddingTop = "4px",
          paddingBottom = "4px"
        )
      )
    )
  })

  # Download annotated counts
  output$download_annotations <- downloadHandler(
    filename = function() {
      paste0("annotated_counts_", Sys.Date(), ".csv")
    },
    content = function(file) {
      req(annotation_rv$annotated_data)
      write.csv(annotation_rv$annotated_data, file, row.names = FALSE)

      showNotification(
        "Annotated counts downloaded!",
        type = "message",
        duration = 3
      )
    }
  )

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
      selected = if ("group" %in% color_choices) "group" else color_choices[1]
    )
  })

  # Dynamic color variable dropdown for heatmap
  output$heatmap_color_ui <- renderUI({
    # Get current metadata using helper function
    current_meta <- get_current_metadata()
    req(current_meta)

    # Get all column names from metadata
    color_choices <- colnames(current_meta)

    selectInput(
      "heatmap_color_by",
      "Color samples by:",
      choices = color_choices,
      selected = if ("group" %in% color_choices) "group" else color_choices[1]
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

    tryCatch(
      {
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
            paste(
              "Warning: Samples in counts but not in metadata:",
              paste(missing_in_meta, collapse = ", ")
            ),
            type = "warning",
            duration = 5
          )
        }

        if (length(missing_in_counts) > 0) {
          # Filter metadata to only include samples that exist in count matrix
          current_meta <- current_meta[
            current_meta$label %in% sample_labels_counts,
            ,
            drop = FALSE
          ]
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
        current_meta <- current_meta[
          current_meta$label %in% common_samples,
          ,
          drop = FALSE
        ]

        cat("PCA using", ncol(count_matrix), "samples\n")
        cat("Metadata has", nrow(current_meta), "samples\n")
        cat(
          "Sample labels:",
          paste(head(colnames(count_matrix)), collapse = ", "),
          "...\n"
        )

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
        colData_df <- current_meta[
          match(sample_labels, current_meta$label),
          ,
          drop = FALSE
        ]
        rownames(colData_df) <- colData_df$label

        # Create DESeqDataSet
        dds <- DESeq2::DESeqDataSetFromMatrix(
          countData = count_matrix,
          colData = colData_df,
          design = ~1 # No design needed for VST
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
          intgroup = "label", # Just use label, we'll color by user choice later
          returnData = TRUE,
          ntop = input$pca_ntop
        )

        # Get the full PCA object for variance explained
        vst_mat <- SummarizedExperiment::assay(vst_result)

        # Select top genes by variance
        rv <- matrixStats::rowVars(vst_mat)
        select <- order(rv, decreasing = TRUE)[seq_len(min(
          input$pca_ntop,
          length(rv)
        ))]

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
          dds = dds, # Store DDS object
          pca_data = pca_coords,
          var_explained = pca_full$sdev^2 / sum(pca_full$sdev^2) * 100,
          ntop = input$pca_ntop,
          params_used = list(
            # Store actual parameters used
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
          sep = ", "
        )

        email$track_change(
          type = "PCA Analysis (DESeq2)",
          details = glue::glue(
            "Ran DESeq2 PCA with VST on current data\nParameters: {pca_params_text}"
          )
        )
      },
      error = function(e) {
        removeNotification("pca_running")
        showNotification(
          paste("PCA failed:", e$message),
          type = "error",
          duration = 10
        )
        cat("PCA Error Details:\n", e$message, "\n")
        pca_rv$computed <- FALSE
      }
    )
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

  # Download PCA plot handler - uses ggplot2 for static exports (more reliable in Docker)
  output$download_pca_plot <- downloadHandler(
    filename = function() {
      filename <- if (
        !is.null(input$plot_download_filename) &&
          input$plot_download_filename != ""
      ) {
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
        plot_data$color_var <- plot_data$sample
      } else {
        plot_data <- merge(
          pca_data,
          current_meta,
          by.x = "sample",
          by.y = "label",
          all.x = TRUE
        )
        plot_data$color_var <- plot_data[[input$pca_color_by]]
      }

      # Get variance explained
      var_explained <- pca_rv$pca_result$var_explained

      # Get customization options
      plot_title <- if (!is.null(input$plot_title) && input$plot_title != "") {
        input$plot_title
      } else {
        "PCA Score Plot"
      }
      plot_xlabel <- sprintf(
        "PC%d (%.1f%% variance)",
        pc_x,
        var_explained[pc_x]
      )
      plot_ylabel <- sprintf(
        "PC%d (%.1f%% variance)",
        pc_y,
        var_explained[pc_y]
      )
      point_size <- if (!is.null(input$plot_point_size)) {
        input$plot_point_size / 5
      } else {
        4
      } # Scale for ggplot
      point_opacity <- if (!is.null(input$plot_point_opacity)) {
        input$plot_point_opacity
      } else {
        1
      }
      show_grid <- if (!is.null(input$plot_show_grid)) {
        input$plot_show_grid
      } else {
        TRUE
      }
      show_legend <- if (!is.null(input$plot_show_legend)) {
        input$plot_show_legend
      } else {
        TRUE
      }
      bg_color <- if (!is.null(input$plot_bg_color)) {
        input$plot_bg_color
      } else {
        "#fffef0"
      }

      # Get number of unique groups for color generation
      n_colors <- length(unique(plot_data$color_var))

      # 3-color gradient interpolation
      c1 <- if (!is.null(input$gradient_color1)) {
        input$gradient_color1
      } else {
        "#636EFA"
      }
      c2 <- if (!is.null(input$gradient_color2)) {
        input$gradient_color2
      } else {
        "#EF553B"
      }
      c3 <- if (!is.null(input$gradient_color3)) {
        input$gradient_color3
      } else {
        "#00CC96"
      }
      color_values <- colorRampPalette(c(c1, c2, c3))(n_colors)

      # Get PC column names
      pc_x_col <- paste0("PC", pc_x)
      pc_y_col <- paste0("PC", pc_y)

      # Get format
      format <- input$plot_download_format %||% "png"

      # Get download dimensions
      width <- (input$plot_download_width %||% 1200) / 100 # Convert pixels to inches (approx)
      height <- (input$plot_download_height %||% 800) / 100

      if (format == "html") {
        # HTML - create interactive plotly plot
        p <- plot_ly(
          plot_data,
          x = ~ get(pc_x_col),
          y = ~ get(pc_y_col),
          color = ~color_var,
          colors = color_values,
          type = "scatter",
          mode = "markers",
          marker = list(
            size = input$plot_point_size %||% 20,
            opacity = point_opacity,
            line = list(color = "white", width = 1)
          ),
          text = ~ paste(
            "Sample:",
            sample,
            "<br>",
            input$pca_color_by,
            ":",
            color_var
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
            margin = list(t = 100, b = 80, l = 80, r = 60, pad = 10)
          )

        htmlwidgets::saveWidget(
          plotly::as_widget(p),
          file,
          selfcontained = TRUE
        )
        removeModal()
        showNotification(
          "Plot downloaded as interactive HTML",
          type = "message",
          duration = 3
        )
      } else {
        # PNG/PDF/SVG - use ggplot2 for reliable static export
        p <- ggplot(
          plot_data,
          aes(x = .data[[pc_x_col]], y = .data[[pc_y_col]], color = color_var)
        ) +
          geom_point(size = point_size, alpha = point_opacity, stroke = 0.5) +
          scale_color_manual(values = color_values, name = input$pca_color_by) +
          labs(
            title = plot_title,
            x = plot_xlabel,
            y = plot_ylabel
          ) +
          theme_minimal(base_size = 14) +
          theme(
            plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
            panel.background = element_rect(fill = bg_color, color = NA),
            plot.background = element_rect(fill = "white", color = NA),
            panel.grid.major = if (show_grid) {
              element_line(color = "#e0e0e0", linewidth = 0.3)
            } else {
              element_blank()
            },
            panel.grid.minor = element_blank(),
            legend.position = if (show_legend) "right" else "none",
            axis.line = element_line(color = "#333333", linewidth = 0.5),
            axis.ticks = element_line(color = "#333333"),
            plot.margin = margin(20, 20, 20, 20)
          ) +
          geom_hline(
            yintercept = 0,
            linetype = "dashed",
            color = "#999999",
            linewidth = 0.3
          ) +
          geom_vline(
            xintercept = 0,
            linetype = "dashed",
            color = "#999999",
            linewidth = 0.3
          )

        # Save based on format
        if (format == "png") {
          ggsave(
            file,
            plot = p,
            width = width,
            height = height,
            dpi = 150,
            bg = "white"
          )
        } else if (format == "pdf") {
          ggsave(
            file,
            plot = p,
            width = width,
            height = height,
            device = "pdf",
            bg = "white"
          )
        } else if (format == "svg") {
          ggsave(
            file,
            plot = p,
            width = width,
            height = height,
            device = "svg",
            bg = "white"
          )
        }

        removeModal()
        showNotification(
          paste0("Plot downloaded as ", toupper(format)),
          type = "message",
          duration = 3
        )
      }
    }
  )

  # Reset plot options to defaults
  observeEvent(input$reset_plot_options, {
    updateTextInput(session, "plot_title", value = "PCA Score Plot")
    updateSliderInput(session, "plot_point_size", value = 20)
    updateSliderInput(session, "plot_point_opacity", value = 1)
    colourpicker::updateColourInput(
      session,
      "gradient_color1",
      value = "#636EFA"
    )
    colourpicker::updateColourInput(
      session,
      "gradient_color2",
      value = "#EF553B"
    )
    colourpicker::updateColourInput(
      session,
      "gradient_color3",
      value = "#00CC96"
    )
    updateCheckboxInput(session, "plot_show_grid", value = TRUE)
    updateCheckboxInput(session, "plot_show_legend", value = TRUE)
    updateSelectInput(session, "plot_bg_color", selected = "#fffef0")

    showNotification(
      "Plot options reset to defaults",
      type = "message",
      duration = 2
    )
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
        all.x = FALSE, # Only keep samples that have PCA scores
        all.y = TRUE # Keep all PCA samples
      )

      # Reorder columns: metadata first, then PC columns
      pc_cols <- grep("^PC[0-9]+$", colnames(export_data), value = TRUE)
      meta_cols <- setdiff(colnames(export_data), pc_cols)
      export_data <- export_data[, c(meta_cols, pc_cols)]

      write.csv(export_data, file, row.names = FALSE)

      showNotification(
        paste(
          "Exported",
          nrow(export_data),
          "samples with",
          length(pc_cols),
          "PCs and metadata"
        ),
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
      return(
        plotly_empty() %>%
          layout(title = "Not enough principal components available")
      )
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
    plot_title <- if (!is.null(input$plot_title)) {
      input$plot_title
    } else {
      "PCA Score Plot"
    }
    # Always show variance percentage on axes
    plot_xlabel <- sprintf("PC%d (%.1f%% variance)", pc_x, var_explained[pc_x])
    plot_ylabel <- sprintf("PC%d (%.1f%% variance)", pc_y, var_explained[pc_y])
    point_size <- if (!is.null(input$plot_point_size)) {
      input$plot_point_size
    } else {
      20
    }
    point_opacity <- if (!is.null(input$plot_point_opacity)) {
      input$plot_point_opacity
    } else {
      1
    }
    show_grid <- if (!is.null(input$plot_show_grid)) {
      input$plot_show_grid
    } else {
      TRUE
    }
    show_legend <- if (!is.null(input$plot_show_legend)) {
      input$plot_show_legend
    } else {
      TRUE
    }
    bg_color <- if (!is.null(input$plot_bg_color)) {
      input$plot_bg_color
    } else {
      "#fffef0"
    }

    # Get number of unique groups for color generation
    n_colors <- length(unique(color_var))

    # 3-color gradient interpolation - works for any number of groups
    c1 <- if (!is.null(input$gradient_color1)) {
      input$gradient_color1
    } else {
      "#636EFA"
    }
    c2 <- if (!is.null(input$gradient_color2)) {
      input$gradient_color2
    } else {
      "#EF553B"
    }
    c3 <- if (!is.null(input$gradient_color3)) {
      input$gradient_color3
    } else {
      "#00CC96"
    }
    color_values <- colorRampPalette(c(c1, c2, c3))(n_colors)

    # Create plot
    pc_x_col <- paste0("PC", pc_x)
    pc_y_col <- paste0("PC", pc_y)

    p <- plot_ly(
      plot_data,
      x = ~ get(pc_x_col),
      y = ~ get(pc_y_col),
      color = color_var,
      colors = color_values,
      type = "scatter",
      mode = "markers",
      marker = list(
        size = point_size,
        opacity = point_opacity,
        line = list(color = "white", width = 1)
      ),
      text = ~ paste(
        "Sample:",
        sample,
        "<br>",
        input$pca_color_by,
        ":",
        color_var
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

    cat(sprintf(
      "\nTotal variance explained by first %d PCs: %.2f%%\n",
      n_show,
      sum(var_explained[1:n_show])
    ))

    cat("\n")
    cat("=", rep("=", 40), "\n", sep = "")
    cat("Parameters Used for This PCA:\n")
    cat("=", rep("=", 40), "\n", sep = "")

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
    cat(
      "  Blind to design:",
      if (!is.null(pca_rv$pca_result$params_used$blind)) {
        pca_rv$pca_result$params_used$blind
      } else {
        "TRUE (assumed)"
      },
      "\n"
    )
    cat(
      "  Fit type:",
      if (!is.null(pca_rv$pca_result$params_used$fitType)) {
        pca_rv$pca_result$params_used$fitType
      } else {
        "parametric (assumed)"
      },
      "\n"
    )

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
    cat(
      "  Blind:",
      if (!is.null(input$vst_blind)) input$vst_blind else "TRUE",
      "\n"
    )
    cat(
      "  Fit type:",
      if (!is.null(input$vst_fitType)) input$vst_fitType else "parametric",
      "\n"
    )
    cat(
      "  Top genes:",
      if (!is.null(input$pca_ntop)) input$pca_ntop else "500",
      "\n"
    )
  })

  # =================================================
  # TOP GENES TAB - LOADING TABLE & HEATMAP
  # =================================================

  # Update PC choices when PCA is computed (for both tabs)
  observe({
    req(pca_rv$pca_result)

    n_pcs <- ncol(pca_rv$pca_result$pca$rotation)
    pc_choices <- setNames(1:min(n_pcs, 10), paste0("PC", 1:min(n_pcs, 10)))

    # Update Loading Table PC selector
    updateSelectInput(
      session,
      "topgenes_pc",
      choices = pc_choices,
      selected = 1
    )

    # Update Heatmap PC selector
    updateSelectInput(
      session,
      "heatmap_pc",
      choices = pc_choices,
      selected = 1
    )
  })

  # Reactive: Get top loading genes for Loading Table
  top_loading_genes <- reactive({
    req(pca_rv$pca_result)
    req(input$topgenes_pc, input$topgenes_n, input$topgenes_direction)

    # Get loadings (rotation matrix)
    loadings <- pca_rv$pca_result$pca$rotation
    pc_idx <- as.integer(input$topgenes_pc)
    pc_col <- paste0("PC", pc_idx)

    # Get loadings for selected PC
    pc_loadings <- loadings[, pc_idx]

    # Create data frame
    loading_df <- data.frame(
      Gene = names(pc_loadings),
      Loading = pc_loadings,
      Abs_Loading = abs(pc_loadings),
      stringsAsFactors = FALSE
    )

    # Filter by direction
    if (input$topgenes_direction == "positive") {
      loading_df <- loading_df[loading_df$Loading > 0, ]
      loading_df <- loading_df[order(-loading_df$Loading), ]
    } else if (input$topgenes_direction == "negative") {
      loading_df <- loading_df[loading_df$Loading < 0, ]
      loading_df <- loading_df[order(loading_df$Loading), ]
    } else {
      # Both - sort by absolute value
      loading_df <- loading_df[order(-loading_df$Abs_Loading), ]
    }

    # Get top N
    n_genes <- min(input$topgenes_n, nrow(loading_df))
    loading_df <- loading_df[1:n_genes, ]

    # Add rank
    loading_df$Rank <- 1:nrow(loading_df)

    # Add gene annotations if available
    if (isTRUE(annotation_rv$ready) && !is.null(annotation_rv$annotated_data)) {
      # Check if coordinates column exists (GTF method)
      has_coordinates <- "coordinates" %in%
        colnames(annotation_rv$annotated_data)

      if (has_coordinates) {
        anno_data <- annotation_rv$annotated_data %>%
          select(gene_id, gene_name, description, coordinates) %>%
          distinct(gene_id, .keep_all = TRUE)
      } else {
        anno_data <- annotation_rv$annotated_data %>%
          select(gene_id, gene_name, description) %>%
          distinct(gene_id, .keep_all = TRUE)
      }

      loading_df <- loading_df %>%
        left_join(anno_data, by = c("Gene" = "gene_id")) %>%
        mutate(
          Gene_Name = ifelse(
            is.na(gene_name) | gene_name == "",
            Gene,
            gene_name
          ),
          Description = ifelse(is.na(description), "", description)
        ) %>%
        select(-gene_name)

      # Add empty coordinates column if not from GTF
      if (!has_coordinates) {
        loading_df$coordinates <- NA_character_
      }
    } else {
      # No annotations available - use gene ID as name
      loading_df$Gene_Name <- loading_df$Gene
      loading_df$Description <- ""
      loading_df$coordinates <- NA_character_
    }

    # Add variance explained for context
    var_explained <- pca_rv$pca_result$var_explained[pc_idx]
    attr(loading_df, "pc_name") <- pc_col
    attr(loading_df, "var_explained") <- var_explained

    loading_df
  })

  # Reactive: Get top loading genes for Heatmap (separate controls)
  heatmap_top_genes <- reactive({
    req(pca_rv$pca_result)

    # Get input values with defaults (inputs might not exist until tab is rendered)
    pc_val <- input$heatmap_pc %||% 1
    n_genes_val <- input$heatmap_n_genes %||% 30

    # Get loadings (rotation matrix)
    loadings <- pca_rv$pca_result$pca$rotation
    pc_idx <- as.integer(pc_val)
    pc_col <- paste0("PC", pc_idx)

    # Get loadings for selected PC
    pc_loadings <- loadings[, pc_idx]

    # Create data frame - always use absolute value for heatmap
    loading_df <- data.frame(
      Gene = names(pc_loadings),
      Loading = pc_loadings,
      Abs_Loading = abs(pc_loadings),
      stringsAsFactors = FALSE
    )

    # Sort by absolute value
    loading_df <- loading_df[order(-loading_df$Abs_Loading), ]

    # Get top N
    n_genes <- min(n_genes_val, nrow(loading_df))
    loading_df <- loading_df[1:n_genes, ]

    # Add variance explained for context
    var_explained <- pca_rv$pca_result$var_explained[pc_idx]
    attr(loading_df, "pc_name") <- pc_col
    attr(loading_df, "var_explained") <- var_explained

    loading_df
  })

  # Render Loading Table
  output$loading_table <- DT::renderDataTable({
    req(top_loading_genes())

    df <- top_loading_genes()
    pc_name <- attr(df, "pc_name")
    var_exp <- attr(df, "var_explained")

    # Check if coordinates are available (from GTF)
    has_coords <- "coordinates" %in% colnames(df) && any(!is.na(df$coordinates))

    # Format for display - include gene name, description, and coordinates if available
    if (has_coords) {
      display_df <- data.frame(
        Rank = df$Rank,
        Gene_ID = df$Gene,
        Gene_Name = df$Gene_Name,
        Coordinates = df$coordinates,
        Description = df$Description,
        Loading = round(df$Loading, 5),
        `Abs Loading` = round(df$Abs_Loading, 5),
        check.names = FALSE
      )
      col_defs <- list(
        list(className = 'dt-center', targets = c(0, 5, 6)),
        list(width = '150px', targets = 3), # Coordinates column width
        list(width = '200px', targets = 4) # Description column width
      )
    } else {
      display_df <- data.frame(
        Rank = df$Rank,
        Gene_ID = df$Gene,
        Gene_Name = df$Gene_Name,
        Description = df$Description,
        Loading = round(df$Loading, 5),
        `Abs Loading` = round(df$Abs_Loading, 5),
        check.names = FALSE
      )
      col_defs <- list(
        list(className = 'dt-center', targets = c(0, 4, 5)),
        list(width = '200px', targets = 3) # Description column width
      )
    }

    DT::datatable(
      display_df,
      filter = 'top', # Enable column filters at the top
      caption = htmltools::tags$caption(
        style = "caption-side: top; text-align: left; font-size: 14px; font-weight: bold;",
        paste0(
          pc_name,
          " Top Loading Genes (",
          sprintf("%.1f%%", var_exp),
          " variance explained)"
        )
      ),
      options = list(
        pageLength = 15,
        lengthMenu = c(10, 15, 25, 50),
        dom = 'lfrtip',
        scrollX = TRUE,
        columnDefs = col_defs
      ),
      rownames = FALSE,
      class = "display compact stripe hover"
    ) %>%
      DT::formatStyle(
        "Loading",
        background = DT::styleColorBar(range(display_df$Loading), "#e3f2fd"),
        backgroundSize = '100% 90%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center'
      ) %>%
      DT::formatStyle(
        "Abs Loading",
        fontWeight = "bold"
      ) %>%
      DT::formatStyle(
        "Description",
        fontSize = '11px',
        color = '#666'
      )
  })

  # Render Interactive Heatmap with heatmaply
  output$topgenes_heatmap <- renderPlotly({
    # Check requirements
    req(pca_rv$pca_result)
    req(pca_rv$pca_result$vst)
    req(pca_rv$pca_result$pca)

    # Get loadings
    loadings <- pca_rv$pca_result$pca$rotation
    pc_idx <- as.integer(input$heatmap_pc %||% 1)
    n_genes <- input$heatmap_n_genes %||% 30

    # Get top genes by absolute loading
    pc_loadings <- loadings[, pc_idx]
    top_idx <- order(abs(pc_loadings), decreasing = TRUE)[
      1:min(n_genes, length(pc_loadings))
    ]
    top_gene_ids <- names(pc_loadings)[top_idx]

    # Get VST expression matrix
    vst_mat <- SummarizedExperiment::assay(pca_rv$pca_result$vst)

    # Subset to top genes
    vst_subset <- vst_mat[top_gene_ids, , drop = FALSE]

    # Replace rownames with gene names if annotations are available
    if (isTRUE(annotation_rv$ready) && !is.null(annotation_rv$annotated_data)) {
      anno_data <- annotation_rv$annotated_data %>%
        select(gene_id, gene_name) %>%
        distinct(gene_id, .keep_all = TRUE)

      # Create a named vector for mapping gene_id to gene_name
      gene_name_map <- setNames(anno_data$gene_name, anno_data$gene_id)

      # Get new row names - use gene_name if available, otherwise keep gene_id
      new_rownames <- sapply(rownames(vst_subset), function(gid) {
        gname <- gene_name_map[gid]
        if (!is.na(gname) && gname != "" && !is.null(gname)) {
          return(gname)
        } else {
          return(gid)
        }
      })

      # Handle duplicates by appending gene_id
      if (any(duplicated(new_rownames))) {
        dup_idx <- which(
          duplicated(new_rownames) | duplicated(new_rownames, fromLast = TRUE)
        )
        new_rownames[dup_idx] <- paste0(
          new_rownames[dup_idx],
          " (",
          rownames(vst_subset)[dup_idx],
          ")"
        )
      }

      rownames(vst_subset) <- new_rownames
    }

    # Get current metadata
    current_meta <- get_current_metadata()
    req(current_meta)
    req(nrow(current_meta) > 0)

    # Match samples
    common_samples <- intersect(colnames(vst_subset), current_meta$label)
    req(length(common_samples) > 0)

    vst_subset <- vst_subset[, common_samples, drop = FALSE]
    meta_ordered <- current_meta[match(common_samples, current_meta$label), ]

    # Get options with defaults
    scale_rows <- if (is.null(input$heatmap_scale_rows)) {
      TRUE
    } else {
      input$heatmap_scale_rows
    }
    cluster_rows <- if (is.null(input$heatmap_cluster_rows)) {
      FALSE
    } else {
      input$heatmap_cluster_rows
    }
    cluster_cols <- if (is.null(input$heatmap_cluster_cols)) {
      FALSE
    } else {
      input$heatmap_cluster_cols
    }
    show_dendro <- if (is.null(input$heatmap_show_dendro)) {
      TRUE
    } else {
      input$heatmap_show_dendro
    }
    show_rownames <- if (is.null(input$heatmap_show_rownames)) {
      TRUE
    } else {
      input$heatmap_show_rownames
    }
    show_colnames <- if (is.null(input$heatmap_show_colnames)) {
      TRUE
    } else {
      input$heatmap_show_colnames
    }
    heatmap_colors <- input$heatmap_colors %||% "RdBu"
    dist_method <- input$heatmap_dist_method %||% "pearson"
    clust_method <- input$heatmap_clust_method %||% "ward.D2"

    # Scale rows if requested (z-score)
    if (scale_rows) {
      vst_subset <- t(scale(t(vst_subset)))
      vst_subset[is.nan(vst_subset)] <- 0
    }

    # Color palette
    color_pal <- colorRampPalette(rev(RColorBrewer::brewer.pal(11, "RdBu")))(
      100
    )
    if (heatmap_colors == "viridis") {
      color_pal <- viridisLite::viridis(100)
    }
    if (heatmap_colors == "PRGn") {
      color_pal <- colorRampPalette(rev(RColorBrewer::brewer.pal(11, "PRGn")))(
        100
      )
    }
    if (heatmap_colors == "RdYlBu") {
      color_pal <- colorRampPalette(rev(RColorBrewer::brewer.pal(
        11,
        "RdYlBu"
      )))(100)
    }
    if (heatmap_colors == "plasma") {
      color_pal <- viridisLite::plasma(100)
    }

    # Get color variable from input (default to "group" if not set or not in metadata)
    color_var <- input$heatmap_color_by %||% "group"
    if (!color_var %in% colnames(meta_ordered)) {
      color_var <- "group"
    }

    # Get annotation values for the selected color variable
    annotation_values <- meta_ordered[[color_var]]

    # Annotation colors for the selected variable
    n_levels <- length(unique(annotation_values))
    annotation_cols <- setNames(
      colorRampPalette(c(
        "#636EFA",
        "#EF553B",
        "#00CC96",
        "#AB63FA",
        "#FFA15A",
        "#19D3F3",
        "#FF6692",
        "#B6E880"
      ))(n_levels),
      unique(annotation_values)
    )

    # Create annotation data frame with dynamic column name
    col_side_df <- data.frame(value = annotation_values)
    colnames(col_side_df) <- color_var

    # Title
    pc_name <- paste0("PC", pc_idx)
    main_title <- paste0(
      "Top ",
      nrow(vst_subset),
      " Loading Genes - ",
      pc_name,
      if (scale_rows) " (Z-score)" else " (VST)"
    )

    # Determine dendrogram setting
    dendro_setting <- "none"
    if (show_dendro) {
      if (cluster_rows && cluster_cols) {
        dendro_setting <- "both"
      } else if (cluster_rows) {
        dendro_setting <- "row"
      } else if (cluster_cols) {
        dendro_setting <- "column"
      }
    }

    # Custom distance function for correlation-based distances
    custom_dist <- function(x) {
      if (dist_method == "pearson") {
        cor_mat <- cor(t(x), method = "pearson", use = "pairwise.complete.obs")
        cor_mat[is.na(cor_mat)] <- 0
        as.dist(1 - cor_mat)
      } else if (dist_method == "spearman") {
        cor_mat <- cor(t(x), method = "spearman", use = "pairwise.complete.obs")
        cor_mat[is.na(cor_mat)] <- 0
        as.dist(1 - cor_mat)
      } else {
        dist(x, method = dist_method)
      }
    }

    # Custom clustering function
    custom_hclust <- function(d) {
      hclust(d, method = clust_method)
    }

    # Build dendrograms manually if clustering is enabled
    if (cluster_rows && nrow(vst_subset) > 1) {
      row_dend <- custom_hclust(custom_dist(vst_subset))
    } else {
      row_dend <- FALSE
    }

    if (cluster_cols && ncol(vst_subset) > 1) {
      col_dend <- custom_hclust(custom_dist(t(vst_subset)))
    } else {
      col_dend <- FALSE
    }

    # Create heatmap with heatmaply using pre-computed dendrograms
    # subplot_heights depends on whether column dendrogram is shown
    # With col dendrogram: 3 subplots (dendrogram, annotation, heatmap)
    # Without col dendrogram: 2 subplots (annotation, heatmap)
    has_col_dendro <- show_dendro && (dendro_setting %in% c("column", "both"))

    if (has_col_dendro) {
      # 3 subplots: dendrogram (8%), annotation (5%), heatmap (87%)
      subplot_heights_val <- c(0.08, 0.05, 0.87)
    } else {
      # 2 subplots: annotation (5%), heatmap (95%)
      subplot_heights_val <- c(0.05, 0.95)
    }

    p <- heatmaply::heatmaply(
      vst_subset,
      Rowv = if (isFALSE(row_dend)) FALSE else row_dend,
      Colv = if (isFALSE(col_dend)) FALSE else col_dend,
      dendrogram = dendro_setting,
      colors = color_pal,
      showticklabels = c(show_colnames, show_rownames),
      col_side_colors = col_side_df,
      col_side_palette = function(n) annotation_cols[1:n],
      subplot_heights = subplot_heights_val,
      subplot_margin = 0.01, # Gap between annotation bar and heatmap
      margins = c(120, 150, 50, 20),
      xlab = "Samples",
      ylab = "Genes",
      main = main_title,
      scale_fill_gradient_fun = ggplot2::scale_fill_gradientn(
        colors = color_pal,
        name = if (scale_rows) "Z-score" else "VST"
      ),
      plot_method = "plotly",
      width = 840,
      height = 1200
    )

    p
  })

  # Download top genes as CSV
  output$download_top_genes <- downloadHandler(
    filename = function() {
      pc_name <- paste0("PC", input$topgenes_pc)
      paste0("top_loading_genes_", pc_name, "_", Sys.Date(), ".csv")
    },
    content = function(file) {
      req(top_loading_genes())

      df <- top_loading_genes()
      pc_name <- attr(df, "pc_name")
      var_exp <- attr(df, "var_explained")

      # Check if coordinates are available
      has_coords <- "coordinates" %in%
        colnames(df) &&
        any(!is.na(df$coordinates))

      # Add metadata columns - include gene names, descriptions, and coordinates if available
      if (has_coords) {
        export_df <- data.frame(
          Rank = df$Rank,
          Gene_ID = df$Gene,
          Gene_Name = df$Gene_Name,
          Coordinates = df$coordinates,
          Description = df$Description,
          Loading = df$Loading,
          Abs_Loading = df$Abs_Loading,
          PC = pc_name,
          Variance_Explained_Pct = var_exp
        )
      } else {
        export_df <- data.frame(
          Rank = df$Rank,
          Gene_ID = df$Gene,
          Gene_Name = df$Gene_Name,
          Description = df$Description,
          Loading = df$Loading,
          Abs_Loading = df$Abs_Loading,
          PC = pc_name,
          Variance_Explained_Pct = var_exp
        )
      }

      write.csv(export_df, file, row.names = FALSE)

      showNotification(
        paste("Exported", nrow(export_df), "top loading genes for", pc_name),
        type = "message",
        duration = 3
      )
    }
  )
}

shinyApp(ui, server)
