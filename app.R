# RNA-Seq Editor — Fully Integrated with Reusable Modules
# With Session Storage and Email Notifications

# Environment setup with package installation
required_packages <- c(
  "shiny",
  "bslib",
  "jsonlite",
  "reactable",
  "dplyr",
  "shinycssloaders",
  "blastula",
  "glue",
  "shinyjs"
)

# Install missing packages
for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
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

      # Use storage module to save (debounced)
      current_edits <- list(
        metadata = current_data(),
        counts = counts_rv$counts
      )
      storage$update_edits(current_edits)

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

      # Save changes (debounced)
      current_edits <- list(
        metadata = current_data(),
        counts = counts_rv$counts
      )
      storage$update_edits(current_edits)

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

      # Save changes (debounced)
      current_edits <- list(
        metadata = current_data(),
        counts = counts_rv$counts
      )
      storage$update_edits(current_edits)

      showNotification("Undo: restored.", type = "message")
    })

    observeEvent(input$reset, {
      old_sample_count <- nrow(current_data())
      current_data(data_rv$orig_meta)
      counts_rv$counts <- data_rv$orig_counts
      deleted_stack(list())

      # Track reset
      email$track_change(
        type = "Dataset Reset",
        details = glue::glue(
          "Reset dataset to original state ({old_sample_count} → {nrow(current_data())} samples)"
        )
      )

      # Save reset state (debounced)
      current_edits <- list(
        metadata = data_rv$orig_meta,
        counts = data_rv$orig_counts
      )
      storage$update_edits(current_edits)

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

linkedTableServer <- function(id, counts_rv, meta_data) {
  moduleServer(id, function(input, output, session) {
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
      tags$link(href = "https://fonts.googleapis.com/css2?family=Stack+Sans+Headline:wght@200..700&display=swap", rel="stylesheet", rel = "stylesheet"),
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
        h4("App Info"),
        tags$ul(
          tags$li("Upload an", tags$code(".RData"), "file containing:"),
          tags$ul(
            # tags$li(
            #   tags$code("metadata"),
            #   "— must have columns:",
            #   tags$strong("label"),
            #   "and",
            #   tags$strong("group")
            # ),
            tags$li(
              tags$code("counts"),
              "— column names must match",
              tags$code("metadata$label")
            )
          ),
          tags$li("Click a row → edit all fields"),
          tags$li("Group dropdown allows adding new groups"),
          tags$li("Delete row → undo available"),
          tags$li("Reset → restores original data"),
          tags$li("Download updated metadata & counts"),
          tags$li(
            "Enable email notifications for batched change summaries (every 2 minutes)"
          ),
          tags$li("Auto-save: Changes saved 3 seconds after last edit"),
          tags$li("Your work is automatically saved with a Session ID"),
          tags$li("Use your Session ID to resume work later")
        )
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
          # Add manual save button
          actionButton(
            "manual_save",
            "Save Now",
            icon = icon("save"),
            class = "btn-outline-success btn-sm",
            style = "margin-top: 10px;"
          ),
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
    )
  )
}

# =================================================
# SERVER
# =================================================
server <- function(input, output, session) {
  # Initialize meta_out
  meta_out <- reactiveValues(data = NULL)

  # =================================================
  # MANUAL SAVE BUTTON WITH PROGRESS
  # =================================================

  observeEvent(input$manual_save, {
    if (!is.null(data_rv$meta)) {
      tryCatch(
        {
          # The storage module now handles the progress bar internally
          storage$force_save(show_progress = TRUE)

          showNotification(
            "Session saved successfully!",
            type = "message",
            duration = 2
          )
          cat("Manual save triggered at:", format(Sys.time(), "%H:%M:%S"), "\n")
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

  # Force save on session end
  session$onSessionEnded(function() {
    cat("Session ended - forcing final save\n")
    # Force immediate save (bypasses debounce)
    if (!is.null(isolate(data_rv$meta))) {
      storage$force_save()
      cat("Final save completed\n")
    }
    stopApp()
  })

  # Also save periodically as backup (every 30 seconds)
  observe({
    invalidateLater(30000, session) # 30 seconds

    if (!is.null(data_rv$meta)) {
      cat("Periodic backup save at:", format(Sys.time(), "%H:%M:%S"), "\n")
      storage$force_save()
    }
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
      req(data_rv$meta)

      current_meta <- data_rv$meta
      if (!is.null(meta_out) && !is.null(meta_out$data)) {
        current_data <- meta_out$data()
        if (!is.null(current_data)) {
          current_meta <- current_data
        }
      }

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
      showNotification(
        "Loading session...",
        type = "message",
        duration = NULL,
        id = "loading_session"
      )

      # Use storage module to load session
      session_data <- storage$load_session(session_id)

      if (!is.null(session_data)) {
        # Restore the edited data
        data_rv$meta <- session_data$current_edits$metadata
        data_rv$orig_meta <- session_data$file_data$metadata
        data_rv$counts <- session_data$current_edits$counts
        data_rv$orig_counts <- session_data$file_data$counts

        removeNotification("loading_session")
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
        removeNotification("loading_session")
        showNotification(
          "Session not found. Please check the Session ID or start a new session.",
          type = "error"
        )
      }
    }
  })

  # Start new session
  observeEvent(input$start_new, {
    # Use storage module to create new session
    new_session_id <- storage$create_session()

    showNotification(
      glue::glue("Starting new session: {new_session_id}"),
      type = "message"
    )

    # Switch to editor tab
    shinyjs::runjs('$("a[data-value=\'editor\']").click();')
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
          mutate(across(where(is.numeric), ~ round(.x, 2)))

        # Store data in reactive values
        data_rv$meta <- meta
        data_rv$orig_meta <- meta
        data_rv$counts <- cnt
        data_rv$orig_counts <- cnt

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
  # RENDER TABLES
  # =================================================

  output$tables_ui <- renderUI({
    req(data_rv$meta, data_rv$counts)

    Sys.sleep(0.5)

    # Pass storage and email modules to the editable table
    meta_module_out <- editableTableServer(
      "meta",
      data_rv,
      counts_rv,
      storage,
      email
    )

    meta_out$data <- meta_module_out$data

    summaryServer("summary", meta_out$data)
    linkedTableServer("counts", counts_rv, meta_out$data)

    tagList(
      editableTableUI("meta"),
      linkedTableUI("counts", "Count Matrix (Live)")
    )
  })
}

shinyApp(ui, server)
