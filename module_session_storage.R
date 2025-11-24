# =================================================
# SESSION STORAGE MODULE
# File: module_session_storage.R
# =================================================
#
# Reusable Shiny module for session management with debounced auto-save
#
# Features:
# - Auto-generates unique session IDs
# - Saves/loads session data to .rds files
# - Debounced auto-save (waits N seconds after last change)
# - Copy session ID to clipboard
# - Visual status indicators
# - Optional database backup support
#
# =================================================

#' Session Storage Module - UI
#'
#' @param id Character string. Module namespace ID
#' @return tagList of UI elements
#'
#' @examples
#' # In your UI:
#' sessionStorageUI("storage")
sessionStorageUI <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("session_status")),
    uiOutput(ns("file_status"))
  )
}

#' Session Storage Module - Server
#'
#' @param id Character string. Module namespace ID
#' @param storage_config List with storage configuration options:
#'   - file_storage_enabled: Enable file-based storage (default: TRUE)
#'   - storage_dir: Directory for session files (default: "saved_sessions")
#'   - db_storage_enabled: Enable database backup (default: FALSE)
#'   - db_path: Path to SQLite database (default: "sessions.db")
#'   - cleanup_days: Days to keep old sessions (default: 7)
#' @param debounce_seconds Number of seconds to wait before auto-saving (default: 3)
#'
#' @return List of functions and reactive values:
#'   - session_id(): Reactive value with current session ID
#'   - file_data(): Reactive value with original file data
#'   - current_edits(): Reactive value with current edits
#'   - create_session(): Function to create new session
#'   - load_session(session_id): Function to load existing session
#'   - save_initial_data(file_data, file_name): Function to save initial upload
#'   - update_edits(current_edits): Function to update edits (debounced save)
#'   - force_save(): Function to force immediate save
#'
#' @examples
#' # In your server:
#' storage <- sessionStorageServer("storage",
#'   storage_config = list(
#'     file_storage_enabled = TRUE,
#'     storage_dir = "saved_sessions",
#'     cleanup_days = 7
#'   ),
#'   debounce_seconds = 3
#' )
#'
#' # Create new session
#' new_id <- storage$create_session()
#'
#' # Save initial data
#' storage$save_initial_data(
#'   file_data = list(metadata = df1, counts = df2),
#'   file_name = "mydata.RData"
#' )
#'
#' # Update edits (debounced auto-save)
#' storage$update_edits(
#'   current_edits = list(metadata = edited_df1, counts = edited_df2)
#' )
#'
#' # Load existing session
#' session_data <- storage$load_session("session_20251119_123456_1234")
sessionStorageServer <- function(
  id,
  storage_config = NULL,
  debounce_seconds = 3
) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Default storage config if not provided
    if (is.null(storage_config)) {
      storage_config <- list(
        file_storage_enabled = TRUE,
        storage_dir = "saved_sessions",
        db_storage_enabled = FALSE,
        db_path = "sessions.db",
        cleanup_days = 7
      )
    }

    # Initialize storage directory
    initialize_storage <- function() {
      if (
        storage_config$file_storage_enabled &&
          !dir.exists(storage_config$storage_dir)
      ) {
        dir.create(storage_config$storage_dir, recursive = TRUE)
        cat("Created storage directory:", storage_config$storage_dir, "\n")
      }
    }

    initialize_storage()

    # Reactive values for session management
    rv <- reactiveValues(
      session_id = NULL,
      file_data = NULL,
      current_edits = NULL,
      file_name = NULL,
      file_timestamp = NULL,
      last_save = NULL,
      auto_save_timer = NULL
    )

    # Generate new session ID
    generate_session_id <- function() {
      paste0(
        "session_",
        format(Sys.time(), "%Y%m%d_%H%M%S"),
        "_",
        sample(1000:9999, 1)
      )
    }

    # Save session to file
    save_to_file <- function(
      session_id,
      file_data,
      current_edits,
      file_name,
      show_progress = FALSE
    ) {
      if (!storage_config$file_storage_enabled) {
        return(FALSE)
      }

      tryCatch(
        {
          # Ensure directory exists
          if (!dir.exists(storage_config$storage_dir)) {
            dir.create(storage_config$storage_dir, recursive = TRUE)
          }

          # Optional progress tracking - ONLY if explicitly TRUE
          progress <- NULL
          if (isTRUE(show_progress)) {
            # Use isTRUE() to be strict
            tryCatch(
              {
                progress <- Progress$new(session, min = 0, max = 100)
                progress$set(message = "Saving session...", value = 0)
              },
              error = function(e) {
                # If progress creation fails, continue without it
                cat("Could not create progress bar:", e$message, "\n")
                progress <- NULL
              }
            )
          }

          # Close progress on exit if it was created
          if (!is.null(progress)) {
            on.exit(progress$close())
          }

          if (!is.null(progress)) {
            progress$set(value = 20, detail = "Preparing session data...")
          }

          session_data <- list(
            session_id = session_id,
            file_data = file_data,
            current_edits = current_edits,
            file_name = file_name,
            timestamp = Sys.time(),
            last_accessed = Sys.time()
          )

          if (!is.null(progress)) {
            progress$set(value = 40, detail = "Serializing data...")
          }

          file_path <- file.path(
            storage_config$storage_dir,
            paste0(session_id, ".rds")
          )

          if (!is.null(progress)) {
            progress$set(value = 60, detail = "Writing to disk...")
          }

          saveRDS(session_data, file = file_path)

          if (!is.null(progress)) {
            progress$set(value = 80, detail = "Verifying save...")
            # Quick verification that file exists
            if (file.exists(file_path)) {
              progress$set(value = 100, detail = "Complete!")
            }
          }

          rv$last_save <- Sys.time()
          cat(
            "Session saved:",
            file_path,
            "at",
            format(Sys.time(), "%H:%M:%S"),
            ifelse(isTRUE(show_progress), "(with progress)", "(silent)"),
            "\n"
          )
          return(TRUE)
        },
        error = function(e) {
          warning("Error saving session:", e$message)
          return(FALSE)
        }
      )
    }

    # Load session from file
    load_from_file <- function(session_id) {
      if (!storage_config$file_storage_enabled) {
        return(NULL)
      }

      tryCatch(
        {
          file_path <- file.path(
            storage_config$storage_dir,
            paste0(session_id, ".rds")
          )

          if (file.exists(file_path)) {
            session_data <- readRDS(file_path)

            # Update last accessed time
            session_data$last_accessed <- Sys.time()
            saveRDS(session_data, file = file_path)

            cat("Session loaded:", file_path, "\n")
            return(session_data)
          }
          return(NULL)
        },
        error = function(e) {
          warning("Error loading session:", e$message)
          return(NULL)
        }
      )
    }

    # Debounced auto-save
    debounced_save <- function() {
      # Cancel existing timer if any
      if (!is.null(rv$auto_save_timer)) {
        time_diff <- as.numeric(Sys.time() - rv$auto_save_timer)
        if (time_diff < debounce_seconds) {
          return() # Still within debounce window
        }
      }

      # Set timer
      rv$auto_save_timer <- Sys.time()

      # Schedule save after debounce period
      invalidateLater(debounce_seconds * 1000, session)

      observeEvent(
        rv$auto_save_timer,
        {
          req(rv$auto_save_timer)

          time_diff <- as.numeric(Sys.time() - rv$auto_save_timer)
          if (time_diff >= debounce_seconds) {
            if (!is.null(rv$session_id) && !is.null(rv$file_data)) {
              save_to_file(
                rv$session_id,
                rv$file_data,
                rv$current_edits,
                rv$file_name,
                show_progress = FALSE # No progress for auto-save
              )
            }
            rv$auto_save_timer <- NULL
          }
        },
        once = TRUE,
        ignoreInit = TRUE
      )
    }

    # Session status UI
    output$session_status <- renderUI({
      req(rv$session_id)

      tagList(
        div(
          class = "session-id-status",
          style = "background-color: #e8f5e8; border: 1px solid #c3e6c3; 
                   border-radius: 5px; padding: 10px; margin: 10px 0; color: #155724;",
          icon("id-card", class = "text-success"),
          strong("Current Session ID:"),
          br(),
          tags$code(rv$session_id),
          br(),
          actionButton(
            ns("copy_session"),
            "Copy Session ID",
            icon = icon("copy"),
            class = "btn-outline-primary btn-sm mt-2"
          )
        )
      )
    })

    # File status UI
    output$file_status <- renderUI({
      req(rv$file_name)

      div(
        class = "file-status",
        style = "background-color: #e8f4fd; border: 1px solid #b6e0fe; 
                 border-radius: 5px; padding: 8px; margin: 5px 0; 
                 color: #0c5460; font-size: 0.85em;",
        icon("check-circle", class = "text-success"),
        "File loaded: ",
        tags$strong(rv$file_name),
        br(),
        tags$small("Uploaded: ", format(rv$file_timestamp, '%H:%M:%S')),
        if (!is.null(rv$last_save)) {
          tagList(
            br(),
            tags$small(
              icon("save"),
              "Last saved: ",
              format(rv$last_save, '%H:%M:%S')
            )
          )
        }
      )
    })

    # Copy session ID to clipboard
    observeEvent(input$copy_session, {
      if (!is.null(rv$session_id)) {
        shinyjs::runjs(sprintf(
          "navigator.clipboard.writeText('%s');",
          rv$session_id
        ))
        showNotification("Session ID copied to clipboard!", type = "message")
      }
    })

    # Return list of functions and reactive values
    list(
      # Reactive values (read-only)
      session_id = reactive(rv$session_id),
      file_data = reactive(rv$file_data),
      current_edits = reactive(rv$current_edits),

      # Functions

      # Create a new session
      create_session = function() {
        rv$session_id <- generate_session_id()
        cat("New session created:", rv$session_id, "\n")
        return(rv$session_id)
      },

      # Load an existing session
      load_session = function(session_id) {
        session_data <- load_from_file(session_id)
        if (!is.null(session_data)) {
          rv$session_id <- session_data$session_id
          rv$file_data <- session_data$file_data
          rv$current_edits <- session_data$current_edits
          rv$file_name <- session_data$file_name
          rv$file_timestamp <- session_data$timestamp
          cat("Session loaded successfully:", session_id, "\n")
          return(session_data)
        }
        cat("Session not found:", session_id, "\n")
        return(NULL)
      },

      # Save initial data (on file upload)
      save_initial_data = function(file_data, file_name) {
        if (is.null(rv$session_id)) {
          rv$session_id <- generate_session_id()
        }

        rv$file_data <- file_data
        rv$file_name <- file_name
        rv$file_timestamp <- Sys.time()
        rv$current_edits <- file_data

        save_to_file(
          rv$session_id,
          file_data,
          file_data,
          file_name,
          show_progress = FALSE
        )
        cat("Initial data saved for session:", rv$session_id, "\n")
        return(rv$session_id)
      },

      # Update edits with debounced save
      update_edits = function(current_edits) {
        rv$current_edits <- current_edits
        debounced_save()
      },

      # Force immediate save (bypasses debounce)
      force_save = function(show_progress = FALSE) {
        # Default to FALSE, not TRUE
        # Use isolate to access reactive values
        session_id <- isolate(rv$session_id)
        file_data <- isolate(rv$file_data)
        current_edits <- isolate(rv$current_edits)
        file_name <- isolate(rv$file_name)

        if (!is.null(session_id) && !is.null(file_data)) {
          save_to_file(
            session_id,
            file_data,
            current_edits,
            file_name,
            show_progress = show_progress
          )
          cat(
            "Forced save completed",
            ifelse(
              isTRUE(show_progress),
              "(with progress requested)",
              "(silent mode)"
            ),
            "\n"
          )
        }
      }
    )
  })
}

# =================================================
# OPTIONAL: Cleanup function for old sessions
# =================================================
# Call this periodically (e.g., daily) to remove old sessions

#' Clean up old session files
#'
#' @param storage_dir Directory containing session files
#' @param cleanup_days Number of days to keep sessions (default: 7)
#'
#' @examples
#' # Run daily cleanup
#' observe({
#'   invalidateLater(24 * 60 * 60 * 1000)  # Every 24 hours
#'   cleanup_old_sessions("saved_sessions", cleanup_days = 7)
#' })
cleanup_old_sessions <- function(
  storage_dir = "saved_sessions",
  cleanup_days = 7
) {
  tryCatch(
    {
      if (!dir.exists(storage_dir)) {
        return()
      }

      cutoff_time <- Sys.time() - (cleanup_days * 24 * 60 * 60)
      session_files <- list.files(
        storage_dir,
        pattern = "\\.rds$",
        full.names = TRUE
      )

      deleted_count <- 0
      for (file_path in session_files) {
        session_data <- readRDS(file_path)
        if (session_data$last_accessed < cutoff_time) {
          file.remove(file_path)
          deleted_count <- deleted_count + 1
        }
      }

      if (deleted_count > 0) {
        cat("Cleaned up", deleted_count, "old session files\n")
      }
    },
    error = function(e) {
      warning("Error cleaning up old sessions:", e$message)
    }
  )
}
