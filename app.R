# fix bugs # frozen.R
# RNA-Seq Editor — FROZEN STATE (November 16, 2025)
# DO NOT MODIFY — Use as baseline for all future fixes

#if (!require(renv)) install.packages("renv")
#renv::restore()

# .libPaths(c("/path/to/your/library", .libPaths()))
#.libPaths(c("/srv/shiny-server/test/renv/library/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu"), .libPaths())

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
# GLOBAL REACTIVE VALUES (Add this section)
# =================================================

# Track changes for email summaries
change_tracker <- reactiveValues(
  changes = list(),
  last_sent = NULL,
  pending_changes = list(),
  batch_timer = NULL,
  is_batching = FALSE
)

# Global reactive value for session ID
current_session_id <- reactiveVal("")

# Global storage for uploaded files
uploaded_files <- reactiveValues(
  file_data = NULL,
  file_name = NULL,
  file_timestamp = NULL,
  current_edits = NULL,
  session_id = NULL
)

# =================================================
# SESSION CONFIGURATION
# =================================================

# Set session timeout to 30 minutes (1800 seconds)
options(shiny.autoreload = FALSE)
if (interactive()) {
  # For testing: 30 minute timeout
  options(shiny.launch.browser = TRUE)
}

# =================================================
# STORAGE CONFIGURATION
# =================================================

# Storage configuration
STORAGE_CONFIG <- list(
  # File-based storage (primary)
  file_storage_enabled = TRUE,
  storage_dir = "saved_sessions",
  
  # Optional database backup (set to FALSE if no database)
  db_storage_enabled = FALSE,
  db_path = "sessions.db",
  
  # Session cleanup (days)
  cleanup_days = 7
)

# =================================================
# STORAGE FUNCTIONS
# =================================================

# Initialize storage directory
initialize_storage <- function() {
  if (
    STORAGE_CONFIG$file_storage_enabled &&
    !dir.exists(STORAGE_CONFIG$storage_dir)
  ) {
    dir.create(STORAGE_CONFIG$storage_dir, recursive = TRUE)
    cat("Created storage directory:", STORAGE_CONFIG$storage_dir, "\n")
  }
}

# Save session data to file
save_session_to_file <- function(
    session_id,
    file_data,
    current_edits,
    file_name
) {
  if (!STORAGE_CONFIG$file_storage_enabled) {
    return(FALSE)
  }
  
  tryCatch(
    {
      session_data <- list(
        session_id = session_id,
        file_data = file_data,
        current_edits = current_edits,
        file_name = file_name,
        timestamp = Sys.time(),
        last_accessed = Sys.time()
      )
      
      file_path <- file.path(
        STORAGE_CONFIG$storage_dir,
        paste0(session_id, ".rds")
      )
      saveRDS(session_data, file = file_path)
      
      cat("Session saved to file:", file_path, "\n")
      return(TRUE)
    },
    error = function(e) {
      cat("Error saving session to file:", e$message, "\n")
      return(FALSE)
    }
  )
}

# Load session data from file
load_session_from_file <- function(session_id) {
  if (!STORAGE_CONFIG$file_storage_enabled) {
    return(NULL)
  }
  
  tryCatch(
    {
      file_path <- file.path(
        STORAGE_CONFIG$storage_dir,
        paste0(session_id, ".rds")
      )
      
      if (file.exists(file_path)) {
        session_data <- readRDS(file_path)
        
        # Update last accessed time
        session_data$last_accessed <- Sys.time()
        saveRDS(session_data, file = file_path)
        
        cat("Session loaded from file:", file_path, "\n")
        return(session_data)
      }
      return(NULL)
    },
    error = function(e) {
      cat("Error loading session from file:", e$message, "\n")
      return(NULL)
    }
  )
}

# Optional: Database functions (if enabled)
initialize_database <- function() {
  if (!STORAGE_CONFIG$db_storage_enabled) {
    return(NULL)
  }
  
  tryCatch(
    {
      if (!requireNamespace("RSQLite", quietly = TRUE)) {
        cat("RSQLite not available. Database storage disabled.\n")
        STORAGE_CONFIG$db_storage_enabled <<- FALSE
        return(NULL)
      }
      
      con <- DBI::dbConnect(RSQLite::SQLite(), STORAGE_CONFIG$db_path)
      
      # Create sessions table if it doesn't exist
      DBI::dbExecute(
        con,
        "
      CREATE TABLE IF NOT EXISTS sessions (
        session_id TEXT PRIMARY KEY,
        file_data BLOB,
        current_edits BLOB,
        file_name TEXT,
        timestamp TEXT,
        last_accessed TEXT
      )
    "
      )
      
      DBI::dbDisconnect(con)
      cat("Database initialized:", STORAGE_CONFIG$db_path, "\n")
    },
    error = function(e) {
      cat("Error initializing database:", e$message, "\n")
      STORAGE_CONFIG$db_storage_enabled <<- FALSE
    }
  )
}

# Optional: Save to database
save_session_to_db <- function(
    session_id,
    file_data,
    current_edits,
    file_name
) {
  if (!STORAGE_CONFIG$db_storage_enabled) {
    return(FALSE)
  }
  
  tryCatch(
    {
      con <- DBI::dbConnect(RSQLite::SQLite(), STORAGE_CONFIG$db_path)
      
      session_data <- data.frame(
        session_id = session_id,
        file_data = I(list(serialize(file_data, NULL))),
        current_edits = I(list(serialize(current_edits, NULL))),
        file_name = file_name,
        timestamp = as.character(Sys.time()),
        last_accessed = as.character(Sys.time())
      )
      
      # Remove existing session if it exists
      DBI::dbExecute(
        con,
        "DELETE FROM sessions WHERE session_id = ?",
        params = list(session_id)
      )
      
      # Insert new session data
      DBI::dbWriteTable(con, "sessions", session_data, append = TRUE)
      
      DBI::dbDisconnect(con)
      cat("Session saved to database:", session_id, "\n")
      return(TRUE)
    },
    error = function(e) {
      cat("Error saving session to database:", e$message, "\n")
      return(FALSE)
    }
  )
}

# Optional: Load from database
load_session_from_db <- function(session_id) {
  if (!STORAGE_CONFIG$db_storage_enabled) {
    return(NULL)
  }
  
  tryCatch(
    {
      con <- DBI::dbConnect(RSQLite::SQLite(), STORAGE_CONFIG$db_path)
      
      result <- DBI::dbGetQuery(
        con,
        "SELECT * FROM sessions WHERE session_id = ?",
        params = list(session_id)
      )
      
      DBI::dbDisconnect(con)
      
      if (nrow(result) > 0) {
        session_data <- list(
          session_id = result$session_id,
          file_data = unserialize(result$file_data[[1]]),
          current_edits = unserialize(result$current_edits[[1]]),
          file_name = result$file_name,
          timestamp = as.POSIXct(result$timestamp),
          last_accessed = as.POSIXct(result$last_accessed)
        )
        
        # Update last accessed time
        save_session_to_db(
          session_id,
          session_data$file_data,
          session_data$current_edits,
          session_data$file_name
        )
        
        cat("Session loaded from database:", session_id, "\n")
        return(session_data)
      }
      return(NULL)
    },
    error = function(e) {
      cat("Error loading session from database:", e$message, "\n")
      return(NULL)
    }
  )
}

# Clean up old sessions
cleanup_old_sessions <- function() {
  if (!STORAGE_CONFIG$file_storage_enabled) {
    return()
  }
  
  tryCatch(
    {
      cutoff_time <- Sys.time() - (STORAGE_CONFIG$cleanup_days * 24 * 60 * 60)
      session_files <- list.files(
        STORAGE_CONFIG$storage_dir,
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
      cat("Error cleaning up old sessions:", e$message, "\n")
    }
  )
}

# Initialize storage systems on app start
initialize_storage()
initialize_database()

# =================================================
# EMAIL CONFIGURATION (Using credentials file)
# =================================================

# Email configuration
EMAIL_CONFIG <- list(
  enabled = TRUE, # Set to FALSE to disable entirely
  sender_email = "your.app.notifications@gmail.com", # Your dedicated app email
  creds_file = "gmail_creds", # Credentials file created once
  batch_delay_seconds = 120, # 2 minutes delay before sending batched changes
  min_changes_for_email = 1 # Minimum number of changes to trigger email
)

# Function to track changes and start batching
track_change <- function(
    type,
    details,
    sample_label = NULL,
    old_value = NULL,
    new_value = NULL
) {
  change_id <- paste0("change_", as.integer(Sys.time()))
  
  change <- list(
    id = change_id,
    timestamp = Sys.time(),
    type = type,
    details = details,
    sample_label = sample_label,
    old_value = old_value,
    new_value = new_value
  )
  
  # Add to pending changes for batching
  change_tracker$pending_changes <- c(
    change_tracker$pending_changes,
    list(change)
  )
  
  # Add to full change history (for reference)
  change_tracker$changes <- c(change_tracker$changes, list(change))
  
  # Limit full history to last 50 changes to prevent memory issues
  if (length(change_tracker$changes) > 50) {
    change_tracker$changes <- change_tracker$changes[-1]
  }
  
  # Start batching timer if not already running
  if (!change_tracker$is_batching) {
    change_tracker$is_batching <- TRUE
    change_tracker$batch_timer <- as.numeric(Sys.time())
    
    # Show notification that batching has started
    showNotification(
      "Changes are being batched... Summary will be sent in 2 minutes.",
      type = "message",
      duration = 3
    )
  }
}

# Function to send batched changes
send_batched_changes <- function(user_email, data_summary) {
  if (!EMAIL_CONFIG$enabled) {
    return(FALSE)
  }
  
  # Check if credentials file exists
  if (!file.exists(EMAIL_CONFIG$creds_file)) {
    warning("Email credentials file not found: ", EMAIL_CONFIG$creds_file)
    return(FALSE)
  }
  
  # Check if we have enough changes to send
  if (
    length(change_tracker$pending_changes) < EMAIL_CONFIG$min_changes_for_email
  ) {
    return(FALSE)
  }
  
  tryCatch(
    {
      # Get pending changes
      pending_changes <- change_tracker$pending_changes
      
      # Group changes by type for summary
      change_counts <- table(sapply(pending_changes, function(x) x$type))
      change_summary <- paste(
        names(change_counts),
        "(",
        change_counts,
        ")",
        collapse = ", "
      )
      
      # Safe timestamp extraction and formatting
      if (length(pending_changes) > 0) {
        # Extract timestamps safely
        timestamps <- sapply(pending_changes, function(x) {
          if (inherits(x$timestamp, "POSIXt")) {
            as.numeric(x$timestamp)
          } else {
            as.numeric(Sys.time()) # fallback to current time
          }
        })
        
        min_time <- format(
          as.POSIXct(min(timestamps), origin = "1970-01-01"),
          '%H:%M:%S'
        )
        max_time <- format(
          as.POSIXct(max(timestamps), origin = "1970-01-01"),
          '%H:%M:%S'
        )
        time_range <- paste(min_time, "to", max_time)
      } else {
        time_range <- "No time data"
      }
      
      # Format detailed changes safely
      detailed_changes <- paste(
        sapply(pending_changes, function(change) {
          # Safely format timestamp
          timestamp_str <- if (inherits(change$timestamp, "POSIXt")) {
            format(change$timestamp, '%H:%M:%S')
          } else {
            "Unknown time"
          }
          
          details <- paste0(
            "**",
            change$type,
            "** (",
            timestamp_str,
            "): ",
            change$details
          )
          if (!is.null(change$sample_label)) {
            details <- paste(
              details,
              paste0("- Sample: ", change$sample_label),
              sep = "\n"
            )
          }
          if (!is.null(change$old_value) && !is.null(change$new_value)) {
            details <- paste(
              details,
              paste0(
                "- Changed from '",
                change$old_value,
                "' to '",
                change$new_value,
                "'"
              ),
              sep = "\n"
            )
          }
          details
        }),
        collapse = "\n\n"
      )
      
      email_body <- blastula::md(glue::glue(
        "## QC-CheckeR Batched Changes Summary
      
      Your RNA-Seq metadata has been updated. Here's a summary of changes made in the last 2 minutes:
      
      **Change Summary:**
      - **Total Changes:** {length(pending_changes)}
      - **Change Types:** {change_summary}
      - **Time Period:** {time_range}
      
      **Detailed Changes:**
      {detailed_changes}
      
      **Current Dataset Status:**
      - **Total Samples:** {data_summary$total_samples}
      - **Experimental Groups:** {data_summary$groups}
      - **Summary Generated:** {format(Sys.time(), '%Y-%m-%d %H:%M:%S')}
      
      **Files Available for Download:**
      - Updated metadata (CSV format)
      - Synchronized count matrix (CSV format)
      
      You can continue editing your data in the app. Changes will be batched and summarized every 2 minutes.
      
      Thank you for using QC-CheckeR!
      
      ---
      *This is an automated batched notification. Please do not reply to this email.*
      "
      ))
      
      email <- blastula::compose_email(body = email_body)
      
      # Send email using credentials file
      email %>%
        blastula::smtp_send(
          to = user_email,
          from = EMAIL_CONFIG$sender_email,
          subject = glue::glue(
            "QC-CheckeR: {length(pending_changes)} Changes in 2 Minutes"
          ),
          credentials = blastula::creds_file(file = EMAIL_CONFIG$creds_file)
        )
      
      # Clear pending changes after successful send
      change_tracker$pending_changes <- list()
      change_tracker$is_batching <- FALSE
      change_tracker$last_sent <- Sys.time()
      
      return(TRUE)
    },
    error = function(e) {
      warning("Failed to send batched changes: ", e$message)
      return(FALSE)
    }
  )
}

# Observer to check and send batched changes
observe_batched_changes <- function(user_prefs, data_summary_func) {
  observe({
    # Only run if notifications are enabled and we have pending changes
    if (
      user_prefs$notifications_enabled &&
      change_tracker$is_batching &&
      length(change_tracker$pending_changes) >=
      EMAIL_CONFIG$min_changes_for_email
    ) {
      # Check if batch delay has passed
      if (!is.null(change_tracker$batch_timer)) {
        time_since_batch_start <- as.numeric(
          Sys.time() - change_tracker$batch_timer
        )
        
        if (time_since_batch_start >= EMAIL_CONFIG$batch_delay_seconds) {
          # Time to send the batched changes
          data_summary <- data_summary_func()
          
          showNotification(
            "Sending batched change summary...",
            type = "message",
            id = "sending_batch",
            duration = NULL
          )
          
          success <- send_batched_changes(
            user_email = user_prefs$user_email,
            data_summary = data_summary
          )
          
          removeNotification("sending_batch")
          
          if (success) {
            showNotification("Batched change summary sent!", type = "message")
          } else {
            showNotification("Failed to send batch summary", type = "warning")
          }
        }
      }
    }
    
    # Check again in 10 seconds
    invalidateLater(10000)
  })
}

# =================================================
# MISSING EMAIL MODULE FUNCTIONS (Add this section)
# =================================================

# Simple email modal - user only provides their email
simpleEmailModalUI <- function(id) {
  ns <- NS(id)
  tagList(
    actionButton(
      ns("setup_notifications"),
      "Enable Email Notifications",
      icon = icon("envelope"),
      class = "btn-outline-info"
    )
  )
}

simpleEmailModalServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    user_prefs <- reactiveValues(
      notifications_enabled = FALSE,
      user_email = NULL
    )
    
    observeEvent(input$setup_notifications, {
      showModal(modalDialog(
        title = "Email Notifications",
        fluidRow(
          column(
            12,
            p("Get notified when you make changes to your metadata."),
            p("Changes are batched and sent every 2 minutes."),
            textInput(
              ns("user_email"),
              "Your Email Address:",
              placeholder = "you@example.com"
            ),
            hr(),
            actionButton(
              ns("test_notification"),
              "Send Test Notification",
              class = "btn-outline-primary"
            ),
            actionButton(
              ns("enable_notifications"),
              "Enable Notifications",
              class = "btn-primary"
            )
          )
        ),
        footer = modalButton("Close"),
        size = "m"
      ))
    })
    
    # Enhanced email sending function
    send_test_email <- function(email) {
      if (!EMAIL_CONFIG$enabled) {
        showNotification(
          "Email notifications are currently disabled",
          type = "warning"
        )
        return()
      }
      
      if (!file.exists(EMAIL_CONFIG$creds_file)) {
        showNotification(
          "Email credentials not configured. Please run setup_email_credentials() first.",
          type = "error",
          duration = 10
        )
        return()
      }
      
      if (!grepl(".+@.+\\..+", email)) {
        showNotification("Please enter a valid email address", type = "error")
        return()
      }
      
      showNotification(
        "Sending test notification...",
        type = "message",
        id = "test_email"
      )
      
      tryCatch(
        {
          email_body <- blastula::md(glue::glue(
            "## QC-CheckeR Test Notification
          
          This is a test notification from QC-CheckeR.
          
          If you received this, you'll get batched change summaries every 2 minutes!
          
          **You'll receive notifications for:**
          - Sample edits (label, group changes)
          - Row deletions and undo operations
          - Dataset resets
          - File uploads
          
          **Batching System:**
          - Changes are collected for 2 minutes
          - Summary emails are sent automatically
          - Multiple changes are grouped together
          
          **Session ID:** {current_session_id()}
          
          Timestamp: {format(Sys.time(), '%Y-%m-%d %H:%M:%S')}
          "
          ))
          subject <- "QC-CheckeR: Test Notification"
          
          test_email <- blastula::compose_email(body = email_body)
          
          test_email %>%
            blastula::smtp_send(
              from = EMAIL_CONFIG$sender_email,
              to = email,
              subject = subject,
              credentials = blastula::creds_file(file = EMAIL_CONFIG$creds_file)
            )
          
          removeNotification("test_email")
          showNotification(
            "Test notification sent! Check your email.",
            type = "message"
          )
        },
        error = function(e) {
          removeNotification("test_email")
          showNotification(
            paste("Failed to send test email:", e$message),
            type = "error",
            duration = 8
          )
        }
      )
    }
    
    observeEvent(input$test_notification, {
      req(input$user_email)
      send_test_email(input$user_email)
    })
    
    observeEvent(input$enable_notifications, {
      req(input$user_email)
      
      if (!grepl(".+@.+\\..+", input$user_email)) {
        showNotification("Please enter a valid email address", type = "error")
        return()
      }
      
      user_prefs$notifications_enabled <- TRUE
      user_prefs$user_email <- input$user_email
      
      # Start batching immediately if there are pending changes
      if (
        length(change_tracker$pending_changes) > 0 &&
        !change_tracker$is_batching
      ) {
        change_tracker$is_batching <- TRUE
        change_tracker$batch_timer <- as.numeric(Sys.time())
        showNotification(
          "Existing changes detected! Batch summary will be sent in 2 minutes.",
          type = "message",
          duration = 3
        )
      }
      
      removeModal()
      showNotification(
        "Email notifications enabled! Changes will be batched and sent every 2 minutes.",
        type = "message"
      )
    })
    
    return(user_prefs)
  })
}

# =================================================
# ONE-TIME SETUP FUNCTION
# =================================================

setup_email_credentials <- function() {
  # Run this function once to set up your email credentials
  # This creates a gmail_creds file that the app will use
  
  cat("Setting up email credentials for QC-CheckeR...\n")
  cat("Please make sure you have:\n")
  cat("1. A dedicated Gmail account for the app\n")
  cat("2. 2-factor authentication enabled\n")
  cat("3. An App Password generated (16 characters)\n\n")
  
  # Get email from user
  email <- readline(prompt = "Enter your Gmail address: ")
  
  # Create credentials file
  blastula::create_smtp_creds_file(
    file = "gmail_creds",
    user = email,
    host = "smtp.gmail.com",
    port = 465,
    use_ssl = TRUE
  )
  
  # Update the sender email in the config
  EMAIL_CONFIG$sender_email <- email
  
  cat("\n✅ Email credentials file created successfully!\n")
  cat("📁 File created: gmail_creds\n")
  cat("📧 Sender email:", email, "\n")
  cat("🔐 You can now use email notifications in the app.\n\n")
  
  # Test the configuration
  cat("Testing email configuration...\n")
  tryCatch(
    {
      test_email <- blastula::compose_email(
        body = blastula::md(
          "## QC-CheckeR Test\n\nThis is a test email to verify your configuration."
        )
      )
      
      test_email %>%
        blastula::smtp_send(
          from = email,
          to = email, # Send test to yourself
          subject = "QC-CheckeR: Configuration Test",
          credentials = blastula::creds_file(file = "gmail_creds")
        )
      
      cat("✅ Test email sent successfully! Check your inbox.\n")
    },
    error = function(e) {
      cat("❌ Test failed:", e$message, "\n")
      cat("Please check your App Password and try again.\n")
    }
  )
}

# Uncomment and run the line below once to set up email:
# setup_email_credentials()

# =================================================
# UI MODULES
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
# EDITABLE SERVER
# =================================================
editableTableServer <- function(id, data_rv, counts_rv, user_prefs) {
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
      changes_made <- c()
      for (col in colnames(updated)) {
        input_id <- paste0("edit_", col)
        if (col == "group") {
          old_val <- as.character(old_row[[col]])
          new_val <- group_value
          if (old_val != new_val) {
            track_change(
              type = "Group Change",
              details = glue::glue("Changed group for sample '{old_label}'"),
              sample_label = old_label,
              old_value = old_val,
              new_value = new_val
            )
            changes_made <- c(
              changes_made,
              glue::glue("group: {old_val} → {new_val}")
            )
          }
          updated[sel, col] <- group_value
        } else if (input_id %in% names(input)) {
          old_val <- as.character(old_row[[col]])
          new_val <- input[[input_id]]
          if (old_val != new_val) {
            track_change(
              type = "Field Update",
              details = glue::glue("Updated {col} for sample '{old_label}'"),
              sample_label = old_label,
              old_value = old_val,
              new_value = new_val
            )
            changes_made <- c(
              changes_made,
              glue::glue("{col}: {old_val} → {new_val}")
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
          track_change(
            type = "Label Change",
            details = glue::glue(
              "Renamed sample from '{old_label}' to '{new_label}'"
            ),
            sample_label = old_label,
            old_value = old_label,
            new_value = new_label
          )
          changes_made <- c(
            changes_made,
            glue::glue("label: {old_label} → {new_label}")
          )
        }
      }
      
      # Update server-side storage with current edits
      if (!is.null(uploaded_files$file_data)) {
        uploaded_files$current_edits <- list(
          metadata = current_data(),
          counts = counts_rv$counts
        )
        
        # Auto-save to persistent storage
        if (!is.null(uploaded_files$session_id)) {
          save_session_to_file(
            uploaded_files$session_id,
            uploaded_files$file_data,
            uploaded_files$current_edits,
            uploaded_files$file_name
          )
          
          if (STORAGE_CONFIG$db_storage_enabled) {
            save_session_to_db(
              uploaded_files$session_id,
              uploaded_files$file_data,
              uploaded_files$current_edits,
              uploaded_files$file_name
            )
          }
        }
      }
      
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
      track_change(
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
      
      # Update server-side storage with current edits
      if (!is.null(uploaded_files$file_data)) {
        uploaded_files$current_edits <- list(
          metadata = current_data(),
          counts = counts_rv$counts
        )
        
        # Auto-save to persistent storage
        if (!is.null(uploaded_files$session_id)) {
          save_session_to_file(
            uploaded_files$session_id,
            uploaded_files$file_data,
            uploaded_files$current_edits,
            uploaded_files$file_name
          )
        }
      }
      
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
      track_change(
        type = "Undo Operation",
        details = glue::glue("Restored deleted sample '{last$label}'"),
        sample_label = last$label
      )
      
      if (last$label %in% colnames(counts_rv$orig_counts)) {
        counts_rv$counts[[last$label]] <- counts_rv$orig_counts[[last$label]]
      }
      
      # Update server-side storage with current edits
      if (!is.null(uploaded_files$file_data)) {
        uploaded_files$current_edits <- list(
          metadata = current_data(),
          counts = counts_rv$counts
        )
        
        # Auto-save to persistent storage
        if (!is.null(uploaded_files$session_id)) {
          save_session_to_file(
            uploaded_files$session_id,
            uploaded_files$file_data,
            uploaded_files$current_edits,
            uploaded_files$file_name
          )
        }
      }
      
      showNotification("Undo: restored.", type = "message")
    })
    
    observeEvent(input$reset, {
      old_sample_count <- nrow(current_data())
      current_data(data_rv$orig_meta)
      counts_rv$counts <- data_rv$orig_counts
      deleted_stack(list())
      
      # Track reset
      track_change(
        type = "Dataset Reset",
        details = glue::glue(
          "Reset dataset to original state ({old_sample_count} → {nrow(current_data())} samples)"
        )
      )
      
      # Update server-side storage with reset state
      if (!is.null(uploaded_files$file_data)) {
        uploaded_files$current_edits <- list(
          metadata = data_rv$orig_meta,
          counts = data_rv$orig_counts
        )
        
        # Auto-save to persistent storage
        if (!is.null(uploaded_files$session_id)) {
          save_session_to_file(
            uploaded_files$session_id,
            uploaded_files$file_data,
            uploaded_files$current_edits,
            uploaded_files$file_name
          )
        }
      }
      
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
    title = div(
      "QC - CheckeR",
      tags$span(
        id = "global-loading-indicator",
        style = "margin-left: 15px; display: none;",
        tags$div(
          style = "display: inline-block;",
          withSpinner(
            ui_element = div(style = "height: 20px; width: 20px;"),
            type = 4,
            color = "#0dcaf0",
            size = 0.5
          )
        )
      )
    ),
    id = "main_tabs",
    header = tags$head(
      shinyjs::useShinyjs(),
      tags$script(HTML(
        "
        // Auto-stop when browser tab is closed
        window.addEventListener('beforeunload', function() {
          // Send a signal to Shiny that the session is ending
          if (window.Shiny) {
            Shiny.setInputValue('browser_closed', true, {priority: 'event'});
          }
        });
        
        // Session timeout warning
        let idleTime = 0;
        const idleInterval = setInterval(timerIncrement, 60000); // 1 minute
        
        function timerIncrement() {
          idleTime++;
          if (idleTime > 25) { // Warn at 25 minutes
            if (window.Shiny) {
              Shiny.setInputValue('idle_warning', true, {priority: 'event'});
            }
          }
          if (idleTime > 30) { // Timeout at 30 minutes
            if (window.Shiny) {
              Shiny.setInputValue('session_timeout', true, {priority: 'event'});
            }
          }
        }
        
        // Reset idle timer on user activity
        function resetIdleTime() {
          idleTime = 0;
        }
        
        document.addEventListener('mousemove', resetIdleTime);
        document.addEventListener('keypress', resetIdleTime);
        document.addEventListener('click', resetIdleTime);
        document.addEventListener('scroll', resetIdleTime);
      "
      )),
      tags$style(HTML(
        "
        .shiny-spinner-output-container {
          position: relative;
        }
        .loading-message {
          text-align: center;
          color: #6c757d;
          font-style: italic;
          margin-top: 10px;
        }
        #global-loading-indicator {
          display: inline-block;
          vertical-align: middle;
        }
        .card {
          transition: opacity 0.3s ease;
        }
        .card.loading {
          opacity: 0.7;
        }
        .notification-status {
          font-size: 0.85em;
          color: #28a745;
          margin-top: 5px;
        }
        .session-warning {
          background-color: #fff3cd;
          border: 1px solid #ffeaa7;
          border-radius: 5px;
          padding: 15px;
          margin: 10px 0;
          color: #856404;
        }
        .batch-status {
          background-color: #e8f4fd;
          border: 1px solid #b6e0fe;
          border-radius: 5px;
          padding: 10px;
          margin: 10px 0;
          color: #0c5460;
        }
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
        .session-info {
          background-color: #f8f9fa;
          border: 1px solid #dee2e6;
          border-radius: 5px;
          padding: 8px;
          margin: 5px 0;
          color: #6c757d;
          font-size: 0.8em;
        }
        .session-id-input {
          margin-bottom: 15px;
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
            div(
              class = "session-id-input",
              textInput(
                "session_id_input",
                "Enter Session ID to resume previous work:",
                placeholder = "Paste your session ID here...",
                width = "100%"
              ),
              actionButton(
                "load_session",
                "Load Session",
                icon = icon("folder-open"),
                class = "btn-outline-primary"
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
                class = "btn-success"
              )
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
          tags$li("Your data is automatically saved with a unique Session ID"),
          tags$li("Use the Session ID to return to your work anytime"),
          tags$li("All changes can be undone with the undo functionality")
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
            tags$li(
              tags$code("metadata"),
              "— must have columns:",
              tags$strong("label"),
              "and",
              tags$strong("group")
            ),
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
          tags$li("Enable email notifications for batched change summaries"),
          tags$li("Your work is automatically saved with a Session ID"),
          tags$li("Use your Session ID to resume work later"),
          tags$li("All edits and changes are preserved between sessions")
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
          # Session ID display
          uiOutput("current_session_ui"),
          hr(),
          h4("Upload .RData"),
          fileInput(
            "file",
            "Choose .RData (metadata + counts)",
            accept = ".RData"
          ),
          # File status indicator
          uiOutput("file_status_ui"),
          # Session info
          uiOutput("session_info_ui"),
          # tags$p(
          #   "Note metadata: must have",
          #   tags$code("label"),
          #   "and",
          #   tags$code("group"),
          #   "columns"
          # ),
          hr(),
          conditionalPanel(
            condition = "output.file_uploaded",
            h5("Summary"),
            summaryUI("summary"),
            hr(),
            h5("Notifications"),
            simpleEmailModalUI("email_config"),
            uiOutput("notification_status_ui"),
            uiOutput("batch_status_ui")
          )
        ),
        mainPanel(
          withSpinner(
            uiOutput("tables_ui"),
            type = 4,
            color = "#0dcaf0",
            size = 1
          ),
          uiOutput("session_warning_ui")
        )
      )
    )
  )
}

# =================================================
# SERVER
# =================================================
server <- function(input, output, session) {
  # Initialize meta_out to avoid reference errors
  meta_out <- reactiveValues(data = NULL)
  
  # Initialize processing_start
  processing_start <- reactiveVal(Sys.time())
  
  session$onSessionEnded(function() {
    cat("Session ended - stopping R session\n")
    stopApp()
  })
  
  output$file_uploaded <- reactive({
    !is.null(uploaded_files$file_data) || !is.null(input$file)
  })
  outputOptions(output, "file_uploaded", suspendWhenHidden = FALSE)
  
  data_rv <- reactiveValues(
    meta = NULL,
    orig_meta = NULL,
    counts = NULL,
    orig_counts = NULL
  )
  
  # Initialize session ID
  observe({
    if (is.null(uploaded_files$session_id)) {
      # Generate a new session ID if none exists
      new_session_id <- paste0(
        "session_",
        format(Sys.time(), "%Y%m%d_%H%M%S"),
        "_",
        sample(1000:9999, 1)
      )
      uploaded_files$session_id <- new_session_id
      current_session_id(new_session_id)
      cat("Generated new session ID:", new_session_id, "\n")
    }
  })
  
  # Current session UI
  output$current_session_ui <- renderUI({
    if (nzchar(current_session_id())) {
      tagList(
        div(
          class = "session-id-status",
          icon("id-card", class = "text-success"),
          strong("Current Session ID:"),
          br(),
          tags$code(current_session_id()),
          br(),
          actionButton(
            "copy_session_id",
            "Copy Session ID",
            icon = icon("copy"),
            class = "btn-outline-primary btn-sm mt-2"
          )
        )
      )
    }
  })
  
  # Copy session ID to clipboard
  observeEvent(input$copy_session_id, {
    session_id <- current_session_id()
    if (nzchar(session_id)) {
      # Create a temporary textarea to copy from
      js_code <- paste0(
        "
        var tempTextArea = document.createElement('textarea');
        tempTextArea.value = '",
        session_id,
        "';
        document.body.appendChild(tempTextArea);
        tempTextArea.select();
        document.execCommand('copy');
        document.body.removeChild(tempTextArea);
      "
      )
      shinyjs::runjs(js_code)
      showNotification("Session ID copied to clipboard!", type = "message")
    }
  })
  
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
      
      # Try to load from persistent storage
      session_data <- load_session_from_file(session_id)
      
      if (is.null(session_data) && STORAGE_CONFIG$db_storage_enabled) {
        session_data <- load_session_from_db(session_id)
      }
      
      if (!is.null(session_data)) {
        # Restore from persistent storage
        uploaded_files$session_id <- session_id
        uploaded_files$file_data <- session_data$file_data
        uploaded_files$current_edits <- session_data$current_edits
        uploaded_files$file_name <- session_data$file_name
        uploaded_files$file_timestamp <- session_data$timestamp
        
        # Restore the edited data
        data_rv$meta <- session_data$current_edits$metadata
        data_rv$orig_meta <- session_data$file_data$metadata
        data_rv$counts <- session_data$current_edits$counts
        data_rv$orig_counts <- session_data$file_data$counts
        
        current_session_id(session_id)
        
        removeNotification("loading_session")
        showNotification(
          "Session loaded successfully!",
          type = "message"
        )
        # Add these 3 lines here ↓
        shinyjs::runjs('
        $("a[data-value=\'editor\']").click();
        ')
        
        
        # Track session load
        track_change(
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
    cat("=== START NEW SESSION CLICKED ===\n")
    
    new_session_id <- paste0(
      "session_",
      format(Sys.time(), "%Y%m%d_%H%M%S"),
      "_",
      sample(1000:9999, 1)
    )
    
    uploaded_files$session_id <- new_session_id
    current_session_id(new_session_id)
    
    showNotification("Starting new session...", type = "message")
    
    # Direct JavaScript to click the Upload/Rename tab
    shinyjs::runjs('
    $("a[data-value=\'editor\']").click();
  ')
    
    cat("Tab clicked via JS\n")
  })
  
  
  # File status UI
  output$file_status_ui <- renderUI({
    if (!is.null(uploaded_files$file_name)) {
      has_edits <- !is.null(uploaded_files$current_edits)
      tagList(
        div(
          class = "file-status",
          icon("check-circle", class = "text-success"),
          "File loaded: ",
          tags$strong(uploaded_files$file_name),
          br(),
          tags$small(
            "Uploaded: ",
            format(uploaded_files$file_timestamp, '%H:%M:%S')
          ),
          if (has_edits) {
            tagList(
              br(),
              tags$small(icon("edit"), "Edits saved for session")
            )
          }
        )
      )
    }
  })
  
  # Session info UI
  output$session_info_ui <- renderUI({
    if (!is.null(uploaded_files$session_id)) {
      div(
        class = "session-info",
        icon("save"),
        "Auto-save enabled",
        br(),
        tags$small("Changes are automatically saved to server storage")
      )
    }
  })
  
  # Initialize email preferences
  user_prefs <- simpleEmailModalServer("email_config")
  
  # File upload handling
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
        
        # Fix: Ensure group is character
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
        
        # Store file data server-side
        uploaded_files$file_data <- list(
          metadata = meta,
          counts = cnt
        )
        uploaded_files$file_name <- input$file$name
        uploaded_files$file_timestamp <- Sys.time()
        uploaded_files$current_edits <- list(
          metadata = meta,
          counts = cnt
        )
        
        # Save to persistent storage
        save_session_to_file(
          uploaded_files$session_id,
          uploaded_files$file_data,
          uploaded_files$current_edits,
          uploaded_files$file_name
        )
        
        if (STORAGE_CONFIG$db_storage_enabled) {
          save_session_to_db(
            uploaded_files$session_id,
            uploaded_files$file_data,
            uploaded_files$current_edits,
            uploaded_files$file_name
          )
        }
        
        # Track file upload
        track_change(
          type = "File Upload",
          details = glue::glue("Uploaded new dataset with {nrow(meta)} samples")
        )
        
        removeNotification("processing")
        showNotification(
          "File Uploaded Successfully! Data saved with current session ID.",
          type = "message"
        )
      },
      error = function(e) {
        removeNotification("processing")
        showNotification(paste("Error:", e$message), type = "error")
      }
    )
  })
  
  # Notification status UI
  output$notification_status_ui <- renderUI({
    if (user_prefs$notifications_enabled) {
      div(
        class = "notification-status",
        icon("check-circle", class = "text-success"),
        "Batched change notifications enabled for: ",
        tags$strong(user_prefs$user_email),
        br(),
        tags$small("Changes are batched and sent every 2 minutes")
      )
    }
  })
  
  # Batch status UI
  output$batch_status_ui <- renderUI({
    if (user_prefs$notifications_enabled && change_tracker$is_batching) {
      # Filter out file uploads from the count
      non_upload_changes <- Filter(
        function(change) change$type != "File Upload",
        change_tracker$pending_changes
      )
      pending_count <- length(non_upload_changes)
      
      if (pending_count > 0) {
        time_remaining <- max(
          0,
          EMAIL_CONFIG$batch_delay_seconds -
            as.numeric(Sys.time() - change_tracker$batch_timer)
        )
        
        div(
          class = "batch-status",
          icon("clock", class = "text-info"),
          strong("Batch in Progress:"),
          br(),
          tags$small(glue::glue("{pending_count} change(s) collected")),
          br(),
          tags$small(glue::glue(
            "Next summary in: {ceiling(time_remaining)} seconds"
          ))
        )
      }
    }
  })
  
  # Function to get current data summary
  get_current_data_summary <- reactive({
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
      
      list(
        total_samples = nrow(current_meta),
        groups = groups
      )
    } else {
      list(
        total_samples = 0,
        groups = "No data"
      )
    }
  })
  
  # Set up batched changes observer
  observe_batched_changes(user_prefs, get_current_data_summary)
  
  # Session warning UI
  output$session_warning_ui <- renderUI({
    if (isTRUE(input$idle_warning)) {
      div(
        class = "session-warning",
        icon("exclamation-triangle"),
        strong("Session Timeout Warning:"),
        " Your session will expire in 5 minutes due to inactivity.",
        br(),
        "Click anywhere to keep your session active."
      )
    }
  })
  
  # Handle browser tab close
  observeEvent(input$browser_closed, {
    if (isTRUE(input$browser_closed)) {
      cat("Browser tab closed - session ending\n")
    }
  })
  
  # Handle session timeout
  observeEvent(input$session_timeout, {
    if (isTRUE(input$session_timeout)) {
      showModal(modalDialog(
        title = "Session Expired",
        "Your session has expired due to 30 minutes of inactivity.",
        "The page will now refresh.",
        footer = NULL,
        easyClose = FALSE
      ))
      
      invalidateLater(3000, session)
      session$reload()
    }
  })
  
  # Global loading state observer
  observe({
    if (!is.null(input$file) && is.null(data_rv$meta)) {
      shinyjs::runjs("$('#global-loading-indicator').show();")
    } else {
      shinyjs::runjs("$('#global-loading-indicator').hide();")
    }
  })
  
  # observeEvent(input$file, {
  #   req(input$file)
  #   updateNavbarPage(session, "main_tabs", selected = "editor")
  # })
  
  # observe({
  #   if (!is.null(data_rv$meta)) {
  #     updateTabsetPanel(session, "main_tabs", selected = "editor")
  #     shinyjs::disable(selector = "a[data-value='help']")
  #   } else {
  #     shinyjs::enable(selector = "a[data-value='help']")
  #   }
  # })
  
  
  counts_rv <- reactiveValues(
    counts = NULL,
    orig_counts = NULL
  )
  
  
  output$tables_ui <- renderUI({
    req(data_rv$meta, data_rv$counts)
    
    Sys.sleep(0.5)
    
    counts_rv <- reactiveValues(
      counts = data_rv$counts,
      orig_counts = data_rv$orig_counts
    )
    
    # Store the module output globally so we can access it
    meta_module_out <- editableTableServer(
      "meta",
      data_rv,
      counts_rv,
      user_prefs
    )
    # Assign to the global meta_out reactiveValues
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
