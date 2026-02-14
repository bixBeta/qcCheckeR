# =================================================
# EMAIL NOTIFICATIONS MODULE
# File: module_email_notifications.R
# =================================================
#
# Reusable Shiny module for email notifications with batched changes
#
# Features:
# - Email setup modal with test functionality
# - Change tracking with timestamps
# - Batched notifications (sends summary every N minutes)
# - Visual status indicators
# - Configurable email credentials
#
# Prerequisites:
# - blastula package installed
# - Gmail credentials file created (see setup function below)
#
# =================================================

#' Email Notifications Module - UI
#'
#' @param id Character string. Module namespace ID
#' @return tagList of UI elements
#'
#' @examples
#' # In your UI:
#' emailNotificationsUI("email")
emailNotificationsUI <- function(id) {
  ns <- NS(id)
  tagList(
    actionButton(
      ns("setup_notifications"),
      "Enable Email Notifications",
      icon = icon("envelope"),
      class = "btn-outline-info"
    ),
    uiOutput(ns("notification_status")),
    uiOutput(ns("batch_status"))
  )
}

#' Email Notifications Module - Server
#'
#' @param id Character string. Module namespace ID
#' @param email_config List with email configuration options:
#'   - enabled: Enable/disable email functionality (default: TRUE)
#'   - sender_email: Email address for sending (default: "your.app@gmail.com")
#'   - creds_file: Path to credentials file (default: "gmail_creds")
#'   - batch_delay_seconds: Seconds to wait before sending batch (default: 120)
#'   - min_changes_for_email: Minimum changes to trigger email (default: 1)
#' @param get_summary_func Reactive function that returns data summary for emails
#'   Should return a list with summary information
#' @param session_id_func Optional reactive function that returns current session/data ID.
#'   When this value changes, pending changes are automatically cleared to prevent
#'   mixing changes from different data contexts in email summaries.
#'
#' @return List of functions and reactive values:
#'   - notifications_enabled(): Reactive value indicating if notifications are on
#'   - user_email(): Reactive value with user's email address
#'   - track_change(type, details, ...): Function to track a change
#'
#' @examples
#' # In your server:
#' email <- emailNotificationsServer("email",
#'   email_config = list(
#'     enabled = TRUE,
#'     sender_email = "myapp@gmail.com",
#'     creds_file = "gmail_creds",
#'     batch_delay_seconds = 120,
#'     min_changes_for_email = 1
#'   ),
#'   get_summary_func = reactive({
#'     list(
#'       info = paste("Total rows:", nrow(my_data)),
#'       timestamp = Sys.time()
#'     )
#'   }),
#'   session_id_func = reactive({ current_session_id })  # Auto-clear on session change
#' )
#'
#' # Track a change
#' email$track_change(
#'   type = "Edit",
#'   details = "User edited cell A1",
#'   sample_label = "Sample1",
#'   old_value = "10",
#'   new_value = "20"
#' )
emailNotificationsServer <- function(
  id,
  email_config = NULL,
  get_summary_func = NULL,
  session_id_func = NULL # NEW: Optional reactive that returns current session ID
) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Default email config with local .credentials folder
    # This works better in Docker and keeps credentials with the app
    if (is.null(email_config)) {
      email_config <- list(
        enabled = TRUE,
        sender_email = "your.app.notifications@gmail.com",
        creds_file = ".credentials/gmail_creds", # Local to app directory
        batch_delay_seconds = 120,
        min_changes_for_email = 1
      )
    }

    # Handle creds_file path - support environment variable override
    if (!is.null(email_config$creds_file)) {
      # Check for environment variable override first
      env_creds <- Sys.getenv("EMAIL_CREDS_FILE", "")
      if (nzchar(env_creds)) {
        email_config$creds_file <- env_creds
      }
      # Only expand ~ if present (don't expand relative paths)
      if (grepl("^~", email_config$creds_file)) {
        email_config$creds_file <- path.expand(email_config$creds_file)
      }
    }

    # Log email configuration at startup
    cat("EMAIL MODULE: Initialized\n")
    cat("  Credentials path:", email_config$creds_file, "\n")
    cat(
      "  Full path:",
      normalizePath(email_config$creds_file, mustWork = FALSE),
      "\n"
    )
    cat("  Working directory:", getwd(), "\n")
    cat("  Credentials exist:", file.exists(email_config$creds_file), "\n")
    if (!file.exists(email_config$creds_file)) {
      cat("  ⚠️  Run setup_email.R to create credentials\n")
    }

    # Reactive values
    rv <- reactiveValues(
      notifications_enabled = FALSE,
      user_email = NULL,
      changes = list(),
      pending_changes = list(),
      batch_timer = NULL,
      is_batching = FALSE,
      last_sent = NULL,
      current_session_id = NULL # Track current session to detect changes
    )

    # AUTO-CLEAR: Watch for session ID changes and clear pending changes
    # This ensures old changes don't mix with new data when user loads different session/file
    if (!is.null(session_id_func)) {
      observe({
        new_session_id <- session_id_func()
        old_session_id <- rv$current_session_id

        # If session ID changed (and we had a previous session), clear pending changes
        if (
          !is.null(old_session_id) && !identical(new_session_id, old_session_id)
        ) {
          old_count <- length(rv$pending_changes)
          if (old_count > 0) {
            cat(
              "📧 Session changed: ",
              old_session_id,
              " → ",
              new_session_id,
              "\n"
            )
            cat(
              "📧 Auto-clearing ",
              old_count,
              " pending changes from previous session\n"
            )

            rv$pending_changes <- list()
            rv$is_batching <- FALSE
            rv$batch_timer <- NULL

            if (rv$notifications_enabled) {
              showNotification(
                paste0(
                  "Email queue cleared (",
                  old_count,
                  " changes from previous session)"
                ),
                type = "message",
                duration = 3
              )
            }
          }
        }

        rv$current_session_id <- new_session_id
      })
    }

    # Show setup modal
    observeEvent(input$setup_notifications, {
      showModal(modalDialog(
        title = "Email Notifications",
        fluidRow(
          column(
            12,
            p("Get notified when changes are made."),
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

    # Send test email
    observeEvent(input$test_notification, {
      req(input$user_email)

      if (!grepl(".+@.+\\..+", input$user_email)) {
        showNotification("Please enter a valid email address", type = "error")
        return()
      }

      if (!file.exists(email_config$creds_file)) {
        # Show detailed debug info
        full_path <- normalizePath(email_config$creds_file, mustWork = FALSE)
        showNotification(
          paste0(
            "Email credentials not found!\n",
            "Looking for: ",
            email_config$creds_file,
            "\n",
            "Full path: ",
            full_path,
            "\n",
            "Working dir: ",
            getwd(),
            "\n\n",
            "Run setup_email.R to create credentials."
          ),
          type = "error",
          duration = 15
        )
        cat("EMAIL DEBUG: Credentials not found\n")
        cat("  Config path:", email_config$creds_file, "\n")
        cat("  Full path:", full_path, "\n")
        cat("  Working dir:", getwd(), "\n")
        cat("  File exists:", file.exists(email_config$creds_file), "\n")
        if (dir.exists(dirname(email_config$creds_file))) {
          cat(
            "  Directory contents:",
            paste(
              list.files(dirname(email_config$creds_file), all.files = TRUE),
              collapse = ", "
            ),
            "\n"
          )
        } else {
          cat(
            "  Directory does not exist:",
            dirname(email_config$creds_file),
            "\n"
          )
        }
        return()
      }

      showNotification(
        "Sending test notification...",
        type = "message",
        id = "test_email"
      )

      tryCatch(
        {
          # Load credentials to get the actual sender email
          creds <- blastula::creds_file(file = email_config$creds_file)
          sender_email <- creds$user # Get actual email from credentials

          email_body <- blastula::md(glue::glue(
            "## Test Notification
          
          This is a test notification from your Shiny app.
          
          If you received this, email notifications are working correctly!
          
          **Configuration:**
          - Batch delay: {email_config$batch_delay_seconds} seconds
          - Sender: {sender_email}
          
          Timestamp: {format(Sys.time(), '%Y-%m-%d %H:%M:%S')}
          "
          ))

          test_email <- blastula::compose_email(body = email_body)

          blastula::smtp_send(
            email = test_email,
            from = sender_email,
            to = input$user_email,
            subject = "Test Notification - Shiny App",
            credentials = creds
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
    })

    # Enable notifications
    observeEvent(input$enable_notifications, {
      req(input$user_email)

      if (!grepl(".+@.+\\..+", input$user_email)) {
        showNotification("Please enter a valid email address", type = "error")
        return()
      }

      rv$notifications_enabled <- TRUE
      rv$user_email <- input$user_email

      # Check if there are already pending changes
      pending_count <- length(rv$pending_changes)

      if (pending_count > 0) {
        # Start batching timer for existing changes
        rv$is_batching <- TRUE
        rv$batch_timer <- Sys.time()

        cat(
          "📧 Notifications enabled with",
          pending_count,
          "existing changes\n"
        )
        cat(
          "⏰ Batch timer started at:",
          format(rv$batch_timer, "%H:%M:%S"),
          "\n"
        )

        showNotification(
          glue::glue(
            "Email notifications enabled! {pending_count} existing change(s) will be sent in ",
            "{email_config$batch_delay_seconds} seconds."
          ),
          type = "message",
          duration = 5
        )
      } else {
        cat("📧 Notifications enabled. No pending changes yet.\n")
        showNotification(
          glue::glue(
            "Email notifications enabled! Changes will be batched and sent every ",
            "{email_config$batch_delay_seconds} seconds."
          ),
          type = "message"
        )
      }

      removeModal()
    })

    # Send batched changes
    send_batched_changes <- function() {
      if (!email_config$enabled || !rv$notifications_enabled) {
        return(FALSE)
      }
      if (!file.exists(email_config$creds_file)) {
        cat("EMAIL BATCH: Credentials file not found, skipping send\n")
        cat("  Expected:", email_config$creds_file, "\n")
        return(FALSE)
      }
      if (length(rv$pending_changes) < email_config$min_changes_for_email) {
        return(FALSE)
      }

      tryCatch(
        {
          pending_changes <- rv$pending_changes

          # Get data summary if function provided (call it within reactive context)
          data_summary <- if (!is.null(get_summary_func)) {
            isolate(get_summary_func())
          } else {
            list(info = "No summary available")
          }

          # Group changes by type
          change_counts <- table(sapply(pending_changes, function(x) x$type))
          change_summary <- paste(
            names(change_counts),
            "(",
            change_counts,
            ")",
            collapse = ", "
          )

          # Format time range
          timestamps <- sapply(pending_changes, function(x) {
            if (inherits(x$timestamp, "POSIXt")) {
              as.numeric(x$timestamp)
            } else {
              as.numeric(Sys.time())
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

          # Format detailed changes
          detailed_changes <- paste(
            sapply(pending_changes, function(change) {
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

              # Add optional fields if present
              if (!is.null(change$sample_label)) {
                details <- paste(
                  details,
                  paste0("  - Sample: ", change$sample_label),
                  sep = "\n"
                )
              }
              if (!is.null(change$old_value) && !is.null(change$new_value)) {
                details <- paste(
                  details,
                  paste0(
                    "  - Changed from '",
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

          # Create summary text from data_summary
          summary_text <- if (is.list(data_summary)) {
            if (!is.null(data_summary$info)) {
              data_summary$info
            } else {
              paste(
                names(data_summary),
                ":",
                unlist(data_summary),
                collapse = "\n"
              )
            }
          } else {
            as.character(data_summary)
          }

          email_body <- blastula::md(glue::glue(
            "## Batched Changes Summary
          
          Here's a summary of changes made in the last {email_config$batch_delay_seconds} seconds:
          
          **Change Summary:**
          - **Total Changes:** {length(pending_changes)}
          - **Change Types:** {change_summary}
          - **Time Period:** {time_range}
          
          **Detailed Changes:**
          {detailed_changes}
          
          **Current Status:**
          {summary_text}
          
          **Summary Generated:** {format(Sys.time(), '%Y-%m-%d %H:%M:%S')}
          
          ---
          *This is an automated notification. Please do not reply to this email.*
          "
          ))

          email <- blastula::compose_email(body = email_body)

          blastula::smtp_send(
            email = email,
            to = rv$user_email,
            from = email_config$sender_email,
            subject = glue::glue(
              "Shiny App: {length(pending_changes)} Changes - Batched Summary"
            ),
            credentials = blastula::creds_file(file = email_config$creds_file)
          )

          # Clear pending changes
          rv$pending_changes <- list()
          rv$is_batching <- FALSE
          rv$last_sent <- Sys.time()

          cat("Batched email sent with", length(pending_changes), "changes\n")
          return(TRUE)
        },
        error = function(e) {
          warning("Failed to send batched changes:", e$message)
          return(FALSE)
        }
      )
    }

    # Observer to check and send batched changes
    # Store observer ID to track if it's recreating
    observer_id <- paste0("observer_", sample(1000:9999, 1))
    cat("Creating observer with ID:", observer_id, "\n")

    observe({
      # Debug output
      if (rv$notifications_enabled) {
        cat("\n=== Observer Check (", observer_id, ") ===\n")
        cat("Notifications enabled:", rv$notifications_enabled, "\n")
        cat("Is batching:", rv$is_batching, "\n")
        cat("Pending changes:", length(rv$pending_changes), "\n")

        if (!is.null(rv$batch_timer)) {
          # Store the timer value to check if it changes
          timer_value <- rv$batch_timer
          cat("Batch timer value:", format(timer_value, "%H:%M:%S.%OS3"), "\n")
          cat("Current time:", format(Sys.time(), "%H:%M:%S.%OS3"), "\n")
        } else {
          cat("Batch timer: NULL\n")
        }
      }

      if (
        rv$notifications_enabled &&
          rv$is_batching &&
          length(rv$pending_changes) >= email_config$min_changes_for_email
      ) {
        if (!is.null(rv$batch_timer)) {
          # Get both times ONCE to avoid race conditions
          timer_value <- rv$batch_timer
          current_time <- Sys.time()

          # Calculate time difference
          time_since_batch <- as.numeric(difftime(
            current_time,
            timer_value,
            units = "secs"
          ))

          cat(
            "Time since batch start:",
            round(time_since_batch, 2),
            "seconds\n"
          )
          cat(
            "Batch delay needed:",
            email_config$batch_delay_seconds,
            "seconds\n"
          )

          if (time_since_batch >= email_config$batch_delay_seconds) {
            cat("✅ TIME TO SEND EMAIL!\n")

            showNotification(
              "Sending batched change summary...",
              type = "message",
              id = "sending_batch",
              duration = NULL
            )

            # Call send function within the reactive context
            success <- send_batched_changes()

            removeNotification("sending_batch")

            if (success) {
              showNotification("Batched change summary sent!", type = "message")
              cat("✅ Email sent successfully!\n")
            } else {
              showNotification("Failed to send batch summary", type = "warning")
              cat("❌ Email send failed\n")
            }
          } else {
            remaining <- email_config$batch_delay_seconds - time_since_batch
            cat("⏳ Waiting", round(remaining), "more seconds\n")
          }
        }
      }

      cat("======================\n\n")

      # Check again in 10 seconds (only if notifications enabled)
      if (rv$notifications_enabled) {
        invalidateLater(10000, session)
      }
    })

    # Notification status UI
    output$notification_status <- renderUI({
      if (rv$notifications_enabled) {
        div(
          class = "notification-status",
          style = "font-size: 0.85em; color: #28a745; margin-top: 5px;",
          icon("check-circle", class = "text-success"),
          "Notifications enabled for: ",
          tags$strong(rv$user_email),
          br(),
          tags$small(
            "Changes batched every ",
            email_config$batch_delay_seconds,
            " seconds"
          )
        )
      }
    })

    # Batch status UI
    output$batch_status <- renderUI({
      if (rv$notifications_enabled && rv$is_batching) {
        pending_count <- length(rv$pending_changes)

        if (pending_count > 0) {
          time_remaining <- max(
            0,
            email_config$batch_delay_seconds -
              as.numeric(Sys.time() - rv$batch_timer)
          )

          div(
            class = "batch-status",
            style = "background-color: #e8f4fd; border: 1px solid #b6e0fe; 
                     border-radius: 5px; padding: 10px; margin: 10px 0; color: #0c5460;",
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

    # Return list of functions and reactive values
    list(
      # Reactive values (read-only)
      notifications_enabled = reactive(rv$notifications_enabled),
      user_email = reactive(rv$user_email),

      # Functions

      # Track a change (will be batched for email)
      track_change = function(
        type,
        details,
        sample_label = NULL,
        old_value = NULL,
        new_value = NULL
      ) {
        change <- list(
          id = paste0(
            "change_",
            as.integer(Sys.time()),
            "_",
            sample(1000:9999, 1)
          ),
          timestamp = Sys.time(),
          type = type,
          details = details,
          sample_label = sample_label,
          old_value = old_value,
          new_value = new_value
        )

        # ALWAYS track changes, even if notifications not enabled yet
        rv$pending_changes <- c(rv$pending_changes, list(change))

        # Add to full change history (for reference)
        rv$changes <- c(rv$changes, list(change))

        # Limit history to last 50 changes to prevent memory issues
        if (length(rv$changes) > 50) {
          rv$changes <- rv$changes[-1]
        }

        cat(
          "Change tracked:",
          type,
          "(Total pending:",
          length(rv$pending_changes),
          ")\n"
        )

        # Start batching timer ONLY if notifications are enabled AND not already batching
        if (rv$notifications_enabled && !rv$is_batching) {
          rv$is_batching <- TRUE
          rv$batch_timer <- Sys.time()

          cat(
            "✅ Batch timer started at:",
            format(rv$batch_timer, "%H:%M:%S"),
            "\n"
          )

          showNotification(
            glue::glue(
              "Changes are being batched... Summary will be sent in ",
              "{email_config$batch_delay_seconds} seconds."
            ),
            type = "message",
            duration = 3
          )
        } else if (rv$is_batching) {
          cat(
            "➕ Adding change to existing batch. Timer started at:",
            format(rv$batch_timer, "%H:%M:%S"),
            "\n"
          )
        } else {
          cat("📝 Change logged. (Notifications not enabled yet)\n")
        }
      }
    )
  })
}

# =================================================
# ONE-TIME SETUP FUNCTION FOR EMAIL CREDENTIALS
# =================================================

#' Set up Gmail credentials for email notifications
#'
#' Run this function once to create the credentials file.
#' By default, stores in a .credentials folder in the current directory
#' (works well in Docker and keeps credentials with the app).
#'
#' You need:
#' 1. A Gmail account (dedicated app account recommended)
#' 2. 2-factor authentication enabled
#' 3. An App Password generated from Google Account settings
#'
#' @param creds_file Path where credentials will be saved.
#'   Default: ".credentials/gmail_creds" (local to app directory)
#'   Use NULL for default location.
#'
#' @return The path to the created credentials file (invisibly)
#'
#' @examples
#' # Run once in your R console (uses local .credentials folder):
#' setup_email_credentials()
#'
#' # Or specify custom file path:
#' setup_email_credentials("/path/to/gmail_creds")
#'
#' # For Docker, you can also set via environment variable:
#' # Sys.setenv(EMAIL_CREDS_FILE = "/app/.credentials/gmail_creds")
setup_email_credentials <- function(creds_file = NULL) {
  if (!requireNamespace("blastula", quietly = TRUE)) {
    stop(
      "Package 'blastula' is required. Install it with: install.packages('blastula')"
    )
  }

  # Use local .credentials folder if not specified
  if (is.null(creds_file)) {
    creds_dir <- ".credentials"
    if (!dir.exists(creds_dir)) {
      dir.create(creds_dir, recursive = TRUE)
      cat("Created credentials directory:", creds_dir, "\n")
    }
    creds_file <- file.path(creds_dir, "gmail_creds")
  } else {
    # Only expand ~ if present
    if (grepl("^~", creds_file)) {
      creds_file <- path.expand(creds_file)
    }
    # Create parent directory if needed
    creds_dir <- dirname(creds_file)
    if (!dir.exists(creds_dir) && creds_dir != ".") {
      dir.create(creds_dir, recursive = TRUE)
      cat("Created credentials directory:", creds_dir, "\n")
    }
  }

  cat("\n=== Email Credentials Setup ===\n")
  cat(
    "Credentials will be saved to:",
    normalizePath(creds_file, mustWork = FALSE),
    "\n\n"
  )
  cat("Please make sure you have:\n")
  cat("1. A Gmail account for the app\n")
  cat("2. 2-factor authentication enabled\n")
  cat("3. An App Password generated (16 characters)\n")
  cat("   Generate at: https://myaccount.google.com/apppasswords\n\n")

  # Get email from user
  email <- readline(prompt = "Enter your Gmail address: ")

  if (!grepl(".+@gmail\\.com$", email)) {
    cat("\n⚠️  Warning: This doesn't look like a Gmail address.\n")
    proceed <- readline(prompt = "Continue anyway? (y/n): ")
    if (tolower(proceed) != "y") {
      cat("Setup cancelled.\n")
      return(invisible(NULL))
    }
  }

  # Create credentials file
  cat("\nCreating credentials file...\n")
  blastula::create_smtp_creds_file(
    file = creds_file,
    user = email,
    host = "smtp.gmail.com",
    port = 465,
    use_ssl = TRUE
  )

  # Set secure file permissions (Unix/Mac only)
  tryCatch(
    {
      Sys.chmod(creds_file, mode = "0600")
      cat("✅ Secure file permissions set (owner read/write only)\n")
    },
    error = function(e) {
      cat("⚠️  Could not set file permissions (Windows system?)\n")
      cat("   Please restrict access to this file manually.\n")
    }
  )

  cat("\n✅ Email credentials file created successfully!\n")
  cat("📁 File location:", normalizePath(creds_file, mustWork = FALSE), "\n")
  cat("📧 Sender email:", email, "\n\n")

  cat("🔐 SECURITY REMINDERS:\n")
  cat("1. Add .credentials/ to your .gitignore file\n")
  cat("2. Do NOT commit this file to version control\n")
  cat("3. In Docker, mount this file or create it during setup\n\n")

  # Test the configuration
  test <- readline(prompt = "Send a test email? (y/n): ")
  if (tolower(test) == "y") {
    test_recipient <- readline(prompt = "Send test to (email): ")

    cat("\nSending test email...\n")
    tryCatch(
      {
        test_email <- blastula::compose_email(
          body = blastula::md(glue::glue(
            "## Email Configuration Test
          
          This is a test email to verify your configuration.
          
          If you received this, everything is working correctly!
          
          **Configuration Details:**
          - Sender: {email}
          - Credentials file: {creds_file}
          - Timestamp: {format(Sys.time(), '%Y-%m-%d %H:%M:%S')}
          "
          ))
        )

        blastula::smtp_send(
          email = test_email,
          from = email,
          to = test_recipient,
          subject = "Shiny App: Configuration Test",
          credentials = blastula::creds_file(file = creds_file)
        )

        cat("✅ Test email sent successfully! Check your inbox.\n")
      },
      error = function(e) {
        cat("❌ Test failed:", e$message, "\n")
        cat("\nTroubleshooting:\n")
        cat(
          "- Verify your App Password is correct (16 characters, no spaces)\n"
        )
        cat("- Make sure 2-factor authentication is enabled\n")
        cat("- Check your internet connection\n")
      }
    )
  }

  cat("\n📝 To use in your app, set:\n")
  cat("   email_config = list(\n")
  cat("     creds_file = \"", creds_file, "\"\n", sep = "")
  cat("   )\n\n")

  invisible(creds_file)
}

#' Get the default credentials file path
#'
#' Returns the secure default path for email credentials.
#' Useful for configuration without hardcoding paths.
#'
#' @return Character string with the default credentials path
#'
#' @examples
#' # Use in your app configuration:
#' email_config = list(
#'   creds_file = get_default_creds_path()
#' )
get_default_creds_path <- function() {
  path.expand("~/.shiny_app_credentials/gmail_creds")
}
