# =============================================================================
# MODULE: User Sessions Management
# =============================================================================
# Shared module for managing user sessions across multiple Shiny apps
# Uses email as user identifier with hashed directory names for privacy
#
# Features:
# - User identification via email
# - Session registry (JSON) for cross-app discovery
# - Namespaced storage per user + app
# - Cross-app session linking
# - Configurable retention policies
#
# Usage:
#   source("path/to/module_user_sessions.R")
#   
#   # In UI:
#   userSessionsUI("sessions")
#   
#   # In Server:
#   sessions <- userSessionsServer(
#     id = "sessions",
#     app_name = "qc-checker",
#     base_path = "/srv/shiny-apps/shared/sessions"
#   )
#
# =============================================================================

library(shiny)
library(jsonlite)
library(digest)

# =============================================================================
# CONFIGURATION
# =============================================================================

DEFAULT_SETTINGS <- list(
  retention_days = NULL,        # NULL = keep forever

  max_sessions_per_user = 50,   # Per app
  session_prefix = list(
    "qc-checker" = "qc_",
    "deg-explorer" = "deg_",
    "pathway-analyzer" = "path_"
  )
)

# =============================================================================
# HELPER FUNCTIONS
# =============================================================================

#' Hash email for directory name (privacy + filesystem safety)
#' @param email User email address
#' @return 12-character hash string
hash_email <- function(email) {

  substr(digest::digest(tolower(trimws(email)), algo = "sha256"), 1, 12)
}

#' Validate email format
#' @param email Email string to validate
#' @return TRUE if valid email format
is_valid_email <- function(email) {

  if (is.null(email) || is.na(email) || email == "") return(FALSE)
  grepl("^[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,}$", email)
}

#' Get or create registry
#' @param base_path Base path for sessions storage
#' @return List containing registry data
get_registry <- function(base_path) {
  registry_file <- file.path(base_path, "registry.json")
  
  if (file.exists(registry_file)) {
    tryCatch({
      jsonlite::fromJSON(registry_file, simplifyVector = FALSE)
    }, error = function(e) {
      warning("Failed to read registry, creating new: ", e$message)
      create_empty_registry()
    })
  } else {
    create_empty_registry()
  }
}

#' Create empty registry structure
#' @return List with empty registry structure
create_empty_registry <- function() {
  list(
    users = list(),
    settings = DEFAULT_SETTINGS[c("retention_days", "max_sessions_per_user")],
    version = "1.0.0",
    created = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ")
  )
}

#' Save registry to disk
#' @param registry Registry list object
#' @param base_path Base path for sessions storage
save_registry <- function(registry, base_path) {
  registry_file <- file.path(base_path, "registry.json")
  
  # Ensure directory exists
  if (!dir.exists(base_path)) {
    dir.create(base_path, recursive = TRUE)
  }
  
  # Write with pretty formatting
  jsonlite::write_json(
    registry, 
    registry_file, 
    pretty = TRUE, 
    auto_unbox = TRUE,
    null = "null"
  )
}

#' Get user's session directory
#' @param base_path Base path for sessions storage
#' @param email User email
#' @param app_name App name
#' @return Full path to user's app session directory
get_user_session_dir <- function(base_path, email, app_name) {
  email_hash <- hash_email(email)
  file.path(base_path, "users", email_hash, app_name)
}

#' Generate session ID with app prefix
#' @param app_name App name
#' @return Unique session ID string
generate_session_id <- function(app_name) {
  prefix <- DEFAULT_SETTINGS$session_prefix[[app_name]] %||% "sess_"
  paste0(prefix, format(Sys.time(), "%Y%m%d%H%M%S"), "_", 
         substr(digest::digest(runif(1)), 1, 8))
}

# =============================================================================
# UI MODULE
# =============================================================================

#' User Sessions UI
#' @param id Module namespace ID
#' @return Shiny UI elements
userSessionsUI <- function(id) {


  ns <- NS(id)
  
  tagList(
    # User identification section
    div(
      id = ns("user_section"),
      class = "user-session-panel",
      
      # Email input with validation
      div(
        class = "mb-3",
        textInput(
          ns("user_email"),
          label = tagList(icon("envelope"), "Your Email"),
          placeholder = "you@institution.edu",
          width = "100%"
        ),
        uiOutput(ns("email_status"))
      ),
      
      # Session selection (shown after email validated)
      conditionalPanel(
        condition = sprintf("output['%s']", ns("user_authenticated")),
        
        hr(),
        
        # Current session info
        uiOutput(ns("current_session_info")),
        
        hr(),
        
        # Session actions
        div(
          class = "d-flex gap-2 flex-wrap",
          actionButton(
            ns("new_session"),
            "New Session",
            icon = icon("plus"),
            class = "btn-outline-primary btn-sm"
          ),
          actionButton(
            ns("load_session"),
            "Load Session",
            icon = icon("folder-open"),
            class = "btn-outline-secondary btn-sm"
          ),
          actionButton(
            ns("save_session"),
            "Save Session",
            icon = icon("save"),
            class = "btn-success btn-sm"
          )
        )
      )
    ),
    
    # CSS for the panel
    tags$style(HTML(sprintf("
      #%s {
        padding: 15px;
        background: #f8f9fa;
        border-radius: 8px;
        border: 1px solid #dee2e6;
      }
      .session-badge {
        font-size: 0.75em;
        padding: 2px 8px;
        border-radius: 4px;
      }
    ", ns("user_section"))))
  )
}

# =============================================================================
# SERVER MODULE
# =============================================================================

#' User Sessions Server
#' @param id Module namespace ID
#' @param app_name Name of current app (e.g., "qc-checker")
#' @param base_path Base path for sessions storage
#' @param get_session_data Function that returns current session data to save
#' @param on_session_load Callback function when session is loaded
#' @return List with reactive values and functions
userSessionsServer <- function(
  id, 

  app_name,
  base_path = "shared/sessions",
  get_session_data = NULL,
  on_session_load = NULL
) {
  
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # =========================================================================
    # REACTIVE VALUES
    # =========================================================================
    
    user_rv <- reactiveValues(
      email = NULL,
      email_hash = NULL,
      authenticated = FALSE,
      current_session_id = NULL,
      session_data = NULL,
      sessions_list = list()
    )
    
    # =========================================================================
    # EMAIL VALIDATION & USER SETUP
    # =========================================================================
    
    # Debounced email validation
    email_debounced <- debounce(reactive(input$user_email), 500)
    
    observe({
      email <- email_debounced()
      
      if (is_valid_email(email)) {
        user_rv$email <- tolower(trimws(email))
        user_rv$email_hash <- hash_email(user_rv$email)
        user_rv$authenticated <- TRUE
        
        # Ensure user directory exists
        user_dir <- get_user_session_dir(base_path, user_rv$email, app_name)
        if (!dir.exists(user_dir)) {
          dir.create(user_dir, recursive = TRUE)
        }
        
        # Register user if new
        register_user(user_rv$email, base_path)
        
        # Load user's session list
        user_rv$sessions_list <- get_user_sessions(
          user_rv$email, 
          app_name, 
          base_path
        )
        
      } else {
        user_rv$authenticated <- FALSE
      }
    })
    
    # Email status indicator
    output$email_status <- renderUI({
      email <- input$user_email
      
      if (is.null(email) || email == "") {
        return(NULL)
      }
      
      if (is_valid_email(email)) {
        span(
          class = "text-success small",
          icon("check-circle"), " Valid email"
        )
      } else {
        span(
          class = "text-danger small",
          icon("exclamation-circle"), " Please enter a valid email"
        )
      }
    })
    
    # Authentication flag for conditional panel
    output$user_authenticated <- reactive({
      user_rv$authenticated
    })
    outputOptions(output, "user_authenticated", suspendWhenHidden = FALSE)
    
    # =========================================================================
    # CURRENT SESSION DISPLAY
    # =========================================================================
    
    output$current_session_info <- renderUI({
      if (!user_rv$authenticated) return(NULL)
      
      if (is.null(user_rv$current_session_id)) {
        div(
          class = "text-muted small",
          icon("info-circle"), " No active session. Create a new one or load an existing session."
        )
      } else {
        div(
          p(
            strong("Session ID: "),
            code(user_rv$current_session_id)
          ),
          if (!is.null(user_rv$session_data$file_name)) {
            p(
              class = "small text-muted mb-0",
              strong("File: "), user_rv$session_data$file_name
            )
          }
        )
      }
    })
    
    # =========================================================================
    # SESSION ACTIONS
    # =========================================================================
    
    # New Session
    observeEvent(input$new_session, {
      req(user_rv$authenticated)
      
      user_rv$current_session_id <- generate_session_id(app_name)
      user_rv$session_data <- list(
        created = Sys.time(),
        updated = Sys.time()
      )
      
      showNotification(
        paste("New session created:", user_rv$current_session_id),
        type = "message",
        duration = 3
      )
    })
    
    # Load Session Modal
    observeEvent(input$load_session, {
      req(user_rv$authenticated)
      
      # Refresh sessions list
      user_rv$sessions_list <- get_user_sessions(
        user_rv$email, 
        app_name, 
        base_path
      )
      
      showModal(modalDialog(
        title = "Load Session",
        size = "l",
        
        if (length(user_rv$sessions_list) == 0) {
          div(
            class = "text-center text-muted py-4",
            icon("folder-open", class = "fa-3x mb-3"),
            p("No saved sessions found.")
          )
        } else {
          tagList(
            p(class = "text-muted", 
              "Select a session to load:"),
            div(
              style = "max-height: 400px; overflow-y: auto;",
              lapply(seq_along(user_rv$sessions_list), function(i) {
                sess <- user_rv$sessions_list[[i]]
                div(
                  class = "card mb-2",
                  style = "cursor: pointer;",
                  onclick = sprintf(
                    "Shiny.setInputValue('%s', '%s', {priority: 'event'})",
                    ns("select_session"),
                    sess$session_id
                  ),
                  div(
                    class = "card-body py-2 px-3",
                    div(
                      class = "d-flex justify-content-between align-items-center",
                      div(
                        strong(sess$file_name %||% "Unnamed"),
                        br(),
                        small(
                          class = "text-muted",
                          sess$session_id
                        )
                      ),
                      div(
                        class = "text-end",
                        if (isTRUE(sess$has_pca)) {
                          span(class = "badge bg-success me-1", "PCA")
                        },
                        if (isTRUE(sess$has_annotations)) {
                          span(class = "badge bg-info me-1", "Annotated
")
                        },
                        br(),
                        small(
                          class = "text-muted",
                          format(
                            as.POSIXct(sess$updated), 
                            "%Y-%m-%d %H:%M"
                          )
                        )
                      )
                    )
                  )
                )
              })
            )
          )
        },
        
        footer = modalButton("Cancel"),
        easyClose = TRUE
      ))
    })
    
    # Handle session selection
    observeEvent(input$select_session, {
      req(user_rv$authenticated)
      session_id <- input$select_session
      
      tryCatch({
        session_file <- file.path(
          get_user_session_dir(base_path, user_rv$email, app_name),
          paste0(session_id, ".rds")
        )
        
        if (file.exists(session_file)) {
          loaded_data <- readRDS(session_file)
          user_rv$current_session_id <- session_id
          user_rv$session_data <- loaded_data
          
          # Call the callback if provided
          if (!is.null(on_session_load) && is.function(on_session_load)) {
            on_session_load(loaded_data)
          }
          
          removeModal()
          showNotification(
            paste("Session loaded:", session_id),
            type = "message",
            duration = 3
          )
        } else {
          showNotification(
            "Session file not found",
            type = "error",
            duration = 5
          )
        }
      }, error = function(e) {
        showNotification(
          paste("Failed to load session:", e$message),
          type = "error",
          duration = 5
        )
      })
    })
    
    # Save Session
    observeEvent(input$save_session, {
      req(user_rv$authenticated)
      
      # Create new session ID if none exists
      if (is.null(user_rv$current_session_id)) {
        user_rv$current_session_id <- generate_session_id(app_name)
      }
      
      # Get current data from callback
      if (!is.null(get_session_data) && is.function(get_session_data)) {
        save_data <- get_session_data()
      } else {
        save_data <- user_rv$session_data
      }
      
      if (is.null(save_data)) {
        showNotification(
          "No data to save",
          type = "warning",
          duration = 3
        )
        return()
      }
      
      tryCatch({
        # Add metadata
        save_data$session_id <- user_rv$current_session_id
        save_data$user_email <- user_rv$email
        save_data$app_name <- app_name
        save_data$updated <- Sys.time()
        if (is.null(save_data$created)) {
          save_data$created <- Sys.time()
        }
        
        # Save RDS file
        session_dir <- get_user_session_dir(base_path, user_rv$email, app_name)
        if (!dir.exists(session_dir)) {
          dir.create(session_dir, recursive = TRUE)
        }
        
        session_file <- file.path(
          session_dir, 
          paste0(user_rv$current_session_id, ".rds")
        )
        saveRDS(save_data, session_file)
        
        # Update registry
        update_session_registry(
          email = user_rv$email,
          app_name = app_name,
          session_info = list(
            session_id = user_rv$current_session_id,
            created = format(save_data$created, "%Y-%m-%dT%H:%M:%SZ"),
            updated = format(save_data$updated, "%Y-%m-%dT%H:%M:%SZ"),
            file_name = save_data$file_name %||% save_data$file_data$file_name,
            samples = if (!is.null(save_data$file_data$metadata)) {
              nrow(save_data$file_data$metadata)
            } else NULL,
            groups = if (!is.null(save_data$file_data$metadata$group)) {
              unique(as.character(save_data$file_data$metadata$group))
            } else NULL,
            has_pca = !is.null(save_data$current_edits$pca),
            has_annotations = !is.null(save_data$current_edits$annotation),
            status = "complete"
          ),
          base_path = base_path
        )
        
        user_rv$session_data <- save_data
        
        showNotification(
          paste("Session saved:", user_rv$current_session_id),
          type = "message",
          duration = 3
        )
        
      }, error = function(e) {
        showNotification(
          paste("Failed to save session:", e$message),
          type = "error",
          duration = 5
        )
      })
    })
    
    # =========================================================================
    # RETURN MODULE API
    # =========================================================================
    
    return(list(
      # Reactive values
      email = reactive(user_rv$email),
      email_hash = reactive(user_rv$email_hash),
      authenticated = reactive(user_rv$authenticated),
      current_session_id = reactive(user_rv$current_session_id),
      session_data = reactive(user_rv$session_data),
      sessions_list = reactive(user_rv$sessions_list),
      
      # Functions
      save_session = function(data) {
        user_rv$session_data <- data
        shinyjs::click(ns("save_session"))
      },
      
      set_session_id = function(id) {
        user_rv$current_session_id <- id
      },
      
      get_upstream_sessions = function(upstream_app) {
        if (!user_rv$authenticated) return(list())
        get_user_sessions(user_rv$email, upstream_app, base_path)
      },
      
      load_upstream_session = function(upstream_app, session_id) {
        if (!user_rv$authenticated) return(NULL)
        session_file <- file.path(
          get_user_session_dir(base_path, user_rv$email, upstream_app),
          paste0(session_id, ".rds")
        )
        if (file.exists(session_file)) {
          readRDS(session_file)
        } else {
          NULL
        }
      }
    ))
  })
}

# =============================================================================
# REGISTRY FUNCTIONS
# =============================================================================

#' Register a new user
#' @param email User email
#' @param base_path Base path for sessions storage
register_user <- function(email, base_path) {
  registry <- get_registry(base_path)
  email_hash <- hash_email(email)
  
  if (is.null(registry$users[[email_hash]])) {
    registry$users[[email_hash]] <- list(
      email = email,
      created = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ"),
      apps = list()
    )
    save_registry(registry, base_path)
  }
  
  invisible(TRUE)
}

#' Get user's sessions for an app
#' @param email User email
#' @param app_name App name
#' @param base_path Base path for sessions storage
#' @return List of session info objects
get_user_sessions <- function(email, app_name, base_path) {
  registry <- get_registry(base_path)
  email_hash <- hash_email(email)
  
  user_data <- registry$users[[email_hash]]
  if (is.null(user_data)) return(list())
  
  sessions <- user_data$apps[[app_name]]
  if (is.null(sessions)) return(list())
  
  # Sort by updated date descending
  sessions <- sessions[order(
    sapply(sessions, function(x) x$updated %||% x$created),
    decreasing = TRUE
  )]
  
  sessions
}

#' Update session in registry
#' @param email User email
#' @param app_name App name
#' @param session_info Session info list
#' @param base_path Base path for sessions storage
update_session_registry <- function(email, app_name, session_info, base_path) {
  registry <- get_registry(base_path)
  email_hash <- hash_email(email)
  
  # Ensure user exists
  if (is.null(registry$users[[email_hash]])) {
    register_user(email, base_path)
    registry <- get_registry(base_path)
  }
  
  # Ensure app list exists
  if (is.null(registry$users[[email_hash]]$apps[[app_name]])) {
    registry$users[[email_hash]]$apps[[app_name]] <- list()
  }
  
  # Find existing session or add new
  sessions <- registry$users[[email_hash]]$apps[[app_name]]
  
  # Handle empty sessions list
  if (length(sessions) == 0) {
    existing_idx <- integer(0)
  } else {
    # Use vapply for type safety - returns logical vector
    matches <- vapply(sessions, function(x) {
      identical(x$session_id, session_info$session_id)
    }, logical(1))
    existing_idx <- which(matches)
  }
  
  if (length(existing_idx) > 0) {
    # Update existing
    registry$users[[email_hash]]$apps[[app_name]][[existing_idx[1]]] <- session_info
  } else {
    # Add new
    registry$users[[email_hash]]$apps[[app_name]] <- c(
      registry$users[[email_hash]]$apps[[app_name]],
      list(session_info)
    )
  }
  
  save_registry(registry, base_path)
  invisible(TRUE)
}

#' Delete a session
#' @param email User email
#' @param app_name App name
#' @param session_id Session ID to delete
#' @param base_path Base path for sessions storage
delete_session <- function(email, app_name, session_id, base_path) {
 # Delete file
  session_file <- file.path(
    get_user_session_dir(base_path, email, app_name),
    paste0(session_id, ".rds")
  )
  if (file.exists(session_file)) {
    file.remove(session_file)
  }
  
  # Log the deletion
  log_session_activity(
    base_path = base_path,
    email = email,
    session_id = session_id,
    app_name = app_name,
    action = "deleted"
  )
  
  # Remove from registry
  registry <- get_registry(base_path)
  email_hash <- hash_email(email)
  
  if (!is.null(registry$users[[email_hash]]$apps[[app_name]])) {
    sessions <- registry$users[[email_hash]]$apps[[app_name]]
    
    # Handle empty sessions list
    if (length(sessions) == 0) {
      # Nothing to delete
      return(invisible(TRUE))
    }
    
    # Use vapply for type safety
    keep_matches <- vapply(sessions, function(x) {
      !identical(x$session_id, session_id)
    }, logical(1))
    keep_idx <- which(keep_matches)
    
    registry$users[[email_hash]]$apps[[app_name]] <- sessions[keep_idx]
    save_registry(registry, base_path)
  }
  
  invisible(TRUE)
}

# =============================================================================
# SESSION ACTIVITY LOGGING
# =============================================================================

#' Log session activity to CSV file
#' @param base_path Base path for sessions storage
#' @param email User email
#' @param session_id Session ID
#' @param app_name App name
#' @param action Action type: created, updated, loaded, deleted
#' @param file_name Optional file name
#' @param samples Optional sample count
#' @param has_pca Optional PCA flag
#' @param has_annotations Optional annotations flag
log_session_activity <- function(
  base_path,
  email,
 session_id,
  app_name,
  action,
  file_name = NA,
  samples = NA,
  has_pca = NA,
  has_annotations = NA
) {
  tryCatch({
    log_file <- file.path(base_path, "session_activity_log.csv")
    
    # Create log entry
    log_entry <- data.frame(
      timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      email = email,
      email_hash = hash_email(email),
      session_id = session_id,
      app_name = app_name,
      action = action,
      file_name = as.character(file_name),
      samples = as.integer(samples),
      has_pca = as.logical(has_pca),
      has_annotations = as.logical(has_annotations),
      stringsAsFactors = FALSE
    )
    
    # Append to CSV (create with header if doesn't exist)
    write_header <- !file.exists(log_file)
    write.table(
      log_entry,
      file = log_file,
      sep = ",",
      row.names = FALSE,
      col.names = write_header,
      append = !write_header,
      quote = TRUE
    )
    
    invisible(TRUE)
  }, error = function(e) {
    warning("Failed to log session activity: ", e$message)
    invisible(FALSE)
  })
}

#' Read session activity log
#' @param base_path Base path for sessions storage
#' @return Data frame of all activity, or empty data frame if no log exists
read_session_log <- function(base_path) {
  log_file <- file.path(base_path, "session_activity_log.csv")
  
  if (!file.exists(log_file)) {
    return(data.frame(
      timestamp = character(),
      email = character(),
      email_hash = character(),
      session_id = character(),
      app_name = character(),
      action = character(),
      file_name = character(),
      samples = integer(),
      has_pca = logical(),
      has_annotations = logical(),
      stringsAsFactors = FALSE
    ))
  }
  
  tryCatch({
    read.csv(log_file, stringsAsFactors = FALSE)
  }, error = function(e) {
    warning("Failed to read session log: ", e$message)
    data.frame()
  })
}

#' Get summary of all users and sessions
#' @param base_path Base path for sessions storage
#' @return Data frame with user summary
get_users_summary <- function(base_path) {
  registry <- get_registry(base_path)
  
  if (length(registry$users) == 0) {
    return(data.frame(
      email = character(),
      email_hash = character(),
      registered = character(),
      total_sessions = integer(),
      apps_used = character(),
      stringsAsFactors = FALSE
    ))
  }
  
  # Build summary from registry
  summary_list <- lapply(names(registry$users), function(hash) {
    user <- registry$users[[hash]]
    
    # Count sessions across all apps
    total_sessions <- sum(sapply(user$apps, length))
    apps_used <- paste(names(user$apps), collapse = ", ")
    
    data.frame(
      email = user$email,
      email_hash = hash,
      registered = user$created,
      total_sessions = total_sessions,
      apps_used = apps_used,
      stringsAsFactors = FALSE
    )
  })
  
  do.call(rbind, summary_list)
}

#' Get detailed sessions table for all users
#' @param base_path Base path for sessions storage
#' @return Data frame with all sessions
get_all_sessions <- function(base_path) {
  registry <- get_registry(base_path)
  
  if (length(registry$users) == 0) {
    return(data.frame(
      email = character(),
      app_name = character(),
      session_id = character(),
      file_name = character(),
      created = character(),
      updated = character(),
      samples = integer(),
      has_pca = logical(),
      has_annotations = logical(),
      status = character(),
      stringsAsFactors = FALSE
    ))
  }
  
  # Build sessions table from registry
  sessions_list <- list()
  
  for (hash in names(registry$users)) {
    user <- registry$users[[hash]]
    
    for (app_name in names(user$apps)) {
      for (sess in user$apps[[app_name]]) {
        sessions_list[[length(sessions_list) + 1]] <- data.frame(
          email = user$email,
          app_name = app_name,
          session_id = sess$session_id,
          file_name = sess$file_name %||% NA,
          created = sess$created %||% NA,
          updated = sess$updated %||% NA,
          samples = sess$samples %||% NA,
          has_pca = sess$has_pca %||% FALSE,
          has_annotations = sess$has_annotations %||% FALSE,
          status = sess$status %||% NA,
          stringsAsFactors = FALSE
        )
      }
    }
  }
  
  if (length(sessions_list) == 0) {
    return(data.frame(
      email = character(),
      app_name = character(),
      session_id = character(),
      file_name = character(),
      created = character(),
      updated = character(),
      samples = integer(),
      has_pca = logical(),
      has_annotations = logical(),
      status = character(),
      stringsAsFactors = FALSE
    ))
  }
  
  do.call(rbind, sessions_list)
}

# =============================================================================
# CROSS-APP LINKING HELPERS
# =============================================================================

#' Create a "Send to App" button
#' @param id Unique button ID
#' @param target_app Target app name
#' @param session_id Session ID to send
#' @param base_url Base URL for the target app
#' @return Action button with link
sendToAppButton <- function(id, target_app, session_id, base_url = NULL) {
  # Format app name for display
  app_display <- gsub("-", " ", target_app)
  app_display <- tools::toTitleCase(app_display)
  
  if (!is.null(base_url)) {
    # External link with session ID as parameter
    url <- paste0(base_url, "?session=", session_id)
    tags$a(
      href = url,
      target = "_blank",
      class = "btn btn-primary btn-sm",
      icon("arrow-right"), 
      paste("Open in", app_display)
    )
  } else {
    # Just show session ID for manual entry
    actionButton(
      id,
      label = tagList(icon("arrow-right"), paste("Send to", app_display)),
      class = "btn-primary btn-sm"
    )
  }
}

#' Parse session ID from URL query
#' @param session Shiny session object
#' @return Session ID string or NULL
getSessionFromURL <- function(session) {
  query <- parseQueryString(session$clientData$url_search)
  query$session
}
