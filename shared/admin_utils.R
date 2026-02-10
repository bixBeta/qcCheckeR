# =============================================================================
# ADMIN UTILITIES
# =============================================================================
# Simple scripts to view user and session data from the command line or R console
#
# Usage:
#   source("admin_utils.R")
#   
#   # View all users
#   view_users()
#   
#   # View all sessions
#   view_sessions()
#   
#   # View activity log
#   view_activity_log()
#   
#   # Export to CSV
#   export_admin_data()
#
# =============================================================================

# Load the user sessions module for helper functions
source("module_user_sessions.R")

# Default path - adjust as needed
DEFAULT_SESSIONS_PATH <- "sessions"

#' View summary of all registered users
#' @param base_path Path to sessions directory
#' @return Data frame of users (also prints to console)
view_users <- function(base_path = DEFAULT_SESSIONS_PATH) {
  users <- get_users_summary(base_path)
  
  if (nrow(users) == 0) {
    cat("No users registered yet.\n")
    return(invisible(users))
  }
  
  cat("\n")
  cat("=======================================================\n")
  cat("                   REGISTERED USERS                     \n")
  cat("=======================================================\n")
  cat(sprintf("Total users: %d\n\n", nrow(users)))
  
  for (i in seq_len(nrow(users))) {
    cat(sprintf("[%d] %s\n", i, users$email[i]))
    cat(sprintf("    Hash: %s\n", users$email_hash[i]))
    cat(sprintf("    Registered: %s\n", users$registered[i]))
    cat(sprintf("    Sessions: %d\n", users$total_sessions[i]))
    cat(sprintf("    Apps: %s\n", users$apps_used[i]))
    cat("\n")
  }
  
  invisible(users)
}

#' View all sessions across all users
#' @param base_path Path to sessions directory
#' @return Data frame of sessions (also prints to console)
view_sessions <- function(base_path = DEFAULT_SESSIONS_PATH) {
  sessions <- get_all_sessions(base_path)
  
  if (nrow(sessions) == 0) {
    cat("No sessions found.\n")
    return(invisible(sessions))
  }
  
  cat("\n")
  cat("=======================================================\n")
  cat("                    ALL SESSIONS                        \n")
  cat("=======================================================\n")
  cat(sprintf("Total sessions: %d\n\n", nrow(sessions)))
  
  # Group by user
  for (email in unique(sessions$email)) {
    user_sessions <- sessions[sessions$email == email, ]
    cat(sprintf("User: %s (%d sessions)\n", email, nrow(user_sessions)))
    cat(paste(rep("-", 55), collapse = ""), "\n")
    
    for (i in seq_len(nrow(user_sessions))) {
      s <- user_sessions[i, ]
      pca_badge <- if (isTRUE(s$has_pca)) "[PCA]" else ""
      anno_badge <- if (isTRUE(s$has_annotations)) "[Anno]" else ""
      
      cat(sprintf("  %s %s %s\n", s$session_id, pca_badge, anno_badge))
      cat(sprintf("    File: %s | Samples: %s | App: %s\n", 
                  s$file_name %||% "N/A", 
                  s$samples %||% "N/A",
                  s$app_name))
      cat(sprintf("    Updated: %s\n", s$updated %||% s$created %||% "N/A"))
      cat("\n")
    }
    cat("\n")
  }
  
  invisible(sessions)
}

#' View activity log
#' @param base_path Path to sessions directory
#' @param n Number of recent entries to show (NULL for all)
#' @return Data frame of activity (also prints to console)
view_activity_log <- function(base_path = DEFAULT_SESSIONS_PATH, n = 50) {
  log <- read_session_log(base_path)
  
  if (nrow(log) == 0) {
    cat("No activity logged yet.\n")
    return(invisible(log))
  }
  
  # Sort by timestamp descending
  log <- log[order(log$timestamp, decreasing = TRUE), ]
  
  # Limit to n entries if specified
  if (!is.null(n) && nrow(log) > n) {
    log_display <- head(log, n)
    cat(sprintf("Showing %d of %d total entries\n\n", n, nrow(log)))
  } else {
    log_display <- log
  }
  
  cat("\n")
  cat("=======================================================\n")
  cat("                   ACTIVITY LOG                         \n")
  cat("=======================================================\n\n")
  
  for (i in seq_len(nrow(log_display))) {
    entry <- log_display[i, ]
    
    # Format action with color hint
    action_str <- toupper(entry$action)
    
    cat(sprintf("[%s] %s - %s\n", 
                entry$timestamp, 
                action_str,
                entry$email))
    cat(sprintf("  Session: %s\n", entry$session_id))
    cat(sprintf("  App: %s | File: %s | Samples: %s\n",
                entry$app_name,
                entry$file_name %||% "N/A",
                entry$samples %||% "N/A"))
    cat("\n")
  }
  
  invisible(log)
}

#' Export all admin data to CSV files
#' @param base_path Path to sessions directory
#' @param output_dir Directory to save CSV files
export_admin_data <- function(base_path = DEFAULT_SESSIONS_PATH, output_dir = ".") {
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  
  # Export users
  users <- get_users_summary(base_path)
  users_file <- file.path(output_dir, sprintf("users_export_%s.csv", timestamp))
  write.csv(users, users_file, row.names = FALSE)
  cat(sprintf("Users exported to: %s\n", users_file))
  
  # Export sessions
  sessions <- get_all_sessions(base_path)
  sessions_file <- file.path(output_dir, sprintf("sessions_export_%s.csv", timestamp))
  write.csv(sessions, sessions_file, row.names = FALSE)
  cat(sprintf("Sessions exported to: %s\n", sessions_file))
  
  # Export activity log
  log <- read_session_log(base_path)
  log_file <- file.path(output_dir, sprintf("activity_log_export_%s.csv", timestamp))
  write.csv(log, log_file, row.names = FALSE)
  cat(sprintf("Activity log exported to: %s\n", log_file))
  
  cat("\nExport complete!\n")
  
  invisible(list(
    users = users,
    sessions = sessions,
    activity = log
  ))
}

#' Quick stats summary
#' @param base_path Path to sessions directory
quick_stats <- function(base_path = DEFAULT_SESSIONS_PATH) {
  users <- get_users_summary(base_path)
  sessions <- get_all_sessions(base_path)
  log <- read_session_log(base_path)
  
  cat("\n")
  cat("=======================================================\n")
  cat("                   QUICK STATS                          \n")
  cat("=======================================================\n\n")
  
  cat(sprintf("Total users:        %d\n", nrow(users)))
  cat(sprintf("Total sessions:     %d\n", nrow(sessions)))
  cat(sprintf("Activity log rows:  %d\n", nrow(log)))
  
  if (nrow(sessions) > 0) {
    cat(sprintf("\nSessions with PCA:  %d (%.1f%%)\n", 
                sum(sessions$has_pca, na.rm = TRUE),
                100 * sum(sessions$has_pca, na.rm = TRUE) / nrow(sessions)))
    cat(sprintf("Sessions with Anno: %d (%.1f%%)\n", 
                sum(sessions$has_annotations, na.rm = TRUE),
                100 * sum(sessions$has_annotations, na.rm = TRUE) / nrow(sessions)))
  }
  
  if (nrow(log) > 0) {
    cat("\nRecent activity:\n")
    recent <- head(log[order(log$timestamp, decreasing = TRUE), ], 5)
    for (i in seq_len(nrow(recent))) {
      cat(sprintf("  %s: %s by %s\n", 
                  recent$timestamp[i], 
                  recent$action[i], 
                  recent$email[i]))
    }
  }
  
  cat("\n")
}

# =============================================================================
# INTERACTIVE MENU (for command line use)
# =============================================================================

admin_menu <- function(base_path = DEFAULT_SESSIONS_PATH) {
  cat("\n")
  cat("=======================================================\n")
  cat("           QC-CHECKER ADMIN UTILITIES                   \n")
  cat("=======================================================\n")
  cat("\nCommands:\n")
  cat("  1. view_users()        - List all registered users\n")
  cat("  2. view_sessions()     - List all sessions\n")
  cat("  3. view_activity_log() - View recent activity\n")
  cat("  4. quick_stats()       - Quick summary statistics\n")
  cat("  5. export_admin_data() - Export all data to CSV\n")
  cat("\nBase path:", base_path, "\n")
  cat("\n")
}

# Show menu when sourced
admin_menu()
