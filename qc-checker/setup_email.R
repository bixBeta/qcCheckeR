#!/usr/bin/env Rscript
# =============================================================================
# EMAIL CREDENTIALS SETUP SCRIPT
# =============================================================================
# Run this script to set up email credentials for the QC-CheckeR app.
# Works in Docker or local environments.
#
# Usage (interactive - in R console):
#   source("setup_email.R")
#
# Usage (non-interactive - command line):
#   Rscript setup_email.R your.email@gmail.com
#
# Or with custom path:
#   Rscript setup_email.R your.email@gmail.com /path/to/creds
#
# =============================================================================

cat("\n")
cat("============================================================\n")
cat("         EMAIL CREDENTIALS SETUP FOR QC-CHECKER             \n")
cat("============================================================\n\n")

# Check for blastula
if (!requireNamespace("blastula", quietly = TRUE)) {
  cat("ERROR: Package 'blastula' is required.\n")
  cat("Install it with: install.packages('blastula')\n")
  quit(status = 1)
}

# Check if running interactively or via command line
args <- commandArgs(trailingOnly = TRUE)
is_interactive <- interactive()

# Get email from args or prompt
if (length(args) >= 1) {
  email <- args[1]
  cat("Email (from argument):", email, "\n")
} else if (is_interactive) {
  email <- readline(prompt = "Enter Gmail address: ")
} else {
  cat("============================================================\n")
  cat("                    NON-INTERACTIVE MODE                    \n")
  cat("============================================================\n\n")
  cat("Usage: Rscript setup_email.R <email> [creds_path]\n\n")
  cat("Examples:\n")
  cat("  Rscript setup_email.R myapp@gmail.com\n")
  cat("  Rscript setup_email.R myapp@gmail.com .credentials/gmail_creds\n\n")
  cat("Or run interactively in R:\n")
  cat("  R\n")
  cat("  > source('setup_email.R')\n\n")
  quit(status = 1)
}

# Get credentials path from args or use default
if (length(args) >= 2) {
  creds_file <- args[2]
} else {
  # Check for environment variable override
  creds_file <- Sys.getenv("EMAIL_CREDS_FILE", "")
  if (!nzchar(creds_file)) {
    creds_file <- ".credentials/gmail_creds"
  }
}

creds_dir <- dirname(creds_file)

cat("Current working directory:", getwd(), "\n")
cat("Credentials will be saved to:", creds_file, "\n")
cat("Full path:", normalizePath(creds_file, mustWork = FALSE), "\n\n")

# Create directory if needed
if (!dir.exists(creds_dir)) {
  cat("Creating directory:", creds_dir, "\n")
  dir.create(creds_dir, recursive = TRUE, mode = "0755")
}

# Check if credentials already exist
if (file.exists(creds_file)) {
  cat("\n⚠️  Credentials file already exists!\n")
  cat("   ", normalizePath(creds_file), "\n\n")
  
  if (is_interactive) {
    overwrite <- readline(prompt = "Overwrite existing credentials? (y/n): ")
    if (tolower(overwrite) != "y") {
      cat("Setup cancelled. Existing credentials preserved.\n")
      quit(status = 0)
    }
  } else {
    cat("Use --force or delete existing file to overwrite.\n")
    cat("Existing credentials preserved.\n")
    quit(status = 0)
  }
  cat("\n")
}

# Validate email
if (!nzchar(email)) {
  cat("ERROR: Email address is required.\n")
  quit(status = 1)
}

if (!grepl("@", email)) {
  cat("ERROR: Invalid email format.\n")
  quit(status = 1)
}

if (!grepl("@gmail\\.com$", email, ignore.case = TRUE)) {
  cat("\n⚠️  Warning: This doesn't appear to be a Gmail address.\n")
  cat("   SMTP settings are configured for Gmail.\n\n")
}

# Create credentials
cat("Creating credentials file...\n")
cat("You will be prompted for your App Password.\n\n")

tryCatch({
  blastula::create_smtp_creds_file(
    file = creds_file,
    user = email,
    host = "smtp.gmail.com",
    port = 465,
    use_ssl = TRUE
  )
  
  # Set permissions (Unix/Mac/Linux)
  tryCatch({
    Sys.chmod(creds_file, mode = "0600")
    Sys.chmod(creds_dir, mode = "0700")
    cat("✅ Secure file permissions set\n")
  }, error = function(e) {
    cat("⚠️  Could not set file permissions (may be Windows)\n")
  })
  
  cat("\n")
  cat("============================================================\n")
  cat("                    SETUP COMPLETE!                         \n")
  cat("============================================================\n\n")
  cat("✅ Credentials file created:\n")
  cat("   ", normalizePath(creds_file), "\n\n")
  cat("📧 Email address: ", email, "\n\n")
  
  cat("IMPORTANT - Add to .gitignore:\n")
  cat("   .credentials/\n\n")
  
}, error = function(e) {
  cat("\n❌ ERROR creating credentials:\n")
  cat("   ", e$message, "\n\n")
  
  cat("Troubleshooting:\n")
  cat("1. Check directory permissions: ", creds_dir, "\n")
  cat("2. Check if blastula is installed correctly\n")
  cat("3. Try running with sudo if permission denied\n")
  quit(status = 1)
})

# Offer to test (only in interactive mode)
if (is_interactive) {
  cat("------------------------------------------------------------\n")
  test <- readline(prompt = "Send a test email now? (y/n): ")
  
  if (tolower(test) == "y") {
    test_to <- readline(prompt = "Send test to (email address): ")
    
    if (nzchar(test_to)) {
      cat("\nSending test email...\n")
      
      tryCatch({
        test_email <- blastula::compose_email(
          body = blastula::md(paste0(
            "## ✅ Email Configuration Test\n\n",
            "This is a test email from **QC-CheckeR**.\n\n",
            "If you're seeing this, email notifications are working!\n\n",
            "---\n\n",
            "*Sent at: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"), "*"
          ))
        )
        
        blastula::smtp_send(
          email = test_email,
          from = email,
          to = test_to,
          subject = "QC-CheckeR Email Test",
          credentials = blastula::creds_file(creds_file)
        )
        
        cat("\n✅ Test email sent successfully!\n")
        cat("   Check inbox of:", test_to, "\n")
        
      }, error = function(e) {
        cat("\n❌ Failed to send test email:\n")
        cat("   ", e$message, "\n\n")
        cat("Common issues:\n")
        cat("1. Wrong App Password (must be 16 chars, no spaces)\n")
        cat("2. 2-factor auth not enabled on Gmail\n")
        cat("3. 'Less secure app access' blocked (use App Password instead)\n")
        cat("4. Network/firewall blocking SMTP port 465\n")
      })
    }
  }
}

cat("\n============================================================\n")
cat("                       ALL DONE!                             \n")
cat("============================================================\n\n")
cat("Your QC-CheckeR app should now be able to send emails.\n")
cat("Restart the app if it's currently running.\n\n")
