# RNA-Seq Pipeline - Deployment Guide
## Directory Structure & File Organization

---

## 📁 Recommended Directory Structure

```
/srv/shiny-server/                    # Your Shiny Server root (or wherever you deploy)
│
├── shared/                           # SHARED RESOURCES (used by all apps)
│   │
│   ├── module_user_sessions.R        # User & session management module
│   ├── module_email_notifications.R  # Email notifications module
│   ├── admin_utils.R                 # Admin CLI tools (optional)
│   ├── DESIGN_SYSTEM.md              # UI/UX guidelines (reference only)
│   │
│   └── sessions/                     # SESSION STORAGE (created automatically)
│       ├── registry.json             # Central user/session registry
│       ├── session_activity_log.csv  # Activity log
│       └── users/                    # User data directories
│           ├── a3f8c2d1e4b5/         # Hashed email directory
│           │   └── qc-checker/
│           │       ├── qc_20250210_abc123.rds
│           │       └── qc_20250210_def456.rds
│           └── b7e9f1a2c3d4/
│               ├── qc-checker/
│               │   └── qc_20250211_xyz789.rds
│               └── deg-explorer/
│                   └── deg_20250211_uvw123.rds
│
├── qc-checker/                       # QC-CHECKER APP
│   ├── app.R                         # Main application (4,700+ lines)
│   ├── module_email_notifications.R  # Local copy (or symlink to shared)
│   ├── Example_Data.RData            # Example dataset
│   └── www/                          # Static assets (if any)
│       └── styles.css
│
└── deg-explorer/                     # DEG-EXPLORER APP (future)
    ├── app.R
    └── www/
```

---

## 🔧 Setup Instructions

### Step 1: Create the directory structure

```bash
# Navigate to your Shiny Server directory
cd /srv/shiny-server

# Create shared directory and subdirectories
mkdir -p shared/sessions/users

# Create app directories
mkdir -p qc-checker
mkdir -p deg-explorer

# Set permissions (Shiny needs to write to sessions)
chmod -R 775 shared/sessions
chown -R shiny:shiny shared/sessions
```

### Step 2: Copy files to their locations

```bash
# Copy shared modules
cp module_user_sessions.R /srv/shiny-server/shared/
cp module_email_notifications.R /srv/shiny-server/shared/
cp admin_utils.R /srv/shiny-server/shared/

# Copy QC-CheckeR app
cp app.R /srv/shiny-server/qc-checker/
cp module_email_notifications.R /srv/shiny-server/qc-checker/
cp Example_Data.RData /srv/shiny-server/qc-checker/
```

### Step 3: Configure environment variables (optional)

In your Shiny Server config or app's `.Renviron`:

```bash
# Path to shared modules (relative from app directory)
PIPELINE_SHARED_PATH=../shared

# Path to sessions storage (relative from app directory)  
PIPELINE_SESSIONS_PATH=../shared/sessions

# DEG-Explorer URL (for cross-app linking)
DEG_EXPLORER_URL=../deg-explorer/
```

---

## 📄 File Descriptions

### Shared Directory (`/shared/`)

| File | Size | Description |
|------|------|-------------|
| `module_user_sessions.R` | ~1,000 lines | Handles user authentication (email), session save/load, registry management, activity logging |
| `module_email_notifications.R` | ~800 lines | Email notification system with batching |
| `admin_utils.R` | ~240 lines | Command-line tools for viewing users, sessions, activity |
| `DESIGN_SYSTEM.md` | ~440 lines | UI/UX guidelines for consistent styling |

### QC-CheckeR App (`/qc-checker/`)

| File | Size | Description |
|------|------|-------------|
| `app.R` | ~4,700 lines | Main Shiny application |
| `module_email_notifications.R` | ~800 lines | Local copy of email module |
| `Example_Data.RData` | varies | Sample dataset for testing |

---

## 🔗 How Apps Reference Shared Resources

In `app.R`, at the top:

```r
# =================================================
# SOURCE THE REUSABLE MODULES
# =================================================
# Get path from environment variable, default to ../shared
SHARED_PATH <- Sys.getenv("PIPELINE_SHARED_PATH", "../shared")

# Source the shared user sessions module
source(file.path(SHARED_PATH, "module_user_sessions.R"))

# Email module can be local or shared
source("module_email_notifications.R")

# Session storage path
SESSIONS_BASE_PATH <- Sys.getenv("PIPELINE_SESSIONS_PATH", "../shared/sessions")
APP_NAME <- "qc-checker"
```

---

## 🗂️ What Gets Created Automatically

When users interact with the app, these are created automatically:

```
shared/sessions/
├── registry.json              # Created on first user registration
├── session_activity_log.csv   # Created on first save/load
└── users/
    └── {email_hash}/          # Created when user enters email
        └── qc-checker/        # Created when user saves first session
            └── {session_id}.rds
```

---

## 📋 Quick Reference: What Goes Where

| File | Location | Notes |
|------|----------|-------|
| `module_user_sessions.R` | `/shared/` | Required - shared by all apps |
| `module_email_notifications.R` | `/shared/` AND `/qc-checker/` | Keep copy in app for now |
| `admin_utils.R` | `/shared/` | Optional - for admin use |
| `app.R` | `/qc-checker/` | Main QC-CheckeR app |
| `Example_Data.RData` | `/qc-checker/` | Example dataset |
| `registry.json` | `/shared/sessions/` | Auto-created |
| `*.rds` session files | `/shared/sessions/users/.../` | Auto-created |

---

## 🚀 Verification Checklist

After deployment, verify:

- [ ] `/shared/` directory exists and is readable
- [ ] `/shared/sessions/` exists and is writable by Shiny
- [ ] `/shared/module_user_sessions.R` exists
- [ ] `/qc-checker/app.R` exists
- [ ] `/qc-checker/Example_Data.RData` exists
- [ ] App loads without "file not found" errors
- [ ] User can enter email and see "Valid email" message
- [ ] User can upload data and click "Save Now"
- [ ] Session appears in "Load Existing Session" dropdown
- [ ] `/shared/sessions/registry.json` was created
- [ ] `/shared/sessions/session_activity_log.csv` was created

---

## 🔧 Troubleshooting

### "Cannot find module_user_sessions.R"
- Check `SHARED_PATH` is correct
- Verify file exists: `ls -la /srv/shiny-server/shared/`

### "Error saving session: permission denied"
- Check sessions directory is writable: `chmod -R 775 shared/sessions`
- Check ownership: `chown -R shiny:shiny shared/sessions`

### Sessions not appearing in dropdown
- Check `registry.json` exists and is valid JSON
- Check session `.rds` file was created in user's directory

---

## 📦 Files You Have (from this session)

These are the files I created that you need to deploy:

```
/mnt/user-data/outputs/
├── shared/
│   ├── module_user_sessions.R      ← COPY TO: /srv/shiny-server/shared/
│   ├── module_email_notifications.R ← COPY TO: /srv/shiny-server/shared/
│   ├── admin_utils.R               ← COPY TO: /srv/shiny-server/shared/
│   └── DESIGN_SYSTEM.md            ← Reference only
│
└── qc-checker/
    └── v2.1.0/
        ├── app.R                   ← COPY TO: /srv/shiny-server/qc-checker/
        └── module_email_notifications.R ← COPY TO: /srv/shiny-server/qc-checker/
```

You also need your existing `Example_Data.RData` in the qc-checker directory.

---

## 📧 Email Notifications Setup

Email credentials are stored in a `.credentials/` folder inside the app directory.
This works well in Docker and keeps credentials with the app.

### Step 1: Create credentials (run once)

```bash
cd /srv/shiny-server/qc-checker
Rscript setup_email.R
```

Or in R console:
```r
setwd("/srv/shiny-server/qc-checker")
source("setup_email.R")
```

### Step 2: Verify the file was created

```bash
ls -la /srv/shiny-server/qc-checker/.credentials/
# Should show: gmail_creds
```

### Directory structure after setup

```
qc-checker/
├── app.R
├── .credentials/           ← Created by setup_email.R
│   └── gmail_creds         ← Your email credentials (encrypted)
├── .gitignore              ← Already ignores .credentials/
├── setup_email.R
└── Example_Data.RData
```

### Docker considerations

In Docker, you have two options:

**Option A: Create credentials inside container (recommended for dev)**
```bash
docker exec -it qc-checker Rscript setup_email.R
```

**Option B: Mount credentials from host (recommended for prod)**
```bash
# Create credentials on host first
mkdir -p ./qc-checker/.credentials
# Then mount in docker-compose.yml:
# volumes:
#   - ./qc-checker/.credentials:/srv/shiny-server/qc-checker/.credentials:ro
```

### Troubleshooting

If emails aren't working, check the Shiny logs for:
```
EMAIL MODULE: Initialized
  Credentials path: .credentials/gmail_creds
  Full path: /srv/shiny-server/qc-checker/.credentials/gmail_creds
  Working directory: /srv/shiny-server/qc-checker
  Credentials exist: TRUE
```

If `Credentials exist: FALSE`, run `setup_email.R` again.
