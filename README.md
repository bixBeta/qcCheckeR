# QC-CheckeR v2.1.0

A comprehensive RNA-Seq quality control Shiny application with email-based session management, interactive PCA analysis, and cross-app pipeline integration.

![Version](https://img.shields.io/badge/version-2.1.0-blue)
![R](https://img.shields.io/badge/R-%3E%3D4.0-blue)
![Shiny](https://img.shields.io/badge/Shiny-1.7+-green)
![License](https://img.shields.io/badge/license-MIT-green)

## 🎯 Features

### Core Functionality
- **Interactive Metadata Editing** — Click-to-edit sample labels and groups with undo support
- **Gene Annotation** — g:Profiler (online) or GTF file parsing for 7+ organisms
- **Count Visualization** — Violin plots with box plots and scatter points
- **PCA Analysis** — DESeq2 VST normalization with interactive Plotly plots
- **Expression Heatmaps** — Top loading genes with customizable clustering

### Session Management (v2.1.0)
- **Email-based sessions** — All your sessions linked to your email address
- **Manual save workflow** — Click 'Save Now' to save (button turns yellow when unsaved)
- **Unsaved changes warning** — Browser confirms before losing unsaved work
- **Activity logging** — All session activity tracked in CSV log
- **Cross-app pipeline** — Send QC'd data directly to DEG-Explorer

### Visualization Options
- Customizable point sizes, colors, and opacity
- 3-color gradient system for large group counts (40+)
- Multiple PC combinations (PC1-PC10+)
- Export plots as PNG, PDF, SVG, or interactive HTML

## 📦 Installation

### Prerequisites

```r
# Install Bioconductor
if (!requireNamespace("BiocManager", quietly = TRUE))
    install.packages("BiocManager")

# Install DESeq2
BiocManager::install("DESeq2")
```

### Required R Packages

The app will auto-install missing packages, but you can pre-install:

```r
install.packages(c(
  "shiny", "bslib", "jsonlite", "reactable", "dplyr", "tidyr", 
  "tibble", "rlang", "shinycssloaders", "blastula", "glue", 
  "shinyjs", "plotly", "ggplot2", "matrixStats", "shinyjqui", 
  "viridisLite", "colourpicker", "heatmaply", "DT", 
  "RColorBrewer", "gprofiler2", "digest"
))
```

## 🚀 Quick Start

### Option 1: Local Development

```bash
git clone https://github.com/yourusername/qc-checker.git
cd qc-checker
```

```r
# In R
shiny::runApp()
```

### Option 2: Docker

```bash
docker build -t qc-checker .
docker run -p 3838:3838 -v $(pwd)/shared:/srv/shiny-server/shared qc-checker
```

### Option 3: Shiny Server

Copy files to your Shiny Server directory:
```bash
cp -r qc-checker /srv/shiny-server/
cp -r shared /srv/shiny-server/
```

## 📁 Directory Structure

```
/srv/shiny-server/
│
├── shared/                           # Shared resources (required)
│   ├── module_user_sessions.R        # User & session management
│   ├── module_email_notifications.R  # Email notifications
│   ├── admin_utils.R                 # Admin CLI tools
│   └── sessions/                     # Session storage (auto-created)
│       ├── registry.json
│       ├── session_activity_log.csv
│       └── users/{email_hash}/qc-checker/*.rds
│
└── qc-checker/                       # Application
    ├── app.R                         # Main application
    ├── module_email_notifications.R  # Email module (local copy)
    ├── setup_email.R                 # Email credentials setup
    ├── Example_Data.RData            # Sample dataset
    ├── .credentials/                 # Email credentials (git-ignored)
    │   └── gmail_creds
    └── .gitignore
```

## 📊 Data Requirements

Your `.RData` file must contain:

```r
# counts: A matrix/data.frame with genes as rows, samples as columns
#         Row names = gene IDs, Column names = sample labels
counts <- matrix(...)

# metadata: A data.frame with sample information
#           Must have 'label' and 'group' columns
metadata <- data.frame(
  label = c("Sample1", "Sample2", ...),
  group = c("Control", "Treatment", ...),
  ...  # Additional columns optional
)

save(counts, metadata, file = "my_data.RData")
```

## 📧 Email Notifications Setup

Optional feature for receiving session change summaries.

### Step 1: Generate Gmail App Password

1. Go to [Google App Passwords](https://myaccount.google.com/apppasswords)
2. Enable 2-factor authentication if not already enabled
3. Generate an App Password for "Mail"
4. Copy the 16-character password

### Step 2: Run Setup Script

```bash
cd /srv/shiny-server/qc-checker
Rscript setup_email.R your.email@gmail.com
```

The script will prompt for your App Password and create `.credentials/gmail_creds`.

## 🔧 Configuration

### Environment Variables

| Variable | Default | Description |
|----------|---------|-------------|
| `PIPELINE_SHARED_PATH` | `../shared` | Path to shared modules |
| `PIPELINE_SESSIONS_PATH` | `../shared/sessions` | Session storage path |
| `DEG_EXPLORER_URL` | `../deg-explorer/` | URL for cross-app linking |
| `EMAIL_CREDS_FILE` | `.credentials/gmail_creds` | Email credentials path |

### Docker Compose Example

```yaml
version: '3.8'
services:
  qc-checker:
    build: .
    ports:
      - "3838:3838"
    volumes:
      - ./shared:/srv/shiny-server/shared
      - ./qc-checker/.credentials:/srv/shiny-server/qc-checker/.credentials:ro
    environment:
      - PIPELINE_SHARED_PATH=/srv/shiny-server/shared
      - PIPELINE_SESSIONS_PATH=/srv/shiny-server/shared/sessions
```

## 👩‍💼 Admin Tools

View user and session data from the command line:

```r
source("shared/admin_utils.R")

# View all users
view_users()

# View all sessions
view_sessions()

# View activity log
view_activity_log()

# Export to CSV
export_admin_data()

# Quick stats
quick_stats()
```

## 🔄 Workflow

```
┌─────────────────────────────────────────────────────────────┐
│                     QC-CheckeR Pipeline                      │
├─────────────────────────────────────────────────────────────┤
│                                                             │
│  1. Enter Email ──► 2. Upload Data ──► 3. Edit Metadata    │
│                            │                                │
│                            ▼                                │
│  6. Send to        5. Run PCA  ◄── 4. Annotate Genes       │
│     DEG-Explorer        │            (optional)             │
│         │               ▼                                   │
│         │          View Results                             │
│         │          • Violin plots                           │
│         │          • PCA scatter                            │
│         │          • Heatmaps                               │
│         │               │                                   │
│         ▼               ▼                                   │
│    DEG-Explorer    Export Data                              │
│    (Coming Soon)   • CSV files                              │
│                    • Plot images                            │
│                                                             │
└─────────────────────────────────────────────────────────────┘
```

## 📝 Changelog

### v2.1.0 (2025-02-10)
- ✨ Email-based user session management
- ✨ Unsaved changes warning with visual indicator
- ✨ Session activity logging to CSV
- ✨ Cross-app linking to DEG-Explorer
- ✨ Improved session loading with file name preservation
- 🔧 Fixed email credentials path for Docker compatibility
- 🔧 Manual save workflow (auto-save disabled)

### v2.0.0 (2025-02-09)
- ✨ Gene annotation via g:Profiler and GTF parsing
- ✨ Expression heatmaps with clustering
- ✨ Session storage with RDS files
- 🔧 Removed Python/gffutils dependency
- 🔧 Fixed PCA plot download in Docker (ggplot2 instead of webshot2)

## 🐛 Troubleshooting

### "Email credentials not found"
```bash
cd /srv/shiny-server/qc-checker
Rscript setup_email.R your.email@gmail.com
```

### "Session file not found"
- Check that `shared/sessions/` directory exists and is writable
- Verify `PIPELINE_SESSIONS_PATH` environment variable

### PCA fails with "Error in DESeq2"
- Ensure counts are raw integers (not normalized)
- Check for samples with all zeros
- Verify metadata labels match count column names

### Docker permission issues
```bash
chmod -R 775 shared/sessions
chown -R shiny:shiny shared/sessions
```

## 📄 License

MIT License - see [LICENSE](LICENSE) for details.

## 🤝 Contributing

1. Fork the repository
2. Create a feature branch (`git checkout -b feature/amazing-feature`)
3. Commit your changes (`git commit -m 'Add amazing feature'`)
4. Push to the branch (`git push origin feature/amazing-feature`)
5. Open a Pull Request

## 📬 Contact

For questions or issues, please open a GitHub issue.
