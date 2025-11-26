# QC-CheckeR 🧬

A Shiny application for quality control and exploratory analysis of RNA-Seq data. QC-CheckeR provides an interactive interface for metadata editing, sample management, and PCA visualization using DESeq2's variance stabilizing transformation.

![R](https://img.shields.io/badge/R-%3E%3D4.0-blue)
![Shiny](https://img.shields.io/badge/Shiny-1.7+-brightgreen)
![License](https://img.shields.io/badge/license-MIT-green)

## ✨ Features

### 📊 Data Management
- **Interactive Metadata Editing**: Click any row to edit sample metadata in a modal dialog
- **Dynamic Group Management**: Add new experimental groups on-the-fly
- **Sample Deletion with Undo**: Delete samples with full undo capability
- **Linked Tables**: Metadata and count matrix stay synchronized automatically
- **CSV Export**: Download edited metadata and count matrices

### 🔬 PCA Analysis (DESeq2)
- **Variance Stabilizing Transformation (VST)**: Industry-standard normalization for RNA-Seq
- **Customizable Parameters**:
  - Blind transformation option
  - Dispersion fit type (parametric, local, mean)
  - Configurable number of top variable genes
- **Interactive Visualization**:
  - Select any PC for X/Y axes
  - Color by any metadata column
  - Scree plot with cumulative variance
  - Full plot customization (colors, size, opacity, palettes)
- **Export Options**: Download plots as PNG or interactive HTML

### 💾 Session Management
- **Persistent Sessions**: Work is saved with a unique session ID
- **Resume Anytime**: Return to your analysis using your session ID
- **Manual Save Control**: Click "Save Now" to persist all changes including PCA results
- **Full State Preservation**: Metadata, counts, DESeq2 objects, and PCA results are all saved

### 📧 Email Notifications
- **Change Tracking**: All edits are tracked and batched
- **Email Summaries**: Receive periodic summaries of changes made to your data
- **Configurable Intervals**: Batch notifications every 2 minutes (configurable)

## 📋 Requirements

### R Packages

```r
# CRAN packages
install.packages(c(
  "shiny",
  "bslib",
  "jsonlite",
  "reactable",
  "dplyr",
  "shinycssloaders",
  "blastula",
  "glue",
  "shinyjs",
  "plotly",
  "ggplot2",
  "matrixStats",
  "shinyjqui",
  "viridisLite",
  "webshot2"
))

# Bioconductor packages
if (!requireNamespace("BiocManager", quietly = TRUE))
    install.packages("BiocManager")
BiocManager::install("DESeq2")
```

### Required Module Files

The app requires two external module files:
- `module_session_storage.R` - Handles session persistence
- `module_email_notifications.R` - Manages email notification system

## 📁 Input Data Format

Upload an `.RData` file containing two objects:

### `metadata` (data.frame)
| Column | Type | Description |
|--------|------|-------------|
| `label` | character | **Required.** Unique sample identifier matching count matrix columns |
| `group` | character | **Required.** Experimental group/condition |
| ... | any | Additional metadata columns (optional) |

### `counts` (data.frame or matrix)
- Rows: Features (genes/transcripts)
- Columns: Sample names matching `metadata$label`
- Values: Raw read counts (integers)

### Example

```r
# Create example data
metadata <- data.frame(
  label = c("Sample1", "Sample2", "Sample3", "Sample4"),
  group = c("Control", "Control", "Treatment", "Treatment"),
  batch = c("A", "B", "A", "B")
)

counts <- data.frame(
  Sample1 = c(100, 200, 50),
  Sample2 = c(120, 180, 45),
  Sample3 = c(80, 250, 100),
  Sample4 = c(90, 230, 95),
  row.names = c("Gene1", "Gene2", "Gene3")
)

save(metadata, counts, file = "my_data.RData")
```

## 🚀 Getting Started

### Local Installation

1. Clone the repository:
```bash
git clone https://github.com/yourusername/qc-checker.git
cd qc-checker
```

2. Install dependencies (see Requirements above)

3. Run the app:
```r
shiny::runApp()
```

### Shiny Server Deployment

Place the following files in your Shiny Server app directory:
- `app.R`
- `module_session_storage.R`
- `module_email_notifications.R`

Ensure the server has write permissions for:
- `saved_sessions/` directory (for session persistence)
- `.credentials/` directory (for email functionality)

## 📖 Usage Guide

### 1. Start Tab
- **Load Existing Session**: Enter a previously saved session ID to resume work
- **Start New Session**: Begin fresh with a new upload

### 2. Upload/Rename Tab
- Upload your `.RData` file
- View and edit the metadata table by clicking rows
- Changes to the `label` column automatically update count matrix headers
- Use "Reset to Original" to revert all changes
- Download edited CSV files

### 3. PCA Analysis Tab

#### Configure Parameters
| Parameter | Description | Default |
|-----------|-------------|---------|
| Blind to design | Ignore sample groups during VST | TRUE |
| Fit type | Dispersion estimation method | parametric |
| Top variable genes | Number of genes for PCA | 500 |

#### Customize Plot
- **Title**: Custom plot title
- **Point Size/Opacity**: Adjust marker appearance
- **Color Palette**: Choose from ColorBrewer and Viridis scales
- **Grid/Legend**: Toggle display options
- **Background**: Select plot background color

### 4. Saving Your Work
Click the **"Save Now"** button in the navbar to persist:
- Current metadata edits
- Count matrix state
- DESeq2 dataset object (DDS)
- VST-transformed counts
- PCA results and plot settings

## 📂 Project Structure

```
qc-checker/
├── app.R                          # Main application
├── module_session_storage.R       # Session management module
├── module_email_notifications.R   # Email notification module
├── saved_sessions/                # Session data storage
│   └── *.rds                      # Individual session files
├── .credentials/                  # Email credentials (not in repo)
│   └── gmail_creds
└── README.md
```

## ⚙️ Configuration

### Session Storage
```r
storage_config = list(
  file_storage_enabled = TRUE,
  storage_dir = "saved_sessions",
  db_storage_enabled = FALSE,
  cleanup_days = 7  # Auto-cleanup old sessions
)
```

### Email Notifications
```r
email_config = list(
  enabled = TRUE,
  sender_email = "your-email@gmail.com",
  creds_file = "/path/to/.credentials/gmail_creds",
  batch_delay_seconds = 120,
  min_changes_for_email = 1
)
```

## 🔒 Security Notes

- Email credentials should never be committed to version control
- Add `.credentials/` to your `.gitignore`
- Session files may contain sensitive data; secure the `saved_sessions/` directory appropriately

## 🐛 Troubleshooting

### PCA Fails to Run
- Ensure count matrix contains only non-negative integers
- Check that sample labels match between metadata and counts
- Verify you have at least 2 samples per group for meaningful PCA

### Session Won't Load
- Verify the session ID is correct (case-sensitive)
- Check that `saved_sessions/` directory exists and is readable
- Session files expire after the configured cleanup period

### Email Notifications Not Working
- Verify credentials file exists and is readable
- Check Gmail app-specific password is configured
- Review server logs for authentication errors

## 📄 License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

## 🤝 Contributing

Contributions are welcome! Please feel free to submit a Pull Request.

1. Fork the repository
2. Create your feature branch (`git checkout -b feature/AmazingFeature`)
3. Commit your changes (`git commit -m 'Add some AmazingFeature'`)
4. Push to the branch (`git push origin feature/AmazingFeature`)
5. Open a Pull Request

## 📬 Contact

For questions or support, please open an issue on GitHub.

---

**Made with ❤️ for the RNA-Seq community**