# QC-CheckeR Project Reference

## Quick Reference Phrase
**To continue this project in a new chat, say:**
> "Let's continue working on the QC-CheckeR project"

or

> "QC-CheckeR Shiny app - checking in on the RNA-Seq QC tool"

---

## Project Summary

**QC-CheckeR** is a Shiny application for RNA-Seq quality control and exploratory data analysis.

### Core Features
1. **Metadata Editor**: Interactive table editing with row selection modals
2. **Count Matrix Viewer**: Live-synced count data display with violin/box plots
3. **Session Management**: Persistent sessions with unique IDs, auto-save
4. **Email Notifications**: Batched change summaries via Gmail
5. **DESeq2 PCA Analysis**: VST transformation with interactive visualizations
6. **Example Data Loading**: One-click example dataset for testing

### File Architecture
```
qc-checker/
├── v1.0.1/
│   ├── app.R                         # Main application
│   ├── module_session_storage.R      # Session persistence
│   ├── module_email_notifications.R  # Email batching
│   └── Example_Data.RData            # Example dataset (server-side)
├── test_data/
│   ├── create_test_rdata.R           # Script to generate test data
│   ├── metadata.RData / counts.RData # Separate data files
│   └── README.md                     # Test data documentation
├── CHANGELOG.md                      # Version history
├── REACTIVES_QUICK_REFERENCE.md      # Reactive flow documentation
├── reactives_reference.tsv           # TSV reference for all reactives
└── PROJECT_REFERENCE.md              # This file
```

### Key Dependencies
- shiny, bslib, reactable, dplyr, tidyr, tibble
- DESeq2, matrixStats, SummarizedExperiment (Bioconductor)
- plotly, ggplot2, viridisLite, colourpicker
- blastula (email), glue, shinyjs, shinyjqui

---

## Current Version: v1.0.1

### Recent Changes (Items #9-12)

**#9: Simplified PCA Color System**
- Removed preset palettes, now uses 3-color gradient picker
- Uses `colourpicker::colourInput` with "square" palette
- Default: Blue (#636EFA) → Red (#EF553B) → Green (#00CC96)

**#10: SARTools-Style Distribution Plots**
- Static ggplot2 violin + box plots (much faster than Plotly)
- Colored by GROUP (not sample)
- Dynamic color palette: 12 base colors, scales to any number
- Lighter background (#fafafa)
- Tabs: Raw Counts, VST Counts, Comparison, Count Table

**#11: Comprehensive Help Tab**
- Session ID warning card (prominent yellow)
- Quick start guide, data requirements, features overview
- Tips for outlier detection and batch effects

**#12: Load Example Data Button**
- One-click button on Start page
- Loads Example_Data.RData from server
- 20 samples, 4 groups, 10k genes, 1 outlier

**Other Improvements**
- Consistent button styling (btn-outline-primary)
- PCA plot margins for better export titles
- Proper plot title positioning

---

## Technical Notes

### Session Storage
- Sessions saved as `.rds` files in `saved_sessions/` directory
- Session ID format: `session_YYYYMMDD_HHMMSS_XXXX`
- Auto-save: 3 seconds after last edit (debounced)
- Manual save via "Save Now" button

### Email Configuration
- Uses Gmail SMTP via blastula package
- Credentials stored at path specified in `email_config$creds_file`
- Batch delay: 120 seconds by default

### PCA Implementation
- Uses DESeq2's `varianceStabilizingTransformation()`
- Configurable: blind mode, fit type (parametric/local/mean), top N genes
- Stores VST object in pca_rv for violin plots
- Exports: eigenvalues CSV, PCA scores with metadata CSV

### Color Systems
- **PCA Plot**: 3-color gradient, user-selectable
- **Violin/Box Plots**: 12 base SARTools-style colors
  - ≤12 groups: direct assignment
  - >12 groups: interpolation via colorRampPalette

### Reactive Data Flow
```
Upload/Example → data_rv → counts_rv → linkedTableServer → violin plots
                    ↓
              editableTableServer → get_current_metadata() → PCA
```

See `REACTIVES_QUICK_REFERENCE.md` for detailed flow diagram.

---

## Setting Up Example Data

1. Run `create_test_rdata.R` in R (in test_data folder)
2. Copy `Example_Data.RData` to v1.0.1 directory
3. The "Load Example Data" button will then work

---

## Common Tasks

### To report a bug:
"I found a bug in QC-CheckeR: [describe the issue]"

### To add a feature:
"Can we add [feature] to the QC-CheckeR app?"

### To review changes:
"Show me what we changed in QC-CheckeR recently"

### To get the latest code:
"Give me the current version of the QC-CheckeR app files"

### To update documentation:
"Update the reactives reference for QC-CheckeR"
