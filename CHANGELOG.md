# QC-CheckeR Changelog

## Project Reference
When starting a new conversation, mention: **"QC-CheckeR project"** to help Claude recall this project context.

---

## [v1.0.1] - 2025-12-17

### Bug Fixes

1. **Fixed duplicate HTML attribute (app.R:489)**
   - **Issue**: `rel="stylesheet"` was duplicated in the Google Fonts link tag
   - **Impact**: HTML validation error, potential rendering issues
   - **Fix**: Removed duplicate `rel` attribute

2. **Fixed count data type for DESeq2 compatibility (app.R:1410-1411)**
   - **Issue**: Count matrix was being rounded to 2 decimal places
   - **Impact**: DESeq2 requires integer counts; decimal values could cause PCA analysis failures
   - **Fix**: Changed `round(.x, 2)` to `as.integer(round(.x))` to ensure integer counts

3. **Fixed module server re-initialization issue (app.R:1454-1477)**
   - **Issue**: `editableTableServer`, `summaryServer`, and `linkedTableServer` were being called inside `renderUI`
   - **Impact**: Every UI re-render caused module servers to re-initialize, creating orphan observers, losing state, and potential memory leaks
   - **Fix**: Moved all module server initializations outside `renderUI` to server level; `renderUI` now only returns UI components

4. **Fixed reset function counts synchronization (app.R:382)**
   - **Issue**: Reset used `data_rv$orig_counts` instead of `counts_rv$orig_counts`
   - **Impact**: Inconsistent state between data sources after reset
   - **Fix**: Changed to use `counts_rv$orig_counts` for consistency with the reactive values structure

### UI/UX Improvements

5. **Updated PCA Plot defaults**
   - **Background color**: Changed default from "Light Gray" to "Light Yellow" (#fffef0)
   - **Show Grid**: Now checked (TRUE) by default
   - **Point Size slider**: Changed range from 3-20 to 10-50, default from 10 to 20
   - Updated in: UI inputs, reset handler, render fallbacks, download handler, session restore

### New Features

6. **Added PCA data export buttons**
   - **Export Eigenvalues**: Downloads CSV with PC number, eigenvalue, standard deviation, variance %, cumulative variance %
   - **Export PCA Scores**: Downloads CSV with PCA scores (all PCs) joined with current metadata
   - Both buttons appear in the PCA plot customization sidebar
   - Exports use current edited metadata (not original)

7. **Fixed "Start New Session" to fully reset app state**
   - **Issue**: Clicking "Start New Session" only created a new session ID but kept all data
   - **Fix**: Now shows confirmation dialog, then resets:
     - All data (metadata, counts, original copies)
     - PCA results and parameters
     - Plot customization options
     - File input field
   - Creates new session ID only after full reset
   - Prevents accidental data loss with confirmation modal

8. **Fixed PCA not reflecting metadata changes when re-run**
   - **Issue**: After editing metadata (deleting samples, changing groups), re-running PCA would not reflect the changes
   - **Root cause**: The reactive chain between the editable table module and PCA handlers was broken due to improper module output access
   - **Fix**: 
     - Restructured module initialization to properly expose `current_data` reactiveVal
     - Created `get_current_metadata()` helper reactive that directly accesses the module's data function
     - This creates a proper reactive dependency so PCA automatically uses edited data
     - Updated all metadata consumers to use the helper: PCA computation, PCA plot coloring, color dropdown, exports, email summaries, global save
   - **Result**: Metadata edits now properly flow through to PCA analysis and visualization

9. **Simplified PCA color system - 3-Color Gradient only**
   - **Removed**: Preset palette option (Set2, Viridis, etc.) - no longer needed
   - **Default**: Three color pickers using `colourpicker::colourInput` with `palette = "square"`
   - **Colors show correctly**: Pickers display actual colors (#636EFA blue, #EF553B red, #00CC96 green) not black
   - **Flexible selection**: Full color picker with square palette for any color choice
   - **Why gradient only**: `colorRampPalette()` interpolation scales to any number of groups (3, 12, 50+)
   - **Cleaner UI**: No radio buttons or conditional panels - just three labeled color pickers (Start, Mid, End)
   - Session save/restore backward compatible with old sessions

10. **Redesigned Count Distribution Plots (SARTools-style)**
    - **Changed from Plotly to static ggplot2** for much faster rendering with many samples
    - **Violin plots with box plot overlay** - shows distribution shape + summary statistics
      - Violin: `trim = FALSE`, `scale = "width"`, `alpha = 0.7`
      - Box: `width = 0.15`, white fill, shows median/IQR/whiskers
      - No individual points plotted (keeps rendering fast with 10k+ genes)
    - **Box plots colored by GROUP** (not by sample) - matches SARTools QC style
    - **Tabs renamed**: "Raw Counts", "VST Counts", "Comparison (Raw vs VST)", "Count Table"
    - **Dynamic color palette**: 12 base colors that scale to any number of groups:
      - Uses direct colors for ≤12 groups
      - Uses `colorRampPalette()` interpolation for >12 groups
      - Base palette: Light blue, Orange, Teal, Pink, Lime, Yellow, Tan, Gray, Periwinkle, Red, Blue, Green
    - **Lighter background**: Custom `theme_qc()` with `#fafafa` panel background (vs darker gray)
    - **Comparison tab**: Side-by-side faceted view of Raw vs VST (like SARTools countsBoxplots.png)
    - **Samples ordered by group** then alphabetically for easy visual comparison
    - **Performance**: Static plots render instantly vs slow Plotly with 20+ samples
    - **Dependencies added**: tidyr, tibble (for data reshaping)

11. **Comprehensive Help Tab Overhaul**
    - **Session ID Warning Card**: Prominent yellow warning card at top reminding users to save their Session ID before closing
    - **About Section**: Overview of what QC-CheckeR does
    - **Quick Start Guide**: Numbered step-by-step instructions for new users
    - **Data Requirements**: Clear specification of required `.RData` contents (counts matrix, metadata with label/group columns)
    - **Features Overview**: 4-column grid layout covering:
      - Metadata Editing capabilities
      - Visualization features
      - PCA Analysis options
      - Session Management (with **bolded** "Use your Session ID to resume work later")
    - **Tips Section**: Best practices for outlier detection, batch effects, large datasets, and sample removal

---

## [v1.0.0] - 2025-12-10 (Approximate)

### Initial Release
- Editable metadata table with row selection and editing modal
- Linked count matrix that syncs with metadata changes
- Session storage with debounced auto-save
- Email notifications with batched change summaries
- DESeq2 PCA analysis with VST transformation
- Interactive PCA plots with customization options
- Session management (save/load by session ID)
- Undo functionality for deleted rows
- Download buttons for metadata and counts CSV

### Features
- **Modules**: `module_session_storage.R`, `module_email_notifications.R`
- **PCA**: Variance stabilizing transformation, scree plots, customizable visualizations
- **UI**: Bootstrap 5 styling, responsive layout, progress indicators

---

## File Structure
```
qc-checker/
├── CHANGELOG.md
├── PROJECT_REFERENCE.md
├── v1.0.0/              # Original version (backup)
│   ├── app.R
│   ├── module_session_storage.R
│   └── module_email_notifications.R
└── v1.0.1/              # Bug fixes
    ├── app.R
    ├── module_session_storage.R
    └── module_email_notifications.R
```

---

## Future Considerations
- Consider adding input validation for email addresses
- Add export options for PCA loadings
- Consider caching VST results for faster re-rendering
- Add batch mode for processing multiple datasets
