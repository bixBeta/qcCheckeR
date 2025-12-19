# QC-CheckeR Reactive Variables - Quick Reference

*Updated: v1.0.1*

## Data Flow Diagram

```
┌─────────────────────────────────────────────────────────────────────────────────┐
│                           USER ACTIONS                                           │
│                                                                                  │
│   [Load Session] [Load Example] [Upload File] [Start New]                        │
│         │              │              │              │                           │
│         └──────────────┴──────────────┴──────────────┘                           │
│                                  │                                               │
│                                  ▼                                               │
│  ┌─────────────────────────────────────────────────────────────────────────┐     │
│  │                         data_rv (Global)                                 │    │
│  │  ├── meta        ─────► Current metadata (editable)                      │    │
│  │  ├── orig_meta   ─────► Original metadata (for reset)                    │    │
│  │  ├── counts      ─────► Synced to counts_rv on init                      │    │
│  │  └── orig_counts ─────► Original counts (for reset)                      │    │
│  └─────────────────────────────────────────────────────────────────────────┘     │
│                                  │                                               │
│                    ┌─────────────┴─────────────┐                                 │
│                    ▼                           ▼                                 │
│  ┌─────────────────────────┐     ┌─────────────────────────────────────────┐     │
│  │    counts_rv (Global)   │     │      editableTableServer (Module)       │     │
│  │  ├── counts             │     │  ├── current_data (reactiveVal)         │     │
│  │  └── orig_counts        │     │  └── deleted_stack (reactiveVal)        │     │
│  └─────────────────────────┘     └─────────────────────────────────────────┘     │
│              │                                   │                               │
│              │                                   ▼                               │
│              │                         ┌──────────────────┐                      │
│              │                         │  meta_out$data   │ (Bridge)             │
│              │                         └──────────────────┘                      │
│              │                                   │                               │
│              │                                   ▼                               │
│              │                    ┌──────────────────────────┐                   │
│              │                    │ get_current_metadata()   │ (Helper)          │
│              │                    └──────────────────────────┘                   │
│              │                                   │                               │
│              └───────────────┬───────────────────┤                               │
│                              │                   │                               │
│                              ▼                   ▼                               │
│  ┌─────────────────────────────────────────────────────────────────────────┐     │
│  │                    linkedTableServer (Module)                            │    │
│  │                                                                          │    │
│  │   dynamic_data() ──► Filtered counts (labels from metadata)              │    │
│  │         │                                                                │    │
│  │         ├──► boxplot_raw (ggplot2 violin+box)                            │    │
│  │         ├──► boxplot_vst (ggplot2 violin+box) ◄── pca_rv$vst             │    │
│  │         ├──► comparison_plot (ggplot2 faceted)                           │    │
│  │         └──► count_table (reactable)                                     │    │
│  └─────────────────────────────────────────────────────────────────────────┘     │
│                                                                                  │
│              ┌───────────────┴───────────────────┐                               │
│              ▼                                   ▼                               │
│  ┌─────────────────────────┐     ┌─────────────────────────────────────────┐     │
│  │    pca_rv (Global)      │     │           PCA OUTPUTS                    │    │
│  │  ├── pca_result         │────►│  ├── pca_plot (Plotly)                   │    │
│  │  │   ├── vst            │     │  ├── pca_scree (Plotly)                  │    │
│  │  │   ├── var_explained  │     │  ├── download_eigenvalues (CSV)          │    │
│  │  │   └── pca_obj        │     │  └── download_pca_scores (CSV)           │    │
│  │  ├── pca_data           │     └─────────────────────────────────────────┘     │
│  │  └── computed (bool)    │                                                     │
│  └─────────────────────────┘                                                     │
│                                                                                  │
└─────────────────────────────────────────────────────────────────────────────────┘
```

## Core Reactive Variables

### Global Data Stores (app.R server)

| Variable | Type | Key Fields | Purpose |
|----------|------|------------|---------|
| `data_rv` | reactiveValues | meta, orig_meta, counts, orig_counts | Primary data container from upload |
| `counts_rv` | reactiveValues | counts, orig_counts | Working count matrix |
| `pca_rv` | reactiveValues | pca_result, pca_data, computed | PCA/VST results storage |
| `meta_out` | reactiveValues | data | Bridge to edited metadata |

### Module-Level Variables

| Variable | Module | Type | Purpose |
|----------|--------|------|---------|
| `current_data` | editableTableServer | reactiveVal | Current metadata table state |
| `deleted_stack` | editableTableServer | reactiveVal | Undo stack for deleted rows |
| `dynamic_data` | linkedTableServer | reactive | Counts filtered by current labels |
| `sartools_base_colors` | linkedTableServer | vector | 12 colors for group palette |
| `get_group_colors` | linkedTableServer | function | Dynamic color generator |
| `theme_qc` | linkedTableServer | function | Custom ggplot2 theme |

### Key Helper Functions

| Function | Location | Returns | Purpose |
|----------|----------|---------|---------|
| `get_current_metadata()` | app.R server | data.frame | Proper reactive access to edited metadata |
| `get_group_colors(n, names)` | linkedTableServer | named vector | Generate n colors for groups |
| `theme_qc(base_size)` | linkedTableServer | ggplot theme | Light background theme |
| `create_boxplot_df()` | linkedTableServer | data.frame | Convert count matrix to long format |

### Module Returns (APIs)

| Module | Returns | Key Functions |
|--------|---------|---------------|
| `storage` | sessionStorageServer | `session_id()`, `load_session()`, `save_initial_data()`, `save_data()` |
| `email` | emailNotificationsServer | `notifications_enabled()`, `track_change()`, `get_summary()` |
| `meta_module_out` | editableTableServer | `$data` (current_data), `$deleted_stack` |

## Output Flags for Conditional Panels

| Output | Type | Controls | TRUE When |
|--------|------|----------|-----------|
| `output$file_uploaded` | reactive | Sidebar visibility | `data_rv$meta` is not NULL |
| `output$pca_computed` | reactive | PCA results panel | `pca_rv$computed` is TRUE |

## Plots and Visualizations

### Count Distribution Plots (linkedTableServer)

| Output | Type | Data Source | Styling |
|--------|------|-------------|---------|
| `output$boxplot_raw` | renderPlot | dynamic_data() + log2 | Violin + box, group colors, theme_qc |
| `output$boxplot_vst` | renderPlot | pca_rv$vst | Violin + box, group colors, theme_qc |
| `output$comparison_plot` | renderPlot | Both raw + VST | Faceted violin + box |

### PCA Plots (app.R server)

| Output | Type | Customization |
|--------|------|---------------|
| `output$pca_plot` | renderPlotly | Title, colors, size, opacity, background, grid, legend, margins |
| `output$pca_scree` | renderPlotly | Bars + cumulative line, margins |

## Key Input Controls

### Session Management
- `input$load_session` - Load existing session by ID
- `input$start_new` - Open new session confirmation modal
- `input$confirm_new_session` - Confirm and reset all data
- `input$load_example` - Load example dataset from server

### PCA Parameters
- `input$run_pca` - Trigger PCA computation
- `input$vst_blind` - Blind mode for VST
- `input$vst_fitType` - Dispersion fit type
- `input$pca_ntop` - Top N genes for PCA
- `input$pca_pc_x`, `input$pca_pc_y` - PC axis selection
- `input$pca_color_by` - Metadata column for coloring

### Plot Customization
- `input$plot_title` - Custom plot title
- `input$plot_point_size` - Point size (5-50)
- `input$plot_point_opacity` - Opacity (0-1)
- `input$gradient_color1/2/3` - 3-color gradient picker
- `input$plot_show_grid` - Toggle grid lines
- `input$plot_show_legend` - Toggle legend
- `input$plot_bg_color` - Background color preset
- `input$reset_plot_options` - Reset to defaults

### Export Controls
- `input$open_download_modal` - Open export dialog
- `input$plot_download_format` - PNG/SVG/PDF
- `input$plot_download_width/height` - Dimensions
- `input$plot_download_filename` - Custom filename

## Key Relationships

1. **File Upload / Example Load / Session Restore**
   ```
   input$file OR input$load_example OR input$load_session
       │
       ▼
   data_rv$meta/counts populated
       │
       ├──► counts_rv$counts synced
       └──► editableTableServer initialized
   ```

2. **Metadata Editing Chain**
   ```
   User edits row
       │
       ▼
   editableTableServer.current_data updated
       │
       ├──► meta_out$data updated
       ├──► counts_rv columns renamed
       ├──► email$track_change() called
       └──► storage$save_data() triggered (debounced)
   ```

3. **PCA Computation Flow**
   ```
   input$run_pca clicked
       │
       ├──► get_current_metadata() → current samples
       ├──► counts_rv$counts → filtered count matrix
       ▼
   DESeq2::vst() + prcomp()
       │
       ▼
   pca_rv populated (vst, pca_data, var_explained)
       │
       ├──► pca_plot renders
       ├──► pca_scree renders
       └──► VST violin plots enabled
   ```

4. **Violin Plot Update Chain**
   ```
   Metadata edited (sample renamed/deleted)
       │
       ▼
   get_current_metadata() changes
       │
       ▼
   dynamic_data() recalculates (filtered counts)
       │
       ├──► boxplot_raw re-renders
       └──► boxplot_vst re-renders (if PCA computed)
   ```

## Color Systems

### PCA Plot Colors (3-color gradient)
- User-selectable via colourInput
- Default: #636EFA (blue) → #EF553B (red) → #00CC96 (green)
- Interpolated for any number of groups

### Violin/Box Plot Colors (SARTools-style)
- 12 base colors: Light blue, Orange, Teal, Pink, Lime, Yellow, Tan, Gray, Periwinkle, Red, Blue, Green
- ≤12 groups: Direct color assignment
- >12 groups: colorRampPalette interpolation
