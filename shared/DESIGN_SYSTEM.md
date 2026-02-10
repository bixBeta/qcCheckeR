# RNA-Seq Pipeline Design System
## Shared UI/UX Guidelines for Multi-App Workflow

---

## 🎨 Visual Identity

### Color Palette

```r
# Primary brand colors
colors <- list(
  primary      = "#0d6efd",  # Blue - primary actions
  secondary    = "#6c757d",  # Gray - secondary actions
  success      = "#198754",  # Green - success states, completed steps
  warning      = "#ffc107",  # Yellow - warnings, pending
  danger       = "#dc3545",  # Red - errors, destructive actions
  info         = "#0dcaf0",  # Cyan - informational
  
  # App-specific accent colors
  qc_checker   = "#6BAED6",  # Light blue
  deg_explorer = "#FC8D62",  # Orange
  pathway      = "#66C2A5",  # Teal
  
  # Backgrounds
  bg_light     = "#f8f9fa",
  bg_card      = "#ffffff",
  bg_plot      = "#fffef0",  # Warm white for plots
  
  # Text
  text_primary = "#212529",
  text_muted   = "#6c757d"
)
```

### Typography

```r
# Using system fonts via bslib
theme <- bs_theme(
  base_font = font_google("Inter"),
  heading_font = font_google("Inter"),
  code_font = font_google("Fira Code"),
  font_scale = 1.0
)
```

### Spacing

```css
/* Consistent spacing units */
--spacing-xs: 4px;
--spacing-sm: 8px;
--spacing-md: 16px;
--spacing-lg: 24px;
--spacing-xl: 32px;
```

---

## 🏗️ Layout Structure

### App Shell

```
┌─────────────────────────────────────────────────────────────┐
│ NAVBAR: App Name | Pipeline Progress | User Email | Logout  │
├──────────────┬──────────────────────────────────────────────┤
│              │                                              │
│   SIDEBAR    │              MAIN CONTENT                    │
│   (300px)    │                                              │
│              │   ┌──────────────────────────────────────┐   │
│  - User      │   │  CARD: Primary Content               │   │
│    Session   │   │                                      │   │
│              │   │  - Tabs/Panels as needed             │   │
│  - Actions   │   │                                      │   │
│              │   └──────────────────────────────────────┘   │
│  - Settings  │                                              │
│              │   ┌──────────────────────────────────────┐   │
│              │   │  CARD: Secondary Content             │   │
│              │   └──────────────────────────────────────┘   │
│              │                                              │
└──────────────┴──────────────────────────────────────────────┘
```

### Standard bslib Theme

```r
# Shared theme configuration
create_pipeline_theme <- function(accent_color = "#0d6efd") {
  bslib::bs_theme(
    version = 5,
    bootswatch = "flatly",
    primary = accent_color,
    "navbar-bg" = "#343a40",
    "body-bg" = "#f8f9fa",
    "card-bg" = "#ffffff",
    "border-radius" = "8px",
    "btn-border-radius" = "6px"
  )
}
```

---

## 🧩 Shared Components

### 1. User Session Panel (Always in Sidebar)

```r
# Standard placement
sidebar_content <- tagList(
  # User sessions always first
  userSessionsUI("sessions"),
  
  hr(),
  
  # App-specific controls below
  # ...
)
```

### 2. Pipeline Progress Indicator

```r
# Shows current position in workflow
pipelineProgressUI <- function(id, current_app) {
  apps <- c("qc-checker", "deg-explorer", "pathway-analyzer")
  current_idx <- which(apps == current_app)
  
  div(
    class = "pipeline-progress d-flex align-items-center gap-2",
    lapply(seq_along(apps), function(i) {
      status <- if (i < current_idx) "complete"
                else if (i == current_idx) "current"
                else "pending"
      
      div(
        class = paste("pipeline-step", status),
        span(class = "step-number", i),
        span(class = "step-name d-none d-lg-inline", 
             gsub("-", " ", tools::toTitleCase(apps[i])))
      )
    })
  )
}
```

### 3. Data Preview Card

```r
# Consistent data preview across apps
dataPreviewCard <- function(title, data, max_rows = 5) {
  card(
    card_header(title),
    card_body(
      class = "p-0",
      DT::datatable(
        head(data, max_rows),
        options = list(
          dom = 't',
          scrollX = TRUE,
          paging = FALSE
        ),
        rownames = FALSE,
        class = "compact stripe"
      )
    )
  )
}
```

### 4. Status Badges

```r
# Consistent badge styling
statusBadge <- function(status, text = NULL) {
  badge_class <- switch(status,
    "complete" = "bg-success",
    "running"  = "bg-primary",
    "pending"  = "bg-secondary",
    "error"    = "bg-danger",
    "warning"  = "bg-warning text-dark",
    "bg-secondary"
  )
  
  display_text <- text %||% tools::toTitleCase(status)
  span(class = paste("badge", badge_class), display_text)
}
```

### 5. Action Button Styles

```r
# Primary action (main workflow step)
actionButton("run_analysis", "Run Analysis", 
             class = "btn-primary btn-lg", 
             icon = icon("play"))

# Secondary action
actionButton("settings", "Settings", 
             class = "btn-outline-secondary btn-sm",
             icon = icon("cog"))

# Danger action (destructive)
actionButton("clear", "Clear All", 
             class = "btn-outline-danger btn-sm",
             icon = icon("trash"))

# Success action (export/download)
downloadButton("download", "Download", 
               class = "btn-success",
               icon = icon("download"))
```

---

## 📊 Plot Styling

### ggplot2 Theme

```r
# Shared ggplot2 theme
theme_pipeline <- function(base_size = 14) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "#fffef0", color = NA),
      panel.grid.major = element_line(color = "#e0e0e0", linewidth = 0.3),
      panel.grid.minor = element_blank(),
      axis.line = element_line(color = "#333333", linewidth = 0.5),
      legend.background = element_rect(fill = "white", color = NA),
      legend.key = element_rect(fill = "white", color = NA),
      plot.margin = margin(20, 20, 20, 20)
    )
}
```

### Plotly Configuration

```r
# Shared plotly config
plotly_config <- function(p) {
  p %>%
    plotly::config(
      displayModeBar = TRUE,
      modeBarButtonsToRemove = c("lasso2d", "select2d"),
      toImageButtonOptions = list(
        format = "png",
        width = 1200,
        height = 800
      )
    ) %>%
    plotly::layout(
      font = list(family = "Inter, sans-serif"),
      plot_bgcolor = "#fffef0",
      paper_bgcolor = "white"
    )
}
```

### Color Palettes

```r
# Categorical (up to 12 groups)
categorical_colors <- c(
 "#6BAED6", "#FC8D62", "#66C2A5", "#E78AC3",
  "#A6D854", "#FFD92F", "#E5C494", "#B3B3B3",
  "#8DA0CB", "#E41A1C", "#377EB8", "#4DAF4A"
)

# Diverging (for fold changes, z-scores)
diverging_colors <- c("#2166AC", "#F7F7F7", "#B2182B")  # Blue-White-Red

# Sequential (for expression levels)
sequential_colors <- viridisLite::viridis(9)
```

---

## 🔄 Cross-App Data Flow

### Session Data Structure

```r
# Standard session structure for pipeline compatibility
session_data <- list(
  # Metadata
  session_id = "qc_20250210_abc123",
  user_email = "user@lab.edu",
  app_name = "qc-checker",
  created = Sys.time(),
  updated = Sys.time(),
  
  # Original uploaded data
  file_data = list(
    file_name = "experiment.RData",
    metadata = data.frame(...),   # Sample metadata
    counts = matrix(...)          # Raw count matrix
  ),
  
  # Current state (edited)
  current_edits = list(
    metadata = data.frame(...),   # Edited metadata
    
    # App-specific results
    pca = list(
      vst = matrix(...),          # VST-transformed counts
      pca = prcomp(...),          # PCA object
      var_explained = numeric()
    ),
    
    annotation = list(
      annotated_data = data.frame(...),
      method = "gprofiler"
    )
  ),
  
  # Pipeline tracking
  pipeline = list(
    upstream_session = NULL,      # Parent session ID
    downstream_sessions = c()     # Child session IDs
  )
)
```

### Handoff Points

| From | To | Data Transferred |
|------|------|-----------------|
| QC-CheckeR | DEG-Explorer | Cleaned metadata, QC'd counts, VST matrix, annotations |
| DEG-Explorer | Pathway-Analyzer | DESeq2 results, gene lists, contrast definitions |

---

## 📁 File Organization

```
/srv/shiny-apps/
├── shared/
│   ├── modules/
│   │   ├── module_user_sessions.R
│   │   ├── module_email_notifications.R
│   │   └── module_pipeline_progress.R
│   ├── R/
│   │   ├── theme.R
│   │   ├── plot_helpers.R
│   │   └── data_validators.R
│   ├── www/
│   │   ├── styles.css
│   │   └── logo.png
│   └── sessions/
│       ├── registry.json
│       └── users/
│
├── qc-checker/
│   ├── app.R
│   └── www/
│
├── deg-explorer/
│   ├── app.R
│   └── www/
│
└── pathway-analyzer/
    ├── app.R
    └── www/
```

---

## ✅ Checklist for New Apps

- [ ] Use `create_pipeline_theme()` for consistent styling
- [ ] Include `userSessionsUI()` in sidebar
- [ ] Add pipeline progress indicator in navbar
- [ ] Follow session data structure for compatibility
- [ ] Use shared color palettes
- [ ] Apply `theme_pipeline()` to ggplot2 plots
- [ ] Use `plotly_config()` for interactive plots
- [ ] Implement "Send to Next App" button
- [ ] Support `?session=ID` URL parameter for incoming links
- [ ] Register session in central registry on save

---

## 🚀 Quick Start Template

```r
# app.R template for new pipeline app
library(shiny)
library(bslib)

# Source shared modules
source("../shared/modules/module_user_sessions.R")
source("../shared/R/theme.R")

APP_NAME <- "my-new-app"
UPSTREAM_APP <- "qc-checker"  # or NULL if first in pipeline

ui <- page_navbar(
  title = "My New App",
  theme = create_pipeline_theme(accent_color = "#FC8D62"),
  
  sidebar = sidebar(
    width = 300,
    userSessionsUI("sessions"),
    hr(),
    # App-specific controls...
  ),
  
  # Main content...
)

server <- function(input, output, session) {
  # Initialize session management
  sessions <- userSessionsServer(
    id = "sessions",
    app_name = APP_NAME,
    base_path = "../shared/sessions",
    get_session_data = function() {
      # Return current app state for saving
    },
    on_session_load = function(data) {
      # Handle loaded session data
    }
  )
  
  # Check for incoming session from URL
  observe({
    incoming_id <- getSessionFromURL(session)
    if (!is.null(incoming_id)) {
      # Load upstream session
    }
  })
  
  # App logic...
}

shinyApp(ui, server)
```
