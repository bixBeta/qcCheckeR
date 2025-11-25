# PCA Results: Three Tabs, Full Screen

## Changes Made

1. ✅ **Added third tab: Variance Explained**
2. ✅ **Moved scree plot into tabs**
3. ✅ **Made PCA Results card full screen capable**
4. ✅ **Increased plot heights to 700px**
5. ✅ **Removed standalone scree card**
6. ✅ **Reordered tabs logically**

---

## New Three-Tab Layout

### Tab Structure:

```
┌────────────────────────────────────────────────┐
│ PCA Results                        │ Plot      │
│ [PCA Score Plot] [Variance] [Summary]│ Options │
├────────────────────────────────────┼───────────┤
│                                    │           │
│                                    │ • Title   │
│        [Active Tab Content]        │ • Size    │
│           700px height             │ • Opacity │
│                                    │ • Palette │
│                                    │ • Grid    │
│                                    │ • Legend  │
│                                    │ • BG      │
│                                    │           │
│                                    │[Download] │
│                                    │[Reset]    │
└────────────────────────────────────┴───────────┘
```

---

## Tab 1: PCA Score Plot

### Content:
- Interactive plotly scatter plot
- PC scores visualization
- Color by metadata
- Hover info

### Features:
- Width: 100% (flexible)
- Height: 700px (increased from 600px)
- Full customization via sidebar
- Real-time updates

### Use Cases:
- Identify sample clustering
- Detect outliers
- Explore treatment effects
- Quality control

---

## Tab 2: Variance Explained

### Content:
- Scree plot (bar chart)
- Shows variance per PC
- Cumulative variance line
- Interactive plotly

### Features:
- Width: 100% (flexible)
- Height: 700px (increased from 400px)
- Full screen available
- Better visibility

### Use Cases:
- Determine number of PCs to retain
- Understand data complexity
- Assess quality of dimensionality reduction
- Report variance captured

### Why Second Tab:
- Natural workflow: View scores → Check variance
- Related to score plot
- Needed for interpretation

---

## Tab 3: PCA Summary

### Content:
- Text statistics
- PC standard deviations
- Proportion of variance
- Cumulative proportions
- Top gene loadings

### Features:
- Scrollable text output
- Monospace font
- Full details
- Copyable

### Use Cases:
- Get exact numbers
- Export statistics
- Check loadings
- Technical details

---

## Full Screen Mode

### New Feature:

```r
card(
  full_screen = TRUE,  # ← Enables full screen button
  card_header("PCA Results"),
  ...
)
```

### Benefits:
- Button in card header (top-right)
- Click to expand to full browser window
- Perfect for presentations
- Better for detailed analysis

### When to Use:
- Presenting results
- Detailed exploration
- Screenshot for publications
- Focus mode (no distractions)

---

## Before vs After Comparison

### Before (2 Tabs + Separate Card):

```
┌────────────────────────────────┐
│ PCA Results        │ Options   │
│ [Score] [Summary]  │           │
├────────────────────┴───────────┤
│     Active Tab (600px)         │
└────────────────────────────────┘

┌────────────────────────────────┐
│ Variance Explained             │
├────────────────────────────────┤
│   Scree Plot (400px)           │
└────────────────────────────────┘
```

**Issues:**
- Scree plot separate
- Requires scrolling
- Smaller plots
- Disconnected

---

### After (3 Tabs, One Card):

```
┌────────────────────────────────────┐
│ PCA Results            │ Options   │🔲
│ [Score][Variance][Summary]         │
├────────────────────────┴───────────┤
│                                    │
│     Active Tab (700px)             │
│                                    │
└────────────────────────────────────┘
```

**Benefits:**
- ✅ All results in one place
- ✅ No scrolling needed
- ✅ Larger plots (700px)
- ✅ Logical grouping
- ✅ Full screen capable

---

## Tab Order Rationale

### Order: Score → Variance → Summary

**Tab 1: Score Plot**
- Most important visualization
- First thing users want to see
- Primary analysis tool

**Tab 2: Variance Explained**
- Supports interpretation of scores
- Helps decide PC selection
- Validates PCA quality

**Tab 3: Summary**
- Detailed statistics
- Reference information
- Less frequently needed

### User Flow:
```
1. View score plot (Tab 1)
   ↓ "How much variance do PCs capture?"
2. Check variance (Tab 2)
   ↓ "What are the exact numbers?"
3. Review summary (Tab 3)
```

---

## Height Increase

### Changes:

**Before:**
- Score plot: 600px
- Scree plot: 400px
- Summary: auto

**After:**
- All tabs: 700px
- Consistent height
- More detail visible

### Benefits:
- ✅ No layout shift when switching tabs
- ✅ More space for data
- ✅ Better readability
- ✅ Professional appearance

---

## Full Screen Implementation

### Card Definition:

```r
card(
  full_screen = TRUE,
  card_header("PCA Results"),
  layout_sidebar(
    sidebar = sidebar(...),
    navset_card_tab(...)
  )
)
```

### Full Screen Behavior:

**Normal View:**
- Card in page layout
- Left sidebar (params) visible
- Right sidebar (options) visible
- Standard size

**Full Screen View:**
- Expands to fill browser
- Only this card visible
- Tabs and sidebar remain
- ESC to exit

### Button Location:
- Top-right of card header
- Standard bslib icon
- Hover to see tooltip
- Click to toggle

---

## Sidebar Behavior Across Tabs

### On Score Plot Tab:
- All controls active
- Affects plot display
- Download works
- Reset works

### On Variance Tab:
- Sidebar remains visible
- Controls don't affect scree plot
- Consistency maintained
- No layout shift

### On Summary Tab:
- Sidebar remains visible
- Controls inactive for text
- Visual consistency
- Easy to switch back

**Design Choice:**
Keep sidebar visible on all tabs for:
- Consistent width
- No jarring layout changes
- Controls ready when switching to plot
- Professional appearance

---

## Code Structure

### Complete Implementation:

```r
conditionalPanel(
  condition = "output.pca_computed",
  card(
    full_screen = TRUE,
    card_header("PCA Results"),
    layout_sidebar(
      sidebar = sidebar(
        id = "plot_customization_sidebar",
        position = "right",
        width = 280,
        open = TRUE,
        
        # All customization controls
        h4("Plot Options"),
        textInput(...),
        sliderInput(...),
        # ... more controls
        actionButton("open_download_modal", ...),
        actionButton("reset_plot_options", ...)
      ),
      
      # Three tabs
      navset_card_tab(
        id = "pca_tabs",
        
        nav_panel(
          "PCA Score Plot",
          withSpinner(
            plotlyOutput("pca_plot", width = "100%", height = "700px"),
            ...
          )
        ),
        
        nav_panel(
          "Variance Explained",
          withSpinner(
            plotlyOutput("pca_scree", width = "100%", height = "700px"),
            ...
          )
        ),
        
        nav_panel(
          "PCA Summary",
          withSpinner(
            verbatimTextOutput("pca_summary"),
            ...
          )
        )
      )
    )
  )
)
```

---

## Visual Layout

### Page Structure:

```
PCA Analysis Tab
┌─────────────────────────────────────────────┐
│ ┌──────────┬──────────────────┬──────────┐ │
│ │  Left    │   PCA Results    │  Right   │ │
│ │ Sidebar  │   (Full Card)    │ Sidebar  │ │
│ │          │                  │          │ │
│ │ • Blind  │ [Score][Var][Sum]│ • Title  │ │
│ │ • Fit    │ ┌──────────────┐ │ • Size   │ │
│ │ • Genes  │ │              │ │ • Colors │ │
│ │ • PC X/Y │ │  Active Tab  │ │ • Grid   │ │
│ │ • Color  │ │   (700px)    │ │ • Legend │ │
│ │          │ │              │ │          │ │
│ │[Run PCA] │ └──────────────┘ │[Download]│ │
│ │          │                  │[Reset]   │ │
│ └──────────┴──────────────────┴──────────┘ │
└─────────────────────────────────────────────┘
```

**Clean, organized, everything in one place!**

---

## Responsive Behavior

### Desktop (Wide):
```
┌────────┬──────────────┬────────┐
│  Left  │     Main     │ Right  │
│ Params │   3 Tabs     │ Options│
│        │   700px      │        │
└────────┴──────────────┴────────┘
```

### Tablet (Medium):
```
┌────────┬──────────────┐
│  Left  │     Main     │
│ Params │   3 Tabs     │
│        │  (Right off) │
└────────┴──────────────┘
```

### Mobile (Small):
```
┌──────────────┐
│     Main     │
│   3 Tabs     │
│ (Both sides  │
│     off)     │
└──────────────┘
```

**Full screen mode:** Always full browser width on all devices

---

## Use Cases

### Research Analysis:
1. Run PCA
2. View score plot (outliers, clustering)
3. Check variance (enough PCs?)
4. Review summary (exact numbers)
5. Present findings (full screen)

### Quality Control:
1. Score plot (detect batch effects)
2. Variance (data quality check)
3. Summary (technical validation)

### Publication:
1. Customize appearance (colors, size)
2. Full screen mode
3. Screenshot/download
4. Include in manuscript

### Presentation:
1. Full screen mode
2. Switch between tabs
3. Interactive demonstration
4. Q&A with live data

---

## Benefits Summary

### Organization:
- ✅ All PCA results in one card
- ✅ Logical tab order
- ✅ No scrolling needed
- ✅ Professional interface

### Visibility:
- ✅ Larger plots (700px vs 600px/400px)
- ✅ Full screen capability
- ✅ Consistent heights
- ✅ Better detail

### User Experience:
- ✅ Intuitive navigation
- ✅ Clear workflow
- ✅ Focus mode available
- ✅ Modern design

### Technical:
- ✅ Clean code structure
- ✅ Single conditional panel
- ✅ Consistent styling
- ✅ bslib best practices

---

## Testing Checklist

- [x] Three tabs render correctly
- [x] Tab 1: Score plot (700px)
- [x] Tab 2: Variance/scree plot (700px)
- [x] Tab 3: Summary (text)
- [x] Sidebar visible on all tabs
- [x] Full screen button appears
- [x] Full screen works
- [x] ESC exits full screen
- [x] Can switch between tabs
- [x] Plot controls work (Tab 1)
- [x] No layout errors
- [x] Consistent heights

---

## Keyboard Shortcuts

### Tab Navigation:
- Click tabs to switch
- (Future: Arrow keys in full screen mode)

### Full Screen:
- Click 🔲 icon in header
- Press ESC to exit
- F11 for browser full screen (separate)

---

## Future Enhancements (Optional)

### Additional Tabs:
- Tab 4: Loadings plot
- Tab 5: Biplot
- Tab 6: Contributions

### Enhanced Full Screen:
- Keyboard tab switching
- Hide sidebars option
- Maximize plot area
- Screenshot tool

### Download Options:
- Per-tab download
- Export all tabs
- Create report (PDF)

---

## Summary

### What Changed:

1. **Three tabs:** Score Plot, Variance, Summary
2. **All in one card:** No separate scree card
3. **Full screen:** Button in card header
4. **Larger plots:** 700px height (up from 600px/400px)
5. **Better flow:** Logical tab order

### Why Better:

- ✅ **Unified:** All PCA results together
- ✅ **Spacious:** 700px plots, full screen capable
- ✅ **Organized:** Clear tab structure
- ✅ **Professional:** Modern interface
- ✅ **Efficient:** No scrolling needed

### Result:

**Complete, professional PCA analysis interface in a single, full-screen-capable card!** 📊✨

All three tabs working perfectly!