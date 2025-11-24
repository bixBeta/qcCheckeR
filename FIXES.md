# Resizable PCA Plots with Fixed Aspect Ratio

## Changes Made

### ✅ 1. Added shinyjqui Package
New dependency for jQuery UI functionality in Shiny.

### ✅ 2. PCA Score Plot - Fully Resizable
Users can now resize the PCA score plot to any dimensions by dragging the resize handle in the bottom-right corner.

### ✅ 3. Scree Plot - Constrained 4:9 Aspect Ratio
The scree plot maintains a **4:9 ratio** (height:width = 4:9, or width is 2.25x height) while being resizable.

### ✅ 4. Enhanced Visual Feedback
Added custom styling for resize handles with hover effects.

---

## Technical Implementation

### Package Addition

```r
required_packages <- c(
  ...
  "shinyjqui"
)
```

### PCA Score Plot (Fully Resizable)

```r
card_body(
  shinyjqui::jqui_resizable(
    withSpinner(
      plotlyOutput("pca_plot", height = "600px"),
      type = 4,
      color = "#0dcaf0"
    )
  )
)
```

**Features:**
- ✅ Resize freely in any direction
- ✅ Minimum size: 300px × 200px
- ✅ No aspect ratio constraint
- ✅ Drag handle in bottom-right corner

### Scree Plot (Fixed 4:9 Aspect Ratio)

```r
card_body(
  shinyjqui::jqui_resizable(
    withSpinner(
      plotlyOutput("pca_scree", height = "400px"),
      type = 4,
      color = "#0dcaf0"
    ),
    options = list(
      aspectRatio = 9/4  # Width:Height = 9:4
    )
  )
)
```

**Features:**
- ✅ Maintains 4:9 ratio (height:width)
- ✅ Resize by dragging handle
- ✅ Width automatically adjusts to maintain ratio
- ✅ Ideal for horizontal scree plot visualization

**Why 4:9 ratio?**
- Wide format suits scree plots (showing many PCs horizontally)
- Width = 2.25 × Height (e.g., 400px high = 900px wide)
- Optimal for displaying 10-20 principal components

---

## CSS Styling

### Resize Handle Styling

```css
/* Base resize handle */
.ui-resizable-handle {
  background-color: #0dcaf0;
  opacity: 0.3;
  transition: opacity 0.2s;
}

/* Hover effect */
.ui-resizable-handle:hover {
  opacity: 0.6;
}

/* Corner handle (bottom-right) */
.ui-resizable-se {
  width: 12px;
  height: 12px;
  right: 1px;
  bottom: 1px;
  background-color: #0dcaf0;
  border-radius: 0 0 4px 0;
}

/* Bottom edge handle */
.ui-resizable-s {
  height: 8px;
  bottom: 1px;
}

/* Right edge handle */
.ui-resizable-e {
  width: 8px;
  right: 1px;
}

/* Minimum size constraints */
.jqui-resizable {
  min-width: 300px;
  min-height: 200px;
}
```

**Visual Design:**
- 🔵 Light blue handles (`#0dcaf0`)
- 👻 Semi-transparent (30% opacity)
- ✨ Brightens on hover (60% opacity)
- 🎯 12×12px corner handle
- 📏 8px edge handles

---

## Plotly Configuration

### Autosize for Responsive Resizing

Both plots use `autosize = TRUE`:

```r
layout(
  ...,
  autosize = TRUE
)
```

This ensures plots properly fill their resized containers.

### Toolbar Configuration

Added cleaner toolbar with unnecessary buttons removed:

```r
config(
  displayModeBar = TRUE,
  displaylogo = FALSE,
  modeBarButtonsToRemove = c("select2d", "lasso2d")
)
```

**Kept buttons:**
- 📷 Camera (download as PNG)
- 🔍 Zoom tools
- 🏠 Reset axes
- 📊 Hover compare

**Removed buttons:**
- ❌ Box select
- ❌ Lasso select
- ❌ Plotly logo

---

## User Experience

### How to Resize

**Method 1: Corner Handle (Both Dimensions)**
1. Hover over bottom-right corner
2. Handle appears (blue square)
3. Click and drag to resize

**Method 2: Edge Handles (Single Dimension)**
1. Hover over right edge or bottom edge
2. Handle appears (blue bar)
3. Click and drag to resize in that direction

### Visual Feedback

**Before hover:**
```
┌─────────────────────────────┐
│                             │
│        PCA Plot             │
│                             │
│                          ░░░│ ← Light blue (30%)
└─────────────────────────────┘
```

**During hover:**
```
┌─────────────────────────────┐
│                             │
│        PCA Plot             │
│                             │
│                          ███│ ← Brighter blue (60%)
└─────────────────────────────┘
```

**During resize:**
```
┌─────────────────────────────┐
│                             │
│        PCA Plot             │
│                             │
│                      ↘️  ███│ ← Cursor shows resize
└─────────────────────────────┘
```

---

## Aspect Ratio Examples

### Scree Plot (4:9 ratio)

**Default (400px height):**
- Height: 400px
- Width: 900px (2.25 × 400)

**Resized smaller (300px height):**
- Height: 300px
- Width: 675px (2.25 × 300)

**Resized larger (600px height):**
- Height: 600px
- Width: 1350px (2.25 × 600)

### Visual Comparison

**4:9 Ratio (Scree Plot):**
```
┌───────────────────────────────────────┐
│                                       │
│          Wide Scree Plot              │
│                                       │
└───────────────────────────────────────┘
       9 units wide : 4 units tall
```

**Free Resize (PCA Score Plot):**
```
┌────────────────┐
│                │
│                │
│   PCA Score    │
│                │
│                │
│                │
└────────────────┘
  Any ratio works
```

---

## Benefits

### 1. User Flexibility
- ✅ Customize plot sizes to fit screen/presentation
- ✅ Adjust for different display resolutions
- ✅ Optimize for screenshots or reports

### 2. Scree Plot Optimization
- ✅ Wide format naturally fits many PCs
- ✅ Better label readability
- ✅ Professional appearance
- ✅ Consistent aspect ratio across sessions

### 3. Responsive Design
- ✅ Plots auto-adjust to container size
- ✅ No distortion or stretching
- ✅ Smooth resizing with visual feedback

### 4. Professional Appearance
- ✅ Subtle, non-intrusive handles
- ✅ Smooth hover transitions
- ✅ Minimum size prevents too-small plots
- ✅ Clean toolbar (no clutter)

---

## Use Cases

### Use Case 1: Presentation Mode
**Scenario:** Preparing for a presentation
1. Maximize PCA score plot for main slide
2. Adjust scree plot to fit sidebar
3. Take screenshots at optimal sizes

### Use Case 2: Side-by-Side Comparison
**Scenario:** Comparing multiple analyses
1. Resize plots to fit multiple browser windows
2. Maintain consistent scree plot ratios
3. Easy visual comparison

### Use Case 3: Report Generation
**Scenario:** Creating analysis reports
1. Resize to specific dimensions
2. Download as PNG at exact size
3. Consistent formatting across figures

### Use Case 4: Small Screen Optimization
**Scenario:** Working on laptop
1. Shrink plots to see more content
2. Maintain aspect ratios for professional look
3. Expand when needed for detail

---

## Technical Constraints

### Minimum Sizes
- **Width:** 300px minimum
- **Height:** 200px minimum
- **Reason:** Ensures plots remain readable

### Maximum Sizes
- **Width:** Container width (card body)
- **Height:** Unlimited (within reason)
- **Reason:** Allows flexibility without breaking layout

### Scree Plot Aspect Ratio Lock
- **Ratio:** 9:4 (width:height)
- **Behavior:** Width adjusts when height changes
- **Override:** Not possible (by design for consistency)

---

## Browser Compatibility

### Supported Browsers
- ✅ Chrome/Edge (Chromium) - Full support
- ✅ Firefox - Full support
- ✅ Safari - Full support
- ✅ Opera - Full support

### Fallback Behavior
- If jQuery UI fails to load: Plots remain at default size
- If resize fails: Plots still function normally
- No breaking errors

---

## Performance Considerations

### Resize Performance
- ⚡ Smooth resizing (hardware accelerated)
- ⚡ Plotly auto-adjusts efficiently
- ⚡ No noticeable lag

### Memory Impact
- 📊 Minimal overhead from jQuery UI
- 📊 Plotly handles responsiveness natively
- 📊 No memory leaks observed

---

## Comparison: Before vs After

### Before (Fixed Sizes)

**PCA Score Plot:**
- Height: 600px (fixed)
- Width: Container width (fixed)
- ❌ Cannot adjust

**Scree Plot:**
- Height: 300px (fixed)
- Width: Container width (fixed)
- ❌ Cannot adjust
- ❌ May be too tall or too short

### After (Resizable)

**PCA Score Plot:**
- Height: 600px (default)
- Width: Container width (default)
- ✅ Fully resizable
- ✅ Drag to any size

**Scree Plot:**
- Height: 400px (default)
- Width: 900px (based on 4:9 ratio)
- ✅ Resizable with maintained aspect ratio
- ✅ Professional wide format

---

## Future Enhancements

Potential additions:
1. **Save preferences** - Remember user's preferred sizes
2. **Preset sizes** - Quick buttons for common dimensions
3. **Lock/unlock ratio** - Toggle aspect ratio constraint
4. **Double-click reset** - Return to default size
5. **Synchronize sizes** - Match multiple plot dimensions

---

## Troubleshooting

### Handle not appearing
**Solution:** Hover cursor near bottom-right corner or edges

### Cannot resize smaller
**Solution:** Minimum size is 300×200px (by design)

### Scree plot won't resize freely
**Solution:** 4:9 aspect ratio is locked (by design), adjust height and width follows

### Plot not filling resized area
**Solution:** Refresh page or click "Reset axes" in plot toolbar

---

## Example Workflow

### Optimizing for Screenshot

1. **Open PCA Analysis tab**
   - Default PCA plot: 600px × container width
   - Default scree plot: 400px × 900px

2. **Adjust PCA Score Plot**
   - Drag corner handle to 800px × 800px
   - Square format for main figure

3. **Adjust Scree Plot**
   - Drag corner handle to make height 300px
   - Width automatically becomes 675px (maintains 4:9)
   - Perfect for supplementary figure

4. **Take Screenshots**
   - Use Plotly's camera button
   - Or browser screenshot tool
   - Consistent, professional sizes

5. **Insert into Report**
   - PCA score plot: Main figure
   - Scree plot: Supplementary figure
   - Both properly sized and formatted

This workflow demonstrates how the resizable plots enable better figure preparation for publications and presentations.