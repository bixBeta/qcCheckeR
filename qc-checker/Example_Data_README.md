# Example_Data.RData

This file should contain sample RNA-Seq data for testing the app.

## Required Format

```r
# counts: A matrix with genes as rows, samples as columns
counts <- matrix(
  rpois(1000 * 12, lambda = 100),
  nrow = 1000,
  dimnames = list(
    paste0("Gene", 1:1000),
    paste0("Sample", 1:12)
  )
)

# metadata: A data.frame with sample information
metadata <- data.frame(
  label = paste0("Sample", 1:12),
  group = rep(c("Control", "Treatment"), each = 6),
  batch = rep(c("A", "B", "C"), 4)
)

# Save
save(counts, metadata, file = "Example_Data.RData")
```

## Creating Example Data

Run the above code in R to generate a sample dataset, or use your own data.
