# Test aspect ratio in desplot (lattice) vs ggdesplot (ggplot2)

library(desplot)
library(agridat)
data(yates.oats)

cat("Testing aspect ratios with yates.oats data\n")
cat("Field has", length(unique(yates.oats$col)), "columns and",
    length(unique(yates.oats$row)), "rows\n\n")

# -----------------------------------------------------------------------------
# Test 1: aspect = 1 (SQUARE CELLS)
# -----------------------------------------------------------------------------
cat("=== Test 1: aspect = 1 (square cells) ===\n\n")

# Lattice version
desplot(yates.oats, yield ~ col*row,
        col=gen, num=nitro, cex=1, out1=block,
        aspect=1,
        main="LATTICE: aspect=1 (cells should be square)")

# ggplot version
ggdesplot(yates.oats, yield ~ col*row,
          col=gen, num=nitro, cex=1, out1=block,
          aspect=1,
          main="GGPLOT: aspect=1 (cells should be square)")

readline("Press Enter for next test...")

# -----------------------------------------------------------------------------
# Test 2: aspect = 2 (CELLS 2x TALLER than wide)
# -----------------------------------------------------------------------------
cat("\n=== Test 2: aspect = 2 (cells 2x taller) ===\n\n")

# Lattice version
desplot(yates.oats, yield ~ col*row,
        col=gen, num=nitro, cex=1, out1=block,
        aspect=2,
        main="LATTICE: aspect=2 (cells 2x taller)")

# ggplot version
ggdesplot(yates.oats, yield ~ col*row,
          col=gen, num=nitro, cex=1, out1=block,
          aspect=2,
          main="GGPLOT: aspect=2 (cells 2x taller)")

readline("Press Enter for next test...")

# -----------------------------------------------------------------------------
# Test 3: aspect = 0.5 (CELLS 2x WIDER than tall)
# -----------------------------------------------------------------------------
cat("\n=== Test 3: aspect = 0.5 (cells 2x wider) ===\n\n")

# Lattice version
desplot(yates.oats, yield ~ col*row,
        col=gen, num=nitro, cex=1, out1=block,
        aspect=0.5,
        main="LATTICE: aspect=0.5 (cells 2x wider)")

# ggplot version
ggdesplot(yates.oats, yield ~ col*row,
          col=gen, num=nitro, cex=1, out1=block,
          aspect=0.5,
          main="GGPLOT: aspect=0.5 (cells 2x wider)")

cat("\n=== COMPARISON ===\n")
cat("Do the lattice and ggplot versions look similar for each aspect ratio?\n")
cat("aspect=1: cells square?\n")
cat("aspect=2: cells taller?\n")
cat("aspect=0.5: cells wider?\n")
