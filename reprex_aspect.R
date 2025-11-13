## Testing aspect ratio support in ggdesplot

library(desplot)
library(agridat)

# Example 1: aspect=1 creates square cells
ggdesplot(yates.oats, yield ~ col*row,
          col=gen, num=nitro, cex=1, out1=block,
          aspect=1,
          main="aspect=1: Square cells")

# Example 2: aspect=2 creates cells 2x taller than wide
ggdesplot(yates.oats, yield ~ col*row,
          col=gen, num=nitro, cex=1, out1=block,
          aspect=2,
          main="aspect=2: Cells 2x taller than wide")

# Example 3: Using physical field dimensions (from README examples)
ggdesplot(yates.oats, yield ~ col*row,
          col=gen, num=nitro, cex=1, out1=block,
          aspect=511/176,
          main="aspect=511/176: True-scale field map")

# Example 4: With facets (besag.met dataset)
ggdesplot(besag.met, yield ~ col*row|county,
          out1=rep, out2=block,
          aspect=1.0,
          main="aspect=1.0 with facets")

# Visual confirmation test: Compare cell shapes
# Create simple test with clearly defined dimensions
test_data <- expand.grid(col=1:4, row=1:4)
test_data$yield <- runif(nrow(test_data), 50, 100)

ggdesplot(test_data, yield ~ col*row,
          aspect=1,
          main="4x4 grid with aspect=1: All cells should be square")
