# Clear aspect ratio test - what SHOULD we see?

library(lattice)
library(ggplot2)

# Simple data: 3 columns x 1 row = 3 tiles in a horizontal line
test_data <- data.frame(
  x = c(1, 2, 3),
  y = c(1, 1, 1),
  value = c(1, 2, 3)
)

cat("Data: 3 tiles in a row (3 units wide, 1 unit tall)\n\n")

# -----------------------------------------------------------------------------
# Test 1: aspect = 1 (SQUARE CELLS)
# -----------------------------------------------------------------------------
cat("=== Test 1: aspect = 1 ===\n")
cat("EXPECTED: Each individual cell/tile should be SQUARE\n")
cat("Since we have 3 tiles in a row, the overall plot should be 3x wider than tall\n\n")

print(levelplot(value ~ x*y, data=test_data,
                aspect=1,
                main="Lattice: aspect=1 (cells should be square)",
                scales=list(draw=TRUE),
                colorkey=FALSE))

print(ggplot(test_data, aes(x=x, y=y, fill=value)) +
  geom_tile() +
  coord_fixed(ratio=1) +
  ggtitle("ggplot: ratio=1 (cells should be square)") +
  theme_minimal() +
  theme(aspect.ratio=NULL))

readline("Press Enter for next test...")

# -----------------------------------------------------------------------------
# Test 2: aspect = 1.5 (CELLS TALLER THAN WIDE)
# -----------------------------------------------------------------------------
cat("\n=== Test 2: aspect = 1.5 ===\n")
cat("EXPECTED: Each cell should be 1.5x TALLER than wide\n")
cat("If a cell is 10cm wide, it should be 15cm tall\n\n")

print(levelplot(value ~ x*y, data=test_data,
                aspect=1.5,
                main="Lattice: aspect=1.5 (cells 1.5x taller than wide)",
                scales=list(draw=TRUE),
                colorkey=FALSE))

print(ggplot(test_data, aes(x=x, y=y, fill=value)) +
  geom_tile() +
  coord_fixed(ratio=1.5) +
  ggtitle("ggplot: ratio=1.5 (cells 1.5x taller than wide)") +
  theme_minimal() +
  theme(aspect.ratio=NULL))

readline("Press Enter for next test...")

# -----------------------------------------------------------------------------
# Test 3: aspect = 2 (CELLS MUCH TALLER)
# -----------------------------------------------------------------------------
cat("\n=== Test 3: aspect = 2 ===\n")
cat("EXPECTED: Each cell should be 2x TALLER than wide\n")
cat("If a cell is 10cm wide, it should be 20cm tall\n\n")

print(levelplot(value ~ x*y, data=test_data,
                aspect=2,
                main="Lattice: aspect=2 (cells 2x taller than wide)",
                scales=list(draw=TRUE),
                colorkey=FALSE))

print(ggplot(test_data, aes(x=x, y=y, fill=value)) +
  geom_tile() +
  coord_fixed(ratio=2) +
  ggtitle("ggplot: ratio=2 (cells 2x taller than wide)") +
  theme_minimal() +
  theme(aspect.ratio=NULL))

cat("\n=== SUMMARY ===\n")
cat("For aspect=1: cells should be SQUARE\n")
cat("For aspect=1.5: cells should be rectangular (1.5x taller)\n")
cat("For aspect=2: cells should be very tall rectangles (2x taller)\n")
cat("\nDo the ggplot versions show this correctly?\n")
cat("Do the lattice versions match the ggplot versions?\n")
