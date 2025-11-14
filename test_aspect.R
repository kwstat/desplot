# Simple test to compare aspect ratio in lattice vs ggplot2

library(lattice)
library(ggplot2)

# Create simple data: 3 x 1 grid (3 wide, 1 tall)
test_data <- data.frame(
  x = c(1, 2, 3),
  y = c(1, 1, 1),
  value = c(1, 2, 3)
)

cat("Data dimensions:\n")
cat("x range:", range(test_data$x), "\n")
cat("y range:", range(test_data$y), "\n")
cat("Should be 3 units wide, 1 unit tall\n\n")

# Test 1: aspect = 1 (square cells)
cat("=== Test 1: aspect = 1 (square) ===\n")
cat("Each cell should be square\n\n")

# Lattice with aspect=1
p1 <- levelplot(value ~ x*y, data=test_data,
                aspect=1,
                main="Lattice: aspect=1",
                scales=list(draw=TRUE),
                colorkey=FALSE)
print(p1)

# ggplot with aspect=1
p2 <- ggplot(test_data, aes(x=x, y=y, fill=value)) +
  geom_tile() +
  coord_fixed(ratio=1) +
  ggtitle("ggplot: coord_fixed(ratio=1)") +
  theme_minimal()
print(p2)

# Test 2: aspect = 2 (twice as tall as wide)
cat("\n=== Test 2: aspect = 2 ===\n")
cat("Each cell should be 2x taller than wide\n\n")

# Lattice with aspect=2
p3 <- levelplot(value ~ x*y, data=test_data,
                aspect=2,
                main="Lattice: aspect=2",
                scales=list(draw=TRUE),
                colorkey=FALSE)
print(p3)

# ggplot with aspect=2
p4 <- ggplot(test_data, aes(x=x, y=y, fill=value)) +
  geom_tile() +
  coord_fixed(ratio=2) +
  ggtitle("ggplot: coord_fixed(ratio=2)") +
  theme_minimal()
print(p4)

# Test 3: aspect = 0.5 (twice as wide as tall)
cat("\n=== Test 3: aspect = 0.5 ===\n")
cat("Each cell should be 2x wider than tall\n\n")

# Lattice with aspect=0.5
p5 <- levelplot(value ~ x*y, data=test_data,
                aspect=0.5,
                main="Lattice: aspect=0.5",
                scales=list(draw=TRUE),
                colorkey=FALSE)
print(p5)

# ggplot with aspect=0.5
p6 <- ggplot(test_data, aes(x=x, y=y, fill=value)) +
  geom_tile() +
  coord_fixed(ratio=0.5) +
  ggtitle("ggplot: coord_fixed(ratio=0.5)") +
  theme_minimal()
print(p6)
