# Test Script for Named Vector Color Support - Issue #10
library(desplot)

# Simple test data: 2 rows x 3 columns, 3 factor levels
test_data <- data.frame(
  row = rep(1:2, each=3),
  col = rep(1:3, times=2),
  treat = factor(rep(c("A", "B", "C"), length.out=6))
)

test_that("named colors", {
  # TEST 1: Named vector - forward order
  my_colors <- c("skyblue", "pink", "lightgreen")
  names(my_colors) <- c("A", "B", "C")
  desplot(test_data, treat ~ col*row, col.regions=my_colors, main="Test 1: Named forward", gg=FALSE)
  desplot(test_data, treat ~ col*row, col.regions=my_colors, main="Test 1: Named forward (gg)", gg=TRUE)
  
  # TEST 2: Named vector - reversed order (KEY TEST from issue #10)
  my_colors_rev <- c("skyblue", "pink", "lightgreen")
  names(my_colors_rev) <- c("C", "B", "A")  # REVERSED!
  desplot(test_data, treat ~ col*row, col.regions=my_colors_rev, main="Test 2: Named reversed", gg=FALSE)
  desplot(test_data, treat ~ col*row, col.regions=my_colors_rev, main="Test 2: Named reversed (gg)", gg=TRUE)
  
  # TEST 3: Partial names (should warn and fallback)
  partial_colors <- c("red", "blue")
  names(partial_colors) <- c("A", "B")  # Missing C
  expect_warning(desplot(test_data, treat ~ col*row, col.regions=partial_colors, main="Test 3: Partial names", gg=FALSE))
  expect_warning(desplot(test_data, treat ~ col*row, col.regions=partial_colors, main="Test 3: Partial names (gg)", gg=TRUE))
  
  # TEST 4: Extra names (should work, extras ignored)
  extra_colors <- c("purple", "orange", "brown", "yellow")
  names(extra_colors) <- c("A", "B", "C", "D")  # D doesn't exist
  desplot(test_data, treat ~ col*row, col.regions=extra_colors, main="Test 4: Extra names", gg=FALSE)
  desplot(test_data, treat ~ col*row, col.regions=extra_colors, main="Test 4: Extra names (gg)", gg=TRUE)
  
  # TEST 5: Unnamed vector (backward compatibility)
  unnamed_colors <- c("coral", "cyan", "gold")
  desplot(test_data, treat ~ col*row, col.regions=unnamed_colors, main="Test 5: Unnamed", gg=FALSE)
  desplot(test_data, treat ~ col*row, col.regions=unnamed_colors, main="Test 5: Unnamed (gg)", gg=TRUE)
  
  # TEST 6: Named col.text (outline colors)
  text_colors <- c("red", "blue", "green")
  names(text_colors) <- c("C", "B", "A")  # Reversed
  desplot(test_data, treat ~ col*row, col=treat, col.text=text_colors, main="Test 6: Named col.text", gg=FALSE)
  desplot(test_data, treat ~ col*row, col=treat, col.text=text_colors, main="Test 6: Named col.text (gg)", gg=TRUE)
})
