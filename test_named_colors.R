# Test Script for Named Vector Color Support in desplot/ggdesplot
# Issue #10: https://github.com/kwstat/desplot/issues/10
#
# This script tests named vector support for col.regions and col.text parameters

library(desplot)
library(agridat)

cat("=================================================================\n")
cat("Testing Named Vector Color Support - Issue #10\n")
cat("=================================================================\n\n")

# Use yates.oats dataset from agridat
data(yates.oats)

# Define colors for testing
my_colors <- c("skyblue", "pink", "lightgreen")
gen_levels <- levels(yates.oats$gen)
cat("Factor levels in data:", paste(gen_levels, collapse=", "), "\n\n")

# =============================================================================
# TEST 1: Named vectors with CORRECT order (forward)
# =============================================================================
cat("TEST 1: Named vector col.regions - FORWARD ORDER\n")
cat("-------------------------------------------------------\n")
named_colors_forward <- my_colors
names(named_colors_forward) <- gen_levels
cat("Named vector:\n")
print(named_colors_forward)

cat("\nTesting desplot()...\n")
try({
  p1 <- desplot(yates.oats, gen ~ col+row,
                col.regions=named_colors_forward,
                main="desplot: Named colors (forward)")
  print(p1)
  cat("✓ desplot with named colors (forward) succeeded\n")
})

cat("\nTesting ggdesplot()...\n")
try({
  p2 <- ggdesplot(yates.oats, gen ~ col+row,
                  col.regions=named_colors_forward,
                  main="ggdesplot: Named colors (forward)")
  print(p2)
  cat("✓ ggdesplot with named colors (forward) succeeded\n")
})

cat("\n")

# =============================================================================
# TEST 2: Named vectors with REVERSED order
# =============================================================================
cat("TEST 2: Named vector col.regions - REVERSED ORDER\n")
cat("-------------------------------------------------------\n")
named_colors_reversed <- my_colors
names(named_colors_reversed) <- rev(gen_levels)  # REVERSED!
cat("Named vector (reversed):\n")
print(named_colors_reversed)

cat("\nTesting desplot()...\n")
try({
  p3 <- desplot(yates.oats, gen ~ col+row,
                col.regions=named_colors_reversed,
                main="desplot: Named colors (reversed)")
  print(p3)
  cat("✓ desplot with named colors (reversed) succeeded\n")
  cat("  Colors should match by NAME, not position!\n")
})

cat("\nTesting ggdesplot()...\n")
try({
  p4 <- ggdesplot(yates.oats, gen ~ col+row,
                  col.regions=named_colors_reversed,
                  main="ggdesplot: Named colors (reversed)")
  print(p4)
  cat("✓ ggdesplot with named colors (reversed) succeeded\n")
  cat("  Colors should match by NAME, not position!\n")
})

cat("\n")

# =============================================================================
# TEST 3: Named vectors with PARTIAL names (should warn + fallback)
# =============================================================================
cat("TEST 3: Named vector col.regions - PARTIAL NAMES (should warn)\n")
cat("-------------------------------------------------------\n")
partial_colors <- c("red", "blue")
names(partial_colors) <- gen_levels[1:2]  # Only first 2 levels
cat("Named vector (partial - missing one level):\n")
print(partial_colors)

cat("\nTesting desplot()...\n")
cat("EXPECTED: Warning about missing level, fallback to positional\n")
try({
  p5 <- desplot(yates.oats, gen ~ col+row,
                col.regions=partial_colors,
                main="desplot: Partial names (should warn)")
  print(p5)
  cat("✓ desplot handled partial names (check for warning above)\n")
})

cat("\nTesting ggdesplot()...\n")
cat("EXPECTED: Warning about missing level, fallback to positional\n")
try({
  p6 <- ggdesplot(yates.oats, gen ~ col+row,
                  col.regions=partial_colors,
                  main="ggdesplot: Partial names (should warn)")
  print(p6)
  cat("✓ ggdesplot handled partial names (check for warning above)\n")
})

cat("\n")

# =============================================================================
# TEST 4: Named vectors with EXTRA names (should work, extras ignored)
# =============================================================================
cat("TEST 4: Named vector col.regions - EXTRA NAMES (should work)\n")
cat("-------------------------------------------------------\n")
extra_colors <- c("purple", "orange", "brown", "yellow")
names(extra_colors) <- c(gen_levels, "ExtraLevel")  # One extra name
cat("Named vector (with extra name):\n")
print(extra_colors)

cat("\nTesting desplot()...\n")
try({
  p7 <- desplot(yates.oats, gen ~ col+row,
                col.regions=extra_colors,
                main="desplot: Extra names (should work)")
  print(p7)
  cat("✓ desplot with extra names succeeded (extras ignored)\n")
})

cat("\nTesting ggdesplot()...\n")
try({
  p8 <- ggdesplot(yates.oats, gen ~ col+row,
                  col.regions=extra_colors,
                  main="ggdesplot: Extra names (should work)")
  print(p8)
  cat("✓ ggdesplot with extra names succeeded (extras ignored)\n")
})

cat("\n")

# =============================================================================
# TEST 5: UNNAMED vectors (backward compatibility)
# =============================================================================
cat("TEST 5: UNNAMED vector col.regions (backward compatibility)\n")
cat("-------------------------------------------------------\n")
unnamed_colors <- c("coral", "cyan", "gold")
cat("Unnamed vector:\n")
print(unnamed_colors)

cat("\nTesting desplot()...\n")
try({
  p9 <- desplot(yates.oats, gen ~ col+row,
                col.regions=unnamed_colors,
                main="desplot: Unnamed colors")
  print(p9)
  cat("✓ desplot with unnamed colors succeeded (positional matching)\n")
})

cat("\nTesting ggdesplot()...\n")
try({
  p10 <- ggdesplot(yates.oats, gen ~ col+row,
                   col.regions=unnamed_colors,
                   main="ggdesplot: Unnamed colors")
  print(p10)
  cat("✓ ggdesplot with unnamed colors succeeded (positional matching)\n")
})

cat("\n")

# =============================================================================
# TEST 6: Named vectors for col.text (outline colors)
# =============================================================================
cat("TEST 6: Named vector col.text (outline colors)\n")
cat("-------------------------------------------------------\n")
text_colors <- c("red", "blue", "green")
names(text_colors) <- rev(gen_levels)  # Reversed order
cat("Named vector for col.text (reversed):\n")
print(text_colors)

cat("\nTesting desplot()...\n")
try({
  p11 <- desplot(yates.oats, gen ~ col+row,
                 col=gen,
                 col.text=text_colors,
                 main="desplot: Named col.text")
  print(p11)
  cat("✓ desplot with named col.text succeeded\n")
})

cat("\nTesting ggdesplot()...\n")
try({
  p12 <- ggdesplot(yates.oats, gen ~ col+row,
                   col=gen,
                   col.text=text_colors,
                   main="ggdesplot: Named col.text")
  print(p12)
  cat("✓ ggdesplot with named col.text succeeded\n")
})

cat("\n")

# =============================================================================
# TEST 7: BOTH col.regions AND col.text with named vectors
# =============================================================================
cat("TEST 7: BOTH col.regions AND col.text with named vectors\n")
cat("-------------------------------------------------------\n")
fill_colors <- c("lightyellow", "lightblue", "lightpink")
names(fill_colors) <- gen_levels
outline_colors <- c("darkred", "darkblue", "darkgreen")
names(outline_colors) <- rev(gen_levels)  # Reversed!

cat("Named col.regions:\n")
print(fill_colors)
cat("\nNamed col.text (reversed):\n")
print(outline_colors)

cat("\nTesting desplot()...\n")
try({
  p13 <- desplot(yates.oats, gen ~ col+row,
                 col.regions=fill_colors,
                 col=gen,
                 col.text=outline_colors,
                 main="desplot: Both named colors")
  print(p13)
  cat("✓ desplot with both named colors succeeded\n")
})

cat("\nTesting ggdesplot()...\n")
try({
  p14 <- ggdesplot(yates.oats, gen ~ col+row,
                   col.regions=fill_colors,
                   col=gen,
                   col.text=outline_colors,
                   main="ggdesplot: Both named colors")
  print(p14)
  cat("✓ ggdesplot with both named colors succeeded\n")
})

cat("\n")

# =============================================================================
# SUMMARY
# =============================================================================
cat("=================================================================\n")
cat("TEST SUMMARY\n")
cat("=================================================================\n")
cat("All tests completed! Check the output above for:\n")
cat("  1. ✓ marks indicate successful execution\n")
cat("  2. Warnings about partial names (TEST 3)\n")
cat("  3. Visual confirmation that colors match correctly\n")
cat("\nKey verification:\n")
cat("  - TEST 2: Reversed names should show SAME colors as TEST 1\n")
cat("            (because matching by name, not position)\n")
cat("  - TEST 3: Should show warnings and fallback behavior\n")
cat("  - TEST 5: Should work exactly as before (backward compatible)\n")
cat("=================================================================\n")
