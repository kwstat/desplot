# Regression tests for ggdesplot() bug fixes
library(desplot)

# 2 sites x 2 reps, 3x3 grid; response constant within a panel, differs across.
twocond <- expand.grid(x = 1:3, row = 1:3,
                       site = c("S1", "S2"), rep = c("R1", "R2"))
twocond$site <- factor(twocond$site)
twocond$rep  <- factor(twocond$rep)
twocond$y    <- as.numeric(interaction(twocond$site, twocond$rep))

# 4 fill levels but names only cover two -> triggers positional fallback
partial <- data.frame(
  col = rep(1:4, times = 4),
  row = rep(1:4, each = 4),
  rep = factor(paste0("R", rep(1:4, length.out = 16)))
)

test_that("ggdesplot does not add a spurious colour legend without 'col'", {
  skip_if_not(utils::packageVersion("ggplot2") >= "3.5.0")
  data(besag.met, package = "agridat")
  p <- suppressWarnings(ggdesplot(besag.met, yield ~ col * row, text = gen))
  # the dummy 'no_color' aesthetic must not produce a drawn colour guide
  expect_null(ggplot2::get_guide_data(p, "colour"))
})

test_that("ggdesplot named col.regions fallback colours every level", {
  p <- suppressWarnings(
    ggdesplot(partial, rep ~ col * row, col.regions = c(R1 = "red", R2 = "blue")))
  b <- ggplot2::ggplot_build(p)
  # no tile should be left unfilled (grey NA) by the positional fallback
  expect_false(any(is.na(b$data[[1]]$fill)))
})

test_that("ggdesplot facets on every conditioning variable", {
  p <- suppressWarnings(ggdesplot(twocond, y ~ x * row | site + rep))
  b <- ggplot2::ggplot_build(p)
  expect_equal(length(unique(b$data[[1]]$PANEL)), 4L)
  # each panel holds exactly its 9 cells (no overplotting from a dropped factor)
  expect_true(all(table(b$data[[1]]$PANEL) == 9L))
})

test_that("ggdesplot single conditioning variable keeps its panel labels", {
  data(besag.met, package = "agridat")
  p <- suppressWarnings(ggdesplot(besag.met, yield ~ col * row | county))
  expect_equal(levels(p$data$.panel), paste0("C", 1:6))
})
