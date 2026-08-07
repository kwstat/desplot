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

# 6 columns x 4 rows: x- and y-range differ, so "all" resolves per-axis and
# 'pretty' (2,4,6) is distinguishable from 'all' (1:6).
fld <- expand.grid(COLUMN = 1:6, ROW = 1:4)
fld$ENTRY <- factor(seq_len(nrow(fld)) %% 12 + 1)
xbreaks <- function(p) { b <- ggplot2::ggplot_build(p)$layout$panel_params[[1]]$x$breaks; b[!is.na(b)] }
ybreaks <- function(p) { b <- ggplot2::ggplot_build(p)$layout$panel_params[[1]]$y$breaks; b[!is.na(b)] }

test_that("ggdesplot does not add a spurious colour legend without 'col'", {
  skip_if_not(utils::packageVersion("ggplot2") >= "3.5.0")
  data(besag.met, package = "agridat")
  p <- ggdesplot(besag.met, yield ~ col * row|county, 
           text = gen)
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
  p <- ggdesplot(twocond, y ~ x * row | site + rep)
  b <- ggplot2::ggplot_build(p)
  expect_equal(length(unique(b$data[[1]]$PANEL)), 4L)
  # each panel holds exactly its 9 cells (no overplotting from a dropped factor)
  expect_true(all(table(b$data[[1]]$PANEL) == 9L))
})

test_that("ggdesplot single conditioning variable keeps its panel labels", {
  data(besag.met, package = "agridat")
  p <- ggdesplot(besag.met, yield ~ col * row | county)
  expect_equal(levels(p$data$.panel), paste0("C", 1:6))
})

test_that("ggdesplot leaves a cell with a missing value empty", {
  # 4x4 field with a hole at col 2 / row 2, once numeric and once a factor
  hole <- data.frame(
    col = rep(1:4, times = 4),
    row = rep(1:4, each = 4),
    yield = c(1:5, NA, 7:16),
    B = factor(c(rep("b1", 5), NA, rep("b2", 10)))
  )

  fill_num <- ggplot2::layer_data(ggdesplot(hole, yield ~ col * row), 1)$fill
  expect_equal(sum(fill_num == "transparent"), 1L)
  # the guard: the other 15 cells still get a real colour from col.regions,
  # so the empty cell comes from the missing value and not from a broken scale
  expect_equal(length(unique(fill_num[fill_num != "transparent"])), 15L)

  fill_fac <- ggplot2::layer_data(ggdesplot(hole, B ~ col * row), 1)$fill
  expect_equal(sum(fill_fac == "transparent"), 1L)
  expect_equal(length(unique(fill_fac[fill_fac != "transparent"])), 2L)
})

test_that("ggdesplot panel.border switch toggles the panel border and axis line", {
  p_on  <- ggdesplot(fld, ENTRY ~ COLUMN * ROW)
  p_off <- ggdesplot(fld, ENTRY ~ COLUMN * ROW, panel.border = FALSE)
  expect_true(inherits(p_on$theme$panel.border,  "element_rect"))   # default keeps it
  expect_true(inherits(p_off$theme$panel.border, "element_blank"))  # switch removes it
  expect_true(inherits(p_off$theme$axis.line,    "element_blank"))  # axis.line follows
})

test_that("ggdesplot ticks='all' puts a break at every integer, resolved per axis", {
  p <- ggdesplot(fld, ENTRY ~ COLUMN * ROW, ticks = "all")
  expect_equal(xbreaks(p), as.numeric(1:6))
  expect_equal(ybreaks(p), as.numeric(1:4))
  expect_false(inherits(p$theme$axis.text.x, "element_blank"))      # axes shown
  # flip keeps the same data-space breaks (scale_y_reverse negates the positions)
  p_flip <- ggdesplot(fld, ENTRY ~ COLUMN * ROW, ticks = "all", flip = TRUE)
  expect_true(setequal(abs(ybreaks(p_flip)), 1:4))
})

test_that("ggdesplot ticks=list gives explicit per-axis breaks", {
  p <- ggdesplot(fld, ENTRY ~ COLUMN * ROW, ticks = list(x = c(1, 3, 5), y = "all"))
  expect_equal(xbreaks(p), c(1, 3, 5))
  expect_equal(ybreaks(p), as.numeric(1:4))
  # a missing list element leaves that axis at the default (pretty) breaks, axes still shown
  p2 <- ggdesplot(fld, ENTRY ~ COLUMN * ROW, ticks = list(x = c(2, 4)))
  expect_equal(xbreaks(p2), c(2, 4))
  expect_equal(ybreaks(p2), ybreaks(ggdesplot(fld, ENTRY ~ COLUMN * ROW, ticks = TRUE)))
  expect_false(inherits(p2$theme$axis.text.x, "element_blank"))
})

test_that("ggdesplot logical ticks stay backward compatible", {
  p_f <- ggdesplot(fld, ENTRY ~ COLUMN * ROW, ticks = FALSE)
  p_t <- ggdesplot(fld, ENTRY ~ COLUMN * ROW, ticks = TRUE)
  expect_true(inherits(p_f$theme$axis.text.x,  "element_blank"))    # FALSE hides axes
  expect_false(inherits(p_t$theme$axis.text.x, "element_blank"))    # TRUE shows them
  expect_false(identical(xbreaks(p_t), as.numeric(1:6)))            # pretty (2,4,6), not 'all'
})

test_that("ggdesplot rejects an invalid ticks specification", {
  expect_error(ggdesplot(fld, ENTRY ~ COLUMN * ROW, ticks = "foo"))
  expect_error(ggdesplot(fld, ENTRY ~ COLUMN * ROW, ticks = list(z = 1)))
  expect_error(ggdesplot(fld, ENTRY ~ COLUMN * ROW, ticks = NA))
})

# ---------------------------------------------------------------------------
# Same 'ticks' / 'panel.border' features on the lattice path, so desplot() and
# ggdesplot() behave identically. The lattice trellis object stores the axis
# spec in p$x.scales$draw / $at and the panel box colour in
# p$par.settings$axis.line$col.
# ---------------------------------------------------------------------------

test_that("desplot ticks='all' puts a break at every integer, resolved per axis", {
  p <- desplot(fld, ENTRY ~ COLUMN * ROW, ticks = "all")
  expect_true(isTRUE(p$x.scales$draw))
  expect_equal(as.numeric(p$x.scales$at), as.numeric(1:6))
  expect_equal(as.numeric(p$y.scales$at), as.numeric(1:4))
  # flip keeps the same break values and does not error while building
  p_flip <- desplot(fld, ENTRY ~ COLUMN * ROW, ticks = "all", flip = TRUE)
  expect_equal(as.numeric(p_flip$y.scales$at), as.numeric(1:4))
})

test_that("desplot ticks=list gives explicit per-axis breaks", {
  p <- desplot(fld, ENTRY ~ COLUMN * ROW, ticks = list(x = c(1, 3, 5), y = "all"))
  expect_equal(as.numeric(p$x.scales$at), c(1, 3, 5))
  expect_equal(as.numeric(p$y.scales$at), as.numeric(1:4))
  # a missing list element leaves that axis at lattice's default (at = FALSE)
  p2 <- desplot(fld, ENTRY ~ COLUMN * ROW, ticks = list(x = c(2, 4)))
  expect_equal(as.numeric(p2$x.scales$at), c(2, 4))
  expect_identical(p2$y.scales$at, FALSE)
  expect_true(isTRUE(p2$x.scales$draw))
})

test_that("desplot logical ticks stay backward compatible", {
  p_f <- desplot(fld, ENTRY ~ COLUMN * ROW, ticks = FALSE)
  p_t <- desplot(fld, ENTRY ~ COLUMN * ROW, ticks = TRUE)
  expect_true(isFALSE(p_f$x.scales$draw))                         # FALSE hides axes
  expect_true(isTRUE(p_t$x.scales$draw))                          # TRUE shows them
  expect_false(identical(as.numeric(p_t$x.scales$at), as.numeric(1:6)))  # default breaks, not 'all'
})

test_that("desplot panel.border switch toggles the panel box and axis line", {
  desplot(besag.met, yield ~ col*row|county, panel.border=FALSE)
  p_on  <- desplot(fld, ENTRY ~ COLUMN * ROW)
  p_off <- desplot(fld, ENTRY ~ COLUMN * ROW, panel.border = FALSE)
  expect_false(identical(p_on$par.settings$axis.line$col, "transparent"))  # default keeps it
  expect_identical(p_off$par.settings$axis.line$col, "transparent")        # switch removes it
  # a par.settings the user passes through '...' survives the merge
  p_mix <- desplot(fld, ENTRY ~ COLUMN * ROW, panel.border = FALSE,
                   par.settings = list(strip.background = list(col = "grey90")))
  expect_identical(p_mix$par.settings$axis.line$col, "transparent")
  expect_identical(p_mix$par.settings$strip.background$col, "grey90")
})

test_that("desplot rejects an invalid ticks specification", {
  expect_error(desplot(fld, ENTRY ~ COLUMN * ROW, ticks = "foo"))
  expect_error(desplot(fld, ENTRY ~ COLUMN * ROW, ticks = list(z = 1)))
  expect_error(desplot(fld, ENTRY ~ COLUMN * ROW, ticks = NA))
})
