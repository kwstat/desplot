library(desplot)
library(agridat)

desplot(
  yates.oats,
  yield ~ col * row,
  col = gen,
  num = nitro,
  cex = 1,
  out1 = block,
  aspect = 1,
  gg = FALSE,
  main = "aspect=1, gg=FALSE"
)

desplot(
  yates.oats,
  yield ~ col * row,
  col = gen,
  num = nitro,
  cex = 1,
  out1 = block,
  aspect = 1,
  gg = TRUE,
  main = "aspect=1, gg=TRUE"
)


desplot(
  yates.oats,
  yield ~ col * row,
  col = gen,
  num = nitro,
  cex = 1,
  out1 = block,
  aspect = 0.5,
  gg = FALSE,
  main = "aspect=0.5, gg=FALSE"
)

desplot(
  yates.oats,
  yield ~ col * row,
  col = gen,
  num = nitro,
  cex = 1,
  out1 = block,
  aspect = 0.5,
  gg = TRUE,
  main = "aspect=0.5, gg=TRUE"
)

cat(
  "Testing with desplot version:",
  as.character(packageVersion("desplot")),
  "\n"
)

library(reprex)
reprex::reprex(venue = "gh")
