
require(desplot)

# ----------------------------------------------------------------------------
# create data for tests

dat0 <- data.frame(loc = c('loc1','loc1','loc1','loc1',
                           'loc2','loc2','loc2','loc2','loc2','loc2'),
                   x=c(1,2,1,2, 1,2,3,1,2,3),
                   y=c(1,1,2,2, 1,1,1,2,2,2),
                   rep=c('R1','R1','R2','R2',' R1','R2','R3','R1','R2','R3'),
                   yield=c(9.29,11.20,9.36,9.89,8.47,9.17,8.86,10.48,10.22,11.29),
                   trt1=c('Treat1','Treat2','Treat2','Treat1',
                          'Trt1','Trt2','Trt1','Trt2','Trt1','Trt2'),
                   trt2=c('Hybrid1','Hybrid1','Hybrid2','Hybrid2','Hybrid1',
                          'Hybrid2','Hybrid3','Hybrid1','Hybrid2','Hybrid3'),
                   trt3=c("A","A","B","B","A","A","A","A","A","A"),
                   dq=c(0,0,0,1,2))
dat3 <- data.frame(
  yield = 7:1,
  x = c(5,  6,     1,  3,  4, 5, 5),
  y = c(2,  3,     1,  2,  3, 1, 2),
  loc = c("L1", "L1",   "L2", "L2", "L2", "L2", "L2"),
  block = c("B1","B1", "B1","B1","B2","B2","B2"))

require(agridat)
data(yates.oats, package="agridat")
oats35 <- yates.oats
oats35$dq <- rep(c(0,0,0,0,0,0,0,0,1,2), length=72)
# ----------------------------------------------------------------------------

# col.regions, at, midpoint

test_that("num,col,text", {
  expect_silent({
    desplot(oats35, ~col+row|block, cex=1, num=gen)
    desplot(oats35, ~col+row|block, cex=1, col=gen)
    desplot(oats35, ~col+row|block, cex=1, text=gen)
    desplot(oats35, ~col+row|block, cex=1, num=gen,col=nitro)
    desplot(oats35, ~col+row|block, cex=1, text=gen,col=nitro)
  })
})

test_that("function `cleanup` checks variables", {
  expect_error( desplot(oats35, yield~col+row, num=junk) )
  expect_error( desplot(oats35, yield~col+row, col=junk) )
  expect_error( desplot(oats35, yield~col+row, text=junk) )
  expect_error( desplot(oats35, yield~col+row, out1=junk) )
  expect_error( desplot(oats35, yield~col+row, out2=junk) )
})


test_that("shorten", {
  expect_silent({
    desplot(oats35, block~col+row, text=gen, cex=1) # default shorten='abb'
    desplot(oats35, block~col+row, text=gen, cex=1, shorten='abb')
    desplot(oats35, block~col+row, text=gen, cex=1, shorten='sub')
    desplot(oats35, block~col+row, text=gen, cex=1, shorten='no')
    desplot(oats35, block~col+row, text=gen, cex=1, shorten='none')
    desplot(oats35, block~col+row, text=gen, cex=1, shorten=FALSE)
  })
})

test_that("out1,out2,out1.gpar,out2.gpar", {
  expect_silent({
    desplot(oats35, yield~col+row, out1=block)
    desplot(oats35, yield~col+row, out2=block)
    desplot(oats35, yield~col+row, out1=block, out2=gen)
    desplot(oats35, yield~col+row,
            out1=block, out1.gpar=list(col="white",lwd=2))
    desplot(oats35, yield~col+row,
            out2=block, out2.gpar=list(col="deeppink",lwd=2))
    desplot(dat3, yield ~ x*y|loc, out1=block, out2=block) # no outline available
    # note, the following line gives a warning, I think because there are
    # no combinations of L1 & B2 ???
    # desplot(dat3, yield ~ x*y|loc, out1=block, gg=TRUE)
  })
})

test_that("dq", {
  desplot(oats35, block~col+row, out1=block, dq=dq)
  desplot(oats35, block~col+row|block, out1=block, dq=dq)
})

test_that("strip.cex", {
  expect_silent({
    desplot(dat0, yield ~ x+y|loc, strip.cex=1.5)
  })
})

test_that("text.levels", {
  expect_silent({
    desplot(oats35, block~col+row, col="nitro",
            text="gen", cex=1, text.levels=c('V','G','M'))
  })
})

test_that("key.cex, show.key", {
  expect_silent({
    desplot( dat0, ~ x+y|loc,
             text=trt1, key.cex=1, shorten='none')
    desplot( dat0, ~ x+y|loc,
             text=trt1, cex=.8, shorten='none', show.key=FALSE)
  })
})

test_that("text,xlab,ylab", {
  expect_silent({
    desplot(dat0, rep ~ x+y|loc,
            main="title", xlab="xlab", ylab="ylab")
  })
})

test_that("tick,flip", {
  expect_silent({
    desplot(oats35, yield~col+row, tick=TRUE)
    desplot(oats35, yield~col+row, tick=TRUE, flip=TRUE)  
  })
})

test_that("other arguments via ...", {
  expect_silent({
    desplot(dat0, rep ~ x+y|loc)
    desplot(dat0, rep ~ x+y|loc, aspect=1)
  })
})

test_that("subset", {
  expect_silent({
    desplot(oats35, ~col+row|block,
            subset = block %in% c("B1","B2"), cex=1, num=gen)
  })
})

# ----------------------------------------------------------------------------


desplot(dat0, yield ~ x+y|loc, col.regions=terrain.colors)

desplot(dat0, ~ x+y|loc,
        text=trt2, col=trt1, cex=1,
        col.text=c('red','black','blue','plum'),
        text.levels=c('A','B','C'))




oats34 <- oats35
# create a row of missing values in the field to test .addLevels
oats34[which(oats34$row==16),'yield'] <- NA
desplot(oats34, yield~col+row, tick=TRUE)
desplot(oats34, yield~col+row|block, tick=TRUE)

# Text over continuous colors
desplot(oats35, yield~col+row,
        out1=block, text=gen, cex=1,
        xlab="x axis", ylab="y axis", ticks=TRUE)

# Test 'at' and 'col.regions' for the ribbon
RedYellowBlue <-
  colorRampPalette(c("#D73027", "#F46D43", "#FDAE61", "#FEE090", "#FFFFBF",
                     "#E0F3F8", "#ABD9E9", "#74ADD1", "#4575B4"))
eightnum <- function(x) {
  x <- x[!is.na(x)]
  st <- grDevices::boxplot.stats(x)$stats
  # eps <- .Machine$double.eps
  eps <- 10^(log10(127.4)-6)
  c(min(x)-eps, st[1:2], st[3]-eps, st[3]+eps, st[4:5], max(x)+eps)
}

desplot(oats35, yield~col+row, col.regions=RedYellowBlue(7))
# extreme values barely visible on ribbon
desplot(oats35, yield~col+row, at=eightnum(oats35$yield), midpoint=NULL)

# fails
#desplot(oats35, yield~col+row, col.regions=RedYellowBlue(7),
#        at=eightnum(oats35$yield))

# Midpoint options
# mean=103.97
# median=102.5
# middle of range =113.5
desplot(oats35, yield~col+row) # default is median
desplot(oats35, yield~col+row, midpoint="median") # same as default
desplot(oats35, yield~col+row, midpoint=102.5) # same as default
desplot(oats35, yield~col+row, midpoint="midrange")
desplot(oats35, yield~col+row, midpoint=NULL) # same as midrange
desplot(oats35, yield~col+row, midpoint=113.5) # same as midrange
desplot(oats35, yield~col+row, midpoint=103.97) # custom, mean
desplot(oats35, yield~col+row, midpoint=0) # nonsensical low
desplot(oats35, yield~col+row, midpoint=200) # nonsensical hi

# Remove the ribbon completely
dd <- desplot(oats35, yield ~  col+row)
dd
dd$legend$right=NULL
dd

# What if the response is character?  Treat it like a factor
oats35$genchar <- as.character(oats35$gen)
desplot(oats35, gen~col+row, col=block, num=nitro, cex=1, out1=block)
desplot(oats35, genchar~col+row, col=block, num=nitro, cex=1, out1=block)

# Show actual yield values
desplot(oats35, block~col+row, text=yield, shorten='no')

desplot(oats35, block~col+row, col=nitro, text=gen, cex=1, out1=block)
desplot(oats35, block~col+row, col=nitro, text=gen, cex=1, out1=block, out2=gen)
desplot(oats35, block~col+row, num="gen", col="nitro", cex=1)

desplot(oats35, nitro~col+row, text="gen", cex=.9)
desplot(oats35, nitro~col+row)
desplot(oats35, nitro~col+row|block, text="gen", cex=.9)


desplot(oats35, nitro~col+row|block, text="gen", cex=1)
desplot(oats35, block~col+row|block, col="nitro", text="gen", cex=1)

# Test cases with 1 or 2 rows or columns

dmet <- agridat::besag.met

desplot(dmet, yield~col*row|county, out1=rep, out2=block, tick=TRUE)

# Create new data in which C1 has one row, C2 two rows
# C4 has one column, C5 two columns
dmet2 <- dmet
dmet2 <- subset(dmet2, !(county=="C1" & row<18))
dmet2 <- subset(dmet2, !(county=="C2" & row<17))
dmet2 <- subset(dmet2, !(county=="C4" & col<11))
dmet2 <- subset(dmet2, !(county=="C5" & col<10))

desplot(dmet2, yield~col*row|county, tick=TRUE)
# fixme
desplot(dmet2, yield~col*row|county, out1=rep, out2=block, tick=TRUE)

# Another midpoint example with strong difference between midpoint
# styles. Only works with agridat 1.13 or greater
if(FALSE){
  library(agridat)
  # The high outlier makes most of the field look red
  desplot(wiedemann.safflower.uniformity, yield~col*row,
          flip=TRUE, tick=TRUE, aspect =99/165, # true aspect
          main="wiedemann.safflower.uniformity - midpoint='midrange'",
          midpoint=NULL) # same as "midrange"
  # Using the median balances the blue/red colors better
  desplot(wiedemann.safflower.uniformity, yield~col*row,
          flip=TRUE, tick=TRUE, aspect =99/165, # true aspect
          main="wiedemann.safflower.uniformity - midpoint='median'",
          midpoint="median")
  desplot(wiedemann.safflower.uniformity, yield~col*row,
          flip=TRUE, tick=TRUE, aspect =99/165, # true aspect
          main="wiedemann.safflower.uniformity - eightnum",
          at=eightnum(wiedemann.safflower.uniformity$yield))

}

# ----------------------------------------------------------------------------
# ----------------------------------------------------------------------------

# ggdesplot
test_that("ggdesplot", {
  ggdesplot(oats35, ~ col+row|block, cex=1, num=gen)
} )

