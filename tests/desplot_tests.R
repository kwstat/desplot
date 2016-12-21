
require(desplot)

dat0 <- data.frame(loc = c('loc1','loc1','loc1','loc1','loc2','loc2','loc2','loc2','loc2','loc2'),
                   x=c(1,2,1,2, 1,2,3,1,2,3),
                   y=c(1,1,2,2, 1,1,1,2,2,2),
                   rep=c('R1','R1','R2','R2',' R1','R2','R3','R1','R2','R3'),
                   yield=c(9.29, 11.20, 9.36, 9.89, 8.47, 9.17, 8.86, 10.48, 10.22, 11.29),
                   trt1=c('Treat1','Treat2','Treat2','Treat1','Trt1','Trt2','Trt1','Trt2','Trt1','Trt2'),
                   trt2=c('Hybrid1','Hybrid1','Hybrid2','Hybrid2','Hybrid1',
                          'Hybrid2','Hybrid3','Hybrid1','Hybrid2','Hybrid3'))

# Rcmd check does not like windows()
# windows(width=3, height=2)
desplot(yield ~ x+y|loc, data=dat0)
desplot(yield ~ x+y|loc, data=dat0, strip.cex=1.5)
desplot(yield ~ x+y|loc, data=dat0, col.regions=terrain.colors)

desplot( ~ x+y|loc, data=dat0, num=trt1, cex=1)
desplot( ~ x+y|loc, data=dat0, col=trt1, cex=1)
desplot( ~ x+y|loc, data=dat0, text=trt1, cex=.8)

desplot( ~ x+y|loc, data=dat0, text=trt1, cex=.8, shorten='none')
desplot( ~ x+y|loc, data=dat0, text=trt1, cex=.8, shorten='none', key.cex=.5)
desplot( ~ x+y|loc, data=dat0, text=trt1, cex=.8, shorten='none', show.key=FALSE)
desplot( ~ x+y|loc, data=dat0, text=trt2, col=trt1, cex=1,
        col.text=c('red','black','blue','plum'), text.levels=c('A','B','C'))

desplot(rep ~ x+y|loc, data=dat0, out1=rep)
desplot(rep ~ x+y|loc, data=dat0, out1=rep, out2=y)
desplot(rep ~ x+y|loc, data=dat0, out1=rep, flip=TRUE)
desplot(rep ~ x+y|loc, data=dat0, tick=TRUE)
desplot(rep ~ x+y|loc, data=dat0, main="title", xlab="xlab", ylab="ylab")
desplot(rep ~ x+y|loc, data=dat0, aspect=2)
dev.off()

require(agridat)

oats35 <- yates.oats
if(is.element("x",names(oats35)))
   oats35 <- transform(oats35, col=x, row=y)

desplot(yield~col+row, oats35)
desplot(yield~col+row|block, oats35)

# missing values
oats34 <- oats35
oats34[1,'yield'] <- NA
desplot(yield~col+row|block, oats34)

# Text over continuous colors
desplot(yield~col+row, oats35, out1=block, text=gen, cex=1,
        xlab="x axis", ylab="y axis", ticks=TRUE)

desplot(yield~col+row, oats35, out2=block)
desplot(yield~col+row, oats35, out1=block, out2=gen)
desplot(gen~col+row, oats35, col=block, num=nitro, cex=1, out1=block)

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

desplot(yield~col+row, oats35, col.regions=RedYellowBlue(7))
# extreme values barely visible on ribbon
desplot(yield~col+row, oats35, at=eightnum(oats35$yield), midpoint=NULL)

# fails
#desplot(yield~col+row, oats35, col.regions=RedYellowBlue(7),
#        at=eightnum(oats35$yield))

# Midpoint options
# midpoint
# mean=103.97
# median=102.5
# mid=113.5
desplot(yield~col+row, oats35)
desplot(yield~col+row, oats35, midpoint=113.5) # same as default
desplot(yield~col+row, oats35, midpoint="median")
desplot(yield~col+row, oats35, midpoint=102.5) # same as median
#desplot(yield~col+row, oats35, midpoint="mean")
desplot(yield~col+row, oats35, midpoint=103.97)

# Remove the ribbon completely
dd <- desplot(yield ~  col+row, oats35)
dd
dd$legend$right=NULL
dd

# What if the response is character?  Treat it like a factor
oats35$genchar <- as.character(oats35$gen)
desplot(gen~col+row, oats35, col=block, num=nitro, cex=1, out1=block)
desplot(genchar~col+row, oats35, col=block, num=nitro, cex=1, out1=block)

# Test abbreviations
desplot(block~col+row, oats35, col=nitro, text=gen, cex=1) # default shorten='abb'
desplot(block~col+row, oats35, col=nitro, text=gen, cex=1, shorten='abb')
desplot(block~col+row, oats35, col=nitro, text=gen, cex=1, shorten='sub')
desplot(block~col+row, oats35, col=nitro, text=gen, cex=1, shorten='no')
desplot(block~col+row, oats35, col=nitro, text=gen, cex=1, shorten='none')
desplot(block~col+row, oats35, col=nitro, text=gen, cex=1, shorten=FALSE)

# Show actual yield values
desplot(block~col+row, oats35, text=yield, shorten='no')

desplot(block~col+row, oats35, col=nitro, text=gen, cex=1, out1=block)
desplot(block~col+row, oats35, col=nitro, text=gen, cex=1, out1=block, out2=gen)
desplot(block~col+row, oats35, num="gen", col="nitro", cex=1)

# Test custom labels
desplot(block~col+row, oats35, text="gen", col="nitro", cex=1, text.levels=c('V','G','M'))
desplot(nitro~col+row, oats35, text="gen", cex=.9)
desplot(nitro~col+row, oats35)
desplot(nitro~col+row|block, oats35, text="gen", cex=.9)

# No fill color at all
desplot(~col+row|block, oats35, text="gen", cex=1)
desplot(~col+row, oats35, col="gen", cex=1)

desplot(nitro~col+row|block, oats35, text="gen", cex=1)
desplot(block~col+row|block, oats35, col="nitro", text="gen", cex=1)

# Test cases with 1 or 2 rows or columns

# data(besag.met, package="agridat") # CRAN check doesn't like 'data' here
if(exists("besag.met")) dmet <- besag.met else dmet <- besag.corn

desplot(yield~col*row|county, dmet, out1=rep, out2=block, tick=TRUE)

# Create new data in which C1 has one row, C2 two rows
# C4 has one column, C5 two columns
dmet2 <- dmet
dmet2 <- subset(dmet2, !(county=="C1" & row<18))
dmet2 <- subset(dmet2, !(county=="C2" & row<17))
dmet2 <- subset(dmet2, !(county=="C4" & col<11))
dmet2 <- subset(dmet2, !(county=="C5" & col<10))

desplot(yield~col*row|county, dmet2, tick=TRUE)
desplot(yield~col*row|county, dmet2, out1=rep, out2=block, tick=TRUE)

if(FALSE){
  # Check the 'cleanup' function.  These all err (as they should)
  desplot(yield~col+row, oats35, num=junk)
  desplot(yield~col+row, oats35, col=junk)
  desplot(yield~col+row, oats35, text=junk)
  desplot(yield~col+row, oats35, out1=junk)
  desplot(yield~col+row, oats35, out2=junk)
}

# Another midpoint example with strong difference between midpoint
# styles. Only works with agridat 1.13 or greater
if(FALSE){
  library(agridat)
  # Old style
  desplot(yield~col*row, wiedemann.safflower.uniformity,
          flip=TRUE, tick=TRUE, aspect =99/165, # true aspect
          main="wiedemann.safflower.uniformity (true shape)",
          midpoint=NULL)
  desplot(yield~col*row, wiedemann.safflower.uniformity,
          flip=TRUE, tick=TRUE, aspect =99/165, # true aspect
          main="wiedemann.safflower.uniformity (true shape)",
          midpoint="median")
  }

