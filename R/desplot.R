# desplot.R
# Time-stamp: <09 Apr 2018 13:22:30 c:/x/rpack/desplot/R/desplot.R>

# TODO:
# If we have 'text' and shorten='no', don't bother with the key.

# Use tidyeval instead ?

# ----------------------------------------------------------------------------

#' Function to create a Red-Gray-Blue palette
#'
#' A function to create a Red-Gray-Blue palette.
#'
#' Using gray instead of white allows missing values to appear as white
#' (actually, transparent).
#'
#' @param n Number of colors to create
#' @return A vector of n colors.
#' @author Kevin Wright
#'
#' @examples
#' pie(rep(1,11), col=RedGrayBlue(11))
#' title("RedGrayBlue(11)")
#' @export
RedGrayBlue <- colorRampPalette(c("firebrick", "lightgray", "#375997"))

# ----------------------------------------------------------------------------

#' Plot the layout/data of a field experiment.
#'
#' Note, not all lattice parameters are passed down to \code{xyplot}, but it
#' is possible to make almost any change to the plot by assigning the
#' desplot object to a variable and then edit the object by hand or use
#' \code{update} to modify the object.  Then print it manually.  See the
#' first example below.
#'
#' Ryder (1981) discusses the need to examine the layout of the
#' experiment design, and not just the data.  This function provides a
#' a tool for plotting the layout of a field experiment and also the
#' observed data.
#'
#' Use \code{col.regions} to specify fill colors.  This can either be a vector
#' of colors or a function that produces a vector of colors.  If the response
#' variable is a factor and \code{col.regions} is a \emph{function}, it will
#' be ignored and the cells are filled with default light-colored backgrounds
#' and a key is placed on the left.  If the response variable is
#' \emph{numeric}, the cells are colored according to \code{col.regions}, and
#' a ribbon key is placed on the right.
#' 
#' The default argument \code{shorten='abb'} will shorten the cell text using
#' the \code{abbreviate} function.
#' Use \code{shorten='sub'} to use a 3-character substring.
#' Use \code{shorten='no'} or \code{shorten=FALSE} for no shortening.
#' 
#' Note that two sub-plots with identical levels of the split-plot factor can
#' be adjacent to each other by virtue of appearing in different whole-plots.
#' To correctly outline the split-plot factor, simply concatenate the
#' whole-plot factor and sub-plot factor together.
#'
#' To get a map of a field with a true aspect ratio, include 'aspect=ylen/xlen'
#' in the call, where 'ylen' is the vertical length of the field and 'xlen'
#' is the horizontal length of the field.
#' 
#' To call this function inside another function, you can hack like this:
#' vr <- "yield"; vx <- "x"; vy <- "y";
#' eval(parse(text=paste("desplot(", vr, "~", vx, "*", vy, ", data=yates.oats)")))
#' 
#' @param form A formula like \code{yield~x*y|location}. Note x,y are numeric.
#'
#' @param data A data frame.
#' 
#' @param num The name of the column of the data to use for plotting numbers.
#' 
#' @param col Column of the data for the color of the number shown in each cell.
#' 
#' @param text Column of the data to use for text labels shown in each cell.
#' 
#' @param out1 Column of the data to use for outlining around blocks of cells.
#' 
#' @param out2 Column of the data to use for outlining around blocks of cells.
#' 
#' @param col.regions Colors for the fill color of cells.
#' 
#' @param col.text Vector of colors for text strings.
#' 
#' @param text.levels Character strings to use instead of default 'levels'.
#' 
#' @param out1.gpar A list of graphics parameters for outlining.  Can either
#'   be an ordinary \code{list()} or A call to \code{gpar()} from the
#'   \code{grid} package.
#' 
#' @param out2.gpar Graphics parameters for the second level of outlining.
#' 
#' @param at Breakpoints for the color ribbon.  Use this instead of 'zlim'.
#' Note: using at causes midpoint to be set to NULL.
#'
#' @param midpoint Method to find midpoint of the color ribbon.
#' One of 'midrange', 'median, or a numeric value.
#' 
#' @param ticks If TRUE, show tick marks along the bottom and left sides.
#' 
#' @param flip If TRUE, vertically flip the image.
#' 
#' @param main Main title.
#' 
#' @param xlab Label for x axis.
#' 
#' @param ylab Label for y axis.
#' 
#' @param shorten Method for shortening text in the key, either 'abb', 'sub', 'no', or FALSE.
#' 
#' @param show.key If TRUE, show the key on the left side. (This is not the ribbon.)
#' 
#' @param key.cex Left legend cex.
#'
#' @param cex Expansion factor for text/number in each cell.
#' 
#' @param strip.cex Strip cex.
#' 
#' @param subset An expression that evaluates to logical index vector for subsetting the data.
#' 
#' @param ... Other.
#' 
#' @return A lattice object
#' 
#' @author Kevin Wright
#' 
#' @references
#' 
#' K. Ryder (1981). Field plans: why the biometrician finds them useful.
#' \emph{Experimental Agriculture}, 17, 243--256.
#' 
#' @import grid
#' @import lattice
#' @import reshape2
#' @importFrom stats as.formula formula median
#' @export
#' @rdname desplot
#' 
#' @examples
#' if(require(agridat)){
#' 
#' # Show how to customize any feature.  Here: make the strips bigger.
#' data(besag.met)
#' dat <- besag.met
#' d1 <- desplot(yield ~ col*row|county, dat, main="besag.met",
#'               out1=rep, out2=block, out2.gpar=list(col="white"), strip.cex=2)
#' d1 <- update(d1, par.settings = list(layout.heights=list(strip=2)))
#' print(d1)
#' 
#' # Show experiment layout
#' data(yates.oats)
#' desplot(yield ~ col+row, yates.oats, out1=block, out2=gen)
#' 
#' desplot(block ~ col+row, yates.oats, col=nitro, text=gen, cex=1, out1=block,
#'         out2=gen, out2.gpar=list(col = "gray50", lwd = 1, lty = 1))
#' 
#'   
#' # Example from Ryder.
#' data(ryder.groundnut)
#' gnut <- ryder.groundnut
#' m1 <- lm(dry~block+gen, gnut)
#' gnut$res <- resid(m1)
#' # Note largest positive/negative residuals are adjacent
#' desplot(res ~ col + row, gnut, text=gen, cex=1,
#'         main="ryder.groundnut residuals from RCB model")
#' }
desplot <- function(form=formula(NULL ~ x + y), data,
                    num=NULL, col=NULL, text=NULL,
                    out1=NULL, out2=NULL,
                    col.regions=RedGrayBlue, col.text=NULL, text.levels=NULL,
                    out1.gpar=list(col="black", lwd=3),
                    out2.gpar=list(col="yellow", lwd=1, lty=1),
                    at, midpoint="median",
                    ticks=FALSE, flip=FALSE,
                    main=NULL, xlab, ylab,
                    shorten='abb',
                    show.key=TRUE,
                    key.cex, # left legend cex
                    cex=.4, # cell cex
                    strip.cex=.75, 
                    subset=TRUE, ...){

  # if(isTRUE(options()$desplot.ggplot))
  #   return(ggdesplot(form=form, data=data, num=num, col=col, text=text, 
  #                    out1=out1, out2=out2, 
  #                    col.regions=col.regions, col.text=col.text,
  #                    out1.gpar=out1.gpar, out2.gpar=out2.gpar,
  #                    at=at, midpoint=midpoint,
  #                    ticks=ticks, flip=flip, main=main, xlab=xlab, ylab=ylab,
  #                    shorten=shorten, show.key=show.key,
  #                    key.cex=key.cex, cex=cex, strip.cex=strip.cex,
  #                    subset=subset))
                          
  # Use data name for default title.  Do this BEFORE subset!
  if(missing(main)) main <- deparse(substitute(data))

  # based on subset() function
  ix <- if (missing(subset)) 
    rep_len(TRUE, nrow(data))
  else {
    e <- substitute(subset)
    ix <- eval(e, data, parent.frame())
    if (!is.logical(ix)) 
      stop("'subset' must be logical")
    ix & !is.na(ix)
  }
  data <- data[ix, ]
  
  
  # Using 'at' overrides 'midpoint'
  if(!missing(at) && !is.null(midpoint))
    midpoint <- NULL

  if(!missing(at) && is.vector(col.regions) &&
       ( length(at) !=  length(col.regions)+1 ) )
    stop("Length of 'at' must be 1 more than length of 'col.regions'\n")
  
  # Force character, in case we forgot to quote the argument.
  # This is non-standard evaluation.  Beware.
  
  dn <- names(data)
  cleanup <- function(x, dn){
    if(is.null(x)) return(x)

    if(!is.character(x)) x <- deparse(x)
    if(!is.element(x, dn))
      stop("Could not find '", x,"' in the data frame.")
    return(x)
  }
  num.var <- cleanup(substitute(num), dn)
  col.var <- cleanup(substitute(col), dn)
  text.var <- cleanup(substitute(text), dn)
  out1.var <- cleanup(substitute(out1), dn)
  out2.var <- cleanup(substitute(out2), dn)

  has.num <- !is.null(num.var)
  has.col <- !is.null(col.var)
  has.text <- !is.null(text.var)
  has.out1 <- !is.null(out1.var)
  has.out2 <- !is.null(out2.var)
  if(has.num & has.text) stop("Specify either 'num' or 'text'. Not both.")

  data <- droplevels(data) # In case the user called with subset(obj, ...)

  # Split a formula like: resp~x*y|cond into a list of text strings called
  # resp, xy (vector like 'x' '*' 'y') , cond ('cond' could be a vector)
  ff <- latticeParseFormula(form, data)
  ff <- list(resp = ff$left.name,
             xy = strsplit(ff$right.name, " ")[[1]],
             cond = names(ff$condition))
  if(length(ff$resp)==0L) ff$resp <- NULL

  fill.var <- ff$resp
  x.var <- ff$xy[1]
  y.var <- ff$xy[3]
  panel.var <- ff$cond[1]

  # If ticks are requested, add axis labels
  if (missing(xlab))
    xlab <- ifelse(ticks, x.var, "")
  if (missing(ylab))
    ylab <- ifelse(ticks, y.var, "")

  # Determine what fills the cells: nothing, character/factor, or numeric
  if(is.null(fill.var)) fill.type="none"
  else if (is.factor(data[[fill.var]]))
    fill.type <- "factor"
  else if (is.character(data[[fill.var]])){
    data[[fill.var]] <- as.factor(data[[fill.var]])
    fill.type <- "factor"
  } else {
    fill.type <- "num"
  }
  
  # Now get the fill values/length
  if(fill.type=="none") {
    fill.val <- rep(1, nrow(data))
    fill.n <- 1
    # Hack.  We need something to plot, call it .const
    form <- as.formula(paste(".const", form[[1]], deparse(form[[2]]), sep=""))
    data[['.const']] <- fill.val
  } else if(fill.type=="num"){
    fill.val <- data[[fill.var]]
  } else { # character/factor
    fill.val <- data[[fill.var]]
    fill.n <- nlevels(fill.val)
  }

  # Define fill colors and 'at' (if not given by the user)
  # at = # cut points for region colors
   if(fill.type=="none") {
    col.regions <- "transparent"
    at <- c(0.5,1.5)
  } else if(fill.type=="factor"){
    # If col.regions is a function, switch to default fill colors
    if(is.function(col.regions))
      col.regions <- c("#E6E6E6","#FFD9D9","#FFB2B2","#FFD7B2","#FDFFB2",
                       "#D9FFB2","#B2D6FF","#C2B2FF","#F0B2FF","#A6FFC9",
                       "#FF8C8C","#B2B2B2","#FFBD80","#BFFF80","#80BAFF",
                       "#9980FF","#E680FF","#D0D192","#59FF9C","#FFA24D",
                       "#FBFF4D","#4D9FFF","#704DFF","#DB4DFF","#808080",
                       "#9FFF40","#C9CC3D")
    col.regions <- rep(col.regions, length=fill.n)
    at <- c((0:fill.n)+.5)
  } else if(fill.type=="num") {
    if(missing(at) && is.null(midpoint)){
        nbins <- 15
        if(is.function(col.regions)) col.regions <- col.regions(nbins)
        # Use lel = lattice:::extend.limits to move breakpoints past ends of fill.val
        zrng <- lel(range(as.numeric(fill.val), finite = TRUE))
        at <- seq(zrng[1], zrng[2], length.out = 16)
    }
    if(missing(at) && midpoint=="median"){ # default case for continuous data
      if(is.function(col.regions)) {
        nbins <- 15
        col.regions <- col.regions(nbins)
      } else {
        nbins <- length(col.regions)
      }
      med <- median(fill.val, na.rm=TRUE)
      radius <- max(max(fill.val, na.rm=TRUE)-med,
                    med-min(fill.val, na.rm=TRUE)) + .Machine$double.eps
      zrng <- lel(range(c(med-radius, med+radius)))
      brks <- seq(zrng[1], zrng[2], length.out = nbins+1)
      binno <- as.numeric(cut(fill.val, breaks=brks)) # bin number for each fill.val
      # select only 'col.regions' and 'at' values we actually need
      minbin <- min(binno, na.rm=TRUE); maxbin <- max(binno, na.rm=TRUE)
      col.regions <- col.regions[minbin:maxbin]
      at <- brks[minbin:(maxbin+1)]
    }
    if(missing(at) && midpoint=="midrange"){ # halfway between min & max
      if(is.function(col.regions)) {
        nbins <- 15
        col.regions <- col.regions(nbins)
      } else {
        nbins <- length(col.regions)
      }
      med <- median(range(fill.val, na.rm=TRUE))
      radius <- max(max(fill.val, na.rm=TRUE)-med,
                    med-min(fill.val, na.rm=TRUE)) + .Machine$double.eps
      zrng <- lel(range(c(med-radius, med+radius)))
      brks <- seq(zrng[1], zrng[2], length.out = nbins+1)
      binno <- as.numeric(cut(fill.val, breaks=brks)) # bin number for each fill.val
      # select only 'col.regions' and 'at' values we actually need
      minbin <- min(binno, na.rm=TRUE); maxbin <- max(binno, na.rm=TRUE)
      col.regions <- col.regions[minbin:maxbin]
      at <- brks[minbin:(maxbin+1)]
    }
    if(missing(at) && is.numeric(midpoint)){
      if(is.function(col.regions)) {
        nbins <- 15
        col.regions <- col.regions(nbins)
      } else {
        nbins <- length(col.regions)
      }
      radius <- max(max(fill.val, na.rm=TRUE)-midpoint,
                    midpoint-min(fill.val, na.rm=TRUE)) + .Machine$double.eps
      zrng <- lel(range(c(midpoint-radius, midpoint+radius)))
      brks <- seq(zrng[1], zrng[2], length.out = nbins+1)
      binno <- as.numeric(cut(fill.val, breaks=brks)) # bin number for each fill.val
      # select only col.regions and at we actually need
      minbin <- min(binno, na.rm=TRUE); maxbin <- max(binno, na.rm=TRUE)
      col.regions <- col.regions[minbin:maxbin]
      at <- brks[minbin:(maxbin+1)]
    }
    if(!missing(at)){
      # user specified 'at' and 'col.regions'
      nbins <- length(at)-1
      if(is.function(col.regions)) col.regions <- col.regions(nbins)
    }
    
  } # end fill.type
  # comment: the Fields package defines breakpoints so that the first and last
  # bins have their midpoints at the minimum and maximum values in z
  # https://www.image.ucar.edu/~nychka/Fields/Help/image.plot.html
  
  # Text colors
  if(is.null(col.text))
    col.text <- c("black", "red3", "darkorange2", "chartreuse4",
                  "deepskyblue4", "blue", "purple4", "darkviolet", "maroon")

  # Change x/y from factor to numeric if needed.  Add missing x,y levels.
  fac2num <- function(x) as.numeric(levels(x))[x]
  if(is.factor(data[[x.var]])) data[[x.var]] <- fac2num(data[[x.var]])
  if(is.factor(data[[y.var]])) data[[y.var]] <- fac2num(data[[y.var]])
  data <- .addLevels(data, x.var, y.var, panel.var)

  # Check for multiple values
  if(is.null(panel.var)){
    tt <- table(data[[x.var]], data[[y.var]])
  } else {
    tt <- table(data[[x.var]], data[[y.var]], data[[panel.var]])
  }
  if(any(tt>1))
    warning("There are multiple data for each x/y/panel combination")

  # lr = legend row count
  # lt = legend text strings
  lr <- 0
  lt <- NULL

  if(has.out1){ # out1
    lr <- lr + 1
    lt <- c(lt, out1.var)
  }
  if(has.out2){ # out2
    lr <- lr + 1
    lt <- c(lt, out2.var)
  }
  if(has.out1 | has.out2) lr <- lr + 1 # blank line

  if(fill.type=="factor") { # fill
    lt.fill <- levels(fill.val)
    lr <- lr + 2 + fill.n
    lt <- c(lt, lt.fill)
  }

  if(has.num) { # number
    num.val <- factor(data[[num.var]])
    lt.num <- levels(num.val)
    num.n <- length(lt.num)
    lr <- lr + 2 + num.n
    lt <- c(lt, lt.num)
  }

  if(has.col) { # color
    col.val <- factor(data[[col.var]]) # In case it is numeric
    lt.col <- levels(col.val)
    col.n <- length(lt.col)
    lr <- lr + 2 + col.n
    lt <- c(lt, lt.col)
    if(length(col.text) < col.n) col.text <- rep(col.text, length=col.n)
  } else {
    col.val <- rep(1, nrow(data)) # No color specified, use black by default
  }

  if(has.text) { # text
    text.val <- factor(data[[text.var]]) # In case it is not a factor
    lt.text <- levels(text.val)
    text.n <- length(lt.text)
    lr <- lr + 2 + text.n
    lt <- c(lt, lt.text)
  }

  # Set up short version of text
  if(has.text & is.null(text.levels)){
    if(shorten=='no' | shorten=='none' | (is.logical(shorten) && !shorten))
      text.levels <- lt.text
    else if (shorten=='abb')
      text.levels <- abbreviate(lt.text, 2, method='both')
    else if (shorten=='sub')
      text.levels <- substring(lt.text, 1, 3)
  } else {
    # Nothing.  Why is this here?
  }

  # We might not have a key, even though it was requested
  if (lr==0) show.key <- FALSE

  # In function call we use 'list' instead of 'gpar' because gpar is not
  # exported from grid, so now fixup the class for out1.gpar, out2.gpar
  if(class(out1.gpar) != "gpar") class(out1.gpar) <- "gpar"
  if(class(out2.gpar) != "gpar") class(out2.gpar) <- "gpar"
  
  # ----- Now we can actually set up the legend grobs -----
  if(show.key) {
    longstring <- lt[which.max(nchar(lt))]
    if(missing(key.cex)) {
      if(lr < 30) key.cex <- 1
      else if(lr < 40) key.cex <- .75
      else key.cex <- 0.5
    }

    foo <- frameGrob(layout = grid.layout(nrow = lr, ncol = 2,
                       heights = unit(rep(key.cex, lr), "lines"),
                       widths = unit(c(1,1), c("cm","strwidth"),
                         data=list(NULL, longstring))))

    offset <- 1

    if(has.out1){ # outline
      foo <- placeGrob(foo, linesGrob(x = unit(c(.2, .8), "npc"),
                                      y = unit(.5, "npc"),
                                      gp=out1.gpar),
                       row = offset, col = 1)
      foo <- placeGrob(foo, textGrob(label = out1.var, gp=gpar(cex=key.cex)),
                       row = offset, col = 2)
      offset <- offset + 1
    }
    if(has.out2){
      foo <- placeGrob(foo, linesGrob(x=c(.2,.8), y=.5, gp=out2.gpar),
                       row = offset, col = 1)
      foo <- placeGrob(foo, textGrob(label = out2.var, gp=gpar(cex=key.cex)),
                       row = offset, col = 2)
      offset <- offset + 1
    }
    if(has.out1 | has.out2) offset <- offset + 1 # blank line

    if(fill.type=='factor') {  # fill
      foo <- placeGrob(foo, textGrob(label = fill.var, gp=gpar(cex=key.cex)),
                       row = offset, col = 2)
      for(kk in 1:fill.n){
        foo <- placeGrob(foo, rectGrob(width = 0.6,
                                       gp = gpar(col="#FFFFCC",
                                         fill=col.regions[kk], cex=key.cex)),
                         row = offset + kk, col = 1)
        foo <- placeGrob(foo, textGrob(label = lt.fill[kk],
                                       gp=gpar(cex=key.cex)),
                         row = offset+kk, col = 2)
      }
      offset <- offset + 1 + fill.n + 1
    }

    if(has.num) { # number
      foo <- placeGrob(foo, textGrob(label = num.var, gp=gpar(cex=key.cex)),
                       row = offset, col = 2)
      for(kk in 1:num.n){
        foo <- placeGrob(foo, textGrob(label = kk, gp=gpar(cex=key.cex)),
                         row = offset + kk, col = 1)
        foo <- placeGrob(foo, textGrob(label = lt.num[kk], gp=gpar(cex=key.cex)),
                         row = offset + kk, col = 2)
      }
      offset <- offset + 1 + num.n + 1
    }

    if(has.col) { # color
      foo <- placeGrob(foo, textGrob(label = col.var, gp=gpar(cex=key.cex)),
                       row = offset, col = 2)
      for(kk in 1:col.n){
        foo <- placeGrob(foo, pointsGrob(.5,.5, pch=19,
                                         gp=gpar(col=col.text[kk],
                                           cex=key.cex)),
                         row = offset + kk, col = 1)
        foo <- placeGrob(foo, textGrob(label = lt.col[kk], gp=gpar(cex=key.cex)),
                         row = offset + kk, col = 2)
      }
      offset <- offset + 1 + col.n + 1
    }

    if(has.text) { # text
      foo <- placeGrob(foo, textGrob(label = text.var, gp=gpar(cex=key.cex)),
                       row = offset, col = 2)
      for(kk in 1:text.n){
        foo <- placeGrob(foo, textGrob(label = text.levels[kk],
                                       gp=gpar(cex=key.cex)),
                         row = offset + kk, col = 1)
        foo <- placeGrob(foo, textGrob(label = lt.text[kk], gp=gpar(cex=key.cex)),
                         row = offset + kk, col = 2)
      }
      offset <- offset + 1 + text.n + 1
    }

  } else foo <- NULL

  # Cell text
  if(has.text) {
    cell.text <- text.levels[as.numeric(text.val)]
  } else if(has.num) {
    cell.text <- as.numeric(num.val)
  } else if(has.col) {
    cell.text <- rep("x", length=nrow(data))
  }

  out1.val <- if(has.out1) data[[out1.var]] else NULL
  out2.val <- if(has.out2) data[[out2.var]] else NULL
  
  out <-
    levelplot(form,
              data=data,
              out1f=out1.val, out1g=out1.gpar,
              out2f=out2.val, out2g=out2.gpar,
              flip=flip,
              col.regions=col.regions,
              colorkey = if(fill.type=="num") TRUE else FALSE,
              as.table=TRUE,
              at=at,
              legend=if(show.key) list(left=list(fun=foo)) else list(),
              main=main,
              xlab=xlab,
              ylab=ylab,
              scales=list(relation='free', # Different scales for each panel
                          draw=ticks # Don't draw panel axes
              ),
              prepanel = prepanel.desplot,
              panel=function(x, y, z, subscripts, groups, ...,
                             out1f, out1g, out2f, out2g){
                # First fill the cells and outline
                panel.outlinelevelplot(x, y, z, subscripts, at, ...,
                                       out1f=out1f, out1g=out1g,
                                       out2f=out2f, out2g=out2g)
                # Then, if we have numbers, colors, or text
                if(has.num|has.text|has.col)
                  panel.text(x[subscripts], y[subscripts],
                             cell.text[subscripts],
                             cex=cex,
                             col=col.text[as.numeric(col.val[subscripts])])
              },
              strip=strip.custom(par.strip.text=list(cex=strip.cex)), ...)

  # Use 'update' for any other modifications
  #if(!show.key) out <- update(out, legend=list(left=NULL))

  return(out)
}

prepanel.desplot <- function (x, y, subscripts, flip, ...) {
  # based on lattice:::prepanel.default.levelplot
  pad <- lattice.getOption("axis.padding")$numeric

  # Note: x and y are NOT factors

  if (length(subscripts) > 0) {
    x <- x[subscripts]
    y <- y[subscripts]

    ux <- sort(unique(x[is.finite(x)]))
    if ((ulen <- length(ux)) < 2)
      xlim <- ux + c(-1, 1)
    else {
      diffs <- diff(as.numeric(ux))[c(1, ulen - 1)]
      xlim <- c(ux[1] - diffs[1]/2, ux[ulen] + diffs[2]/2)
    }
    uy <- sort(unique(y[is.finite(y)]))
    if ((ulen <- length(uy)) < 2)
      ylim <- uy + c(-1, 1)
    else {
      diffs <- diff(as.numeric(uy))[c(1, ulen - 1)]
      ylim <- c(uy[1] - diffs[1]/2, uy[ulen] + diffs[2]/2)
    }

    # This is returned
    ret <- list(xlim = lel(xlim, prop = -pad/(1 + 2 * pad)),
                ylim = lel(ylim, prop = -pad/(1 + 2 * pad)),
                dx = length(ux),
                dy = length(uy))
    if(flip) ret$ylim <- rev(ret$ylim)

    ret
  }
  else {
    # This is the value of the prepanel.null() function
    list(xlim = rep(NA_real_, 2), ylim = rep(NA_real_, 2), dx = NA_real_,
        dy = NA_real_)
  }
}

#' Panel Function for desplot
#'
#' This is a panel function for \code{desplot} which fills cells with
#' a background color and adds outlines around blocks of cells.
#'
#' It does not add the text labels, numbers, or colors.
#'
#' The rule for determining where to draw outlines is to compare the
#' levels of the factor used for outlining.  If bordering cells have
#' different levels of the factor, then a border is drawn.  'NA' values
#' are ignored (otherwise, too many lines would be drawn).
#'
#' The code works, but is probably overkill and has not been streamlined.
#' 
#' @param x Coordinates
#' 
#' @param y Coordinates
#' 
#' @param z Value for filling each cell.
#' 
#' @param subscripts For compatability.
#' 
#' @param at Breakpoints for the colors.
#' 
#' @param ... Other
#' 
#' @param alpha.regions Transparency for fill colors. Not well tested.
#' 
#' @param out1f Factors to use for outlining.
#' 
#' @param out1g Factors to use for outlining.
#' 
#' @param out2f Graphics parameters to use for outlining.
#' 
#' @param out2g Graphics parameters to use for outlining.
#' 
#' @export 
#' @references
#' Derived from lattice::panel.levelplot
panel.outlinelevelplot <- function(x, y, z, subscripts, at,
                                   ...,
                                   alpha.regions = 1,
                                   out1f, out1g, out2f, out2g) {
  dots=list(...)
  col.regions=dots$col.regions
  # Based on panel.levelplot
  
  # parent function forces x,y to be numeric, not factors
  
  if (length(subscripts) == 0L) return()
  
  z <- as.numeric(z)
  zcol <- level.colors(z, at, col.regions, colors = TRUE)
  x <- x[subscripts]
  y <- y[subscripts]
  
  zlim <- range(z, finite = TRUE)
  z <- z[subscripts]
  zcol <- zcol[subscripts]
  
  ux <- sort(unique(x[!is.na(x)]))
  bx <- if (length(ux) > 1L) { # breakpoints
    c(3 * ux[1] - ux[2], ux[-length(ux)] + ux[-1],
      3 * ux[length(ux)] - ux[length(ux) - 1])/2
  } else ux + c(-0.5, 0.5)
  lx <- diff(bx) # lengths? I think this is same as rep(1, length(ux))
  cx <- (bx[-1] + bx[-length(bx)])/2  # centers
  
  uy <- sort(unique(y[!is.na(y)]))
  by <- if (length(uy) > 1) {
    c(3 * uy[1] - uy[2], uy[-length(uy)] + uy[-1],
      3 * uy[length(uy)] - uy[length(uy) - 1])/2
  } else uy + c(-0.5, 0.5)
  ly <- diff(by)
  cy <- (by[-1] + by[-length(by)])/2
  
  idx <- match(x, ux)
  idy <- match(y, uy)
  
  # Fill the cells
  grid.rect(x = cx[idx], y = cy[idy],
            width=lx[idx],
            height = ly[idy],
            default.units = "native",
            gp = gpar(fill = zcol, lwd = 1e-05, col="transparent",
                      alpha = alpha.regions))

  # Outline factor 1
  if(!is.null(out1f)){
    bb <- calc_borders(x, y, as.character(out1f[subscripts]))
    grid.segments(x0 = bb$x, y0=bb$y, x1=bb$xend, y1=bb$yend,
                  default.units="native", gp=out1g)
  }

  # Outline factor 2
  if(!is.null(out2f)){
    bb <- calc_borders(x, y, as.character(out2f[subscripts]))
    grid.segments(x0 = bb$x, y0=bb$y, x1=bb$xend, y1=bb$yend,
                  default.units="native", gp=out2g)
  }
  
  return()
}

.addLevels <- function(dat, xvar='x', yvar='y', locvar=NULL){
  # For each loc, we want x/y coords to be complete.
  # NO: 1,2,4.  YES: 1,2,3,4.
  # Add one NA datum for each missing x and each missing y
  # This does NOT completely fill in the grid (as needed by asreml)

  # Original values
  ox <- dat[[xvar]]
  oy <- dat[[yvar]]

  x.is.factor <- is.factor(ox)
  y.is.factor <- is.factor(oy)
  if(x.is.factor | y.is.factor) stop("FIXME: x or y are factors.")

  if(is.null(locvar)) {
    loclevs <- factor("1") # hack alert
  } else {
    oloc <- factor(dat[[locvar]]) # In case loc is character
    loclevs <- levels(oloc)
  }

  for(loc.i in loclevs){

    if(is.null(locvar)){
      ux <- sort(unique(ox))
      uy <- sort(unique(oy))
    } else {
      ux <- sort(unique(ox[oloc==loc.i]))
      uy <- sort(unique(oy[oloc==loc.i]))
    }
    # Add new rows and columns. Fill with missing data
    xnew <- setdiff(seq(from=min(ux), to=max(ux), by=1), ux)
    ynew <- setdiff(seq(from=min(uy), to=max(uy), by=1), uy)
    if(length(xnew) > 0){
      newrows <- nrow(dat) + 1:length(xnew)
      dat[newrows, xvar] <- xnew # R creates these rows
      if(!is.null(locvar))
        dat[newrows, locvar] <- rep(loc.i, length(xnew))
    }
    if(length(ynew) > 0){
      newrows <- nrow(dat) + 1:length(ynew)
      dat[newrows, yvar] <- ynew
      if(!is.null(locvar))
        dat[newrows, locvar] <- rep(loc.i, length(ynew))
    }
  }
  return(dat)
}

# lel is a very simple version of lattice:::extend.limits
lel <- function (lim, prop = lattice.getOption("axis.padding")$numeric) {

  if (lim[1] == lim[2])
    lim + 0.5 * c(-1, 1)
  else {
    d <- diff(as.numeric(lim))
    lim + prop * d * c(-1, 1)
  }
}
