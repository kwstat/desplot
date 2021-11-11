# desplot.R

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
#' Use this function to plot the layout of a rectangular lattice
#' field experiment and also the observed data values.
#' 
#' To create the plot using lattice graphics: 
#' 1. \code{desplot(...)}.
#' 
#' To create the plot using ggplot2 graphics, use one of the following:
#' 1. \code{ggdesplot(...)}.
#' 2. \code{desplot(..., gg=TRUE)}. 
#' 3. \code{options(desplot.gg=TRUE); desplot(...)}. 
#' Method 3 is useful to modify all results from existing scripts.
#' 
#' The lattice version is complete, mature, and robust.
#' The ggplot2 version is incomplete.  The legend can only show colors, 
#' and some function arguments are ignored.
#' In general, lattice graphics are about 4-5 times faster than ggplot2 graphics.

#' Not all lattice parameters are passed down to \code{xyplot}, but it
#' is possible to make almost any change to the plot by assigning the
#' desplot object to a variable and then edit the object by hand or use
#' \code{update} to modify the object.  Then print it manually.  See the
#' first example below.
#'
#' Use \code{col.regions} to specify fill colors.  This can either be a vector
#' of colors or a function that produces a vector of colors.  If the response
#' variable is a factor and \code{col.regions} is a \emph{function}, it will
#' be ignored and the cells are filled with default light-colored backgrounds
#' and a key is placed on the left.  If the response variable is
#' \emph{numeric}, the cells are colored according to \code{col.regions}, and
#' a ribbon key is placed on the right.
#' 
#' Use \code{shorten='abb'} (this is default) to shorten the cell text using
#' the \code{abbreviate} function.
#' Use \code{shorten='sub'} to use a 3-character substring.
#' Use \code{shorten='no'} or \code{shorten=FALSE} for no shortening.
#' 
#' Note that two sub-plots with identical levels of the split-plot factor can
#' be adjacent to each other by virtue of appearing in different whole-plots.
#' To correctly outline the split-plot factor, simply concatenate the
#' whole-plot factor and sub-plot factor together.
#'
#' To get a map of a field with a true aspect ratio (lattice version only), 
#' include 'aspect=ylen/xlen'
#' in the call, where 'ylen' is the vertical length of the field and 'xlen'
#' is the horizontal length of the field.
#' 
#' To call this function inside another function, you can hack like this:
#' vr <- "yield"; vx <- "x"; vy <- "y";
#' eval(parse(text=paste("desplot(", vr, "~", vx, "*", vy, ", data=yates.oats)")))
#' 
#' @param data A data frame.
#' 
#' @param form A formula like \code{yield~x*y|location}. Note x,y are numeric.
#'
#' @param num Bare name (no quotes) of the column of the data to use 
#' as a factor for number-coding the text in each cell.
#' 
#' @param num.string String name of the column of the data to use 
#' as a factor for number-coding the text in each cell.
#' 
#' @param col Bare name (no quotes) of the column of the data to use
#' for color-coding the text shown in each cell.
#' 
#' @param col.string String name of the column of the data to use
#' for color-coding the text shown in each cell.
#' 
#' @param text Bare name (no quotes) of the column of the data to use 
#' for the actual text shown in each cell.
#' 
#' @param text.string String name of the column of the data to use 
#' for the actual text shown in each cell.
#' 
#' @param out1 Bare name (no quotes) of the column of the data to use 
#' for first-level outlining around blocks of cells.
#' 
#' @param out1.string String name of the column of the data to use 
#' for first-level outlining around blocks of cells.
#' 
#' @param out2 Bare name (no quotes) of the column of the data to use 
#' for second-level outlining around blocks of cells.
#' 
#' @param out2.string String name of the column of the data to use 
#' for second-level outlining around blocks of cells.
#' 
#' @param dq Bare name (no quotes) of the column of the data to use 
#' for indicating bad data quality with diagonal lines.
#' This can either be a numeric vector or a factor/text. 
#' Cells with 1/"Q"/"Questionable" have one diagonal line.
#' Cells with 2/"B"/"Bad","S","Suppressed" have crossed diagonal lines.
#' 
#' @param dq.string String name of the column of the data to use 
#' for indicating bad data quality with diagonal lines.
#' 
#' @param col.regions Colors for the fill color of cells.
#' 
#' @param col.text Vector of colors for text strings.
#' 
#' @param text.levels Character strings to use instead of default 'levels'.
#' 
#' @param out1.gpar A list of graphics parameters for first-level outlining.  
#' Can either be an ordinary \code{list()} or a call to \code{gpar()} from the
#'   \code{grid} package.
#' 
#' @param out2.gpar Graphics parameters for second-level of outlining.
#' 
#' @param at Breakpoints for the color ribbon.  Use this instead of 'zlim'.
#' Note: using 'at' causes 'midpoint' to be set to NULL.
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
#' @param gg If TRUE, desplot() switches to ggdesplot().
#' 
#' @param ... Other.
#' 
#' @return A lattice or ggplot2 object
#' 
#' @author Kevin Wright
#' 
#' @references
#' 
#' K. Ryder (1981).
#' Field plans: why the biometrician finds them useful.
#' \emph{Experimental Agriculture}, 17, 243--256.
#' 
#' @import grid
#' @import lattice
#' @importFrom reshape2 acast melt
#' @importFrom stats as.formula formula median
#' @export
#' @rdname desplot
#' 
#' @examples
#' if(require(agridat)){
#' 
#' # Show how to customize any feature.  Here: make the strips bigger.
#' data(besag.met)
#' d1 <- desplot(besag.met, 
#'               yield ~ col*row|county, 
#'               main="besag.met",
#'               out1=rep, out2=block, out2.gpar=list(col="white"), strip.cex=2)
#' d1 <- update(d1, par.settings = list(layout.heights=list(strip=2)))
#' print(d1)
#' 
#' # Show experiment layout
#' data(yates.oats)
#' desplot(yates.oats, 
#'         yield ~ col+row, 
#'         out1=block, out2=gen)
#' 
#' desplot(yates.oats, 
#'         block ~ col+row, 
#'         col=nitro, text=gen, cex=1, out1=block,
#'         out2=gen, out2.gpar=list(col = "gray50", lwd = 1, lty = 1))
#' 
#' }
desplot <- function(data, 
                    form=formula(NULL ~ x + y),
                    num=NULL, num.string=NULL,
                    col=NULL, col.string=NULL,
                    text=NULL, text.string=NULL,
                    out1=NULL, out1.string=NULL,
                    out2=NULL, out2.string=NULL,
                    dq=NULL, dq.string=NULL,
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
                    subset=TRUE, gg=FALSE, ...){

  # Would be nice to remove this code someday, maybe 2022?
  if(inherits(data, "formula")) {
    # Old style: desplot(form, data)
    # Use data name for default title.  Do this BEFORE subset!
    if(missing(main)) main <- deparse(substitute(form))
    tmp <- form
    form <- data
    data <- tmp
    message("Please use desplot(data,form) instead of desplot(form,data)")
  } else {
    # New style: desplot(data, form)
    # Use data name for default title.  Do this BEFORE subset!
    if(missing(main)) main <- deparse(substitute(data))
  }

  # subset, based on subset() function
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
  data <- droplevels(data) # In case the user called with subset(obj, ...)
  
  # Using 'at' overrides 'midpoint'
  if(!missing(at) && !is.null(midpoint))
    midpoint <- NULL

  if(!missing(at) && is.vector(col.regions) &&
       ( length(at) !=  length(col.regions)+1 ) )
    stop("Length of 'at' must be 1 more than length of 'col.regions'\n")
  
  # Assume num.string contains the name/string of a column in data. 
  # If num.string is NULL, then get its value by converting 'num' 
  # from a bare name to a string.  We MUST do this here
  # so that if we switch from desplot to ggdesplot, we can pass 
  # arguments as strings.  
  mc <- as.list(match.call())
  
  if(is.null(num.string)){
    if("num" %in% names(mc)) { # user did supply argument
      if(!is.character(mc$num) & !is.null(mc$num)) { # it is a bare name
        num.string <- deparse(substitute(num)) # evaluate to string
      }
    }
  }
  
  if(is.null(col.string)){
    if("col" %in% names(mc)) {
      if(!is.character(mc$col) & !is.null(mc$col)) {
        col.string <- deparse(substitute(col))
      }
    }
  }
  
  if(is.null(text.string)){
    if("text" %in% names(mc)) {
      if(!is.character(mc$text) & !is.null(mc$text)) {
        text.string <- deparse(substitute(text))
      }
    }
  }

  if(is.null(out1.string)){
    if("out1" %in% names(mc)) {
      if(!is.character(mc$out1) & !is.null(mc$out1)) {
        out1.string <- deparse(substitute(out1))
      }
    }
  }
  
  if(is.null(out2.string)){
    if("out2" %in% names(mc)) {
      if(!is.character(mc$out2) & !is.null(mc$out2)) {
        out2.string <- deparse(substitute(out2))
      }
    }
  }

  if(is.null(dq.string)){
    if("dq" %in% names(mc)) {
      if(!is.character(mc$dq) & !is.null(mc$dq)) {
        dq.string <- deparse(substitute(dq))
      }
    }
  }

  if(gg | isTRUE(options()$desplot.gg)) {
    #if (!requireNamespace("ggplot2")) 
    #  stop("You must first install the ggplot2 package: install.packages('ggplot2')")
    out <- ggdesplot(form=form, data=data, 
                     num.string=num.string, col.string=col.string, 
                     text.string=text.string, 
                     out1.string=out1.string, out2.string=out2.string,
                     dq.string=dq.string,
                     col.regions=col.regions, col.text=col.text,
                     out1.gpar=out1.gpar, out2.gpar=out2.gpar,
                     at=at, midpoint=midpoint,
                     ticks=ticks, flip=flip, main=main, xlab=xlab, ylab=ylab,
                     shorten=shorten, show.key=show.key,
                     key.cex=key.cex, cex=cex, strip.cex=strip.cex,
                     subset=subset)
    return(out)
  }
  
  dn <- names(data)
  checkvars <- function(x, dn){
    if(!is.null(x) && !is.element(x, dn))
      stop("Could not find '", x,"' in the data frame.")
  }
  checkvars(num.string, dn)
  checkvars(col.string, dn)
  checkvars(text.string, dn)
  checkvars(out1.string, dn)
  checkvars(out2.string, dn)
  checkvars(dq.string, dn)

  has.num <- !is.null(num.string)
  has.col <- !is.null(col.string)
  has.text <- !is.null(text.string)
  has.out1 <- !is.null(out1.string)
  has.out2 <- !is.null(out2.string)
  has.dq <- !is.null(dq.string)
  if(has.num & has.text) stop("Specify either 'num' or 'text'. Not both.")


  # Split a formula like: resp~x*y|cond into a list of text strings called
  # resp, xy (vector like 'x' '*' 'y') , cond ('cond' could be a vector)
  ff <- latticeParseFormula(form, data)
  ff <- list(resp = ff$left.name,
             xy = strsplit(ff$right.name, " ")[[1]],
             cond = names(ff$condition))
  if(length(ff$resp)==0L) ff$resp <- NULL

  fill.string <- ff$resp
  x.string <- ff$xy[1]
  y.string <- ff$xy[3]
  panel.string <- ff$cond[1]

  # If ticks are requested, add axis labels
  if (missing(xlab))
    xlab <- ifelse(ticks, x.string, "")
  if (missing(ylab))
    ylab <- ifelse(ticks, y.string, "")

  # Determine what fills the cells: nothing, character/factor, or numeric
  if(is.null(fill.string)) fill.type="none"
  else if (is.factor(data[[fill.string]]))
    fill.type <- "factor"
  else if (is.character(data[[fill.string]])){
    data[[fill.string]] <- as.factor(data[[fill.string]])
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
    fill.val <- data[[fill.string]]
  } else { # character/factor
    fill.val <- data[[fill.string]]
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

  # Text colors
  if(is.null(col.text))
    col.text <- c("black", "red3", "darkorange2", "chartreuse4",
                  "deepskyblue4", "blue", "purple4", "darkviolet", "maroon")

  # Change x/y from factor to numeric if needed.  Add missing x,y levels.
  fac2num <- function(x) as.numeric(levels(x))[x]
  if(is.factor(data[[x.string]])) data[[x.string]] <- fac2num(data[[x.string]])
  if(is.factor(data[[y.string]])) data[[y.string]] <- fac2num(data[[y.string]])
  data <- .addLevels(data, x.string, y.string, panel.string)

  # Check for multiple values
  if(is.null(panel.string)){
    tt <- table(data[[x.string]], data[[y.string]])
  } else {
    tt <- table(data[[x.string]], data[[y.string]], data[[panel.string]])
  }
  if(any(tt>1))
    warning("There are multiple data for each x/y/panel combination")

  # lr = legend row count
  # lt = legend text strings
  lr <- 0
  lt <- NULL

  # should I add a legend line for dq ?
  
  if(has.out1){ # out1
    lr <- lr + 1
    lt <- c(lt, out1.string)
  }
  if(has.out2){ # out2
    lr <- lr + 1
    lt <- c(lt, out2.string)
  }
  if(has.out1 | has.out2) lr <- lr + 1 # blank line

  if(fill.type=="factor") { # fill
    lt.fill <- levels(fill.val)
    lr <- lr + 2 + fill.n
    lt <- c(lt, lt.fill)
  }

  if(has.num) { # number
    num.val <- factor(data[[num.string]])
    lt.num <- levels(num.val)
    num.n <- length(lt.num)
    lr <- lr + 2 + num.n
    lt <- c(lt, lt.num)
  }

  if(has.col) { # color
    col.val <- factor(data[[col.string]]) # In case it is numeric
    lt.col <- levels(col.val)
    col.n <- length(lt.col)
    lr <- lr + 2 + col.n
    lt <- c(lt, lt.col)
    if(length(col.text) < col.n) col.text <- rep(col.text, length=col.n)
  } else {
    col.val <- rep(1, nrow(data)) # No color specified, use black by default
  }

  if(has.text) { # text
    text.val <- factor(data[[text.string]]) # In case it is not a factor
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
      foo <- placeGrob(foo, textGrob(label = out1.string, gp=gpar(cex=key.cex)),
                       row = offset, col = 2)
      offset <- offset + 1
    }
    if(has.out2){
      foo <- placeGrob(foo, linesGrob(x=c(.2,.8), y=.5, gp=out2.gpar),
                       row = offset, col = 1)
      foo <- placeGrob(foo, textGrob(label = out2.string, gp=gpar(cex=key.cex)),
                       row = offset, col = 2)
      offset <- offset + 1
    }
    if(has.out1 | has.out2) offset <- offset + 1 # blank line

    if(fill.type=='factor') {  # fill
      foo <- placeGrob(foo, textGrob(label = fill.string, gp=gpar(cex=key.cex)),
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
      foo <- placeGrob(foo, textGrob(label = num.string, gp=gpar(cex=key.cex)),
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
      foo <- placeGrob(foo, textGrob(label = col.string, gp=gpar(cex=key.cex)),
                       row = offset, col = 2)
      for(kk in 1:col.n){
        foo <- placeGrob(foo, pointsGrob(.5,.5, pch=19, # 19 = solid circle
                                         gp=gpar(col=col.text[kk],
                                           cex=key.cex)),
                         row = offset + kk, col = 1)
        foo <- placeGrob(foo, textGrob(label = lt.col[kk], gp=gpar(cex=key.cex)),
                         row = offset + kk, col = 2)
      }
      offset <- offset + 1 + col.n + 1
    }

    if(has.text) { # text
      foo <- placeGrob(foo, textGrob(label = text.string, gp=gpar(cex=key.cex)),
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
    cell.text <- rep("+", length=nrow(data))
  }
  
  # Data quality flag
  if(has.dq) {
    dq.val <- as.factor(data[[dq.string]])
    levels(dq.val) <- list("0"=c("0","G","Good"),
                           "1"=c("1","Q","Questionable"),
                           "2"=c("2","B","Bad","S","Suppressed"))
    dq.val <- as.numeric(as.character(dq.val))
  } else dq.val <- NULL
    
  out1.val <- if(has.out1) data[[out1.string]] else NULL
  out2.val <- if(has.out2) data[[out2.string]] else NULL
  
  out <-
    levelplot(form,
              data=data,
              out1f=out1.val, out1g=out1.gpar,
              out2f=out2.val, out2g=out2.gpar,
              dq=dq.val,
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
                             out1f, out1g, out2f, out2g, dq){
                # Fill the cells and outline
                panel.outlinelevelplot(x, y, z, subscripts, at, ...,
                                       out1f=out1f, out1g=out1g,
                                       out2f=out2f, out2g=out2g,
                                       dq=dq)

                # Numbers, colors, or text
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
#' @param out1f Factor to use for outlining (level 1).
#' 
#' @param out1g Factor to use for outlining (level 2).
#' 
#' @param out2f Graphics parameters to use for outlining.
#' 
#' @param out2g Graphics parameters to use for outlining.
#' 
#' @param dq Indicator of which cells should be flagged for data quality.
#' 
#' @export 
#' @references
#' None
#' 
panel.outlinelevelplot <- function(x, y, z, subscripts, at,
                                   ...,
                                   alpha.regions = 1,
                                   out1f, out1g, out2f, out2g,
                                   dq) {
  #  Derived from lattice::panel.levelplot
  dots=list(...)
  col.regions=dots$col.regions

  # parent function forces x,y to be numeric, not factors
  
  if (length(subscripts) == 0L) return()

  dq <- dq[subscripts]
  x <- x[subscripts]
  y <- y[subscripts]
  z <- as.numeric(z)
  zcol <- level.colors(z, at, col.regions, colors = TRUE)
  
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
  
  # Fill the cells with background color
  grid.rect(x = cx[idx], y = cy[idy],
            width=lx[idx],
            height = ly[idy],
            default.units = "native",
            gp = gpar(fill = zcol, lwd = 1e-05, 
                      col="transparent",
                      alpha = alpha.regions))

  # Data quality indicator
  gp=list(col="black", lwd=1)
  class(gp) <- "gpar"
  # draw diagonal line from lower-left to upper-right
  xd=x[dq >= 1L]
  yd=y[dq >= 1L]
  if(length(xd)>0) grid.segments(x0=xd-.5, y0=yd-.5,
                                 x1=xd+.5, y1=yd+.5, 
                                 default.units="native", gp=gp)
  # draw diagonal line from upper-left to lower-right
  xd=x[dq >= 2L]
  yd=y[dq >= 2L]
  if(length(xd)>0) grid.segments(x0=xd-.5, y0=yd+.5,
                                 x1=xd+.5, y1=yd-.5, 
                                 default.units="native", gp=gp)
  
  # Outline factor 1
  if(!is.null(out1f)){
    bb <- calc_borders(x, y, as.character(out1f[subscripts]))
    if(!is.null(bb) && nrow(bb)>0) {
      grid.segments(x0 = bb$x, y0=bb$y, x1=bb$xend, y1=bb$yend,
                    default.units="native", gp=out1g)
    }
  }

  # Outline factor 2
  if(!is.null(out2f)){
    bb <- calc_borders(x, y, as.character(out2f[subscripts]))
    if(!is.null(bb) && nrow(bb)>0){
      grid.segments(x0 = bb$x, y0=bb$y, x1=bb$xend, y1=bb$yend,
                    default.units="native", gp=out2g)
    }
  }
  
  return()
}

#' @noRd
.addLevels <- function(dat, xvar='x', yvar='y', locvar=NULL){
  # For each loc, we want x/y coords to be complete.
  # NO: 1,2,4.  YES: 1,2,3,4.
  # Add one NA datum for each missing x and each missing y
  # This does NOT completely fill in the rectangle (as needed by asreml)

  # Original values
  ox <- dat[[xvar]]
  oy <- dat[[yvar]]

  if( is.factor(ox) | is.factor(oy) )
    stop("FIXME: ", xvar, " or ", yvar, " are factors.")

  ## if(is.null(locvar)) {
  ##   loclevs <- factor("1") # hack alert
  ## } else {
  ##   oloc <- factor(dat[[locvar]]) # In case loc is character
  ##   loclevs <- levels(oloc)
  ## }

  ## for(loc.i in loclevs){

  ##   if(is.null(locvar)){
  ##     ux <- sort(unique(ox))
  ##     uy <- sort(unique(oy))
  ##   } else {
  ##     ux <- sort(unique(ox[oloc==loc.i]))
  ##     uy <- sort(unique(oy[oloc==loc.i]))
  ##   }
  ##   # Add new rows and columns. Fill with missing data
  ##   xnew <- setdiff(seq(from=min(ux), to=max(ux), by=1), ux)
  ##   ynew <- setdiff(seq(from=min(uy), to=max(uy), by=1), uy)
  ##   if(length(xnew) > 0){
  ##     newrows <- nrow(dat) + 1:length(xnew)
  ##     dat[newrows, xvar] <- xnew # R creates these rows
  ##     if(!is.null(locvar))
  ##       dat[newrows, locvar] <- rep(loc.i, length(xnew))
  ##   }
  ##   if(length(ynew) > 0){
  ##     browser()
  ##     newrows <- nrow(dat) + 1:length(ynew)
  ##     dat[newrows, yvar] <- ynew
  ##     if(!is.null(locvar))
  ##       dat[newrows, locvar] <- rep(loc.i, length(ynew))
  ##   }
  ## }

  # The old code above assumed locvar was character/factor, but still worked
  # if locvar was numeric because R was coercing the data when assigning the
  # locvar to the new row.  BUT, if dat was a tibble, then the coercion was
  # not working.  The code below fixes that problem and is slightly cleaner.
  
  if(is.null(locvar)) {
    # If there is no location variable, we use "1" as the location level
    uniquelocs <- "1"
  } else {
    oloc <- dat[[locvar]]
    uniquelocs <- unique( oloc )
  }

  for(loc.i in uniquelocs){
    # Index of data rows for this loc.
    if(is.null(locvar)){
      ix <- 1:nrow(dat)
    } else {
      ix <- which(dat[[locvar]] == loc.i)
    }
    
    # Add rows with new x values that were missing.
    ux <- unique(dat[ix, xvar])
    newx <- setdiff(seq(from=min(ux), to=max(ux), by=1), ux)
    n.newx <- length(newx)
    if(n.newx > 0){
      ix.new <- nrow(dat) + 1:n.newx
      dat[ix.new, xvar] <- newx # R creates these rows on the fly
      if(!is.null(locvar))
        dat[ix.new, locvar] <- rep(loc.i, n.newx)
    }
    # Same for y.
    uy <- unique(dat[ix, yvar])
    newy <- setdiff(seq(from=min(uy), to=max(uy), by=1), uy)
    n.newy <- length(newy)
    if(n.newy > 0){
      ix.new <- nrow(dat) + 1:n.newy
      dat[ix.new, yvar] <- newy
      if(!is.null(locvar))
        dat[ix.new, locvar] <- rep(loc.i, n.newy)
    }
  }
  
  return(dat)
}

#' @noRd
# lel is a very simple version of lattice:::extend.limits
lel <- function (lim, prop = lattice.getOption("axis.padding")$numeric) {

  if (lim[1] == lim[2])
    lim + 0.5 * c(-1, 1)
  else {
    d <- diff(as.numeric(lim))
    lim + prop * d * c(-1, 1)
  }
}
