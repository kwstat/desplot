# ggdesplot.R

# Currently the "outline" and "text" do not have guides.
# Perhaps this can be used for multiple scales:
# https://github.com/eliocamp/ggnewscale

if(0){
  
  # multiple legends
  # https://stackoverflow.com/questions/18394391/r-custom-legend-for-multiple-layer-ggplot#18395012
  
  # perfect example for desplot, but no facets
  # https://stackoverflow.com/questions/25704567/overlay-ggplot-grouped-tiles-with-polygon-border-depending-on-extra-factor
  
  # https://stackoverflow.com/questions/36156387/how-to-make-a-custom-ggplot2-geom-with-multiple-geometries
  
  # https://stackoverflow.com/questions/11846295/how-to-add-different-lines-for-facets#11847210
  
}

if(0){
  libs(agridat)
  ggdesplot(besag.met, ~ col*row|county)
  ggdesplot(besag.met, ~ col*row|county, col=block)
  ggdesplot(besag.met, ~ col*row|county, num=block)
  ggdesplot(besag.met,  ~ col*row|county, text=block)
  
  ggdesplot(besag.met, rep ~ col*row|county)
  ggdesplot(besag.met, rep ~ col*row|county, col=block, cex=.8)
  ggdesplot(besag.met, rep ~ col*row|county, num=block, cex=.8)
  ggdesplot(besag.met, rep ~ col*row|county, text=block, cex=.8)
  
  ggdesplot(besag.met, yield ~ col*row|county, out1=rep)
  ggdesplot(besag.met, yield ~ col*row|county, out2=block)
  ggdesplot(besag.met, yield ~ col*row|county, out1=rep, out2=block)
  
  ggdesplot(besag.met, rep ~ col*row|county, ticks=TRUE)
  ggdesplot(besag.met, rep ~ col*row|county, ticks=TRUE, flip=TRUE)
                      
  ggdesplot(besag.met, rep ~ col*row|county, out1=rep, ticks=TRUE,
            main="besag.met", xlab="column", ylab="row")

  ggdesplot(besag.met, rep ~ col*row|county, out1=rep, show.key=FALSE)
  ##                   col.regions=RedGrayBlue, col.text=NULL, text.levels=NULL,
  ##                   out1.gpar=list(col="black", lwd=3),
  ##                   out2.gpar=list(col="yellow", lwd=1, lty=1),
  ##                   at, midpoint="median",
  ##                   shorten='abb',
  ##                   key.cex, # left legend cex
  ##                   strip.cex=.75, 
  
}

# Note: To avoid a similar note from the CMD check about .data,
# use #' @importFrom rlang .data

#' @import ggplot2
#' @importFrom stats as.formula formula median
#' @importFrom rlang .data
#' @export
#' @rdname desplot
ggdesplot <- function(data, 
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
  
  # Assume num.string contains the name of a column in data. 
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

  if(has.col){
    data[[col.string]] <- factor(data[[col.string]]) # In case it is numeric
  } else {
    # We still need a column for color to pass to aes( color=.data[[col.string]] )
    col.string="no_color"
    data[[col.string]] <- factor(1)
  }
  
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
  # comment: the Fields package defines breakpoints so that the first and last
  # bins have their midpoints at the minimum and maximum values in z
  # https://www.image.ucar.edu/~nychka/Fields/Help/image.plot.html
  
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

  # Calculate 'lr' rows in legend, 'lt' legend text strings
  lr <- 0
  lt <- NULL

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

  # fixme, col.val not used anywhere
  if(has.col) { # color
    col.val <- data[[col.string]]
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
  #if(class(out1.gpar) != "gpar") class(out1.gpar) <- "gpar"
  #if(class(out2.gpar) != "gpar") class(out2.gpar) <- "gpar"
  class(out1.gpar) <- "gpar"
  class(out2.gpar) <- "gpar"

  # Cell text
  if(has.text) {
    data$cell.text <- text.levels[as.numeric(text.val)]
  } else if(has.num) {
    data$cell.text <- as.numeric(num.val)
  } else if(has.col) {
    data$cell.text <- rep("x", length=nrow(data))
  }

  # Data quality flag
  if(has.dq) {
    data$dq.val <- factor(data[[dq.string]])
    levels(data$dq.val) <- list("0"=c("0","G","Good"),
                           "1"=c("1","Q","Questionable"),
                           "2"=c("2","B","Bad","S","Suppressed"))
    data$dq.val <- as.numeric(as.character(data$dq.val))
  }

  # --------------- build the plot ---------------

  #out <- ggplot(data, aes_string(x=x.string, y=y.string))
  out <- ggplot(data, aes(x=.data[[x.string]], y=.data[[y.string]]))
  
  if(!is.null(panel.string))
    out <- out + 
      facet_wrap(panel.string, scales="free")
  
  if(fill.type=="num")
    out <- out +
    #geom_tile(aes_string(fill = fill.string)) +
    geom_tile(aes(fill = .data[[fill.string]])) +
    scale_fill_gradientn(colours=col.regions, guide="colorbar")
  
  if(fill.type=="factor")
    out <- out +
    #geom_tile(aes_string(fill = fill.string)) +
    geom_tile(aes(fill = .data[[fill.string]])) +
    scale_fill_manual(values=col.regions)
  
  if(has.out1)
    out <- out +
    #geom_tileborder(aes_string(group=1, grp=out1.string),
    geom_tileborder(aes(group=1, grp=.data[[out1.string]]),
                    lineend="round",
                    color=out1.gpar$col,
                    lwd=out1.gpar$lwd,
                    linetype=if(is.null(out1.gpar$lty)) 1 else out1.gpar$lty)

  if(has.out2)
    out <- out +
    #geom_tileborder(aes_string(group=1, grp=out2.string),
    geom_tileborder(aes(group=1, grp=.data[[out2.string]]),
                    color=out2.gpar$col,
                    lwd=out2.gpar$lwd,
                    linetype=if(is.null(out2.gpar$lty)) 1 else out2.gpar$lty)
  # use '4*cex' so that cex in lattice/ggplot2 is roughly the same size
  if(has.text|has.num|has.col) # cell text
    #out = out + geom_text(aes_string(x.string, y.string, 
    out = out + geom_text(aes(x=.data[[x.string]], y=.data[[y.string]], 
                              label=.data[["cell.text"]], color=.data[[col.string]]), 
                          size=4*cex) + 
    scale_color_manual(values=col.text)
  
  if(has.dq) {
    # Data quality indicator

    # draw diagonal line from lower-left to upper-right
    data$x1l = data[[x.string]]-.5
    data$x1r = data[[x.string]]+.5
    data$y1l = data[[y.string]]-.5
    data$y1r = data[[y.string]]+.5
    # need to use aes_string to prevent CRAN check
    # "no visible binding for global variable"
    # Same for data[data$dq.val ,] instead of subset
    out <- out +
      geom_segment(data=data[data$dq.val >= 1L, ],
                   #aes_string(x="x1l", xend="x1r", y="y1l", yend="y1r"),
                   aes(x=.data[["x1l"]], xend=.data[["x1r"]], y=.data[["y1l"]], yend=.data[["y1r"]]),
                   color="black")
    # draw diagonal line from upper-left to lower-right
    data$y1l = data[[y.string]]+.5
    data$y1r = data[[y.string]]-.5
    out <- out +
      geom_segment(data=data[data$dq.val >= 2L, ],
                   #aes_string(x="x1l", xend="x1r", y="y1l", yend="y1r"),
                   aes(x=.data[["x1l"]], xend=.data[["x1r"]], y=.data[["y1l"]], yend=.data[["y1r"]]),
                   color="black")
  }
  
  if(!show.key)
    out <- out + theme(legend.position="none")
  
  # axis labels
  out <- out +
    ggtitle(main) +
    theme(plot.title = element_text(hjust = 0.5)) + # center title
    xlab(xlab) + 
    ylab(ylab)
  
  if(flip)
    out <- out + scale_y_reverse()
  
  # remove axis ticks and labels
  if(!ticks)
    out <- out + 
      theme(axis.text.x=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks=element_blank())

  # blank theme
  out <- out +
    coord_cartesian(expand = FALSE) + # no extra space between facet heatmaps
    theme(#aspect.ratio = (18*2)/(11*1),
          axis.line = element_line(colour = "black"), # left/bottom border
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_rect(fill = NA, colour = "black"), # top/right
          panel.background = element_blank(),
          panel.spacing = unit(0, "lines"),   # space between panels
          strip.text = element_text(size = 11 * strip.cex)
          )
  
  out
}
