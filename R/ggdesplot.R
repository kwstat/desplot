# Ideas for ggplot version of desplot
if(0){
  ggdesplot( ~ col*row|county, data=besag.met, out1=rep, out2=block)
  ggdesplot(yield ~ col*row|county, data=besag.met, out1=rep, out2=block)
  ggdesplot(rep ~ col*row|county, data=besag.met)
  ggdesplot(rep ~ col*row|county, data=besag.met, text=rep)
  ggdesplot(rep ~ col*row|county, data=besag.met, num=rep)
  ggdesplot(rep ~ col*row|county, data=besag.met, col=rep)

  ggdesplot(rep ~ col*row|county, data=besag.met, ticks=TRUE)
  ggdesplot(rep ~ col*row|county, data=besag.met, ticks=TRUE, flip=TRUE)
                      
  ggdesplot(rep ~ col*row|county, data=besag.met, out1=rep, ticks=TRUE,
            main="BESAG", xlab="column", ylab="row")
  ggdesplot(rep ~ col*row|county, data=besag.met, out1=rep)
  ggdesplot(rep ~ col*row|county, data=besag.met, out1=rep, show.key=FALSE)
  ## num=NULL, col=NULL, text=NULL,
  ##                   col.regions=RedGrayBlue, col.text=NULL, text.levels=NULL,
  ##                   out1.gpar=list(col="black", lwd=3),
  ##                   out2.gpar=list(col="yellow", lwd=1, lty=1),
  ##                   at, midpoint="median",
  ##                   shorten='abb',
  ##                   key.cex, # left legend cex
  ##                   cex=.4, # cell cex
  ##                   strip.cex=.75, 
  ##                   subset=TRUE
}

#' @importFrom ggplot2 ggplot
#' @importFrom stats as.formula formula median
#' @export
ggdesplot <- function(form=formula(NULL ~ x + y), data,
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

  # Calculate 'lr' rows in legend, 'lt' legend text strings
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

  #browser()

  compute_borders <- function(data, x, y, facet, outline){
    # x,y,facet,outline are STRINGS naming the columns of data that are to
    # be used for x,y coordinates; facets; outline.
    # return a list of two dataframes:
    #   top has cell coordinates that have a border along the top
    #   right has ceel coordinates that have a border along the right
    
    # re-name columns for simplification
    names(data)[names(data)==x] <- "x"
    names(data)[names(data)==y] <- "y"
    names(data)[names(data)==facet] <- "facet"
    names(data)[names(data)==outline] <- "outline"
    
    topall <- rightall <- NULL
    # loop over facets
    for(ii in unique(data$facet)){
      datai <- data[data$facet == ii, ]
      datai <- datai[ , c('outline','x','y')]
      # create a matrix with the outline factor in each cell
      outi <- reshape2::melt(datai, id.var=c('x','y'))
      # reshape melts char vector to char, reshape2 melts to factor!
      # # both packages might be attached, so
      # call reshape2::melt and then conright to factor
      outi$value <- as.character(outi$value) # do I really need character?
      outi <- reshape2::acast(outi, y~x)
      # Careful.  The 'out' matrix is upside down from the levelplot
      
      # Since 'out' could be 1 row or column, surround it with NAs
      outi <- cbind(NA, rbind(NA, outi, NA), NA)
      
      # top borders
      top <- outi[2:nrow(outi)-1, ] != outi[2:nrow(outi), ]
      top <- reshape2::melt(top)
      top <- top[!(top$value==FALSE | is.na(top$value)),]
      if(nrow(top)>0) {
        #names(top) <- c('hy','hx')
        top$facet <- ii
        topall <- rbind(topall, top)
      }
      # right borders
      right <- outi[ , 2:ncol(outi)-1] != outi[ , 2:ncol(outi)]
      right <- reshape2::melt(right)
      right <- right[!(right$value==FALSE | is.na(right$value)),]
      if(nrow(right)>0) {
        #names(right) <- c('vy','vx')
        right$facet <- ii
        rightall <- rbind(rightall, right)
      }
    } # loop over facets
    names(topall) <- names(rightall) <- c("y","x","flag",facet)
    list(top=topall, right=rightall)
  }

  # ----- build the plot -----
  out <- ggplot(data, aes_string(x=x.var, y=y.var))
  if(!is.null(panel.var))
    out <- out + 
      facet_wrap(panel.var, scales="free")
  if(fill.type=="num")
    out <- out +
      geom_tile(aes_string(fill = fill.var)) +
      scale_fill_gradientn(colours=col.regions, guide="colorbar")
  if(fill.type=="factor"){
    #browser()
    out <- out +
      geom_tile(aes_string(fill = fill.var)) +
      scale_fill_manual(values=col.regions)
    
    # categorical fill colors
    # scale_fill_manual(values = c('A' = '#F8766D','C' = '#8ABF54','B' = '#C1DDA5'))+
  }
  if(has.out1) {
    borders <- compute_borders(data, x.var, y.var, panel.var, out1.var)
    out <- out + 
      geom_segment(data=borders$top,
                   aes(x=x-.5, xend=x+.5, y=y+.5, yend=y+.5),
                   col="black", lwd=1) + 
      geom_segment(data=borders$right,
                   aes(x=x+.5, xend=x+.5, y=y-.5, yend=y+.5),
                   col="black", lwd=1)
  }
  if(has.out2) {
    borders <- compute_borders(data, x.var, y.var, panel.var, out2.var)
    out <- out + 
      geom_segment(data=borders$top,
                   aes(x=x-.5, xend=x+.5, y=y+.5, yend=y+.5),
                   col="yellow", lwd=.5) + 
      geom_segment(data=borders$right,
                   aes(x=x+.5, xend=x+.5, y=y-.5, yend=y+.5),
                   col="yellow", lwd=.5)
  }
  if(has.text) { # fill text
    out = out + geom_text(aes_string(x.var, y.var, label=text.var), size=1)
  }
  #if(has.num)
  #  out = out + geom_text(aes_string(x.var, y.var, label=text.var), size=1)

  if(has.col){}

  if(!show.key)
    out <- out + theme(legend.position="none")
  
  # labels
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
    theme(axis.line = element_line(colour = "white"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank())
  out

} # end ggdesplot

if(0){
  
  libs(ggplot2)
  libs(proto)
  libs(grid)
  
  
  # multiple legends
  # https://stackoverflow.com/questions/18394391/r-custom-legend-for-multiple-layer-ggplot#18395012
  
  # example tufte geom
  #https://github.com/jrnold/ggthemes/blob/master/R/geom-tufteboxplot.R
  
  # how to create new geom
  # https://cran.r-project.org/web/packages/ggplot2/vignettes/extending-ggplot2.html

  # perfect example for desplot, but no facets
  # https://stackoverflow.com/questions/25704567/overlay-ggplot-grouped-tiles-with-polygon-border-depending-on-extra-factor

  # https://stackoverflow.com/questions/24578352/add-a-segment-only-to-one-facet-using-ggplot2



  # https://stackoverflow.com/questions/36156387/how-to-make-a-custom-ggplot2-geom-with-multiple-geometries

  GeomOutline <- ggproto("GeomOutline",
                         Geom,
                         setup_data = function(self, data, params) {
                           data <- ggproto_parent(Geom, self)$setup_data(data, params)
                           data
                         },
                         required_aes=c("x","y"))
  
  ggdesplot <- function(dat, form, out1=block){
    ggplot(besag.met, aes(col, row)) + 
      facet_wrap(~county, scales="free") + 
      geom_tile(aes(fill = yield)) + 
      scale_fill_gradientn(colours=RedGrayBlue(15), guide="colorbar") +
      theme(axis.line = element_line(colour = "white"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank())
  }
  ggdesplot(besag.met, yield~col*row)

  # ----------------------------------------------------------------------------
  
  # hard-coded, but outlines work with facets
  
  # https://stackoverflow.com/questions/25704567/overlay-ggplot-grouped-tiles-with-polygon-border-depending-on-extra-factor
  # https://stackoverflow.com/questions/11846295/how-to-add-different-lines-for-facets#11847210
  
  dat <- agridat::besag.met
  # give one county a different set of coordinates
  dat[dat$county=="C6", "col"] <- dat[dat$county=="C6", "col"] + 10
  dat[dat$county=="C6", "row"] <- dat[dat$county=="C6", "row"] + 10
  
  # make sure we have missing values in response
  dat[is.na(dat$yield),"rep"] <- NA
  
  horall <- vertall <- NULL
  # loop over facets
  for(ii in unique(dat$county)){
    dati <- subset(dat, county==ii)
    dati$x <- dati$col # need to fix these variable names
    dati$y <- dati$row
    dati <- dati[ , c('rep','x','y')]
    outi <- reshape2::melt(dati, id.var=c('x','y'))
    # reshape melts char vector to char, reshape2 melts to factor!
    # # both packages might be attached, so
    # call reshape2::melt and then convert to factor
    # do I really need character?
    outi$value <- as.character(outi$value)
    
    outi <- reshape2::acast(outi, y~x)
    # Careful.  The 'out' matrix is upside down from the levelplot
    
    # Since 'out' could be 1 row or column, surround it with NAs
    outi <- cbind(NA, rbind(NA, outi, NA), NA)
    
    # Horizontal lines above boxes
    hor <- outi[2:nrow(outi)-1, ] != outi[2:nrow(outi), ]
    hor <- reshape2::melt(hor)
    hor <- hor[!(hor$value==FALSE | is.na(hor$value)),]
    if(nrow(hor)>0) {
      names(hor) <- c('hy','hx')
      hor$county <- ii
      horall <- rbind(horall, hor)
    }
    # Vertical lines along right side of boxes
    vert <- outi[ , 2:ncol(outi)-1] != outi[ , 2:ncol(outi)]
    vert <- reshape2::melt(vert)
    vert <- vert[!(vert$value==FALSE | is.na(vert$value)),]
    if(nrow(vert)>0) {
      names(vert) <- c('vy','vx')
      vert$county <- ii
      vertall <- rbind(vertall, vert)
    }
  } # loop over facets
  
  oo <- ggplot(dat, aes(col, row)) + 
    facet_wrap(~county, scales="free") + 
    geom_tile(aes(fill = yield)) + 
    scale_fill_gradientn(colours=RedGrayBlue(15), guide="colorbar") +
    # categorical fill colors
    # scale_fill_manual(values = c('A' = '#F8766D','C' = '#8ABF54','B' = '#C1DDA5'))+
    geom_segment(data=horall, aes(x=hx-.5, xend=hx+.5, y=hy+.5, yend=hy+.5), lwd=1) + 
    geom_segment(data=vertall, aes(x=vx+.5, xend=vx+.5, y=vy-.5, yend=vy+.5), lwd=1) +
    # fill text
    geom_text(aes(col, row, label=dat$rep), size=1) +
    # remove axis ticks and labels
    
    theme(axis.text.x=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks=element_blank()) + 
    # labels
    xlab("") + 
    ylab("") + 
    theme(axis.line = element_line(colour = "white"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank())

}
