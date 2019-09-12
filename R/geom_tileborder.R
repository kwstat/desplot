# geom_tileborder.R

if(0){
  ggplot(agridat::besag.met, aes(x=col, y=row)) +
    facet_wrap( ~ county) +
    geom_tile(aes(fill=yield)) +
    geom_tileborder(aes(group=1, grp=rep), lwd=1.5, lineend="round") +
    geom_tileborder(aes(group=1, grp=block), color="yellow", lwd=0.5)
  
    desplot(yield~col*row|county, agridat::besag.met, out1=rep, out2=block)
    ggdesplot(yield~col*row|county, agridat::besag.met, out1=rep, out2=block)
}

#' Borders between tiles
#' 
#' `geom_tileborder` draws a border between tiles of different classes.
#' The required aesthetics are `aes(x,y,grp)`, where `grp` is the grouping
#' classification that separates tiles.
#' 
#' Note, we cannot use `aes(group)` because it groups the interaction of
#' ALL discrete variables including facets.  Since we do not want to draw
#' a border between facets, we had to define a new aesthetic. 
#' See: # http://ggplot2.tidyverse.org/reference/aes_group_order.html
#' 
#' Also, we do not want to split the data into separate groups for each level 
#' of `grp`, so we need to include `aes(group=1)`.
#' 
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::geom_segment
#' @import ggplot2
#' @export
#' 
#' @examples
#' dd <- data.frame(
#'   x=c(1,2,1,2,3,1,2,1,2,3),
#'   y=c(2,2,2,2,2,1,1,1,1,1),
#'   loc=factor(c(1,1,2,2,2,1,1,2,2,2)),
#'   rep=factor(c(2,2,1,2,3,1,1,1,2,3)))
#' library(ggplot2)
#' ggplot(dd, aes(x=x, y=y)) +
#'   facet_wrap( ~ loc) +
#'   geom_tile(aes(fill=rep)) +
#'   geom_tileborder(aes(group=1, grp=rep), lwd=1.5)
#' # Compare to lattice version of desplot
#' # desplot::desplot(rep ~ x*y|loc, data=dd, out1=rep)
#' 
geom_tileborder<- function(mapping = NULL, data = NULL, geom = "segment",
                           position = "identity", na.rm = TRUE, 
                           show.legend = NA, 
                           inherit.aes = TRUE, ...) {
  ggplot2::layer(
    geom = geom,
    stat = StatTileBorder, 
    data = data, 
    mapping = mapping,  
    position = position, 
    show.legend = show.legend, 
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}


#' @rdname geom_tileborder
#' @format NULL
#' @usage NULL
#' @importFrom ggplot2 ggproto Stat
#' @export
StatTileBorder <-
  ggplot2::ggproto("StatTileBorder",
          ggplot2::Stat,
          required_aes=c("x","y","grp"),
          compute_group = function(data, scales) {
            # print(data) # so we can see the data groups
            #cat("hello\n")
            calc_borders(data$x, data$y, data$grp)
          })

calc_borders <- function(x,y,grp){
  # x,y: coordinates for tiles in a heatmap
  # grp: class identifier for each tile
  # Create a data frame with all line segments needed to draw borders between 
  # tiles in different grp. 
  # Note, NA values in grp are not considered a separate class, because we
  # do NOT want to outline missing values in the heatmap.

  # Create a matrix with the class membership in each cell.
  # Note, the 'mat' matrix is upside down from the levelplot
  dat <- data.frame(x=x, y=y, grp=grp)
  mat <- reshape2::acast(dat, y ~ x, value.var="grp")
  
  # Since 'mat' could be 1 row or column, surround it with NAs
  mat <- cbind(NA, rbind(NA, mat, NA), NA)
  
  # Find cells with top borders
  top <- mat[2:nrow(mat)-1, ] != mat[2:nrow(mat), ]
  top <- reshape2::melt(top)
  top <- top[!(top$value==FALSE | is.na(top$value)),]
  names(top) <- c('y','x','grp')
  # line segments above each cell
  top <- transform(top, x=x-.5, xend=x+.5, y=y+.5, yend=y+.5)
  
  # Find cells with right borders
  right <- mat[ , 2:ncol(mat)-1] != mat[ , 2:ncol(mat)]
  right <- reshape2::melt(right)
  right <- right[!(right$value==FALSE | is.na(right$value)),]
  names(right) <- c('y','x','grp')
  right <- transform(right, x=x+.5, xend=x+.5, y=y-.5, yend=y+.5)
  
  segs <- rbind(top,right)
  segs[,c("x","y","xend","yend")]
  if(nrow(segs)>0) segs else NULL
}
