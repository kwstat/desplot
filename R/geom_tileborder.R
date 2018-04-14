# geom_tileborder.R

#' Borders between tiles
#' 
#' `geom_tileborder` draws a border between tiles of different classes
#' 
#' @inheritParams layer
#' @inheritParams geom_segment
#' @param grp Classification variable for the tiles. 
#' @export
#' @examples
#' dd <- data.frame(
#'   x=c(1,2,1,2,3,1,2,1,2,3),
#'   y=c(2,2,2,2,2,1,1,1,1,1),
#'   loc=factor(c(1,1,2,2,2,1,1,2,2,2)),
#'   rep=factor(c(2,2,1,2,3,1,1,1,2,3)))
#' ggplot(dd, aes(x=x, y=y)) +
#' facet_wrap( ~ loc) +
#' geom_tile(aes(fill=rep)) +
#' geom_tileborder(aes(group=1, grp=rep), lwd=2)
#' # Compare to lattice package
#' # desplot::desplot(rep ~ x*y|loc, data=dd, out1=rep)
#' 
geom_tileborder<- function(mapping = NULL, data = NULL, geom = "segment",
                           position = "identity", na.rm = FALSE, 
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
#' @export
StatTileBorder <-
  ggproto("StatTileBorder",
          Stat,
          required_aes=c("x","y","grp"),
          compute_group = function(data, scales) {
            # print(data) # so we can see the data groups
            
            # x,y are coordinates for the tiles
            # grp 
            # compute a data frame with all line segments needed
            # to draw borders between tiles in each grp

            # create a matrix with the group membership in each cell
            mat <- reshape2::acast(data, y ~ x, value.var="grp")
            # Note, the 'out' matrix is upside down from the levelplot
      
            # Since 'mat' could be 1 row or column, surround it with NAs
            mat <- cbind(NA, rbind(NA, mat, NA), NA)
            
            # find cells with top borders
            top <- mat[2:nrow(mat)-1, ] != mat[2:nrow(mat), ]
            top <- reshape2::melt(top)
            top <- top[!(top$value==FALSE | is.na(top$value)),]
            names(top) <- c('y','x','grp')
            # line segments above each cell
            top <- transform(top,
                             x=x-.5, xend=x+.5, y=y+.5, yend=y+.5)
            
            # cells with right borders
            right <- mat[ , 2:ncol(mat)-1] != mat[ , 2:ncol(mat)]
            right <- reshape2::melt(right)
            right <- right[!(right$value==FALSE | is.na(right$value)),]
            names(right) <- c('y','x','grp')
            right <- transform(right,
                               x=x+.5, xend=x+.5, y=y-.5, yend=y+.5)
            
            segs <- rbind(top,right)
            segs[,c("x","y","xend","yend")]
      })


# http://ggplot2.tidyverse.org/reference/aes_group_order.html
# By default, the group is set to the interaction of all discrete variables in the
# plot. This often partitions the data correctly, but when it does not, or when
# no discrete variable is used in the plot, you will need to explicitly define the
# grouping structure, by mapping group to a variable that has a different value
# for each group.
# We do not want to split the data into separate groups for each grp



ggplot(agridat::besag.met, aes(x=col, y=row)) +
  facet_wrap( ~ county) +
  geom_tile(aes(fill=yield)) +
  geom_tileborder(aes(group=1, grp=rep), lwd=2, lineend="round") +
  geom_tileborder(aes(group=1, grp=block), color="yellow", lwd=0.5)
#desplot(yield~col*row|county, agridat::besag.met, out1=rep, out2=block)

