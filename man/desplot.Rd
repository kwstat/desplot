% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/desplot.r
\name{desplot}
\alias{desplot}
\title{Plot the layout/data of a field experiment.}
\usage{
desplot(form = formula(NULL ~ x + y), data, num = NULL, col = NULL,
  text = NULL, out1 = NULL, out2 = NULL, col.regions = RedGrayBlue,
  col.text = NULL, text.levels = NULL, out1.gpar = list(col = "black", lwd
  = 3), out2.gpar = list(col = "yellow", lwd = 1, lty = 1), at,
  ticks = FALSE, flip = FALSE, main = NULL, xlab, ylab, shorten = "abb",
  show.key = TRUE, key.cex, cex = 0.4, strip.cex = 0.75, ...)
}
\arguments{
\item{form}{A formula like \code{yield~x*y|location}}

\item{data}{A data frame}

\item{num}{The column of the data to use for plotting numbers}

\item{col}{Column of the data for the color of the number}

\item{text}{Column to use for text labels}

\item{out1}{Column to use for outlining}

\item{out2}{Column to use for outlining}

\item{col.regions}{Colors for regions}

\item{col.text}{Colors for text strings}

\item{text.levels}{Character strings to use instead of default 'levels'}

\item{out1.gpar}{A list of graphics parameters for outlining.  Can either be an ordinary \code{list()} or A call to \code{gpar()} from the \code{grid} package.}

\item{out2.gpar}{Second level of outlining.}

\item{at}{Breakpoints for the color ribbon}

\item{ticks}{If TRUE, show tick marks row/column}

\item{flip}{If TRUE, vertically flip the image}

\item{main}{Main title}

\item{xlab}{Label for x axis}

\item{ylab}{Label for y axis}

\item{shorten}{Method for shortening text in the key}

\item{show.key}{If TRUE, show the key}

\item{key.cex}{Left legend cex}

\item{cex}{Expansion factor for text/number in each cellExpansion factor for text/number in each cell}

\item{strip.cex}{Strip cex}

\item{...}{Other}
}
\value{
A lattice object
}
\description{
Note, not all lattice parameters are passed down to \code{xyplot}, but it
is possible to make almost any change to the plot by assigning the
desplot object to a variable and then edit the object by hand or use
\code{update} to modify the object.  Then print it manually.  See the
first example below.
}
\details{
Ryder (1981) discusses the need to examine the layout of the
experiment design, and not just the data.  This function provides a
a tool for plotting the layout of a field experiment and also the
observed data.

Use \code{col.regions} to specify fill colors.  This can either be a vector
of colors or a function that produces a vector of colors.  If the response
variable is a factor and \code{col.regions} is a \emph{function}, it will
be ignored and the cells are filled with default light-colored backgrounds
and a key is placed on the left.  If the response variable is
\emph{numeric}, the cells are colored according to \code{col.regions}, and
a ribbon key is placed on the right.

The default \code{shorten='abb'} will shorten the cell text using the
\code{abbreviate} function. Other choices include \code{shorten='sub'} to
use a 3-character substring, and \code{shorten='no'} for no shortening.

Note that two sub-plots with identical levels of the split-plot factor can
be adjacent to each other by virtue of appearing in different whole-plots.
To correctly outline the split-plot factor, simply concatenate the
whole-plot factor and sub-plot factor together.

To call this function inside another function, you can hack like this:
vr <- "yield"; vx <- "x"; vy <- "y";
eval(parse(text=paste("desplot(", vr, "~", vx, "*", vy, ", data=yates.oats)")))
}
\examples{
if(require(agridat)){
# Show how to customize any feature.  Here: make the strips bigger.
data(besag.met)
d1 <- desplot(yield ~ col*row|county, besag.met, main="besag.met",
              out1=rep, out2=block, out2.gpar=list(col="white"), strip.cex=3)
d1 <- update(d1, par.settings = list(layout.heights=list(strip=2)))
print(d1)

# Show experiment layout
data(yates.oats)
desplot(yield ~ x+y, yates.oats, out1=block, out2=gen)

desplot(block ~ x+y, yates.oats, col=nitro, text=gen, cex=1, out1=block,
        out2=gen, out2.gpar=list(col = "gray50", lwd = 1, lty = 1))

  
# Example from Ryder.
data(ryder.groundnut)
gnut <- ryder.groundnut
m1 <- lm(dry~block+gen, gnut)
gnut$res <- resid(m1)
# Note largest positive/negative residuals are adjacent
desplot(res ~ col + row, gnut, text=gen, cex=1,
        main="ryder.groundnut residuals from RCB model")
}
}
\author{
Kevin Wright
}
\references{
K. Ryder (1981). Field plans: why the biometrician finds them useful.
\emph{Experimental Agriculture}, 17, 243--256.
}
