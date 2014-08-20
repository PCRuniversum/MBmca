\name{mcaPeaks}
\alias{mcaPeaks}
\title{
Function to estimate the approximate local minima and maxima of melting 
curve data.
}
\description{
The \code{mcaPeaks()} is used to estimate the approximate local minima and 
maxima of melting curve data. This can be useful to define a temperature 
range for melting curve analysis, quality control of the melting curve or 
to define a threshold of peak heights. Melting curves may consist of 
multiple significant and insignificant melting peaks. \code{mcaPeaks()} 
uses estimated the temperatures and fluorescence values of the local minima 
and maxima. The original data remain unchanged and only the approximate 
local minima and maxima are returned. \code{mcaPeaks()} takes modified code 
proposed earlier by Brian Ripley 
(https://stat.ethz.ch/pipermail/r-help/2002-May/021934.html). 
}

\usage{ mcaPeaks(x, y, span = 3) }

\arguments{
  \item{x}{ \code{x} is the column of a data frame for the temperature. }
  \item{y}{ \code{y} is the column of a data frame for the fluorescence 
	     values. }
  \item{span}{ \code{span} is used to adjust the window span. }
}
\value{ 
	\item{p.min }{returns a \code{data.frame} with the temperatures ("T 
(minima)") and the fluorescence ("F (minima)") for the local minima of the 
processed data. }
	
	\item{p.max }{returns a \code{data.frame} with the temperatures ("T 
(minima)") and the fluorescence ("F (minima)") for the local maxima of the 
processed data. }
}


\author{ Stefan Roediger }

\seealso{ \code{\link{mcaSmoother}}, \code{\link{smooth.spline}}}

\examples{
# First Example
data(DMP)
# Smooth and Min-Max normalize melting curve data with mcaSmoother()
tmp <- mcaSmoother(DMP[, 1], DMP[,6], minmax = TRUE, n = 2)

# Extract the first derivative melting curve data
tmp.d <- diffQ(tmp, verbose = TRUE)$xy

# Determine the approximate local minima and maxima of a curve
peak.val <-mcaPeaks(tmp.d[, 1], tmp.d[, 2])
peak.val

# Plot the first derivative melting curve
# Add a horizontal line and points for the approximate local minima 
# and maxima to the plot
plot(tmp.d, type = "l", 
     main = "Show the approximate local minima and maxima of a curve")
  abline(h = 0)
  points(peak.val$p.min, col = 1, pch = 19)
  points(peak.val$p.max, col = 2, pch = 19)
  legend(25, 0.08, c("Minima", "Maxima"), col = c(1,2), pch = c(19,19))

# Second example
# Signifcant peaks can be distinguished by peak hight
plot(tmp.d, type = "l", 
      main = "Show the approximate local minima and maxima of a curve")
  abline(h = 0)
  points(peak.val$p.min, col = 1, pch = 19)
  points(peak.val$p.max, col = 2, pch = 19)
  legend(25, 0.08, c("Minima", "Maxima"), col = c(1,2), pch = c(19,19))

# Test which local maxima peak is above the median + 3 * Median Absolute 
# Add a threshold (th) line to the plot
th.max <- median(peak.val$p.max[, 2]) + 3 * mad(peak.val$p.max[, 2])
abline(h = th.max, col = 3)

# add the approximate temperatures of the local minima and 
# maxima to the plot
T.val <- c(peak.val$p.min[, 1], peak.val$p.max[, 1])
text(T.val, rep(-0.05, length(T.val)), round(T.val,0))

# Use a temperature range from the plot to calculate the Tm of 
# the maximum Trange is used between 37 and 74 degree Celsius

tmp <- mcaSmoother(DMP[, 1], DMP[, 6], minmax = TRUE, Trange = c(37,74), 
		    n = 2)
# Tm 48.23, fluoTm 0.137
diffQ(tmp, fct = max, plot = TRUE)

}

\keyword{ smooth }
\keyword{ peaks }

