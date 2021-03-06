% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/diffQ.R
\name{diffQ}
\alias{diffQ}
\title{Calculation of the melting temperature (Tm) from the first derivative}
\usage{
diffQ(
  xy,
  fct = min,
  fws = 8,
  col = 2,
  plot = FALSE,
  verbose = FALSE,
  warn = TRUE,
  peak = FALSE,
  negderiv = TRUE,
  deriv = FALSE,
  derivlimits = FALSE,
  derivlimitsline = FALSE,
  vertiline = FALSE,
  rsm = FALSE,
  inder = FALSE
)
}
\arguments{
\item{xy}{is a \code{data.frame} containing in the first column the
temperature and in the second column the fluorescence values. Preferably the
output from \code{mcaSmoother} is used.}

\item{fct}{accepts \code{min} or \code{max} as option and is used to define
whether to find a local minimum (``negative peak'') or local maximum
(``positive peak'').}

\item{fws}{defines the number (n) of left and right neighbors to use for the
calculation of the quadratic polynomial.}

\item{col}{is a graphical parameter used to define the length of the line
used in the plot.}

\item{plot}{shows a plot of a single melting curve. To draw multiple curves
in a single plot set \code{plot = FALSE} and create and empty plot instead
(see examples).}

\item{verbose}{shows additional information (e.g., approximate derivative,
ranges used for calculation, approximate Tm) of the calculation.}

\item{warn}{diffQ tries to keep the user as informed as possible about the
quality of the analysis. However, in some scenarios are the warning and
message about analysis not needed or disturbing.  \code{warn} can be used to
stop the swapping of the output.}

\item{peak}{shows the peak in the plot (see examples).}

\item{negderiv}{uses the positive first derivative instead of the negative.}

\item{deriv}{shows the first derivative with the color assigned to
\code{col} (see examples).}

\item{derivlimits}{shows the neighbors (fws) used to calculate the Tm as
points in the plot (see examples).}

\item{derivlimitsline}{shows the neighbors (fws) used to calculate the Tm as
line in the plot (see examples).}

\item{vertiline}{draws a vertical line at the Tms (see examples).}

\item{rsm}{performs a doubling of the temperature resolution by calculation
of the mean temperature and mean fluorescence between successive temperature
steps. Note: \code{mcaSmoother} has the "n" parameter with a similar but
advanced functionality.}

\item{inder}{Interpolates first derivatives using the five-point stencil.
See \code{chipPCR} package for details.}
}
\value{
\item{diffQ() }{returns a comprehensive list (if parameter verbose
is TRUE) with results from the first derivative. The list includes a
\code{data.frame} of the derivative ("xy"). The temperature range
("limits.xQ") and fluorescence range ("limits.diffQ") to calculate the peak
value. "fluo.x" is the approximate fluorescence at the approximate melting
temperature. The calculated melting temperature ("Tm") with the
corresponding fluorescence intensity ("fluoTm"). The number of neighbors
("fws"), the adjusted R-squared ("adj.r.squared") and the
normalized-root-mean-squared-error ("NRMSE") to fit. The quality of the
calculated melting temperature ("Tm") can be checked with devsum which
reports the relative deviation (in percent) between the approximate melting
temperature and the calculated melting temperature, if NRMSE is less than
0.08 and the adjusted R-squared is less than 0.85. A relative deviation
larger than 10 percent will result in a warning. Reducing fws might improve
the result. }
\item{Tm }{returns the calculated melting temperature ("Tm").}

\item{fluoTm }{returns the calculated fluorescence at the calculated melting
temperature.}

\item{Tm.approx }{returns the approximate melting temperature.}

\item{fluo.x }{returns the approximate fluorescence at the calculated
melting temperature.}

\item{xy }{returns the approximate derivative value used for the calculation
of the melting peak.}

\item{limits.xQ }{returns a data range of temperature values used to
calculate the melting temperature.}

\item{limits.diffQ }{returns a data range of fluorescence values used to
calculate the melting temperature.}

\item{adj.r.squared }{returns the adjusted R-squared from the quadratic
model fitting function (see also \code{fit}).}

\item{NRMSE }{returns the normalized root-mean-squared-error (NRMSE) from
the quadratic model fitting function (see also \code{fit}).}

\item{fws }{returns the number of points used for the calculation of the
melting temperature.}

\item{devsum }{returns measures to show the difference between the
approximate and calculated melting temperature.}

\item{temperature }{returns measures to investigate the temperature
resolution of the melting curve. Raw fluorescence measurements at irregular
temperature resolutions (intervals) can introduce artifacts and thus lead to
wrong melting point estimations.}

\item{temperature$T.delta }{returns the difference between two successive
temperature steps.}

\item{temperature$mean.T.delta }{returns the mean difference between two
temperature steps.}

\item{temperature$sd.T.delta }{returns the standard deviation of the
temperature.}

\item{temperature$RSD.T.delta }{returns the relative standard deviation
(RSD) of the temperature in percent.}

\item{fit }{returns the summary of the results of the quadratic model
fitting function.}
}
\description{
\code{diffQ} is used to calculate the melting temperature (Tm) but also for
elementary graphical operations (e.g., show the Tm or the derivative). It
does not require smoothed data for the MCA. The parameter \code{rsm} can be
used to double the temperature resolution by calculation of the mean
temperature and mean fluorescence. Note: mcaSmoother has the \code{n}
parameter with a similar functionality. First the approximate Tm is
determined as the \code{min()} and/or \code{max()} from the first
derivative. The first numeric derivative (Forward Difference) is estimated
from the values of the function values obtained during an experiment since
the exact function of the melting curve is unknown. The method used in
\code{diffQ} is suitable for independent variables that are equally and
unequally spaced. Alternatives for the numerical differentiation include
Backward Differences, Central Differences or Three-Point (Forward or
Backward) Difference based on Lagrange Estimation (currently not implemented
in \code{diffQ}). The approximate peak value is the starting-point for a
function based calculation. The function takes a defined number n (maximum
8) of the left and the right neighbor values and fits a quadratic
polynomial. The quadratic regression of the X (temperature) against the Y
(fluorescence) range gives the coefficients. The optimal quadratic
polynomial is chosen based on the highest adjusted R-squared value.
\code{diffQ} returns an objects of the class \code{list}. To accessing
components of lists is done as described elsewhere either by name or by
number. \code{diffQ} has a simple plot function. However, for sophisticated
analysis and plots its recommended to use \code{diffQ} as presented in the
examples as part of algorithms.
}
\examples{

# First Example
# Plot the first derivative of different samples for single melting curve
# data. Note that the argument "plot" is TRUE.

default.par <- par(no.readonly = TRUE)

data(MultiMelt)
par(mfrow = c(1,2))
sapply(2L:14, function(i) {
        tmp <- mcaSmoother(MultiMelt[, 1], MultiMelt[, i])
        diffQ(tmp, plot = TRUE)
  }
)
par(mfrow = c(1,1))
# Second example
# Plot the first derivative of different samples from MultiMelt
# in a single plot.
data(MultiMelt)

# First create an empty plot
plot(NA, NA, xlab = "Temperature", ylab ="-d(refMFI)/d(T)",
        main = "Multiple melting peaks in a single plot", xlim = c(65,85),
        ylim = c(-0.4,0.01), pch = 19, cex = 1.8)
# Prepossess the selected melting curve data (2,6,12) with mcaSmoother 
# and apply them to diffQ. Note that the argument "plot" is FALSE
# while other arguments like derivlimitsline or peak are TRUE. 
sapply(c(2,6,12), function(i) {
	tmp <- mcaSmoother(MultiMelt[, 1], MultiMelt[, i], 
			    bg = c(41,61), bgadj = TRUE)
	diffQ(tmp, plot = FALSE, derivlimitsline = TRUE, deriv = TRUE, 
	      peak = TRUE, derivlimits = TRUE, col = i, vertiline = TRUE)
  }
)
legend(65, -0.1, colnames(MultiMelt[, c(2,6,12)]), pch = c(15,15,15), 
	col = c(2,6,12))

# Third example
# First create an empty plot
plot(NA, NA, xlim = c(50,85), ylim = c(-0.4,2.5), 
     xlab = "Temperature", 
     ylab ="-refMFI(T) | refMFI'(T) | refMFI''(T)",
     main = "1st and 2nd Derivatives", 
     pch = 19, cex = 1.8)

# Prepossess the selected melting curve data with mcaSmoother 
# and apply them to diffQ and diffQ2. Note that 
# the argument "plot" is FALSE while other 
# arguments like derivlimitsline or peak are TRUE.

tmp <- mcaSmoother(MultiMelt[, 1], MultiMelt[, 2], 
		    bg = c(41,61), bgadj = TRUE)
lines(tmp, col= 1, lwd = 2)

# Note the different use of the argument derivlimits in diffQ and diffQ2
diffQ(tmp, fct = min, derivlimitsline = TRUE, deriv = TRUE, 
	    peak = TRUE, derivlimits = FALSE, col = 2, vertiline = TRUE)
diffQ2(tmp, fct = min, derivlimitsline = TRUE, deriv = TRUE, 
	    peak = TRUE, derivlimits = TRUE, col = 3, vertiline = TRUE)

# Add a legend to the plot
legend(65, 1.5, c("Melting curve",
		  "1st Derivative", 
		  "2nd Derivative"), 
		  pch = c(19,19,19), col = c(1,2,3))

# Fourth example
# Different curves may potentially have different quality in practice. 
# For example, using data from MultiMelt should return a 
# valid result and plot.
data(MultiMelt)

diffQ(cbind(MultiMelt[, 1], MultiMelt[, 2]), plot = TRUE)$Tm
# limits_xQ
#  77.88139

# Imagine an experiment that went terribly wrong. You would 
# still get an estimate for the Tm. The output from diffQ, 
# with an error attached, lets the user know that this Tm 
# is potentially meaningless. diffQ() will give several 
# warning messages.

set.seed(1)
y = rnorm(55,1.5,.8)
diffQ(cbind(MultiMelt[, 1],y), plot = TRUE)$Tm

# The distribution of the curve data indicates noise.
# The data should be visually inspected with a plot 
# (see examples of diffQ). The Tm calculation (fit, 
# adj. R squared ~ 0.157, NRMSE ~ 0.279) is not optimal 
# presumably due to noisy data. Check raw melting 
# curve (see examples of diffQ).
# Calculated Tm 
#      56.16755


# Sixth example
# Curves may potentially have a low temperature resolution. The rsm 
# parameter can be used to double the temperature resolution.
# Use data from MultiMelt column 15 (MLC2v2).
data(MultiMelt)
tmp <- cbind(MultiMelt[, 1], MultiMelt[, 15])

# Use diffQ without and with the rsm parameter and plot
# the results in a single row
par(mfrow = c(1,2))

diffQ(tmp, plot = TRUE)$Tm
  text(60, -0.15, "without rsm parameter")

diffQ(tmp, plot = TRUE, rsm = TRUE)$Tm
  text(60, -0.15, "with rsm parameter")
par(default.par)

}
\references{
A Highly Versatile Microscope Imaging Technology Platform for
the Multiplex Real-Time Detection of Biomolecules and Autoimmune Antibodies.
S. Roediger, P. Schierack, A. Boehm, J. Nitschke, I. Berger, U. Froemmel, C.
Schmidt, M. Ruhland, I. Schimke, D. Roggenbuck, W. Lehmann and C.
Schroeder.  \emph{Advances in Biochemical Bioengineering/Biotechnology}.
133:33--74, 2013. \url{https://pubmed.ncbi.nlm.nih.gov/22437246/}

Nucleic acid detection based on the use of microbeads: a review. S.
Roediger, C. Liebsch, C. Schmidt, W. Lehmann, U. Resch-Genger, U. Schedler,
P. Schierack. \emph{Microchim Acta} 2014:1--18. DOI:
10.1007/s00604-014-1243-4

Roediger S, Boehm A, Schimke I. Surface Melting Curve Analysis with R.
\emph{The R Journal} 2013;5:37--53.

Roediger S et al. R as an Environment for the Reproducible 
Analysis of DNA Amplification Experiments. \emph{The R Journal} 
2015;7:127--150.
}
\seealso{
\code{\link{diffQ2}}, \code{\link{mcaSmoother}}
}
\author{
Stefan Roediger
}
\keyword{Tm}
\keyword{melting}
