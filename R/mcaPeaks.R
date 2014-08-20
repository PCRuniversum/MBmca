"mcaPeaks" <- function(x, y, span = 3) {
  # Take the x and y values from the object of type data.frame.
  
  options(warn = -1)
  # Test if x and y exist.
  if (is.null(x)) 
      stop("Enter temperature")
  if (is.null(y)) 
      stop("Enter fluorescence data")
  # Test if x and y have the same length.
  if (length(x) != length(y)) 
      stop("Use temperature and fluorescence data with same number of elements")
  # Test if span is odd.
  if (span%/%2 != 1) {
      message("span must be odd. Automatically set to span = 3")
      span <- 3
  }

  # smooth the input data slightly with a spline
  input.y <- smooth.spline(x, y)$y

  # Estimate the local minima and local maxima
  # autor Brian Ripley, https://stat.ethz.ch/pipermail/r-help/2002-May/021934.html
  # modified for the MBmca package to find the approximate maxima and minima of
  # the melting peak data
  s <- span%/%2

  z.max <- embed(input.y, span)
  v.max <- max.col(z.max) == 1 + s
  maxima <- c(rep(FALSE, s), v.max)
  maxima <- maxima[1L:(length(maxima) - s)]

  z.min <- embed(input.y * -1, span)
  v.min <- max.col(z.min) == 1 + s
  minima <- c(rep(FALSE, s), v.min)
  minima <- minima[1L:(length(minima) - s)]

  out <- data.frame(x, input.y)

  p.min <- data.frame(out[which(minima == TRUE), 1], 
		      out[which(minima == TRUE), 2]
		      )
  colnames(p.min) <- c("T (minima)", "F (minima)")

  p.max <- data.frame(out[which(maxima == TRUE), 1], 
		      out[which(maxima == TRUE), 2]
		      )
  colnames(p.max) <- c("T (maxima)", "F (maxima)")
  
  return(list(p.min = p.min, p.max = p.max))
} 
