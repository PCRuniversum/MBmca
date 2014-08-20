"mcaSmoother" <- function(x, y, bgadj = FALSE, bg = NULL, Trange = NULL, 
				minmax = FALSE, df.fact = 0.95, n = NULL) {
  options(warn = -1)
  # Test if df.fact is within a meaningful range.
  if (df.fact < 0.6 || df.fact > 1.1) 
      stop("df.fact size must be within 0.6 and 1.1.")
  # Test if x and y exist and have identical lengths.
  if (is.null(x)) 
      stop("Enter temperature")
  if (is.null(y)) 
      stop("Enter fluorescence data")
  if (length(x) != length(y)) 
      stop("Use temperature and fluorescence data with same number of elements")
	
  # Test if bg has only two values
  if (!is.null(bg) && length(bg) != 2)
      stop("Use only two temperatures (e.g., bg = c(45,55)) to set the range for the background correction")
  bg <- sort(bg)
  # Test if background adjustment was set and background vector is not empty.
  if ((is.null(bg)) && (bgadj == TRUE))
      stop("Enter temperature background range (e.g., bg = c(45,55)).")
  # Test if Trange has only two values
  if (!is.null(Trange) && length(Trange) != 2)
      stop("Use only two temperatures (e.g., Trange = c(40,70)) to set the range for the melting curve analysis")
  # Test if bg range is in the range of Trange
  Trange <- sort(Trange)
  if (!is.null(Trange) && !is.null(bg) && (bgadj == TRUE)) {
      if ((bg[1] < Trange[1]) || (bg[1] > Trange[2])) {stop("Trange and bg overlapp wrongly")}
      if ((bg[2] < Trange[1]) || (bg[2] > Trange[2])) {stop("Trange and bg overlapp wrongly")}
  }
  # Test if temperature background range is unchanged. If not change to new values.
  if (!is.null(bg) && (bgadj == TRUE)) {
      tmp.data <- data.frame(x,y)
      bg <- c(head(which(tmp.data[, 1] >= bg[1]))[1]:tail(which(tmp.data[, 1] <= bg[2]))[1])
  }
  # Test if temperature range for melting curve analysis is unchanged.
  if (!is.null(Trange)) {
      tmp.data <- data.frame(x,y)
      range <- c(head(which(tmp.data[, 1] >= Trange[1]))[1]:tail(which(tmp.data[, 1] <= Trange[2]))[5])
      x <- tmp.data[range, 1]
      y <- tmp.data[range, 2]
  }
  # Test if y contains missing values. In case of missing values a regression is 
  # used to estimate the missing value.
  if (length(which(is.na(y) == TRUE)) > 0) { 
      y[which(is.na(y))] <- approx(x, y, n = length(x))$y[c(which(is.na(y)))]
  }

  # Smooth the curve with a cubic spline. Takes first the degree of freedom from the cubic spline.
  # The degree of freedom is than used to smooth the curve by a user defined factor.
  df.tmp <- data.frame(smooth.spline(x,y)$df)
  y.sp <- smooth.spline(x, y, df = (df.tmp * df.fact))$y
  
  if (!is.null(n)) {
      if (n < 0.1 || n > 10) 
	  stop("n must be a number between 0.1 and 10")
	  tmp.xy <- spline(x, y.sp, n = n * length(x))
	  x <- tmp.xy$x
	  y.sp <- tmp.xy$y
  }

  # If the argument bgadj is set TRUE, bg must be  used to define a temperature range for a linear 
  # background correction. The linear trend is estimated by a robust linear regression using lmrob().
  # In case criteria for a robust linear regression are violated lm() is automatically used.
  if (bgadj) {
      if (class(try(lmrob(y.sp[bg] ~ x[bg]), silent = T)) == "try-error") { 
	  coefficients <- data.frame(lm(y.sp[bg] ~ x[bg])[1]) 
	  } else {
	      lmrob.control <- suppressWarnings(lmrob(y.sp[bg] ~ x[bg])) 
	      if ((class(lmrob.control) != "try-error") && (lmrob.control$converged == TRUE)) { 
			      coefficients <- data.frame(lmrob(y.sp[bg] ~ x[bg])[1]) 
	      } else { 
		  coefficients <- data.frame(lm(y.sp[bg] ~ x[bg])[1]) 
		} 
	    } 
	    y.norm <- y.sp - (coefficients[2, 1] * x + coefficients[1, 1]) # Subtracts the linear trend from the smoothed values.
  } else {
      y.norm <- data.frame(y.sp)
    }
  # Performs a "Min-Max Normalization" between 0 and 1.	  
  if (minmax) {
      y.norm <- (y.norm - min(y.norm)) / (max(y.norm) - min(y.norm))
  }
  
  # Returns an object of the type data.frame containing the temperature in the first column 
  # and the pre-processed fluorescence data in the second column.
  return(xy = data.frame(x,y.norm))
}