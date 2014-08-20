"diffQ2" <- function(xy, fct = max, fws = 8, col = 2, plot = FALSE, 
		     verbose = FALSE, peak = FALSE, 
		     deriv = FALSE, negderiv = TRUE, derivlimits = FALSE, 
		     derivlimitsline = FALSE, vertiline = FALSE, 
		     rsm = FALSE, inder = FALSE, warn = TRUE) {
  # Test if fws (number of neighbors) is within a meaningful range.
  options(warn = -1)
    fws <- round(fws)
    if (fws < 2 || fws > 8) 
    stop("Fit window size must be within 2 and 8.")
  
  # Calls instances of diffQ to calculate the Tm of the first and the 
  # second derivatives.The arguments are similar to diffQ.
  # Calculates the first derivative and provides the starting information 
  # for the second derivative.
  TmD1 <- diffQ(xy, fct = fct, fws = fws, negderiv = negderiv, 
		verbose = TRUE, rsm = rsm, inder = inder, 
		warn = warn)

  if (TmD1[["temperature"]][["mean.T.delta"]] >= 0.5) {
      tmp.xy <- predict(smooth.spline(TmD1$xy[, 1], TmD1$xy[, 2]), 
		        seq(min(TmD1$xy[, 1]), max(TmD1$xy[, 1]), 
		        TmD1[["temperature"]][["mean.T.delta"]] / 1.3))
      xy.smoothed <- data.frame(tmp.xy$x, tmp.xy$y)
  } else (xy.smoothed <-  data.frame(TmD1$xy[, 1], 
				     smooth.spline(TmD1$xy[, 1], 
				     TmD1$xy[, 2])$y)
				     )

  # Calculates the second derivative and from the first derivative for the 
  # minimum of the melting curve.
  Tm1D2 <- diffQ(xy.smoothed, fct = min, fws = fws, verbose =  TRUE, 
		 col = col, peak = peak, deriv = deriv, negderiv = FALSE, 
		 derivlimits = derivlimits, derivlimitsline = derivlimitsline, 
		 vertiline = vertiline, inder = inder, 
		 warn = warn)
  # Calculates the second derivative and from the first derivative for the 
  # maximum of the melting curve.
  Tm2D2 <- diffQ(xy.smoothed, fct = max, fws = fws, verbose =  TRUE, 
		 col = col, peak = peak, deriv = deriv, negderiv = FALSE, 
		 derivlimits = derivlimits, derivlimitsline = derivlimitsline, 
		 vertiline = vertiline, inder = inder, 
		 warn = warn)
  
  # Vectors of the two melting temperatures of the second derivative.
  x <- c(Tm1D2[["Tm"]], Tm2D2[["Tm"]])
  # Vectors of the two intensities at the melting temperatures of the 
  # second derivative.
  # y <- c(coeflm2.y.1, coeflm2.y.2)
  y <- c(Tm1D2[["fluoTm"]], Tm2D2[["fluoTm"]])

  # Polynom to fit the area of the calculated Tm
  poly.fct.TmD1 <- function(xi) TmD1[["fit"]][[4]][1] + TmD1[["fit"]][[4]][2] * xi + TmD1[["fit"]][[4]][3] * xi^2
  poly.fct.Tm1D2 <- function(xi) Tm1D2[["fit"]][[4]][1] + Tm1D2[["fit"]][[4]][2] * xi + Tm1D2[["fit"]][[4]][3] * xi^2
  poly.fct.Tm2D2 <- function(xi) Tm2D2[["fit"]][[4]][1] + Tm2D2[["fit"]][[4]][2] * xi + Tm2D2[["fit"]][[4]][3] * xi^2


  if (plot) {
  # Plot the first derivative
      par(fig = c(0,1,0.475,1))
      plot(TmD1$xy, xlab = "Temperature", ylab = "-d(F) / dT", type = "b")
	abline(v = (TmD1$Tm), col = "grey", lwd = 1.25)
	abline(v = (Tm1D2$Tm), col = "grey")
	abline(v = (Tm2D2$Tm), col = "grey")
	points(TmD1$limits.xQ, TmD1$limits.diffQ, col = "orange", pch = 19)
	points(TmD1$Tm, TmD1$fluoTm, pch = 19, col = 2)
	curve(poly.fct.TmD1, TmD1$limits.xQ[1], TmD1$limits.xQ[length(TmD1$limits.xQ)], col = "red", add = TRUE)
	if (derivlimits) {
	    points(TmD1$limits.xQ, TmD1$limits.diffQ, cex = 1, pch = 19, col = col)
	}

  # Plot the second derivative
	par(fig = c(0,1,0,0.525), new = TRUE)
	plot(Tm1D2$xy, xlim = c(min(TmD1$xy[, 1]), max(TmD1$xy[, 1])), 
	     xlab = "Temperature", ylab = "-d^2(F) / dT^2", type = "b")
	  abline(v = (TmD1$Tm), col = "grey")
	  points(Tm1D2$limits.xQ, Tm1D2$limits.diffQ, col = "green", pch = 19)
	  points(Tm2D2$limits.xQ, Tm2D2$limits.diffQ, col = "blue", pch = 1)
	  points(Tm1D2$Tm, Tm1D2$fluo.x, pch = 19, col = 2)
	  points(Tm2D2$Tm, Tm2D2$fluo.x, pch = 19, col = 2)
	  curve(poly.fct.Tm1D2, Tm1D2$limits.xQ[1], Tm1D2$limits.xQ[length(Tm1D2$limits.xQ)], col = "red", add = TRUE)
	  curve(poly.fct.Tm2D2, Tm2D2$limits.xQ[1], Tm2D2$limits.xQ[length(Tm2D2$limits.xQ)], col = "red", add = TRUE)
    }

  # Returns an object of the type list containing the data and data.frames from above including the approximate 
  # difference quotient values, melting temperatures of the first derivative and the second derivative, intensities and used neighbors.
  if (verbose) {
      return(list(TmD1 = TmD1, Tm1D2 = Tm1D2, Tm2D2 = Tm2D2, xTm1.2.D2 = x, 
	    yTm1.2.D2 = y, temperature = TmD1$temperature))
      } else (return(list(Tm = TmD1$Tm, fluoTm = TmD1$fluoTm, 
			  xTm1.2.D2 = x, yTm1.2.D2 = y)))

}