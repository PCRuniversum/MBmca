"diffQ" <- function(xy, fct = min, fws = 8, col = 2, plot = FALSE, 
		    verbose =  FALSE, warn = TRUE, 
		    peak = FALSE, negderiv = TRUE, deriv = FALSE, 
		    derivlimits = FALSE, derivlimitsline = FALSE, 
		    vertiline = FALSE, rsm = FALSE, inder = FALSE) { 
  # Test if fws (number of neighbors) is within a meaningful range.
  options(warn = -1)
    fws <- round(fws)
  if (fws < 2 || fws > 8) 
      stop("Fit window size must be within 2 and 8.")
    
  list.res <- list()

  # Take the x and y values from the object of type data.frame.
  x <- xy[, 1]
  y <- xy[, 2]

  options(warn = -1)
  # Test if x and y exist.
  if (is.null(x)) 
      stop("Enter temperature")
  if (is.null(y)) 
      stop("Enter fluorescence data")

  # Determine the temperature resolution of the melting curve data
  deltaT <- vector()
  for (i in 1L:(length(x) - 1)){
    tmp <- abs(x[i] - x[i + 1])
    deltaT <- c(deltaT,tmp)
  }
  
  deltaT.mean 	<- mean(deltaT)
  deltaT.sd 	<- sd(deltaT)
  deltaT.RSD	<- deltaT.sd/deltaT.mean * 100
  temperature <- list(T.delta = deltaT, mean.T.delta = deltaT.mean, 
		      sd.T.delta = deltaT.sd, 
		      RSD.T.delta = deltaT.RSD)

  # Function to increase the resolution of the melting curve by 
  # calculation of the mean temperature and mean fluorescence
  if (rsm == TRUE) {
	ind.low <- seq(1, length(x) - 1, 1)
	ind.up <- seq(2, length(x), 1)
	xt <- (x[ind.up] + x[ind.low]) / 2
	yt <- (y[ind.up] + y[ind.low]) / 2
	xy <- data.frame(c(x, xt), c(y, yt))
	xy <- xy[order (xy[, 1]), ]
	x <- xy[, 1]
	y <- xy[, 2]
  }

  # Function to calculate the approximate derivative.
  if (negderiv) {y <- y * -1} 	# Change sign of curve data
  
  N <- length(x); low <- 1:2; up <- (N - 1):N
  
  if (inder) {	inder.tmp <- inder(x, y, Nip = 1)
		xQ <- inder.tmp[, "x"]
		diffQ <- inder.tmp[, "d1y"]
	    
  } else {  xQ <- x[3L:length(x) - 1] 
	    h <- (x[-low] - x[-up]) 	# Step size
	    diffQ <- (y[-low] - y[-up]) / h
	    }
	    
  out <- data.frame(xQ, diffQ)
  names(out) <- c("Temperature", "d(F) / dT")
  
  # First: Find approximate range of neighbors for minimum or 
  # maximum of temperature peak.Second: Calculate quadratic 
  # polynomial for ranges of minimum or maximum temperature peaks.
  # Return the results in an object of the type list.
  for (i in 2L:9) {
    suppressMessages(RANGE <- which(diffQ == fct(diffQ[(i + 1):(N - i)]))) 
    if (class(try(na.omit(xQ[(RANGE - i):(RANGE + i)]), 
	silent = T)) == "try-error") {
      list.res[[i-1]] <- NA
    } else {
      limits.xQ <- na.omit(xQ[(RANGE - i):(RANGE + i)])
      limits.diffQ <- na.omit(diffQ[(RANGE - i):(RANGE + i)])
      fluo.x <- limits.diffQ[length(limits.diffQ) - i]
      lm2 <- lm(limits.diffQ ~ limits.xQ + I(limits.xQ^2))
      coeflm2 <- data.frame(lm2[1])[c(1:3), ]
      coeflm2.y <- coeflm2[1] + coeflm2[2] * limits.xQ + coeflm2[3] * limits.xQ^2
      # lm2sum <- summary(lm(coeflm2.y ~ limits.diffQ))
      lm2sum <- summary(lm2)
      list.res[[i-1]] <- list(i, limits.xQ, limits.diffQ, fluo.x, 
			      lm2, coeflm2, coeflm2.y, lm2sum
			      )
    }
  }

  # Determine the optimal fitted quadratic polynomial for ranges 
  # of minimum or maximum temperature peaksbases on the adjusted 
  # R squared.
  Rsq <- matrix(NA, nrow = fws, ncol = 2)
  colnames(Rsq) <- c("fw", "Rsqr")
      for (i in 1L:fws) {
	Rsq[i, 1] <- list.res[[i]][[1]]
	Rsq[i, 2] <- list.res[[i]][[8]]$adj.r.squared
      }
      list.res <- list.res[[which(Rsq[, 2] == max(na.omit(Rsq[, 2])))]]
      names(list.res) <- list("fw", "limits.xQ", "limits.diffQ", 
			      "fluo.x", "lm2", "coeflm2", 
			      "coeflm2.y", "lm2sum")

      limits.xQ <- list.res$limits.xQ
      limits.diffQ <- list.res$limits.diffQ

      fluo.x <- list.res$fluo.x
      names(fluo.x) <- c("Signal hight at approximate Tm")

      lm2 <- list.res$lm2
      coeflm2 <- list.res$coeflm2
      coeflm2.y <- list.res$coeflm2.y
      lm2sum <- list.res$lm2sum
      # fw <- list.res$fw

      # Polynom to fit the area of the calculated Tm
      poly.fct <- function(xi) coeflm2[1] + coeflm2[2] * xi + coeflm2[3] * xi^2
      # Calculate the Tm and assign meaningful names to variables
      abl <- -lm2$coefficients[2] / (2 * lm2$coefficients[3])
      names(abl) <- "Calculated Tm"
      y <-	coeflm2[1] + coeflm2[2] * abl + coeflm2[3] * abl^2
      names(y) <- c("Signal hight at calculated Tm")

  # Optional draw line of calculated approximate derivative.
  if ((!plot) && (.Device != "null device")) {
    if (vertiline) {
	abline(v = abl, col = "grey")
    }
    if (deriv) {
	lines(xQ, diffQ, col = col)  
    } 
    if (derivlimits) {
	points(limits.xQ, limits.diffQ, cex = 1, pch = 19, col = col)
    }
    if (derivlimitsline) {
	lines(spline(limits.xQ[1L:length(limits.xQ)], 
	      lm2[["fitted.values"]][1L:length(lm2[["fitted.values"]])]), 
	      col = "orange", lwd = 2)
    }
    if (peak) {
	points(abl, y, cex = 1, pch = 19, col = col)
    }
  }

  # Test if the calculated Tm and the approximate Tm differ strongly
  dev.dat <- data.frame(limits.xQ, limits.diffQ)
  dev <- dev.dat[which(dev.dat[, 2] == max(dev.dat[, 2])), ]
  dev.var <- sd(c(dev[1, 1], abl)) / mean(c(dev[1, 1], abl)) * 100
  dev.sum <- data.frame(dev.var, dev[1, 1], abl)
  colnames(dev.sum) <- c("Relative Deviation (%)", "Approximate Tm", 
			 "Calculated Tm")
  rownames(dev.sum) <- NULL
  
  if (warn && dev.sum[1] > 5) {
      message("Approximate and calculated Tm varri. This is an expected behaviour \nbut the calculation should be confirmed with a plot (see examples of diffQ).")
      print(dev.sum)
  }

  # Calculates the Root Mean Squared Error
  NRMSE <- function(model = model, mes = mes) {
	    RMSE <- sqrt(mean(residuals(model)^2))
	    NRMSE <- RMSE / (max(mes) - min(mes))
	    if(NRMSE > 0.08) {
		NRMSE.warning <- "NRMSE bad"
	    } else (NRMSE.warning <- "NRMSE ok")
		return(list(NRMSE = NRMSE, RMSE = RMSE, 
			    NRMSE.warning = NRMSE.warning))
	  }
  
  NRMSE.res <- NRMSE(model = lm2, mes = limits.diffQ)

  # Simple test if data come from noise or presumably a melting curve
  if (warn && shapiro.test(xy[, 2])$p.value >= 0.0000001) {
      message("The distribution of the curve data indicates noise.\nThe data should be visually inspected with a plot (see examples of diffQ).")
  }
  # Simple test if polynomial fit performed accaptable
  if (warn && (max(na.omit(Rsq[, 2])) < 0.85))
      message(paste0("The Tm calculation (fit, adj. R squared ~ ", 
		    round(max(na.omit(Rsq[, 2])), 3), 
		    ", NRMSE ~ ", round(NRMSE.res$NRMSE, 3), 
		    ") is not optimal presumably due to noisy data.\nCheck raw melting curve (see examples of diffQ).")
      )

  if (plot) {
      plot(xQ, diffQ, xlab = "Temperature", ylab = "-d(F) / dT", 
	   type = "b", col = col)
      points(limits.xQ, limits.diffQ, col = "orange", pch = 19)
	curve(poly.fct, limits.xQ[1], limits.xQ[length(limits.xQ)], 
	      col = col, add = TRUE)
	points(abl, y, pch = 19, col = 2)
	if (vertiline) {
	    abline(v = abl, col = "grey")
	}
	if (derivlimits) {
	    points(limits.xQ, limits.diffQ, cex = 1, pch = 19, col = col)
	}
	if (derivlimitsline) {
	    lines(spline(limits.xQ[1L:length(limits.xQ)], 
		  lm2[["fitted.values"]][1L:length(lm2[["fitted.values"]])]), 
		  col = "orange", lwd = 2)
	}
  }

  # Returns an object of the type list containing the data and 
  # data.frames from above including the approximate difference 
  # quotient values, melting temperatures, intensities and used 
  #neighbors.
  if (verbose) {
      return(list(Tm = abl, fluoTm = y, 
		  Tm.approx = dev[1], fluo.x = fluo.x, 
		  xy = out, limits.xQ = limits.xQ, 
		  limits.diffQ = limits.diffQ,
		  adj.r.squared = lm2sum$adj.r.squared, 
		  NRMSE = NRMSE.res$NRMSE, 
		  fws = list.res$fw, devsum=dev.sum, 
		  temperature = temperature, 
		  fit = summary(list.res$lm2)))
  } else (return(list(Tm = abl, fluoTm = y)))
}