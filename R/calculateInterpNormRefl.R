#' Calculate interpolated normalized reflectance
#' @description Interpolate the radiance and irradiance data for a single event file. Then calculate normalized reflectance for the event file. Arguments include a single event dataset, the average panel reflectance, and the interpolation type (linear, spline, or cubic). Return a data frame with four columns: wavelength, irradiance, radiance, and normrefl (normal reflectance).
#' @param eventdata A data frame of one observation made by a hyperspectrometer. This should have three columns: wavelength, irradiance, and radiance.
#' @param calreflavgs The reflectance averages from a calibration panel (usually averaged from multiple scans of a panel).
#' @param interpolation The interpolation type. Values include "linear", "natural" (spline), and "fmm" (cubic; the Forsythe, Malcolm and Moler method as described in the R help files for the spline() function. "...an exact cubic is fitted through the four points at each end of the data, and this is used to determine the end conditions")
#' @param limitNR The limit of the normalized reflectance that should be used. "nl" (no limit) = allow all values; "lim1" = if a value is greater than 1 or less than -1, change the value to 1 or -1, respectively; limNA" = if a value is greater than 1 or less than -1, change the value to NA.
#' @return A data frame with four columns: wavelength, irradiance, radiance, and normrefl (normal reflectance).
#' @export

calculateInterpNormRefl <- function(eventdata, calreflavgs, interpolation, limitNR){
  data <- eventdata
  calrefl <- calreflavgs
  interpolation <- interpolation
  allwaves <- seq(303, 1147, by = 1)
  
  #Step 1: interpolation
  #Linear
  if(interpolation == "linear"){
    InterCalIrr <- approx(x = data$wavelength, y = data$irradiance,
                          xout = allwaves,  method = "linear")
    InterCalRad <- approx(x = data$wavelength, y = data$radiance,
                          xout = allwaves,  method = "linear")
    InterWhite <- approx(x = data$wavelength, y = calrefl$avg,
                         xout = allwaves,  method = "linear")
  }
  
  #Spline 
  if (interpolation == "spline"){
    InterCalIrr <- spline(x = data$wavelength, y = data$irradiance,
                          xout = allwaves, method = "natural")
    InterCalRad <- spline(x = data$wavelength, y = data$radiance,
                          xout = allwaves, method = "natural")
    InterWhite <- spline(x = data$wavelength, y = calrefl$avg,
                         xout = allwaves, method = "natural")  
  }
  
  #Cubic 
  if (interpolation == "cubic"){
    InterCalIrr <- spline(x = data$wavelength, y = data$irradiance,
                          xout = allwaves, method = "fmm")
    InterCalRad <- spline(x = data$wavelength, y = data$radiance,
                          xout = allwaves, method = "fmm")
    InterWhite <- spline(x = data$wavelength, y = calrefl$avg,
                         xout = allwaves, method = "fmm")  
  }
  
  interpdata <- data.frame(wavelength = InterCalIrr$x, irradiance = InterCalIrr$y,
                           radiance = InterCalRad$y, calrefl = InterWhite$y)
  
  #Step 2: Calculate reflectance data by dividing the radiance by the irradiance
  #at each wavelength
  interpdata$refl <- (interpdata$radiance/interpdata$irradiance)
  
  #Step 3: Calculate the final reflectance by comparing the irradiance signal to
  #the mean white panel signal (that is, normalize the data)
  interpdata$normrefl <- (interpdata$refl/interpdata$calrefl)
  
  #Step 4: Remove values outside of the possible range of -1 to 1
  if(limitNR == "nl"){}
  if(limitNR == "lim1"){
    interpdata$normrefl[which(interpdata$normrefl > 1)] <- 1
    interpdata$normrefl[which(interpdata$normrefl < -1)] <- -1
  }
  if(limitNR == "limNA"){
    interpdata$normrefl[which(interpdata$normrefl > 1)] <- NA
    interpdata$normrefl[which(interpdata$normrefl < -1)] <- NA
  }
  return(interpdata)
}
