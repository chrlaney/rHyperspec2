#' Calculate a specified index
#' @description Given a data frame of indices and a row number, calculate the specified index.
#' @param normReflDF A data frame of normal reflectances with three columns: location, wavelength, and normrefl (normal reflectance).
#' @param indexDF A data frame of indices that provides information on how to calculate each index.
#' @param indexno Index number
#' @return A data frame of locations and calculated index values
#' @export

calculateIndex <- function(normReflDF, indexDF, indexno){ 
  p1 <- normReflDF$normrefl[which(normReflDF$wavelength == indexDF$w1[indexno])]
  p2 <- normReflDF$normrefl[which(normReflDF$wavelength == indexDF$w2[indexno])]
  p3 <- normReflDF$normrefl[which(normReflDF$wavelength == indexDF$w3[indexno])]
  p4 <- normReflDF$normrefl[which(normReflDF$wavelength == indexDF$w4[indexno])]
  index <- eval(parse(text = indexDF$expression_form[indexno]))
  location <- unique(data$location)
  indexdata <- data.frame(location, index)
  return(indexdata)
}