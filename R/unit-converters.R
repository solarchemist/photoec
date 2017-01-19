#' nm2eV
#'
#' Convert from nanometer to electron volt
#'
#' @param nm vector or scalar, in nm
#'
#' @return electron volts
#' @export
nm2eV <- function(nm) {
   # Converts wavelength in nm to energy in eV
   eV <-
      subset(solarconstants, label == "h.eV")$value *
      1E9 * subset(solarconstants, label == "c")$value / nm
   return(eV)
}


#' eV2nm
#'
#' Convert from electron volt to nanometer
#'
#' @param eV vector or scalar, in eV
#'
#' @return nanometers
#' @export
eV2nm <- function(eV) {
   # Converts energy in eV to wavelength in nm
   nm <-
      subset(solarconstants, label == "h.eV")$value *
      1E9 * subset(solarconstants, label == "c")$value / eV
   return(nm)
}
