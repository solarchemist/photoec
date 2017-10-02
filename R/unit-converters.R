# Note: functions to convert to/from wavenumber/wavelength exist in your R-common package.

#' nm2eV
#'
#' Convert from nanometer to electron volt
#' DEPRECATED. Please use wavelength2energy() instead.
#'
#' @param nm vector or scalar, in nm
#'
#' @return electron volts
#' @export
nm2eV <- function(nm) {
   .Deprecated("wavelength2energy")
   # Converts wavelength in nm to energy in eV
   eV <-
      subset(solarconstants, label == "h.eV")$value *
      1E9 * subset(solarconstants, label == "c")$value / nm
   return(eV)
}


#' wavelength2energy
#'
#' Convert from nanometer to electron volt
#'
#' @param nm vector or scalar, in nm
#'
#' @return electron volts
#' @export
wavelength2energy <- function(nm) {
   # Converts wavelength in nm to energy in eV
   eV <-
      subset(solarconstants, label == "h.eV")$value *
      1E9 * subset(solarconstants, label == "c")$value / nm
   return(eV)
}


#' eV2nm
#'
#' Convert from electron volt to nanometer
#' DEPRECATED. Please use energy2wavelength() instead.
#'
#' @param eV vector or scalar, in eV
#'
#' @return nanometers
#' @export
eV2nm <- function(eV) {
   .Deprecated("energy2wavelength")
   # Converts energy in eV to wavelength in nm
   nm <-
      subset(solarconstants, label == "h.eV")$value *
      1E9 * subset(solarconstants, label == "c")$value / eV
   return(nm)
}


#' energy2wavelength
#'
#' Convert from electron volt to nanometer
#'
#' @param eV vector or scalar, in eV
#'
#' @return nanometers
#' @export
energy2wavelength <- function(eV) {
   # Converts energy in eV to wavelength in nm
   nm <-
      subset(solarconstants, label == "h.eV")$value *
      1E9 * subset(solarconstants, label == "c")$value / eV
   return(nm)
}


#' Convert wavenumber to energy
#'
#' Converts wavenumber (cm-1) to energy (eV)
#'
#' @param wavenumber  numeric, scalar or vector
#'
#' @return numeric, scalar or vector
#' @export
wavenum2energy <- function(wavenumber) {
   energy <-
      subset(solarconstants, label == "h.eV")$value *
          subset(solarconstants, label == "c")$value * 1E2 * wavenumber
   return(energy)
}


#' Convert energy to wavenumber
#'
#' Converts energy (eV) to wavenumber (cm-1)
#'
#' @param energy  numeric, scalar or vector
#'
#' @return numeric, scalar or vector
#' @export
energy2wavenum <- function(energy) {
   wavenum <-
      1E-2 * energy /
      (subset(solarconstants, label == "h.eV")$value *
          subset(solarconstants, label == "c")$value)
   return(wavenum)
}
