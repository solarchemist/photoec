# Note: functions to convert to/from wavenumber/wavelength exist in your R-common package.

#' wavelength2energy
#'
#' Convert from nanometer to electron volt
#'
#' @param nm vector or scalar, in nm
#'
#' @return electron volts
#' @examples
#' \dontrun{
#' wavelength2energy(432)
#' wavelength2energy(733)
#' }
#' @export
wavelength2energy <- function(nm) {
   # Converts wavelength in nm to energy in eV
   eV <-
      subset(photoec::solarconstants, label == "h.eV")$value *
      1E9 *
      subset(photoec::solarconstants, label == "c")$value /
      nm
   return(eV)
}


#' energy2wavelength
#'
#' Convert from electron volt to nanometer
#'
#' @param eV vector or scalar, in eV
#'
#' @return nanometers
#' @examples
#' \dontrun{
#' energy2wavelength(3.25)
#' energy2wavelength(1.92)
#' }
#' @export
energy2wavelength <- function(eV) {
   # Converts energy in eV to wavelength in nm
   nm <-
      subset(photoec::solarconstants, label == "h.eV")$value *
      1E9 *
      subset(photoec::solarconstants, label == "c")$value /
      eV
   return(nm)
}


#' Convert wavenumber to energy
#'
#' Converts wavenumber (cm-1) to energy (eV)
#'
#' @param wavenumber  numeric, scalar or vector
#'
#' @return numeric, scalar or vector
#' @examples
#' \dontrun{
#' wavenum2energy(1660)
#' wavenum2energy(3500)
#' }
#' @export
wavenum2energy <- function(wavenumber) {
   energy <-
      subset(photoec::solarconstants, label == "h.eV")$value *
      subset(photoec::solarconstants, label == "c")$value *
      1E2 *
      wavenumber
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
      1E-2 * energy / (
         subset(photoec::solarconstants, label == "h.eV")$value *
         subset(photoec::solarconstants, label == "c")$value
      )
   return(wavenum)
}
