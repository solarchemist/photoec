#' @name solar.constants
#' @title Solar constants
#' @description A dataset containing some constants related to sunlight.
#' @docType data
#' @format A data frame with 11 rows and 6 variables:
#' \describe{
#'   \item{name}{Common name of constant}
#'   \item{label}{Short label, useful for indexing}
#'   \item{value}{The numeric value of the constant}
#'   \item{label.tex}{The label, LaTeX formatted}
#'   \item{unit.tex}{The unit, LaTeX formatted (using siunitx package)}
#'   \item{reference}{Reference, if any}
#' }
#' @source \url{http://physics.nist.gov/cuu/Constants/}
#' @author Taha Ahmed
NULL


#' @name ASTMG173
#' @title ASTM G173-03 reference spectra
#' @description ASTM G173-03 reference spectra, derived from SMARTS v2.9.2
#' @docType data
#' @format A data frame with 2002 rows and 4 variables:
#' \describe{
#'   \item{wavelength}{Wavelength, in nm}
#'   \item{extraterrestrial}{AM0 reference spectrum, extraterrestrial radiation, in W * m-2 * nm-1}
#'   \item{globaltilt}{AM1.5 global tilt reference spectrum, in W * m-2 * nm-1}
#'   \item{direct.circumsolar}{Direct normal circumsolar reference spectrum, in W * m-2 * nm-1}
#' }
#' @source \url{http://rredc.nrel.gov/solar/spectra/am1.5/astmg173/astmg173.html}
NULL
