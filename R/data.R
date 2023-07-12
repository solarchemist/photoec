#' @name solarconstants
#' @title Solar constants
#' @description A dataset containing some constants related to sunlight.
#' @docType data
#' @format A data frame with 12 rows and 8 variables:
#' \describe{
#'   \item{name}{Common name of constant}
#'   \item{label}{Short label, useful as a unique identifier}
#'   \item{value}{The numeric value of the constant}
#'   \item{reference}{Reference, if any}
#'   \item{label.html}{The label, HTML formatted (requires Unicode support)}
#'   \item{label.tex}{The label, LaTeX formatted}
#'   \item{unit.html}{The unit, HTML formatted (requires Unicode support)}
#'   \item{unit.tex}{The unit, LaTeX formatted (using siunitx package v2)}
#' }
#' @source \url{http://physics.nist.gov/cuu/Constants/}
#' @author Taha Ahmed
NULL


#' @name ASTMG173
#' @title ASTM G173-03 reference spectra
#' @description ASTM G173-03 reference spectra, derived from SMARTS v2.9.2.
#'     This dataset has at best a wavelength step of 0.5 nm:
#'     + 280 nm  -- 400 nm, step 0.5 nm
#'     + 400 nm  -- 1700 nm, step 1.0 nm (there is also a datapoint at 1702 nm)
#'     + 1705 nm -- 4000 nm, step 5 nm
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
