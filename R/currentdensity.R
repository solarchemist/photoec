#' Calculate short-circuit current density
#'
#' Calculate short-circuit (maximum) current density (Jsc) from the photon flux.
#'
#' @description Note that for the sake of following convention this function
#'    uses energy (in eV) rather than wavelength as its x-axis.
#'
#' @param energy, energy/eV, numeric vector
#' @param photonflux, cumulative photon flux/s⁻¹ m⁻², vector
#' @param EQE, external quantum efficiency, numeric (0 - 1)
#' @param bandgap, band gap of photoabsorber, numeric/eV (equivalent to electrode potential)
#'
#' @return dataframe with 3 columns:
#'    \describe{
#'       \item{energy}{energy/eV, same as input vector}
#'       \item{currentdensity}{current density/mA cm⁻² (or equivalently, A m⁻²)}
#'       \item{currentdensity.fraction}{relative current density}
#'    }
#'
#' @export
#' @examples
#' \dontrun{
#'    currentdensity(
#'       energy = photoec::sunlight.ASTM(model="AM1.5G")$energy,
#'       photonflux = photoec::sunlight.ASTM(model="AM1.5G")[["AM1.5G.photonflux"]]
#'    )
#' }
currentdensity <- function(
   energy,
   photonflux,
   EQE = 1,
   bandgap = 1.23) {

   #### arg checks
   stopifnot(
      length(energy) == length(photonflux),
      EQE >= 0, EQE <= 1,
      bandgap > 0)

   echarge <- dplyr::filter(photoec::solarconstants, label=="e")$value

   result <- data.frame(
      energy = energy,
      currentdensity = EQE * photonflux * echarge)

   # set all current densities below the band gap to zero
   result$currentdensity[which(result$energy < bandgap)] <- 0

   # calculate a current density fraction (useful in some circumstances)
   result$currentdensity.fraction <- result$currentdensity / max(result$currentdensity)

   return(result)
}
