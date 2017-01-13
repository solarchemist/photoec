#' Calculate short-circuit current density
#'
#' Calculate the short-circuit current density (Jsc) from wavelength and photon flux.
#'
#' @details If you use this function together with STH(), set the quantum.efficiency only once, either here or in \code{\link{STH}}.
#'
#' @param wavelength, in nanometer
#' @param photonflux.csum, s-1 m-2
#' @param quantum.efficiency, between 0 and 1
#' @param bandgap, in eV (equivalent to electrode potential)
#'
#' @return dataframe: energy (eV), wavelength (nm), and current density (A m-2)
#' @export
currentdensity <- function(wavelength,
                           photonflux.csum,
                           quantum.efficiency = 1,
                           bandgap = 1.23) {

   # check that the bandgap arg is ok
   # assuming bandgap is numeric...
   if (bandgap < 10) {
      # which puts us past the far ultraviolet region
      # Assume the user intended to use eV
      eV <- TRUE
   } else {
      # assume larger numeric values mean nanometers
      eV <- FALSE
   }

   bg.nm <- ifelse(eV, eV2nm(bandgap), bandgap)

   sun.constants <- common::LoadRData2Variable("data/solarconstants.rda")

   # current density = photonflux * elementary charge
   currentdensity <-
      data.frame(energy = nm2eV(wavelength),
                 wavelength = wavelength,
                 currentdensity = quantum.efficiency * photonflux.csum *
                    sun.constants["e", "value"])

   # set all current densities above the band gap to zero
   currentdensity$currentdensity[which(currentdensity$wavelength > bg.nm)] <- 0

   # calculate a current density fraction (useful in some circumstances)
   currentdensity$currentdensity.fraction <-
      currentdensity$currentdensity / max(currentdensity$currentdensity)

   return(currentdensity)
}
