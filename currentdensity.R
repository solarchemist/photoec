source("/home/taha/chepec/chetex/common/R/sunlight/solarconstants.R")

currentdensity <- function(wavelength, 
                           photonflux.csum, 
                           quantum.efficiency = 1,
                           bandgap = 1.23) {
   #' @title Calculate the short-circuit current density
   #'
   #' @description
   #' Calculate the short-circuit current density from wavelength and photon flux
   #'
   #' @details
   #' Calculates short-circuit current density (Jsc).
   #'
   #' @param wavelength: vector         / nm
   #' @param photonflux.csum: vector    / s-1 m-2
   #' @param quantum.efficiency: 0 <= QE <= 1 (constant)
   #' @param bandgap: scalar            / eV # consider renaming "electrode.potential" [cf. STH()]
   #' @examples
   #' currentdensity(wavelength, photonflux.csum)
   #' @author Taha Ahmed <taha@@chepec.se>
   #' @return wavelength / nm
   #' @return currentdensity / A m-2
   
   # Note: if you use this function together with STH(), set the quantum.efficiency only once
   # [either here or in STH(..., quantum.efficiency = )].
   
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
   
   sun.constants <- solar.constants()
   
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
