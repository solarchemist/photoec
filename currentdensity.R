source("/home/taha/chepec/chetex/common/R/sunlight/solarconstants.R")

currentdensity <- function(wavelength, 
                           photonflux.csum, 
                           quantum.efficiency = 1) {
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
   #' @examples
   #' currentdensity(wavelength, photonflux.csum)
   #' @author Taha Ahmed <taha@@chepec.se>
   #' @return wavelength / nm
   #' @return currentdensity / A m-2
   
   # Note: if you use this function together with STH(), set the quantum.efficiency only once
   # [either here or in STH(..., quantum.efficiency = )].
   
   sun.constants <- solar.constants()
   
   currentdensity <-
      data.frame(wavelength = wavelength,
                 # current density = photonflux * elementary charge
                 currentdensity = quantum.efficiency * photonflux.csum * 
                    sun.constants["e", "value"])
      
   return(currentdensity)
}
