source("/home/taha/chepec/chetex/common/R/common/trapz.R")
source("/home/taha/chepec/chetex/common/R/sunlight/solarconstants.R")

photonflux <- function(wavelength, spectralirradiance) {
   #' @title Calculate the photon flux
   #'
   #' @description
   #' Calculate the photon flux from wavelength and spectral radiance
   #'
   #' @details
   #' Provide wavelength and spectral radiance.
   #' It is important that provided wavelength and spectral radiance
   #' use the same length unit (for example, nm and W m-2 nm-1,
   #' or m and W m-2 m-1). 
   #'
   #' @param wavelength: vector       / nm
   #' @param spectralirradiance: vector / W m-2 nm-1
   #' @examples
   #' photonflux(wavelength, spectralirradiance)
   #' @author Taha Ahmed <taha@@chepec.se>
   #' @return wavelength               / nm
   #' @return photonflux               / s-1 m-2 nm-1
   #' @return photonflux.trapz         / s-1 m-2
   #' @return photonflux.csum          / s-1 m-2
      
   solar.constants <- solar.constants()
   
   # flux is the rate of flow of a property per unit area
   # here we have flux / s-1 m-2 nm-1
   photonflux <- 
      spectralirradiance * 
      (wavelength / 
          (1E9 * solar.constants["c", "value"] * 
              solar.constants["h", "value"]))
   # integrate numerically under curve with trapz()
   photonflux.trapz <-
      c(0, 
        trapz(x = wavelength, 
              y = photonflux))
   # calculate cumulative sum (i.e., flux / s-1 m-2)
   photonflux.csum <- cumsum(photonflux.trapz)
   # photon flux fraction (from 0 - 1)
   photonflux.fraction <- 
      photonflux.csum / tail(photonflux.csum, 1)
   
   flux <-
      data.frame(wavelength = wavelength,
                 # flux is the rate of flow of a property per unit area
                 # here we have flux / s-1 m-2 nm-1
                 photonflux =  photonflux,
                 # integrate numerically under curve with trapz()
                 photonflux.trapz = photonflux.trapz,
                 # calculate cumulative sum (i.e., flux / s-1 m-2)
                 photonflux.csum = photonflux.csum,
                 # photon flux fraction (from 0 - 1)
                 photonflux.fraction = photonflux.fraction)
   
   return(flux)
}
