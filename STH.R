source("/home/taha/chepec/chetex/common/R/sunlight/sunlightASTM.R")
source("/home/taha/chepec/chetex/common/R/common/nm2eV.R")

STH <- function(wavelength,
                currentdensity,
                total.irradiance = 
                   max(sunlight.ASTM(model = "AM1.5G")$irradiance),
                electrode.potential = 1.23,
                faradaic.efficiency = 1) {
   #' @title Calculate maximium solar-to-hydrogen efficiency
   #'
   #' @description
   #' Calculate the maximum STH efficiency
   #'
   #' @details
   #' Calculate the maximum STH efficiency
   #'
   #' @param wavelength: vector                 / nm
   #' @param currentdensity: vector             / A m-2
   #' @param total.irradiance: scalar           / W m-2
   #' @param electrode.potential: scalar        / eV
   #' @param faradaic.efficiency: scalar        / 1
   #' 
   #' @examples
   #' STH()
   #' 
   #' @author Taha Ahmed <taha@@chepec.se>
   #' 
   #' @return wavelength / nm
   #' @return currentdensity / A m-2
   
   STH <- (currentdensity * electrode.potential * faradaic.efficiency) / total.irradiance
   
   df <- data.frame(energy = nm2eV(wavelength),
                    wavelength = wavelength,
                    STH = STH)
   
   return(df)
}
