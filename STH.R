source("/home/taha/chepec/chetex/common/R/sunlight/sunlightASTM.R")
source("/home/taha/chepec/chetex/common/R/sunlight/photonflux.R")
source("/home/taha/chepec/chetex/common/R/sunlight/solarconstants.R")

# We are going to be using ASTM data many times, so to save time we
# call sunlight.ASTM() only once up here.
# astm <- sunlight.ASTM() 
# put in the function args instead

# Solar constants
solar.constants <- solar.constants()


STH <- function(bandgap, # in nanometre!
                #currentdensity, 
                #electrode.potential = 1.23, 
                #quantum.efficiency = 1, 
                # arguments that are passed on to sunlight.ASTM()
                astm.model = "AM1.5G",
                astm.wavelength = c(seq(280, 400, 0.5), seq(401, 1700, 1), 
                                    1702, seq(1705, 4000, 5))) {
   #' @title Calculate the solar-to-hydrogen efficiency
   #'
   #' @description
   #' Calculate the solar-to-hydrogen efficiency
   #'
   #' @details
   #' Calculates the solar-to-hydrogen (STH) efficiency
   #'
   #' @param wavelength: vector              / nm
   #' @param bandgap: scalar                 / nm
   #' @param currentdensity: vector          / A m-2
   #' @param electrode.potential: scalar     / volt
   #' @param quantum.efficiency: scalar, 0 <= QE <= 1
   #' @param solar.model: string, one of "AM1.5G", "AM0", or "DNCS"
   #' @param total.irradiance: scalar        / W m-2
   #'        defaults to total radiance of full wavelength range of AM1.5G
   #' @examples
   #' currentdensity(wavelength, photonflux.csum)
   #' @author Taha Ahmed <taha@@chepec.se>
   #' @return STH: vector, units percent
   
   
   #### Check the arguments...
   # please make sure the lower wl boundary starts at 280 nm
   # ...
   # bandgap must lie inside the supplied wavelength range
   if ((bandgap < min(astm.wavelength)) | 
          (bandgap > max(astm.wavelength))) {
      stop(paste0("Specified bandgap cannot be outside the ",
                  min(astm.wavelength),
                  " nm to ",
                  max(astm.wavelength),
                  " nm range."))
   }
   
   
   #### Call sunlight.ASTM()
   astm.bandgap <- 
      sunlight.ASTM(wavelength = 
                       astm.wavelength[which(astm.wavelength <= bandgap)], 
                    model = astm.model)
   astm.full <- 
      sunlight.ASTM(wavelength = astm.wavelength,
                    model = astm.model)
   
   
   
   
   
   # Let's follow the approach described by Varghese2008b
   # overall.solar.energy.conversion.efficiency = 
   #    solar.irradiance.efficiency * quantum.efficiency * chemical.efficiency
   
   # For the sake of brevity of code, we will rename these quantities as follows:
   # overall.conversion.eff <- overall.solar.energy.conversion.efficiency
   # solar.irrad.eff        <- solar.irradiance.efficiency
   # quant.eff              <- quantum.efficiency
   # chem.eff               <- chemical.efficiency
   
   #### Solar irradiance efficiency
   # is defined as the fraction of the solar irradiance with 
   # photon energy E <= Eg
   
   # photon flux for the full wavelength range
   photonflux.full <-
      photonflux(wavelength = astm.full$wavelength,
                 spectralradiance = astm.full$spectralradiance)
   # note: Pt is a scalar (the total photonflux for the current ASTM model)
   Pt <- tail(photonflux.full$photonflux.csum, 1)
   
   # photon flux up to the bandgap
   photonflux.bandgap <-
      photonflux(wavelength = astm.bandgap$wavelength,
                 spectralradiance = astm.bandgap$spectralradiance)
   # note: Fg is a vector
   Fg <- photonflux.bandgap$photonflux # / s-1 m-2 nm-1
   # fill Fg with zeroes for all wl above the bandgap
   Fg <- c(Fg, rep(0, dim(photonflux.full)[1] - length(Fg)))
   
   ### INFO
   # The highest Fg occurs at 669 nm
   # Fg(669 nm) = 4.847647e+18 s-1 m-2 nm-1
   # The calculated efficiency depends on the chosen bandgap, so to get
   # a constant reference frame irrespective of chosen bandgap, we 
   # need to choose one bandgap (arbitrary) to normalize against.
   # We'll choose the bandgap of the water-splitting reaction (1.23 eV)
   # wavelength bandgap           Fg           Pt solar.irradiation.efficiency
   #        669    1008 4.847647e+18 4.305572e+21                     1.134908
   
   # this is effectively flux efficiency (we divide flux by total flux), thus
   # unitless
   solar.irrad.eff <- bandgap * (Fg / Pt)
#    # normalize it, so it goes from 0 - 1
#    solar.irrad.eff <- (solar.irrad.eff / max(solar.irrad.eff)) * 1.134908

                 
   
   
   
   
   
#    l <- 1
#    # pre-assign Fg.vec
#    Fg.vec <- rep(0, length(which(astm$wavelength <= bandgap)))
   
#    Fg.vec <- 
#       astm[which(astm$wavelength <= bandgap), 
#            paste0(solar.model, ".spectralradiance")] / 
#       (sun.constants["h", "value"] * sun.constants["c", "value"] / 
#           (1E-9 * astm$wavelength[which(astm$wavelength <= bandgap)]))

#    Fg.vec <- 
#       trapz(y = sun.constants["h", "value"] * sun.constants["c", "value"] / 
#                (1E-9 * astm$wavelength[which(astm$wavelength <= bandgap)]), 
#             x = astm[which(astm$wavelength <= bandgap), 
#                      paste0(astm.model, ".radiance")])

   
#    while (astm$wavelength[l] <= bandgap) {
#       # # # # # # # # # # # # # # # # # # # # # # # 
#       # You should probably take into account that
#       # wavelength has units of meters, not nm!!
#       # # # # # # # # # # # # # # # # # # # # # # # 
#       Fg.vec[l] <- 
#          astm[l, paste0(solar.model, ".spectralradiance")] / 
#          (sun.constants["h", "value"] * sun.constants["c", "value"] / 
#              (1E-9 * astm$wavelength[l]))
#       l <- l + 1
#    }
   # Fg is the nominator in the Eq defining the solar irradiance efficiency
#    Fg <- sum(Fg.vec)
   
#    total.irradiance <- tail(astm[, paste0(solar.model, ".radiance")], 1)
   
#    solar.irrad.eff <- 
#       bandgap * (Fg / total.irradiance)
   
   
   # vector with all wavelengths from minimum up to and including the bandgap
#    wavelength.min.to.bandgap <- 
#       wavelength[which(wavelength <= bandgap)]
#    
#    absorbed.photon.flux <- 
#       sunlight.ASTM(wavelength=wavelength.min.to.bandgap)[, paste0(solar.model, ".spectralradiance")]
#    
#    STH <- (currentdensity * electrode.potential * faradaic.efficiency) / total.irradiance

   STH <- 
      data.frame(wavelength = astm.wavelength,
                 bandgap = bandgap,
                 Fg = Fg,
                 Pt = Pt,
                 FgPt.ratio = Fg / Pt,
                 solar.irradiation.efficiency = solar.irrad.eff)
   
   return(STH)
}
