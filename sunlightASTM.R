source("/home/taha/chepec/chetex/common/R/common/trapz.R")

sunlight.ASTM <- 
   function(wavelength = c(seq(280, 400, 0.5), seq(401, 1700, 1), 
                           1702, seq(1705, 4000, 5)),
            model = "") {
   #' @title Calculate the spectral irradiance and other 
   #' @title sunlight characteristics based on ASTM model data
   #'
   #' @description
   #' Calculate the spectral irradiance for an sunlight at or below 
   #' Earth's atmosphere based on ASTM G173-03 Reference Spectra 
   #' Derived from SMARTS v. 2.9.2
   #'
   #' @details
   #' Calculate the spectral irradiance of sunlight on Earth
   #' based on ASTM G173-03 model for any wavelength between 
   #' 280 nm and 4000 nm with at most 0.01 nm resolution (interpolated). 
   #'
   #' @param wavelength: vector of wavelengths / nm
   #'        Defaults to the wavelength values used in the G173-03 model
   #' @examples
   #' sunlight.ASTM(wavelength = seq(400, 450, by = 0.1), model = "AM1.5G")
   #' @author Taha Ahmed <taha@@chepec.se>
   #' @return NOTE: which columns returned depends on model = ...
   #' @return wavelength                     / nm
   #' @return AM0.spectralradiance           / W m-2 nm-1
   #' @return AM1.5G.spectralradiance        / W m-2 nm-1
   #' @return DNCS.spectralradiance          / W m-2 nm-1
   #' @return AM0.spectralradiance.trapz     / W m-2
   #' @return AM1.5G.spectralradiance.trapz  / W m-2
   #' @return DNCS.spectralradiance.trapz    / W m-2
   #' @return AM0.radiance                   / W m-2
   #' @return AM1.5G.radiance                / W m-2
   #' @return DNCS.radiance                  / W m-2
   
   #### Check args
   # Check that wavelength limits lies within 280 nm and 4000 nm
   if ((min(wavelength) < 280) | 
          (max(wavelength) > 4000)) {
      stop(paste0("ASTM model does not extend beyond 280 nm -- 4000 nm.\n",
                  "Wavelength limits need to lie within that range."))
   }
      
   astm <- list()
   astm[["source"]] <-
      read.table("/home/taha/chepec/laboratory/PEC/ASTMG173.csv", 
                 skip = 1, 
                 header = T, 
                 sep = ",", 
                 dec = ".",
                 col.names = c("wavelength", "AM0", "AM1.5G", "DNCS"))
   ## Description of ASTMG173.csv columns
   # name        description                                  unit
   # ====        =================                            =================
   # wavelength  wavelength (280 nm - 4000 nm, 0.5 nm step)   /(m)
   # AM0         AM0 reference spectrum                       /(W * m-2 * nm-1)
   # AM1.5G      AM1.5 global tilt reference spectrum         /(W * m-2 * nm-1)
   # DNCS        direct normal circumsolar reference spectrum /(W * m-2 * nm-1)
   ## The ASTM data has at best a wavelength step of 0.5 nm
   # 280 nm -- 400 nm, step 0.5 nm
   # 400 nm -- 1700 nm, step 1.0 nm (there is also a datapoint at 1702 nm)
   # 1705 nm -- 4000 nm, step 5 nm
   
   
   # The following approach was decided some discussion with Pavlin and Seif
   # -- code is my own
   # ok, step through the user-submitted wavelength vector (element-by-element)
   # If wavelength matches one already existing in ASTMG173 dataframe, 
   # return AM0, AM1.5G and DNCS values for that wavelength - done!
   # If wavelength does not match, find straddling values in ASTMG173 dataframe
   # and use linear interpolation to calculate new AM0, AM1.5G and DNCS values.
   astm[["interp"]] <- 
      data.frame(wavelength = NULL,
                 AM0 = NULL,
                 AM1.5G = NULL,
                 DNCS = NULL)
   # tolerance for comparing floats
   epsilon <- 0.01 # nanometers
   for (i in 1:length(wavelength)) {
      if (any(abs(wavelength[i] - astm[["source"]]$wavelength) <= epsilon)) {
         # if wavelength[i] matches in astm[["source"]]
         astm[["interp"]] <- 
            rbind(astm[["interp"]],
                  astm[["source"]][
                     which(abs(wavelength[i] - 
                                  astm[["source"]]$wavelength) <= epsilon), ])
      } else {
         # wavelength does not match any already existing, so find 
         # value just-smaller and just-larger in source dataframe
         row.no.smaller.point <- 
            tail(which(astm[["source"]]$wavelength < wavelength[i]), 1)
         row.no.larger.point <- 
            head(which(astm[["source"]]$wavelength > wavelength[i]), 1)
         inflection <- 
            astm[["source"]][c(row.no.smaller.point, row.no.larger.point), ]
         
         # interpolate AM0, AM1.5G, and DNCS...
         astm[["interp"]] <- 
            rbind(astm[["interp"]],
                  data.frame(wavelength = wavelength[i],
                             AM0 = approx(x = inflection$wavelength, 
                                          y = inflection$AM0, 
                                          method = "linear", 
                                          xout = wavelength[i])$y,
                             AM1.5G = approx(x = inflection$wavelength, 
                                             y = inflection$AM1.5G, 
                                             method = "linear", 
                                             xout = wavelength[i])$y,
                             DNCS = approx(x = inflection$wavelength, 
                                           y = inflection$DNCS, 
                                           method = "linear", 
                                           xout = wavelength[i])$y))
      }
   }
   
   # reset row.names of astm[["interp"]]
   row.names(astm[["interp"]]) <- seq(1, dim(astm[["interp"]])[1])
   
   # Rename columns (in accordance with sunlight.Planck() terminology)
   names(astm[["interp"]])[2:length(astm[["interp"]])] <-
      paste0(names(astm[["interp"]])[2:length(astm[["interp"]])], 
             ".spectralradiance")
   
   
   # So now we have spectral radiances according to AM0, AM1.5G and DNCS.
   # We will now calculate integrated spectral radiance and radiance
   # (compare with sunlight.Planck() function).
   
   # calculate spectralradiance.trapz and assign to new columns
   for (k in 2:dim(astm[["interp"]])[2]) {
      astm[["interp"]] <- 
         cbind(astm[["interp"]], 
               c(0, trapz(astm[["interp"]]$wavelength, astm[["interp"]][, k])))
      colnames(astm[["interp"]])[dim(astm[["interp"]])[2]] <- 
         paste0(names(astm[["interp"]])[k], ".trapz")
   }
   
   
   # calculate radiance and assign to three new columns
   for (k in 5:dim(astm[["interp"]])[2]) {
      astm[["interp"]] <- 
         cbind(astm[["interp"]], 
               cumsum(c(0, trapz(astm[["interp"]]$wavelength, 
                                 astm[["interp"]][, k]))))
      colnames(astm[["interp"]])[dim(astm[["interp"]])[2]] <- 
         paste0(sub(pattern = "\\.[a-z]+$", replacement = "", 
                    x = names(astm[["interp"]])[k-3]), ".radiance")
   }
   
   
   
   # If arg "model" is not equal to either "AM1.5G", "AM0", or "DNCS",
   # return all three models. Otherwise return only the wanted model.
   if (model == "AM1.5G") {
      astm[["AM1.5G"]] <- 
         astm[["interp"]][, c(1, grep(pattern = "^AM1.5G", names(astm[["interp"]])))]
      # remove the "AM1.5G" label from all column names before return
      # (unnecessary since model selected explicitly)
      names(astm[["AM1.5G"]]) <-
          sub(pattern = "^AM1.5G.", 
              replacement = "", 
              x = names(astm[["AM1.5G"]]))
      return(astm[["AM1.5G"]])
   }
   # 
   if (model == "AM0") {
      astm[["AM0"]] <- 
         astm[["interp"]][, c(1, grep(pattern = "^AM0", names(astm[["interp"]])))]
      # remove the "AM0" label from all column names before return
      # (unnecessary since model selected explicitly)
      names(astm[["AM0"]]) <-
         sub(pattern = "^AM0.", 
             replacement = "", 
             x = names(astm[["AM0"]]))
      return(astm[["AM0"]])
   }
   #
   if (model == "DNCS") {
      astm[["DNCS"]] <- 
         astm[["interp"]][, c(1, grep(pattern = "^DNCS", names(astm[["interp"]])))]
      # remove the "DNCS" label from all column names before return
      # (unnecessary since model selected explicitly)
      names(astm[["DNCS"]]) <-
         sub(pattern = "^DNCS.", 
             replacement = "", 
             x = names(astm[["DNCS"]]))
      return(astm[["DNCS"]])
   }
   # 
   if (!(model %in% c("AM1.5G", "AM0", "DNCS"))) {
      return(astm[["interp"]])
   }
}

