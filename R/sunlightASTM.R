#' Calculate irradiances based on ASTM G173-03 reference
#'
#' @description Calculate the spectral irradiance for sunlight at or below
#'     Earth's atmosphere based on ASTM G173-03 reference spectra
#'
#' @details Calculate the spectral irradiance of sunlight on Earth
#'     based on ASTM G173-03 model for any wavelength between
#'     280 nm and 4000 nm with at best 0.01 nm resolution (interpolated).
#'
#' @param wavelength defaults to the wavelength values used in the G173-03 model
#' @param model defaults to empty string, accepts "AM1.5G", "AM0", or "DNCS"
#'
#' @return NOTE: which columns are returned depends on the argument model = ...
#'     \describe{
#'        \item{wavelength}{Wavelength, in nm}
#'        \item{AM0.spectralirradiance}{W m-2 nm-1}
#'        \item{AM1.5G.spectralirradiance}{W m-2 nm-1}
#'        \item{DNCS.spectralirradiance}{W m-2 nm-1}
#'        \item{AM0.spectralirradiance.trapz}{W m-2}
#'        \item{AM1.5G.spectralirradiance.trapz}{W m-2}
#'        \item{DNCS.spectralirradiance.trapz}{W m-2}
#'        \item{AM0.irradiance}{W m-2}
#'        \item{AM1.5G.irradiance}{W m-2}
#'        \item{DNCS.irradiance}{W m-2}
#'        \item{AM0.irradiance.fraction}{fraction}
#'        \item{AM1.5G.irradiance.fraction}{fraction}
#'        \item{DNCS.irradiance.fraction}{fraction}
#'     }
#' @export
#'
#' @examples
#' \dontrun{
#' sunlight.ASTM(wavelength = seq(400, 450, by = 0.1), model = "AM1.5G")
#' }
sunlight.ASTM <- function(wavelength = c(seq(280, 400, 0.5),
                                         seq(401, 1700, 1),
                                         1702,
                                         seq(1705, 4000, 5)),
                          model = "") {

   #### Check args
   # Check that wavelength limits lies within 280 nm and 4000 nm
   if ((min(wavelength) < 280) |
       (max(wavelength) > 4000)) {
      stop(paste0("ASTM model does not extend beyond 280 nm -- 4000 nm.\n",
                  "Wavelength limits need to lie within that range."))
   }

   astm <- list()
   astm[["source"]] <- photoec::ASTMG173
   names(astm[["source"]]) <-
      c("wavelength",
        "AM0",
        "AM1.5G",
        "DNCS")
   ## Description of ASTMG173.csv columns
   # name        description                                  unit
   # ====        =================                            =================
   # wavelength  wavelength (280 nm - 4000 nm, 0.5 nm step)   /(nm)
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
            utils::tail(which(astm[["source"]]$wavelength < wavelength[i]), 1)
         row.no.larger.point <-
            utils::head(which(astm[["source"]]$wavelength > wavelength[i]), 1)
         inflection <-
            astm[["source"]][c(row.no.smaller.point, row.no.larger.point), ]

         # interpolate AM0, AM1.5G, and DNCS...
         astm[["interp"]] <-
            rbind(astm[["interp"]],
                  data.frame(wavelength = wavelength[i],
                             AM0 = stats::approx(x = inflection$wavelength,
                                                 y = inflection$AM0,
                                                 method = "linear",
                                                 xout = wavelength[i])$y,
                             AM1.5G = stats::approx(x = inflection$wavelength,
                                                    y = inflection$AM1.5G,
                                                    method = "linear",
                                                    xout = wavelength[i])$y,
                             DNCS = stats::approx(x = inflection$wavelength,
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
             ".spectralirradiance")

   # So now we have spectral radiances according to AM0, AM1.5G and DNCS.
   # We will now calculate integrated spectral radiance and radiance
   # (compare with sunlight.Planck() function).

   # calculate spectralirradiance.trapz and assign to new columns
   for (k in 2:dim(astm[["interp"]])[2]) {
      astm[["interp"]] <-
         cbind(astm[["interp"]],
               c(0, common::trapz(astm[["interp"]]$wavelength, astm[["interp"]][, k])))
      colnames(astm[["interp"]])[dim(astm[["interp"]])[2]] <-
         paste0(names(astm[["interp"]])[k], ".trapz")
   }


   # calculate irradiance and assign to three new columns
   for (k in 5:dim(astm[["interp"]])[2]) {
      irradiance <- rep(0, dim(astm[["interp"]])[1])
      for (j in 2:dim(astm[["interp"]])[1]) {
         irradiance[j] <- irradiance[j - 1] + astm[["interp"]][j, k]
      }
      astm[["interp"]] <-
         cbind(astm[["interp"]],
               irradiance)
      colnames(astm[["interp"]])[dim(astm[["interp"]])[2]] <-
         paste0(sub(pattern = "\\.[a-z]+$", replacement = "",
                    x = names(astm[["interp"]])[k-3]), ".irradiance")
   }


   # calculate radiance fraction and assign to three new columns
   for (k in 8:dim(astm[["interp"]])[2]) {
      astm[["interp"]] <-
         cbind(astm[["interp"]],
               astm[["interp"]][, k] / utils::tail(astm[["interp"]][, k], 1))
      colnames(astm[["interp"]])[dim(astm[["interp"]])[2]] <-
         paste0(names(astm[["interp"]])[k], ".fraction")
   }



   # If arg "model" is not equal to either "AM1.5G", "AM0", or "DNCS",
   # return all three models. Otherwise return only the wanted model.
   astm.model <- "AM1.5G"
   if (model == astm.model) {
      astm[[astm.model]] <-
         cbind(model = astm.model,
               energy = photoec::wavelength2energy(astm[["interp"]]$wavelength),
               astm[["interp"]][, c(1, grep(pattern = paste0("^", astm.model),
                                            x = names(astm[["interp"]])))])
      # remove the "AM1.5G" label from all column names before return
      # (unnecessary since model selected explicitly)
      names(astm[[astm.model]]) <-
         sub(pattern = paste0("^", astm.model, "."),
             replacement = "",
             x = names(astm[[astm.model]]))
      return(astm[[astm.model]])
   }
   #
   astm.model = "AM0"
   if (model == astm.model) {
      astm[[astm.model]] <-
         cbind(model = astm.model,
               energy = photoec::wavelength2energy(astm[["interp"]]$wavelength),
               astm[["interp"]][, c(1, grep(pattern = paste0("^", astm.model),
                                            x = names(astm[["interp"]])))])
      # remove the "AM0" label from all column names before return
      # (unnecessary since model selected explicitly)
      names(astm[[astm.model]]) <-
         sub(pattern = paste0("^", astm.model, "."),
             replacement = "",
             x = names(astm[[astm.model]]))
      return(astm[[astm.model]])
   }
   #
   astm.model = "DNCS"
   if (model == astm.model) {
      astm[[astm.model]] <-
         cbind(model = astm.model,
               energy = photoec::wavelength2energy(astm[["interp"]]$wavelength),
               astm[["interp"]][, c(1, grep(pattern = paste0("^", astm.model),
                                            x = names(astm[["interp"]])))])
      # remove the "DNCS" label from all column names before return
      # (unnecessary since model selected explicitly)
      names(astm[[astm.model]]) <-
         sub(pattern = paste0("^", astm.model, "."),
             replacement = "",
             x = names(astm[[astm.model]]))
      return(astm[[astm.model]])
   }
   #
   if (!(model %in% c("AM1.5G", "AM0", "DNCS"))) {
      return(cbind(energy = photoec::wavelength2energy(astm[["interp"]]$wavelength),
                   astm[["interp"]]))
   }
}
