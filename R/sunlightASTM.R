#' Calculate irradiances based on ASTM G173-03 reference
#'
#' @description Calculate the spectral irradiance for sunlight at or below
#'     Earth's atmosphere based on ASTM G173-03 reference spectra
#'
#' @details Calculate the spectral irradiance of sunlight on Earth
#'     based on ASTM G173-03 model for any wavelength between
#'     280 nm and 4000 nm with at best 0.01 nm resolution (interpolated).
#'
#' @param wavelength defaults to the wavelength values used in the G173-03 model
#' @param model accepts empty string (default), or "AM1.5G", "AM0", or "DNCS"
#'
#' @importFrom magrittr "%>%"
#'
#' @return NOTE: which model is returned depends on the "model" argument
#'     \describe{
#'        \item{wavelength}{Wavelength/nm}
#'        \item{<model>.spectralirradiance}{Spectral irradiance/W m⁻² nm⁻¹}
#'        \item{<model>.irradiance}{Cumulative irradiance/W m⁻²}
#'        \item{<model>.irradiance.fraction}{Relative irradiance (0-1)}
#'        \item{<model>.spectralphotonflux}{Spectral photon flux/s⁻¹ m⁻² nm⁻¹}
#'        \item{<model>.photonflux}{Cumulative photon flux/s⁻¹ m⁻²}
#'        \item{<model>.photonflux.fraction}{Relative photon flux (0-1)}
#'     }
#' @export
#'
#' @examples
#' \dontrun{
#'     sunlight.ASTM()
#'     sunlight.ASTM(model = "AM0")
#'     sunlight.ASTM(wavelength = seq(400, 450, by = 0.1), model = "AM1.5G")
#'     sunlight.ASTM(wavelength = seq(500, 600, by = 1))
#' }
sunlight.ASTM <- function(
   wavelength = c(seq(280, 400, 0.5), seq(401, 1700, 1), 1702, seq(1705, 4000, 5)),
   model = "") {

   #### Check args
   # Check that wavelength limits lies within 280 nm and 4000 nm
   if ((min(wavelength) < 280) || (max(wavelength) > 4000)) {
      stop(paste0(
         "ASTM model does not extend beyond 280 nm -- 4000 nm.\n",
         "Please adjust your wavelength range to lie within this limit."))
   }
   # Check that model is one of the allowed or empty string
   models.dataset <- c("AM0", "AM1.5G", "DNCS")
   models.allowed <- c(models.dataset, "")
   stopifnot(is.character(model), model %in% models.allowed)

   astm <- list()
   astm[["dataset"]] <- photoec::ASTMG173
   # rename column names in ASTMG173
   # note that this assignment is fragile; relies on column order in ASTMG173 not changing
   names(astm[["dataset"]]) <- c(
      "wavelength",
      paste0(models.dataset, ".spectralirradiance"))

   ##### Local variables
   c0 <- dplyr::filter(photoec::solarconstants, label=="c")$value
   planck <- dplyr::filter(photoec::solarconstants, label=="h")$value
   echarge <- dplyr::filter(photoec::solarconstants, label=="e")$value

   # The following approach was decided based on discussions with Pavlin Mitev and
   # Seif Alwan - much appreciated guys. Code is my own, any mistakes are my own.
   # Ok, step through the user-submitted wavelength vector (element-by-element)
   # If wavelength matches one already existing in ASTMG173 dataframe,
   # return AM0, AM1.5G and DNCS values for that wavelength - done!
   # If wavelength does not match, find straddling values in ASTMG173 dataframe
   # and use linear interpolation to calculate new AM0, AM1.5G and DNCS values.
   astm[["interp"]] <- data.frame(
      wavelength                = NULL,
      AM0.spectralirradiance    = NULL,
      AM1.5G.spectralirradiance = NULL,
      DNCS.spectralirradiance   = NULL)
   # tolerance for comparing floats
   epsilon <- 0.01 # nanometers
   for (i in 1:length(wavelength)) {
      if (any(abs(wavelength[i] - astm[["dataset"]]$wavelength) <= epsilon)) {
         # if wavelength[i] matches in astm[["dataset"]]
         astm[["interp"]] <- rbind(
            astm[["interp"]],
            astm[["dataset"]][which(abs(wavelength[i] - astm[["dataset"]]$wavelength) <= epsilon), ])
      } else {
         # wavelength does not match any already existing, so find
         # value just-smaller and just-larger in source dataframe
         row.no.smaller.point <- utils::tail(which(astm[["dataset"]]$wavelength < wavelength[i]), 1)
         row.no.larger.point <- utils::head(which(astm[["dataset"]]$wavelength > wavelength[i]), 1)
         inflection <- astm[["dataset"]][c(row.no.smaller.point, row.no.larger.point), ]

         # interpolate for AM0, AM1.5G, and DNCS
         astm[["interp"]] <- rbind(
            astm[["interp"]],
            data.frame(
               wavelength = wavelength[i],
               AM0.spectralirradiance = stats::approx(
                  x = inflection$wavelength,
                  y = inflection$AM0,
                  method = "linear",
                  xout = wavelength[i])$y,
               AM1.5G.spectralirradiance = stats::approx(
                  x = inflection$wavelength,
                  y = inflection$AM1.5G,
                  method = "linear",
                  xout = wavelength[i])$y,
               DNCS.spectralirradiance = stats::approx(
                  x = inflection$wavelength,
                  y = inflection$DNCS,
                  method = "linear",
                  xout = wavelength[i])$y))
      }
   }

   # reset row.names of astm[["interp"]] (just in case)
   row.names(astm[["interp"]]) <- seq(1, dim(astm[["interp"]])[1])

   # So now we have spectral irradiances according to the models AM0, AM1.5G and DNCS.
   # Each model is a column (dataframe in so-called wide format).
   # For each derived property below, we assign three new columns (one for each model).

   # based on the integrated values, calculate cumulative irradiance
   this.property <- astm$interp %>% dplyr::select(dplyr::ends_with(".spectralirradiance"))
   for (k in 1:dim(this.property)[2]) {
      astm$interp[[paste0(models.dataset[k], ".irradiance")]] <-
         # irradiance (cumulative) is calculated by first integrating under each slice
         # of the curve (here using trapezoidal approximation) then summing
         cumsum(c(0, common::trapz(astm$interp$wavelength, this.property[, k])))
   }

   # based on cumulative irradiance, calculate irradiance fraction
   this.property <- astm$interp %>% dplyr::select(dplyr::ends_with(".irradiance"))
   for (k in 1:dim(this.property)[2]) {
      astm$interp[[paste0(names(this.property)[k], ".fraction")]] <-
         this.property[, k] / utils::tail(this.property[, k], 1)
   }

   # based on spectral irradiance, calculate spectral photon flux
   this.property <- astm$interp %>% dplyr::select(dplyr::ends_with(".spectralirradiance"))
   for (k in 1:dim(this.property)[2]) {
      astm$interp[[paste0(models.dataset[k], ".spectralphotonflux")]] <-
         this.property[, k] * (astm$interp$wavelength / (1E9 * c0 * planck))
   }

   # based on spectral photon flux, calculate cumulative photon flux
   this.property <- astm$interp %>% dplyr::select(dplyr::ends_with(".spectralphotonflux"))
   for (k in 1:dim(this.property)[2]) {
      astm$interp[[paste0(models.dataset[k], ".photonflux")]] <-
         cumsum(c(0, common::trapz(astm$interp$wavelength, this.property[, k])))
   }

   # based on cumulative photon flux, calculate flux fraction
   this.property <- astm$interp %>% dplyr::select(dplyr::ends_with(".photonflux"))
   for (k in 1:dim(this.property)[2]) {
      astm$interp[[paste0(names(this.property)[k], ".fraction")]] <-
         this.property[, k] / utils::tail(this.property[, k], 1)
   }

   # based on cumulative photon flux, calculate maximum current density (assuming EQE=1 and no bandgap)
   # note that this property is "cumulative" in the sense established in this function,
   # although not usually described as such
   this.property <- astm$interp %>% dplyr::select(dplyr::ends_with(".photonflux"))
   for (k in 1:dim(this.property)[2]) {
      astm$interp[[paste0(models.dataset[k], ".currentdensity")]] <-
         this.property[, k] * echarge
   }

   # based on current density, calculate maximum solar-to-hydrogen (STH) efficiency
   # (assuming Faradaic efficiency of unity and electrode potential 1.23 V
   # note that this property is "cumulative" in the sense established in this function,
   # although not usually described as such
   this.property <- astm$interp %>% dplyr::select(dplyr::ends_with(".currentdensity"))
   for (k in 1:dim(this.property)[2]) {
      astm$interp[[paste0(models.dataset[k], ".solartohydrogen")]] <-
         (this.property[, k] * 1.23) /
         utils::tail((astm$interp %>% dplyr::select(dplyr::ends_with(".irradiance")))[, k], 1)
   }

   # return block
   if (!(model %in% models.dataset)) {
      # if arg "model" is empty string, return all three models
      return(
         astm$interp %>%
         tibble::add_column(energy = photoec::wavelength2energy(astm$interp$wavelength), .after = "wavelength")
      )
   } else {
      # otherwise return only the requested model
      return(
         astm$interp %>%
         dplyr::select(wavelength, dplyr::starts_with(model)) %>%
         tibble::add_column(energy = photoec::wavelength2energy(astm$interp$wavelength), .after = "wavelength")
      )
   }
}
