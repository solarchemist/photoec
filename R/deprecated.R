#' nm2eV
#'
#' Convert from nanometer to electron volt
#' DEPRECATED. Please use wavelength2energy() instead.
#'
#' @param nm vector or scalar, in nm
#'
#' @return electron volts
#' @export
nm2eV <- function(nm) {
   .Deprecated("wavelength2energy")
   # Converts wavelength in nm to energy in eV
   eV <-
      subset(photoec::solarconstants, label == "h.eV")$value *
      1E9 *
      subset(photoec::solarconstants, label == "c")$value /
      nm
   return(eV)
}


#' eV2nm
#'
#' Convert from electron volt to nanometer
#' DEPRECATED. Please use energy2wavelength() instead.
#'
#' @param eV vector or scalar, in eV
#'
#' @return nanometers
#' @export
eV2nm <- function(eV) {
   .Deprecated("energy2wavelength")
   # Converts energy in eV to wavelength in nm
   nm <-
      subset(photoec::solarconstants, label == "h.eV")$value *
      1E9 *
      subset(photoec::solarconstants, label == "c")$value /
      eV
   return(nm)
}


#' Photon flux at a particular wavelength
#'
#' DEPRECATED. Please use \link{sunlight.ASTM()} instead.
#'
#' @param wavelength One or more wavelength values, in nanometres
#' @param model defaults to AM1.5G. Options are: AM0, AM1.5G, or DNCS
#'
#' @importFrom magrittr "%>%"
#'
#' @return Vector with flux values at the given wavelength values.
#' @export
#' @importFrom rlang .data
flux <- function(wavelength, model = "AM1.5G") {
   .Deprecated(sunlight.ASTM)
   # check that argument model is not empty string and one of
   # "AM1.5G", "AM0", or "DNCS"
   if (!is.character(model) | !(model %in% c("AM1.5G", "AM0", "DNCS"))) {
      stop("The argument 'model' must be one of: 'AM1.5G', 'AM0', or 'DNCS'.")
   }

   solarconstants <- photoec::solarconstants
   astm.data <-
      photoec::sunlight.ASTM() %>%
      dplyr::select(.data$wavelength, .data$energy, dplyr::starts_with(model))
   # strip that part of the name from every column
   names(astm.data) <- sub(paste0("^", model, "\\."), "", names(astm.data))

   # check that wavelength is inside the ASTM range: 280 nm -- 4000 nm
   if (wavelength < min(astm.data$wavelength) | wavelength > max(astm.data$wavelength)) {
      stop("Extrapolation is not possible. Wavelength must lie inside the range 280 nm -- 4000 nm (inclusive).")
   }

   c0 <- dplyr::filter(photoec::solarconstants, label=="c")$value
   planck <- dplyr::filter(photoec::solarconstants, label=="h")$value
   astm.data$flux <-
      astm.data$spectralirradiance * (astm.data$wavelength / (1E9 * c0 * planck))

   # use linear interpolation to calculate the flux at the wavelength in question
   return(
      stats::approx(
         x = astm.data$wavelength,
         y = astm.data$flux,
         xout = wavelength,
         method = "linear")$y)
}


#' Total photon flux across one ore more wavelength ranges
#'
#' DEPRECATED. please use \link{sunlight.ASTM()} instead.
#'
#' @param wl.start numeric vector, in nanometres
#' @param wl.stop numeric vector, in nanometres
#' @param model string, defaults to AM1.5G. Options are: AM0, AM1.5G, or DNCS
#'
#' @importFrom magrittr "%>%"
#'
#' @return the total flux across the wl range, number (vector if more than one range)
#' @importFrom rlang .data
#' @export
cumflux <- function(wl.start = 280, wl.stop = 4000, model = "AM1.5G") {
   .Deprecated("sunlight.ASTM")
   # I would like this function to work like this:
   # if the  user supplies just one number, that should be interpreted as wl.stop
   # (the reason I don't just switch places of start and stop in the function header
   #  is because that would be confusing, in my opinion).
   # The following if-clause accomplishes this, plus gracefully handles one edge-case
   # where the user explicitly states only wl.stop.
   if ((nargs() == 1)) {
      if (missing(wl.start)) {
         # reset the length of wl.start to match that of wl.stop
         wl.start <- rep(280, length(wl.stop))
      } else {
         message("cumflux(): using supplied value as wl.stop and resetting wl.start to 280 nm")
         wl.stop <- wl.start
         wl.start <- rep(280, length(wl.stop))
      }
   }

   # check that wl.start and wl.stop are the same length
   if (length(wl.start) != length(wl.stop)) stop("wl.start and wl.stop must be the same length")

   # check that argument model is not empty string and one of
   # "AM1.5G", "AM0", or "DNCS"
   if (!is.character(model) | !(model %in% c("AM1.5G", "AM0", "DNCS"))) {
      stop("The argument 'model' must be one of: 'AM1.5G', 'AM0', or 'DNCS'.")
   }

   solarconstants <- photoec::solarconstants
   astm.data <- photoec::sunlight.ASTM(model = model)

   # check that wavelength is inside the ASTM range: 280 nm -- 4000 nm
   if (any(wl.start < min(astm.data$wavelength), na.rm = T) | any(wl.start > max(astm.data$wavelength), na.rm = T)) {
      stop("Extrapolation is not possible. Wavelength must lie inside the range 280 nm -- 4000 nm (inclusive).")
   }
   if (any(wl.stop < min(astm.data$wavelength), na.rm = T) | any(wl.stop > max(astm.data$wavelength), na.rm = T)) {
      stop("Extrapolation is not possible. Wavelength must lie inside the range 280 nm -- 4000 nm (inclusive).")
   }

   # this loop is to handle more than one element in wl.start or wl.stop
   cflux <- NULL
   for (i in 1:length(wl.start)) {
      # if wl.start[i] or wl.stop[i] is.na() == TRUE, skip this iteration and move to the next one
      if (is.na(wl.start[i]) | is.na(wl.stop[i])) {
         cflux <- c(cflux, NA)
         next
      }

      this.data <-
         astm.data %>%
         dplyr::filter(.data$wavelength > wl.start[i] & .data$wavelength < wl.stop[i])
      # strip that part of the name from every column
      names(astm.data) <- sub(paste0("^", model, "\\."), "", names(astm.data))
      names(this.data) <- sub(paste0("^", model, "\\."), "", names(this.data))

      # add the user's max-wl to the df
      if (max(this.data$wavelength) != wl.stop[i]) {
         this.data <- rbind(this.data,
                            data.frame(wavelength = wl.stop[i],
                                       energy = photoec::wavelength2energy(wl.stop[i]),
                                       # get spectral irradiance at wavelength by linear interpolation
                                       spectralirradiance = stats::approx(astm.data$wavelength,
                                                                          astm.data$spectralirradiance,
                                                                          method = "linear",
                                                                          xout = wl.stop[i])$y,
                                       # get spectral irradiance trapz at wl by linear interp
                                       spectralirradiance.trapz = stats::approx(astm.data$wavelength,
                                                                                astm.data$spectralirradiance.trapz,
                                                                                method = "linear",
                                                                                xout = wl.stop[i])$y,
                                       # get irradiance at wavelength by linear interpolation
                                       irradiance = stats::approx(astm.data$wavelength,
                                                                  astm.data$irradiance,
                                                                  method = "linear",
                                                                  xout = wl.stop[i])$y,
                                       # get irradiance fraction at wavelength by linear interpolation
                                       irradiance.fraction = stats::approx(astm.data$wavelength,
                                                                           astm.data$irradiance.fraction,
                                                                           method = "linear",
                                                                           xout = wl.stop[i])$y))
      }
      # add the user's min-wl to the df
      if (min(this.data$wavelength) != wl.start[i]) {
         this.data <- rbind(data.frame(wavelength = wl.start[i],
                                       energy = photoec::wavelength2energy(wl.start[i]),
                                       # get spectral irradiance at wavelength by linear interpolation
                                       spectralirradiance = stats::approx(astm.data$wavelength,
                                                                          astm.data$spectralirradiance,
                                                                          method = "linear",
                                                                          xout = wl.start[i])$y,
                                       # get spectral irradiance trapz at wl by linear interp
                                       spectralirradiance.trapz = stats::approx(astm.data$wavelength,
                                                                                astm.data$spectralirradiance.trapz,
                                                                                method = "linear",
                                                                                xout = wl.start[i])$y,
                                       # get irradiance at wavelength by linear interpolation
                                       irradiance = stats::approx(astm.data$wavelength,
                                                                  astm.data$irradiance,
                                                                  method = "linear",
                                                                  xout = wl.start[i])$y,
                                       # get irradiance fraction at wavelength by linear interpolation
                                       irradiance.fraction = stats::approx(astm.data$wavelength,
                                                                           astm.data$irradiance.fraction,
                                                                           method = "linear",
                                                                           xout = wl.start[i])$y),
                            this.data)
      }

      c0 <- dplyr::filter(photoec::solarconstants, label=="c")$value
      planck <- dplyr::filter(photoec::solarconstants, label=="h")$value
      this.data$flux <-
         this.data$spectralirradiance * (this.data$wavelength / (1E9 * c0 * planck))
      this.data$cumflux <- cumsum(c(0, common::trapz(this.data$wavelength, this.data$flux)))
      cflux <- c(cflux, this.data$cumflux[length(this.data$cumflux)])
   }

   # return number of photons (per second per square metre) across the specified wavelength range
   return(cflux)
}
