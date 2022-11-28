#' Calculate spectral radiance and other characteristics of a black-body
#'
#' @description Calculate the spectral radiance for an ideal black-body
#'     using Planck's law, for a specified wavelength range and step-size
#'
#' @param wavelength vector, in meter
#' @param temperature vector, blackbody surface temperature, in Kelvin
#'
#' @return dataframe
#' @export
sunlight.Planck <- function(wavelength, temperature = 5778) {
   # Throw error if wavelength contains any values <= 0
   # Throw error if temperature is T <= 0

   # tibble [11 × 6]
   sun.constants <- photoec::solarconstants

   factor.inversesquare <-
      (subset(sun.constants, label == "R.Sun")$value /
       subset(sun.constants, label == "R.AU")$value)^2

   # calculate spectral radiance for all wavelength values
   spectralradiance <-
      ((2 * pi * subset(sun.constants, label == "h")$value *
        subset(sun.constants, label == "c")$value^2) / (wavelength^5)) *
      (1 / (exp((subset(sun.constants, label == "h")$value *
                 subset(sun.constants, label == "c")$value) /
                (wavelength * subset(sun.constants, label == "k")$value *
                 temperature)) - 1))

   # based on theory (i.e., Planck's law) we can calculate Solar output
   # at the surface of the Sun as well as outside the Earth's atmosphere
   theory <-
      data.frame(
         wavelength = wavelength,
         #####################################################
         ### Characteristics of sunlight at the Sun's surface
         Sun.spectralradiance = spectralradiance,
         Sun.spectralradiance.powerterm =
            ((2 * pi * subset(sun.constants, label == "h")$value *
              subset(sun.constants, label == "c")$value^2) / (wavelength^5)),
         Sun.spectralradiance.expterm =
            (1 / (exp((subset(sun.constants, label == "h")$value *
                       subset(sun.constants, label == "c")$value) /
                      (wavelength * subset(sun.constants, label == "k")$value *
                       temperature)) - 1)),
         # spectral radiance numerically integrated using trapezoidal approx
         Sun.spectralradiance.trapz =
            c(0, common::trapz(wavelength, spectralradiance)),
         # radiance (total radiant power) units of \watt\per\square\metre
         Sun.radiance =
            cumsum(c(0, common::trapz(wavelength, spectralradiance))),
         # luminosity (total radiance times surface area) units of \watt
         Sun.luminosity =
            sum(c(0, common::trapz(wavelength, spectralradiance))) *
            subset(sun.constants, label == "A.Sun")$value,
         ######################################################################
         ### Characteristics of sunlight immediately outside Earth's atmosphere
         E.spectralradiance = factor.inversesquare * spectralradiance,
         E.spectralradiance.trapz =
            c(0, common::trapz(wavelength, factor.inversesquare * spectralradiance)),
         E.radiance =
            cumsum(c(
               0,
               common::trapz(wavelength, factor.inversesquare * spectralradiance))),
         # total luminosity hitting Earth's surface (day-side)
         E.luminosity =
            sum(c(
               0,
               common::trapz(wavelength, factor.inversesquare * spectralradiance))) *
            0.5 * subset(sun.constants, label == "A.Earth")$value
      )

   return(theory)
}
