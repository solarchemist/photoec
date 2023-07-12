#' Black body spectral radiance, radiance and luminosity
#'
#' Calculate spectral radiance, radiance and luminosity of a black body
#'
#' @description Calculate the spectral radiance, radiance and luminosity
#'     for a black body with surface temperature \code{temperature} (e.g., the Sun
#'     has a surface temperature of 5772 K) using Planck's law, for a
#'     specified wavelength range.
#'
#' @param wavelength wavelength/m, numeric vector
#'     Please note: expected unit is **meter** and *not* nanometer!
#' @param temperature black body surface temperature/Kelvin, numeric scalar
#'
#' @return dataframe with 9 columns:
#'     \describe{
#'        \item{wavelength}{Wavelength/nm}
#'        \item{Sun.spectralradiance}{Spectral radiance of the black body per Planck's law/W m⁻² nm⁻¹}
#'        \item{Sun.spectralradiance.powerterm}{Power term of Planck's law/W m⁻² nm⁻¹}
#'        \item{Sun.spectralradiance.expterm}{Exponential term of Planck's law/1}
#'        \item{Sun.radiance}{Cumulative radiance/W m⁻²}
#'        \item{Sun.luminosity}{Luminosity of the spherical black body/W}
#'        \item{Earth.spectralirradiance}{Spectral irradiance at a distance of 1 AU from the black body/W m⁻² nm⁻¹}
#'        \item{Earth.irradiance}{Irradiance at a distance of 1 AU from the black body/W m⁻²}
#'        \item{Earth.luminosity}{Luminosity impinging on Earth (hemisphere)/W}
#'     }
#' @export
sunlight.Planck <- function(
   wavelength = seq(1E-7, 4E-6, 1E-9), # 100 nm - 4000 nm, stepsize 1 nm
   temperature = dplyr::filter(photoec::solarconstants, label == "T.Sun")$value) {

   stopifnot(
      # stop if wavelength contains any values <= 0
      all(wavelength > 0),
      # stop if temperature is T <= 0
      temperature > 0)


   ##### Local variables
   c0         <- dplyr::filter(photoec::solarconstants, label=="c")$value
   planck     <- dplyr::filter(photoec::solarconstants, label=="h")$value
   boltzmann  <- dplyr::filter(photoec::solarconstants, label=="k")$value
   r.Sun      <- dplyr::filter(photoec::solarconstants, label=="R.Sun")$value
   r.AU       <- dplyr::filter(photoec::solarconstants, label=="R.AU")$value
   area.Sun   <- dplyr::filter(photoec::solarconstants, label=="A.Sun")$value
   area.Earth <- dplyr::filter(photoec::solarconstants, label=="A.Earth")$value

   # spectral radiance at Sun's surface using Planck's law
   sprad.power.term <- (2 * pi * planck * c0^2) / (wavelength^5)
   sprad.exp.term   <- 1 / (exp((planck * c0) / (wavelength * boltzmann * temperature)) - 1)
   spectralradiance <- sprad.power.term * sprad.exp.term

   # based on theory (i.e., Planck's law) we can calculate Solar output
   # at the surface of the Sun as well as outside the Earth's atmosphere
   theory <-
      data.frame(
         wavelength = wavelength,
         # cannot easily know unit of provided wavelength (without proper units support)
         # which makes conversion to eV a very complex affair best avoided at this point
         # energy = photoec::wavelength2energy(1E9 * wavelength),
         #####################################################
         ### Characteristics of sunlight at the Sun's surface
         Sun.spectralradiance = spectralradiance,
         Sun.spectralradiance.powerterm = sprad.power.term,
         Sun.spectralradiance.expterm = sprad.exp.term,
         # radiance (total radiant power) units of \watt\per\square\metre
         Sun.radiance = cumsum(c(0, common::trapz(wavelength, spectralradiance))),
         # luminosity (total radiance times surface area) units of \watt
         Sun.luminosity = area.Sun * sum(c(0, common::trapz(wavelength, spectralradiance))),
         ######################################################################
         ### Characteristics of sunlight immediately outside Earth's atmosphere
         Earth.spectralirradiance = (r.Sun / r.AU)^2 * spectralradiance,
         Earth.irradiance = cumsum(c(
            0, common::trapz(wavelength, (r.Sun / r.AU)^2 * spectralradiance))),
         # total luminosity hitting Earth's surface (day-side)
         Earth.luminosity = 0.5 * area.Earth * sum(c(
            0, common::trapz(wavelength, (r.Sun / r.AU)^2 * spectralradiance))))

   return(theory)
}
