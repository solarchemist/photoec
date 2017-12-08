#' Calculate the photon flux
#'
#' Calculate photon flux from spectral irradiance
#'
#' @details Provide wavelength and spectral irradiance.
#'     It is important that provided wavelength and spectral irradiance
#'     use the same length unit (for example, nm and W m-2 nm-1, or m and W m-2 m-1).
#'
#' @param wavelength, usually in nm
#' @param spectralirradiance, usually in W m-2 nm-1
#'
#' @return Dataframe with 4 columns:
#'     \describe{
#'        \item{photonflux}{Photons per unit area, photon flux / s-1 m-2 nm-1}
#'        \item{photonflux.trapz}{numerically integrated}
#'        \item{photonflux.csum}{cumulative photon flux, s-1 m-2}
#'        \item{photonflux.fraction}{photon flux fraction (from 0 - 1)}
#' }
#' @export
photonflux <- function(wavelength, spectralirradiance) {
   solar.constants <- photoec::solarconstants

   # flux is the rate of flow of a property per unit area
   # here we have flux / s-1 m-2 nm-1
   photonflux <-
      spectralirradiance *
      (wavelength /
          (1E9 * subset(solar.constants, label == "c")$value *
              subset(solar.constants, label == "h")$value))
   # integrate numerically under curve with trapz()
   photonflux.trapz <-
      c(0,
        common::trapz(x = wavelength,
                      y = photonflux))
   # calculate cumulative sum (i.e., flux / s-1 m-2)
   photonflux.csum <- cumsum(photonflux.trapz)
   # photon flux fraction (from 0 - 1)
   photonflux.fraction <-
      photonflux.csum / utils::tail(photonflux.csum, 1)

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
