#' Calculate air mass (AM) from zenith angle
#'
#' @description Calculate the air mass value from the zenith angle
#'
#' @details Calculate the air mass (AM) value using the secant approximation.
#'     Note that this function only returns physically meaningful values
#'     for z <= 70°,
#'
#' @param z zenith angle in degrees (default 48.2° corresponds to AM1.5)
#'
#' @seealso https://en.wikipedia.org/wiki/Air_mass_(astronomy)
#'
#' @return air mass value, numeric
#' @export
#'
#' @examples
#' \dontrun{
#'     airmass()
#'     airmass(z = 59.83919) # latitude of chemistry dept at Ångström laboratory
#' }
airmass <- function(z = 48.2) {
   # the approximate secant formula works well for z < 70
   return(1 / cos(common::as.radians(z)))
}


#' Calculate zenith angle from air mass (AM)
#'
#' @description Calculate zenith angle corresponding to an air mass value
#'
#' @details Calculate angle from the zenith line corresponding to an air mass value.
#'     This is the inverted function of \code{\link{airmass}}.
#'     Except I can't seem to figure out how to express it...
#'
#' @param AM air mass, numeric (default 1.5)
#'
#' @return zenith angle, numeric
angzenith <- function(AM = 1.5) {
   # NOTE, at the moment this function does nothing!
   # It is also not exported.
   return(AM)
}


#' Astronomical refraction
#'
#' @description Calculate the astronomical refraction from zenith angle
#'
#' @details Calculate the astronomical refraction, R,
#'     based on Oriani's power series in terms of tan Z (where Z is
#'     the zenith angle) neglecting terms beyond third order and under
#'     the assumption that the height of the atmosphere is small compared
#'     to the radius of the Earth.
#'     This function works well up to around 70° of zenith angle, but will
#'     return non-physical values at higher angles.
#'     Astronomical refraction becomes complicated close to the horizon (for more
#'     on that, see quoted sources).
#'
#' @seealso Mahan, A.I., 1962. Astronomical refraction–some history and theories. Appl. Opt., AO 1, 497–511. https://doi.org/10.1364/AO.1.000497
#' @seealso Woolard, E.W., Clemence, G.M., 1966. Spherical astronomy. Elsevier.
#' @seealso Auer, L.H., Standish, E.M., 2000. Astronomical refraction: computational method for all zenith angles. AJ 119, 2472. https://doi.org/10.1086/301325
#'
#' @param z zenith angle in degrees
#'
#' @return refraction, in degrees
#'     You might want to convert this value back to arcseconds using
#'     \code{\link[common]{degrees2asec}}
#'
#' @export
astrorefraction <- function(z = 48.2) {
   # here we use the constants as calculated by Woolard & Clemence, 1966 (chapter 5, page 84)
   A <- common::asec2degrees(60.29)
   B <- common::asec2degrees(0.06688)
   R <- A * tan(common::as.radians(z)) - B * tan(common::as.radians(z))^3
   return(R)
}
