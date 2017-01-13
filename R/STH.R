#' Calculate maximium solar-to-hydrogen efficiency
#'
#' Calculate the maximum STH efficiency
#'
#'
#' @param wavelength, in nanometer
#' @param currentdensity, in A m-2
#' @param total.irradiance, in W m-2 (note: scalar, not vector)
#' @param electrode.potential, in eV (note: scalar, not vector)
#' @param faradaic.efficiency scalar, not vector
#'
#' @return dataframe with 3 columns: : energy (eV), wavelength (nm), and current density (A m-2)
#' @export
STH <- function(wavelength,
                currentdensity,
                total.irradiance =
                   max(sunlight.ASTM(model = "AM1.5G")$irradiance),
                electrode.potential = 1.23,
                faradaic.efficiency = 1) {

   STH <- (currentdensity * electrode.potential * faradaic.efficiency) / total.irradiance

   df <- data.frame(energy = nm2eV(wavelength),
                    wavelength = wavelength,
                    STH = STH)

   return(df)
}
