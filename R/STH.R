#' Calculate maximum solar-to-hydrogen efficiency
#'
#' Calculate the maximum STH efficiency
#'
#' @param energy, energy/eV, numeric vector
#' @param currentdensity, current density/mA cm⁻²
#' @param max.irradiance, total irradiance (for specified model)/W m⁻², numeric scalar.
#'     Note that `max.irradiance` only takes precedence if model = ""
#' @param model, ASTM model: "AM1.5G" (default), "AM0", or "DNCS"
#' @param electrode.potential, electrode potential/eV, numeric scalar
#' @param faradaic.efficiency Faradaic efficiency, numeric (0 - 1)
#'
#' @return dataframe with 2 columns:
#'     \describe{
#'        \item{energy}{energy/eV, same as input vector}
#'        \item{STH}{STH/\%}
#'     }
#' @export
#'
#' @examples
#' \dontrun{
#'    STH(
#'       energy = photoec::sunlight.ASTM(model = model)$energy,
#'       currentdensity = photoec::sunlight.ASTM(model = model)[[paste0(model, ".currentdensity")]]
#'    )
#' }
STH <- function(
   energy,
   currentdensity,
   max.irradiance = 1000.371,
   model = "AM1.5G",
   electrode.potential = 1.23,
   faradaic.efficiency = 1) {

   # Note to future developer: R allows us to reference a default function arg in another function arg,
   # *but* seems to spaz out and throw weird errors if the default function arg is used inside
   # calls to other functions (while still inside the function declaration).
   # This is why we use the max.irradiance/model setup seen above (originally I wanted
   # max.irradiance = max(sunlight.ASTM(model = model)[[paste0(model, ".irradiance")]]), but that
   # did not work; like I said, weird errors about "model not defined".)
   # https://stackoverflow.com/questions/54755882/function-argument-that-depends-on-default-arguments

   # message(paste("model:", model))
   # message(paste("energy:", energy))
   # message(paste("currentdensity:", currentdensity))
   # message(paste("max.irradiance:", max.irradiance))
   # message(paste("electrode.potential:", electrode.potential))
   # message(paste("faradaic.efficiency:", faradaic.efficiency))

   #### arg checks
   models.allowed <- c("AM0", "AM1.5G", "DNCS")
   stopifnot(
      length(energy) == length(currentdensity),
      is.character(model), model %in% models.allowed,
      electrode.potential > 0,
      faradaic.efficiency >= 0, faradaic.efficiency <= 1)

   # if model is empty string, just leave max.irradiance as it was
   # if model is not empty string, reset max.irradiance based on model
   if (model != "") {
      # setting "model" resets "max.irradiance" value provided in function args
      max.irradiance <-
         max(photoec::sunlight.ASTM(model = model)[[paste0(model, ".irradiance")]])
   }

   result <- data.frame(
      energy = energy,
      STH = (currentdensity * electrode.potential * faradaic.efficiency) / max.irradiance)

   # set all STH values below the electrode potential to zero
   result$STH[which(result$energy < electrode.potential)] <- 0

   return(result)
}
