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
#' @param model accepts empty string (default), or "AM1.5G", "AM0", or "DNCS"
#'
#' @return NOTE: which model is returned depends on the "model" argument
#'     \describe{
#'        \item{wavelength}{Wavelength, in nm}
#'        \item{<model>.spectralirradiance}{W m-2 nm-1}
#'        \item{<model>.spectralirradiance.trapz}{W m-2}
#'        \item{<model>.irradiance}{W m-2}
#'        \item{<model>.irradiance.fraction}{fraction}
#'     }
#' @export
#'
#' @examples
#' \dontrun{
#' sunlight.ASTM(wavelength = seq(400, 450, by = 0.1), model = "AM1.5G")
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
   stopifnot(model %in% models.allowed)

   astm <- list()
   astm[["dataset"]] <- photoec::ASTMG173
   # rename column names in ASTMG173
   # note that this assignment is fragile; relies on column order in ASTMG173 not changing
   names(astm[["dataset"]]) <- c("wavelength", "AM0", "AM1.5G", "DNCS")


   # The following approach was decided based on discussions with Pavlin Mitev and
   # Seif Alwan - much appreciated guys. Code is my own, any mistakes are my own.
   # Ok, step through the user-submitted wavelength vector (element-by-element)
   # If wavelength matches one already existing in ASTMG173 dataframe,
   # return AM0, AM1.5G and DNCS values for that wavelength - done!
   # If wavelength does not match, find straddling values in ASTMG173 dataframe
   # and use linear interpolation to calculate new AM0, AM1.5G and DNCS values.
   astm[["interp"]] <- data.frame(
      wavelength = NULL,
      AM0        = NULL,
      AM1.5G     = NULL,
      DNCS       = NULL)
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

         # interpolate AM0, AM1.5G, and DNCS...
         astm[["interp"]] <- rbind(
            astm[["interp"]],
            data.frame(
               wavelength = wavelength[i],
               AM0 = stats::approx(
                  x = inflection$wavelength,
                  y = inflection$AM0,
                  method = "linear",
                  xout = wavelength[i])$y,
               AM1.5G = stats::approx(
                  x = inflection$wavelength,
                  y = inflection$AM1.5G,
                  method = "linear",
                  xout = wavelength[i])$y,
               DNCS = stats::approx(
                  x = inflection$wavelength,
                  y = inflection$DNCS,
                  method = "linear",
                  xout = wavelength[i])$y))
      }
   }

   # reset row.names of astm[["interp"]]
   row.names(astm[["interp"]]) <- seq(1, dim(astm[["interp"]])[1])

   # So now we have spectral irradiances according to AM0, AM1.5G and DNCS.
   # We will now calculate integrated spectral irradiance (trapz) and irradiance (cumulative).

   # integrate trapz for each model and assign to new columns
   for (k in 2:dim(astm$interp)[2]) {
      astm$interp[[paste0(names(astm$interp)[k], ".trapz")]] <- c(
         0,
         common::trapz(astm$interp$wavelength, astm$interp[, k]))
   }

   # based on the integrated values, calculate cumulative irradiance and assign to new columns
   for (k in 5:dim(astm[["interp"]])[2]) {
      irradiance <- rep(0, dim(astm[["interp"]])[1])
      for (j in 2:dim(astm[["interp"]])[1]) {
         irradiance[j] <- irradiance[j - 1] + astm[["interp"]][j, k]
      }
      astm[["interp"]] <- cbind(astm[["interp"]], irradiance)
      colnames(astm[["interp"]])[dim(astm[["interp"]])[2]] <- paste0(
         sub(
            pattern = "\\.[a-z]+$",
            replacement = "",
            x = names(astm[["interp"]])[k-3]),
         ".irradiance")
   }

   # based on cumulative irradiance, calculate irradiance fraction and assign to new columns
   for (k in 8:dim(astm[["interp"]])[2]) {
      astm[["interp"]] <- cbind(
         astm[["interp"]],
         astm[["interp"]][, k] / utils::tail(astm[["interp"]][, k], 1))
      colnames(astm[["interp"]])[dim(astm[["interp"]])[2]] <-
         paste0(names(astm[["interp"]])[k], ".fraction")
   }


   # return block
   if (!(model %in% models.dataset)) {
      # if arg "model" is not equal to any of models.dataset, return all three models.
      return(cbind(
         energy = photoec::wavelength2energy(astm[["interp"]]$wavelength),
         astm[["interp"]]))
   } else {
      # otherwise return only the requested model
      astm[[model]] <- cbind(
         model = model,
         energy = photoec::wavelength2energy(astm[["interp"]]$wavelength),
         astm[["interp"]][, c(
            1,
            grep(
               pattern = paste0("^", model),
               x = names(astm[["interp"]])))])
      # remove the model label from all column names (unnecessary distinction with only one model)
      names(astm[[model]]) <- sub(
         pattern = paste0("^", model, "."),
         replacement = "",
         x = names(astm[[model]]))
      return(astm[[model]])
   }
}
