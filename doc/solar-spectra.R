## ----packages, echo=T, message=FALSE----------------------------------------------
library(dplyr)
library(tidyr)
library(stringr)  # str_remove
library(knitr)
library(ggplot2)
library(latex2exp)
library(cowplot)
library(here)

## ----global_options, echo=T, message=FALSE----------------------------------------
options(
   digits   = 7,
   width    = 84,
   continue = " ",
   prompt   = "> ",
   warn = 0,
   stringsAsFactors = FALSE)
opts_chunk$set(
   dev        = "svg",
   fig.align  = "center",
   fig.width  = 7.10,
   # 7.10/4.39=1.617 golden ratio
   # but we increase height slightly to make allowance for title,
   # secondary axis, caption, etc.
   fig.height = 4.58,
   echo       = TRUE,
   eval       = TRUE,
   cache      = FALSE,
   collapse   = TRUE,
   results    = 'hide',
   message    = FALSE,
   warning    = FALSE,
   tidy       = FALSE)

## ----echo=FALSE-------------------------------------------------------------------
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

## ----dataset, echo=TRUE, results="markup"-----------------------------------------
dataset <- photoec::ASTMG173
dataset %>% glimpse()

## ----transform-data, echo=TRUE, results="markup"----------------------------------
# while we are at it, let's give the existing columns more descriptive names
dataset <- dataset %>%
   rename(AM0 = extraterrestrial) %>%
   rename(AM1.5G = globaltilt) %>%
   # direct normal circumsolar
   rename(DNCS = direct.circumsolar)
datalong <- dataset %>% pivot_longer(
   cols = !starts_with("wavelength"),
   names_to = "model",
   values_to = "value") %>%
   mutate(property = "spectral.irradiance")
datalong %>% glimpse()

## ----astmg173-dataset, echo=FALSE-------------------------------------------------
ggplot(datalong) +
   geom_line(linewidth = 0.2,
      aes(
         x = wavelength,
         y = value,
         colour = model)) +
   scale_x_continuous(
      name = "Wavelength / nm",
      breaks = c(250, 1000, 2000, 3000, 4000),
      sec.axis = sec_axis(
         ~ 1239.842 / .,
         name = "Energy / eV",
         breaks = c(4, 2, 1, 0.5))) +
   scale_y_continuous(name = "Spectral irradiance / (W/m²/nm)") +
   labs(
      title = "ASTM G173-03 reference solar spectra",
      # note that <code></code> (i.e., ``) is not supported yet by ggtext
      # and apparently \texttt{} is not supported by latex2exp::TeX()
      caption = latex2exp::TeX(r"(Data source: \textbf{photoec::ASTMG173})")) +
   theme(
      legend.position = c(0.98, 0.98),
      legend.justification = c(1, 1))

## ----astmg173-function, echo=FALSE------------------------------------------------
dflong <- sunlight.ASTM() %>%
   pivot_longer(
      cols = !starts_with(c("wavelength", "energy")),
      names_to = "astm.colname",
      values_to = "value") %>%
   # replace everything *after* AM0/AM1.5G/DNCS (starting with the dot) with empty string
   # this way we extract the "model" out of the former column names now populating
   # the "astm.colname" column
   mutate(model = sub("\\.[a-z]+(\\.[a-z]+)?", "", astm.colname)) %>%
   mutate(property = sub("^\\.", "", str_remove(astm.colname, model))) %>%
   # this leaves spectral irradiance values with empty string in "property" column
   mutate(property = ifelse(property == "", "spectral.irradiance", property)) %>%
   # with that we have successfully parsed the original column names and no longer
   # need the astm.colname column
   select(-astm.colname)
dflong %>% glimpse()

## ----astm-function, echo=FALSE----------------------------------------------------
ggplot(dflong %>% filter(property == "spectral.irradiance")) +
   geom_line(linewidth = 0.2,
      aes(
         x = wavelength,
         y = value,
         colour = model)) +
   scale_x_continuous(
      name = "Wavelength / nm",
      breaks = c(250, 1000, 2000, 3000, 4000),
      sec.axis = sec_axis(
         ~ 1239.842 / .,
         name = "Energy / eV",
         breaks = c(4, 2, 1, 0.5))) +
   scale_y_continuous(name = "Spectral irradiance / (W/m²/nm)") +
   labs(
      title = "Reference solar spectra by photoec::sunlight.ASTM()",
      # note that <code></code> (i.e., ``) is not supported yet by ggtext
      # and apparently \texttt{} is not supported by latex2exp::TeX()
      caption = latex2exp::TeX(r"(Data source: \textbf{photoec::sunlight.ASTM()})")) +
   theme(
      legend.position = c(0.98, 0.98),
      legend.justification = c(1, 1))

## ----diff-dataset-vs-function, echo=TRUE, results="markup"------------------------
identical(
   datalong %>% filter(model == "AM1.5G") %>% filter(property == "spectral.irradiance") %>% pull(value),
   dflong %>% filter(model == "AM1.5G") %>% filter(property == "spectral.irradiance") %>% pull(value))

## ----function-exploration, echo=FALSE---------------------------------------------
p.irr <- ggplot(dflong %>% filter(property == "irradiance")) +
   geom_line(
      aes(
         x = wavelength,
         y = value,
         colour = model)) +
   geom_text(
      data = dflong %>%
         filter(property == "irradiance") %>%
         select(model, wavelength, value) %>%
         group_by(model) %>%
         # get the final irradiance value for each model
         # which is total irradiance since irradiance is the cumulative spectral irradiance
         # https://stackoverflow.com/a/53994503/1198249
         slice(tail(row_number(), 1)),
      hjust = 1.0, vjust = 1.4,
      aes(
         x = wavelength,
         y = value,
         colour = model,
         label = paste0(
            formatC(value, digits = 5, format = "fg"),
            " W/m²"))) +
   scale_x_continuous(
      name = "Wavelength / nm",
      breaks = c(250, 1000, 2000, 3000, 4000),
      sec.axis = sec_axis(
         ~ 1239.842 / .,
         name = "Energy / eV",
         breaks = c(4, 2, 1, 0.5))) +
   scale_y_continuous(name = "Irradiance / (W/m²)") +
   theme(legend.position = "none")
p.frac <- ggplot(dflong %>% filter(property == "irradiance.fraction")) +
   geom_line(aes(x = wavelength, y = value, colour = model)) +
   scale_x_continuous(
      name = "Wavelength / nm",
      breaks = c(250, 1000, 2000, 3000, 4000),
      sec.axis = sec_axis(
         ~ 1239.842 / .,
         name = "Energy / eV",
         breaks = c(4, 2, 1, 0.5))) +
   scale_y_continuous(name = "Irradiance fraction") +
   theme(
      legend.position = c(0.98, 0.02),
      legend.justification = c(1, 0))
# https://datavizpyr.com/join-multiple-plots-with-cowplot/
cowtitle <- ggdraw() +
   draw_label(
      "Cumulative irradiance in (a) absolute and (b) relative terms",
      x = 0, hjust = 0, size = 14) +
   # manually adjusted title left-side margin to align with left plot panel
   theme(plot.margin = margin(0, 0, 0, 45))
cowrow <- plot_grid(p.irr, p.frac, nrow = 1, labels = c("a", "b"))
cowplot::plot_grid(cowtitle, cowrow, ncol = 1, rel_heights = c(0.1, 1))

