## ----packages, echo=FALSE, message=FALSE------------------------------------------
library(dplyr)
library(tidyr)
# to use variables in dplyr::rename
# https://stackoverflow.com/a/67644830/1198249
library(glue)
library(stringr)  # str_remove
library(tibble)   # add_column
library(knitr)
library(ggplot2)
library(latex2exp)
library(cowplot)
library(ggrepel)
library(gt)
library(xtable)
library(here)
library(common)

## ----global_options, echo=FALSE, message=FALSE------------------------------------
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

## ----global_settings--------------------------------------------------------------
spectral.min <- 1E-7 # meter
spectral.max <- 4E-6 # meter
spectral.stepsize <- 1E-9 # meter
vis.min <- 380e-9 # m
vis.max <- 780e-9 # m

## ----echo=FALSE-------------------------------------------------------------------
sunlight.Planck <- function(
   wavelength = seq(1E-7, 4E-6, 1E-9), # 100 nm - 4000 nm, stepsize 1 nm
   temperature = dplyr::filter(photoec::solarconstants, label == "T.Sun")$value) {

   stopifnot(
      # stop if wavelength contains any values <= 0
      all(wavelength > 0),
      # stop if temperature is T <= 0
      temperature > 0)


   ##### Local variables
   c0 <- dplyr::filter(photoec::solarconstants, label=="c")$value
   planck <- dplyr::filter(photoec::solarconstants, label=="h")$value
   boltzmann <- dplyr::filter(photoec::solarconstants, label=="k")$value
   r.Sun <- dplyr::filter(photoec::solarconstants, label=="R.Sun")$value
   r.AU <- dplyr::filter(photoec::solarconstants, label=="R.AU")$value
   area.Sun <- dplyr::filter(photoec::solarconstants, label=="A.Sun")$value
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
solarconstants <- tribble(
   ~name,                       ~label,    ~value,                 ~reference,        ~label.html,                ~label.tex,                      ~unit.html,  ~unit.tex,
   "Speed of light",            "c",       constants::syms$c0,     "per definition",  "<i>c</i>",                 "\\ensuremath{c}",               "m s⁻¹",     "\\si{\\metre\\per\\second}",
   "Elementary charge",         "e",       constants::syms$e,      "CODATA 2018",     "<i>e</i>",                 "\\ensuremath{e}",               "C",         "\\si{\\coulomb}",
   "Planck's constant",         "h",       constants::syms$h,      "CODATA 2018",     "<i>h</i>",                 "\\ensuremath{h}",               "J s⁻¹",     "\\si{\\joule\\second}",
   "Planck's constant",         "h.eV",    constants::syms$hev,    "CODATA 2018",     "<i>h</i>",                 "\\ensuremath{h}",               "eV s⁻¹",    "\\si{\\electronvolt\\second}",
   "Boltzmann's constant",      "k",       constants::syms$k,      "CODATA 2018",     "<i>k</i>",                 "\\ensuremath{k}",               "J K⁻¹",     "\\si{\\joule\\per\\kelvin}",
   "Stefan-Boltzmann constant", "sigma",   constants::syms$sigma0, "CODATA 2018",     "σ",                        "\\ensuremath{\\sigma}",         "W m⁻² K⁻⁴", "\\si{\\watt\\per\\square\\metre\\per\\kelvin\\tothe{4}}",
   "Temperature of the Sun",    "T.Sun",   5772,                   "NASA, Wikipedia", "<i>T</i><sub>Sun</sub>",   "\\ensuremath{T_\\text{Sun}}",   "K",         "\\si{\\kelvin}",
   "Astronomical unit",         "R.AU",    149597870700,           "per definition",  "<i>R</i><sub>AU</sub>",    "\\ensuremath{R_\\text{AU}}",    "m",         "\\si{\\metre}",
   "Radius of the Sun",         "R.Sun",   695700000,              "Wikipedia",       "<i>R</i><sub>Sun</sub>",   "\\ensuremath{R_\\text{Sun}}",   "m",         "\\si{\\metre}",
   "Surface area of the Sun",   "A.Sun",   NA,                     "A=4×π×R²",        "<i>A</i><sub>Sun</sub>",   "\\ensuremath{A_\\text{Sun}}",   "m²",        "\\si{\\square\\metre}",
   "Radius of the Earth",       "R.Earth", 6371008.7714,           "IUGG, Wikipedia", "<i>R</i><sub>Earth</sub>", "\\ensuremath{R_\\text{Earth}}", "m",         "\\si{\\metre}",
   "Surface area of the Earth", "A.Earth", NA,                     "A=4×π×R²",        "<i>A</i><sub>Earth</sub>", "\\ensuremath{A_\\text{Earth}}", "m²",        "\\si{\\square\\metre}")
solarconstants <-
   solarconstants %>%
   # surface area of the Sun
   mutate(value =
      replace(
         value,
         label == "A.Sun",
         4 * pi * (solarconstants %>% filter(label == "R.Sun") %>% pull(value))^2)) %>%
   # surface area of the Earth
   mutate(value =
      replace(
         value,
         label == "A.Earth",
         4 * pi * (solarconstants %>% filter(label == "R.Earth") %>% pull(value))^2))

## ----echo=FALSE, results="asis"---------------------------------------------------
xtab.solarconstants <-
   solarconstants |>
   select(name, label.html, value, unit.html, reference) |>
   xtable::xtable()
names(xtab.solarconstants) <- c("Name", "Symbol", "Value", "Unit", "Reference")
# remember, digits() display() align() are length + 1 (include row.names)
digits(xtab.solarconstants) <- c(0, 0, 0, 4, 0, 0)
display(xtab.solarconstants) <- c("s", "s", "s", "e", "s", "s")
align(xtab.solarconstants) <- c("l", "l", "c", "r", "l", "l")
print(
   xtab.solarconstants,
   include.rownames = FALSE,
   # disabling sanitization is important to not mangle HTML markup inside the dataframe
   sanitize.text.function = function(x){x},
   type = "html")

## ----blackbody-different-temperatures---------------------------------------------
surface.temperature <- c(
   # https://en.wikipedia.org/wiki/Gamma_Cassiopeiae # 25000, # K
   # https://en.wikipedia.org/wiki/Sirius            # 9940, # K
   # https://en.wikipedia.org/wiki/Vega              # 9602, # K
   # https://en.wikipedia.org/wiki/Fomalhaut         # 8590, # K
   # https://en.wikipedia.org/wiki/Deneb
   8525,
   # Sun
   filter(solarconstants, label=="T.Sun")$value,
   # https://en.wikipedia.org/wiki/Beta_Andromedae   # 3842, # K
   # https://en.wikipedia.org/wiki/Betelgeuse
   3600)
bblong <- NULL
for (i in 1:length(surface.temperature)) {
   bblong <- bind_rows(
      bblong,
      sunlight.Planck(
         wavelength = seq(spectral.min, spectral.max, spectral.stepsize),
         temperature = surface.temperature[i]) %>%
      # convert spectral radiance/irradiance columns from W m⁻² m⁻¹ to W m⁻² nm⁻¹
      mutate(Sun.spectralradiance = 1e-9 * Sun.spectralradiance) %>%
      mutate(Earth.spectralirradiance = 1e-9 * Earth.spectralirradiance) %>%
      add_column(energy = photoec::wavelength2energy(.$wavelength), .after = "wavelength") %>%
      add_column(temperature = surface.temperature[i], .after = "energy") %>%
      pivot_longer(
         cols = !starts_with(c("wavelength", "energy", "temperature")),
         names_to = "property",
         values_to = "value"))
}

## ----radiance-total-and-in-the-visible-range--------------------------------------
# you want to be careful when extending the wavelength range towards larger values that
# you don't use too large step sizes in the lower ranges, because that will mess up
# the cumulative radiance values (due to trapezoidal integration)
# NOTE that adding the two higher-wavelength ranges makes no discernible difference
# to the calculated totals or visible range fractions
wl.extended <- c(seq(1E-9, 2E-5, 1E-9), seq(3E-05, 1, 1E-4), seq(2, 1E6, 1E2))
radiances <- NULL
for (i in 1:length(surface.temperature)) {
   radiances <- bind_rows(
      radiances,
      sunlight.Planck(
         wavelength = wl.extended,
         temperature = surface.temperature[i]) %>%
      add_column(temperature = surface.temperature[i], .after = "wavelength") %>%
      select(wavelength, temperature, Sun.radiance) %>%
      rename(total = Sun.radiance) %>%
      # find the total radiance (last value in vector, corresponds to largest wavelength)
      group_by(temperature) %>%
      slice_tail(n = 1)
   )
}
# to avoid stupid errors due to temperatures possibly ordered differently, use join
radiances <-
   left_join(
      radiances,
      bblong %>%
         group_by(temperature) %>%
         filter(property == "Sun.radiance") %>%
         # careful, this equals sign only works reliably if stepsize 1 nm
         filter(wavelength == vis.min) %>%
         # slice unnecessary if preceding comparison uses ==, but needed if we use > or <
         slice_head(n = 1) %>%
         select(temperature, value) %>%
         # dynamically construct new column name using glue notation
         # https://stackoverflow.com/a/67644830/1198249
         rename("at{vis.min}" := value),
      by = "temperature") %>%
   left_join(
      bblong %>%
         group_by(temperature) %>%
         filter(property == "Sun.radiance") %>%
         filter(wavelength == vis.max) %>%
         slice_head(n = 1) %>%
         select(temperature, value) %>%
         rename("at{vis.max}" := value),
      by = "temperature")
# radiance in the visible range
# note the use of drop=TRUE to force return of vector and not tibble, which is otherwise the default
# https://tibble.tidyverse.org/reference/subsetting.html
radiances$visible <- radiances[, paste0("at", vis.max), drop=T] - radiances[, paste0("at", vis.min), drop=T]
# fraction of radiance in the visible range compared to total radiance
radiances$fraction <- radiances$visible / radiances$total

## ----blackbody-radiance, echo=FALSE-----------------------------------------------
this.labeller <- setNames(
   # note that knitr device=svg throws lot of warnings when we used unicode
   # char (no-break space) instead of regular space character here
   # https://github.com/yihui/knitr/issues/496
   paste0(unique(bblong$temperature), " K"),
   unique(bblong$temperature))
p.bb.spradiance <- ggplot(bblong %>% filter(property == "Sun.spectralradiance")) +
   facet_wrap(
      ~temperature, ncol = 1, scales = "free_y",
      labeller = as_labeller(this.labeller)) +
   # highlight visible spectrum range as area
   # would be really cool if we could colour this area as a rainbow...
   geom_ribbon(
      data = bblong %>%
         filter(property == "Sun.spectralradiance") %>%
         filter(wavelength >= vis.min & wavelength <= vis.max),
      fill = alpha("orange", 0.35),
      colour = NA,
      ymin = 0,
      aes(x = wavelength, ymax = value)) +
   geom_path(
      aes(
         x = wavelength,
         y = value)) +
   geom_text(
      data = radiances,
      x = mean(range(vis.min, vis.max)),
      # Y=Inf in combination with hjust=1 (note the angle) aligns the right-edge of the text
      # with top of plot area. hjust > 1 pushes the text downwards (also affected by textsize).
      y = Inf, hjust = 2.65, size = 3.25,
      # note that due to angle=90 here vjust affects horizontal positioning
      vjust = 0.5, angle = 90,
      colour = "#b36200", # dark orange
      aes(
         label = paste0(formatC(100 * fraction, digits=2), "%"),
         group = temperature)) +
   geom_label_repel(
      data = . %>% group_by(temperature) %>% filter(max(value) == value),
      size = 3.0, nudge_x = 1e-6,
      point.padding = unit(0.75, "lines"),
      arrow = arrow(length = unit(0.05, "npc")),
      aes(
         label = paste0(numbers2prefix(wavelength), "m"),
         x = wavelength,
         y = value)) +
   scale_x_continuous(
      name = "Wavelength / nm",
      labels = rlang::as_function(~ 1e9 * .)) +
   scale_y_continuous(
      name = "Spectral radiance / kW m⁻² nm⁻¹",
      labels = rlang::as_function(~ 1e-3 * .)) +
   theme(
      legend.position = c(1, 1),
      legend.justification = c(1, 1))
p.bb.radiance <- ggplot(bblong %>% filter(property == "Sun.radiance")) +
   facet_wrap(
      ~temperature, ncol = 1, scales = "free_y",
      labeller = as_labeller(this.labeller)) +
   geom_ribbon(
      data = bblong %>%
         filter(property == "Sun.radiance") %>%
         filter(wavelength >= vis.min & wavelength <= vis.max),
      fill = alpha("orange", 0.35),
      colour = NA,
      ymin = 0,
      aes(x = wavelength, ymax = value)) +
   geom_path(
      aes(
         x = wavelength,
         y = value)) +
   geom_text(
      data = radiances,
      x = Inf, y = Inf,
      vjust = 2.25, hjust = 1.2, size = 3.25,
      aes(
         label = paste0(formatC(1E-6 * total, format="f", digits = 1), " MW m⁻²"),
         group = temperature)) +
   scale_x_continuous(
      name = "Wavelength / nm",
      labels = rlang::as_function(~ 1e9 * .)) +
   scale_y_continuous(
      name = "Radiance / MW m⁻²",
      labels = rlang::as_function(~ 1e-6 * .)) +
   theme(legend.position = "none")
title.cow <- ggdraw() +
   draw_label(
      "Black body spectra at three different temperatures",
      x = 0, hjust = 0, size = 14) +
   # manually adjusted title left-side margin to align with left plot panel
   theme(plot.margin = margin(0, 0, 0, 40))
subtitle.cow <- ggdraw() +
   draw_label(
      "Exemplified with Betelgeuse, the Sun, and Deneb. Visible range demarcated in yellow.",
      x = 0, hjust = 0, y = 1, size = 10) +
   # manually adjusted title left-side margin to align with left plot panel
   theme(plot.margin = margin(0, 0, 0, 40))
bb.row <- cowplot::plot_grid(p.bb.spradiance, p.bb.radiance, nrow = 1, rel_widths = c(1, 1))
cowplot::plot_grid(title.cow, subtitle.cow, bb.row, ncol = 1, rel_heights = c(0.1, 0.04, 1))

## ----Sun-blackbody-df-long, results="markup"--------------------------------------
dflong <- sunlight.Planck(wavelength = wl.extended) %>%
   # convert spectral radiance/irradiance columns from W m⁻² m⁻¹ to W m⁻² nm⁻¹
   mutate(Sun.spectralradiance = 1e-9 * Sun.spectralradiance) %>%
   mutate(Earth.spectralirradiance = 1e-9 * Earth.spectralirradiance) %>%
   # the use of .$ notation was necessary here (because inside function call, I guess)
   add_column(energy = photoec::wavelength2energy(.$wavelength), .after = "wavelength") %>%
   # transform dataframe to long format (better suited for tidy and gpplot2)
   pivot_longer(
      cols = !starts_with(c("wavelength", "energy")),
      names_to = "property",
      values_to = "value")
dflong %>% glimpse()

## ----Sun-blackbody-spectrum-radiance, echo=FALSE----------------------------------
# using facetting is not suitable because we have different y-axis for every plot...
p.sun.spradiance <-
   ggplot(dflong %>%
             filter(property == "Sun.spectralradiance") %>%
             filter(wavelength >= spectral.min & wavelength <= spectral.max)) +
   geom_line(
      aes(
         x = wavelength,
         y = value)) +
   scale_x_continuous(
      name = "Wavelength / nm",
      labels = rlang::as_function(~ 1e9 * .)) +
   scale_y_continuous(
      name = "Sp. radiance / kW m⁻² nm⁻¹",
      labels = rlang::as_function(~ 1e-3 * .)) +
   theme(legend.position = "none")
p.sun.radiance <-
   ggplot(dflong %>%
             filter(property == "Sun.radiance") %>%
             filter(wavelength >= spectral.min & wavelength <= spectral.max)) +
   geom_line(
      aes(
         x = wavelength,
         y = value)) +
   geom_text(
      # cannot use . here, must be explicit, otherwise we get the data truncated
      # at vis.max whereas we want the radiance for the extended spectral range
      data = dflong %>%
         filter(property == "Sun.radiance") %>%
         slice_tail(n = 1),
      x = Inf, y = Inf,
      vjust = 2.50, hjust = 1.2, size = 3.25,
      aes(label = paste0(formatC(1E-6 * value, format = "f", digits = 1), " MW m⁻²"))) +
   scale_x_continuous(
      name = "Wavelength / nm",
      labels = rlang::as_function(~ 1e9 * .)) +
   scale_y_continuous(
      name = "Radiance / MW m⁻²",
      labels = rlang::as_function(~ 1e-6 * .)) +
   theme(legend.position = "none")
p.sun.luminosity <-
   ggplot(dflong %>%
             filter(property == "Sun.luminosity") %>%
             filter(wavelength >= spectral.min & wavelength <= spectral.max)) +
   geom_line(aes(x = wavelength, y = value)) +
   scale_x_continuous(
      name = "Wavelength / nm",
      labels = rlang::as_function(~ 1e9 * .)) +
   scale_y_continuous(name = "Luminosity/W") +
   theme(legend.position = "none")
##
p.earth.spirr <-
   ggplot(dflong %>%
             filter(property == "Earth.spectralirradiance") %>%
             filter(wavelength >= spectral.min & wavelength <= spectral.max)) +
   geom_line(
      aes(
         x = wavelength,
         y = value)) +
   scale_x_continuous(
      name = "Wavelength / nm",
      labels = rlang::as_function(~ 1e9 * .)) +
   scale_y_continuous(name = "Sp. irradiance / W m⁻² nm⁻¹") +
   theme(legend.position = "none")
p.earth.irr <-
   ggplot(dflong %>%
             filter(property == "Earth.irradiance") %>%
             filter(wavelength >= spectral.min & wavelength <= spectral.max)) +
   geom_line(
      aes(
         x = wavelength,
         y = value)) +
   geom_text(
      # cannot use . here, must be explicit, otherwise we get the data truncated
      # at vis.max whereas we want the irradiance for the extended spectral range
      data = dflong %>%
         filter(property == "Earth.irradiance") %>%
         slice_tail(n = 1),
      x = Inf, y = Inf,
      vjust = 2.50, hjust = 1.2, size = 3.25,
      aes(label = paste0(formatC(value, format = "f", digits = 1), " W m⁻²"))) +
   scale_x_continuous(
      name = "Wavelength / nm",
      labels = rlang::as_function(~ 1e9 * .)) +
   scale_y_continuous(
      name = "Irradiance / kW m⁻²",
      labels = rlang::as_function(~ 1e-3 * .)) +
   theme(legend.position = "none")
p.earth.luminosity <-
   ggplot(dflong %>%
             filter(property == "Earth.luminosity") %>%
             filter(wavelength >= spectral.min & wavelength <= spectral.max)) +
   geom_line(aes(x = wavelength, y = value)) +
   scale_x_continuous(
      name = "Wavelength / nm",
      labels = rlang::as_function(~ 1e9 * .)) +
   scale_y_continuous(name = "Luminosity/W") +
   theme(legend.position = "none")
# https://datavizpyr.com/join-multiple-plots-with-cowplot/
title.cow <- ggdraw() +
   draw_label(
      paste0(
         "Spectrum of the Sun modelled as a black body (",
         paste0(1e9 * spectral.min, " nm"),
         # en dash
         "–",
         paste0(1e6 * spectral.max, " µm"),
         ")"),
      x = 0, hjust = 0, size = 14) +
   # manually adjusted title left-side margin to align with left plot panel
   theme(plot.margin = margin(0, 0, 0, 45))
title.sun <- ggdraw() + draw_label("Solar radiance", angle=-90, x=0.5, y=0, hjust=1.65, vjust=0.5, size=12)
# hjust adjust up/down position of right-side labels and was adjusted manually
title.earth <- ggdraw() + draw_label("Irradiance on Earth", angle=-90, x=0.5, y=0, hjust=1.37, vjust=0.5, size=12)
row.sun <- cowplot::plot_grid(p.sun.spradiance, p.sun.radiance, title.sun, nrow = 1, rel_widths = c(1, 1, 0.1), labels = c("a", "b"), label_x = 0.16, label_y = 0.97)
row.earth <- cowplot::plot_grid(p.earth.spirr, p.earth.irr, title.earth, nrow = 1, rel_widths = c(1, 1, 0.1), labels = c("c", "d"), label_x = 0.16, label_y = 0.97)
cowplot::plot_grid(title.cow, row.sun, row.earth, ncol = 1, rel_heights = c(0.1, 1, 1))

## ----results="markup"-------------------------------------------------------------
dflong %>% filter(property == "Sun.spectralradiance.powerterm") %>% head()
dflong %>% filter(property == "Sun.spectralradiance.powerterm") %>% tail()

## ----results="markup"-------------------------------------------------------------
dflong %>% filter(property == "Sun.spectralradiance.expterm") %>% head()
dflong %>% filter(property == "Sun.spectralradiance.expterm") %>% tail()

## ----two-terms-Plancks-law, eval=FALSE, echo=FALSE--------------------------------
#  # 3.74e19 W m⁻² nm⁻¹ at 1 nm, drops to 3.654e11 at 4000 nm (8 orders of magnitude)
#  p <- ggplot(data = bblong %>%
#            filter(property == "Sun.spectralradiance.powerterm") %>%
#            filter(temperature == filter(solarconstants, label == "T.Sun")$value)) +
#     coord_cartesian(y = c(0, 1e13)) +
#     geom_line(
#        aes(
#           x = wavelength,
#           y = value)) +
#     scale_x_continuous(
#        name = "Wavelength / nm",
#        labels = rlang::as_function(~ 1e9 * .)) +
#     scale_y_continuous(
#        name = "Spectral radiance / W m⁻² nm⁻¹") +
#     #    labels = rlang::as_function(~ 1e-6 * .)) +
#     theme(legend.position = "none")
#  # 1.49e-11 W m⁻² nm⁻¹ at 1 nm, rises to 1.16 at 4000 nm (11 orders of magnitude)
#  ggplot(data = bblong %>%
#            filter(property == "Sun.spectralradiance.expterm") %>%
#            filter(temperature == filter(solarconstants, label == "T.Sun")$value)) +
#     # coord_cartesian(y = c(0, 1e13)) +
#     geom_line(
#        aes(
#           x = wavelength,
#           y = value)) +
#     scale_x_continuous(
#        name = "Wavelength / nm",
#        labels = rlang::as_function(~ 1e9 * .)) +
#     scale_y_continuous(
#        name = "Spectral radiance / W m⁻² nm⁻¹",
#        sec.axis = dup_axis()) +
#     #    labels = rlang::as_function(~ 1e-6 * .)) +
#     theme(legend.position = "none")

