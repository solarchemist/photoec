## ----packages, echo=T, message=FALSE----------------------------------------------
library(dplyr)
library(knitr)
library(usethis)   # use_data()
library(tibble)    # tribble()
library(constants) # syms
library(here)      # here()

## ----global_options, echo=T, message=FALSE----------------------------------------
options(
   digits   = 7,
   width    = 84,
   continue = " ",
   prompt   = "> ",
   warn = 0,
   stringsAsFactors = FALSE)
opts_chunk$set(
   echo       = TRUE,
   eval       = TRUE,
   cache      = TRUE,
   collapse   = TRUE,
   results    = 'hide',
   message    = FALSE,
   warning    = FALSE,
   tidy       = FALSE)

## ----solarconstants---------------------------------------------------------------
solarconstants <- tribble(
   ~name,                       ~label,     ~value,       ~reference,        ~label.html,                ~label.tex,                      ~unit.html,  ~unit.tex,
   "Speed of light",            "c",        syms$c0,      "per definition",  "<i>c</i>",                 "\\ensuremath{c}",               "m s⁻¹",     "\\si{\\metre\\per\\second}",
   "Elementary charge",         "e",        syms$e,       "CODATA 2018",     "<i>e</i>",                 "\\ensuremath{e}",               "C",         "\\si{\\coulomb}",
   "Planck's constant",         "h",        syms$h,       "CODATA 2018",     "<i>h</i>",                 "\\ensuremath{h}",               "J s⁻¹",     "\\si{\\joule\\second}",
   "Planck's constant",         "h.eV",     syms$hev,     "CODATA 2018",     "<i>h</i>",                 "\\ensuremath{h}",               "eV s⁻¹",    "\\si{\\electronvolt\\second}",
   "Boltzmann's constant",      "k",        syms$k,       "CODATA 2018",     "<i>k</i>",                 "\\ensuremath{k}",               "J K⁻¹",     "\\si{\\joule\\per\\kelvin}",
   "Stefan-Boltzmann constant", "sigma",    syms$sigma0,  "CODATA 2018",     "σ",                        "\\ensuremath{\\sigma}",         "W m⁻² K⁻⁴", "\\si{\\watt\\per\\square\\metre\\per\\kelvin\\tothe{4}}",
   "Temperature of the Sun",    "T.Sun",    5772,         "NASA, Wikipedia", "<i>T</i><sub>Sun</sub>",   "\\ensuremath{T_\\text{Sun}}",   "K",         "\\si{\\kelvin}",
   "Astronomical unit",         "R.AU",     149597870700, "per definition",  "<i>R</i><sub>AU</sub>",    "\\ensuremath{R_\\text{AU}}",    "m",         "\\si{\\metre}",
   "Radius of the Sun",         "R.Sun",    695700000,    "Wikipedia",       "<i>R</i><sub>Sun</sub>",   "\\ensuremath{R_\\text{Sun}}",   "m",         "\\si{\\metre}",
   "Surface area of the Sun",   "A.Sun",    NA,           "A=4×π×R²",        "<i>A</i><sub>Sun</sub>",   "\\ensuremath{A_\\text{Sun}}",   "m²",        "\\si{\\square\\metre}",
   "Radius of the Earth",       "R.Earth",  6371008.7714, "IUGG, Wikipedia", "<i>R</i><sub>Earth</sub>", "\\ensuremath{R_\\text{Earth}}", "m",         "\\si{\\metre}",
   "Surface area of the Earth", "A.Earth",  NA,           "A=4×π×R²",        "<i>A</i><sub>Earth</sub>", "\\ensuremath{A_\\text{Earth}}", "m²",        "\\si{\\square\\metre}")
solarconstants <-
   solarconstants %>%
   # surface area of the Sun
   mutate(value =
      replace(
         value,
         label == "A.Sun",
         4 * pi * filter(solarconstants, label == "R.Sun")$value^2)) %>%
   # surface area of the Earth
   mutate(value =
      replace(
         value,
         label == "A.Earth",
         4 * pi * filter(solarconstants, label == "R.Earth")$value^2))

## ----ASTMG173---------------------------------------------------------------------
# the CSV file we downloaded from NREL is saved in inst/extdata (it's not modified by this script)
ASTMG173 <- read.csv(
   file = here::here("inst/extdata", "ASTMG173.csv"),
   skip = 1,
   col.names = c(
      "wavelength",          # nm
      "extraterrestrial",    # W m⁻² nm⁻¹
      "globaltilt",          # W m⁻² nm⁻¹
      "direct.circumsolar")) # W m⁻² nm⁻¹

## ---- echo=T----------------------------------------------------------------------
# use_data() saves each object as rda file in ./data/
usethis::use_data(solarconstants, ASTMG173, overwrite = TRUE)
# also save the dataset as csv files, for ease of reading
write.csv(solarconstants, file = here::here("data", "solarconstants.csv"), row.names = FALSE)
write.csv(ASTMG173, file = here::here("data", "ASTMG173.csv"), row.names = FALSE)

