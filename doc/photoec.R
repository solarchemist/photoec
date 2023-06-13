## ----packages, echo=T, message=FALSE------------------------------------------
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
   ~name,                       ~label,     ~value,       ~reference,           ~label.tex,                      ~unit.tex,
   "Speed of light",            "c",        syms$c0,      "per definition",     "\\ensuremath{c}",               "\\si{\\metre\\per\\second}",
   "Elementary charge",         "e",        syms$e,       "CODATA 2018",        "\\ensuremath{e}",               "\\si{\\coulomb}",
   "Planck's constant",         "h",        syms$h,       "CODATA 2018",        "\\ensuremath{h}",               "\\si{\\joule\\second}",
   "Planck's constant",         "h.eV",     syms$hev,     "CODATA 2018",        "\\ensuremath{h_\\text{eV}}",    "\\si{\\electronvolt\\second}",
   "Boltzmann's constant",      "k",        syms$k,       "CODATA 2018",        "\\ensuremath{k}",               "\\si{\\joule\\per\\kelvin}",
   "Stefan-Boltzmann constant", "sigma",    syms$sigma0,  "CODATA 2018",        "\\ensuremath{\\sigma}",         "\\si{\\watt\\per\\square\\metre\\per\\kelvin\\tothe{4}}",
   "Astronomical unit",         "R.AU",     149597870700, "per definition",     "\\ensuremath{R_\\text{AU}}",    "\\si{\\metre}",
   "Radius of the Sun",         "R.Sun",    695700000,    "Wikipedia",          "\\ensuremath{R_\\text{Sun}}",   "\\si{\\metre}",
   "Surface area of the Sun",   "A.Sun",    NA,           "A = 4*pi*R.Sun^2",   "\\ensuremath{A_\\text{Sun}}",   "\\si{\\square\\metre}",
   "Radius of the Earth",       "R.Earth",  6371008.7714, "IUGG, Wikipedia",    "\\ensuremath{R_\\text{Earth}}", "\\si{\\metre}",
   "Surface area of the Earth", "A.Earth",  NA,           "A = 4*pi*R.Earth^2", "\\ensuremath{A_\\text{Earth}}", "\\si{\\square\\metre}")
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

## ----ASTMG173---------------------------------------------------------------------
# the CSV file we downloaded from NREL is saved in inst/extdata (it's not modified by this script)
ASTMG173 <- read.csv(
   file = here("inst/extdata", "ASTMG173.csv"),
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
write.csv(solarconstants, file = here("data", "solarconstants.csv"), row.names = FALSE)
write.csv(ASTMG173, file = here("data", "ASTMG173.csv"), row.names = FALSE)

