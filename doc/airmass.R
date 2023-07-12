## ----packages, echo=FALSE, message=FALSE--------------------------------------
library(dplyr)
library(tidyr)
library(knitr)
library(ggplot2)
library(here)

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

## ----echo=F-----------------------------------------------------------------------
astrorefraction <- function(z = 48.2) {
   # here we use the constants as calculated by Woolard & Clemence, 1966 (chapter 5, page 84)
   A <- common::asec2degrees(60.29)
   B <- common::asec2degrees(0.06688)
   R <- A * tan(common::as.radians(z)) - B * tan(common::as.radians(z))^3
   return(R)
}

## ----airmass-secant-vs-Bemporad---------------------------------------------------
z <- seq(0, 90, 1)
amdf <- data.frame(
   zenith = z,
   model = "PPLM",
   AM = 1 / cos(common::as.radians(z))
)
amdf <- bind_rows(
   amdf,
   data.frame(
     zenith = z,
      model = "Bemporad",
      AM = astrorefraction(z) / (common::asec2degrees(58.36) * sin(common::as.radians(z)))
   )
)

## ----echo=FALSE, warning=FALSE----------------------------------------------------
ggplot(amdf) +
   coord_cartesian(y = c(0, 88)) +
   geom_line(aes(x = zenith, y = AM, colour = model)) +
   scale_x_continuous(
      name = "Angle from zenith, z/°",
      breaks = seq(0, 90, 10)) +
   theme(
      legend.position = c(0.02, 0.98),
      legend.justification = c(0, 1))

## ----sketch, results="markup", out.width="33%", echo=FALSE------------------------
knitr::include_graphics(here("man/figures/airmass-geometry.png"))

