solar.constants <- function() {

   solar.constants <- 
      structure(list(name = c("Speed of light", 
                              "Elementary charge", 
                              "Planck's constant", 
                              "Planck's constant", 
                              "Boltzmann's constant", 
                              "Stefan-Boltzmann constant", 
                              "Astronomical unit", 
                              "Radius of the Sun", 
                              "Surface area of the Sun", 
                              "Radius of the Earth",
                              "Surface area of the Earth"), 
                     symbol = c("$c$", 
                                "$e$", 
                                "$h$", 
                                "$h_\\text{eV}$", 
                                "$k$", 
                                "$\\sigma$", 
                                "$R_\\text{AU}$", 
                                "$R_\\text{Sun}$", 
                                "$A_\\text{Sun}$", 
                                "$R_\\text{Earth}$",
                                "$A_\\text{Earth}$"), 
                     value = c(299792458, 
                               1.602176487e-19, 
                               6.62606896e-34, 
                               4.13566733e-15, 
                               1.3806504e-23, 
                               5.67040047372095e-08, 
                               1.496e+11, 
                               695500000, 
                               6.078608e+18, 
                               6371008.7714,
                               5.100659e+14), 
                     unit = c("\\si{\\metre\\per\\second}", 
                              "\\si{\\coulomb}", 
                              "\\si{\\joule\\second}", 
                              "\\si{\\electronvolt\\second}", 
                              "\\si{\\joule\\per\\kelvin}", 
                              "\\si{\\watt\\per\\square\\metre\\per\\kelvin\\tothe{4}}", 
                              "\\si{\\metre}", 
                              "\\si{\\metre}", 
                              "\\si{\\metre\\square}",
                              "\\si{\\metre}",
                              "\\si{\\metre}"), 
                     reference = c("per definition", 
                              "", 
                              "", 
                              "", 
                              "", 
                              "", 
                              "", 
                              "", 
                              "", 
                              "",
                              "A = 4 * pi * R.Earth^2")), 
                .Names = c("name", "symbol", "value", "unit", "reference"), 
                row.names = c("c",        # Speed of light 
                              "e",        # Elementary charge 
                              "h",        # Planck's constant 
                              "h.eV",     # Planck's constant 
                              "k",        # Boltzmann's constant 
                              "sigma",    # Stefan-Boltzmann constant 
                              "R.AU",     # Astronomical unit 
                              "R.Sun",    # Radius of the Sun 
                              "A.Sun",    # Surface area of the Sun
                              "R.Earth",  # Radius of the Earth
                              "A.Earth"), # Surface area of the Earth
                class = "data.frame")
   
}