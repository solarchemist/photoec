# Solar spectra and constants for photoelectrochemistry

With functions for calculating photon flux, current density, and solar-to-hydrogen
efficiency based on either the ASTM G173-03 reference spectrum (AM1.5G) or
on the black-body radiation (Planck theory), for relevant wavelength ranges.

This package includes a dataframe of relevant solar constants.
These constants are show-cased in the
[**package vignette**](https://htmlpreview.github.io/?https://github.com/solarchemist/photoec/blob/master/doc/photoec.html)


## Install this package

To use this package, install it from this repo:

```
install.packages("remotes")
remotes::install_github("solarchemist/photoec")
```

If you encounter bugs or have questions
[please open an issue](https://github.com/solarchemist/photoec/issues).


## Develop this package

Check out the source code from this repo:
```
git clone https://github.com/solarchemist/photoec.git
```

I suggest the following package rebuild procedure (in RStudio IDE):

+ Run `devtools::check()` (in the console or via the **Build** pane).
  Should complete with one note about `undefined global functions or variables: label`
  but no warnings nor errors:
```
── R CMD check results ───────────────────────────────────────────── photoec 0.2.3.9000 ────
Duration: 18.5s

❯ checking R code for possible problems ... NOTE
  currentdensity: no visible binding for global variable ‘label’
  eV2nm: no visible binding for global variable ‘label’
  energy2wavelength: no visible binding for global variable ‘label’
  energy2wavenum: no visible binding for global variable ‘label’
  nm2eV: no visible binding for global variable ‘label’
  photonflux: no visible binding for global variable ‘label’
  sunlight.Planck: no visible binding for global variable ‘label’
  wavelength2energy: no visible binding for global variable ‘label’
  wavenum2energy: no visible binding for global variable ‘label’
  Undefined global functions or variables:
    label

0 errors ✔ | 0 warnings ✔ | 1 note ✖
```
+ Run `devtools::build_vignettes()` in the console. This recompiles the vignettes
  and populates the `doc/` directory, and should produce one warning
  about the vignette title being different from the package title (safe to ignore).
+ Manually remove the line `doc` from `.gitignore` (the build step keeps adding it).

Contributions are welcome, no matter if code, bug reports or suggestions!


## Suggested future work

Consider scrapping our own unit converters in favour of [units](https://cran.r-project.org/web/packages/units/index.html).


## Refs

+ https://cran.r-project.org/web/views/ChemPhys.html
