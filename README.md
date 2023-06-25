# Solar spectra and constants for photoelectrochemistry

<!-- badges: start -->
[![DOI](https://raw.githubusercontent.com/solarchemist/photoec/master/man/figures/badge-doi.svg)](https://doi.org/10.5281/zenodo.8034780)
[![Vignettes](https://raw.githubusercontent.com/solarchemist/photoec/master/man/figures/badge-vignette.svg)](https://github.com/solarchemist/photoec#read-the-vignettes)
<!-- badges: end -->

With functions for calculating photon flux, current density, and solar-to-hydrogen
efficiency based on either the ASTM G173-03 reference spectrum (AM1.5G) or
on the black-body radiation (Planck theory), for relevant wavelength ranges.


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
── R CMD check results ───────────────────────────────────────────── photoec 0.4.1.9000 ────
Duration: 17.3s

❯ checking R code for possible problems ... NOTE
  cumflux: no visible binding for global variable ‘label’
  currentdensity: no visible binding for global variable ‘label’
  eV2nm: no visible binding for global variable ‘label’
  energy2wavelength: no visible binding for global variable ‘label’
  energy2wavenum: no visible binding for global variable ‘label’
  flux: no visible binding for global variable ‘label’
  nm2eV: no visible binding for global variable ‘label’
  sunlight.ASTM: no visible binding for global variable ‘label’
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

Contributions are welcome, no matter whether code, bug reports or suggestions!


## Read the vignettes

This package includes two datasets: `solarconstants` a dataframe of some solar constants,
and `ASTMG173` the ASTM G173-03 reference solar spectra.

+ The [first vignette demonstrates how to update the datasets in this package](https://htmlpreview.github.io/?https://github.com/solarchemist/photoec/blob/master/doc/datasets.html),
  and also show-cases the `solarconstants` dataset.
+ The [second vignette show-cases the solar spectra](https://htmlpreview.github.io/?https://github.com/solarchemist/photoec/blob/master/doc/solar-spectra.html).


## Citing this package

To cite `photoec` in publications use:

Taha Ahmed (2023). The photoec package: solar spectra and constants for photoelectrochemistry.
DOI: [10.5281/zenodo.8034780](https://doi.org/10.5281/zenodo.8034780).

Or see the `CITATION.cff` ([citation file format](https://citation-file-format.github.io/))
file in this repo or in the sidebar.
Please note that the DOI above always resolves to the latest release of this package.
If you want to explicitly cite a particular version, please use the corresponding DOI
(listed on the [Zenodo page](https://doi.org/10.5281/zenodo.8034780)).


## Suggested future work

Consider scrapping our own unit converters in favour of [units](https://cran.r-project.org/web/packages/units/index.html).


## Links and notes

+ https://cran.r-project.org/web/views/ChemPhys.html
