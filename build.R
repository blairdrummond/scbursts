#!/usr/bin/R

library(devtools)
library(knitr)
library(tools)
names(vignetteEngine(package = 'knitr'))

devtools::install(build_vignettes = TRUE)
devtools::document()

# devtools::use_vignette()
# devtools::build()
