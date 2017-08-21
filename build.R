#!/usr/bin/R

library(devtools)
library(knitr)
library(tools)
names(vignetteEngine(package = 'knitr'))

devtools::build()
devtools::document()
devtools::install(build_vignettes = TRUE)

# devtools::use_vignette()
