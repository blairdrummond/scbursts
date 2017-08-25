#!/usr/bin/R

library(devtools)
library(knitr)
library(tools)
names(vignetteEngine(package = 'knitr'))

devtools::document()
devtools::build()
devtools::install(build_vignettes = TRUE)

# devtools::use_vignette()
