# UOttawa Ion Channel Analysis

This `R` Package was designed for use by the [dacosta lab](http://www.dacosta.net/). The package contains the following features:

- Transform data between formats (`.evt`, `.dwt`, etc)
- Correct for effects of the Gaussian filter on the time-series data.
- Identify bursts in time series, and isolate those bursts.
- Provide toolkit for sorting, filtering, and studying individual bursts.
- **Tentatively** Burst detection.

If any features seem wrongfully absent, or if any methods can be improved upon, feel free to either create an [issue](https://github.com/blairdrummond/bio/issues) or (even better) submit a pull request.

# Installation

## With `devtools` (**WILL NOT WORK UNTIL REPO IS PUBLIC!!!**)

Open an R console, and run the following lines

```{.R}
install.package("devtools")
install.package("knitr")
install.package("rmarkdown")
install.package("roxygen2")
library(devtools)
install_github("blairdrummond/bio")
```

You should then be able to call

```{.R}
library(uottawaionchannel)
```

## From Source 

1. Clone this git repository. 

You may do this by whatever means you like. For example, by hitting the clone button and downloading (and then unpacking) the zip file. Alternatively, on mac or linux, this can be accomplished by opening a terminal in your directory of choice and running 

```
git clone https://github.com/blairdrummond/bio
```

2. Open a terminal and navigate to this folder

3. Build the package: 
	- If you have [LaTeX](https://www.latex-project.org/) and [knitr](https://github.com/yihui/knitr) installed, run
	  
	~~~
	R CMD build .
	~~~
	
	to build the package.
	
	- If you don't have these, you can still build the package, you just won't get a copy of the manual. You can still run
	
	~~~
	R CMD build --no-build-vignettes .
	~~~
	
4. You'll notice that there is now a `tar.gz` package. That's what we need: you can install it with

~~~
R CMD INSTALL uottawaionchannel_*.tar.gz
~~~

You should then have the package

## From CRAN

We don't have the package up yet, but should be easy to do when the time comes.

## RStudio might have a fancy way too. Not sure.

TODO: Look into this.

# Manual

You can view the soft-documentation for this package by calling

```{.R}
vignette("uottawaionchannel")
```

from an R console.
