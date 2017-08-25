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

### If you have LaTeX

You can install this from source with `Make`. You will need to make sure that you have this installed. Once you have it, the steps are:

1. Get a copy of this repository, either by downloading a zip or by `git clone`-ing. 

2. Open a terminal in the directory, and run

~~~
make
~~~

to prepare the package. This will build it and download all the dependencies that you need.

3. To install, then just run

~~~
make install
~~~

or possibly

~~~
sudo make install
~~~

And then the package should be installed.


### If you don't have LaTeX

You can install this from source with `Make`. You will need to make sure that you have this installed. Once you have it, the steps are:

1. Get a copy of this repository, either by downloading a zip or by `git clone`-ing. 

2. Open a terminal in the directory, and run

~~~
make all-no-doc
~~~

to prepare the package. This will build it and download all the dependencies that you need.

3. To install, then just run

~~~
make install-no-doc
~~~

or possibly

~~~
sudo make install-no-doc
~~~

And then the package should be installed. (though it won't have a manual)


### All Together

All assembled, this is all you need to running

```{.bash}
#!/bin/bash
git clone https://github.com/blairdrummond/bio
cd bio
R CMD build                     .   # If you have LaTeX and knitr 
R CMD build --no-build-vignette .   # Otherwise
R CMD INSTALL uottawaionchannel_*.tar.gz
```

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
