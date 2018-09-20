# SCBursts - Single Channel Bursts Analysis

This `R` Package was designed for use by the [dacosta lab](http://www.dacosta.net/). The package contains the following features:

- Transform data between formats (`.evt`, `.dwt`, etc)
- Correct for effects of the Gaussian filter on the time-series data.
- Identify bursts in time series, and isolate those bursts.
- Provide toolkit for sorting, filtering, and studying individual bursts.
- **In the future** Burst detection.

If any features seem wrongfully absent, or if any methods can be improved upon, feel free to either create an [issue](https://github.com/dacostalab/scbursts/issues) or (even better) submit a pull request.

# Installation

## From `R`

Open an R console, and run the following lines

```{.R}
install.packages("devtools")
install.packages("knitr")
install.packages("rmarkdown")
install.packages("roxygen2")
library(devtools)
install_github("dacostalab/scbursts")
```

You should then be able to call

```{.R}
library(scbursts)
```

## From Source 

Start by installing the dependencies. On Ubuntu

```
# apt-get install texlive-full pandoc pandoc-citeproc make r-base pkg-config libcurl4-openssl-dev libxml2-dev
```

With those installed, you can install this from source with `make`. You will need to make sure that you have this installed. Once you have it, the steps are:

1. Get a copy of this repository, either by downloading a zip or by `git clone`-ing. 

2. Open a terminal in the directory, and run

~~~
make
~~~

to prepare the package. This will build it and download all the `R` dependencies that you need.

3. To install, then just run

~~~
make install
~~~

or possibly

~~~
sudo make install
~~~

And then the package should be installed. Though, you won't be able to create any manuals unless you have LaTeX installed.

## From CRAN

We don't have the package up yet, but should be easy to do when the time comes.

# Manual

You can view the soft-documentation for this package by calling

```{.R}
vignette("scbursts")
```

from an R console.
