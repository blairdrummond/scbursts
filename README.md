# UOttawa Ion Channel Analysis

This `R` Package was designed for use by the [dacosta lab](http://www.dacosta.net/). The package contains the following features:

- Transform data between formats (`.evt`, `.dwt`, etc)
- Correct for effects of the Gaussian filter on the time-series data.
- Identify bursts in time series, and isolate those bursts.
- Provide toolkit for sorting, filtering, and studying individual bursts.
- **Tentatively** Burst detection.

If any features seem wrongfully absent, or if any methods can be improved upon, feel free to either create an [issue](https://github.com/blairdrummond/bio/issues) or (even better) submit a pull request.

# Installation

Requires:
	- LaTeX
	- texinfo


## With `devtools` (Recommended) (**WILL NOT WORK UNTIL REPO IS PUBLIC!!!**)

Open an R console, and run the following lines

```{.R}
install.package("devtools")
install.package("knitr")
install.package("rmarkdown")
install.package("roxygen2")
library(devtools)
install_github("blairdrummond/bio")
```

~~~
R CMD BUILD install.R
~~~

(This will just run those lines above for you.)

## From Source 

1. Clone this git repository. 

You may do this by whatever means you like. For example, by hitting the clone button and downloading (and then unpacking) the zip file. Alternatively, on mac or linux, this can be accomplished by opening a terminal in your directory of choice and running 

```
git clone https://github.com/blairdrummond/bio
```

2. Run the build script.
Execute the contents of the `build.R` file. Either with `R CMD BUILD build.r` or by copy-pasting the code into an R-console.



## From CRAN

Not possible yet, but maybe at some point in the future...

# How To:

After installing, there will be included documentation in the form of a pdf automatically included in the package root directory. **Discuss how to find that**

For a *not-necessarily up-to-date* pdf, click here **Provide a link to a reasonably up-to-date pdf**.


