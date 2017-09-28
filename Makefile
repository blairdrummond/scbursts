PACKAGE = uottawaionchannel
VERSION = 0.1

all: deps docs build check clean

all-no-doc: deps build-no-doc check-no-doc clean

deps:
	Rscript -e 'if (!require("devtools"))  install.packages("devtools",  repos="http://cran.rstudio.com")';\
	Rscript -e 'if (!require("roxygen2"))  install.packages("roxygen2",  repos="http://cran.rstudio.com")';\
	Rscript -e 'if (!require("rmarkdown")) install.packages("rmarkdown", repos="http://cran.rstudio.com")';\
	Rscript -e 'if (!require("knitr"))     install.packages("knitr",     repos="http://cran.rstudio.com")'

docs:
	R -e 'devtools::document()'

build: 
	R CMD build .

build-no-doc:
	R CMD build --no-build-vignette .

install: build
	R CMD INSTALL $(PACKAGE)_$(VERSION).tar.gz

install-no-doc: build-no-doc
	R CMD INSTALL $(PACKAGE)_$(VERSION).tar.gz

check: build
	# R CMD check $(PACKAGE)_$(VERSION).tar.gz --as-cran
	R CMD check $(PACKAGE)_$(VERSION).tar.gz

check-no-doc: build-no-doc
	R CMD check $(PACKAGE)_$(VERSION).tar.gz --as-cran

clean:
	$(RM) -r $(PACKAGE).Rcheck/
