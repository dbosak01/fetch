<!-- badges: start -->

[![fetch version](https://www.r-pkg.org/badges/version/fetch)](https://cran.r-project.org/package=fetch)
[![fetch lifecycle](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://cran.r-project.org/package=fetch)
[![fetch downloads](https://cranlogs.r-pkg.org/badges/fetch)](https://cran.r-project.org/package=fetch)
[![fetch total downloads](https://cranlogs.r-pkg.org/badges/grand-total/fetch)](https://cran.r-project.org/package=fetch)
[![R-CMD-check](https://github.com/dbosak01/fetch/workflows/R-CMD-check/badge.svg)](https://github.com/dbosak01/fetch/actions)
<!-- badges: end -->

# Introduction to **fetch**
<!--<img src="man/images/reporter_new.png" align="left" height="138px" style="margin-right:10px;height:138px"/>-->

Many R programs require multiple input datasets.  The purpose of the 
**fetch** package is to make it easier to access multiple datasets from
an R program.  The package allows you to load a catalog of all the 
datasets in a directory, and then load each desired dataset as needed into
your program.  The method minimizes the memory used for the data. 


### Installation

The **fetch** package can be installed from the console.  Simply run 
the following command: 

    install.packages("fetch")
    
Or if you want the latest development version, you can install it directly
from github:

    devtools::install_github("https://github.com/dbosak01/fetch")


Then put the following line at the top of your program or script:

    library(fetch)

The **fetch** package will give you access to a number of functions
to import data into your programs more easily and efficiently. 
For examples and usage information, visit the **fetch** documentation
site [here](https://fetch.r-sassy.org/articles/fetch.html).

### Getting Help

If you need help, the first place 
to turn to is the [fetch](https://fetch.r-sassy.org/) web site. The web site
has full documentation on all **fetch** functions.

If you want to look at the code for the **fetch** package, visit the
github page [here](https://github.com/dbosak01/fetch/).

If you encounter a bug or have a feature request, please submit an issue 
[here](https://github.com/dbosak01/fetch/issues/).

### See Also

The **sassy** meta-package includes several packages that help make R
easier for everyone.  You can read more about the **sassy** package
[here](https://r-sassy.org/).
