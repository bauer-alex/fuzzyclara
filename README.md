
# fuzzyclara <img src="man/figures/hex-sticker/fuzzyclara.png" align="right" width="200"/>

<!-- badges: start -->

[![R build status](https://github.com/bauer-alex/fuzzyclara/workflows/R-CMD-check/badge.svg)](https://github.com/bauer-alex/fuzzyclara/actions)
[![Codecov test coverage](https://codecov.io/github/bauer-alex/fuzzyclara/graph/badge.svg?token=XK0OBP9TA2)](https://app.codecov.io/gh/bauer-alex/fuzzyclara)
[![MIT license](https://img.shields.io/badge/license-MIT-brightgreen.svg)](https://opensource.org/licenses/MIT)
<!-- [![](https://cranlogs.r-pkg.org/badges/grand-total/fuzzyclara)](https://cran.r-project.org/package=fuzzyclara) -->
<!-- badges: end -->

Efficient and fuzzy clustering based on the CLARA algorithm

-   Authors: Maximilian Weigert,
    [Alexander Bauer](https://github.com/bauer-alex/), Jana Gauss
-   Contributors: Theresa Kriecherbauer, Asmik Nalmpatian
-   Version: 1.0.1

## Aim of this Package

The `fuzzyclara` package tackles two issues of cluster analysis applications.
First, it includes routines for fuzzy clustering which avoid the common hard
clustering assumption that each observation is a clear member of one sole
cluster. Instead, membership probabilities indicate to which extent the
characteristics of each observation are shaped by the characteristics of several
'typical' clusters. Second, the estimation of classical clustering algorithms
is often only hardly or not at all feasible in large data situations with
thousands of observations. Subsampling-based algorithms building on the CLARA
algorithm are implemented to make the estimation feasible in such situations.
Building on these two points, the 'fuzzyclara' package offers routines for all
aspects of a cluster analysis, including the use of user-defined distance
functions and diverse visualization techniques.

## Documentation and Useful Materials

To get an overview of the functionalities of the package, check out the
[JOSS publication](https://joss.theoj.org/papers/10.21105/joss.07887)
or the 
[package vignette](https://bauer-alex.github.io/fuzzyclara/articles/main_functionality.html).

## Installation

The most current version from GitHub can be installed via

``` r
devtools::install_github("bauer-alex/fuzzyclara")

# potential installation problems (specifically on MacOS) might be resolved
# by previously specifically installing some dependency packages
install.packages(c("vegclust", "ggwordcloud", "ggpubr", "factoextra"))
```

## How to Contribute

If you encounter problems with the package, find bugs or have
suggestions for additional functionalities please open a [GitHub
issue](https://github.com/bauer-alex/fuzzyclara/issues). Alternatively,
feel free to contact us directly [via email](mailto:baueralexander@posteo.de).

Contributions (via pull requests or otherwise) are welcome.
Please adhere to the [Advanced R style guide](http://adv-r.had.co.nz/Style.html) when contributing code.
Before you open a pull request or share your updates with us, please make sure that
all unit tests pass without errors or warning messages. You can run the
unit tests by calling

``` r
devtools::test()
```

