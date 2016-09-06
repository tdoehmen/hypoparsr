# hypoparsr

[![Build Status](https://travis-ci.org/tdoehmen/hypoparsr.svg?branch=master)](https://travis-ci.org/tdoehmen/hypoparsr)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/hypoparsr)](https://cran.r-project.org/package=hypoparsr) 

`hypoparsr` takes a different approach to CSV parsing by creating different parsing hypotheses for a given file and ranking them based on data quality features.

## Installation

* the latest released version from [CRAN](https://cran.r-project.org/package=hypoparsr) with

    ```R
    install.packages("hypoparsr")
    ````

* the latest development version from github with

    ```R
    devtools::install_github("tdoehmen/hypoparsr")
    ```

If you encounter a bug, please file a minimal reproducible example on [github](https://github.com/tdoehmen/hypoparsr/issues).

## Usage

```R
# generate a CSV
csv <- tempfile()
write.csv(iris, csv, row.names=FALSE)

# call hypoparsr
res <- hypoparsr::parse_file(csv)

# show result overview
print(res)

# get result data frames
best_guess <- as.data.frame(res)
second_best_guess <- as.data.frame(res, rank=2)
```
