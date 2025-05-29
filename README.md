# morseDR <img src="morseDR-logo.png" align="right" height="139" />

Advanced methods for a valuable quantitative environmental risk assessment using 
Bayesian inference with several type of ecotoxicological data: 'binary' 
(e.g., survival, mobility), 'count' (e.g., reproduction) and 'continuous'
(e.g., growth rate, length, weight).

## Install from CRAN

```sh
library(remotes)
remotes::install_gitlab("mosaic-software/morsedr", host = "gitlab.in2p3.fr")
```

## Submission

Before a submission, you can look at [prepare-for-cran](https://github.com/ThinkR-open/prepare-for-cran)
, which is an open and collaborative list of things you have to check before 
submitting your package to the CRAN.

Otherwise, check "as-cran"" using the source package:
```
library(devtools)
# create documentation
devtools::document(roclets = c('rd', 'collate', 'namespace'))
```

Once the archive is done, check that '.Rbuildignore' was applied properly.
Try to have a low size archive (< 2Mb)

Either directly
```R
# build and check the archive
devtools::check()
```

Or in 2 steps:

```
# 1. build the package. 
devtools::build()
# 2. check the archive. 
devtools::check_built("../morseDR_0.1.1.tar.gz")
```

See the CRAN status of your sumbmission:
- incoming R CRAN packages: [Index of /incoming](https://cran.r-project.org/incoming/)
- incoming dashboard: [incoming dashboard](https://r-hub.github.io/cransays/articles/dashboard.html)


## Build the manual

```R
library('devtools')
devtools::document(roclets = c('rd', 'collate', 'namespace'))
devtools::build_manual()
```

## Coverage:

From R session

```R
library(covr)
cov <- package_coverage("morseDR")
```

# Style of process

## The succession of steps

1. `data`: load the data set.
2. `BinaryData`, `CountData` or `ContinuousData`: make a `ModelData` object for binary, count and quantitative continuous data, respectively. 
3. The above-mentioned objects inherit of `data.frame`
4. `plot`: plot a `ModelData` object.
5. `summary`: provides a summary of a `ModelData` object. 
7. `doseResponse`: return a `DoseResponse` object.
8. `plot`: plot a `DoseResponse` object.
8. `fit`: fit a `ModelData` object and return a `Fit` object.
9. `plot`: plot a `Fit` object.
10. `ppc`: return a `PPC` object.
11. `plot`: plot a `PPC` object.


# Coding Style

Object: `BigCamelCase`

```R
class(x) <- append("ObjectCamelCase", class(x))
```

Methods: `small_snake_case`

```R
methods_snake_case.ObjectCamelCase <- function(...){}
```

Function (no methods - not linked to object): `smallCamelCase`

```R
smallCamelCase <- function(...){}
```
