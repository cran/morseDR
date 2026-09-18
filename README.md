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

# Development

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
# not: `devtools::check(cran = TRUE)` is the default
```

Or in 2 steps:

```R
# 1. build the package. 
devtools::build()
# 2. check the archive. 
devtools::check_built("../morseDR_X.Y.Z.tar.gz")
```

See the CRAN status of your sumbmission:
- incoming R CRAN packages: [Index of /incoming](https://cran.r-project.org/incoming/)
- incoming dashboard: [incoming dashboard](https://r-hub.github.io/cransays/articles/dashboard.html)

Instead of doing `check` and the  `build`, we can do:

```R
devtools::release()
```

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

# Container

## A docker image ready to use

The dockerfile and the following script prepared a docker image ready to use in
a docker machine.

Build the Docker Image: First, ensure that you have built your Docker
image using a Dockerfile. You can do this with the following command:

```shell
docker build -t setup-morsedr .
```

Run the Docker Image: Once the image is built, you can run it
as a container using the following command:

```shell
docker run -it setup-morsedr /bin/bash
```

### push docker image on gitlab container registry

```shell
docker login gitlab-registry.in2p3.fr

# docker build -t gitlab-registry.in2p3.fr/mosaic-software/morsedr .
# docker push gitlab-registry.in2p3.fr/mosaic-software/morsedr

docker build -t gitlab-registry.in2p3.fr/mosaic-software/morsedr/morsedr-setup .
docker push gitlab-registry.in2p3.fr/mosaic-software/morsedr/morsedr-setup
```

```shell
docker run -it gitlab-registry.in2p3.fr/mosaic-software/morsedr/morsedr-setup /bin/bash
```
