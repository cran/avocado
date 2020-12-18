
<!-- README.md is generated from README.Rmd. Please edit that file -->

# avocado

<!-- badges: start -->

[![CRAN\_Status\_Badge](https://www.r-pkg.org/badges/version/avocado)](https://cran.r-project.org/package=avocado)
[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![License:
MIT](https://img.shields.io/badge/License-MIT-blue.svg)](https://opensource.org/licenses/MIT)
[![GitHub
commit](https://img.shields.io/github/last-commit/nikdata/avocado)](https://github.com/nikdata/avocado/commit/main)
[![R-CMD-check](https://github.com/nikdata/avocado/workflows/R-CMD-check/badge.svg)](https://github.com/nikdata/avocado/actions)
<!-- badges: end -->

The {avocado} package provides a summary of weekly Hass avocado sales
for the contiguous United States. The underlying data are from [The Hass
Avocado Board](https://hassavocadoboard.com) (free registration
required). Hass Avocados are the most popular variety of avocados sold
in the United States and the Haas Avocado Board (HAB) provides crucial
data on them to growers and marketers.

The HAB makes this information available to anyone who may be interested
(free registration required). An important note to remember is that when
they use the term ‘units’, it often means the weight in US pounds. The
HAB does not provide (at least publicly) actual piece-count sales to
retailers.

## About the PLUs

The {avocadoo} package consists of 6 different
[PLUs](https://en.wikipedia.org/wiki/Price_look-up_code):

  - 4046: non-organic small/medium Hass Avocados (\~3-5 oz)
  - 4225: non-organic large Hass Avocados (\~8-10 oz)
  - 4770: non-organic extra large Hass Avocados (\~10-15 oz)
  - 94046: organic small/medium Hass Avocados (\~3-5 oz)
  - 94225: organic large Hass Avocados (\~8-10 oz)
  - 94770: organic extra large Hass Avocados (\~10-15 oz)

Source: [Love One
Today](https://loveonetoday.com/how-to/identify-hass-avocados/)

## Bags vs PLU

Another distinction that the HAB makes is between bags versus bulk. Bulk
typically means avocados sold as individual pieces and are easily
distinguishable with their PLU codes. Hence, the PLU refers to a bulk
sale. On the other hand, the bags indicates a pre-packaged container
consisting of a variable number of avocados of mixed PLU type. For
instance, a package of six avocados may consist of 2 PLU 4046, 3 PLU
4770 and 1 PLU 4225. In other words, bagged sales are unable to account
for individual PLU sales.

See this
[vignette](https://nikdata.github.io/avocado/articles/a_intro.html) for
more information.

## Installation

Install the development version from [GitHub](https://github.com/):

``` r
# install.packages("devtools")
devtools::install_github("nikdata/avocado", ref = 'main')
```

## Datasets

The {avocado} package consists of three different datasets:

  - `hass_usa`: weekly contiguous US avocado sales at the country level
  - `hass_region`: weekly contiguous US avocado sales at the region
    level
  - `hass`: weekly contiguous US avocado sales at the city/sub-region
    level
