# Unique Location Extractor <img src="man/figures/logo.png" align="right" width="200" />

## Overview
Text often contains references to locations that we want to extract. For example, less than 1% of Twitter users have their geolocation enabled; however, users often reference a specific location that we are interested in. In particular, the package is designed to extract the locations of individual events. However, when users report the location of an event, they may reference contextual location information (e.g., the event occurred near landmark X on road Y heading towards landmark Z). This package is designed to extract the relevant coordinates of the event, both

1. taking advantage of contextual information to identify the relevant locations
2. ignoring irrelevant location information

This package was originally developed to extract locations of road traffic crashes from reports of crashes via Twitter, specifically in the context of Nairobi, Kenya using the Twitter feed [Ma3Route](https://twitter.com/Ma3Route?ref_src=twsrc%5Egoogle%7Ctwcamp%5Eserp%7Ctwgr%5Eauthor).



## Installation

``` r
## Dependencies
library(magrittr)
library(lubridate)
library(dplyr)
library(tidyr)
library(readr)
library(purrr)
library(tidytext)
library(stringr)
library(stringi)
library(ngram)
library(hunspell)
library(stringdist)
library(tm)
library(raster)
library(rgeos)
library(parallel)
library(jsonlite)
library(maptools)
library(sf)

## Load functions
source("https://raw.githubusercontent.com/ramarty/Unique-Location-Extractor/master/R/helper_functions.R")
source("https://raw.githubusercontent.com/ramarty/Unique-Location-Extractor/master/R/locate_event.R")
```

## Example

``` r
# Have example data, where geospatial is .geojson


```
