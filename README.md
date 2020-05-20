# Unique Location Extractor <img src="man/figures/logo.png" align="right" width="200" />

## Overview
Text often contains references to the locations of events where we want to extract the location of the event. For example, consider this example tweet that reports a crash in Nairobi, Kenya, where we are interested in extracting the location of the crash:

> crash occurred near garden city on thika road on your way towards thika mall.

The tweet contains three location references: (1) garden city, (2) thika road and (3) thika mall, where 'garden city' is the name of multiple locations that are over 20 kilometers apart. Here, we are interested in extracting the location of the garden city location on thika road that represents the crash site.

The Unique Location Extractor (ULEx) geoparses text to extract the unique location of events. The algorithm takes advantage of contextual information contained within text (references to roads or administrate areas, such as neighborhoods) and determines which location references should be ignored in order to determine the unique location of events.

This package was originally developed to extract locations of road traffic crashes from reports of crashes via Twitter, specifically in the context of Nairobi, Kenya using the Twitter feed [@Ma3Route](https://twitter.com/Ma3Route?ref_src=twsrc%5Egoogle%7Ctwcamp%5Eserp%7Ctwgr%5Eauthor).

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

## Main functions

The package contains two main functions: `augment_gazetteer` and `locate_event`. The backbone of locating events is looking up location references in a gazetteer, or a geographic dictionary. The `augment_gazetteer` facilitates cleaning a gazetteer that may have been constructed from sources such as Open Street Maps, geonames or Google Maps. It is specifically design to clean point locations or landmarks. The `locate_event` function then uses the gazetteer. `locate_event` takes text as input and returns the location of the relevant event.

### augment_gazetteer

##### Description

The `augment_gazetteer` function adds additional landmarks to account for different ways of saying the same landmark name. For example, raw gazetteers may contain long, formal names, where shorter versions of the name are more often used. In addition, the function facilities removing landmarks names that are spurious or may confuse the algorithm; these include landmark names that are common words that may be used in different contexts, or frequent and generic landmarks such as `hotel`. Key components of the function include:

1. Adding additional landmarks based off of n-grams and skip-grams of landmark names. For example, from the original landmark `garden city mall`, the following landmarks will be added: `garden city`, `city mall`, and  `garden mall`.
2. Adding landmarks according to a set of rules: for example, if a landmark starts or ends with a certain word, an alternative version of the landmark is added that removes that word. Here, words along categories of landmarks are removed, where a user may not reference the category; for example, a user will more likely say `McDonalds` than `McDonalds restaurant.`
3. Removes landmarks that refer to large geographic areas (e.g., roads). Roads and areas are dealt with separately; this function focuses on cleaning a gazetteer of specific points.
4. Determines whether a landmark should be categorized as `specific` or `general`. Specific landmarks are those where the name uniquely identifies a location. General landmarks are those where the names do no uniquely identify a location; however, a general landmark with contextual information such as a road can uniquely determine a location. Note that when multiple landmarks have the same name, but >90% of the landmarks are very closely clustered together, the landmarks are in the cluster are designated as `specific` while the other 10% are designated as `general`. The `locate_event` function only considers general landmarks when contextual information (roads or areas) are also referenced in the text.

##### Parameters

### locate_event

##### Description

The `locate_event` function extracts landmarks from text and determines the unique location of events from the text.

To extract locations from tex

##### Parameters


## Example

``` r
# Have example data, where geospatial is .geojson


```
