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

## augment_gazetteer

##### Description

The `augment_gazetteer` function adds additional landmarks to account for different ways of saying the same landmark name. For example, raw gazetteers may contain long, formal names, where shorter versions of the name are more often used. In addition, the function facilities removing landmarks names that are spurious or may confuse the algorithm; these include landmark names that are common words that may be used in different contexts, or frequent and generic landmarks such as `hotel`. Key components of the function include:

1. Adding additional landmarks based off of n-grams and skip-grams of landmark names. For example, from the original landmark `garden city mall`, the following landmarks will be added: `garden city`, `city mall`, and  `garden mall`.
2. Adding landmarks according to a set of rules: for example, if a landmark starts or ends with a certain word, an alternative version of the landmark is added that removes that word. Here, words along categories of landmarks are removed, where a user may not reference the category; for example, a user will more likely say `McDonalds` than `McDonalds restaurant.`
3. Removes landmarks that refer to large geographic areas (e.g., roads). Roads and areas are dealt with separately; this function focuses on cleaning a gazetteer of specific points.
4. Determines whether a landmark should be categorized as `specific` or `general`. Specific landmarks are those where the name uniquely identifies a location. General landmarks are those where the names do no uniquely identify a location; however, a general landmark with contextual information such as a road can uniquely determine a location. Note that when multiple landmarks have the same name, but >90% of the landmarks are very closely clustered together, the landmarks are in the cluster are designated as `specific` while the other 10% are designated as `general`. The `locate_event` function only considers general landmarks when contextual information (roads or areas) are also referenced in the text.

##### Parameters

* __landmarks:__ Spatial Points Dataframe (or sf equivalent) of landmarks.
* __landmarks.name_var:__ Name of variable indicating name of landmark
* __landmarks.type_var:__ Name of variable indicating type of landmark
* __grams_min_words:__ Minimum number of words in name to make n/skip-grams out of name
* __grams_max_words:__ Maximum number of words in name to make n/skip-grams out of name.
                  Setting a cap helps to reduce spurious landmarks that may come
                  out of really long names
* __skip_grams_first_last_word:__ For skip-grams, should first and last word be the
                             same as the original word? (TRUE/FASLE)
* __types_remove:__ If landmark has one of these types, remove - unless 'types_always_keep'
               or 'names_always_keep' prevents removing.
* __types_always_keep:__ landmark types to always keep. This parameter only becomes
                    relevant in cases where a landmark has more than one type.
                    If a landmark has both a "types_remove" and a "types_always_keep"
                    landmark, this landmark will be kept.
* __names_always_keep:__ landmark names to always keep. This parameter only
                   becomes relevant in cases where a landmark is one of
                    "types_remove." Here, we keep the landmark if "names_always_keep"
                    is somewhere in the name. For example, if the landmark is
                    a road but has flyover in the name, we may want to keep
                    the landmark as flyovers are small spatial areas.
* __parallel.rm_begin:__ If a landmark name begins with one of these words, add a
                    landmark that excludes the word.
* __parallel.rm_end:__ If a landmark name ends with one of these words, add a
                    landmark that excludes the word.
* __parallel.rm_begin_iftype:__ If a landmark name begins with one of these words, add a
                          landmark that excludes the word if the landmark is a
                           certain type.
* __parallel.rm_end_iftype:__ If a landmark name ends with one of these words, add a
                         landmark that excludes the word if the landmark is a
                         certain type.
* __parallel.word_diff_iftype:__ If the landmark includes one of these words, add a
                            landmarks that swap the word for the other words.
                           Only do if the landmark is a certain type.
* __parallel.word_end_addtype:__ If the landmark ends with one of these words,
                            add the type. For example, if landmark is "X stage",
                           this indicates the landmark is a bus stage. Adding the
                            "stage" to landmark ensures that the type is reflected.
* __rm.contains:__ Remove the landmark if it contains one of these words. Implemented
             after N/skip-grams and parallel landmarks are added.
* __rm.name_begin:__ Remove the landmark if it begins with one of these words. Implemented
             after N/skip-grams and parallel landmarks are added.
* __rm.name_end:__ Remov ethe landmark if it ends with one of these words. Implemented
              after N/skip-grams and parallel landmarks are added.
* __crs_distance:__ Coordiante reference system to use for distance calculations.

## locate_event

##### Description

The `locate_event` function extracts landmarks from text and determines the unique location of events from the text.

To extract location references from text, the function implements the following steps. Some parts of each step will extract the same landmark so to some extent are redundant; however, they all in some circumstances uniquely add landmarks.

1. Determines whether any text matches names in the gazetteer. Both exact and 'fuzzy' matches (allowing a certain levenstein distance) are used.
2. Relying on words after prepositions to find locations. The algorithm starts with a word after a preposition and extracts all landmarks that contain that word. Then, the algorithm takes the next word in the text and further subsets the landmarks. This process is repeated until adding a word removes all landmarks. If a road or area (eg, neighborhood) is found in the previous step, only landmarks near that road or neighborhood are considered. Landmarks with the shortest number of words are kept (i.e., if this process finds 5 landmarks with 2 words and 7 landmarks with 3 words, only the 5 landmarks with 2 words are kept).
3. If a road or area is mentioned and a landmark is not near that road or landmark, longer versions of the landmark that are near the road or area are searched for. For example, if a user says `crash near garden on thika road`, the algorithm may extract multiple landmarks with the name `garden`, none of which are near thika road. It will then search for all landmarks that contain `garden` in them that are near thika road.
4. If two roads are mentioned, the algorithm extracts the intersection of the roads.

After extracting landmarks, the algorithm chooses the correct landmark using a series of steps. These steps consider a defined list of event words (eg, for road traffic crashes, these could include 'crash', 'accident', 'overturn', etc), whether the user mentions a junction word (e.g., 'junction' or 'intersection') and a list of prepositions. Certain prepositions are given precedent over others to distinguish between locations indicating the location of an event versus locations further away that provide additional context; for example, `at` takes higher precedence that `towards`. The following main series of steps are used in the following order

1. Locations that follow the pattern [even word] [preposition] [location] are extracted.
2. Locations that follow the pattern [preposition] [location] are extracted. If multiple occurrences, the location near the higher order preposition is used. If a tie, the location closest to the event word is used. TODO: parameterize which should be prioritized: (1) location to event word or (2) preposition priority. Which one should we default and which should be tie-breaker? Not obvious, for example: `accident towards thika mall at garden city`.
3. If a junction word is used, two roads are mentioned, and the two roads intersect once, the intersection point is used.
4. The location closest to the event word is used.
5. If the location name has multiple locations, we (1) restrict to locations near any mentioned road or area, (2) check for a dominant cluster of locations and (3) prioritize certain landmark types over others (e.g., a user is more likely to reference a large, well known location type like a stadium).
6. If a landmark is not found, but a road or area are found, the road or area are returned. If a road and area are mentioned, the intersection of the road and area is returned.

##### Parameters

* __landmark_gazetteer:__ SpatialPointsDataframe or SpatialFeatures object with points.
* __landmark_gazetteer.name_var:__ Name of variable indicating name of landmark
* __landmark_gazetteer.type_var:__ Name of variable indicating type of landmark
* __landmark_gazetteer.gs_var:__   Name of variable indicating whether landmark is general or specific
* __roads:__ SpatialLinesDataframe or SpatialFeatures object with lines.
* __roads.name_var:__ Name of variable indicating name of road
* __areas:__ SpatialPolygonDataframe or SpatialFeatures object with polygons. Represents administrative areas.
* __areas.name_var:__ Name of variable indicating name of area.
* __prepositions_list:__  List of vectors of prepositions. Order of list determines order or prepsoition precedence.
* __event_words:__  Vector of event words.
* __junction_words:__  Vector of junction words.
* __false_positive_phrases:__  Common words found in text that include spurious
                        location references (eg, githurai bus is the name of a bus)
                        that includes the location githurai. This is common enough
                        that we should look for and remove.
* __type_list:__  List of vectors of types. Order of list determines order or type precedence.
* __clost_dist_thresh:__  Distance (meters) as to what is considered "close" (eg, is the landmark "close" to a road?)
* __fuzzy_match:__  Whether to implement fuzzy matching of landmarks using levenstein distance.
* TODO: Combine below two into one... eg, a list?
* __fuzzy_match.min_word_length:__  Minimum word length to use fuzzy/levenstein distance for matching.
* __fuzzy_match.dist:__  Allowable levenstein distances. Vector length must be same as above vector.
* __fuzzy_match.ngram_max:__  The number of n-grams that should be extracted from text
   to calculate a levensteing distance against landmarks. For example, if the text is
   composed of 5 words: w1 w2 w3 w4 and fuzzy_match.ngram_max=3, the function extracts
   [w1 w2 w3] and compares the levenstein distance to all landmarks. Then in checks
   [w2 w3 w4], etc.
* __fuzzy_match.first_letters_same:__  When implementing a fuzzy match, should the first
                                 letter of the original and found word be the same?
* __fuzzy_match.last_letters_same:__  When implementing a fuzzy match, should the last
                                 letter of the original and found word be the same?
* __crs_distance:__  Coordinate reference system to calculate distances. Should be projected.
* __crs_out:__  Coordinate reference system for output.
* __quiet:__  If TRUE, lets user know how far along the algorithm is.

## Example

``` r
# Have example data, where geospatial is .geojson


```
