# Unique Location Extractor <img src="man/figures/logo.png" align="right" width="200" />

## Overview
Text often contains references to the locations of events where we want to extract the location of the event. For example, consider this example tweet that reports a crash in Nairobi, Kenya, where we are interested in extracting the location of the crash:

> crash occurred near garden city on thika road on your way towards roysambu.

The tweet contains three location references: (1) garden city, (2) thika road and (3) roysambu, where 'garden city' is the name of multiple locations that are over 20 kilometers apart. Here, we are interested in extracting the location of the garden city location on thika road that represents the crash site.

The Unique Location Extractor (ULEx) geoparses text to extract the unique location of events. The algorithm takes advantage of contextual information contained within text (references to roads or administrate areas, such as neighborhoods) and determines which location references should be ignored in order to determine the unique location of events.

This package was originally developed to extract locations of road traffic crashes from reports of crashes via Twitter, specifically in the context of Nairobi, Kenya using the Twitter feed [@Ma3Route](https://twitter.com/Ma3Route?ref_src=twsrc%5Egoogle%7Ctwcamp%5Eserp%7Ctwgr%5Eauthor).

## Installation

Until the package is made available via devtools (coming soon!), the functions can be loaded by running the following script

``` r
source("https://raw.githubusercontent.com/ramarty/Unique-Location-Extractor/master/R/load_ulex.R")
```

## Main functions

The package contains two main functions: `augment_gazetteer` and `locate_event`. The backbone of locating events is looking up location references in a gazetteer, or a geographic dictionary. The `augment_gazetteer` facilitates cleaning a gazetteer that may have been constructed from sources such as [Open Street Maps](https://cran.r-project.org/web/packages/osmdata/vignettes/osmdata.html), [Geonames](https://github.com/ropensci/geonames) or [Google Maps](https://www.rdocumentation.org/packages/googleway/versions/2.7.1/topics/google_places). It is specifically designed to clean a dictionary of point locations/landmarks. The `locate_event` function then uses the gazetteer. `locate_event` takes text as input and returns the location of the relevant event.

## Example

``` r
#### Packages
library(leaflet) # needed just to display output

#### Load Example Data
landmarks     <- st_read("https://raw.githubusercontent.com/ramarty/Unique-Location-Extractor/master/data/example_landmarks.geojson")
neighborhoods <- st_read("https://raw.githubusercontent.com/ramarty/Unique-Location-Extractor/master/data/example_areas.geojson")
roads         <- st_read("https://raw.githubusercontent.com/ramarty/Unique-Location-Extractor/master/data/example_roads.geojson")

#### Augment Gaztteer
landmarks_aug <- augment_gazetteer(landmarks,
                                   crs_distance = "+init=epsg:21037")

#### Locate Crashes in example tweets
tweets <- c("crash occurred near garden city on thika road on your way towards roysambu",
            "crash at garden city",
            "crash at intersection of juja road and outer ring rd",
            "crash occured near roysambu on thika rd",
            "crash at pangani")

crash_locs <- locate_event(text = tweets,
                           landmark_gazetteer = landmarks_aug,
                           areas = neighborhoods,
                           roads = roads,
                           crs_distance = "+init=epsg:21037")

#### Display output
leaflet() %>%
  addTiles() %>%
  addCircles(data=crash_locs,
             label = ~text,
             opacity = 1,
             weight=10,
             color = "red")
```

## augment_gazetteer

### Description

The `augment_gazetteer` function adds additional landmarks to account for different ways of saying the same landmark name. For example, raw gazetteers may contain long, formal names, where shorter versions of the name are more often used. In addition, the function facilitates removing landmarks names that are spurious or may confuse the algorithm; these include landmark names that are common words that may be used in different contexts, or frequent and generic landmarks such as `hotel`. Key components of the function include:

1. Adding additional landmarks based off of n-grams and skip-grams of landmark names. For example, from the original landmark `garden city mall`, the following landmarks will be added: `garden city`, `city mall`, and  `garden mall`.
2. Adding landmarks according to a set of rules: for example, if a landmark starts or ends with a certain word, an alternative version of the landmark is added that removes that word. Here, words along categories of landmarks are removed, where a user may not reference the category; for example, a user will more likely say `McDonalds` than `McDonalds restaurant.`
3. Removes landmarks that refer to large geographic areas (e.g., roads). Roads and areas are dealt with separately; this function focuses on cleaning a gazetteer of specific points/landmarks.
4. Determines whether a landmark should be categorized as `specific` or `general`. Specific landmarks are those where the name uniquely identifies a location. General landmarks are those where the names do no uniquely identify a location; however, a general landmark with contextual information such as a road can uniquely determine a location. Note that when multiple landmarks have the same name, but >90% of the landmarks are very closely clustered together, the landmarks in the cluster are designated as `specific` while the other 10% are designated as `general`. The `locate_event` function only considers general landmarks when contextual information (roads or areas) are also referenced in the text.

### Parameters

_Landmark Gazetteer_

Parameters for the raw landmark gazetteer.

* __landmarks:__ Spatial Points Dataframe (or sf equivalent) of landmarks.
* __landmarks.name_var:__ Name of variable indicating name of landmark (default: "name")
* __landmarks.type_var:__ Name of variable indicating type of landmark (default: "type")

_Remove Landmark Types_

Removing landmarks based on the type of the landmark. 'types_rm' indicates which types of landmarks should be removed, and 'types_rm.except_with_type' and 'types_rm.except_with_name' indicate situations when 'types_rm' should be ignored. Note that a landmark can have more than one type.

* __types_rm:__ If landmark is one of these types, remove the landmark - unless prevented by 'types_rm.except_with_type' or 'types_rm.except_with_name'. Here, types that do not represent a single location are removed. (default: c("route", "road", "political", "locality", "neighborhood")).
* __types_rm.except_with_type:__ Landmark type to always keep if includes one of these types; overrides 'types_rm'. Includes types that indicate a specific location, even if another type category suggests it covers a larger area. For example, if a landmark has types: 'route' and 'flyover', we want to keep this landmark as flyovers represent specific locations, not longer roads. (default: c("flyover"))
* __types_rm.except_with_name:__ Landmark type to always keep if the landmark name includes one of these words; overrides 'types_rm'. Includes names that indicate a specific location, even if another type category suggests it covers a larger area. For example, if a landmark has type: 'route' and includes 'flyover' in name, we want to keep this landmark as flyovers represent specific locations, not longer roads. (default: c("flyover"))

_N/Skip-Grams_

Parameters that determine how N and Skip-Grams should be generated and when they should be added to the gazetteer.

* __grams.min_words:__ Minimum number of words in name to make n/skip-grams out of name (default: 2)
* __grams.max_words:__ Maximum number of words in name to make n/skip-grams out of name. Setting a cap helps to reduce spurious landmarks that may come out of really long names. (default: 6)
* __grams.skip_gram_first_last_word_match:__ For skip-grams, should first and last word be the
                             same as the original word? (default: TRUE)
* __grams.add_only_if_name_new:__ Only add N/skip-grams if these names do not already exist in the gazetteer (default: FALSE)
* __grams.add_only_if_specific:__ Only add N/skip-grams if the name represents a specific location (ie, not a 'general' landmark with multiple, far away locations) (default: FALSE)

_Parallel Landmarks_

Changes the name of a landmark and adds the landmark as a new landmark to the gazetteer. Parameters indicate when and how to change names, and when parallel landmarks should be added to the gazetteer.

* __parallel.sep_slash:__ If a landmark has a slash, separate the landmark at the slash and add the components as new landmarks. (For example, landmark "a / b / c" will generate three new landmarks: "a", "b" and "c"). (default: TRUE)
* __parallel.rm_begin:__ If a landmark name begins with one of these words, add a landmark that excludes the word. (default: tm::stopwords("en"))
* __parallel.rm_end:__ If a landmark name ends with one of these words, add a landmark that excludes the word. (default: c("bar", "shops", "restaurant","sports bar","hotel", "bus station"))
* __parallel.word_diff:__ Generates parallel landmarks by swapping words with another word in a list. For example, "center" is replaced with "centre". OPTIONS: "none", "default" (accounts for some differences in british and american spelling), list of vectors (e.g., list(c("center", "centre"), c("theater", "theatre"))). (default: "default")
* __parallel.rm_begin_iftype:__ If a landmark name begins with one of these words, add a landmark that excludes the word if the landmark is a certain type. Input is a list of lists, where each sublist contains a vector of words and a vector of types (e.g., list(list(words = c("a", "b"), type = "t"))) (default: NULL)
* __parallel.rm_end_iftype:__ If a landmark name ends with one of these words, add a landmark that excludes the word if the landmark is a certain type. Input is a list of lists, where each sublist contains a vector of words and a vector of types (e.g., list(list(words = c("a", "b"), type = "t"))). (default: list(list(words = c("stage", "bus stop", "bus station"), type = "transit_station")).
* __parallel.word_diff_iftype:__ If the landmark includes one of these words, add a landmarks that swaps the word for the other words. Only do if the landmark is a certain type. (default: list(list(words = c("stage", "bus stop", "bus station"), type = "transit_station")
* __parallel.add_only_if_name_new:__ Only add parallel landmarks if the name doesn't already exist in the gazetteer (default: TRUE)
* __parallel.add_only_if_specific:__ Only add parallel landmarks if the landmark name represents a specific location (ie, not a 'general' landmark with multiple, far away locations) (default: FALSE)

_Add Parallel Landmarks: Same name, but add type_

Add a parallel landmark that includes an additional type

* __parallel_type.word_begin_addtype:__ If the landmark begins with one of these words, add the type. (default: NULL)
* __parallel_type.word_end_addtype:__ If the landmark ends with one of these words, add the type. For example, if landmark is "X stage", this indicates the landmark is a bus stage. Adding the "stage" to landmark ensures that the type is reflected. (default: list(list(words = c("stage", "bus stop", "bus station"), type = "stage")))

_Remove Landmarks_

After N/Skip-grams and parallel landmarks are added, parameters to decide which landmarks to remove based on the name

* __rm.contains:__ Remove the landmark if it contains one of these words. Implemented after N/skip-grams and parallel landmarks are added. (default: c("road", "rd"))
* __rm.name_begin:__ Remove the landmark if it begins with one of these words. Implemented after N/skip-grams and parallel landmarks are added. (default: c(stopwords("en"), c("near","at","the", "towards", "near")))
* __rm.name_end:__ Remove the landmark if it ends with one of these words. Implemented after N/skip-grams and parallel landmarks are added. (default: c("highway", "road", "rd", "way", "ave", "avenue", "street", "st"))

_Other_

* __close_dist_thresh:__ The distance to consider landmarks close together; relevant when generating 'specific' and 'general' landmarks. Distance is in spatial units of 'crs_distance'; if projected, then meters. (default: 500)
* __crs_distance:__ Coordinate reference system to use for distance calculations.
* __crs_out:__  Coordinate reference system for output. (default: "+init=epsg:4326")
* __quiet:__  Show algorithm progress (default: FALSE)

## locate_event

### Description

The `locate_event` function extracts landmarks from text and determines the unique location of events from the text.

To extract location references from text, the function implements the following steps. Some parts of each step will extract the same landmark so to some extent are redundant; however, they all in some circumstances uniquely add landmarks.

1. Determines whether any text matches names in the gazetteer. Both exact and 'fuzzy' matches (allowing a certain levenstein distance) are used.
2. Relying on words after prepositions to find locations. The algorithm starts with a word after a preposition and extracts all landmarks that contain that word. Then, the algorithm takes the next word in the text and further subsets the landmarks. This process is repeated until adding a word removes all landmarks. If a road or area (eg, neighborhood) is found in the previous step, only landmarks near that road or neighborhood are considered. Landmarks with the shortest number of words are kept (i.e., if this process finds 5 landmarks with 2 words and 7 landmarks with 3 words, only the 5 landmarks with 2 words are kept).
3. If a road or area is mentioned and a landmark is not near that road or landmark, longer versions of the landmark that are near the road or area are searched for. For example, if a user says `crash near garden on thika road`, the algorithm may extract multiple landmarks with the name `garden`, none of which are near thika road. It will then search for all landmarks that contain `garden` in them (e.g., `garden city mall`) that are near thika road.
4. If two roads are mentioned, the algorithm extracts the intersection of the roads.

After extracting landmarks, the algorithm chooses the correct landmark using a series of steps. These steps consider a defined list of event words (eg, for road traffic crashes, these could include 'crash', 'accident', 'overturn', etc), whether the user mentions a junction word (e.g., 'junction' or 'intersection') and a list of prepositions. Certain prepositions are given precedent over others to distinguish between locations indicating the location of an event versus locations further away that provide additional context; for example, `at` takes higher precedence that `towards`. The following main series of steps are used in the following order

1. Locations that follow the pattern [even word] [preposition] [location] are extracted.
2. Locations that follow the pattern [preposition] [location] are extracted. If multiple occurrences, the location near the higher order preposition is used. If a tie, the location closest to the event word is used. TODO: parameterize which should be prioritized: (1) location to event word or (2) preposition priority. Which one should we default and which should be tie-breaker? Not obvious, for example: `accident towards thika mall at garden city`.
3. If a junction word is used, two roads are mentioned, and the two roads intersect once, the intersection point is used.
4. The location closest to the event word is used.
5. If the location name has multiple locations, we (1) restrict to locations near any mentioned road or area, (2) check for a dominant cluster of locations and (3) prioritize certain landmark types over others (e.g., a user is more likely to reference a large, well known location type like a stadium).
6. If a landmark is not found, but a road or area are found, the road or area are returned. If a road and area are mentioned, the intersection of the road and area is returned.

### Parameters

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
   composed of 5 words: w1 w2 w3 w4 w5 and fuzzy_match.ngram_max=3, the function extracts
   [w1 w2 w3] and compares the levenstein distance to all landmarks. Then in checks
   [w2 w3 w4], etc.
* __fuzzy_match.first_letters_same:__  When implementing a fuzzy match, should the first
                                 letter of the original and found word be the same?
* __fuzzy_match.last_letters_same:__  When implementing a fuzzy match, should the last
                                 letter of the original and found word be the same?
* __crs_distance:__  Coordinate reference system to calculate distances. Should be projected.
* __crs_out:__  Coordinate reference system for output.
* __quiet:__  If TRUE, lets user know how far along the algorithm is.
