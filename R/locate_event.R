# Crashmap Algorithm

# 1. Search for other landmarks after preopostions, ignoring certain stopwords (the)
# if there are prepositions where landmark hasn't been found.
# a. Until no matches
# a. Check for dominant cluster?
# a. And check if priprity category... accident at [landmark] but can't find landmark,
# so no geocode.

# Package Dependencies ---------------------------------------------------------
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

counter.number <- 1

# Algorithm Inputs -------------------------------------------------------------
if(F){
  
  text <- "crash near airtel on mombasa rd words words words yaya center kenyatta ave"
  text_i <- text
  
  AUG_GAZ <- T
  
  # ALGORITHM PARAMETERS
  fuzzy_match_landmark <- TRUE
  fuzzy_match_landmark.min.word.length <- c(5,11) # minimum word length for fuzzy match
  fuzzy_match_landmark.dist <- c(1,2) # maximum levenstein distance to use
  fuzzy_match_ngram_max <- 3
  prepositions_list <- c("near", "at")
  crash_words <- c("accidents", "accident", "crash", "overturn", "collision", "wreck") # hit?
  junction_words <- c("intersection", "junction")
  first_letters_same <- TRUE
  last_letters_same <- TRUE # !!!!!!!! fails for roysambo/u. Maybe: if 1 letter off, only first letter needs to be same. But be more restrictive with 2?
  import_files <- FALSE
  tier_1_prepositions <- c("at", "next to","around", "just after", "opposite","opp", "apa", "hapa","happened at","just before","at the","outside","right before")
  tier_2_prepositions <- c("near", "after", "toward","along", "towards", "approach")
  tier_3_prepositions <- c("past","from","on")
  projection <- "+init=epsg:21037"
  
  # False positive words are those that frequently appear in tweets that refer to 
  # something specific, but part of the name refers to a useful location. So 
  # algorithm thinks is location but is really something else. An example is 
  # "githurai bus", which refers to a bus that travels to/from githurai -- but
  # where githurai is also a location.
  false_positive_phrases <- c("githurai bus", "githurai matatu", 
                              "githurai 45 bus", "githurai 45 matatu",
                              "city hoppa bus", "hoppa bus",
                              "rongai bus", "rongai matatu", "rongai matatus",
                              "machakos bus", "machakos minibus", "machakos matatu")
  
  # Import
  if(AUG_GAZ){
    landmark_gazetteer_orig <- readRDS(file.path(algorithm_inputs, "gazetteers_augmented", "gazetteer_aug.Rds"))
  } else{
    landmark_gazetteer_orig <- readRDS(file.path(algorithm_inputs, "gazetteers_raw","merged", "gazetter_allsources_raw.Rds"))
  }
  
  landmark_gazetteer <- landmark_gazetteer_orig
  landmark_gazetteer <- landmark_gazetteer[!is.na(landmark_gazetteer$lat),]
  coordinates(landmark_gazetteer) <- ~lon+lat
  crs(landmark_gazetteer) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
  
  roads <- readRDS(file.path(algorithm_inputs, "roads_augmented", "osm_roads_aug.Rds"))
  neighborhoods <- readRDS(file.path(algorithm_inputs, "nairobi_estates", "nairobi_estates.Rds"))
  
}

# Functions Used in Algorithm ==================================================
bind_rows_sf <- function(...){
  # Description: Bind rows of spatial features
  
  sf_list <- rlang::dots_values(...)[[1]]
  
  sfg_list_column <- lapply(sf_list, function(sf) sf$geometry[[1]]) %>% st_sfc
  df <- lapply(sf_list, function(sf) st_set_geometry(sf, NULL)) %>% bind_rows
  
  sf_appended <- st_sf(data.frame(df, geom=sfg_list_column))
  
  return(sf_appended)
}

extract_dominant_cluster <- function(sdf,
                                     close_thresh_km = 0.5,
                                     cluster_thresh = 0.9,
                                     N_loc_limit = 100,
                                     return_coord_only = F){
  # DESCRIPTION: Finds a dominant spatial cluster in a set of points
  # ARGS:
  # sdf: Spatial points dataframe. Assumed to be projected.
  # close_thresh_km: distance threshold (in kilometers) at which points
  #                  are considered close
  # cluster_thresh: Cluster threshold, which defines the proportion of points
  #                 that must be close by to be considered a dominant 
  #                 spatial cluster.
  # N_loc_limit: If points aren't close, only attempts to find a dominant
  #              spatial cluster if the number of points is less than this 
  #              threshold. If the number of points is above this threshold,
  #              we run into computational efficiency issues in calculating
  #              distance matrix and just assume unlikely to find a dominant
  #              cluster.
  # return_coord_only: If TRUE, just returns the centroid coordinates of the
  #                    dominant spatial cluster. If FALSE, returns all 
  #                    the original spatial dataframe but subsetted to the
  #                    observations in the dominant spatial cluster. If no
  #                    dominant spatial cluster is found, a NULL dataframe
  #                    is returned.
  
  # Default output if not cluster is found
  sdf_out <- data.frame(NULL)
  
  # 1. Fast all close check ----------------------------------------------
  sdf_extent <- extent(sdf) 
  
  max_dist_km <- sqrt((sdf_extent@xmax - sdf_extent@xmin)^2 +
                        (sdf_extent@ymax - sdf_extent@ymin)^2) * 1000
  
  # If everything is close...
  if(max_dist_km < close_thresh_km){
    
    sdf_out <- sdf
    
    # If locations aren't close and not too many locations...  
  } else if ( (max_dist_km > close_thresh_km) & (nrow(sdf) <= N_loc_limit)){
    
    ## Distance matrix
    sdf_dist_mat <- gDistance(sdf, byid=T) * 1000
    
    # Check if cluster exists
    sdf_dist_mat_list <- sdf_dist_mat %>% as.list %>% unlist
    cluster_exists <- mean(sdf_dist_mat_list <= close_thresh_km) >= cluster_thresh
    
    # If cluster exists, extract coordinates of dominant cluster
    if(cluster_exists){
      # Extract columns where more than X % are close together
      close_thresh_km_closeTF <- sdf_dist_mat <= close_thresh_km
      in_dominant_cluster <- (colSums(close_thresh_km_closeTF) / nrow(close_thresh_km_closeTF) >= .9)
      
      sdf <- sdf[in_dominant_cluster,]
      
      sdf_out <- sdf
    }
    
  }
  
  return(sdf_out)
}

phrase_in_sentence_exact <- function(sentence,
                                     phrase_list){
  
  # Description: Determines which words or phrases are in a sentence using an exact match
  # sentence: sentence to examine
  # phrase_list: list of phrases to check if in sentence
  
  # Grabs landmarks in sentence, regardless of word boundaries (faster than checking with boundaries)
  locations_candidates <- sentence %>%
    str_extract_all(fixed(phrase_list)) %>% 
    unlist() %>%
    unique
  
  if(length(locations_candidates) > 0){
    
    # Add word boundaries
    locations_candidates <- paste0("\\b", locations_candidates, "\\b")
    
    locations_df <- sentence %>%
      str_extract_all(locations_candidates) %>% 
      unlist() %>%
      unique %>%
      as.data.frame %>%
      dplyr::rename(matched_words_tweet_spelling = ".") %>%
      dplyr::mutate(matched_words_correct_spelling = matched_words_tweet_spelling) %>%
      dplyr::mutate(exact_match = T)
    
  } else{
    locations_df <- data.frame(NULL)
  }
  
  return(locations_df)
}

phrase_in_sentence_fuzzy_i <- function(sentence, 
                                       phrase_list, 
                                       fuzzy_match_landmark.min.word.length, 
                                       fuzzy_match_landmark.dist,
                                       fuzzy_match_ngram_max,
                                       first_letters_same,
                                       last_letters_same){
  # Description: Determine if a word or phrase (ie, ngram) is in a sentence using 
  # a fuzzy match
  # sentence: sentence to examine
  # phrase_list: list of phrases to check if in sentence
  # fuzzy_match_landmark.min.word.length: excludes phrases below this number of characters
  # fuzzy_match_landmark.dist: maximum levenstein distance to allow phrases to match
  # fuzzy_match_ngram_max: number of ngrams to check if in sentence (equal or less than max words in phrase in phrase_list)
  # first_letters_same: only keep matches where first letters are the same
  # last_letters_same: only keep matches where last letters are the same
  
  #### Tweet preparation
  # Remove stop words (replace with other words to preserve fact that words are
  # between other words)
  
  remove_words_regex <- paste(c(paste0("\\b",tier_1_prepositions,"\\b"),
                                paste0("\\b",tier_2_prepositions,"\\b"),
                                paste0("\\b",tier_3_prepositions,"\\b"),
                                paste0("\\b",crash_words,"\\b"),
                                paste0("\\b",junction_words,"\\b")), collapse="|")
  sentence <- sentence %>% str_replace_all(remove_words_regex, "thisisafillerwordpleaseignoremore") %>% str_squish
  
  #### Checks
  if(length(fuzzy_match_landmark.min.word.length) != length(fuzzy_match_landmark.dist)){
    stop("fuzzy_match_landmark.min.word.length and fuzzy_match_landmark.dist must be the same length")
  }
  
  phrase_list <- unique(phrase_list)
  
  #### Extract n-grams from sentence
  if(str_count(sentence, '\\w+') > 1){
    sentence_words <- ngram::ngram_asweka(sentence, min=1, max=fuzzy_match_ngram_max) 
  } else{
    sentence_words <- sentence
  }
  
  ## Limit n-grams
  # Only consider n-grams that have more than X characters
  # Don't include certain words (eg, "road") in count
  sentence_words_nchar <- nchar(sentence_words)
  
  for(word_dont_consider in c("road", "rd", "street", "st", "avenue", "ave")){
    sentence_words_nchar[grepl(paste0("\\b", word_dont_consider, "\\b"), sentence_words)] <- sentence_words_nchar[grepl(paste0("\\b", word_dont_consider, "\\b"), sentence_words)] - nchar(word_dont_consider)
  }
  
  sentence_words <- sentence_words[sentence_words_nchar >= fuzzy_match_landmark.min.word.length] 
  
  # Only consider n-grams that don't start with a stop word
  stopwords_regex <- paste0("^", stopwords() , "\\b") %>% paste(collapse="|")
  sentence_words <- sentence_words[!grepl(stopwords_regex, sentence_words)]
  
  sentence_words <- sentence_words[!grepl("thisisafillerwordpleaseignoremore",sentence_words)]
  
  #### Limit phrase_list to speed up matching
  # If say that first and last letters must be same, restrict to phrases where
  # this must be the case
  if(first_letters_same){
    first_letters_sentence <- sentence_words %>% str_sub(1,1)
    phrase_list <- phrase_list[str_sub(phrase_list,1,1) %in% first_letters_sentence]
  }
  
  if(last_letters_same){
    last_letters_sentence <- sentence_words %>% str_sub(-1,-1)
    phrase_list <- phrase_list[str_sub(phrase_list,-1,-1) %in% last_letters_sentence]
  }
  
  # Levenstein Distance
  phrase_match_loc <- amatch(sentence_words, phrase_list, maxDist = fuzzy_match_landmark.dist, method="lv")
  
  # Check if any matches found
  if(sum(!is.na(phrase_match_loc)) > 0){
    
    # Locations Dataframe
    locations_df <- data.frame(matched_words_tweet_spelling = sentence_words[!is.na(phrase_match_loc)],
                               matched_words_correct_spelling = phrase_list[phrase_match_loc[!is.na(phrase_match_loc)]],
                               exact_match = F)
    
    # Restrict to ones where first letter is the same
    if(first_letters_same == TRUE){
      
      first_letter_tweet_spelling <- locations_df$matched_words_tweet_spelling %>% str_sub(1,1)
      first_letter_correct_spelling <- locations_df$matched_words_correct_spelling %>% str_sub(1,1)
      
      locations_df <- locations_df[first_letter_tweet_spelling %in% first_letter_correct_spelling,]
      
    }
    
    # Restrict to ones where last letter is the same
    if(last_letters_same == TRUE){
      
      last_letter_tweet_spelling <- locations_df$matched_words_tweet_spelling %>% str_sub(-1,-1)
      last_letter_correct_spelling <- locations_df$matched_words_correct_spelling %>% str_sub(-1,-1)
      
      locations_df <- locations_df[last_letter_tweet_spelling %in% last_letter_correct_spelling,]
      
    }
    
    # Format
    locations_df <- locations_df %>%
      mutate(matched_words_tweet_spelling   = matched_words_tweet_spelling %>% as.character(),
             matched_words_correct_spelling = matched_words_correct_spelling %>% as.character())
    
  } else{
    locations_df <- data.frame(NULL)
  }
  
  return(locations_df)
}

phrase_in_sentence_fuzzy <- function(text_i, 
                                     landmark_list,
                                     fuzzy_match_landmark.min.word.length, 
                                     fuzzy_match_landmark.dist, 
                                     fuzzy_match_ngram_max,
                                     first_letters_same,
                                     last_letters_same){
  
  # Implements phrase_in_sentence_fuzzy_i, looping through different
  # values of fuzzy_match_landmark.min.word.length and fuzzy_match_landmark.dist
  
  df <- lapply(1:length(fuzzy_match_landmark.min.word.length), function(i){
    df_i <- phrase_in_sentence_fuzzy_i(text_i, 
                                       landmark_list,
                                       fuzzy_match_landmark.min.word.length[i], 
                                       fuzzy_match_landmark.dist[i], 
                                       fuzzy_match_ngram_max,
                                       first_letters_same,
                                       last_letters_same)
    return(df_i)
  }) %>% 
    bind_rows %>% 
    unique
  
  return(df)
}

phrase_locate <- function(phrase, sentence){
  # Description: Takes a word or phrase and a sentence as input and returns the
  # start and end word location of the phrase within the sentence. For example,
  # if phrase is "garden city" and sentence is "crash near garden city involving
  # pedestrian", the function will return a dataframe with three variables:
  # word = "garden city", word_loc_min = 3, word_loc_min = 4.
  # phrase: phrase or word 
  # sentence: sentence to search in
  
  # Check if phrase in sentence
  if(grepl(paste0("\\b",phrase,"\\b"), sentence)){
    
    # Sentence Word and Character Position
    sentence_words_loc <- strsplit(sentence," ") %>% 
      as.data.frame %>% 
      dplyr::rename(word = names(.)[1])
    sentence_words_loc$word <- as.character(sentence_words_loc$word)
    sentence_words_loc$word_number <- 1:nrow(sentence_words_loc)
    sentence_words_loc$word_length <- nchar(sentence_words_loc$word)
    sentence_words_loc$word_char_start <- lapply(1:nrow(sentence_words_loc), tweet_word_start_character, sentence_words_loc) %>% unlist
    
    phrase_char_start_end <- str_locate_all(sentence, phrase)[[1]]
    
    phrase_location_df <- lapply(1:nrow(phrase_char_start_end),
                                 function(i){
                                   df <- sentence_words_loc[sentence_words_loc$word_char_start >= phrase_char_start_end[i,][1] & sentence_words_loc$word_char_start <= phrase_char_start_end[i,][2],]
                                   df_out <- data.frame(word_loc_min = min(df$word_number),
                                                        word_loc_max = max(df$word_number))
                                   return(df_out)
                                 }) %>% bind_rows
    
    phrase_location_df$word <- phrase
  } else{
    warning("phrase is not in sentence")
    phrase_location_df <- data.frame(NULL)
  }
  
  return(phrase_location_df)
}

tweet_word_start_character <- function(i, tweet_words_loc){
  # Supports: phrase_locate function
  if(i == 1){
    return(1)
  } else{
    return(sum(tweet_words_loc$word_length[1:(i-1)]) + i)
  }
}

#aaa <- map_df(prep_locs, 
#              extract_locations_after_words,
#              text_i_no_stopwords,
#              landmark_gazetteer)

#word_loc = 4
#text = text_i_no_stopwords
#landmarks = landmark_gazetteer

extract_locations_after_words <- function(word_loc, 
                                          text, 
                                          landmarks){
  # DESCRIPTION: Searches for location references after words (typically 
  # prepositions). Allows for partially matchine names.
  # ARGS:
  # word_loc: Index of the word (eg, preposition) in the text. For example,
  #           "accident near garden city", if the word for "word_loc" is "near",
  #           the index would be 2.
  # text: Text to search for locations
  # landmark_gazetteer: Spatial dataframe of landmarks with a "name" variable.
  
  ## Default - blank spatial polygons dataframe. Make sure has a variable
  # that typical output would include
  landmarks_out <- data.frame(NULL)
  
  # 1. Conditions to check for word ------------------------------------------
  # Check for conditions based on the next word after the propsotion. If one
  # of the following conditions exists, we don't consider words after the 
  # preposition
  next_word <- word(text, word_loc+1)
  
  next_word_none <- is.na(next_word)
  next_word_ignoreword <- next_word %in% c(stopwords::stopwords(language="en"), "exit", "top", "bottom", "scene")
  next_word_short <- nchar(next_word) < 3
  
  if(!next_word_none & !next_word_ignoreword & !next_word_short){
    
    # 2. Grab gazeteer words after preposition -------------------------------
    
    ## Start with full gazeteer
    landmarks_subset <- landmarks
    
    ## Loop through words after preposition
    for(i in 1:10){
      word_i <- word(text, word_loc+i)
      
      # If first word after preposition, the gazetteer word must start with that word.
      # Restrict words in gazetteer, creating a temporary dataframe
      if(i == 1) landmarks_subset_candidate <- landmarks_subset[grepl(paste0("^", word_i, "\\b"),   landmarks_subset$name), ]
      if(i > 1)  landmarks_subset_candidate  <- landmarks_subset[grepl(paste0("\\b", word_i, "\\b"), landmarks_subset$name), ]
      
      # If the gazeteer still has words after subsetting, replace
      # gazeteer with gazeteer_temp. If gazeteer doesn't have words left,
      # break out of loop and use last version of gazeteer with words.
      if(nrow(landmarks_subset_candidate) > 0){
        landmarks_subset <- landmarks_subset_candidate
      } else{
        break
      }
      
    }
    
    # 3. Subset selected words -----------------------------------------------
    # Keep landmarks with shortest word length. For example, if landmark
    # with fewest words has 2 words, we only keep landmarks with 2 words
    min_words <- min(str_count(landmarks_subset$name, "\\S+"))
    landmarks_subset <- landmarks_subset[str_count(landmarks_subset$name, "\\S+") %in% min_words,]
    landmarks_subset <- landmarks_subset[!is.na(landmarks_subset$lat),] # if blank, will give one row with NAs
    
    # 4. Check for dominant cluster ------------------------------------------
    landmarks_subset <- extract_dominant_cluster(landmarks_subset)
    
    # 5. Format Output -------------------------------------------------------
    if(nrow(landmarks_subset) >= 1){
      
      landmarks_subset@data <- landmarks_subset@data %>%
        dplyr::rename(matched_words_correct_spelling = name) %>%
        mutate(exact_match = FALSE,
               location_type = "landmark")
      
      ## Add tweet spelling
      max_word_length <- landmarks_subset$matched_words_correct_spelling %>% str_count("\\S+") %>% max()
      
      landmarks_subset@data <- landmarks_subset@data %>%
        mutate(matched_words_tweet_spelling = word(text,
                                                   word_loc + 1,
                                                   word_loc + max_word_length))
      
      landmarks_out <- landmarks_subset@data
    }
    
  }
  
  return(landmarks_out)
  
}

##### ******************************************************************** #####
# ALGORITHM NEW ================================================================
locate_event <- function(text){
  
  # 1. Checks ------------------------------------------------------------------
  # Check inputs and output errors if anything is wrong.
  
  # 2. Clean/Prep Input Files --------------------------------------------------
  # Cleans and preps gazetteer and road files
  
  #### Project Data
  landmark_gazetteer <- spTransform(landmark_gazetteer, CRS(projection))
  roads              <- spTransform(roads,              CRS(projection))
  neighborhoods      <- spTransform(neighborhoods,      CRS(projection))
  
  #### Clean Names
  landmark_gazetteer$name <- landmark_gazetteer$name %>%
    str_replace_all("[[:punct:]]", "")
  
  roads$name <- roads$name %>% 
    as.character %>% 
    tolower
  
  ## Names into lists
  landmark_list <- landmark_gazetteer$name
  roads_list <- roads$name %>% as.character %>% tolower
  neighborhoods_list <- neighborhoods$estate
  
  ## Preposition list
  tier_1_prepositions_startendword <- paste0("\\b",tier_1_prepositions,"\\b")
  tier_2_prepositions_startendword <- paste0("\\b",tier_2_prepositions,"\\b")
  tier_3_prepositions_startendword <- paste0("\\b",tier_3_prepositions,"\\b")
  
  prepositions_list <- c(tier_1_prepositions, tier_2_prepositions)
  prepositions_list_all <- c(tier_1_prepositions, tier_2_prepositions, tier_3_prepositions)
  
  ## Add unique ID to gazetteer
  landmark_gazetteer$uid <- 1:nrow(landmark_gazetteer)
  roads$uid              <- 1:nrow(roads)
  neighborhoods$uid      <- 1:nrow(neighborhoods)
  
  # 3. Clean/Prep Text ---------------------------------------------------------
  # Cleans and preps text
  
  #### Clean text
  text <- text %>%
    
    iconv("latin1", "ASCII", sub="") %>%
    
    str_to_lower %>%
    str_replace_all("\\br/about\\b", "round about") %>%
    
    # Remove select stopwords
    str_replace_all("\\bthe\\b", " ") %>%
    str_replace_all("\\ba\\b", " ") %>%
    
    str_replace_all("-", " ") %>%
    str_replace_all("\\.", " ") %>%
    str_replace_all("via @[a-z_,A-Z_,0-9_]*", "") %>%
    str_replace_all("\\@", "at ") %>% # "@cabanas, saying crash is at cabanas"
    str_replace_all("@[a-z_,A-Z_,0-9_]*", "") %>%
    str_replace_all(","," , ") %>% # Add space between commas (eg, "road,allsops") 
    str_replace_all("\n", "") %>%
    str_replace_all("~", "") %>%
    str_replace_all("\\b(http|https)://t.co/[0-9,A-Z, a-z]*\\b", "") %>%
    str_replace_all("\\b(http|https)://t.co/[0-9,A-Z, a-z]", "") %>%
    str_replace_all("\\b(http|https)://t.co\\b", "") %>%
    str_replace_all("\\b(http|https):", "") %>%
    str_replace_all("~more*", "") %>%
    str_replace_all("(RT|rt) @[a-z,A-Z,0-9, _]*:", "") %>%
    str_replace_all("^[0-9][0-9]\\:[0-9][0-9]", "") %>%
    str_replace_all("\\+", " ") %>%
    str_replace_all("[[:punct:]]", "") %>%
    str_replace_all("\\bamp\\b", "and") %>%
    
    # Replace Accronmys
    # TODO: Make this a separate file csv file that gets used
    str_replace_all("\\bpri sch\\b", "primary school") %>%
    str_replace_all("\\bsec sch\\b", "secondary school") %>%
    
    # Squish strings. Should be last thing done
    str_squish()
  
  #### Remove false positive phrases
  for(phrase in false_positive_phrases){
    # Replace false positive phrases with "blankword", where blankword appears the
    # number of times for each word in the phrase. For example, "githurai bus"
    # becomes "blankword blankword" and "githurai 45 bus" becomes "blankword
    # blankword blankword". Replacing with "bankword" is important as it preserves
    # the number of words between different words -- which is used later in the algorithm.
    text <- text %>% str_replace_all(paste0("\\b",phrase,"\\b"), 
                                     rep("bankword", wordcount(phrase)) %>% 
                                       paste(collapse=" "))
  }
  
  # 4. OTHER STUFF HERE --------------------------------------------------------
}

locate_event_i <- function(text_i){
  
  # 1. Determine Location Matches in Gazetteer ---------------------------------
  #### Exact Match
  landmark_match     <- phrase_in_sentence_exact(text_i, landmark_list) 
  road_match         <- phrase_in_sentence_exact(text_i, roads_list)
  neighborhood_match <- phrase_in_sentence_exact(text_i, neighborhoods_list)
  
  #### Fuzzy
  if(fuzzy_match_landmark == TRUE){
    landmark_match_fuzzy <- phrase_in_sentence_fuzzy(text_i, 
                                                     landmark_list,
                                                     fuzzy_match_landmark.min.word.length, 
                                                     fuzzy_match_landmark.dist, 
                                                     fuzzy_match_ngram_max,
                                                     first_letters_same,
                                                     last_letters_same) 
    road_match_fuzzy <- phrase_in_sentence_fuzzy(text_i, 
                                                 roads_list,
                                                 fuzzy_match_landmark.min.word.length, 
                                                 fuzzy_match_landmark.dist, 
                                                 fuzzy_match_ngram_max,
                                                 first_letters_same,
                                                 last_letters_same) 
    neighborhood_match_fuzzy <- phrase_in_sentence_fuzzy(text_i, 
                                                         neighborhoods_list,
                                                         fuzzy_match_landmark.min.word.length, 
                                                         fuzzy_match_landmark.dist, 
                                                         fuzzy_match_ngram_max,
                                                         first_letters_same,
                                                         last_letters_same) 
    
    #### Remove fuzzy match if:
    # (1) Tweet spelling is one word
    # (2) Tweet spelling is correctly spelled
    # TODO: If landmark is two words long and both words spelled correctly,
    #       hunspell things incorrectly spelled?
    landmark_match_fuzzy <- landmark_match_fuzzy %>%
      filter(!(str_count(matched_words_tweet_spelling, "\\S+") %in% 1)) %>%
      filter(!hunspell_check(matched_words_tweet_spelling))
    
    road_match_fuzzy <- road_match_fuzzy %>%
      filter(!(str_count(matched_words_tweet_spelling, "\\S+") %in% 1)) %>%
      filter(!hunspell_check(matched_words_tweet_spelling))
    
    neighborhood_match_fuzzy <- neighborhood_match_fuzzy %>%
      filter(!(str_count(matched_words_tweet_spelling, "\\S+") %in% 1)) %>%
      filter(!hunspell_check(matched_words_tweet_spelling))
    
    #### Add fuzzy match to full match list
    # Starting with exact match ensures, if both exact and fuzzy, only
    # exact is kept.
    landmark_match <- bind_rows(landmark_match, landmark_match_fuzzy) %>%
      distinct(matched_words_correct_spelling, .keep_all = TRUE)
    
    road_match <- bind_rows(road_match, road_match_fuzzy) %>%
      distinct(matched_words_correct_spelling, .keep_all = TRUE)
    
    neighborhood_match <- bind_rows(neighborhood_match, neighborhood_match_fuzzy) %>%
      distinct(matched_words_correct_spelling, .keep_all = TRUE)
    
  }
  
  #### Dataframe of all locations found in tweet, appending across
  ## Add types
  landmark_match     <- landmark_match     %>% mutate(location_type = "landmark")
  road_match         <- road_match         %>% mutate(location_type = "road")
  neighborhood_match <- neighborhood_match %>% mutate(location_type = "neighborhood")
  
  ## Append
  # Don't append before as there could be cases where a landmark and road has 
  # the same name, and appending and making distict would pick one over the
  # other (?), which we deal with in a separate process.
  locations_in_tweet <- bind_rows(landmark_match, road_match, neighborhood_match)
  
  # 2. Landmarks after prepositions --------------------------------------------
  
  # When grabbing landmarks after prepositions we ignore stopwords. So for:
  # "accident near the garden city", we ignore "the" 
  text_i_no_stopwords <- text_i %>% str_replace_all("\\bthe\\b", " ") %>% str_squish
  
  ## Create vector of locations in tweets where prepositions occur
  preps_in_tweet <- phrase_in_sentence_exact(text_i_no_stopwords, prepositions_list_all)
  
  ## Locations of Prepositions
  prep_locs_df <- lapply(as.character(preps_in_tweet$matched_words_tweet_spelling), 
                         phrase_locate, 
                         text_i_no_stopwords) %>% 
    bind_rows %>%
    filter(!(word_loc_max %in% c(-Inf, Inf))) #TODO Check why getting Inf using `phrase_locate()` function
  
  prep_locs <- prep_locs_df$word_loc_max %>% unique # vector of locations of prepositions in tweet
  
  ## Extract landmarks
  locations_in_tweet_prep <- map_df(prep_locs, 
                                    extract_locations_after_words,
                                    text_i_no_stopwords,
                                    landmark_gazetteer) 
  
  ### Remove if landmark already found
  if(nrow(locations_in_tweet_prep) > 0){
    
    locations_in_tweet_prep <- locations_in_tweet_prep %>%
      
      ## Prep Variables
      dplyr::select(matched_words_tweet_spelling,
                    matched_words_correct_spelling) %>%
      mutate(exact_match = FALSE,
             location_type = "landmark") %>%
      
      ## Remove if landmark already found
      filter(!(matched_words_tweet_spelling %in% locations_in_tweet$matched_words_tweet_spelling))
    
    ## Add to main locations dataframe
    locations_in_tweet <- bind_rows(locations_in_tweet, locations_in_tweet_prep)
  }
  
  # 3. Quick Location Dataset Prep ---------------------------------------------
  
  locations_in_tweet <- locations_in_tweet[!(locations_in_tweet$location_type %in% "neighborhood"),]
  
  landmark_match <- locations_in_tweet[locations_in_tweet$location_type %in% "landmark",]
  road_match     <- locations_in_tweet[locations_in_tweet$location_type %in% "road",]
  
  road_match_sp <- roads[roads$name %in% road_match$matched_words_correct_spelling,]
  
  ## Aggregate roads so one row; makes distance calculations faster
  road_match_agg_sp <- road_match_sp
  road_match_agg_sp$id <- 1
  road_match_agg_sp <- raster::aggregate(road_match_agg_sp, by="id")
  
  # 4. Choosing which landmarks to use -----------------------------------------
  df_out <- data.frame(matrix(nrow=1,ncol=0))
  
  if(nrow(locations_in_tweet) > 0){
    
    # 4.1 Locations of Words in Tweet ------------------------------------------
    #### Locations 
    # Add location of words in tweet to locations_in_tweet dataframe
    word_locations <- lapply(as.character(locations_in_tweet$matched_words_tweet_spelling), phrase_locate, text_i) %>% bind_rows
    locations_in_tweet <- merge(locations_in_tweet, word_locations, by.x="matched_words_tweet_spelling", by.y="word")
    #locations_in_tweet_original <- locations_in_tweet
    
    ## check this issue??
    locations_in_tweet <- locations_in_tweet[!(locations_in_tweet$word_loc_min %in% c(Inf,-Inf)),]
    locations_in_tweet <- locations_in_tweet[!(locations_in_tweet$word_loc_max %in% c(Inf,-Inf)),]
    
    #### Other word types
    # Create dataframes indicating locations of (1) crash words and (2) prepositions
    # in tweets
    crash_word_locations <- lapply(crash_words, phrase_locate, text_i) %>% bind_rows
    preposition_word_locations <- lapply(prepositions_list, phrase_locate, text_i) %>% bind_rows
    
    # 4.2 Restrict Locations/Landmarks to Consider -----------------------------
    ## Remove general landmarks
    rm_gen_out <- remove_general_landmarks(landmark_match,
                                           landmark_gazetteer,
                                           road_match_sp)
    landmark_match     <- rm_gen_out$landmark_match
    landmark_gazetteer <- rm_gen_out$landmark_gazetteer
    
    ## Subset
    locations_in_tweet <- locations_in_tweet %>%
      landmark_road_overlap() %>%
      exact_fuzzy_overlap() %>%
      phase_overlap() %>%
      exact_fuzzy_startendsame()
    
    # 4.3 Find Intersections ---------------------------------------------------
    road_inter_points <- extract_intersections(locations_in_tweet, roads)
    
    # 4.4 Add Variables to Location Dataframes ---------------------------------
    # Add variables such as word location relative to prepositions and crash words
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
  }
  
  
  
}



# Algorithm ====================================================================
counter_to_display <- 1
locate_event <- function(tweet){
  
  # Choosing which landmarks to use --------------------------------------------
  df_out <- data.frame(matrix(nrow=1,ncol=0))
  
  if(nrow(locations_in_tweet) > 0){
    
    # *** ADD VARIABLES TO LOCATIONS DATAFRAME =================================
    
    # [Crash word] [tier x prepositon] [landmark] ------------------------------
    # Check if landmark fits pattern of [crash word] [preposition] [landmark]
    
    #### Determine which locations come as: crashword, prep, crashword
    crash_words_regex <- paste(paste0("\\b",crash_words,"\\b"),collapse="|")
    tier_1_prepositions_regex <- paste(tier_1_prepositions_startendword, collapse="|")
    tier_2_prepositions_regex <- paste(tier_2_prepositions_startendword, collapse="|")
    tier_3_prepositions_regex <- paste(tier_3_prepositions_startendword, collapse="|")
    
    ## Locations in Tweet
    locations_in_tweet$crashword_prepos_tier_1 <- lapply(locations_in_tweet$matched_words_tweet_spelling, function(location){
      regex_expression <- paste0(paste0("(",crash_words_regex,")."),
                                 paste0("(",tier_1_prepositions_regex,")."),
                                 location)
      out <- grepl(regex_expression, tweet)
      return(out)
    }) %>% unlist
    
    locations_in_tweet$crashword_prepos_tier_2 <- lapply(locations_in_tweet$matched_words_tweet_spelling, function(location){
      regex_expression <- paste0(paste0("(",crash_words_regex,")."),
                                 paste0("(",tier_2_prepositions_regex,")."),
                                 location)
      out <- grepl(regex_expression, tweet)
      return(out)
    }) %>% unlist
    
    locations_in_tweet$crashword_prepos_tier_3 <- lapply(locations_in_tweet$matched_words_tweet_spelling, function(location){
      regex_expression <- paste0(paste0("(",crash_words_regex,")."),
                                 paste0("(",tier_3_prepositions_regex,")."),
                                 location)
      out <- grepl(regex_expression, tweet)
      return(out)
    }) %>% unlist
    
    ## Intersections
    if(nrow(road_intersections) > 0){
      road_intersections$crashword_prepos_tier_1 <- lapply(1:nrow(road_intersections), function(i){
        road_intersections_i <- road_intersections[i,]
        
        crashword_prepos <- TRUE %in% grepl(paste0(road_intersections_i$road_tweet_spelling_1,"|",road_intersections_i$road_tweet_spelling_2) , 
                                            locations_in_tweet$matched_words_tweet_spelling[locations_in_tweet$location_type == "road" & locations_in_tweet$crashword_prepos_tier_1 == TRUE])
        return(crashword_prepos)
      }) %>% unlist
      
      road_intersections$crashword_prepos_tier_2 <- lapply(1:nrow(road_intersections), function(i){
        road_intersections_i <- road_intersections[i,]
        
        crashword_prepos <- TRUE %in% grepl(paste0(road_intersections_i$road_tweet_spelling_1,"|",road_intersections_i$road_tweet_spelling_2) , 
                                            locations_in_tweet$matched_words_tweet_spelling[locations_in_tweet$location_type == "road" & locations_in_tweet$crashword_prepos_tier_2 == TRUE])
        return(crashword_prepos)
      }) %>% unlist
      
      road_intersections$crashword_prepos_tier_3 <- lapply(1:nrow(road_intersections), function(i){
        road_intersections_i <- road_intersections[i,]
        
        crashword_prepos <- TRUE %in% grepl(paste0(road_intersections_i$road_tweet_spelling_1,"|",road_intersections_i$road_tweet_spelling_2) , 
                                            locations_in_tweet$matched_words_tweet_spelling[locations_in_tweet$location_type == "road" & locations_in_tweet$crashword_prepos_tier_3 == TRUE])
        return(crashword_prepos)
      }) %>% unlist
    }
    
    # [Crash word] [other stuff] [prepositon] [landmark] -----------------------
    # Check if landmark fits pattern of [crash word] [other stuff] [preposition] [landmark]
    # where [other stuff] are just words in between accident word and preposition.
    # Count number of words in between [other stuff] and [preposition] !!!!!!!!!!!!!!!!!!!!!!!!!!!
    
    #### Determine which locations come as: crashword, (other stuff), prep, landmark
    crash_words_regex <- paste(paste0("\\b",crash_words,"\\b"),collapse="|")
    tier_1_prepositions_regex <- paste(tier_1_prepositions_startendword, collapse="|")
    tier_2_prepositions_regex <- paste(tier_2_prepositions_startendword, collapse="|")
    tier_3_prepositions_regex <- paste(tier_3_prepositions_startendword, collapse="|")
    
    find_crashword_otherstuff_prep_landmark <- function(location, tier_prepositions_regex, crash_words_regex, tier){
      regex_expression <- paste0(paste0("(",crash_words_regex,")."),
                                 "(.*).",
                                 paste0("(",tier_prepositions_regex,")."),
                                 location)
      fits_pattern <- grepl(regex_expression, tweet)
      
      if(fits_pattern){
        # remove all text after location and before crashword [MIGHT FAIL IF >2 CRASHWORDS]
        otherwords_numwords <- tweet %>% 
          str_replace_all(paste0("(",location,").*"),"\\1") %>% 
          str_replace_all(paste0(".*(",crash_words_regex,")"),"\\1") %>% 
          str_replace_all(regex_expression,"\\2") %>% 
          str_count("\\S+")
        
      } else{
        otherwords_numwords <- NA
      }
      
      df_out <- data.frame(v1 = fits_pattern,
                           v2 = otherwords_numwords)
      names(df_out) <- c(paste0("crashword_otherstuff_prepos_tier_", tier),
                         paste0("crashword_otherstuff_prepos_tier_", tier, "_otherwords_numwords"))
      
      
      
      return(df_out)
    }
    
    crashword_otherstuff_prep_tier_1_df <- lapply(locations_in_tweet$matched_words_tweet_spelling, 
                                                  find_crashword_otherstuff_prep_landmark,
                                                  tier_1_prepositions_regex,
                                                  crash_words_regex,
                                                  1) %>% bind_rows
    
    crashword_otherstuff_prep_tier_2_df <- lapply(locations_in_tweet$matched_words_tweet_spelling, 
                                                  find_crashword_otherstuff_prep_landmark,
                                                  tier_2_prepositions_regex,
                                                  crash_words_regex,
                                                  2) %>% bind_rows
    
    crashword_otherstuff_prep_tier_3_df <- lapply(locations_in_tweet$matched_words_tweet_spelling, 
                                                  find_crashword_otherstuff_prep_landmark,
                                                  tier_3_prepositions_regex,
                                                  crash_words_regex,
                                                  3) %>% bind_rows
    
    locations_in_tweet <- cbind(locations_in_tweet, crashword_otherstuff_prep_tier_1_df)
    locations_in_tweet <- cbind(locations_in_tweet, crashword_otherstuff_prep_tier_2_df)
    locations_in_tweet <- cbind(locations_in_tweet, crashword_otherstuff_prep_tier_3_df)
    
    # Preposition in front of crashword ----------------------------------------
    locations_in_tweet$tier_1_prepos_before_crashword <- lapply(locations_in_tweet$matched_words_tweet_spelling, function(location){
      regex_expression <- paste0(paste0("(",tier_1_prepositions_regex,")."),
                                 location)
      out <- grepl(regex_expression, tweet)
      return(out)
    }) %>% unlist
    
    locations_in_tweet$tier_2_prepos_before_crashword <- lapply(locations_in_tweet$matched_words_tweet_spelling, function(location){
      regex_expression <- paste0(paste0("(",tier_2_prepositions_regex,")."),
                                 location)
      out <- grepl(regex_expression, tweet)
      return(out)
    }) %>% unlist
    
    locations_in_tweet$tier_3_prepos_before_crashword <- lapply(locations_in_tweet$matched_words_tweet_spelling, function(location){
      regex_expression <- paste0(paste0("(",tier_3_prepositions_regex,")."),
                                 location)
      out <- grepl(regex_expression, tweet)
      return(out)
    }) %>% unlist
    
    # Distance to Crashword ----------------------------------------------------
    # If no landmark fits pattern of [crashword] [preposition] [landmark],
    # calculate distance of each landmark to closest crash word
    if(grepl(paste(paste0("\\b", crash_words,"\\b"), collapse = "|"), tweet) == TRUE){
      
      #### Add distance to crash word
      loc_crash_distance <- lapply(crash_word_locations$word_loc_min, function(crash_word_loc_i){
        location_dist_to_crash <- locations_in_tweet$word_loc_min - crash_word_loc_i
        return(location_dist_to_crash)
      }) %>% bind_cols
      
      loc_crash_distance_choose <- apply(abs(loc_crash_distance), 1, which.min)
      
      locations_in_tweet$dist_closest_crash_word <- lapply(1:length(loc_crash_distance_choose), function(i) as.numeric(loc_crash_distance[i,loc_crash_distance_choose[i]])) %>% unlist
      locations_in_tweet$comes_after_crash_word <-  locations_in_tweet$dist_closest_crash_word > 0
    }
    
    # If road is very short, make a landmark -----------------------------------
    if(T){
      if("road" %in% locations_in_tweet$location_type){
        
        # Add length of roads
        road_locations <- locations_in_tweet[locations_in_tweet$location_type %in% "road",]
        
        road_lengths <- lapply(1:nrow(road_locations), function(i){
          #road_i_length <- gLength(roads[roads$name %in% road_locations$matched_words_correct_spelling[i],])*111.12
          
          road_extent_i <- extent(roads[roads$name %in% road_locations$matched_words_correct_spelling[i],])
          road_i_length <- sqrt((road_extent_i@xmin - road_extent_i@xmax)^2 + (road_extent_i@ymin - road_extent_i@ymax)^2)*111.12
          
          return(road_i_length)
        }) %>% unlist
        
        locations_in_tweet$length <- 0
        locations_in_tweet$length[locations_in_tweet$location_type %in% "road"] <- road_lengths
        
        ### If road length is small
        # Make landmark
        # Add to gazetteer
        road_locations_short <- locations_in_tweet[(locations_in_tweet$location_type %in% "road") & 
                                                     (locations_in_tweet$length < 0.75),]
        if(nrow(road_locations_short) > 0){
          road_locations_short$location_type <- "landmark"
          locations_in_tweet <- bind_rows(locations_in_tweet, road_locations_short)
          
          road_locations_short_center <- lapply(1:nrow(road_locations_short), function(i){
            road_i <- roads[roads$name %in% road_locations_short$matched_words_correct_spelling[i],]
            
            road_center_df <- gCentroid(road_i) %>% as.data.frame %>%
              dplyr::rename(lon = x) %>%
              dplyr::rename(lat = y) %>%
              mutate(name = road_locations_short$matched_words_correct_spelling[i]) %>%
              #mutate(type = "short_road") %>%
              mutate(type = "landmark") %>%
              mutate(source = "osm")
          }) %>% bind_rows
          
          landmark_gazetteer <- bind_rows(landmark_gazetteer, road_locations_short_center)
          landmark_gazetteer <- landmark_gazetteer[!is.na(landmark_gazetteer$lat),]
          
        }
        
      }
    }
    
    # *** FUNCTIONS USES FOR FINAL DECISION MAKING =============================
    intersection_words <- c("intersection", "junction")
    intersection_words_regex <- paste0("\\b", intersection_words, "\\b") %>% paste(collapse = "|")
    
    locations_in_tweet <- locations_in_tweet %>% unique
    neighborhoods_final <- locations_in_tweet[locations_in_tweet$location_type == "neighborhood",] %>% unique
    roads_final <- locations_in_tweet[locations_in_tweet$location_type == "road",] %>% unique
    landmarks_final <- locations_in_tweet[locations_in_tweet$location_type == "landmark",] %>% unique
    road_intersections_final <- road_intersections %>% unique
    
    #### Function for strategy of dealing with multiple landmarks (different names)
    choose_between_multiple_landmarks <- function(df_out){
      
      # 1. Restrict landmarks based on preposition tiers
      if(TRUE %in% df_out$tier_1_prepos_before_crashword){
        df_out <- df_out[df_out$tier_1_prepos_before_crashword %in% TRUE,]
        df_out$how_determined_landmark <- paste(df_out$how_determined_landmark, "restrict_to_tier1_prepositions", sep=";")
      } else if(TRUE %in% df_out$tier_2_prepos_before_crashword){
        df_out <- df_out[df_out$tier_2_prepos_before_crashword %in% TRUE,]
        df_out$how_determined_landmark <- paste(df_out$how_determined_landmark, "restrict_to_tier2_prepositions", sep=";")
      } else if(TRUE %in% df_out$tier_3_prepos_before_crashword){
        df_out <- df_out[df_out$tier_3_prepos_before_crashword %in% TRUE,]
        df_out$how_determined_landmark <- paste(df_out$how_determined_landmark, "restrict_to_tier3_prepositions", sep=";")
      }
      
      # 2. If road mentioned, restrict to landmarks near road
      if(nrow(roads_final) > 0){
        df_out_sp <- df_out
        coordinates(df_out_sp) <- ~lon+lat
        crs(df_out_sp) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
        
        roads_in_tweet <- roads[roads$name %in% roads_final$matched_words_correct_spelling,]
        roads_in_tweet$id <- 1
        roads_in_tweet <- raster::aggregate(roads_in_tweet, by="id")
        df_out_sp$distance_road <- as.numeric(gDistance(roads_in_tweet, df_out_sp, byid=T)) * 111.12
        df_out_sp <- df_out_sp[df_out_sp$distance_road < 0.5,]
        
        if(nrow(df_out_sp) > 0){
          df_out <- df_out[df_out$matched_words_correct_spelling %in% df_out_sp$matched_words_correct_spelling,]
          df_out$how_determined_landmark <- paste(df_out$how_determined_landmark, "multiple_landmarks_restrict_to_near_roads", sep=";")
        } else{
          df_out$how_determined_landmark <- paste(df_out$how_determined_landmark, "multiple_landmarks_tried_restrict_to_near_roads_but_none_near_road", sep=";")
        }
      }
      
      # 3. Use landmark closest to crash word. 
      # if there are two words, one with distance -2 and one with distance 2, which.min()
      # will just give the first one. Consequently, use another approach to grab both.
      # Only use if a crash word exists in tweet.
      if(is.null(df_out$dist_closest_crash_word) %in% FALSE){
        landmark_closest_crashword <- df_out$matched_words_correct_spelling[abs(df_out$dist_closest_crash_word) %in% min(abs(df_out$dist_closest_crash_word))] %>% unique()
        df_out <- df_out[df_out$matched_words_correct_spelling %in% landmark_closest_crashword,]
        df_out$how_determined_landmark <- paste(df_out$how_determined_landmark, "multiple_landmarks_choose_closest_crashword", sep=";")
      } 
      
      return(df_out)
    }
    
    
    #### Function for strategy of dealing with landmarks with same name, diff loc
    choose_between_landmark_same_name <- function(df_out){
      
      # 1. If multiple landmarks with same name and a road name, restrict to
      #    ones close to any road that is mentioned
      if(nrow(roads_final) >= 1){
        # Spatial dataframe of landmark candidates
        df_out_sp <- df_out
        coordinates(df_out_sp) <- ~lon+lat
        crs(df_out_sp) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
        
        # Road shapefile of roads in tweet
        roads_in_tweet <- roads[roads$name %in% roads_final$matched_words_correct_spelling,]
        roads_in_tweet$id <- 1
        roads_in_tweet <- raster::aggregate(roads_in_tweet, by="id")
        
        df_out$distance_road_in_tweet <- as.numeric(gDistance(df_out_sp, roads_in_tweet, byid=T)) * 111.12
        
        if(TRUE %in% (df_out$distance_road_in_tweet < 0.5)){
          df_out <- df_out[df_out$distance_road_in_tweet < 0.5,]
          df_out$how_determined_landmark <- paste(df_out$how_determined_landmark, "restrict_landmarks_close_to_road", sep=";")
        } else{
          df_out$how_determined_landmark <- paste(df_out$how_determined_landmark, "tried_restricting_landmarks_close_to_road_but_none_close", sep=";")
        }
        
      }
      
      # TODO --- If lower tier preposition, restrict to ones close to those !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      
      # 2. If multiple landmarks with same name, use one if bus station
      if(TRUE %in% grepl("bus_station|transit_station|stage_added", df_out$type)){
        df_out <- df_out[grepl("bus_station|transit_station|stage_added", df_out$type),]
        df_out$how_determined_landmark <- paste(df_out$how_determined_landmark, "choose_bus_station", sep=";")
      } 
      
      # 3. If multiple landmarks with same name, use one if mall
      if(TRUE %in% grepl("shopping_mall", df_out$type)){
        df_out <- df_out[grepl("shopping_mall", df_out$type),]
        df_out$how_determined_landmark <- paste(df_out$how_determined_landmark, "choose_shopping_mall", sep=";")
      } 
      
      # 3. If multiple landmarks with same name, use one with more types 
      nrow_before <- nrow(df_out)
      df_out <- df_out[stri_count(df_out$type, fixed = ";") %in% max(stri_count(df_out$type, fixed = ";")),]
      nrow_after <- nrow(df_out)
      if(nrow_before > nrow_after) df_out$how_determined_landmark <- paste(df_out$how_determined_landmark, "choose_landmark_more_types", sep=";")
      
      return(df_out)
    }
    
    #### Function for snapping landmark to road
    snap_landmark_to_road <- function(df_out, roads_final){
      df_out_sp <- df_out
      coordinates(df_out_sp) <- ~lon+lat
      crs(df_out_sp) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
      
      roads_i <- roads[roads$name %in% roads_final$matched_words_correct_spelling,]
      
      if((gDistance(df_out_sp, roads_i) * 111.12) < 0.5){
        df_out_sp_snap <- snapPointsToLines(df_out_sp, 
                                            as(roads_i, "SpatialLinesDataFrame"), 
                                            maxDist=1, withAttrs = F, idField=NA)
        df_out_sp_snap@data <- df_out_sp@data
        
        df_out_sp_snap <- as.data.frame(df_out_sp_snap) %>%
          dplyr::rename(lon = X) %>%
          dplyr::rename(lat = Y)
        df_out_sp_snap$how_determined_landmark <- paste(df_out_sp_snap$how_determined_landmark, "snapped_to_road", sep=";")
      } else{
        df_out_sp_snap <- df_out
        df_out_sp_snap$how_determined_landmark <- paste(df_out_sp_snap$how_determined_landmark, "tried_to_snapped_to_road_but_road_too_far", sep=";")
      }
      
      return(df_out_sp_snap)
      
    }
    
    #### Find other landmarks that might be near road
    find_landmark_similar_name_close_to_road <- function(df_out, roads_final){
      # Find other landmarks with similar name as landmarks in df_out that might
      # be near the road. Here, we start with the landmark names in df_out. If
      # they are far (more than 500 meters) from the mentioned road, this might
      # suggest that we have the incorrect landmark; the correct location is
      # probably near the mentioned road. Consequently, we broaden our landmark
      # search. This involves:
      # 1. Check whether landmark names are part of *any* part of gazetteer entries
      # 2. Of above landmarks, checks whether they are within 100 meters of mentioned road
      # 3. If more than one landmark found, check if all close together
      # 4. If the above conditions don't hold, we stay with the original landmarks
      
      df_out_sp <- df_out
      coordinates(df_out_sp) <- ~lon+lat
      crs(df_out_sp) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
      
      roads_i <- roads[roads$name %in% roads_final$matched_words_correct_spelling,]
      
      if((gDistance(df_out_sp, roads_i) * 111.12) >= 0.5){
        
        #regex_search <- paste0("^", unique(df_out$matched_words_correct_spelling), "\\b") %>% paste(collapse="|")
        regex_search <- paste0("\\b", unique(df_out$matched_words_correct_spelling), "\\b") %>% paste(collapse="|")
        
        landmark_gazetteer_subset <- landmark_gazetteer[grepl(regex_search,landmark_gazetteer$name),]
        coordinates(landmark_gazetteer_subset) <- ~lon+lat
        crs(landmark_gazetteer_subset) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
        
        roads_i$id <- 1
        roads_i <- aggregate(roads_i, by="id")
        
        landmark_gazetteer_subset$distance_road <- as.numeric(gDistance(landmark_gazetteer_subset, roads_i, byid=T) * 111.12)
        landmark_gazetteer_subset <- landmark_gazetteer_subset[landmark_gazetteer_subset$distance_road <= 0.1,]
        
        ##### If multiple close by, use one where mentioned word is at start of landmark
        # If none start a landmark, don't subset.
        # E.g., "accident at dbt mombasa rd" , following found: "dbt center" and "moneygram at dbt"; choose "dbt center"
        regex_search_startstring <- paste0("^", unique(df_out$matched_words_correct_spelling), "\\b") %>% paste(collapse="|")
        landmark_gazetteer_subset_TEMP <- landmark_gazetteer_subset[grepl(regex_search_startstring,landmark_gazetteer_subset$name),]
        if(nrow(landmark_gazetteer_subset_TEMP) >= 1) landmark_gazetteer_subset <- landmark_gazetteer_subset_TEMP
        
        if(nrow(landmark_gazetteer_subset) >= 1){
          landmark_gazetteer_subset <- as.data.frame(landmark_gazetteer_subset)
          
          ## Maximum distance between coordinates
          lat_min <- min(landmark_gazetteer_subset$lat)
          lat_max <- max(landmark_gazetteer_subset$lat)
          lon_min <- min(landmark_gazetteer_subset$lon)
          lon_max <- max(landmark_gazetteer_subset$lon)
          max_dist <- sqrt((lat_max - lat_min)^2 + (lon_max - lon_min)^2)*111.12
          
          # SEE IF THERE IS A DOMINANT CLUSTER HERE !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! TODO
          
          ## If coordinates close
          if(max_dist <= 0.5){
            lat_mean <- mean(landmark_gazetteer_subset$lat)
            lon_mean <- mean(landmark_gazetteer_subset$lon)
            
            df_out <- df_out[1,]
            
            df_out$lat <- lat_mean
            df_out$lon <- lon_mean
            df_out$matched_words_correct_spelling <- paste(c(unique(df_out$matched_words_correct_spelling),unique(landmark_gazetteer_subset$name)), collapse=";")
            df_out$how_determined_landmark <- paste(df_out$how_determined_landmark, "broadended_landmark_search_found_landmarks_similar_name_near_road", sep=";")
          }
        }
        
      }
      
      return(df_out)
      
    }
    
    # *** GRAB RELEVANT COORDINATES ============================================
    if(TRUE %in% landmarks_final$crashword_prepos_tier_1){ 
      df_out <- landmarks_final[landmarks_final$crashword_prepos_tier_1 %in% TRUE,]
      df_out <- subset(df_out, select=c(matched_words_tweet_spelling, matched_words_correct_spelling)) %>% unique
      df_out <- merge(df_out, landmark_gazetteer, by.x="matched_words_correct_spelling", by.y="name", all.x=T, all.y=F)
      
      df_out$how_determined_landmark <- "crashword_tier_1_preposition_landmark"
      
      if(length(unique(df_out$matched_words_correct_spelling)) > 1) df_out <- choose_between_multiple_landmarks(df_out)
      if(nrow(df_out) > 1) df_out <- choose_between_landmark_same_name(df_out)
      if(nrow(roads_final) %in% 1) df_out <- find_landmark_similar_name_close_to_road(df_out, roads_final)
      if(nrow(roads_final) %in% 1) df_out <- snap_landmark_to_road(df_out, roads_final)
      
      df_out$type <- "landmark"
      
      df_out <- subset(df_out, select=c(lon, lat, matched_words_correct_spelling, matched_words_tweet_spelling, type, how_determined_landmark))
      
    } else if(TRUE %in% landmarks_final$crashword_prepos_tier_2){
      df_out <- landmarks_final[landmarks_final$crashword_prepos_tier_2 %in% TRUE,]
      df_out <- subset(df_out, select=c(matched_words_tweet_spelling, matched_words_correct_spelling)) %>% unique
      df_out <- merge(df_out, landmark_gazetteer, by.x="matched_words_correct_spelling", by.y="name", all.x=T, all.y=F)
      
      df_out$how_determined_landmark <- "crashword_tier_2_preposition_landmark"
      
      if(length(unique(df_out$matched_words_correct_spelling)) > 1) df_out <- choose_between_multiple_landmarks(df_out)
      if(nrow(df_out) > 1) df_out <- choose_between_landmark_same_name(df_out)
      if(nrow(roads_final) %in% 1) df_out <- find_landmark_similar_name_close_to_road(df_out, roads_final)
      if(nrow(roads_final) %in% 1) df_out <- snap_landmark_to_road(df_out, roads_final)
      
      df_out$type <- "landmark"
      
      df_out <- subset(df_out, select=c(lon, lat, matched_words_correct_spelling, matched_words_tweet_spelling, type, how_determined_landmark))
      
    } else if(TRUE %in% road_intersections_final$crashword_prepos_tier_1){
      df_out <- road_intersections_final[road_intersections_final$crashword_prepos_tier_1 %in% TRUE,]
      
      df_out$matched_words_correct_spelling <- paste(df_out$road_correct_spelling_1, df_out$road_correct_spelling_2, sep=",")
      df_out$matched_words_tweet_spelling <- paste(df_out$road_tweet_spelling_1, df_out$road_tweet_spelling_2, sep=",")
      
      df_out <- subset(df_out, select=c(lon, lat, matched_words_correct_spelling, matched_words_tweet_spelling))
      df_out$type <- "intersection"
      df_out$how_determined_landmark <- "crashword_preposition_intersection"
    } else if(TRUE %in% (landmarks_final$crashword_otherstuff_prepos_tier_1_otherwords_numwords <= 5)){
      df_out <- landmarks_final[landmarks_final$crashword_otherstuff_prepos_tier_1 %in% TRUE,]
      df_out <- df_out[df_out$crashword_otherstuff_prepos_tier_1_otherwords_numwords <= 5,]
      df_out <- merge(df_out, landmark_gazetteer, by.x="matched_words_correct_spelling", by.y="name", all.x=T, all.y=F)
      
      #df_out <- subset(df_out, select=c(matched_words_tweet_spelling, matched_words_correct_spelling)) %>% unique
      #df_out <- merge(df_out, landmark_gazetteer, by.x="matched_words_correct_spelling", by.y="name", all.x=T, all.y=F)
      
      
      df_out$how_determined_landmark <- "crashword_otherstuff_prepos_tier_1"
      
      if(length(unique(df_out$matched_words_correct_spelling)) > 1) df_out <- choose_between_multiple_landmarks(df_out)
      if(nrow(df_out) > 1) df_out <- choose_between_landmark_same_name(df_out)
      if(nrow(roads_final) %in% 1) df_out <- find_landmark_similar_name_close_to_road(df_out, roads_final)
      if(nrow(roads_final) %in% 1) df_out <- snap_landmark_to_road(df_out, roads_final)
      
      df_out$type <- "landmark"
      df_out <- subset(df_out, select=c(lon, lat, matched_words_correct_spelling, matched_words_tweet_spelling, type, how_determined_landmark))
      
    } else if(grepl(intersection_words_regex, tweet) & nrow(road_intersections_final) > 0){
      df_out <- road_intersections_final
      
      df_out$matched_words_correct_spelling <- paste(df_out$road_correct_spelling_1, df_out$road_correct_spelling_2, sep=",")
      df_out$matched_words_tweet_spelling <- paste(df_out$road_tweet_spelling_1, df_out$road_tweet_spelling_2, sep=",")
      
      df_out <- subset(df_out, select=c(lon, lat, matched_words_correct_spelling, matched_words_tweet_spelling))
      df_out$type <- "intersection"
      df_out$how_determined_landmark <- "crashword_preposition_intersection"
    } else if(nrow(road_intersections_final) == 1){
      df_out <- road_intersections_final
      
      df_out$matched_words_correct_spelling <- paste(df_out$road_correct_spelling_1, df_out$road_correct_spelling_2, sep=",")
      df_out$matched_words_tweet_spelling <- paste(df_out$road_tweet_spelling_1, df_out$road_tweet_spelling_2, sep=",")
      
      df_out <- subset(df_out, select=c(lon, lat, matched_words_correct_spelling, matched_words_tweet_spelling))
      df_out$type <- "intersection"
      df_out$how_determined_landmark <- "crashword_preposition_intersection"
    } else if(nrow(landmarks_final) > 0){
      
      df_out <- landmarks_final
      df_out <- merge(df_out, landmark_gazetteer, by.x="matched_words_correct_spelling", by.y="name", all.x=T, all.y=F)
      
      df_out$how_determined_landmark <- "landmark_ambiguous_pattern"
      
      if(length(unique(df_out$matched_words_correct_spelling)) > 1) df_out <- choose_between_multiple_landmarks(df_out)
      if(nrow(df_out) > 1) df_out <- choose_between_landmark_same_name(df_out)
      if(nrow(roads_final) %in% 1) df_out <- find_landmark_similar_name_close_to_road(df_out, roads_final)
      if(nrow(roads_final) %in% 1) df_out <- snap_landmark_to_road(df_out, roads_final)
      
      df_out <- subset(df_out, select=c(lon, lat, matched_words_correct_spelling, matched_words_tweet_spelling, how_determined_landmark))
      df_out$type <- "landmark"
    }
    
    #### Account for whether df_out is more than one row
    # If coordiantes are within X distance, average; otherwise, can't geocode
    if(nrow(df_out) > 1){
      dominant_cluster <- 0.75
      
      df_out_orig <- df_out
      
      df_out_sdf <- df_out
      coordinates(df_out_sdf) <- ~lon+lat
      crs(df_out_sdf) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
      distances <- gDistance(df_out_sdf, byid=T) * 111.12
      
      distances_percentile <- apply(distances, 1, quantile, probs=(1-dominant_cluster))
      cluster_exists <- mean(distances_percentile < 0.5) >= dominant_cluster
      
      if(cluster_exists == TRUE){
        
        df_out <- df_out[distances_percentile < 0.5,]
        
        df_out <- data.frame(
          lon = df_out$lon %>% mean(),
          lat = df_out$lat %>% mean(),
          matched_words_correct_spelling = df_out$matched_words_correct_spelling %>% paste(collapse=","),
          matched_words_tweet_spelling = df_out$matched_words_tweet_spelling %>% paste(collapse=","),
          type = df_out$type %>% paste(collapse=","))
        
        if(!is.null(df_out_orig$dist_closest_crash_word)) df_out$dist_closest_crash_word <- df_out_orig$dist_closest_crash_word %>% unique %>% paste(collapse=",")
        df_out$how_determined_landmark <- df_out_orig$how_determined_landmark %>% unique %>% paste(collapse=",")
        df_out$how_determined_landmark <- paste(df_out$how_determined_landmark, "multiple_landmarks_same_name_diff_loc_avg_coords", sep=";")
        df_out$multiple_landmarks_same_name_diff_loc_avg_coords_max_dist <- max(distances)
        df_out$tweet_clean <- tweet
      } else{
        
        df_out <- data.frame(
          matched_words_correct_spelling = df_out$matched_words_correct_spelling %>% paste(collapse=","),
          matched_words_tweet_spelling = df_out$matched_words_tweet_spelling %>% paste(collapse=","),
          type = df_out$type %>% paste(collapse=","))
        
        if(!is.null(df_out_orig$dist_closest_crash_word)) df_out$dist_closest_crash_word <- df_out_orig$dist_closest_crash_word %>% unique %>% paste(collapse=",")
        df_out$how_determined_landmark <- df_out_orig$how_determined_landmark %>% unique %>% paste(collapse=",")
        df_out$how_determined_landmark <- paste(df_out$how_determined_landmark, "multiple_landmarks_same_name_diff_loc_avg_coords_far_apart", sep=";")
        df_out$multiple_landmarks_same_name_diff_loc_avg_coords_max_dist <- max(distances)
        df_out$tweet_clean <- tweet
      }
    }
    
    #### If roads mentioned, check if location is near road; if not, no lat/lon (probably false positive)
    if(nrow(roads_final) > 0 & !is.null(df_out$lat)){
      if(TRUE %in% !is.na(df_out$lat)){
        df_out_sp <- df_out
        coordinates(df_out_sp) <- ~lon+lat
        crs(df_out_sp) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
        
        roads_in_tweet <- roads[roads$name %in% roads_final$matched_words_correct_spelling,]
        roads_in_tweet$id <- 1
        roads_in_tweet <- raster::aggregate(roads_in_tweet, by="id")
        df_out_sp$distance_road <- as.numeric(gDistance(roads_in_tweet, df_out_sp, byid=T)) * 111.12
        df_out_sp <- df_out_sp[df_out_sp$distance_road < 0.5,]
        
        if(min(df_out_sp$distance_road) > 0.5){
          #df_out$lat <- NA
          #df_out$lon <- NA
          df_out$how_determined_landmark <- paste(df_out$how_determined_landmark, "location_not_near_mentioned_roads_so_make_NA", sep=";")
        }
      }
    }
    
    #### Check if road should be used as crash location
    # Check if only one road found
    if(nrow(roads_final) %in% 1){
      
      # Create spatial dataframe of road
      roads_final_sp <- merge(roads, roads_final, by.x="name", by.y="matched_words_correct_spelling", all.x=F)
      
      # Only use if none of road segments is ambiguous
      if(!(TRUE %in% roads_final_sp$ambiguous_road)){
        
        # Use road if df_out is blank
        if(ncol(df_out) %in% 0){
          roads_final_sp$id <- 1
          df_out <- raster::aggregate(roads_final_sp, by="id")
          df_out <- st_as_sf(df_out)
        }
        
      }
      
    }
    
    # *** CLEAN OUTPUT =========================================================
    # 1. Add all location types found
    # 2. Add tweet
    if("landmark" %in% locations_in_tweet_original$location_type){
      df_out$landmarks_all_tweet_spelling <- locations_in_tweet_original$matched_words_tweet_spelling[locations_in_tweet_original$location_type %in% "landmark"] %>% unique %>% paste(collapse=",")
      df_out$landmarks_all_correct_spelling <- locations_in_tweet_original$matched_words_correct_spelling[locations_in_tweet_original$location_type %in% "landmark"] %>% unique %>% paste(collapse=",")
    }
    
    if("road" %in% locations_in_tweet_original$location_type){
      df_out$roads_all_tweet_spelling <- locations_in_tweet_original$matched_words_tweet_spelling[locations_in_tweet_original$location_type %in% "road"] %>% unique %>% paste(collapse=",")
      df_out$roads_all_correct_spelling <- locations_in_tweet_original$matched_words_correct_spelling[locations_in_tweet_original$location_type %in% "road"] %>% unique %>% paste(collapse=",")
    }
    
    if("neighborhood" %in% locations_in_tweet_original$location_type){
      df_out$neighborhoods_all_tweet_spelling <- locations_in_tweet_original$matched_words_tweet_spelling[locations_in_tweet_original$location_type %in% "neighborhood"] %>% unique %>% paste(collapse=",")
      df_out$neighborhoods_all_correct_spelling <- locations_in_tweet_original$matched_words_correct_spelling[locations_in_tweet_original$location_type %in% "neighborhood"] %>% unique %>% paste(collapse=",")
    }
  }
  
  df_out$tweet_clean <- tweet
  if(!is.null(df_out$dist_closest_crash_word)) df_out$dist_closest_crash_word <- as.character(df_out$dist_closest_crash_word)
  
  #### If output is point, make spatial point
  if(!is.null(df_out$lat)){
    if(!is.na(df_out$lat)){
      coordinates(df_out) <- ~lon+lat
      crs(df_out) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
      df_out <- st_as_sf(df_out)
    }
  }
  
  #### If output doesn't have a geometry, give a null geometry
  if(is.null(df_out$geometry)) df_out <- st_sf(df_out, geom = st_sfc(st_point()))
  
  counter_to_display <<- counter_to_display + 1
  print(counter_to_display)
  
  return(df_out)
}

