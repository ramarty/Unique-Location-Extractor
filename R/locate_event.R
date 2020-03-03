# Crashmap Algorithm

# 1. Search for other landmarks after preopostions, ignoring certain stopwords (the)
# if there are prepositions where landmark hasn't been found.
   # a. Until no matches
   # a. Check for dominant cluster?
   # a. And check if priprity category... accident at [landmark] but can't find landmark,
   # so no geocode.

# Packages ---------------------------------------------------------------------
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
#library(doBy)
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
  
  tweet <- "crash near airtel on mombasa rd words words words yaya center"
  
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

phrase_in_sentence_fuzzy <- function(sentence, 
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
    
  } else{
    locations_df <- data.frame(NULL)
  }
  
  return(locations_df)
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

# Algorithm ====================================================================
counter_to_display <- 1
locate_event <- function(tweet,
                         crash_words,
                         junction_words,
                         tier_1_prepositions,
                         tier_2_prepositions,
                         tier_3_prepositions,
                         false_positive_phrases,
                         first_letters_same,
                         last_letters_same,
                         fuzzy_match_landmark,
                         fuzzy_match_landmark.min.word.length,
                         fuzzy_match_landmark.dist,
                         fuzzy_match_ngram_max,
                         landmark_gazetteer,
                         roads,
                         neighborhoods){
  
  print(tweet)
  
  # Checks ---------------------------------------------------------------------
  if(length(fuzzy_match_landmark.min.word.length) != length(fuzzy_match_landmark.dist)){
    stop("fuzzy_match_landmark.min.word.length and fuzzy_match_landmark.dist must be the same length")
  }
  
  # Load Location Files --------------------------------------------------------
  #if(import_files){
  #  landmark_gazetteer <- readRDS(file.path(project_file_path, "Twitter Geocode Algorithm", "data", "finaldata", "gazetteers_augmented", "gazetteer_aug.Rds"))
  #  roads <- readRDS(file.path(project_file_path, "Twitter Geocode Algorithm", "data", "finaldata", "roads_augmented", "osm_roads_aug.Rds"))
  #  neighborhoods <- readRDS(file.path(project_file_path, "Twitter Geocode Algorithm", "data", "finaldata", "nairobi_estates", "nairobi_estates.Rds"))
  #} else{
  #  landmark_gazetteer <- landmark_gazetteer_orig
  #}

  # Extra Cleaning
  landmark_gazetteer$name <- landmark_gazetteer$name %>% str_replace_all("[[:punct:]]", "")
  roads$name <- roads$name %>% as.character %>% tolower
  
  # Names into Lists
  landmark_list <- landmark_gazetteer$name
  roads_list <- roads$name %>% as.character %>% tolower
  neighborhoods_list <- neighborhoods$estate

  # Clean Prepositions List ----------------------------------------------------
  tier_1_prepositions_startendword <- paste0("\\b",tier_1_prepositions,"\\b")
  tier_2_prepositions_startendword <- paste0("\\b",tier_2_prepositions,"\\b")
  tier_3_prepositions_startendword <- paste0("\\b",tier_3_prepositions,"\\b")
  
  prepositions_list <- c(tier_1_prepositions, tier_2_prepositions)
  
  # Clean Tweet ------------------------------------------------------------------
  tweet <- iconv(tweet, "latin1", "ASCII", sub="")

  #### Initial Cleaning
  tweet <- tweet %>%
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
    str_replace_all("\\bamp\\b", "and") 
  
  #### Remove false positive phrases
  for(phrase in false_positive_phrases){
    # Replace false positive phrases with "blankword", where blankword appears the
    # number of times for each word in the phrase. For example, "githurai bus"
    # becomes "blankword blankword" and "githurai 45 bus" becomes "blankword
    # blankword blankword". Replacing with "bankword" is important as it preserves
    # the number of words between different words -- which is used later in the algorithm.
    tweet <- tweet %>% str_replace_all(paste0("\\b",phrase,"\\b"), 
                                       rep("bankword", wordcount(phrase)) %>% paste(collapse=" "))
  }
  
  tweet <- tweet %>%
  
    # Replace Accronmys
    # TODO: Make this a separate file csv file that gets used
    str_replace_all("\\bpri sch\\b", "primary school") %>%
    str_replace_all("\\bsec sch\\b", "secondary school") %>%
    
    # Squish strings. Should be last thing done
    str_squish
  
  # Locations in Tweet -----------------------------------------------------------
  #### Exact Match
  landmark_match <- phrase_in_sentence_exact(tweet, landmark_list) 
  road_match <- phrase_in_sentence_exact(tweet, roads_list)
  neighborhood_match <- phrase_in_sentence_exact(tweet, neighborhoods_list)
  
  #### Fuzzy Match
  if(fuzzy_match_landmark == TRUE){
    
    # Landmarks
    landmark_match_fuzzy <- lapply(1:length(fuzzy_match_landmark.min.word.length), function(i){
      landmark_match_fuzzy_i <- phrase_in_sentence_fuzzy(tweet, 
                                                       landmark_list,
                                                       fuzzy_match_landmark.min.word.length[i], 
                                                       fuzzy_match_landmark.dist[i], 
                                                       fuzzy_match_ngram_max,
                                                       first_letters_same,
                                                       last_letters_same)
      return(landmark_match_fuzzy_i)
    }) %>% 
      bind_rows %>% 
      unique
    
    # Roads
    road_match_fuzzy <- lapply(1:length(fuzzy_match_landmark.min.word.length), function(i){
      road_match_fuzzy_i <- phrase_in_sentence_fuzzy(tweet, 
                                                     roads_list,
                                                     fuzzy_match_landmark.min.word.length[i], 
                                                     fuzzy_match_landmark.dist[i], 
                                                     fuzzy_match_ngram_max,
                                                     first_letters_same,
                                                     last_letters_same)
      return(road_match_fuzzy_i)
    }) %>% 
      bind_rows %>% 
      unique
    
    # Neighborhoods
    neighborhood_match_fuzzy <- lapply(1:length(fuzzy_match_landmark.min.word.length), function(i){
      neighborhood_match_fuzzy_i <- phrase_in_sentence_fuzzy(tweet, 
                                                             neighborhoods_list,
                                                     fuzzy_match_landmark.min.word.length[i], 
                                                     fuzzy_match_landmark.dist[i], 
                                                     fuzzy_match_ngram_max,
                                                     first_letters_same,
                                                     last_letters_same)
      return(neighborhood_match_fuzzy_i)
    }) %>% 
      bind_rows %>% 
      unique
    
    #### Remove fuzzy match if:
    # (1) tweet spelling is one word and
    # (2) tweet spelling is correctly spelled
    if(nrow(landmark_match_fuzzy) > 0) landmark_match_fuzzy <- landmark_match_fuzzy[!((str_count(landmark_match_fuzzy$matched_words_tweet_spelling, "\\S+") %in% 1) & hunspell_check(as.character(landmark_match_fuzzy$matched_words_tweet_spelling))),]
    if(nrow(road_match_fuzzy) > 0) road_match_fuzzy <- road_match_fuzzy[!((str_count(road_match_fuzzy$matched_words_tweet_spelling, "\\S+") %in% 1) & hunspell_check(as.character(road_match_fuzzy$matched_words_tweet_spelling))),]
    if(nrow(neighborhood_match_fuzzy) > 0) neighborhood_match_fuzzy <- neighborhood_match_fuzzy[!((str_count(neighborhood_match_fuzzy$matched_words_tweet_spelling, "\\S+") %in% 1) & hunspell_check(as.character(neighborhood_match_fuzzy$matched_words_tweet_spelling))),]
    
    #### Add fuzzy match to full match list
    landmark_match <- bind_rows(landmark_match, landmark_match_fuzzy) %>%
      distinct(matched_words_correct_spelling, .keep_all = TRUE)
    
    road_match <- bind_rows(road_match, road_match_fuzzy) %>%
      distinct(matched_words_correct_spelling, .keep_all = TRUE)
    
    neighborhood_match <- bind_rows(neighborhood_match, neighborhood_match_fuzzy) %>%
      distinct(matched_words_correct_spelling, .keep_all = TRUE)
  }
  
  #### Create dataframe of all locations found in tweet, appending across
  # datasets of different location types -- landmarks, roads and neighborhoods.
  if(nrow(landmark_match) > 0) landmark_match$location_type <- "landmark"
  if(nrow(road_match) > 0) road_match$location_type <- "road"
  if(nrow(neighborhood_match) > 0) neighborhood_match$location_type <- "neighborhood"
  locations_in_tweet <- bind_rows(landmark_match, road_match, neighborhood_match)
  
  # Grab Landmarks After Prepositions ------------------------------------------
  #### When grabbing landmarks after prepositions we ignore stopwords. So we consider:
  # "accident near the garden city"
  tweet_no_stopwords <- tweet %>% str_replace_all("\\bthe\\b", " ") %>% str_squish
  
  #### Create vector of locations in tweets where prepositions occur
  prepositions_in_tweet <- phrase_in_sentence_exact(tweet_no_stopwords, c(tier_1_prepositions, tier_2_prepositions, tier_3_prepositions))
  prepositions_locations <- lapply(as.character(prepositions_in_tweet$matched_words_tweet_spelling), phrase_locate, tweet_no_stopwords) %>% bind_rows
  prepositions_locations <- prepositions_locations[!(prepositions_locations$word_loc_max %in% c(-Inf, Inf)),] #TODO Check why getting Inf using `phrase_locate()` function
  prepositions_locations <- prepositions_locations$word_loc_max %>% unique # vector of locations of prepositions in tweet
  
  #### Extract words after prepositions
  locations_in_tweet_prep <- lapply(prepositions_locations, function(prep_loc_i){
  
    locations_in_tweet_prep_i <- data.frame(NULL)
    
    # If first word is certain word (eg, stopword), skip / don't use words after this prepositions
      # Stopwords
      # Common traffic words (eg, on "exit"), where likely not referencing landmark
      # Common location words that may come after preposition (top, bottom)
    # First word must be 3 or more letters long
    
    # Check if next word is not NA // not at end of sentence
    if(!is.na(word(tweet_no_stopwords, prep_loc_i+1))){ 
      
      # Check if next word is common traffic word 
      if(!(word(tweet_no_stopwords, prep_loc_i+1) %in% c(stopwords::stopwords(language="en"), "exit", "top", "bottom", "scene"))){
        
        # Check if next word is more than three characters long
        if(nchar(word(tweet_no_stopwords, prep_loc_i+1)) >= 3){
    
          
          
        landmark_gazetteer_aug_withword <- landmark_gazetteer
        for(i in 1:10){
          word_i <- word(tweet_no_stopwords, prep_loc_i+i)
          
          if(i == 1) landmark_gazetteer_aug_withword_temp <- landmark_gazetteer_aug_withword[grepl(paste0("^", word_i, "\\b"), landmark_gazetteer_aug_withword$name), ]
          if(i > 1) landmark_gazetteer_aug_withword_temp <- landmark_gazetteer_aug_withword[grepl(paste0("\\b", word_i, "\\b"), landmark_gazetteer_aug_withword$name), ]
          
          if(nrow(landmark_gazetteer_aug_withword_temp) > 0){
            landmark_gazetteer_aug_withword <- landmark_gazetteer_aug_withword_temp
          } else{
            break
          }
        }
        
        min_words <- min(str_count(landmark_gazetteer_aug_withword$name, "\\S+"))
        landmark_gazetteer_aug_withword <- landmark_gazetteer_aug_withword[str_count(landmark_gazetteer_aug_withword$name, "\\S+") %in% min_words,]
        landmark_gazetteer_aug_withword <- landmark_gazetteer_aug_withword[!is.na(landmark_gazetteer_aug_withword$lat),] # if blank, will give one row with NAs
        
        if(nrow(landmark_gazetteer_aug_withword) >= 1){
          
          max_dist_km <- sqrt((max(landmark_gazetteer_aug_withword$lat) - min(landmark_gazetteer_aug_withword$lat))^2 +
                                (max(landmark_gazetteer_aug_withword$lon) - min(landmark_gazetteer_aug_withword$lon))^2) * 111.12
          
          
          if(max_dist_km <= 0.5){
            #### If all close together
            
            locations_in_tweet_prep_i <- landmark_gazetteer_aug_withword$name %>% 
              unique %>% 
              as.data.frame %>%
              dplyr::rename(matched_words_correct_spelling = ".") %>%
              mutate(matched_words_tweet_spelling = word(tweet_no_stopwords, prep_loc_i+1, prep_loc_i+i-1)) %>%
              mutate(exact_match = FALSE) %>%
              mutate(location_type = "landmark")
          } 
          
          if((max_dist_km > 0.5) & nrow(landmark_gazetteer_aug_withword) < 60){
          
            #### If not all close together check for dominant cluster
            
            # Create distance matrix
            landmark_gazetteer_aug_withword_sp <- landmark_gazetteer_aug_withword
            coordinates(landmark_gazetteer_aug_withword_sp) <- ~lon+lat
            landmark_gazetteer_aug_withword_distances_mat <- gDistance(landmark_gazetteer_aug_withword_sp, byid=T) * 111.12
            
            # Check if cluster exists
            landmark_gazetteer_aug_withword_distances_list <- landmark_gazetteer_aug_withword_distances_mat %>% as.list %>% unlist
            cluster_exists <- mean(landmark_gazetteer_aug_withword_distances_list <= 0.5) > .9
            
            # If cluster exists, extract coordinates of dominant cluster
            if(cluster_exists){
              # Extract columns where more than X % are close together
              landmark_gazetteer_aug_withword_distances_mat_closeTF <- landmark_gazetteer_aug_withword_distances_mat < 0.5
              in_dominant_cluster <- (colSums(landmark_gazetteer_aug_withword_distances_mat_closeTF) / nrow(landmark_gazetteer_aug_withword_distances_mat_closeTF) >= .9)
              
              landmark_gazetteer_aug_withword <- landmark_gazetteer_aug_withword[in_dominant_cluster,]
              
              # Resulting dataframe
              if(nrow(landmark_gazetteer_aug_withword) > 0){ # just in case...
                locations_in_tweet_prep_i <- landmark_gazetteer_aug_withword$name %>% 
                  unique %>% 
                  as.data.frame %>%
                  dplyr::rename(matched_words_correct_spelling = ".") %>%
                  mutate(matched_words_tweet_spelling = word(tweet_no_stopwords, prep_loc_i+1, prep_loc_i+i-1)) %>%
                  mutate(exact_match = FALSE) %>%
                  mutate(location_type = "landmark")
              }
            }
          }
        }
      }
      }
    }
    
    return(locations_in_tweet_prep_i)

  }) %>% bind_rows

  #### Remove if landmark already found
  if(nrow(locations_in_tweet_prep) > 0){
    locations_in_tweet_prep <- locations_in_tweet_prep[!(locations_in_tweet_prep$matched_words_tweet_spelling %in% locations_in_tweet$matched_words_tweet_spelling),]
  }
  
  #### Add locations in tweets found after prepositions to main locations in tweet dataframe
  locations_in_tweet <- bind_rows(locations_in_tweet, locations_in_tweet_prep)

  # Prep Location Datasets -----------------------------------------------------
  locations_in_tweet <- locations_in_tweet[!(locations_in_tweet$location_type %in% "neighborhood"),]
  landmark_match <- landmark_match[landmark_match$location_type %in% "landmark",]
  
  # Choosing which landmarks to use --------------------------------------------
  df_out <- data.frame(matrix(nrow=1,ncol=0))

  if(nrow(locations_in_tweet) > 0){
    
    # Locations of Words in Tweet --------------------------------------------------
    #### Locations 
    # Add location of words in tweet to locations_in_tweet dataframe
    word_locations <- lapply(as.character(locations_in_tweet$matched_words_tweet_spelling), phrase_locate, tweet) %>% bind_rows
    locations_in_tweet <- merge(locations_in_tweet, word_locations, by.x="matched_words_tweet_spelling", by.y="word")
    locations_in_tweet_original <- locations_in_tweet
    
    #### Other word types
    # Create dataframes indicating locations of (1) crash words and (2) prepositions
    # in tweets
    crash_word_locations <- lapply(crash_words, phrase_locate, tweet) %>% bind_rows
    preposition_word_locations <- lapply(prepositions_list, phrase_locate, tweet) %>% bind_rows
    
    # *** RESTRICT LOCATIONS TO CONSIDER =======================================
    
    # General Landmarks --------------------------------------------------------
    # General landmarks are those with multiple names, are not close to each
    # other and there is no dominant cluster. These are more likely to have
    # spurious names (names not relevant for a location). These will only help
    # pinpoint a location if a road is mentioned. Consequently, we throw these 
    # out if a road is not mentioned. If a road is mentioned, we restrict to ones
    # that are close to a road.
    
    # WILL THIS WORK? NEED TO SUBSET ACTUAL LANDMARK_GAZETTEER AND CHANGE LOCATION_IN_TWEET !!!!!!!!!!!!!!!!!!!!!!!!!
    # NEED TO ACCOUNT FOR WHEN NAME IS BOTH GENERAL AND SPECIFIC
    
    landmark_match_general <- merge(landmark_match, landmark_gazetteer, by.x="matched_words_correct_spelling", by.y="name", all.x=T, all.y=F)

    # If there are any general landmarks
    if("general" %in% landmark_match_general$general_specific){
      
      landmark_match_general <- landmark_match_general[landmark_match_general$general_specific %in% "general",]
      
      # If there are roads
      if(nrow(road_match) > 0){
        
        # Add distance to road to general landmarks
        coordinates(landmark_match_general) <- ~lon+lat
        crs(landmark_match_general) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
        
        roads_in_tweet <- roads[roads$name %in% road_match$matched_words_correct_spelling,]
        roads_in_tweet$id <- 1
        roads_in_tweet <- raster::aggregate(roads_in_tweet, by="id")
        
        landmark_match_general$distance_to_road <- as.numeric(gDistance(landmark_match_general, roads_in_tweet, byid = T)) * 111.12
        
        # (1) List of general landmarks far from a road; (2) remove these
        landmark_match_general <- landmark_match_general[landmark_match_general$distance_to_road >= 0.5,]
        landmark_match <- landmark_match[!(landmark_match$matched_words_tweet_spelling %in% landmark_match_general$matched_words_tweet_spelling),]
        
      # If there are no roads, remove general landmarks    
      } else{
        landmark_gazetteer <- landmark_gazetteer[!(landmark_gazetteer$general_specific %in% "general"),]
        
        landmark_match_temp <- landmark_match[landmark_match$matched_words_correct_spelling %in% landmark_gazetteer$name,]
        
        if(nrow(landmark_match_temp) > 0){
          landmark_match <- landmark_match_temp
          locations_in_tweet <- locations_in_tweet[(locations_in_tweet$matched_words_correct_spelling %in% landmark_gazetteer$name) | 
                                                     !(locations_in_tweet$location_type %in% "landmark"),]
        }

        #landmark_match <- landmark_match[!(landmark_match$matched_words_tweet_spelling %in% landmark_match_general$matched_words_tweet_spelling),]
      }
      
    }
    
    # Landmark intersects with road, choose road -------------------------------
    locations_in_tweet <- locations_in_tweet[!(locations_in_tweet$word_loc_min %in% c(Inf,-Inf)),]
    locations_in_tweet <- locations_in_tweet[!(locations_in_tweet$word_loc_max %in% c(Inf,-Inf)),]
    # If road name intersects with landmark name, remove landmark 
    # airtel msa rd: landmark: "airtel mesa", road: "msa rd"
    
    if( (TRUE %in% (locations_in_tweet$location_type %in% "landmark")) & (TRUE %in% (locations_in_tweet$location_type %in% "road")) ){
      
      # Road locations
      locations_in_tweet_roads <- locations_in_tweet[locations_in_tweet$location_type %in% "road",]
      road_locations <- lapply(1:nrow(locations_in_tweet_roads), function(i){
        road_locs <- locations_in_tweet_roads$word_loc_min[i]:locations_in_tweet_roads$word_loc_max[i]
        return(road_locs)
      }) %>% unlist
      
      locations_keep <- lapply(1:nrow(locations_in_tweet), function(i){
        
        keep <- TRUE
        
        if(locations_in_tweet$location_type[i] %in% "landmark"){
          if(TRUE %in% (locations_in_tweet$word_loc_min[i]:locations_in_tweet$word_loc_max[i] %in% road_locations)){
            keep <- FALSE
          }
        }
        
        return(keep)
      }) %>% unlist
      
      locations_in_tweet <- locations_in_tweet[locations_keep,] %>% unique
      
    }
    
    # If exact intersects with fuzzy, choose exact -----------------------------
    if((TRUE %in% locations_in_tweet$exact_match) & (FALSE %in% locations_in_tweet$exact_match)){
      
      # Road locations
      locations_in_tweet_exactTRUE <- locations_in_tweet[locations_in_tweet$exact_match %in% TRUE,]
      exactTRUE_locations <- lapply(1:nrow(locations_in_tweet_exactTRUE), function(i){
        exactTRUE_locs <- locations_in_tweet_exactTRUE$word_loc_min[i]:locations_in_tweet_exactTRUE$word_loc_max[i]
        return(exactTRUE_locs)
      }) %>% unlist
      
      locations_keep <- lapply(1:nrow(locations_in_tweet), function(i){
        
        keep <- TRUE
        
        if(locations_in_tweet$exact_match[i] %in% FALSE){
          if(TRUE %in% (locations_in_tweet$word_loc_min[i]:locations_in_tweet$word_loc_max[i] %in% exactTRUE_locations)){
            keep <- FALSE
          }
        }
        
        return(keep)
      }) %>% unlist
      
      locations_in_tweet <- locations_in_tweet[locations_keep,] %>% unique
      
    }
    
    # Phase within Phrase ------------------------------------------------------
    # If phrase within another phrase, choose longer one (pick "garden city mall" over "garden city").
    # If phrases are same word length, keep both
    locations_remove <- lapply(1:nrow(locations_in_tweet), function(i){
      phrase_in_longer_phrase <- ((locations_in_tweet[i,]$word_loc_min >= locations_in_tweet[-i,]$word_loc_min) &
                                  (locations_in_tweet[i,]$word_loc_max <= locations_in_tweet[-i,]$word_loc_max))
      
      same_start_end <- ((locations_in_tweet[i,]$word_loc_min == locations_in_tweet[-i,]$word_loc_min) &
                         (locations_in_tweet[i,]$word_loc_max == locations_in_tweet[-i,]$word_loc_max))
      
      phrase_in_longer_phrase[same_start_end] <- FALSE
  
      return(TRUE %in% phrase_in_longer_phrase)
    }) %>% unlist
    
    locations_in_tweet <- locations_in_tweet[!locations_remove,] %>% unique
    
    # Same Start/End, choose correctly spelled ---------------------------------
    # If phrase has same start/end location, only keep ones that are correctly spelled
    locations_keep <- lapply(1:nrow(locations_in_tweet), function(i){
      
      keep_location <- TRUE
      
      # Grab all landmarks that start/end same location
      same_start_end <- ((locations_in_tweet[i,]$word_loc_min == locations_in_tweet$word_loc_min) &
                           (locations_in_tweet[i,]$word_loc_max == locations_in_tweet$word_loc_max))
      
      # Check if one of landmarks with same start/end is spelled correctly;
      # only subset if that is the case
      if(TRUE %in% locations_in_tweet$exact_match[same_start_end]){
        keep_location <- locations_in_tweet$exact_match[i]
      }
      
      return(keep_location)
      
    }) %>% unlist
    
    locations_in_tweet <- locations_in_tweet[locations_keep,] %>% unique
    
    # If same name both road and landmark, drop landmark -----------------------
    if(("landmark" %in% locations_in_tweet$location_type) & ("road" %in% locations_in_tweet$location_type)){
      
      drop_if_landmark <- locations_in_tweet$matched_words_tweet_spelling %in% 
        locations_in_tweet$matched_words_tweet_spelling[locations_in_tweet$location_type %in% "road"]
      
      locations_in_tweet <- locations_in_tweet[!(locations_in_tweet$location_type %in% "landmark" & drop_if_landmark),]
    }
    
    # Find Road Intersection ---------------------------------------------------
    # Iterates through all 2-road combinations and checks for intersections. Uses
    # the intersection if intersection points are close together.
    # TODO: what if road self intersects?
    
    locations_in_tweet_roads <- locations_in_tweet[locations_in_tweet$location_type %in% "road",]
    
    if(nrow(locations_in_tweet_roads) >= 2){
    
      road_combn <- combn(nrow(locations_in_tweet_roads), 2)
      
      #### Define Function
      extract_road_intersection <- function(i){
        intersection_point <- data.frame(NULL)
        
        locations_in_tweet_roads_i <- locations_in_tweet_roads[road_combn[,i],]
        
        road_1 <- roads[tolower(roads$name) %in% locations_in_tweet_roads_i$matched_words_correct_spelling[1],]
        road_2 <- roads[tolower(roads$name) %in% locations_in_tweet_roads_i$matched_words_correct_spelling[2],]
        
        intersection_points <- gIntersection(road_1, road_2)
        
        # Check if intersection points
        if(!is.null(intersection_points)){
        
          # If maximum distance between points are close together, then use
          intersection_points_extent <- extent(intersection_points)
          max_points_dist <- sqrt((intersection_points_extent@xmin - intersection_points_extent@xmax)^2+ (intersection_points_extent@ymin - intersection_points_extent@ymax)^2)*111.12
          
          if(max_points_dist < 0.5){
            intersection_point <- gCentroid(intersection_points)@coords %>% 
              as.data.frame %>%
              dplyr::rename(lon = x) %>%
              dplyr::rename(lat = y) %>%
              dplyr::mutate(road_correct_spelling_1 = locations_in_tweet_roads_i$matched_words_correct_spelling[1],
                            road_tweet_spelling_1 = locations_in_tweet_roads_i$matched_words_tweet_spelling[1],
                            road_correct_spelling_2 = locations_in_tweet_roads_i$matched_words_correct_spelling[2],
                            road_tweet_spelling_2 = locations_in_tweet_roads_i$matched_words_tweet_spelling[2])
          }
        }
        
        return(intersection_point)
      }
      
      #### Implement Function; Grab Intersections
      road_intersections <- lapply(1:ncol(road_combn), extract_road_intersection) %>% bind_rows
      
      #### Add variable to "locations_in_tweet" if road is part of intersection
      # TODO
        
    } else{
      # Other parts check nrow(road_intersections), so make blank dataframe
      road_intersections <- data.frame(NULL)
    }

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

