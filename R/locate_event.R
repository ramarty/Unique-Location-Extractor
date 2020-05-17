# Crashmap Algorithm

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

# Algorithm Inputs -------------------------------------------------------------
if(F){
  
  text <- "crash near airtel on mombasa rd words words words yaya center kenyatta ave westlands"
  text_i <- text
  
  # Load Data ------------------------------------------------------------------
  AUG_GAZ <- T
  
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
  
  areas <- readRDS(file.path(algorithm_inputs, "nairobi_estates", "nairobi_estates.Rds"))
  areas@data <- areas@data %>%
    dplyr::rename(name = estate)
  
  # Parameters -----------------------------------------------------------------
  text <- text
  
  landmarks <- landmark_gazetteer_orig
  roads <- roads
  areas <- areas
  
  preposition_list <- list(c("at", "next to","around", "just after", "opposite","opp", "apa", "hapa","happened at","just before","at the","outside","right before"),
                           c("near", "after", "toward","along", "towards", "approach"),
                           c("past","from","on"))
  event_words <- c("accidents", "accident", "crash", "overturn", "collision", "wreck")
  junction_words <- c("intersection", "junction")
  false_positive_phrases <- c("githurai bus", "githurai matatu", 
                              "githurai 45 bus", "githurai 45 matatu",
                              "city hoppa bus", "hoppa bus",
                              "rongai bus", "rongai matatu", "rongai matatus",
                              "machakos bus", "machakos minibus", "machakos matatu")
  type_list <- c("")
  
  fuzzy_match <- TRUE
  fuzzy_match.min_word_length <- c(5,11)
  fuzzy_match.dist <- c(1,2)
  fuzzy_match.ngram_max <- 3
  fuzzy_match.first_letters_same <- TRUE
  fuzzy_match.last_letters_same <- TRUE
  
  crs_distance <- "+init=epsg:21037"
  crs_out <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
  
  # ******* OLD ************************************************************** #
  
  # ALGORITHM PARAMETERS
  fuzzy_match_landmark <- TRUE
  fuzzy_match_landmark.min.word.length <- c(5,11) # minimum word length for fuzzy match
  fuzzy_match_landmark.dist <- c(1,2) # maximum levenstein distance to use
  fuzzy_match_ngram_max <- 3
  crash_words <- c("accidents", "accident", "crash", "overturn", "collision", "wreck") # hit?
  junction_words <- c("intersection", "junction")
  first_letters_same <- TRUE
  last_letters_same <- TRUE # !!!!!!!! fails for roysambo/u. Maybe: if 1 letter off, only first letter needs to be same. But be more restrictive with 2?
  
  projection <- "+init=epsg:21037"
  
  prepositions_list <- list(c("at", "next to","around", "just after", "opposite","opp", "apa", "hapa","happened at","just before","at the","outside","right before"),
                            c("near", "after", "toward","along", "towards", "approach"),
                            c("past","from","on"))
  
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
  
}

# ALGORITHM NEW ================================================================
locate_event <- function(text,
                         landmarks, 
                         roads, 
                         areas, 
                         prepositions_list, 
                         event_words, 
                         junction_words, 
                         false_positive_phrases, 
                         type_list, 
                         fuzzy_match = TRUE,
                         fuzzy_match.min_word_length = c(5,11),
                         fuzzy_match.dist = c(1,2),
                         fuzzy_match.ngram_max = 3,
                         fuzzy_match.first_letters_same = TRUE,
                         fuzzy_match.last_letters_same = TRUE,
                         crs_distance, 
                         crs_out = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0",
                         quiet = F){
  
  # 1. Checks ------------------------------------------------------------------
  # Check inputs and output errors if anything is wrong.
  
  if(!is.list(prepositions_list)) stop("prepositions_list must be a list")
  
  # 2. Clean/Prep Input Files --------------------------------------------------
  # Cleans and preps gazetteer and road files
  
  #### Project Data
  landmark_gazetteer <- spTransform(landmark_gazetteer, CRS(crs_distance))
  roads              <- spTransform(roads,              CRS(crs_distance))
  areas              <- spTransform(areas,              CRS(crs_distance))
  
  #### Clean Names
  landmark_gazetteer$name <- landmark_gazetteer$name %>%
    str_replace_all("[[:punct:]]", "")
  
  roads$name <- roads$name %>% 
    as.character %>% 
    tolower
  
  ## Names into lists
  landmark_list <- landmark_gazetteer$name
  roads_list <- roads$name %>% as.character %>% tolower
  areas_list <- areas$name
  
  prepositions_all <- prepositions_list %>% unlist()
  #prepositions_list <- c(tier_1_prepositions, tier_2_prepositions)
  #prepositions_list_all <- c(tier_1_prepositions, tier_2_prepositions, tier_3_prepositions)
  
  ## Add unique ID to gazetteer
  landmark_gazetteer$uid <- 1:nrow(landmark_gazetteer)
  roads$uid              <- 1:nrow(roads)
  areas$uid              <- 1:nrow(areas)
  
  ## Intersection Words
  intersection_words <- c("intersection", "junction")
  intersection_words_regex <- paste0("\\b", intersection_words, "\\b") %>% paste(collapse = "|")
  
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
  
  # 4. Implement Algorithm -----------------------------------------------------
  counter_number <- 1
  
  out_all <- lapply(text,
                    locate_event_i,
                    landmarks                      = landmarks, 
                    roads                          = roads, 
                    areas                          = areas, 
                    prepositions_list              = prepositions_list, 
                    event_words                    = event_words, 
                    junction_words                 = junction_words, 
                    false_positive_phrases         = false_positive_phrases, 
                    type_list                      = type_list, 
                    fuzzy_match                    = fuzzy_match,
                    fuzzy_match.min_word_length    = fuzzy_match.min_word_length,
                    fuzzy_match.dist               = fuzzy_match.dist,
                    fuzzy_match.ngram_max          = fuzzy_match.ngram_max,
                    fuzzy_match.first_letters_same = fuzzy_match.first_letters_same,
                    fuzzy_match.last_letters_same  = fuzzy_match.last_letters_same,
                    crs_out                        = crs_out,
                    quiet                          = quiet) # %>%
  # [append sf objects]
  
  return(out_all)
}

locate_event_i <- function(text_i,
                           landmarks,
                           roads,
                           areas,
                           prepositions_list,
                           event_words,
                           junction_words,
                           false_positive_phrases,
                           type_list,
                           fuzzy_match,
                           fuzzy_match.min_word_length,
                           fuzzy_match.dist,
                           fuzzy_match.ngram_max,
                           fuzzy_match.first_letters_same,
                           fuzzy_match.last_letters_same,
                           crs_out,
                           quiet){
  
  # 1. Determine Location Matches in Gazetteer ---------------------------------
  #### Exact Match
  landmark_match     <- phrase_in_sentence_exact(text_i, landmark_list) 
  road_match         <- phrase_in_sentence_exact(text_i, roads_list)
  area_match <- phrase_in_sentence_exact(text_i, areas_list)
  
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
    area_match_fuzzy <- phrase_in_sentence_fuzzy(text_i, 
                                                 areas_list,
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
    
    area_match_fuzzy <- area_match_fuzzy %>%
      filter(!(str_count(matched_words_tweet_spelling, "\\S+") %in% 1)) %>%
      filter(!hunspell_check(matched_words_tweet_spelling))
    
    #### Add fuzzy match to full match list
    # Starting with exact match ensures, if both exact and fuzzy, only
    # exact is kept.
    landmark_match <- bind_rows(landmark_match, landmark_match_fuzzy) %>%
      distinct(matched_words_correct_spelling, .keep_all = TRUE)
    
    road_match <- bind_rows(road_match, road_match_fuzzy) %>%
      distinct(matched_words_correct_spelling, .keep_all = TRUE)
    
    area_match <- bind_rows(area_match, area_match_fuzzy) %>%
      distinct(matched_words_correct_spelling, .keep_all = TRUE)
    
  }
  
  #### Dataframe of all locations found in tweet, appending across
  ## Add types
  landmark_match     <- landmark_match     %>% mutate(location_type = "landmark")
  road_match         <- road_match         %>% mutate(location_type = "road")
  area_match         <- area_match %>% mutate(location_type = "area")
  
  ## Append
  # Don't append before as there could be cases where a landmark and road has 
  # the same name, and appending and making distict would pick one over the
  # other (?), which we deal with in a separate process.
  locations_in_tweet <- bind_rows(landmark_match, road_match, area_match)
  
  # 2. Landmarks after prepositions --------------------------------------------
  
  # ** 2.1 Subset locations by roads and neighborhood -----------------------------
  # Before searching for words after landmarks, restrict by roads and neighborhoods.
  # This process restricts the gazetteer, and may make more likely to find a
  # dominant cluster
  
  ## Roads
  if(nrow(road_match) > 0){
    road_match_sp <- roads[roads$name %in% road_match$matched_words_correct_spelling,]
    
    land_road_restrict <- restrict_landmarks_by_location(landmark_match,
                                                         landmark_gazetteer,
                                                         road_match_sp)
    landmark_match     <- land_road_restrict$landmark_match
    landmark_gazetteer <- land_road_restrict$landmark_gazetteer
  }
  
  ## Areas
  if(nrow(area_match) > 0){
    area_match_sp <- areas[areas$name %in% area_match$matched_words_correct_spelling,]
    
    area_road_restrict <- restrict_landmarks_by_location(landmark_match,
                                                         landmark_gazetteer,
                                                         area_match_sp)
    landmark_match     <- area_road_restrict$landmark_match
    landmark_gazetteer <- area_road_restrict$landmark_gazetteer
  }
  
  ## Update locations_in_tweet with new landmark dataframe
  locations_in_tweet <- locations_in_tweet %>%
    filter((location_type %in% "road") | 
             ((location_type %in% "landmark") & 
                (matched_words_correct_spelling %in% landmark_match$matched_words_correct_spelling)))
  
  # ** 2.2 Preposition Locations --------------------------------------------------
  
  # When grabbing landmarks after prepositions we ignore stopwords. So for:
  # "accident near the garden city", we ignore "the" 
  text_i_no_stopwords <- text_i %>% str_replace_all("\\bthe\\b", " ") %>% str_squish
  
  ## Create vector of locations in tweets where prepositions occur
  preps_in_tweet <- phrase_in_sentence_exact(text_i_no_stopwords, prepositions_all)
  
  ## Locations of Prepositions
  prep_locs_df <- lapply(as.character(preps_in_tweet$matched_words_tweet_spelling), 
                         phrase_locate, 
                         text_i_no_stopwords) %>% 
    bind_rows %>%
    filter(!(word_loc_max %in% c(-Inf, Inf))) #TODO Check why getting Inf using `phrase_locate()` function
  
  prep_locs <- prep_locs_df$word_loc_max %>% unique # vector of locations of prepositions in tweet
  
  # ** 2.3 Extract landmarks ------------------------------------------------------
  locations_in_tweet_prep <- map_df(prep_locs, 
                                    extract_locations_after_words,
                                    text_i_no_stopwords,
                                    landmark_gazetteer) 
  
  # ** 2.4 Remove if landmark already found ---------------------------------------
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
  
  # 3. Location Dataset Prep ---------------------------------------------------
  # Prep location datasets before continuing with search
  
  ## Original Landmarks
  # Before any subsetting, save locations dataframe. Add all locations originally
  # found as variables in final dataframe.
  locations_in_tweet_original <- locations_in_tweet
  
  ## Dataset per type
  # Create dataset for each type
  landmark_match <- locations_in_tweet[locations_in_tweet$location_type %in% "landmark",]
  road_match     <- locations_in_tweet[locations_in_tweet$location_type %in% "road",]
  area_match     <- locations_in_tweet[locations_in_tweet$location_type %in% "area",]
  
  ## Locations in tweet
  # Take area out. By doing this, we allow name conflicts between locations and
  # areas.
  locations_in_tweet <- locations_in_tweet[!(locations_in_tweet$location_type %in% "area"),]
  
  ## Road shapefile
  if(nrow(road_match) > 0){
    road_match_sp <- roads[roads$name %in% road_match$matched_words_correct_spelling,]
    
    ## Aggregate roads so one row; makes distance calculations easier
    road_match_agg_sp <- road_match_sp
    road_match_agg_sp$id <- 1
    road_match_agg_sp <- raster::aggregate(road_match_agg_sp, by="id")
  }
  
  ## Areas shapefile
  if(nrow(area_match) > 0){
    area_match_sp <- areas[areas$name %in% area_match$matched_words_correct_spelling,]
    
    ## Aggregate roads so one row; makes distance calculations easier
    area_match_agg_sp <- area_match_sp
    area_match_agg_sp$id <- 1
    area_match_agg_sp <- raster::aggregate(area_match_agg_sp, by="id")
  }
  
  # 4. Choosing which landmarks to use -----------------------------------------
  df_out <- data.frame(matrix(nrow=1,ncol=0))
  
  if(nrow(locations_in_tweet) > 0){
    
    # ** 4.1 Locations of Words in Tweet ---------------------------------------
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
    preposition_word_locations <- lapply(prepositions_all, phrase_locate, text_i) %>% bind_rows
    
    # ** 4.2 Restrict by roads and neighborhood --------------------------------
    ## Roads
    if(nrow(road_match) > 0){
      land_road_restrict <- restrict_landmarks_by_location(landmark_match,
                                                           landmark_gazetteer,
                                                           road_match_sp)
      landmark_match     <- land_road_restrict$landmark_match
      landmark_gazetteer <- land_road_restrict$landmark_gazetteer
    }
    
    ## Areas
    if(nrow(area_match) > 0){
      area_road_restrict <- restrict_landmarks_by_location(landmark_match,
                                                           landmark_gazetteer,
                                                           area_match_sp)
      landmark_match     <- area_road_restrict$landmark_match
      landmark_gazetteer <- area_road_restrict$landmark_gazetteer
    }
    
    ## Update locations_in_tweet with new landmark dataframe
    locations_in_tweet <- locations_in_tweet %>%
      filter((location_type %in% "road") | 
               ((location_type %in% "landmark") & 
                  (matched_words_correct_spelling %in% landmark_match$matched_words_correct_spelling)))
    
    # ** 4.3 Restrict Locations/Landmarks to Consider --------------------------
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
    
    # 5. Find Intersections ----------------------------------------------------
    road_inter_points <- extract_intersections(locations_in_tweet, roads)
    
    # 6. Add Variables to Location Dataframes ----------------------------------
    # Add variables indicating the following:
    #   1. [Crash word] [tier x prepositon] [landmark]
    #   2. [Crash word] [other words] [prepositon] [landmark]
    #   3. Preposition before crashword
    
    # Loop through preposition tiers
    for(i in length(prepositions_list)){
      
      prepositions <- prepositions_list[[i]]
      
      #### locations_in_tweet dataframe
      locations_in_tweet[[paste0("crashword_prepos_tier_", i)]] <- 
        search_crashword_prepos(text_i, locations_in_tweet$matched_words_tweet_spelling, crash_words, prepositions)
      
      locations_in_tweet[[paste0("crashword_other_prepos_tier_", i)]] <- 
        search_crashword_other_prepos(text_i, locations_in_tweet$matched_words_tweet_spelling, crash_words, prepositions)
      
      locations_in_tweet[[paste0("prepos_before_crashword_tier_", i)]] <- 
        search_prep_loc(text_i, locations_in_tweet$matched_words_tweet_spelling, prepositions)
      
      #### road_intersections dataframe
      if(nrow(road_intersections) > 0){
        
        road_intersections[[paste0("crashword_prepos_tier_", i)]] <- 
          search_crashword_prepos(text_i, road_intersections$road_tweet_spelling_1, crash_words, prepositions) | 
          search_crashword_prepos(text_i, road_intersections$road_tweet_spelling_2, crash_words, prepositions)
        
        road_intersections[[paste0("crashword_other_prepos_tier_", i)]] <- 
          search_crashword_other_prepos(text_i, road_intersections$road_tweet_spelling_1, crash_words, prepositions) |
          search_crashword_other_prepos(text_i, road_intersections$road_tweet_spelling_2, crash_words, prepositions)
        
        road_intersections[[paste0("prepos_before_crashword_tier_", i)]] <- 
          search_prep_loc(text_i, road_intersections$road_tweet_spelling_1, prepositions) |
          search_prep_loc(text_i, road_intersections$road_tweet_spelling_2, prepositions)
        
      }
    }
    
    # 7. Determine Event Location ----------------------------------------------
    
    # ** 7.1 Prep Location Dataframes ---------------------------------------------
    locations_in_tweet <- locations_in_tweet %>% unique
    
    neighborhoods_final <- locations_in_tweet[locations_in_tweet$location_type %in% "neighborhood",] %>% unique
    roads_final <- locations_in_tweet[locations_in_tweet$location_type %in% "road",] %>% unique
    landmarks_final <- locations_in_tweet[locations_in_tweet$location_type %in% "landmark",] %>% unique
    road_intersections_final <- road_intersections 
    
    # ** 7.2 Landmark Decision Process --------------------------------------------
    ## Null output
    
    df_out <- data.frame(lat = NA,
                         lon = NA)
    
    loc_searched <- FALSE
    
    if(nrow(landmarks_final) > 0){
      # TODO: If intersection word, prioritize intersection - even if no preposition.
      #       For example, if intersection word and prep_i > 1, then go into intersection?
      
      # **** 7.2.1 Preposition Search: Landmark then Intersection ---------------------
      for(prep_i in 1:length(prepositions_list)){
        for(prep_pattern in c("crashword_prepos_tier_",
                              "crashword_other_prepos_tier_",
                              "prepos_before_crashword_tier_")){
          
          # 1. [crashword] [preposition] [location]
          if(TRUE %in% landmarks_final[[paste0("crashword_prepos_tier_", prep_i)]] & !loc_searched){
            
            df_out <- determine_location_from_landmark(
              landmarks_final[landmarks_final[[paste0("crashword_prepos_tier_", prep_i)]] %in% TRUE,],
              paste0("crashword_tier_",prep_i,"_preposition_landmark"))
            
            loc_searched <- TRUE
          }
          
          ## Do regardless of whether intersection word?
          if(TRUE %in% road_intersections_final[[paste0("crashword_prepos_tier_", prep_i)]] & !loc_searched){
            
            df_out <- determine_location_from_intersection(
              road_intersections_final[road_intersections_final$crashword_prepos_tier_1 %in% TRUE,],
              paste0("crashword_tier_",prep_i,"_preposition_intersection"))
            
            loc_searched <- TRUE
          }
          
        }
      }
      
      # **** 7.2.2 Intersection Search ------------------------------------------------
      
      #### If there is an intersection word and more than one intersection
      if(grepl(intersection_words_regex, tweet) & nrow(road_intersections_final) > 0 & !loc_searched){
        
        df_out <- determine_location_from_intersection(
          road_intersections_final,
          "intersection_word")
        
        loc_searched <- TRUE
        
      }
      
      #### If there is only one intersection
      if(nrow(road_intersections_final) %in% 1 & !loc_searched){
        
        df_out <- determine_location_from_intersection(
          road_intersections_final,
          "one_intersection")
        
        loc_searched <- TRUE
      }
      
      # **** 7.2.3 Ambiguous Pattern -------------------------------------------
      if(!loc_searched){
        
        df_out <- determine_location_from_landmark(
          landmarks_final,
          "landmark_ambiguous_pattern")
        
        loc_searched <- TRUE
        
      }
      
      # **** 7.2.4 Output Cleaning and Checks ---------------------------------------
      
      #### Spatially define
      df_out_sp <- df_out 
      coordinates(df_out_sp) <- ~lon+lat
      crs(df_out_sp) <- CRS(projection)
      
      #### If dataframe more than one row, collapse to one row
      df_out$id <- 1
      df_out <- df_out %>%
        group_by(id) %>%
        summarise_all(function(x) x %>% unique %>% paste(collapse = ";")) %>%
        dplyr::rename(lon_all = lon,
                      lat_all = lat) %>%
        dplyr::select(-id)
      
      #### Dominant Cluster
      df_out_sp <- extract_dominant_cluster(df_out_sp)
      
      if(nrow(df_out_sp) > 0){
        coords <- gCentroid(df_out_sp) 
        coords$id <- 1 # dummy variable so if spatial dataframe
        coords@data <- df_out
        
        df_out <- coords
        
        #### Add distance to mentioned road
        if(nrow(road_match_sp) > 0){
          df_out$dist_mentioned_road <- gDistance(road_match_agg_sp, df_out) < 500
        }
        
        # If no dominant cluster  
      } else{
        df_out <- df_out %>%
          mutate(lon = NA,
                 lat = NA,
                 no_dominant_cluster = T)
      }
      
    }
    
    if((nrow(roads_final) > 0 | nrow(areas_final) > 0) & !loc_searched){
      
      # ** 7.3 Road and Area ---------------------------------------------------
      # If don't intersect, ignore
      if(nrow(roads_final) > 0 & nrow(areas_final) > 0 & !loc_searched){
        
        df_out_candidate <- gIntersection(roads_final, areas_final)
        
        if(!is.null(df_out_candidate)){
          df_out_candidate$id <- 1
          df_out <- raster::aggregate(df_out_candidate, by="id")
          
          # If extent is small, make point
          df_out <- make_point_small_extent(df_out)
          
          df_out$how_determined_landmark <- "road_area_intersection"
          
          loc_searched <- TRUE
        }
        
      }
      
      # ** 7.4 Road Decision Process ------------------------------------------------
      # If no landmark, output road only if one road
      
      if(nrow(roads_final) %in% 1 & !loc_searched){
        
        # Create spatial dataframe of road
        roads_final_sp <- merge(roads, roads_final, by.x="name", by.y="matched_words_correct_spelling", all.x=F)
        
        # Only use if none of road segments is ambiguous
        if(!(TRUE %in% roads_final_sp$ambiguous_road)){
          
          # Use road if df_out is blank
          roads_final_sp$id <- 1
          df_out <- raster::aggregate(roads_final_sp, by="id")
          
          # If extent is small, make point
          df_out <- make_point_small_extent(df_out)
          
          df_out$how_determined_landmark <- "one_road"
          
          loc_searched <- TRUE
          
        }
      }
      
      # ** 7.5 Area Decision Process -------------------------------------------
      
      if(nrow(areas_final) %in% 1 & !loc_searched){
        
        # Create spatial dataframe of road
        areas_final_sp <- merge(areas, areas_final, by.x="name", by.y="matched_words_correct_spelling", all.x=F)
        
        # Use road if df_out is blank
        areas_final_sp$id <- 1
        df_out <- raster::aggregate(areas_final_sp, by="id")
        
        # If extent is small, make point
        df_out <- make_point_small_extent(df_out)
        
        df_out$how_determined_landmark <- "one_area"
        
        loc_searched <- TRUE
        
      }
      
    }
    
    # ** 7.4 TODO: Estate / Estate + Road Combination ------------------------------
    
    # 8. Add Variables to Output -----------------------------------------------
    
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
    
    df_out$text <- text_i
    if(!is.null(df_out$dist_closest_crash_word)) df_out$dist_closest_crash_word <- as.character(df_out$dist_closest_crash_word)
    
  }
  
  # 9. Clean Spatial Output --------------------------------------------------
  # If spatial object, reproject and make sf
  if(typeof(df_out) %in% "S4"){
    df_out <- spTransform(df_out, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
    df_out <- st_as_sf(df_out)
  } else{
    # If not spatial object, give null geometry
    df_out <- st_sf(df_out, geom = st_sfc(st_point()))
  }
  
  # Printing to show progress
  counter_number <<- counter_number + 1
  print(counter_number)
  
  return(df_out)
}






