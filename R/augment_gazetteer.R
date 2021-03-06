# Augment Gazetteer

# TODO:
# 1. More efficient way of appending sp objects than do.call(what="rbind")?
# 2. Make function that wraps this into lapply: extract_dominant_cluster
# 3. parallel.sep_slash - should consider as any other parallel landmark. Add to
#     others and determine of should add? Or should that be a consideration here?
#     e.g., newname, etc? Should separate into unique/nonunique and only implement
#     on nonunique for efficiency. Other efficiency gains here?? For non-unique,
#     can we do a group_by to check max distance? I think yes... grab coordinats,
#     and within group lat/lon min/max, then can quickly say specific. But might
#     still want to collapse??
# 4. parallel. if only add if no name conflict or specifc, could add an 
#    parallel.always_add" parameter.
# 5. Ordering - group into chunks and think about ordering. What makes sense? What assume?
#    - Does parallel make sense to go first?
#    - Maybe: (1) parallel_names, (2) grams, (3) parallel_types
# 6. Should separapte parallel into parallel_names and parallel_types ??

library(raster)
library(rgdal)
library(dplyr)
library(readr)
library(quanteda)
library(stringr)
library(stringi)
library(rgeos)
library(hunspell)
#library(spacyr) # NEED THESE; UNCOMMENT
#spacy_initialize()

augment_gazetteer <- function(landmarks,
                              landmarks.name_var = "name",
                              landmarks.type_var = "type",
                              close_dist_thresh = 500, # NOT IMPLEMENTED YET
                              types_rm = c("route", "road", "toilet", "political", "locality", "neighborhood", "area", "section of populated place"),
                              types_rm.except_with_type = c("flyover", "round about", "roundabout"),
                              types_rm.except_with_name = c("flyover", "round about", "roundabout"),
                              grams.min_words = 3,
                              grams.max_words = 6,
                              grams.skip_gram_first_last_word_match = T,
                              grams.add_only_if_name_new = F,
                              grams.add_only_if_specific = F,
                              parallel.sep_slash = T,
                              parallel.rm_begin = c(stopwords("en"), c("near","at","the", "towards", "near")),
                              parallel.rm_end = c("bar", "shops", "restaurant","sports bar","hotel", "bus station"),
                              parallel.word_diff = "default", 
                              parallel.rm_begin_iftype = NULL,
                              parallel.rm_end_iftype = list(list(words = c("stage", "bus stop"), type = "transit_station")),
                              parallel.word_diff_iftype = list(list(words = c("stage", "bus stop", "bus station"), type = "transit_station")),
                              parallel.add_only_if_name_new = F, 
                              parallel.add_only_if_specific = F, 
                              parallel.word_begin_addtype = NULL,
                              parallel.word_end_addtype = list(list(words = c("stage", "bus stop", "bus station"), type = "stage")),
                              rm.contains = c("road", "rd"),
                              rm.name_begin = c(stopwords("en"), c("near","at","the", "towards", "near")),
                              rm.name_end = c("highway", "road", "rd", "way", "ave", "avenue", "street", "st"),
                              pos_rm.all = c("ADJ", "ADP", "ADV", "AUX", "CCONJ", "INTJ", "NUM", "PRON", "SCONJ", "VERB", "X"),
                              pos_rm.except_type = list(list(pos = c("NOUN", "PROPN"), 
                                                             type = c("bus", "restaurant", "bank"),
                                                             name = c("parliament"))),
                              crs_distance,
                              crs_out = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0",
                              quiet = T){
  
  # DESCRIPTION: Augments landmark gazetteer
  # ARGS:
  # landmarks: Spatial Points Dataframe (or sf equivalent) of landmarks.
  # landmarks.name_var: Name of variable indicating name of landmark
  # landmarks.type_var: Name of variable indicating type of landmark
  # grams_min_words: Minimum number of words in name to make n/skip-grams out of name
  # grams_max_words: Maximum number of words in name to make n/skip-grams out of name.
  #                  Setting a cap helps to reduce spurious landmarks that may come
  #                  out of really long names
  # grams.skip_gram_first_last_word_match: For skip-grams, should first and last word be the
  #                             same as the original word? (TRUE/FASLE)
  # types_rm: If landmark has one of these types, remove - unless 'types_always_keep' 
  #               or 'names_always_keep' prevents removing.
  # types_rm.except_with_type: landmark types to always keep. This parameter only becomes
  #                    relevant in cases where a landmark has more than one type.
  #                    If a landmark has both a "types_rm" and a "types_always_keep" 
  #                    landmark, this landmark will be kept.
  # types_rm.except_with_name: landmark names to always keep. This parameter only
  #                    becomes relevant in cases where a landmark is one of 
  #                    "types_rm" Here, we keep the landmark if "names_always_keep" 
  #                    is somewhere in the name. For example, if the landmark is 
  #                    a road but has flyover in the name, we may want to keep 
  #                    the landmark as flyovers are small spatial areas.
  # parallel.rm_begin: If a landmark name begins with one of these words, add a
  #                    landmark that excludes the word.
  # parallel.rm_end: If a landmark name ends with one of these words, add a
  #                    landmark that excludes the word.
  # parallel.rm_begin_iftype: If a landmark name begins with one of these words, add a
  #                           landmark that excludes the word if the landmark is a 
  #                           certain type.
  # parallel.rm_end_iftype: If a landmark name ends with one of these words, add a
  #                         landmark that excludes the word if the landmark is a
  #                         certain type.
  # parallel.word_diff_iftype: If the landmark includes one of these words, add a
  #                            landmarks that swap the word for the other words. 
  #                            Only do if the landmark is a certain type.
  # parallel.word_end_addtype: If the landmark ends with one of these words,
  #                            add the type. For example, if landmark is "X stage",
  #                            this indicates the landmark is a bus stage. Adding the
  #                            "stage" to landmark ensures that the type is reflected.
  # rm.contains: Remove the landmark if it contains one of these words. Implemented 
  #              after N/skip-grams and parallel landmarks are added.
  # rm.name_begin: Remove the landmark if it begins with one of these words. Implemented 
  #              after N/skip-grams and parallel landmarks are added.
  # rm.name_end: Remov ethe landmark if it ends with one of these words. Implemented 
  #              after N/skip-grams and parallel landmarks are added.
  # crs_distance: Coordiante reference system to use for distance calculations.
  # crs_out: Coordinate reference system for output. 
  
  # 1. Checks ------------------------------------------------------------------
  quiet <<- quiet # do I need to do this? sometimes goes into function without defining.
  
  if(!quiet) print("Checks")
  
  if(class(landmarks)[1] %in% "sf") landmarks <- landmarks %>% as("Spatial")
  
  if(!(class(landmarks)[1] %in% c("SpatialPointsDataFrame", "sf"))){
    stop("landmarks must be a SpatialPointsDataFrame or an sf object")
  }
  
  # 2. Prep landmark object ----------------------------------------------------
  if(!quiet) print("Prep Landmark Object")
  
  #### Prep variables
  landmarks$name <- landmarks[[landmarks.name_var]]
  landmarks$type <- landmarks[[landmarks.type_var]]
  landmarks$number_words <- str_count(landmarks$name, "\\S+")
  landmarks@data <- landmarks@data %>%
    dplyr::select(name, type, number_words)
  
  #### Prep spatial
  landmarks <- spTransform(landmarks, CRS(crs_distance))
  
  # 2. Basic initial cleaning --------------------------------------------------
  landmarks$name <- landmarks$name %>%
    str_replace_all("&", " and ") %>%
    str_squish()
  
  # 3. Initial Parallel Landmarks ----------------------------------------------
  # These are parallel landmarks that should be added before any cleaning is 
  # done. For example, these rely on symbols to break up the landmark
  # e.g., slash (/)
  
  if(!quiet) print("Separating Slashes")
  
  if(parallel.sep_slash){
    
    # Split at slash, open parenthese, dash, and comma. / ( - ,
    breaks <- c("/", "\\(", "-", ",") %>% paste(collapse = "|")
    
    landmarks_slash <- landmarks[grepl(breaks, landmarks$name),]
    
    # Separate landmarks with slashes into multiple slashes. Handles cases with 
    # multiple slashes. For example if: a/b/c, makes three landmarks: a, b and c
    # as three separate rows.
    if(nrow(landmarks_slash) >= 1){
      par_landmarks.slash <- lapply(1:nrow(landmarks_slash), function(landmark_i){
        if(!quiet){
          if((landmark_i %% 100) == 0) print(paste0(landmark_i, " / ", nrow(landmarks_slash)))
        } 
        landmarks_slash_i <- landmarks_slash[landmark_i,]
        alt_names <- strsplit(landmarks_slash_i$name, breaks)[[1]]
        df_spread <- lapply(1:length(alt_names), function(i) landmarks_slash_i) %>% do.call(what = "rbind")
        df_spread$name <- alt_names
        return(df_spread)  
      }) %>% do.call(what = "rbind")
      
      par_landmarks.slash$name <- par_landmarks.slash$name %>% 
        str_replace_all("/", " / ") %>%
        str_replace_all("-", " ") %>%
        str_replace_all("[[:punct:]]", "") %>%
        str_replace_all("[^[:alnum:]| ]", "") %>% 
        str_replace_all("\\|","") %>%
        str_squish 
      
      # We don't add to landmarks yet. We added to list of parallel landmarks
      # and use decision making rules from parallel as to whether we add.
      
      ### ADD THEM NOW, SO IMPELMENT N/SKIP AND PARALLEL LANDMARKS TO THESE
      
      ## Remove if short
      par_landmarks.slash <- par_landmarks.slash[nchar(par_landmarks.slash$name) >= 3,]
      
      ## Remove if name exists
      if(parallel.add_only_if_name_new){
        par_landmarks.slash <- par_landmarks.slash[!(par_landmarks.slash$name %in% landmarks$name),]
      }
      
      ## Add
      if(nrow(par_landmarks.slash) > 0){
        landmarks <- list(landmarks, 
                          par_landmarks.slash) %>%
          do.call(what = "rbind")
      }
      
    } 
    
  }
  
  landmarks$number_words <- str_count(landmarks$name, "\\S+")
  
  # 3. Text Cleaning -----------------------------------------------------------
  if(!quiet) print("Clean Text")
  
  # Clean landmark name
  landmarks$name <- landmarks$name %>% 
    str_replace_all("/", " / ") %>%
    str_replace_all("-", " ") %>%
    str_replace_all("[[:punct:]]", "") %>%
    str_replace_all("[^[:alnum:]| ]", "") %>% 
    str_replace_all("\\|","") %>%
    str_squish 
  
  # 4. Remove landmarks --------------------------------------------------------
  if(!quiet) print("Removing landmarks")
  
  #### Landmarks must be 2 or more characters
  landmarks <- landmarks[nchar(landmarks$name) >= 2,]
  
  #### Remove certain types
  types_rm_rx                  <- types_rm      %>% paste(collapse="|") 
  types_rm.except_with_type_rx <- types_rm.except_with_type %>% paste(collapse="|") 
  types_rm.except_with_name_rx <- types_rm.except_with_name %>% paste(collapse="|") 
  
  landmarks <- landmarks[!grepl(types_rm_rx, landmarks$type) | 
                           grepl(types_rm.except_with_type_rx, landmarks$type) | 
                           grepl(types_rm.except_with_name_rx, landmarks$name),]
  
  # 5. N-Grams and Skip-Grams --------------------------------------------------
  
  # ** 5.1 Create N-Grams and Skip-Grams ---------------------------------------
  if(!quiet) print("Create N-Grams and Skip-Grams")
  
  #### N-grams
  # Only make ngrams if the number of words in the landmark name is between
  # N_words.min and N_words.max. Additionally, only make n-grams of length 2-3.
  
  # Grab landmarks to make landmarks from
  
  landmarks_for_ngrams_df <- landmarks %>%
    as.data.frame() %>%
    filter(number_words %in% grams.min_words:grams.max_words)
  
  ## TODO: GENERALIZE!!
  if(is.null(landmarks_for_ngrams_df$lat[1])){
    landmarks_for_ngrams_df <- landmarks_for_ngrams_df %>%
      dplyr::rename(lon = coords.x1,
                    lat = coords.x2)
  }
  
  # Make dataframe, where each row is an n-gram, and includes all the other
  # variables from the landmark dataframe (lat, lon, type, etc). 
  make_ngram_df <- function(df){
    # Function for making an n-gram dataframe
    n_gram_df <- df %>%
      dplyr::pull(name) %>%
      tokens(remove_symbols = F, remove_punct = F) %>% 
      tokens_ngrams(n=2:3, concatenator = " ") %>% # TODO: parameterize 2:3
      as.list() %>%
      lapply(function(x) x %>% t %>% as.data.frame()) %>%
      bind_rows() %>%
      bind_cols(df) %>%
      dplyr::rename(name_original = name) %>%
      pivot_longer(c(-name_original, -type, -number_words, -lat, -lon),
                   names_to = "name_iter_N", values_to = "name") %>%
      filter(!is.na(name)) %>%
      filter(name != name_original) %>%
      dplyr::select(-name_iter_N)
    
    return(n_gram_df)
  }
  
  if(!quiet) print("Make N Grams")
  n_gram_df <- make_gram_df_chunks(landmarks_for_ngrams_df, 1000, make_ngram_df)
  
  #### Skip-grams
  # Grab landmarks to make landmarks from
  landmarks_for_skipgrams_df <- landmarks %>%
    as.data.frame() %>%
    filter(number_words %in% grams.min_words:grams.max_words)
  
  ## TODO: GENERALIZE!!
  if(is.null(landmarks_for_skipgrams_df$lat[1])){
    landmarks_for_skipgrams_df <- landmarks_for_skipgrams_df %>%
      dplyr::rename(lon = coords.x1,
                    lat = coords.x2)
  }
  
  # Make dataframe, where each row is an n-gram, and includes all the other
  # variables from the landmark dataframe (lat, lon, type, etc)
  make_skipgram_df <- function(df){
    skip_gram_df <- df %>%
      dplyr::pull(name) %>%
      tokens(remove_symbols = F, remove_punct = F) %>% 
      tokens_skipgrams(n=2:3, 
                       skip=0:4, 
                       concatenator = " ") %>%
      as.list() %>%
      lapply(function(x) x %>% t %>% as.data.frame()) %>%
      bind_rows() %>%
      bind_cols(df) %>%
      dplyr::rename(name_original = name) %>%
      pivot_longer(c(-name_original, -type, -number_words, -lat, -lon),
                   names_to = "name_iter_N", values_to = "name") %>%
      filter(!is.na(name)) %>%
      filter(name != name_original) %>%
      dplyr::select(-name_iter_N)
    
    return(skip_gram_df)
  }
  
  if(!quiet) print("Make Skip Grams")
  skip_gram_df <- make_gram_df_chunks(landmarks_for_skipgrams_df, 1000, make_skipgram_df)
  
  if(grams.skip_gram_first_last_word_match){
    skip_gram_df <- skip_gram_df %>%
      filter(word(name_original,1) == word(name,1),
             word(name_original,-1) == word(name,-1))
  }
  
  #### Append
  # Append and prep dataframe
  landmarks_grams <- bind_rows(n_gram_df,
                               skip_gram_df) %>%
    unique() %>%
    group_by(name) %>%
    mutate(name_N = n()) %>%
    ungroup()
  
  coordinates(landmarks_grams) <- ~lon+lat
  crs(landmarks_grams) <- CRS(crs_distance)
  
  # ** 5.2 Determine Which N/Skip-Grams to Add to Dictionary -------------------
  if(!quiet) print("Add N-Grams and Skip Grams")
  
  #### If two words and one word is one letter, remove
  remove <- (str_count(landmarks_grams$name, "\\S+") %in% 2) &
    ((nchar(word(landmarks_grams$name, 1)) %in% 1) |
       (nchar(word(landmarks_grams$name, -1)) %in% 1))
  
  landmarks_grams <- landmarks_grams[!remove,]
  
  #### Remove names if already in landmark dictionary
  if(grams.add_only_if_name_new){
    landmarks_grams <- landmarks_grams[!(landmarks_grams$name %in% landmarks$name),]
  }
  
  #### Separate into unique/non-unique  
  if(grams.add_only_if_specific){
    
    if(!quiet) print("N/Skip-Grams Landmarks: General Specific Check")
    
    # TODO: Speed gains if do in chunks, as large spdf slowing things down.
    # so chunks by name.
    landmarks_grams_all <- extract_dominant_cluster_all(landmarks_grams,
                                                        collapse_specific_coords = F,
                                                        return_general_landmarks = "none",
                                                        quiet = F)
    landmarks_grams_all$general_specific <- NA
    
  } else{
    landmarks_grams_all <- landmarks_grams
    landmarks_grams_all$general_specific <- NA
  }
  
  #### Append to landmark dictionary
  ## Ensure have same variables
  landmarks$general_specific <- NA
  landmarks$name_original <- landmarks$name
  landmarks$name_N <- 1
  
  landmarks <- list(landmarks,
                    landmarks_grams_all) %>% 
    do.call(what = "rbind")
  
  # 6. Create Parallel Landmarks -----------------------------------------------
  
  # ** 6.1 All Landmarks ----------------------------------------------------------
  if(!quiet) print("Create Parallel Landmarks - All Landmarks")
  
  # **** 6.1.1 If beings/ends with certain word/phrase, remove word/phrase -----
  # If ends with words like bar, shops, sports bar, restaurant -- words that people
  # may not use when mentioning landmark --- exclude these words.
  
  #### Beginnings
  if(is.null(parallel.rm_begin)){
    par_landmarks.rm_begin <- NULL
  } else{
    words_rm_begin_rx <- paste0("^", parallel.rm_begin, "\\b") %>% paste(collapse="|")
    par_landmarks.rm_begin <- landmarks[grepl(words_rm_begin_rx, landmarks$name),]
    par_landmarks.rm_begin$name <- par_landmarks.rm_begin$name %>% str_replace_all(words_rm_begin_rx, "") %>% str_squish
  }
  
  #### Endlings
  if(is.null(parallel.rm_end)){
    par_landmarks.rm_end <- NULL
  } else{
    words_rm_end_rx <- paste0("\\b", parallel.rm_end, "$") %>% paste(collapse="|")
    par_landmarks.rm_end <- landmarks[grepl(words_rm_end_rx, landmarks$name),]
    par_landmarks.rm_end$name <- par_landmarks.rm_end$name %>% str_replace_all(words_rm_end_rx, "") %>% str_squish
  }
  
  # **** 6.1.2 Replace words with different versions ---------------------------
  # e.g., replace "center" with "centre" and vice versa
  
  #### Prep list of words
  if(is.null(parallel.word_diff)){
    word_diffs <- NULL
  } else if(parallel.word_diff %in% "default"){
    word_diffs <- read.csv("https://raw.githubusercontent.com/ramarty/Unique-Location-Extractor/master/data/word_differences.csv",
                           stringsAsFactors = F) 
    word_diffs <- lapply(1:nrow(word_diffs), function(i) c(word_diffs$version_1[i], word_diffs$version_2[i]))
    
    # User defined list
  } else{
    word_diffs <- parallel.word_diff
  }
  
  #### Replace words
  # replace in name AND name_original. This is helpful in algorithm when preferencing
  # matches with the original name
  if(is.null(word_diffs)){
    par_landmarks.word_diff <- NULL
  } else{
    par_landmarks.word_diff <- lapply(word_diffs, function(words_i){
      
      ## All combinations of word pairs
      word_combns <- combn(words_i,2)
      
      ## Loop through word pair combinations and swap
      landmarks_i <- lapply(1:ncol(word_combns), function(ci){
        v1 <- word_combns[,ci][1]
        v2 <- word_combns[,ci][2]
        
        landmarks_v1               <- landmarks[grepl(paste0("\\b",v1,"\\b"), landmarks$name),]
        landmarks_v1$name          <- landmarks_v1$name          %>% str_replace_all(v1, v2) %>% str_squish
        landmarks_v1$name_original <- landmarks_v1$name_original %>% str_replace_all(v1, v2) %>% str_squish
        
        landmarks_v2               <- landmarks[grepl(paste0("\\b",v2,"\\b"), landmarks$name),]
        landmarks_v2$name          <- landmarks_v2$name          %>% str_replace_all(v2, v1) %>% str_squish
        landmarks_v2$name_original <- landmarks_v2$name_original %>% str_replace_all(v2, v1) %>% str_squish
        
        landmarks_v12 <- list(landmarks_v1, landmarks_v2) %>% purrr::discard(nrow_0) %>% do.call(what = "rbind")
      }) %>%
        purrr::discard(is.null) %>%
        do.call(what = "rbind")
      
      return(landmarks_i)
      
    }) %>%
      purrr::discard(is.null) %>% 
      do.call(what="rbind")
  }
  
  # ** 6.2 If Landmark is Certain Type -----------------------------------------
  if(!quiet) print("Create Parallel Landmarks - If Type")
  
  # **** 6.2.1 Remove word beginings if landmark is certain type ---------------
  if(is.null(parallel.rm_begin_iftype)){
    par_landmarks.rm_begin_iftype <- NULL
  } else{
    par_landmarks.rm_begin_iftype <- lapply(1:length(parallel.rm_begin_iftype), function(i){
      
      words_i <- paste0("^", parallel.rm_begin_iftype[[i]]$words, "\\b") %>% paste(collapse = "|")
      type_i <- parallel.rm_begin_iftype[[i]]$type %>% paste(collapse = "|")
      
      landmarks_i <- landmarks[grepl(words_i, landmarks$name) & grepl(type_i, landmarks$type),]
      landmarks_i$name <- landmarks_i$name %>% str_replace_all(words_i, "") %>% str_squish
      
      return(landmarks_i)
      
    }) %>%
      purrr::discard(nrow_0) %>% 
      do.call(what="rbind")
  }
  
  # **** 6.2.2 Remove word endings if landmark is certain type -----------------
  if(is.null(parallel.rm_end_iftype)){
    par_landmarks.rm_end_iftype <- NULL
  } else{
    par_landmarks.rm_end_iftype <- lapply(1:length(parallel.rm_end_iftype), function(i){
      
      words_i <- paste0("\\b", parallel.rm_end_iftype[[i]]$words, "$") %>% paste(collapse = "|")
      type_i <- parallel.rm_end_iftype[[i]]$type %>% paste(collapse = "|")
      
      landmarks_i <- landmarks[grepl(words_i, landmarks$name) & grepl(type_i, landmarks$type),]
      landmarks_i$name <- landmarks_i$name %>% str_replace_all(words_i, "") %>% str_squish
      
      return(landmarks_i)
      
    }) %>%
      purrr::discard(nrow_0) %>% 
      do.call(what="rbind")
  }
  
  # **** 6.2.3 Swap out words if landmark is certain type ----------------------
  if(is.null(parallel.word_diff_iftype)){
    par_landmarks.word_diff_iftype <- NULL
  } else{
    par_landmarks.word_diff_iftype <- lapply(1:length(parallel.word_diff_iftype), function(i){
      
      ## Words and types
      words_i <- parallel.word_diff_iftype[[i]]$words
      type_i <- parallel.word_diff_iftype[[i]]$type
      
      ## All combinations of word pairs
      word_combns <- combn(words_i,2)
      
      ## Loop through word pair combinations and swap
      landmarks_i <- lapply(1:ncol(word_combns), function(ci){
        v1 <- word_combns[,ci][1]
        v2 <- word_combns[,ci][2]
        
        landmarks_v1 <- landmarks[grepl(paste0("\\b",v1,"\\b"), landmarks$name) & grepl(type_i, landmarks$type),]
        landmarks_v1$name <- landmarks_v1$name %>% str_replace_all(v1, v2) %>% str_squish
        landmarks_v1$name_original <- landmarks_v1$name_original %>% str_replace_all(v1, v2) %>% str_squish
        
        landmarks_v2 <- landmarks[grepl(paste0("\\b",v2,"\\b"), landmarks$name) & grepl(type_i, landmarks$type),]
        landmarks_v2$name <- landmarks_v2$name %>% str_replace_all(v2, v1) %>% str_squish
        landmarks_v2$name_original <- landmarks_v2$name_original %>% str_replace_all(v2, v1) %>% str_squish
        
        landmarks_v12 <- list(landmarks_v1, landmarks_v2) %>% purrr::discard(nrow_0) %>% do.call(what = "rbind")
      }) %>%
        purrr::discard(is.null) %>%
        do.call(what = "rbind")
      
      return(landmarks_i)
      
    }) %>%
      purrr::discard(is.null) %>% 
      do.call(what="rbind")
  }
  
  # ** 6.3 Add Types -----------------------------------------------------------
  if(!quiet) print("Create Parallel Landmarks - Add Type")
  
  # **** 6.3.1 Add type if landmark begins with word ---------------------------
  
  if(is.null(parallel.word_begin_addtype)){
    par_landmarks.word_begin_addtype <- NULL
  } else{
    par_landmarks.word_begin_addtype <- lapply(1:length(parallel.word_begin_addtype), function(i){
      
      words_i <- paste0("^", parallel.word_begin_addtype[[i]]$words, "\\b") %>% paste(collapse = "|")
      type_i <- parallel.word_begin_addtype[[i]]$type
      
      landmarks_i <- landmarks[grepl(words_i, landmarks$name),]
      landmarks_i$type <- paste0(landmarks_i$type, ";", type_i)
      
      return(landmarks_i)
      
    }) %>%
      purrr::discard(nrow_0) %>% 
      do.call(what="rbind")
    
  }
  
  # **** 6.3.2 Add type if landmark ends with word -----------------------------
  
  if(is.null(parallel.word_end_addtype)){
    par_landmarks.word_end_addtype <- NULL
  } else{
    par_landmarks.word_end_addtype <- lapply(1:length(parallel.word_end_addtype), function(i){
      
      words_i <- paste0("\\b", parallel.word_end_addtype[[i]]$words, "$") %>% paste(collapse = "|")
      type_i <- parallel.word_end_addtype[[i]]$type
      
      landmarks_i <- landmarks[grepl(words_i, landmarks$name),]
      if(nrow(landmarks_i) > 0) landmarks_i$type <- paste0(landmarks_i$type, ";", type_i)
      
      return(landmarks_i)
      
    }) %>%
      purrr::discard(nrow_0) %>% 
      do.call(what="rbind")
  }
  
  # ** 6.7 Add parallel landmarks to master list -------------------------------
  if(!quiet) print("Add Parallel Landmarks")
  
  #### Parallel landmarks: New Name
  par_landmarks_newname <- list(par_landmarks.rm_begin, 
                                par_landmarks.rm_end, 
                                par_landmarks.word_diff,
                                par_landmarks.word_diff_iftype, 
                                par_landmarks.rm_begin_iftype, 
                                par_landmarks.rm_end_iftype) %>%
    purrr::discard(is.null) %>% 
    do.call(what = "rbind")
  
  #### Remove if short
  par_landmarks_newname <- par_landmarks_newname[nchar(par_landmarks_newname$name) >= 3,]
  
  #### Remove if name exists
  if(parallel.add_only_if_name_new){
    par_landmarks_newname <- par_landmarks_newname[!(par_landmarks_newname$name %in% landmarks$name),]
  }
  
  #### Remove if general
  if(parallel.add_only_if_specific){
    if(!quiet) print("Parallel Landmarks: General Specific Check")
    
    par_landmarks_newname <- extract_dominant_cluster_all(par_landmarks_newname,
                                                          collapse_specific_coords = F,
                                                          return_general_landmarks = "none",
                                                          quiet = F)
    
  }
  
  #### Parallel landmarks: New Type
  par_landmarks_newtype <- list(par_landmarks.word_begin_addtype, 
                                par_landmarks.word_end_addtype) %>%
    purrr::discard(is.null) %>% 
    do.call(what = "rbind")
  
  #### Append parallel landmarks to master landmarks
  landmarks <- list(landmarks, par_landmarks_newname, par_landmarks_newtype) %>% 
    purrr::discard(is.null) %>%  
    do.call(what = "rbind")
  
  # 7. Final cleaning ----------------------------------------------------------
  if(!quiet) print("Final Cleaning")
  
  # Implement a final series of cleaning after different processes have added
  # landmark names. This includes cleaning the text and removing landmarks
  
  # ** 7.1 Remove with stopwords --------------------------------------------------
  # If has stop word and is two or less words, remove 
  stopwords <- paste0("\\b", stopwords(), "\\b") %>% paste(collapse="|")
  landmarks <- landmarks[!((grepl(stopwords, landmarks$name)) & (str_count(landmarks$name, "\\S+") <= 2)),]
  
  # ** 7.2 Remove if name contains/begins/ends with word -----------------------
  rm.contains_regex   <- paste0("\\b", rm.contains, "\\b")   %>% paste(collapse = "|") 
  rm.name_begin_regex <- paste0("\\b^", rm.name_begin, "\\b") %>% paste(collapse = "|") 
  rm.name_end_regex   <- paste0("\\b", rm.name_end, "$\\b")   %>% paste(collapse = "|") 
  
  landmarks <- landmarks[!grepl(rm.contains_regex, landmarks$name),]
  landmarks <- landmarks[!grepl(rm.name_begin_regex, landmarks$name),]
  landmarks <- landmarks[!grepl(rm.name_end_regex, landmarks$name),]
  
  # ** 7.3 Remove based on patterns --------------------------------------------
  landmarks <- landmarks[!(landmarks$name %in% ""),]
  landmarks <- landmarks[nchar(landmarks$name) > 1,]
  
  # ** 7.3 Final Text Cleaning -------------------------------------------------
  # TODO: Do we need this? Should just need in beginning?
  landmarks$name <- landmarks$name %>% 
    str_replace_all("/", " / ") %>%
    str_replace_all("[[:punct:]]", "") %>%
    str_replace_all("[^[:alnum:]| |/]", "") %>% 
    str_replace_all("\\|","") %>%
    str_squish 
  landmarks$name <- gsub("\\|","",landmarks$name)
  
  # ** 7.4 General/Specific ----------------------------------------------------
  if(!quiet) print("Separating into General and Specific")
  #aa <<- landmarks
  # TODO: Not sure if needed? As recalc in algorithm?
  #landmarks_out <- extract_dominant_cluster_all(landmarks,
  #                                              N_loc_limit = 500,
  #                                              collapse_specific_coords = F, # false because recalculate g/s in algorithm
  #                                              return_general_landmarks = "all",
  #                                              quiet = F)
  landmarks$general_specific <- NA
  
  # ** 7.5 Remove landmarks based on type of POS -------------------------------
  landmarks$uid <- 1:nrow(landmarks)
  
  ## Restrict to cases where potentially remove
  landmarks_rm <- landmarks[str_count(landmarks$name, '\\w+') %in% 1,] # one word
  landmarks_rm <- landmarks_rm[hunspell_check(landmarks_rm$name),] # spelled correctly
  landmarks_rm <- landmarks_rm[is.na(as.numeric(landmarks_rm$name)),] # is not number (eg, 87)
  
  ## Add parts of speech
  pos_df <- spacy_parse(landmarks_rm$name %>% unique(), tag = TRUE) %>%
    dplyr::select(token, pos, tag) %>%
    dplyr::rename(name = token) %>%
    unique()
  landmarks_rm <- merge(landmarks_rm, pos_df, by = "name")
  
  ## remove uids
  uid_to_remove <- landmarks_rm$uid[landmarks_rm$pos %in% pos_rm.all]
  landmarks <- landmarks[!(landmarks$uid %in% uid_to_remove),]
  
  ## remove uids except types
  for(i in 1:length(pos_rm.except_type)){
    pos_i <- pos_rm.except_type[[i]][[1]] 
    types_i <- pos_rm.except_type[[i]][[2]] %>% paste(collapse = "|")
    names_i <- pos_rm.except_type[[i]][[3]] 
    
    landmarks_rm_i <- landmarks_rm[landmarks_rm$pos %in% pos_i,]
    landmarks_rm_i <- landmarks_rm_i[!grepl(types_i, landmarks_rm_i$type),] # TODO: only do if not NULL?
    landmarks_rm_i <- landmarks_rm_i[!(landmarks_rm_i$name %in% names_i),] # TODO: only do if not NULL?
    
    # Remove landmarks
    landmarks <- landmarks[!(landmarks$uid %in% landmarks_rm_i$uid),]
  }
  
  # ** 7.6 Variables to output -------------------------------------------------
  landmarks@data <- landmarks@data %>%
    dplyr::select(name, type, general_specific, name_original)
  
  landmarks <- spTransform(landmarks, CRS(crs_out))
  
  # Remove landmarks that would highly raise likelihood of false positives
  #landmarks <- landmarks[!(landmarks$name %in% landmarks_to_remove),]
  #landmarks <- unique(landmarks)
  
  # Manually Change Coordinates for Some Locations 
  # This requires local knowledge, but can be guided by algorithm output. Eg, 
  # are there locations that are wrong very commonly?
  # Do this for minimal number of locations.
  
  # JKIA [put along entrance to JKIA, near mombasa rd]
  ###### too specific, do separately.
  #landmarks$lat[landmarks$name == "jkia"] <- -1.344459 
  #landmarks$lon[landmarks$name == "jkia"] <- 36.902554
  
  return(landmarks)
}

