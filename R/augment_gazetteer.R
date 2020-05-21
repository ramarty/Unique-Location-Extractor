# Augment Gazetteer

# TODO:
# 1. More efficient way of appending sp objects than do.call(what="rbind")?

library(raster)
library(rgdal)
library(dplyr)
library(readr)
library(quanteda)
library(stringr)
library(stringi)
library(rgeos)
library(hunspell)

augment_gazetteer <- function(landmarks,
                              landmarks.name_var = "name",
                              landmarks.type_var = "type",
                              grams_min_words = 3,
                              grams_max_words = 6,
                              skip_grams_first_last_word = T,
                              types_remove = c("route", "road", "toilet", "political", "locality", "neighborhood"),
                              types_always_keep = c("flyover"),
                              names_always_keep = c("flyover"),
                              parallel.rm_begin = c(stopwords("en"), c("near","at","the", "towards", "near")),
                              parallel.rm_end = c("bar", "shops", "restaurant","sports bar","hotel", "bus station"),
                              parallel.rm_begin_iftype = "", # NOT YET IMPLEMENTED
                              parallel.rm_end_iftype = list(list(words = c("stage", "bus stop"), type = "transit_station")),
                              parallel.word_diff_iftype = list(list(words = c("stage", "bus stop", "bus station"), type = "transit_station")),
                              parallel.word_end_addtype = list(list(words = c("stage", "bus stop", "bus station"), type = "stage")),
                              rm.contains = c("road", "rd"),
                              rm.name_begin = c(stopwords("en"), c("near","at","the", "towards", "near")),
                              rm.name_end = c("highway", "road", "rd", "way", "ave", "avenue", "street", "st"),
                              crs_distance){
  
  # DESCRIPTION: Augments landmark gazetteer
  # ARGS:
  # landmarks: Spatial Points Dataframe (or sf equivalent) of landmarks.
  # landmarks.name_var: Name of variable indicating name of landmark
  # landmarks.type_var: Name of variable indicating type of landmark
  # grams_min_words: Minimum number of words in name to make n/skip-grams out of name
  # grams_max_words: Maximum number of words in name to make n/skip-grams out of name.
  #                  Setting a cap helps to reduce spurious landmarks that may come
  #                  out of really long names
  # skip_grams_first_last_word: For skip-grams, should first and last word be the
  #                             same as the original word? (TRUE/FASLE)
  # types_remove: If landmark has one of these types, remove - unless 'types_always_keep' 
  #               or 'names_always_keep' prevents removing.
  # types_always_keep: landmark types to always keep. This parameter only becomes
  #                    relevant in cases where a landmark has more than one type.
  #                    If a landmark has both a "types_remove" and a "types_always_keep" 
  #                    landmark, this landmark will be kept.
  # names_always_keep: landmark names to always keep. This parameter only
  #                    becomes relevant in cases where a landmark is one of 
  #                    "types_remove." Here, we keep the landmark if "names_always_keep" 
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

  # 1. Checks ------------------------------------------------------------------
  if(class(landmarks)[1] %in% "sf") landmarks <- landmarks %>% as("Spatial")
  
  if(!(class(landmarks)[1] %in% c("SpatialPointsDataFrame", "sf"))){
    stop("landmarks must be a SpatialPointsDataFrame or an sf object")
  }
  
  # 2. Prep landmark object ----------------------------------------------------
  
  #### Prep variables
  landmarks$name <- landmarks[[landmarks.name_var]]
  landmarks$type <- landmarks[[landmarks.type_var]]
  landmarks$number_words <- str_count(landmarks$name, "\\S+")
  landmarks@data <- landmarks@data %>%
    dplyr::select(name, type, number_words)
  
  #### Prep spatial
  landmarks <- spTransform(landmarks, CRS(crs_distance))
  
  # 3. Text Cleaning -----------------------------------------------------------
  # Remove extract whitespace
  landmarks$name_withslash <- landmarks$name %>% 
    str_replace_all("/", " / ") %>%
    #str_replace_all("[[:punct:]]", "") %>%
    #str_replace_all("[^[:alnum:]| |/]", "") %>% 
    str_replace_all("\\|","") %>%
    str_squish 
  
  landmarks$name <- landmarks$name %>% 
    str_replace_all("/", " / ") %>%
    str_replace_all("-", " ") %>%
    str_replace_all("[[:punct:]]", "") %>%
    str_replace_all("[^[:alnum:]| ]", "") %>% 
    str_replace_all("\\|","") %>%
    str_squish 
  
  # 4. Remove landmarks --------------------------------------------------------
  
  #### Landmarks must be 2 or more characters
  landmarks <- landmarks[nchar(landmarks$name) >= 2,]
  
  #### Remove certain types
  types_remove_regex      <- types_remove      %>% paste(collapse="|") 
  types_always_keep_regex <- types_always_keep %>% paste(collapse="|") 
  names_always_keep_regex <- types_always_keep %>% paste(collapse="|") 
  
  landmarks <- landmarks[!grepl(types_remove_regex, landmarks$type) | 
                           grepl(types_always_keep_regex, landmarks$type) | 
                           grepl(names_always_keep_regex, landmarks$name),]
  
  # 5. N-Grams and Skip-Grams --------------------------------------------------
  
  # ** 5.1 Create N-Grams and Skip-Grams ---------------------------------------
  
  #### N-grams
  # Only make ngrams if the number of words in the landmark name is between
  # N_words.min and N_words.max. Additionally, only make n-grams of length 2-3.
  
  # Grab landmarks to make landmarks from
  landmarks_for_ngrams_df <- landmarks %>%
    as.data.frame() %>%
    filter(number_words %in% grams_min_words:grams_min_words)
  
  ## TODO: GENERALIZE!!
  if(is.null(landmarks_for_ngrams_df$lat[1])){
    landmarks_for_ngrams_df <- landmarks_for_ngrams_df %>%
      dplyr::rename(lon = coords.x1,
                    lat = coords.x2)
  }
  
  # Make dataframe, where each row is an n-gram, and includes all the other
  # variables from the landmark dataframe (lat, lon, type, etc)
  n_gram_df <- landmarks_for_ngrams_df %>%
    dplyr::pull(name) %>%
    tokens(remove_symbols = F, remove_punct = F) %>% 
    tokens_ngrams(n=2:3, concatenator = " ") %>% # TODO: parameterize 2:3
    lapply(t %>% as.data.frame) %>% 
    bind_rows() %>%
    bind_cols(landmarks_for_ngrams_df) %>%
    dplyr::rename(name_original = name) %>%
    pivot_longer(c(-name_original, -type, -number_words, -lat, -lon),
                 names_to = "name_iter_N", values_to = "name") %>%
    filter(!is.na(name)) %>%
    filter(name != name_original) %>%
    dplyr::select(-name_iter_N)
  
  #### Skip-grams
  # Grab landmarks to make landmarks from
  landmarks_for_skipgrams_df <- landmarks %>%
    as.data.frame() %>%
    filter(number_words %in% grams_min_words:grams_min_words)
  
  ## TODO: GENERALIZE!!
  if(is.null(landmarks_for_skipgrams_df$lat[1])){
    landmarks_for_skipgrams_df <- landmarks_for_skipgrams_df %>%
      dplyr::rename(lon = coords.x1,
                    lat = coords.x2)
  }
  
  # Make dataframe, where each row is an n-gram, and includes all the other
  # variables from the landmark dataframe (lat, lon, type, etc)
  
  skip_gram_df <- landmarks_for_skipgrams_df %>%
    dplyr::pull(name) %>%
    tokens(remove_symbols = F, remove_punct = F) %>% 
    tokens_skipgrams(n=2:3, 
                     skip=0:4, 
                     concatenator = " ") %>%
    lapply(t %>% as.data.frame) %>% 
    bind_rows() %>%
    bind_cols(landmarks_for_skipgrams_df) %>%
    dplyr::rename(name_original = name) %>%
    pivot_longer(c(-name_original, -type, -number_words, -lat, -lon),
                 names_to = "name_iter_N", values_to = "name") %>%
    filter(!is.na(name)) %>%
    filter(name != name_original) %>%
    dplyr::select(-name_iter_N)
  
  if(skip_grams_first_last_word){
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
  
  #### If two words and one word is one letter, remove
  remove <- (str_count(landmarks_grams$name, "\\S+") %in% 2) &
    ((nchar(word(landmarks_grams$name, 1)) %in% 1) |
       (nchar(word(landmarks_grams$name, -1)) %in% 1))
  
  landmarks_grams <- landmarks_grams[!remove,]
  
  #### Separate into unique/non-unique  
  # We keep all unique skip grams, then determine which to keep among nonunique grams
  landmarks_grams_unique    <- landmarks_grams[landmarks_grams$name_N == 1,]
  landmarks_grams_unique$general_specific <- "specific"
  
  landmarks_grams_nonunique <- landmarks_grams[landmarks_grams$name_N > 1,]
  
  #### Amount non-unique, define as general or specific (looking for dominant spatial cluster)
  print(length(unique(landmarks_grams_nonunique$name)))
  counter_i <<- 1
  landmarks_grams_nonunique_gs <- lapply(unique(landmarks_grams_nonunique$name), function(name){
    #print(name)
    out <- extract_dominant_cluster(landmarks_grams_nonunique[landmarks_grams_nonunique$name %in% name,],
                                    collapse_specific_coords = T,
                                    return_general_landmarks = "all")
    
    # where are we?
    counter_i <<- counter_i + 1
    if((counter_i %% 50) == 0) print(counter_i)
    
    if(nrow(out) == 0) out <- NULL
    return(out)
  }) %>%
    purrr::discard(is.null) %>%
    do.call(what="rbind")
  
  #### Append to landmark dictionary
  ## Ensure have same variables
  landmarks$general_specific <- NA
  landmarks$name_original <- landmarks$name
  landmarks$name_N <- 1
  
  landmarks_grams_unique$name_withslash <- landmarks_grams_unique$name
  landmarks_grams_nonunique_gs$name_withslash <- landmarks_grams_nonunique_gs$name
  
  landmarks <- list(landmarks,
                    landmarks_grams_unique,
                    landmarks_grams_nonunique_gs) %>% 
    do.call(what = "rbind")
  
  # 6. Create Parallel Landmarks -----------------------------------------------
  
  # ** 6.1 Add type if landmark ends with word ---------------------------------
  par_landmarks.word_end_addtype <- lapply(1:length(parallel.word_end_addtype), function(i){
    
    words_i <- paste0("\\b", parallel.word_end_addtype[[i]]$words, "$") %>% paste(collapse = "|")
    type_i <- parallel.word_end_addtype[[i]]$type
    
    landmarks_i <- landmarks[grepl(words_i, landmarks$name),]
    landmarks_i$type <- paste0(landmarks_i$type, ";", type_i)
    
    return(landmarks_i)
    
  }) %>%
    do.call(what="rbind")
  
  # ** 6.2 Remove word endings if landmark is certain type ---------------------
  par_landmarks.rm_end_iftype <- lapply(1:length(parallel.rm_end_iftype), function(i){
    
    words_i <- paste0("\\b", parallel.rm_end_iftype[[i]]$words, "$") %>% paste(collapse = "|")
    type_i <- parallel.rm_end_iftype[[i]]$type
    
    landmarks_i <- landmarks[grepl(words_i, landmarks$name) & grepl(type_i, landmarks$type),]
    landmarks_i$name <- landmarks_i$name %>% str_replace_all(words_i, "") %>% str_squish
    
    return(landmarks_i)
    
  }) %>%
    do.call(what="rbind")
  
  # ** 6.3 Swap out words if landmark is certain type --------------------------
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
      
      landmarks_v2 <- landmarks[grepl(paste0("\\b",v2,"\\b"), landmarks$name) & grepl(type_i, landmarks$type),]
      landmarks_v2$name <- landmarks_v2$name %>% str_replace_all(v2, v1) %>% str_squish
      
      landmarks_v12 <- list(landmarks_v1, landmarks_v2) %>% purrr::discard(nrow_0) %>% do.call(what = "rbind")
    }) %>%
      purrr::discard(is.null) %>%
      do.call(what = "rbind")
    
    return(landmarks_i)
    
  }) %>%
    purrr::discard(is.null) %>% 
    do.call(what="rbind")
  
  # ** 6.4 Replace words with different versions --------------------------------------
  # e.g., replace "center" with "centre" and vice versa

  word_differences <- read.csv("https://raw.githubusercontent.com/ramarty/Unique-Location-Extractor/master/data/word_differences.csv")
  
  par_landmarks.worddiff <- lapply(1:nrow(word_differences), function(i){
    
    ## Grab words
    word_differences_i <- word_differences[i,]
    version_1 <- word_differences_i$version_1
    version_2 <- word_differences_i$version_2
    
    ## Replace v1 with v2
    df_v1 <- landmarks[grepl(paste0("\\b",version_1,"\\b"), landmarks$name),]
    df_v1$name <- gsub(version_1, version_2, df_v1$name)
    
    ## Replace v2 with v1
    df_v2 <- landmarks[grepl(paste0("\\b",version_2,"\\b"), landmarks$name),]
    df_v2$name <- gsub(version_2, version_1, df_v2$name)
    
    ## Append
    df_out <- list(df_v1, df_v2) %>% purrr::discard(nrow_0) %>% do.call(what = "rbind")
    
    return(df_out)
  }) %>%
    purrr::discard(is.null) %>%
    do.call(what = "rbind")
  
  # ** 6.5 If beings/ends with certain word/phrase, remove word/phrase ----------------
  # If ends with words like bar, shops, sports bar, restaurant -- words that people
  # may not use when mentioning landmark --- exclude these words.
  
  words_rm_begin_regex <- paste0("^", parallel.rm_begin, "\\b") %>% paste(collapse="|")
  words_rm_end_regex <- paste0("\\b", parallel.rm_end, "$") %>% paste(collapse="|")
  
  words_remove_regex <- paste(c(words_rm_begin_regex, words_rm_end_regex), collapse = "|")
  
  par_landmarks.rm_endings <- landmarks[grepl(words_remove_regex, landmarks$name),]
  par_landmarks.rm_endings$name <- par_landmarks.rm_endings$name %>% str_replace_all(words_remove_regex, "") %>% str_squish
  
  # ** 6.6 If landmark has slash, separate into separate landmarks -------------
  # TODO: None appearing? Eliminating before??
  if(F){
    landmarks_with_slash <- landmarks[grepl("/", landmarks$name_withslash),]
    
    par_landmarks.slash <- lapply(1:nrow(landmarks_with_slash), function(landmark_i){
      landmarks_with_slash_i <- landmarks_with_slash[landmark_i,]
      alt_names <- strsplit(landmarks_with_slash_i$name_withslash, "/")[[1]]
      df_spread <- lapply(1:length(alt_names), function(i) landmarks_with_slash_i) %>% bind_rows
      df_spread$name <- alt_names
      return(df_spread)  
    }) %>% bind_rows
  }
  
  # ** 6.7 Add parallel landmarks to master list -------------------------------
  
  #### Append and prep parallel landmarks
  # 1. Some processes of producing parallel landmarks involved removing words. Here,
  #    we ensure that names meet a minimum length
  # 2. Only add parallel landmark if name doesn't conflict with existing landmark
  
  par_landmarks <- list(par_landmarks.word_end_addtype, 
                        par_landmarks.rm_end_iftype, 
                        par_landmarks.word_diff_iftype, 
                        par_landmarks.worddiff, 
                        # par_landmarks.slash
                        par_landmarks.rm_endings) %>%
    purrr::discard(is.null) %>% 
    do.call(what = "rbind")
  
  par_landmarks <- par_landmarks[nchar(par_landmarks$name) >= 3,]
  par_landmarks <- par_landmarks[!(par_landmarks$name %in% landmarks$name),]
  
  #### Append to all landmarks
  landmarks <- list(landmarks, par_landmarks) %>% do.call(what = "rbind")
  
  # 7. Final cleaning ----------------------------------------------------------
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
  landmarks$general_specific <- NULL
  print(length(unique(landmarks$name)))
  counter_i <<- 1
  landmarks_out <- lapply(unique(landmarks$name), function(name){
    out <- extract_dominant_cluster(landmarks[landmarks$name %in% name,],
                                    N_loc_limit = 500,
                                    collapse_specific_coords = T,
                                    return_general_landmarks = "all")
    
    # where are we?
    counter_i <<- counter_i + 1
    if((counter_i %% 50) == 0) print(counter_i)
    
    if(nrow(out) == 0) out <- NULL
    return(out)
  }) %>%
    purrr::discard(is.null) %>%
    do.call(what="rbind")
  
  # ** 7.5 Variables to output -------------------------------------------------
  landmarks_out@data <- landmarks_out@data %>%
    dplyr::select(name, type, general_specific, name_original)
  
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
  
  return(landmarks_out)
}

