# Augment Gazetteer

library(raster)
library(rgdal)
library(dplyr)
library(readr)
library(quanteda)
library(stringr)
library(stringi)
library(rgeos)
library(hunspell)

if(F){
  landmarks <- readRDS(file.path(algorithm_inputs, "gazetteers_raw","merged", "gazetter_allsources_raw.Rds"))
  landmarks <- landmarks[!is.na(landmarks$lat),]
  coordinates(landmarks) <- ~lon+lat
  crs(landmarks) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
  
  crs_distance <- "+init=epsg:21037"
}
# landmarks <- readRDS(file.path(algorithm_inputs, "gazetteers_raw","merged", "gazetter_allsources_raw.Rds"))

augment_gazetteer <- function(landmarks,
                              landmarks.name_var = "name",
                              landmarks.type_var = "type",
                              types_remove = c("route", "road", "toilet", "political", "locality", "neighborhood"),
                              types_always_keep = c("flyover"),
                              names_always_keep = c("flyover"),
                              crs_distance,
                              skip_grams_first_last_word,
                              landmarks_to_remove){
  
  # Augments Gazetteer
  # landmarks: Spatial Points Dataframe (or sf equivalent) of landmarks.
  # crs_distance: Projection
  # types_remove: landmark types to remove
  # types_always_keep: landmark types to always keep. This parameter only
  #   becomes relevant in cases where a landmark has more than one type. If 
  #   a landmark has both a "types_remove" and a "types_always_keep" landmark,
  #   this landmark will be kept.
  # names_always_keep: landmark names to always keep. This parameter only
  #   becomes relevant in cases where a landmark is one of "types_remove." Here,
  #   we keep the landmark if "names_always_keep" is somewhere in the name. For
  #   example, if the landmark is a road but has flyover in the name, we may 
  #   want to keep the landmark as flyovers are small spatial areas.
  # skip_grams_first_last_word: For skip grams, only keep ones where first and
  #   last words are included. 
  
  # 1. Checks ------------------------------------------------------------------
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
  if(class(landmarks)[1] %in% "sf") landmarks <- landmarks %>% as("Spatial")
  landmarks <- spTransform(landmarks, CRS(crs_distance))
  
  # 1. Text Cleaning -----------------------------------------------------------
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
  
  # 2. Remove Landmarks --------------------------------------------------------
  
  #### Landmarks must be 2 or more characters
  landmarks <- landmarks[nchar(landmarks$name) >= 2,]
  
  #### Remove certain types
  types_remove_regex      <- types_remove      %>% paste(collapse="|") 
  types_always_keep_regex <- types_always_keep %>% paste(collapse="|") 
  names_always_keep_regex <- types_always_keep %>% paste(collapse="|") 
  
  landmarks <- landmarks[!grepl(types_remove_regex, landmarks$type) | 
                           grepl(types_always_keep_regex, landmarks$type) | 
                           grepl(names_always_keep_regex, landmarks$name),]
  
  # 3. N-Grams and Skip-Grams --------------------------------------------------
  
  # ** 3.1 Create N-Grams and Skip-Grams ---------------------------------------
  
  N_words.min <- 3
  N_words.max <- 6
  
  #### N-grams
  # Only make ngrams if the number of words in the landmark name is between
  # N_words.min and N_words.max. Additionally, only make n-grams of length 2-3.
  
  # Grab landmarks to make landmarks from
  landmarks_for_ngrams_df <- landmarks %>%
    as.data.frame() %>%
    filter(number_words %in% N_words.min:N_words.max)
  
  # Make dataframe, where each row is an n-gram, and includes all the other
  # variables from the landmark dataframe (lat, lon, type, etc)
  n_gram_df <- landmarks_for_ngrams_df %>%
    dplyr::pull(name) %>%
    tokens(remove_symbols = F, remove_punct = F) %>% 
    tokens_ngrams(n=2:3, concatenator = " ") %>%
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
    filter(number_words %in% N_words.min:N_words.max)
  
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
  
  # ** 3.2 Determine Which N/Skip-Grams to Add to Dictionary -------------------
  
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
  landmarks_grams_nonunique_gs <- lapply(unique(landmarks_grams_nonunique$name), function(name){
    out <- extract_dominant_cluster(landmarks_grams_nonunique[landmarks_grams_nonunique$name %in% name,],
                             collapse_specific_coords = T,
                             return_general_landmarks = "all")
    if(nrow(out) == 0) out <- NULL
    return(out)
  }) %>%
    purrr::discard(is.null) %>%
    do.call(what="rbind")

  #### Append to landmark dictionary
  landmarks <- bind_rows(landmarks,
                         landmarks_ngrams_skipgrams_unique %>% mutate(general_specific = "specific"),
                         landmarks_ngrams_skipgrams_nonunique_close_df %>% mutate(general_specific = "specific"),
                         landmarks_ngrams_skipgrams_nonunique_notclose_general_df,
                         landmarks_ngrams_skipgrams_nonunique_notclose_cluster)
  
  # Create Parallel Landmarks ----------------------------------------------------
  
  # OTHER SOLUTION. USE A TREE APPROACH. IF LANDMARK IS GM STATE BUT MENTIONS GM,
  # SAY: GM IS IN A LANDMARK. IF MULTIPLE LANDMARKS, SEE IF THEY ARE CLOSE
  
  # If bus station and has word stage, remove word "stage" --------------------- 
  bus.stages.with.stage <- landmarks[grepl("stage", landmarks$name) & grepl("transit_station",landmarks$type),]
  bus.stages.with.stage$name <- gsub("stage", "", bus.stages.with.stage$name) %>% str_squish
  bus.stages.with.stage <- bus.stages.with.stage[is.na(as.numeric(bus.stages.with.stage$name)),] # Remove numbers
  parallel.landmarks <- bus.stages.with.stage
  
  # If bus station and has word "bus stop", remove word "bus stop" -------------
  bus.stages.with.bus.stop <- landmarks[grepl("bus stop", landmarks$name) & grepl("transit_station",landmarks$type),]
  bus.stages.with.bus.stop$name <- gsub("bus stop", "", bus.stages.with.bus.stop$name) %>% str_squish
  bus.stages.with.bus.stop <- bus.stages.with.bus.stop[is.na(as.numeric(bus.stages.with.bus.stop$name)),] # Remove numbers
  parallel.landmarks <- bind_rows(parallel.landmarks, bus.stages.with.bus.stop)
  
  # If bus station and has word "bus station", remove word "bus station" -------
  bus.stages.with.bus.station <- landmarks[grepl("bus station", landmarks$name) & grepl("transit_station",landmarks$type),]
  bus.stages.with.bus.station$name <- gsub("bus station", "", bus.stages.with.bus.station$name) %>% str_squish
  bus.stages.with.bus.station <- bus.stages.with.bus.station[is.na(as.numeric(bus.stages.with.bus.station$name)),] # Remove numbers
  parallel.landmarks <- bind_rows(parallel.landmarks, bus.stages.with.bus.station)
  
  # If NOT bus station and ENDS WITH WORD stage, remove word "stage" [but add "stage_added" to Type] --------------------- 
  not.bus.stages.with.stage <- landmarks[grepl("\\bstage$", landmarks$name) & !grepl("transit_station",landmarks$type),]
  not.bus.stages.with.stage$name <- gsub("stage", "", not.bus.stages.with.stage$name) %>% str_squish
  not.bus.stages.with.stage$type <- paste(not.bus.stages.with.stage$Type, ";stage_added", sep="")
  not.bus.stages.with.stage <- not.bus.stages.with.stage[is.na(as.numeric(not.bus.stages.with.stage$name)),] # Remove numbers
  parallel.landmarks <- bind_rows(parallel.landmarks, not.bus.stages.with.stage)
  
  # If bus station has word "bus station", replace "bus station" with "stage" ----
  bus.station.replace.with.stage <- landmarks[grepl("bus station", landmarks$name) & grepl("transit_station",landmarks$type),]
  bus.station.replace.with.stage$name <- gsub("bus station", "stage", bus.station.replace.with.stage$name) %>% str_squish
  parallel.landmarks <- bind_rows(parallel.landmarks, bus.station.replace.with.stage)
  
  # Replace words with "centre" as "center" ------------------------------------
  
  landmarks_re <- landmarks[grepl("re$",landmarks$name),]
  
  
  df.center <- landmarks[grepl("\\bcenter\\b", landmarks$name),]
  df.center$name <- gsub("center", "centre", df.center$name)
  parallel.landmarks <- bind_rows(parallel.landmarks, df.center)
  
  # Replace words with "center" as "centre" ------------------------------------
  df.centre <- landmarks[grepl("\\bcentre\\b", landmarks$name),]
  df.centre$name <- gsub("centre", "center", df.centre$name)
  parallel.landmarks <- bind_rows(parallel.landmarks, df.centre)
  
  # If ends with certain word/phrase, remove word/phrase -------------------------
  # If ends with words like bar, shops, sports bar, restaurant -- words that people
  # may not use when mentioning landmark --- exclude these words.
  # \\bword$ indicates: (start of word)(word)(end of line)
  words_remove <- c("bar","shops","restaurant","sports bar","hotel", "bus station") # but bus station probably is just nieghborhood name, so use that.
  words_remove_regex <- paste0("\\b", words_remove, "$") %>% paste(collapse="|")
  
  landmarks.end.words.removed <- landmarks[grepl(words_remove_regex, landmarks$name),]
  landmarks.end.words.removed$name <- landmarks.end.words.removed$name %>% str_replace_all(words_remove_regex, "") %>% str_squish
  landmarks.end.words.removed <- landmarks.end.words.removed[!(landmarks.end.words.removed$name %in% ""),]
  landmarks.end.words.removed <- landmarks.end.words.removed[nchar(landmarks.end.words.removed$name) >= 3,]
  
  parallel.landmarks <- bind_rows(parallel.landmarks, landmarks.end.words.removed)
  
  # If landmark has slash, separate into separate landmarks ----------------------
  landmarks_with_slash <- landmarks[grepl("/", landmarks$name_withslash),]
  
  landmarks_with_slash_separated <- lapply(1:nrow(landmarks_with_slash), function(landmark_i){
    landmarks_with_slash_i <- landmarks_with_slash[landmark_i,]
    alt_names <- strsplit(landmarks_with_slash_i$name_withslash, "/")[[1]]
    df_spread <- lapply(1:length(alt_names), function(i) landmarks_with_slash_i) %>% bind_rows
    df_spread$name <- alt_names
    return(df_spread)  
  }) %>% bind_rows
  
  parallel.landmarks <- bind_rows(parallel.landmarks, landmarks_with_slash_separated)
  
  # Add parallel landmarks to master landmark list -----------------------------
  # Only add if doesn't already exist
  #parallel.landmarks <- parallel.landmarks[!(parallel.landmarks$name %in% landmarks$name),]
  landmarks <- bind_rows(landmarks, parallel.landmarks)
  
  # If has stop word and is two or less words, remove ----------------------------
  stopwords <- paste0("\\b", stopwords(), "\\b") %>% paste(collapse="|")
  landmarks <- landmarks[!((grepl(stopwords, landmarks$name)) & (str_count(landmarks$name, "\\S+") <= 2)),]
  

  # Final Cleaning ---------------------------------------------------------------
  landmarks$name <- landmarks$name %>% 
    str_replace_all("/", " / ") %>%
    str_replace_all("[[:punct:]]", "") %>%
    str_replace_all("[^[:alnum:]| |/]", "") %>% 
    str_replace_all("\\|","") %>%
    str_squish 
  landmarks$name <- gsub("\\|","",landmarks$name)
  
  # Remove words
  landmarks <- landmarks[landmarks$name != "",]
  landmarks <- landmarks[!grepl("\\broad\\b", landmarks$name),]
  landmarks <- landmarks[!grepl("\\brd\\b", landmarks$name),]
  
  # Remove if last word is specific word (eg, highway)
  last_word <- word(landmarks$name,-1)
  landmarks <- landmarks[!(last_word %in% c("highway", "road", "rd", "way", "ave", "avenue", "street", "st")),]
  
  # Remove if first word is specific word (eg, stopwords)
  first_word <- word(landmarks$name,1)
  landmarks <- landmarks[!(first_word %in% stopwords("en")),]
  landmarks <- landmarks[!(first_word %in% c("near","at","the", "towards", "near")),]
  
  landmarks <- landmarks[!(landmarks$name %in% ""),]
  landmarks <- landmarks[nchar(landmarks$name) > 1,]
  
  # Remove landmarks that would highly raise likelihood of false positives
  landmarks <- landmarks[!(landmarks$name %in% landmarks_to_remove),]
  landmarks <- unique(landmarks)
  
  # Manually Change Coordinates for Some Locations -------------------------------
  # This requires local knowledge, but can be guided by algorithm output. Eg, 
  # are there locations that are wrong very commonly?
  # Do this for minimal number of locations.
  
  # JKIA [put along entrance to JKIA, near mombasa rd]
  ###### too specific, do separately.
  #landmarks$lat[landmarks$name == "jkia"] <- -1.344459 
  #landmarks$lon[landmarks$name == "jkia"] <- 36.902554
  
  return(landmarks)
}

