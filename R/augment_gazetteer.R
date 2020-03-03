# Augment Gazetteer

# TODO
#  1. At very end, see which cause algorithm to fail and take those out 
#     (ie, extra last step of using training data)
#  2. Faster way to grepl for clustering function? Could restrict landmarks_sp to
#     only those with unique or cluster ngram, so less to search through. Could use
#     dfm for that.
# 3. Simba cement, another far away from main nairobi area

# Setup ------------------------------------------------------------------------
if(Sys.info()["user"] == "WB521633") project_file_path <- "C:/Users/wb521633/Dropbox/World Bank/IEs/CrashMap-Nairobi"
if(Sys.info()["user"] == "robmarty") project_file_path <- "~/Dropbox/World Bank/IEs/CrashMap-Nairobi"

library(raster)
library(rgdal)
library(dplyr)
library(readr)
library(quanteda)
library(stringr)
library(stringi)
library(rgeos)
library(doBy)
library(hunspell)

NAIROBI_PROJ <- "+init=epsg:21037" # http://spatialreference.org/ref/epsg/arc-1960-utm-zone-37s/
skip_grams_first_last_word <- TRUE

# Define Main Functions --------------------------------------------------------
##### Collapse Landmarks Same Name Different Location (sort into general/specific)
collapse_landmarks_same_name_diff_loc <- function(landmark, df, dist_thresh_km){
  df_i <- df[df$name %in% landmark,]
  df_i_nrow <- nrow(df_i)
  
  if(nrow(df_i) == 1){
    df_i$general_specific <- "specific"
    df_out <- df_i
    df_out <- df_i@data
  } else{
    distance <- gDistance(df_i, byid=T) %>% max / 1000
    
    if(distance > dist_thresh_km){
      df_i$general_specific <- "general"
      df_i$same_name_max_distance <- distance
      
      df_out <- df_i@data
    } else{
      
      types <- strsplit(df_i$type, ";") %>% unlist %>% unique %>% paste(collapse=";")
      source <- df_i$source %>% unique %>% paste(collapse=";")
      
      df_out <- data.frame(name = landmark,
                           lat = mean(df_i$lat),
                           lon = mean(df_i$lon),
                           type = types,
                           same_name_max_distance = distance,
                           source = source,
                           general_specific="specific")
      
    }
  }
  
  return(df_out)
}

##### Cluster N-Grams
#ngram <- landmarks_ngrams_skipgrams_nonunique_notclose_sp$name[1]
#landmarks_sp <- landmarks_ngrams_skipgrams_nonunique_notclose_sp
#NGRAM_CLOSE_THRESHOLD <- 0.5
#NGRAM_CLOSE_PROPORTION_THRESHOLD <- 0.8
#MAX_N_CONSIDER <- 100
#exact_name_match <- TRUE

multiple_landmarknames_cluster <- function(ngram, 
                                           landmarks_sp, 
                                           NGRAM_CLOSE_THRESHOLD, 
                                           NGRAM_CLOSE_PROPORTION_THRESHOLD, 
                                           MAX_N_CONSIDER,
                                           exact_name_match,
                                           include_general_landmarks){
  # Captures all landmarks with the same ngram in them. If multiple landmarks,
  # uses centroid if a large proportion are clustered together. If only one landmark
  # returns that landmark
  
  # NGRAM: ngram to check if in landmarks
  # landmarks_sp: landmark spatial points data frame
  # NGRAM_CLOSE_THRESHOLD: Threshold distance to check if landmarks are close together (kilometers)
  # NGRAM_CLOSE_PROPORTION_THRESHOLD: Threshold proportion of landmark distance that must 
    # be close together to be considered a 'cluster'
  # MAX_N_CONSIDER: If many landmarks have the same ngram, function will slow considerable
    # due to create a large spatial distance matrix. If there are more than
  # MAX_N_CONSIDER landmarks, returns null dataframe (assumes there is no
    # dominant cluster) 
  # exact_name_match: If true, checks for exact name match. If false, uses grepl
  # include_general_landmarks: If true, returns landmarks that are not close and
    # where there is no dominant cluster. If false, only returns landmarks that are
    # either close or are not close but have a dominant cluster
  
  #print(ngram)
  
  if(exact_name_match){
    landmarks_with_ngram <- landmarks_sp[landmarks_sp$name %in% ngram,]
  } else{
    landmarks_with_ngram <- landmarks_sp[grepl(ngram, landmarks_sp$name),] # stri_detect_fixed
  }

  # Blank dataframe unless replaced
  df_out <- data.frame(NULL)
  
  # If multiple landmarks with ngram
  if(nrow(landmarks_with_ngram) > 1 & nrow(landmarks_with_ngram) <= MAX_N_CONSIDER){
    
    # Check if maximum distance is within close together threshold. If it is,
    # don't need to check clusters (which is computationally slower)
    max_distance <- sqrt((extent(landmarks_with_ngram)@xmin - extent(landmarks_with_ngram)@xmax)^2 + (extent(landmarks_with_ngram)@ymin - extent(landmarks_with_ngram)@ymax)^2)
    all_close_together <- (max_distance <= NGRAM_CLOSE_THRESHOLD*1000)
    
    if(all_close_together == TRUE){
      
      types <- strsplit(landmarks_with_ngram$type, ";") %>% unlist %>% unique %>% paste(collapse=";")
      source <- landmarks_with_ngram$source %>% unique %>% paste(collapse=";")
      
      df_out <- data.frame(name = ngram,
                           lat = mean(landmarks_with_ngram$lat),
                           lon = mean(landmarks_with_ngram$lon),
                           type = types,
                           same_name_max_distance = max_distance,
                           same_name_N_landmarks = nrow(landmarks_with_ngram),
                           same_name_N_landmarks_before_drop = nrow(landmarks_with_ngram),
                           source = source,
                           general_specific="specific")
      
    } 
    
    if(all_close_together == FALSE){
    
    landmarks_with_ngram_distances_mat <- gDistance(landmarks_with_ngram, byid=T)
    landmarks_with_ngram_distances_list <- landmarks_with_ngram_distances_mat %>% as.list %>% unlist
    cluster_exists <- mean(landmarks_with_ngram_distances_list <= NGRAM_CLOSE_THRESHOLD*1000) > NGRAM_CLOSE_PROPORTION_THRESHOLD
    
      if(cluster_exists){
        
        landmarks_with_ngram_distances_mat_closeTF <- landmarks_with_ngram_distances_mat
        landmarks_with_ngram_distances_mat_closeTF <- landmarks_with_ngram_distances_mat_closeTF < NGRAM_CLOSE_THRESHOLD*1000
        in_dominant_cluster <- (colSums(landmarks_with_ngram_distances_mat_closeTF) / nrow(landmarks_with_ngram_distances_mat_closeTF) > NGRAM_CLOSE_PROPORTION_THRESHOLD)
        
        landmarks_with_ngram_cluster <- landmarks_with_ngram[in_dominant_cluster,]
        
        types <- strsplit(landmarks_with_ngram_cluster$type, ";") %>% unlist %>% unique %>% paste(collapse=";")
        source <- landmarks_with_ngram_cluster$source %>% unique %>% paste(collapse=";")
        
        df_out <- data.frame(name = ngram,
                             lat = mean(landmarks_with_ngram_cluster$lat),
                             lon = mean(landmarks_with_ngram_cluster$lon),
                             type = types,
                             same_name_max_distance = max(landmarks_with_ngram_distances_list[landmarks_with_ngram_distances_list < NGRAM_CLOSE_THRESHOLD*1000]),
                             same_name_N_landmarks = nrow(landmarks_with_ngram_cluster),
                             same_name_N_landmarks_before_drop = nrow(landmarks_with_ngram),
                             source = source,
                             general_specific="specific")
      } else{

        if(include_general_landmarks){
          types <- strsplit(landmarks_with_ngram$type, ";") %>% unlist %>% unique %>% paste(collapse=";")
          source <- landmarks_with_ngram$source %>% unique %>% paste(collapse=";")
          
          df_out <- data.frame(name = ngram,
                               lat = mean(landmarks_with_ngram$lat),
                               lon = mean(landmarks_with_ngram$lon),
                               type = types,
                               same_name_max_distance = max(landmarks_with_ngram_distances_list),
                               same_name_N_landmarks = nrow(landmarks_with_ngram),
                               same_name_N_landmarks_before_drop = nrow(landmarks_with_ngram),
                               source = source,
                               general_specific="general") # samename_notclose_nocluster
        }
      }
    
    }
  }
  
  if(nrow(landmarks_with_ngram) == 1){
    df_out <- data.frame(name = ngram,
                         lat = landmarks_with_ngram$lat,
                         lon = landmarks_with_ngram$lon,
                         type = landmarks_with_ngram$type,
                         same_name_max_distance = 0,
                         same_name_N_landmarks = 1,
                         source = landmarks_with_ngram$source,
                         general_specific="specific")
  }
  
  return(df_out)
}

# Load Data --------------------------------------------------------------------
landmarks <- readRDS(file.path(project_file_path, "Twitter Geocode Algorithm", "data", "finaldata", "gazetteers_raw","merged", "gazetter_allsources_raw.Rds"))
landmarks <- landmarks[landmarks$source %in% c("geonames", "google", "osm"),]

# Basic Cleaning ---------------------------------------------------------------
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

# Remove landmarks with 1 or 0 character lengths
landmarks <- landmarks[nchar(landmarks$name) > 1,]

# Remove Common English Words --------------------------------------------------
# Remove common english words, except in some circumstances
#landmarks$correctly_spelled <- hunspell_check(landmarks$name)

#str_count(landmarks$correctly_spelled, "\\S+")

#unique(landmarks$name[landmarks$correctly_spelled==TRUE])


# Remove Routes ----------------------------------------------------------------
# Remove routes, except if have "flyover" in them as these are typically short
route_types <- c("route", "road") %>% paste(collapse="|")
landmarks <- landmarks[!(grepl(route_types, landmarks$type) & !grepl("flyover", landmarks$name)),]

# Remove certain types of landmarks --------------------------------------------
osm_types_remove <- c("toilet")
google_types_remove <- c("political", "locality","neighborhood","primary school")
geonames_types_remove <- c("toilet") # toilet not in there; just putting something for now TODO
types_remove <- c(osm_types_remove, google_types_remove, geonames_types_remove) %>% unique %>% paste(collapse="|")

landmarks <- landmarks[!grepl(types_remove, landmarks$type),]

# Landmarks with Same Name But Different Locations -----------------------------
if(F){
# Add variable for how many times landmark repeats
landmarks <- landmarks %>% 
  group_by(name) %>% 
  mutate(count = n()) %>% 
  as.data.frame

# Landmarks where names don't repeat
landmarks_norepeat <- landmarks[landmarks$count == 1,] %>% 
  mutate(general_specific = "specific")

# Collapse landmarks where name repeates
landmarks_repeats <- landmarks[landmarks$count > 1,]
landmarks_repeats$latitude <- landmarks_repeats$lat
landmarks_repeats$longitude <- landmarks_repeats$lon
coordinates(landmarks_repeats) <- ~longitude+latitude
crs(landmarks_repeats) <- CRS("+init=epsg:4326")
landmarks_repeats <- spTransform(landmarks_repeats, CRS(NAIROBI_PROJ))

# Collapse Landmarks
landmarks_repeats_collapsed <- lapply(unique(landmarks_repeats$name), collapse_landmarks_same_name_diff_loc, landmarks_repeats, 0.2) %>% bind_rows
landmarks <- bind_rows(landmarks, landmarks_repeats_collapsed)
}

# Create N-Grams and Skip Grams ------------------------------------------------
#### Function for Parallel Landmarks
skipgram_ngram_parallel_landmark <- function(var_i, landmarks_sp_Nword, skip_grams_Nword_df){
  landmarks_sp_Nword$name <- skip_grams_Nword_df[,var_i] %>% as.character
  return(landmarks_sp_Nword)
}

landmarks$number_words <- str_count(landmarks$name, "\\S+")
landmarks_3words <- landmarks[landmarks$number_words %in% 3,]
landmarks_4words <- landmarks[landmarks$number_words %in% 4,]
landmarks_5words <- landmarks[landmarks$number_words %in% 5,]
landmarks_6words <- landmarks[landmarks$number_words %in% 6,]

#### N-Grams
landmarks_3words_ngrams <- landmarks_3words$name %>%
  tokens(remove_symbols = F, remove_punct = F) %>% 
  tokens_ngrams(n=2, concatenator = " ")
landmarks_3words_ngrams_df <- landmarks_3words_ngrams %>% as.list %>% as.data.frame %>% t

landmarks_4words_ngrams <- landmarks_4words$name %>%
  tokens(remove_symbols = F, remove_punct = F) %>% 
  tokens_ngrams(n=2:3, concatenator = " ")
landmarks_4words_ngrams_df <- landmarks_4words_ngrams %>% as.list %>% as.data.frame %>% t

landmarks_5words_ngrams <- landmarks_5words$name %>%
  tokens(remove_symbols = F, remove_punct = F) %>% 
  tokens_ngrams(n=2:3, concatenator = " ")
landmarks_5words_ngrams_df <- landmarks_5words_ngrams %>% as.list %>% as.data.frame %>% t

landmarks_6words_ngrams <- landmarks_6words$name %>%
  tokens(remove_symbols = F, remove_punct = F) %>% 
  tokens_ngrams(n=2:3, concatenator = " ")
landmarks_6words_ngrams_df <- landmarks_6words_ngrams %>% as.list %>% as.data.frame %>% t

landmarks_3word_parallel_ngrams <- lapply(1:ncol(landmarks_3words_ngrams_df), skipgram_ngram_parallel_landmark, landmarks_3words, landmarks_3words_ngrams_df) %>% bind_rows
landmarks_4word_parallel_ngrams <- lapply(1:ncol(landmarks_4words_ngrams_df), skipgram_ngram_parallel_landmark, landmarks_4words, landmarks_4words_ngrams_df) %>% bind_rows
landmarks_5word_parallel_ngrams <- lapply(1:ncol(landmarks_5words_ngrams_df), skipgram_ngram_parallel_landmark, landmarks_5words, landmarks_5words_ngrams_df) %>% bind_rows
landmarks_6word_parallel_ngrams <- lapply(1:ncol(landmarks_6words_ngrams_df), skipgram_ngram_parallel_landmark, landmarks_6words, landmarks_6words_ngrams_df) %>% bind_rows

#### Skip Grams
skip_grams_3word <- landmarks_3words$name %>% 
  tokens(remove_symbols = F, remove_punct = F) %>% 
  tokens_skipgrams(n=2:3, 
                   skip=0:4, 
                   concatenator = " ")
skip_grams_3word_df <- skip_grams_3word %>% as.list %>% as.data.frame %>% t
if(skip_grams_first_last_word) skip_grams_3word_df <- skip_grams_3word_df[,2] %>% as.data.frame

skip_grams_4word <- landmarks_4words$name %>% 
  tokens(remove_symbols = F, remove_punct = F) %>% 
  tokens_skipgrams(n=2:3, 
                   skip=0:4, 
                   concatenator = " ")
skip_grams_4word_df <- skip_grams_4word %>% as.list %>% as.data.frame %>% t
if(skip_grams_first_last_word) skip_grams_4word_df <- skip_grams_4word_df[,c(3,8)]

skip_grams_5word <- landmarks_5words$name %>% 
  tokens(remove_symbols = F, remove_punct = F) %>% 
  tokens_skipgrams(n=2:4, 
                   skip=0:4, 
                   concatenator = " ")
skip_grams_5word_df <- skip_grams_5word %>% as.list %>% as.data.frame %>% t
if(skip_grams_first_last_word) skip_grams_5word_df <- skip_grams_5word_df[,c(4,13,15,16,22,23,24)]

skip_grams_6word <- landmarks_6words$name %>% 
  tokens(remove_symbols = F, remove_punct = F) %>% 
  tokens_skipgrams(n=2:5, 
                   skip=0:4, 
                   concatenator = " ")
skip_grams_6word_df <- skip_grams_6word %>% as.list %>% as.data.frame %>% t
if(skip_grams_first_last_word) skip_grams_6word_df <- skip_grams_6word_df[,c(5,19,22,24,25,38,40,41,43,44,45,52,53,54,55)]

landmarks_3word_parallel_skipgrams <- lapply(1:ncol(skip_grams_3word_df), skipgram_ngram_parallel_landmark, landmarks_3words, skip_grams_3word_df) %>% bind_rows
landmarks_4word_parallel_skipgrams <- lapply(1:ncol(skip_grams_4word_df), skipgram_ngram_parallel_landmark, landmarks_4words, skip_grams_4word_df) %>% bind_rows
landmarks_5word_parallel_skipgrams <- lapply(1:ncol(skip_grams_5word_df), skipgram_ngram_parallel_landmark, landmarks_5words, skip_grams_5word_df) %>% bind_rows
landmarks_6word_parallel_skipgrams <- lapply(1:ncol(skip_grams_6word_df), skipgram_ngram_parallel_landmark, landmarks_6words, skip_grams_6word_df) %>% bind_rows

#### Append
landmarks_ngrams_skipgrams <- bind_rows(landmarks_3word_parallel_ngrams,
                                        landmarks_4word_parallel_ngrams,
                                        landmarks_5word_parallel_ngrams,
                                        landmarks_6word_parallel_ngrams,
                                        landmarks_3word_parallel_skipgrams,
                                        landmarks_4word_parallel_skipgrams,
                                        landmarks_5word_parallel_skipgrams,
                                        landmarks_6word_parallel_skipgrams)

# Determine Which N/Skip-Grams to Add to Dictionary ----------------------------
#### 1. If n/skip-gram already exists in landmark dictionary, don't include
landmarks_ngrams_skipgrams <- landmarks_ngrams_skipgrams[!(landmarks_ngrams_skipgrams$name %in% landmarks$name),]

#### 2. Determine whether n/skip-gram is unique or not
landmarks_ngrams_skipgrams <- landmarks_ngrams_skipgrams %>%
  group_by(name) %>%
  mutate(name_N = n())

#### 3. Grab unique n/skip-grams 
landmarks_ngrams_skipgrams_unique <- landmarks_ngrams_skipgrams[landmarks_ngrams_skipgrams$name_N == 1,]

#### 4. Deal with non-unique skip grams
landmarks_ngrams_skipgrams_nonunique <- landmarks_ngrams_skipgrams[landmarks_ngrams_skipgrams$name_N > 1,]

### 4.1 Remove n/skip-gram if very frequent
landmarks_ngrams_skipgrams_nonunique <- landmarks_ngrams_skipgrams_nonunique[landmarks_ngrams_skipgrams_nonunique$name_N <= 100,]

### 4.2 Check min/max lat lon to determine if within threshold distance
landmarks_ngrams_skipgrams_nonunique <- as.data.frame(landmarks_ngrams_skipgrams_nonunique)

landmarks_ngrams_skipgrams_nonunique_minlatlat <- doBy::summaryBy(lon + lat ~ name, data=landmarks_ngrams_skipgrams_nonunique, keep.names=T, FUN=min) %>%
  dplyr::rename(lon_min = lon) %>%
  dplyr::rename(lat_min = lat)

landmarks_ngrams_skipgrams_nonunique_maxlatlat <- summaryBy(lon+lat+name_N ~ name, data=landmarks_ngrams_skipgrams_nonunique, keep.names=T, FUN=max) %>%
  dplyr::rename(lon_max = lon) %>%
  dplyr::rename(lat_max = lat)

landmarks_ngrams_skipgrams_nonunique_minmaxlatlat <- merge(landmarks_ngrams_skipgrams_nonunique_minlatlat,
                                                    landmarks_ngrams_skipgrams_nonunique_maxlatlat,
                                                    by="name")

landmarks_ngrams_skipgrams_nonunique_minmaxlatlat$distance <- sqrt((landmarks_ngrams_skipgrams_nonunique_minmaxlatlat$lon_min -landmarks_ngrams_skipgrams_nonunique_minmaxlatlat$lon_max)^2 +
                                                              (landmarks_ngrams_skipgrams_nonunique_minmaxlatlat$lat_min - landmarks_ngrams_skipgrams_nonunique_minmaxlatlat$lat_max)^2) * 111.12

### 4.3 Separate: (1) close together, (2) not close together 
landmarks_ngrams_skipgrams_nonunique_close <- landmarks_ngrams_skipgrams_nonunique_minmaxlatlat[landmarks_ngrams_skipgrams_nonunique_minmaxlatlat$distance < 0.5,]
landmarks_ngrams_skipgrams_nonunique_notclose <- landmarks_ngrams_skipgrams_nonunique_minmaxlatlat[landmarks_ngrams_skipgrams_nonunique_minmaxlatlat$distance >= 0.5,]

landmarks_ngrams_skipgrams_nonunique_close_df <- landmarks_ngrams_skipgrams[landmarks_ngrams_skipgrams$name %in% landmarks_ngrams_skipgrams_nonunique_close$name,]
landmarks_ngrams_skipgrams_nonunique_notclose_df <- landmarks_ngrams_skipgrams[landmarks_ngrams_skipgrams$name %in% landmarks_ngrams_skipgrams_nonunique_notclose$name,]

### 4.4 If Close together, collapse
# Use type, source and other data from one of the landmarks to make code faster
landmarks_ngrams_skipgrams_nonunique_close_vars <- landmarks_ngrams_skipgrams_nonunique_close_df[!duplicated(landmarks_ngrams_skipgrams_nonunique_close_df[,c('name')]),]
landmarks_ngrams_skipgrams_nonunique_close_vars <- subset(landmarks_ngrams_skipgrams_nonunique_close_vars, select=-c(lat,lon))

landmarks_ngrams_skipgrams_nonunique_close_latlon <- summaryBy(lat+lon ~ name, data=as.data.frame(landmarks_ngrams_skipgrams_nonunique_close_df), keep.names = T, FUN=mean)

landmarks_ngrams_skipgrams_nonunique_close <- merge(landmarks_ngrams_skipgrams_nonunique_close_latlon, landmarks_ngrams_skipgrams_nonunique_close_vars, by="name")

### 4.4 If not close and 4 or less landmarks, keep as general
landmarks_ngrams_skipgrams_nonunique_notclose_general_df <- landmarks_ngrams_skipgrams_nonunique_notclose_df[landmarks_ngrams_skipgrams_nonunique_notclose_df$name_N < 5,]
landmarks_ngrams_skipgrams_nonunique_notclose_general_df$general_specific <- "general"

# If two words and one word is one letter, remove
remove <- (str_count(landmarks_ngrams_skipgrams_nonunique_notclose_general_df$name, "\\S+") %in% 2) &
          (nchar(word(landmarks_ngrams_skipgrams_nonunique_notclose_general_df$name, 1)) %in% 1) |
          (nchar(word(landmarks_ngrams_skipgrams_nonunique_notclose_general_df$name, -1)) %in% 1)
landmarks_ngrams_skipgrams_nonunique_notclose_general_df <- landmarks_ngrams_skipgrams_nonunique_notclose_general_df[!remove,]

### 4.5 If not close and 5 or more landmarks, look for dominant clusters
landmarks_ngrams_skipgrams_nonunique_notclose_df <- landmarks_ngrams_skipgrams_nonunique_notclose_df[landmarks_ngrams_skipgrams_nonunique_notclose_df$name_N >= 5,]

# Extract dominant clusters
landmarks_ngrams_skipgrams_nonunique_notclose_sp <- landmarks_ngrams_skipgrams_nonunique_notclose_df
landmarks_ngrams_skipgrams_nonunique_notclose_sp$latitude <- landmarks_ngrams_skipgrams_nonunique_notclose_sp$lat
landmarks_ngrams_skipgrams_nonunique_notclose_sp$longitude <- landmarks_ngrams_skipgrams_nonunique_notclose_sp$lon
coordinates(landmarks_ngrams_skipgrams_nonunique_notclose_sp) <- ~longitude+latitude
crs(landmarks_ngrams_skipgrams_nonunique_notclose_sp) <- CRS("+init=epsg:4326")
landmarks_ngrams_skipgrams_nonunique_notclose_sp <- spTransform(landmarks_ngrams_skipgrams_nonunique_notclose_sp, CRS(NAIROBI_PROJ))

landmarks_ngrams_skipgrams_nonunique_notclose_cluster <- lapply(unique(landmarks_ngrams_skipgrams_nonunique_notclose_sp$name), multiple_landmarknames_cluster, landmarks_ngrams_skipgrams_nonunique_notclose_sp, 0.5, 0.8, 100, TRUE, TRUE) %>% bind_rows

### 4. Append to landmark dictionary
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

# Remove extraneous landmarks -------------------------------------------------
# One one character, remove
#landmarks <- landmarks[nchar(landmarks$name) > 1,]

# If two characters and numeric (eg, 56), remove
#landmarks_two_characters <- landmarks$name[nchar(landmarks$name) == 2]
#landmarks_two_characters_numeric <- landmarks_two_characters[!is.na(as.numeric(landmarks_two_characters))]
#landmarks <- landmarks[!(landmarks$name %in% landmarks_two_characters_numeric),]

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
landmarks_to_remove <- c("school",
                         "hospital",
                         "mombasa",
                         "thika",
                         "stage",
                         "town",
                         "center",
                         "traffic",
                         "b,b",
                         "nairobi",
                         "bus",
                         "matatu",
                         "highway",
                         "cars",
                         "junction",
                         "flyover",
                         "bumps",
                         "site",
                         "high way",
                         "main",
                         #"nation",
                         "post",
                         "bar",
                         "kampala",
                         "mess",
                         "near",
                         "car",
                         "one",
                         "kiambu",
                         "thika superhighway",
                         "towards town",
                         "three",
                         "toll",
                         "route",
                         "there is a",
                         "city",
                         "mpya bus",
                         "saloon",
                         "salon",
                         "total",
                         "tuskys",
                         "group",
                         "house",
                         "centre",
                         "police station",
                         "accident emergency",
                         "leaders",
                         "view",
                         "new",
                         "mini",
                         "back",
                         "roundabout",
                         "ke",
                         "club",
                         "bus stop",
                         "stop",
                         "park",
                         "mall",
                         "citi hoppa",
                         "spot",
                         "juction",
                         "st",
                         "traffic police",
                         "two saloon",
                         "juction",
                         "school bus",
                         "shuttle bus",
                         "scone",
                         "airtel just",
                         "just",
                         "man",
                         "motor bike",
                         "motor cycle",
                         "nice",
                         "opposite",
                         "outside",
                         "point",
                         "school buses",
                         "boda boda",
                         "just",
                         "outside",
                         "tuck",
                         "major",
                         "two",
                         "man",
                         "round",
                         "accident area",
                         "bond",
                         "well",
                         "super higway",
                         "2 salon",
                         "24 hours",
                         "5 miutes",
                         "good",
                         "high",
                         "tree",
                         "best",
                         "bodaboda",
                         "catholic",
                         "city bus",
                         "green",
                         "outside",
                         "building",
                         "hotel",
                         "joy",
                         "stage",
                         "zebra",
                         "coast",
                         "bypass",
                         "roundabout", #or keep as general?
                         "by pass",
                         "bypass",
                         "kenya red cross",
                         "water",
                         "bridge",
                         "s chool",
                         "county",
                         "roundabout",
                         "one car",
                         "will",
                         "light",
                         "red",
                         "nissan",
                         "entrance",
                         "side",
                         "sure",
                         "left",
                         "porsche",
                         "ladies",
                         "unknown",
                         "scene",
                         "entrance",
                         "southern",
                         "small",
                         "south",
                         "toyota",
                         "jam",
                         "trucks",
                         "check",
                         "allsopps bus",
                         "moment",
                         "end",
                         "small",
                         "north",
                         "overpass",
                         "kenya",
                         "end",
                         "r d",
                         "unknown",
                         "take") 
landmarks <- landmarks[!(landmarks$name %in% landmarks_to_remove),]
landmarks <- unique(landmarks)

# Manually Change Coordinates for Some Locations -------------------------------
  # This requires local knowledge, but can be guided by algorithm output. Eg, 
    # are there locations that are wrong very commonly?
  # Do this for minimal number of locations.

# JKIA [put along entrance to JKIA, near mombasa rd]
landmarks$lat[landmarks$name == "jkia"] <- -1.344459 
landmarks$lon[landmarks$name == "jkia"] <- 36.902554

# Export -----------------------------------------------------------------------
saveRDS(landmarks, file.path(project_file_path, "Twitter Geocode Algorithm", "data", "finaldata", "gazetteers_augmented", "gazetteer_aug.Rds"))

