remove_general_landmarks <- function(landmark_match,
                                     landmark_gazetteer,
                                     road_match_sp){
  
  # General landmarks are those with multiple names, are not close to each
  # other and there is no dominant cluster. These are more likely to have
  # spurious names (names not relevant for a location). These will only help
  # pinpoint a location if a road is mentioned. Consequently, we throw these 
  # out if a road is not mentioned. If a road is mentioned, we restrict to ones
  # that are close to a road.
  
  # Note that a landmark with the same name can be both general and specific if
  # there is a dominant cluster: the landmarks in the dominant cluster are
  # specific, while the ones not are general.
  
  # Remove general landmarks from:
  # (1) Landmark matched list
  # (2) Gazeteer
  
  landmark_match_gs <- merge(landmark_match, landmark_gazetteer@data, 
                             by.x="matched_words_correct_spelling", by.y="name", 
                             all.x=T, all.y=F)
  
  # If there are general landmarks AND roads
  if(("general" %in% landmark_match_gs$general_specific) & nrow(road_match_sp) > 0){
    
    ## Grab spatial points of general landmarks found in text
    landmark_general_sp <- landmark_gazetteer[(landmark_gazetteer$general_specific %in% "general") &
                                                (landmark_gazetteer$uid %in% landmark_match_gs$uid),]
    
    ## Remove general landmarks far from road
    landmark_general_sp$distance <- gDistance(landmark_general_sp, road_match_agg_sp, byid=T) %>% as.vector()
    general_landmark_uid_remove <- landmark_general_sp$uid[landmark_general_sp$distance > 0.5*1000]
    
    ## Remove general landmarks
    landmark_gazetteer <- landmark_gazetteer[!(landmark_gazetteer$uid %in% general_landmark_uid_remove),]
    landmark_match_gs  <- landmark_match_gs[!(landmark_match_gs$uid %in% general_landmark_uid_remove),]
    
    # If no roads, remove all general  
  } else{
    landmark_match_gs  <- landmark_match_gs[!(landmark_match_gs$general_specific %in% "general"),]
    landmark_gazetteer <- landmark_gazetteer[!(landmark_gazetteer$general_specific %in% "general"),]
  }
  
  # Restrict dataframe to potentially removing general landmarks and ensure
  # same variables as before
  landmark_match <- landmark_match[landmark_match$matched_words_correct_spelling %in%
                                     landmark_match_gs$matched_words_correct_spelling,]
  # TODO: Also restrict locations_match??
  
  return(list(landmark_match = landmark_match,
              landmark_gazetteer = landmark_gazetteer))
}

# TODO: landmark_road_overlap and exact_fuzzy_overlap could be made into one
# function where have a parameter for the (1) variable and (2) which one wins,
# if make this into something where only two types
landmark_road_overlap <- function(locations_in_tweet){
  # Landmark intersects with road, choose road 
  
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
  
  return(locations_in_tweet)
  
}

exact_fuzzy_overlap <- function(locations_in_tweet){
  # If exact intersects with fuzzy, choose exact 
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
  
  return(locations_in_tweet)
}

phase_overlap <- function(locations_in_tweet){
  # Phase within Phrase 
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
  
  return(locations_in_tweet)
}

exact_fuzzy_startendsame <- function(locations_in_tweet){
  
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
  
  return(locations_in_tweet)
}

landmark_road_samename <- function(locations_in_tweet){
  # If same name both road and landmark, drop landmark 
  if(("landmark" %in% locations_in_tweet$location_type) & ("road" %in% locations_in_tweet$location_type)){
    
    drop_if_landmark <- locations_in_tweet$matched_words_tweet_spelling %in% 
      locations_in_tweet$matched_words_tweet_spelling[locations_in_tweet$location_type %in% "road"]
    
    locations_in_tweet <- locations_in_tweet[!(locations_in_tweet$location_type %in% "landmark" & drop_if_landmark),]
  }
  
  return(locations_in_tweet)
}

extract_intersections <- function(locations_in_tweet,
                                  roads,
                                  point_dist_thresh = 500,
                                  road_buffer = 50){
  # DESCRIPTION: Finds intersections between roads and returns if points of 
  # intersections are close together (ie, returns nothing if two roads 
  # intersect in multiple places far apart)
  # ARGS:
  # locations_in_tweet: Dataframe of locations in tweet
  # roads: Roads shapefile
  # point_dist_thresh: Threshold at which intersection points must be close
  #                    together. (meters / projected units)
  # road_buffer: Width to buffer roads before determining intersections (default
  #              0). Allows cases where roads are close but technically no 
  #              point of intersections. (meters / projected units)
  
  # Find Road Intersection 
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
      
      if(road_buffer > 0){
        road_1 <- gBuffer(road_1, byid=T, width=road_buffer)
        road_2 <- gBuffer(road_2, byid=T, width=road_buffer)
      }
      
      intersection_points <- gIntersection(road_1, road_2)
      
      # Check to make sure intersection points are close together. Roads could
      # intersect in multiple places, where then location is ambiguous
      if(!is.null(intersection_points)){
        
        # If maximum distance between points are close together, then use
        intersection_points_extent <- extent(intersection_points)
        max_points_dist <- sqrt((intersection_points_extent@xmin - intersection_points_extent@xmax)^2 + (intersection_points_extent@ymin - intersection_points_extent@ymax)^2)
        
        if(max_points_dist < point_dist_thresh){
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
    
    ## Spatially define
    if(nrow(road_intersections) > 0){
      coordinates(road_intersections) <- ~lon+lat
      crs(road_intersections) <- CRS(as.character(roads@proj4string))
    }
    
    #### Add variable to "locations_in_tweet" if road is part of intersection
    # TODO
    
  } else{
    # Other parts check nrow(road_intersections), so make blank dataframe
    road_intersections <- data.frame(NULL)
  }
  
  return(road_intersections)
  
}
