

# Landmarks
landmark_gazetteer_orig <- readRDS(file.path(algorithm_inputs, "gazetteers_raw","merged", "gazetter_allsources_raw.Rds"))
landmark_gazetteer_orig <- landmark_gazetteer_orig[!is.na(landmark_gazetteer_orig$lat),]
landmark_gazetteer_orig <- landmark_gazetteer_orig[grepl("garden city|yaya center|roasters|pangani", landmark_gazetteer_orig$name),]
coordinates(landmark_gazetteer_orig) <- ~lon+lat
crs(landmark_gazetteer_orig) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
landmark_gazetteer_orig@data <- landmark_gazetteer_orig@data %>%
  dplyr::select(name, type)

##
roads_nairobi <- readRDS(file.path(algorithm_inputs, "roads_augmented", "osm_roads_aug.Rds"))
roads_nairobi <- roads_nairobi[grepl("mombasa|thika", roads_nairobi$name),]

##
areas_nairobi <- readRDS(file.path(algorithm_inputs, "nairobi_estates", "nairobi_estates.Rds"))
areas_nairobi@data <- areas_nairobi@data %>%
  dplyr::rename(name = estate)
areas_nairobi <- areas_nairobi[areas_nairobi$name %in% c("kilimani", "parklands", "upper hill"),]

## TYPE
landmark_sf <- landmark_gazetteer_orig %>% st_as_sf()
roads_sf <- roads_nairobi %>% st_as_sf()
areas_sf <- areas_nairobi %>% st_as_sf()

st_write(landmark_sf, file.path("~/Documents/Github/Unique-Location-Extractor/data/example_landmarks.geojson"), delete_dsn=T)
st_write(roads_sf, file.path("~/Documents/Github/Unique-Location-Extractor/data/example_roads.geojson"), delete_dsn=T)
st_write(areas_sf, file.path("~/Documents/Github/Unique-Location-Extractor/data/example_areas.geojson"), delete_dsn=T)
