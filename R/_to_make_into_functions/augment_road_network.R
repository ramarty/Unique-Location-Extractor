# Augment Road Network

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

# Load Raw Road Network --------------------------------------------------------
roads <- readRDS(file.path(project_file_path, "Twitter Geocode Algorithm", "data", "finaldata", "roads", "osm_roads.Rds"))

roads$name <- roads$name %>% as.character %>% tolower
roads$ambiguous_road <- FALSE

# Parallel Road Type Abbreviations ---------------------------------------------
roads_includes_road <- roads[grepl("\\broad\\b", roads$name),]
roads_includes_road$name <- gsub("\\broad\\b", "rd", roads_includes_road$name)

roads_includes_avenue <- roads[grepl("\\bavenue\\b", roads$name),]
roads_includes_avenue$name <- gsub("\\bavenue\\b", "ave", roads_includes_avenue$name)

roads_includes_street <- roads[grepl("\\bstreet\\b", roads$name),]
roads_includes_street$name <- gsub("\\bstreet\\b", "st", roads_includes_street$name)

# Parallel Common Alternate Names ----------------------------------------------
#### Thika Road
roads_thika <- roads[roads$name %in% "thika road",]

roads_thika_parallel_1 <- roads_thika
roads_thika_parallel_1$name <- "thika superhighway"

roads_thika_parallel_2 <- roads_thika
roads_thika_parallel_2$name <- "thika highway"

roads_thika_parallel_3 <- roads_thika
roads_thika_parallel_3$name <- "superhighway"

#### Mombasa Road
roads_mombasa <- roads[roads$name %in% "mombasa road",]

roads_mombasa_parallel_1 <- roads_mombasa
roads_mombasa_parallel_1$name <- "msa road"

roads_mombasa_parallel_2 <- roads_mombasa
roads_mombasa_parallel_2$name <- "msa rd"

# Ambiguous Roads --------------------------------------------------------------
roads_bypass_ambiguous <- roads[grepl("bypass", roads$name),]
roads_bypass_ambiguous$name <- "bypass"
roads_bypass_ambiguous$ambiguous_road <- TRUE

# Append -----------------------------------------------------------------------
roads_aug <- list(roads,
                  roads_includes_road, 
                  roads_includes_avenue, 
                  roads_includes_street,
                  roads_thika_parallel_1,
                  roads_thika_parallel_2,
                  roads_thika_parallel_3,
                  roads_mombasa_parallel_1,
                  roads_mombasa_parallel_2,
                  roads_bypass_ambiguous) %>% 
  do.call(what="rbind")

# Remove Generic Names ---------------------------------------------------------
generic_names <- c("bridge", "roundabout", "bypass","by pass", "round about", "entrance")
roads_aug <- roads_aug[!((roads_aug$name %in% generic_names) & (roads_aug$ambiguous_road %in% FALSE)),]

# Buffer Slightly --------------------------------------------------------------
# Buffer by 10 meters. See lusaka rd and enterprise rd. There's a roundabout at
# the intersection where they actually don't touch.
roads_aug <- gBuffer(roads_aug, width=.01/111.12, byid=T)

# Export -----------------------------------------------------------------------
saveRDS(roads_aug, file.path(project_file_path, "Twitter Geocode Algorithm", "data", "finaldata", "roads_augmented", "osm_roads_aug.Rds"))



