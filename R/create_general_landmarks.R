# Create General Landmarks

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

NAIROBI_PROJ <- "+init=epsg:21037" # http://spatialreference.org/ref/epsg/arc-1960-utm-zone-37s/

# Load Data --------------------------------------------------------------------
landmarks <- readRDS(file.path(project_file_path, "Twitter Geocode Algorithm", "data", "finaldata", "gazetteers_raw","merged", "gazetter_allsources_raw.Rds"))
landmarks <- landmarks[landmarks$source %in% c("geonames", "google", "osm"),]

# Gas Stations -----------------------------------------------------------------
gas_stations <- landmarks[grepl("gas_station", landmarks$type),]

gas_stations_total <- gas_stations[grepl("total", gas_stations$name),]
gas_stations_total$name <- "total"

gas_stations_shell <- gas_stations[grepl("shell", gas_stations$name),]
gas_stations_shell$name <- "shell"

# Append Landmarks -------------------------------------------------------------
landmarks_general <- bind_rows(gas_stations_total, gas_stations_shell)

# Export -----------------------------------------------------------------------
saveRDS(landmarks, file.path(project_file_path, "Twitter Geocode Algorithm", "data", "finaldata", "general_landmarks", "general_landmarks.Rds"))



truth_data <- readRDS(file.path(project_file_path, "Data", "FinalData", "Twitter", "Truth Data", "twitter_truth_data.Rds"))
out <- truth_data[grepl("shell", truth_data$tweet),]

