library(tidyverse)
library(sf)

source("H:/Programmering/R/byindeks/get_from_trafficdata_api.R")
source("H:/Programmering/R/byindeks/split_road_system_reference.R")

# Downloaded urban area geodata from
# https://kartkatalog.geonorge.no/metadata/tettsteder/
# as shapefile

urban_areas <- sf::st_read("Tettsted2020/SSB_tettsted_flate_2020.shp") %>%
  dplyr::select(population = TOT_BEF,
                urban_area_number = TETTNR,
                urban_area_name = TETTSTEDSN,
                geometry) %>%
  dplyr::filter(population >= 5000) %>%
  sf::st_transform("+proj=longlat +datum=WGS84")

urban_areas_selection <- urban_areas %>%
  dplyr::filter(urban_area_number %in% c(5001))

trp <- get_points() %>%
  dplyr::distinct(trp_id, .keep_all = T) %>%
  dplyr::filter(traffic_type == "VEHICLE") %>%
  dplyr::select(trp_id, name, road_reference, county_name,
                municipality_name, lat, lon) %>%
  split_road_system_reference() %>%
  dplyr::filter(is.na(intersection_part_number)) %>%
  dplyr::select(trp_id, name, road_reference, #road_category, road_number,
                road_category_and_number,
                section_number, #subsection_number, meter,
                #intersection_part_number, intersection_meter,
                county_name, municipality_name,
                lat, lon) %>%
  sf::st_as_sf(coords = c("lon", "lat"),
               crs = 4326)

# Finding every trps urban area - if within
trp_urban_area <- sf::st_join(trp, urban_areas, join = st_within)

trp_urban <- trp_urban_area %>%
  dplyr::filter(!is.na(urban_area_name))

trp_non_urban <- trp_urban_area %>%
  dplyr::filter(is.na(urban_area_name))
