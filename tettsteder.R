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

#urban_areas_selection <- urban_areas %>%
#  dplyr::filter(urban_area_number %in% c(5001))

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


# Looking at what we've used by July 2020 in the total Vegtrafikkindeks
vti_trp <- read.csv2("punktindeks-2020-07.csv") %>%
  dplyr::select(trpid,
                day_type = 6,
                lengdeklasse,
                periode,
                dekning) %>%
  dplyr::filter(day_type == "Alle",
                lengdeklasse == "Alle",
                periode == "Hittil i år",
                dekning > 50)

# Choosing only the good ones used already
trp_urban_good <- trp_urban %>%
  sf::st_drop_geometry() %>%
  dplyr::filter(trp_id %in% vti_trp$trpid)

trp_non_urban_good <- trp_non_urban %>%
  sf::st_drop_geometry() %>%
  dplyr::filter(trp_id %in% vti_trp$trpid)
