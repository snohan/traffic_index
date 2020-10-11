library(tidyverse)
library(sf)

source("H:/Programmering/R/byindeks/get_from_trafficdata_api.R")
source("H:/Programmering/R/byindeks/split_road_system_reference.R")

# Downloaded urban area geodata from
# https://kartkatalog.geonorge.no/metadata/tettsteder/173f4a15-dead-4f82-b92e-f37396b72cea
# as shapefile

urban_areas <- sf::st_read(
  "H:/Programmering/R/traffic_index/Tettsted2020/SSB_tettsted_flate_2020.shp"
  ) %>%
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
  dplyr::filter(traffic_type == "VEHICLE"#,
                #number_of_directions == 2
                ) %>%
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

number_of_points_in_urban_areas <- trp_urban %>%
  sf::st_drop_geometry() %>%
  dplyr::group_by(urban_area_name, population) %>%
  dplyr::summarise(number_of_points = n())


# Bestilling fra ØKV ####
trp_urban_chosen <- trp_urban %>%
  sf::st_drop_geometry() %>%
  dplyr::select(trp_id, name, road_reference,
                county_name, municipality_name, urban_area_name) %>%
  dplyr::filter(urban_area_name %in% c("Bodø",
                                       "Ålesund",
                                       "Haugesund",
                                       "Kopervik",
                                       "Åkrehamn",
                                       "Arendal",
                                       "Grimstad",
                                       "Fevik",
                                       "Holmestrand",
                                       "Horten",
                                       "Tønsberg",
                                       "Sandefjord",
                                       "Larvik"))

trp_additions <- trp_urban_area %>%
  sf::st_drop_geometry() %>%
  dplyr::select(trp_id, name, road_reference,
                county_name, municipality_name, urban_area_name) %>%
  dplyr::filter(trp_id %in% c("74801V2676825",
                              "33696V2676853",
                              "66316V2676855",
                              "85956V248955",
                              "38589V248954",
                              "33030V248950",
                              "58648V248949",
                              "84170V249428",
                              "81666V249662",
                              "78164V249524",
                              "55272V248971",
                              "39691V249524",
                              "33685V3109883",
                              "18554V248977",
                              "16799V248977",
                              "44415V319821",
                              "57053V319651",
                              "77208V2370670",
                              "87850V2527472",
                              "73725V320638",
                              "93598V22200",
                              "54428V2809635",
                              "88478V22214",
                              "67354V21986",
                              "93168V22215",
                              "97995V1175857",
                              "68114V1175858",
                              "37692V1827282",
                              "30201V1175821",
                              "19302V1175853",
                              "13515V1175879",
                              "02466V1175469",
                              "24127V1175846",
                              "43417V1175565",
                              "22051V1175839",
                              "17134V1175669",
                              "25260V1175672",
                              "92838V1175672",
                              "09610V1175654",
                              "18049V1175491",
                              "17769V1175834"
                              ))

trp_urban_chosen_and_additions <-
  dplyr::bind_rows(trp_urban_chosen,
                   trp_additions) %>%
  dplyr::arrange(#road_reference,
                 county_name,
                 municipality_name,
                 urban_area_name
                 )


write.csv2(trp_urban_chosen_and_additions,
           file = "trafikkregistreringspunkter_utvalgte_byer.csv",
           row.names = FALSE)

# VTI ####
# Looking at what we've used by July 2020 in the total Vegtrafikkindeks
# vti_trp <- read.csv2("punktindeks-2020-07.csv") %>%
#   dplyr::select(trpid,
#                 day_type = 6,
#                 lengdeklasse,
#                 periode,
#                 dekning) %>%
#   dplyr::filter(day_type == "Alle",
#                 lengdeklasse == "Alle",
#                 periode == "Hittil i år",
#                 dekning > 50)
#
# # Choosing only the good ones used already
# trp_urban_good <- trp_urban %>%
#   sf::st_drop_geometry() %>%
#   dplyr::filter(trp_id %in% vti_trp$trpid)
#
# trp_non_urban_good <- trp_non_urban %>%
#   sf::st_drop_geometry() %>%
#   dplyr::filter(trp_id %in% vti_trp$trpid)
