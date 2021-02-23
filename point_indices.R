#

source("H:/Programmering/R/byindeks/get_from_trafficdata_api.R")
source("H:/Programmering/R/byindeks/split_road_system_reference.R")
source("H:/Programmering/R/byindeks/get_from_nvdb_api.R")
library(writexl)

trps <- get_points()

trps_chosen <- trps %>%
  dplyr::select(trp_id, name, traffic_type, registration_frequency,
                county_geono, county_name, municipality_name,
                road_reference, valid_from = validFrom) %>%
  dplyr::group_by(trp_id) %>%
  dplyr::slice(which.min(valid_from)) %>%
  dplyr::ungroup() %>%
  dplyr::filter(traffic_type == "VEHICLE",
                registration_frequency == "CONTINUOUS") %>%
  split_road_system_reference() %>%
  dplyr::filter(road_category != "K",
                valid_from < "2020-10-01") %>%
  dplyr::mutate(road_category = dplyr::case_when(road_category == "E" ~ "R",
                                                 road_category == "R" ~ "R",
                                                 road_category == "F" ~ "F"),
                name = stringr::str_to_title(name, locale = "no")) %>%
  dplyr::select(trp_id, name, county_geono, county_name, municipality_name, road_category,
                road_reference)


# All point indices in 2020 ####
# Caution! Takes 60 min
trp_index <- get_pointindices_for_trp_list(trps_chosen$trp_id, "2020")

trp_index_tidy <- trp_index %>%
  dplyr::filter(day_type == "ALL",
                period == "year_to_date",
                month == 12) %>%
  dplyr::select(-road_category) %>%
  dplyr::inner_join(trps_chosen) %>%
  dplyr::select(name, county_name, municipality_name, road_category, road_reference,
                year, index_total = index_total_p, coverage = index_total_coverage,
                index_heavy = index_long, length_excluded, length_coverage)

writexl::write_xlsx(trp_index_tidy,
                    path = "punktindekser_2020.xlsx")

# points_per_municipality <- trps_chosen %>%
#   dplyr::group_by(county_name) %>%
#   dplyr::summarise(number_of_points = n())

# En journalist som spurte om disse: ####
# point_indices <- dplyr::bind_rows(get_pointindices("06390V1204620", "2020"),
#                                   get_pointindices("39158V121754", "2020"),
#                                   get_pointindices("20575V121775", "2020"),
#                                   get_pointindices("83871V2654774", "2020"),
#                                   get_pointindices("00091V121407", "2020"),
#                                   get_pointindices("00671V121415", "2020"),
#                                   get_pointindices("10649V121767", "2020")
#                                   )

# Ferietrafikken til utfartssteder ####
point_indices <- dplyr::bind_rows(
  get_pointindices("54653V1751052", "2020"),
  get_pointindices("35258V2475662", "2020"),
  get_pointindices("43920V121762", "2020"),
  get_pointindices("00671V121415", "2020"),
  get_pointindices("03486V319647", "2020"),
  get_pointindices("12510V521383", "2020"),
  get_pointindices("48271V2370824", "2020"),
  get_pointindices("73809V805608", "2020"),
  get_pointindices("20810V384475", "2020"),
  get_pointindices("96883V2682363", "2020"),
  get_pointindices("54929V805598", "2020"),
  get_pointindices("00598V1060635", "2020"),
  get_pointindices("04673V2712750", "2020"),
  get_pointindices("09750V705194", "2020"),
  get_pointindices("01236V705218", "2020"),
  get_pointindices("19923V1060616", "2020"),
  get_pointindices("03080V1060138", "2020"),
  get_pointindices("46288V1060102", "2020"),
  get_pointindices("29481V384100", "2020"),
  get_pointindices("74431V384079", "2020"),
  get_pointindices("92559V248993", "2020"),
  get_pointindices("77722V249572", "2020"),
  get_pointindices("97843V1060647", "2020"),
  get_pointindices("89208V249580", "2020"),
  get_pointindices("48910V72822", "2020"),
  get_pointindices("38212V72305", "2020"),
  get_pointindices("62393V72804", "2020"),
  get_pointindices("46610V578105", "2020"),
  get_pointindices("00509V885112", "2020"),
  get_pointindices("39676V885922", "2020"),
  get_pointindices("82860V885983", "2020"),
  get_pointindices("65823V1668921", "2020"),
  get_pointindices("19632V1665341", "2020"),
  get_pointindices("28120V1126319", "2020"),
  get_pointindices("95423V930614", "2020"),
  get_pointindices("14490V930350", "2020")
)

point_indices_final <- point_indices %>%
  dplyr::select(-index_total) %>%
  dplyr::filter(day_type == "ALL") %>%
  dplyr::filter(period == "month") %>%
  dplyr::filter(month %in% c(6, 7)) %>%
  dplyr::select(trp_id, year, month, index_total_p, index_total_coverage) %>%
  dplyr::mutate(index_total_p = round(index_total_p, digits = 1)) %>%
  dplyr::left_join(points) %>%
  dplyr::select(name:municipality_name, year:index_total_coverage)

point_indices_public_friendly <- point_indices_final %>%
  dplyr::mutate(year_month =
                  lubridate::ymd(paste(year, month, "01", sep = "-")),
                month = lubridate::month(month, label = TRUE, abbr = FALSE) %>%
                  stringr::str_to_title()) %>%
  dplyr::select(Trafikkregistreringspunkt = name,
                Veg = road_category_and_number,
                Fylke = county_name,
                Kommune = municipality_name,
                month,
                index_total_p) %>%
  tidyr::pivot_wider(names_from = month, values_from = index_total_p)

writexl::write_xlsx(point_indices_public_friendly,
           path = "ferietrafikken.xlsx")



# E16 ####
points_e16 <- points %>%
  dplyr::filter(road_category_and_number == "Ev134") %>%
  # keeping only points on "mountain part"
  dplyr::filter(section_number >= 4) %>%
  dplyr::filter(section_number <= 44)

points_e16_aadt <- points_e16$trp_id %>%
  get_aadt_for_trp_list() %>%
  dplyr::filter(year == 2019) %>%
  dplyr::filter(coverage > 50)

points_e16_aadt_joined <- points_e16 %>%
  dplyr::left_join(points_e16_aadt) %>%
  dplyr::select(-section_number, -year, -coverage,
                -valid_speed_volume, -valid_length_volume)

# Must supply missing AADTs from NVDB based on road reference
missing_aadt <- points_e16_aadt_joined %>%
  dplyr::filter(adt == 0 | is.na(adt)) %>%
  dplyr::mutate(
    adt = mapply(getAadtByRoadlinkposition, road_link_position))

with_aadt <- points_e16_aadt_joined %>%
  dplyr::filter(adt > 0)

points_e16_aadt_final <-
  points_e16_aadt_joined
  #bind_rows(with_aadt, missing_aadt) %>%
  dplyr::select(-road_category_and_number) %>%
  split_road_system_reference()

# Get point indices
points_e16_index <- points_e16$trp_id %>%
  get_pointindices_for_trp_list("2020") %>%
  dplyr::select(-index_total) %>%
  dplyr::filter(day_type == "ALL") %>%
  dplyr::filter(period == "month") %>%
  #dplyr::filter(month %in% c(7)) %>%
  dplyr::select(trp_id, year, month, index_total_p, index_total_coverage) %>%
  dplyr::mutate(index_total_p = round(index_total_p, digits = 1))

points_e16_index_wide <- points_e16_index %>%
  dplyr::mutate(month_as_date = lubridate::ymd(paste(year, month, "01", sep = "-")),
                month_label = lubridate::month(month_as_date, label = TRUE)) %>%
  dplyr::select(trp_id, index_total_p, month_label) %>%
  tidyr::pivot_wider(names_from = month_label, values_from = index_total_p)

points_e16_aadt_index <- points_e16_aadt_final %>%
  dplyr::left_join(points_e16_index_wide) %>%
  dplyr::select(name, road_reference, municipality_name, adt, jan:sep)

#
# points_e16_aadt_index <- points_e16_aadt_final %>%
#   dplyr::left_join(points_e16_index) %>%
#   dplyr::filter(index_total_coverage > 50) %>%
#   dplyr::select(name, road_reference, municipality_name, adt, index_total_p)

point_indices_public_friendly <- points_e16_aadt_index %>%
  dplyr::select(Trafikkregistreringspunkt = name,
                Vegreferanse = road_reference,
                Kommune = municipality_name,
                Årsdøgntrafikk = adt,
                jan:sep)

writexl::write_xlsx(point_indices_public_friendly,
                    path = "e134.xlsx")
