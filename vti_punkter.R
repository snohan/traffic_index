# Punkter til VTI
# Erstattes av vti_trp.Rmd

# Packages ####
library(tidyverse)
library(jsonlite)
library(lubridate)

# DEPRECATED: Functions ####
# UNØDVENDIG, bruk fromJSON direkte!
# Funksjon for å lese inn json-fil og lage data.frame:
jsonToDf <- function(jsonfil) {

  json.inn <- fromJSON(jsonfil,
                       simplifyDataFrame = TRUE,
                       flatten = TRUE)
  #json.df <- flatten(as.data.frame(json.inn))

  return(json.inn)
}


# DEPRECATED: Feil oppsett i Nortraf ####

# Dersom stasjonen har endra oppsett (splittet i mor-datter) klarer vi ikke
# nå å bruke data fra NorTraf.
stasjoner_endret_oppsett <- read.csv2("endret_oppsett.csv") %>%
  select(1, 2) %>%
  mutate(msnr = as.character(msnr))

# TODO: hente info fra trp-app i stedet for Kibana

Les_inn_nortraf_json <- function(filnavn) {

  mstr_nortraf <- jsonToDf(filnavn) %>%
    select(msnr = '_id',
           #mor = '_source.measure_point_number_mother',
           navn = '_source.station_name',
           nortrafstatus = '_source.nortraf_status',
           nivaa = '_source.traffic_counting_station_type',
           nivaa_id = '_source.traffic_counting_station_type_id',
           #virtuelt = '_source.is_virtual',
           #datalogger_nortraf = '_source.instrument_type_name',
           trafikantgruppe = '_source.traffic_registration_type',
           fylke = '_source.county_id',
           vegkategori = '_source.road_reference.road_category',
           vegstatus = '_source.road_reference.road_status',
           vegnr = '_source.road_reference.road_number',
           parsell = '_source.road_reference.main_parcel',
           meter = '_source.road_reference.meter',
           #utm_east = '_source.utm_coord_east',
           #utm_north = '_source.utm_coord_north',
           lat = '_source.location.lat',
           lon = '_source.location.lon'
    )
}

Les_inn_datainn_json <- function(filnavn) {

  mstr_datainn <- jsonToDf(filnavn) %>%
    select(msnr = '_id',
           igangsatt = '_source.operational_in_datainn_utc_timestamp',
           sambandsnr = '_source.network.network_access_number',
           ip = '_source.network.host',
           ruter_ip = '_source.network.router_ip',
           operasjonell_datainn = "_source.is_operational_in_datainn",
           datalogger_datainn = '_source.device_type',
           firmware = '_source.firmware_version'
    ) %>%
    mutate(igangsatt = round_date(with_tz(ymd_hms(igangsatt)), unit = "day"))
}

mstr_nortraf <- Les_inn_nortraf_json("mstr_nortraf.json")
mstr_datainn <- Les_inn_datainn_json("mstr_datainn.json")

vti_stasjoner <- left_join(mstr_datainn, mstr_nortraf,
                           by = c("msnr")) %>%
  filter(nivaa_id == 1) %>%
  filter(trafikantgruppe == 1) %>%
  select(msnr, igangsatt, navn, nivaa, fylke:lon) %>%
  arrange(fylke, vegkategori, vegnr, parsell, meter) %>%
  left_join(stasjoner_endret_oppsett, by = c("msnr")) %>%
  filter(is.na(endret) | endret == "Nei") %>%
  mutate(fylke = recode(fylke, "16" = 50L, "17" = 50L),
         msnr = as.integer(msnr))

# DEPRECATED: Telledager fra NorTraf ####
telledager <- read.csv2("telledager_2018.csv") %>%
  select(msnr, dager)

# Punktindekser ####
# Bruk siste års punktindekser for å finne problempunkter
# Spørs hvilket år problemet var i...
punktindeks <- read.csv2("pointindex-2018.csv") %>%
  filter(periode == "Hittil i år" & døgn == "Alle" &
           lengdeklasse == "Alle") %>%
  select(msnr, dekning, indeks)

# ÅDT ####
adt <- read.csv2("adt_2017_nortraf.csv") %>%
  select(1, 7:9) %>%
  filter(Felt == "R0" & L.Klasse == 20) %>%
  rename(msnr = Tellepunkt) %>%
  select(1, 4) %>%
  mutate(ADT = round(ADT, digits = -2))

# Commissions ####
# Commission-historikk fra trfaikkdata-API
# {
#   trafficRegistrationPoints {
#     id
#     commissions {
#       validFrom
#       validTo
#     }
#   }
# }
# TODO: Kalle API-et direkte

commissions <- jsonToDf("commissions.json") %>%
  unnest() %>%
  mutate(validFrom = round_date(with_tz(ymd_hms(validFrom)), unit = "day"),
         validTo = with_tz(ymd_hms(validTo))) %>%
  rename(trp = id) %>%
  group_by(trp) %>%
  summarise(min(validFrom)) %>%
  rename(igangsetting_1 = "min(validFrom)")

# Sammenheng TRS-TRP ####

# Sammenhengen mellom TRS og TRP fra TRP-app.
# Hent trp under sensorconfig!
# {
#   trafficRegistrationStations(stationType: CONTINUOUS, trafficType: VEHICLE){
#     id
#     sensorConfigurations{
#       legacyNortrafMpn
#       trafficRegistrationPoint{
#         id
#       }
#     }
#   }
#   }
# (OBS! Mangler de som er svartelista!)

trs_trp <- jsonToDf("trs_trp.json") %>%
  unnest() %>%
  rename(msnr = legacyNortrafMpn,
         trp = trafficRegistrationPoint.id) %>%
  select(-1) %>%
  arrange(msnr)

# Slår sammen
trs_first_commission <- left_join(trs_trp, commissions) %>%
  right_join(vti_stasjoner) %>%
  select(msnr, navn, fylke:lon, igangsetting_1) %>%
  arrange(msnr) %>%
  # Tar bort ramper
  filter(parsell < 70) %>%
  left_join(telledager) %>%
  left_join(punktindeks) %>%
  left_join(adt)

write.csv2(trs_first_commission, file = "vti_punkter_2019.csv",
           row.names = F)

# For nærme og kø ####
# Tar inn punkter angitt som for tette og kø
tette_punkter <- read.csv2("vti_punkter_2019_tette.csv") %>%
  select(msnr, tette)

ko_punkter <- read.csv2("punkter_med_ko.csv") %>%
  filter(!is.na(legacyNortrafMpn)) %>%
  select(legacyNortrafMpn) %>%
  as.list()

# Liste over de vi ikke vil ha med
#tas_ut <- c(100153)

# Filterer og legger inn info om tette og kø
vti_punkter_2019 <- trs_first_commission %>%
  left_join(tette_punkter) %>%
  filter(tette != "nei") %>%
  filter(!msnr %in% ko_punkter) %>%
  filter(dager > 279 & igangsetting_1 < "2018-01-01") %>%
  filter(vegkategori != "K") %>%
  filter(ADT > 500) %>%
  mutate(vegkategori = recode(vegkategori, "E" = "R"),
         adt_klasse = case_when(ADT < 1000 ~ "1",
                                ADT < 4000 ~ "2",
                                ADT < 8000 ~ "3",
                                ADT < 12000 ~ "4",
                                ADT >= 12000 ~ "5")) %>%
  mutate(adt_klasse = as.factor(adt_klasse))

write.csv2(vti_punkter_2019, file = "vti_punkter_2019_filtrert.csv",
           row.names = F)

# Apparattype ####
# Firmwareoppgradering på EMU3 høsten 2018 førte til endring i
# lengdeklassifisering - såpass mye for "tunge" at vi ikke kan bruke EMU3 i
# grunnlaget for lengdeklasseindeks i 2019. Derfor lager vi en egen
# lengdeklassevariant av VTI der kun Loop Monitor inngår.
# Må derfor finne ut hivlke stasjoner som har hatt LM i hele 2018.

apparattype <- jsonToDf("firmware_history.json")

apparathistorikk <- apparattype %>%
  dplyr::select(5:8) %>%
  dplyr::rename(msnr = 1,
                timestamp = 2,
                firmware = 3,
                apparattype = 4) %>%
  dplyr::group_by(msnr) %>%
  dplyr::arrange(msnr, timestamp) %>%
  dplyr::mutate(timestamp_start = round_date(with_tz(ymd_hms(timestamp)),
                                       unit = "hour"),
                timestamp_end = dplyr::lead(timestamp_start),
                firmware_short = str_sub(firmware, 1, 3),
                apparattype_utledet = case_when(
                  firmware_short == "1.1" ~ "LM",
                  firmware_short == "Pri" ~ "EMU3",
                  firmware_short == "1.0" ~ "EMU3",
                  # Det er en del CMUer som også har 1.0, så dette blir feil,
                  # men er ok når det gjelder VTI. Sykkelstasjoner filtreres ut.
                  firmware_short == "2.1" ~ "CMU",
                  firmware_short == "2.5" ~ "CMU",
                )) %>%
  dplyr::select(-timestamp)# %>%
  #dplyr::filter(timestamp_end > "2018-01-01 00:00:00")

# Hvilke firmwareversjoner har vi og hvilke apparater skal de mappes til?
firmware_versjoner <- apparattype$firmware_short %>%
  unique()

# Antall ulike apparater per stasjon
antall_apparater <- apparathistorikk %>%
  dplyr::group_by(msnr) %>%
  dplyr::summarise(antall = n_distinct(apparattype_utledet)) %>%
  dplyr::filter(antall > 1)

# Hvilket apparat stasjonen har hatt
apparater_hvilke <- apparathistorikk %>%
  dplyr::select(msnr, apparattype_utledet) %>%
  dplyr::distinct() %>%
  dplyr::filter(!(msnr %in% antall_apparater$msnr))

vti_punkter_2019_lm <- vti_punkter_2019  %>%
  dplyr::filter(!(msnr %in% antall_apparater$msnr)) %>%
  dplyr::left_join(apparater_hvilke) %>%
  dplyr::filter(apparattype_utledet == "LM")

# Antall per fylke ####
ant_per_fylke_veg <- vti_punkter_2019_lm %>%
  group_by(fylke, vegkategori) %>%
  summarise(antall = as.double(n()))

write.csv2(vti_punkter_2019_lm, file = "vti_punkter_2019_kun_loop_monitor.csv",
           row.names = F)

# Kart ####
# TODO: Kart