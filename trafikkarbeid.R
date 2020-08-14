#
# Summering av trafikkarbeid henta via NVDB123
# Henter ut rapporten ÅDT og fart med vegnett gyldig
# 31.12. det aktuelle året, med fagdata gyldig 31.12. året etter.
# Inkluderer både eksisterende og midlertidige veger.
# Tar ikke med rundkjøringer eller plasser og lommer.
#
# Etter at Trøndelag ble sammenslått fra 1.1.2018, får vi ikke lenger ta ut
# historisk vegnett. Derfor må alle historiske trafikktall bruke vegnett
# per 1.1.2018 for Trøndelag.
# Tar derfor ut egen rapport for Trøndelag og limer det inn i rapport
# for resten, bortsett fra for 2017  hvor jeg bruker vegnett 1.1.18 for alle.

# Pakker ####
library(tidyverse)
library(writexl)

# Funksjoner ####
les_sdv <- function(sdv_fil) {

  aarstall <- substr(sdv_fil, 12, 15)

  adt_df <- read.csv2(sdv_fil, skip = 1) %>%
    select(1, 3:9, 11, 13, 14) %>%
    mutate(aar = aarstall)

  colnames(adt_df) <- c("fylkenr", "vegkat", "vegstat", "vegnr", "fra_hp",
                        "fra_m", "til_hp", "til_m", "adt_total", "fartsgrense",
                        "parsellengde_m", "aar")
  return(adt_df)
}

Beregn_trafikkarbeid_fra_innlest <- function(innlest) {
  # Må renske innleste fil og beregne tafikkarbeid
  adt_nvdb <- innlest %>%
    mutate(vegkat = str_replace(vegkat, "=", "")) %>%
    mutate(vegstat = str_replace(vegstat, "=", "")) %>%
    filter(vegstat == "V") %>%
    filter(vegkat %in% c("E", "R", "F")) %>%
    filter(!is.na(adt_total)) %>%
    filter(!is.na(fartsgrense)) %>%
    mutate(trafikkarbeid = (adt_total * parsellengde_m * 365)) %>%
  # Endrer fylkenr til nye
    mutate(fylkenr = recode(fylkenr,
                            '1' = 30L, '2' = 30L, '6' = 30L,
                            '4' = 34L, '5' = 34L,
                            '7' = 38L, '8' = 38L,
                            '9' = 42L, '10' = 42L,
                            '12' = 46L, '14' = 46L,
                            '19' = 54L, '20' = 54L))

  return(adt_nvdb)
}

myRounding <- function(vector) {
  round(vector, digits = 0)
}

# Innlesing ####

# Enkeltfil
# adt_2017 <- les_sdv("adt_belagt_2017.sdv")

# Fylkenavn
fylker <- read.csv2("fylker.csv")

# Leser inn filene for alle nedlastede år
adt_filer <- list.files(pattern = "adt_belagt*")
adt_innlest <- do.call(rbind, lapply(adt_filer, les_sdv))

# Beregner trafikkarbeid
adt_nvdb <- Beregn_trafikkarbeid_fra_innlest(adt_innlest)

# Beregner for fylkene ####
# Beregner trafikkarbeid
# per fylke,
# per vegkategori,
# per fartsgrense
trafikkarbeid_nvdb <- adt_nvdb %>%
  mutate(vegkat = str_replace(vegkat, "E", "R")) %>%
  mutate(vegkat = str_replace(vegkat, "R", "E+R")) %>%
  group_by(fylkenr, vegkat,
           #fartsgrense,
           aar) %>%
  # HUSK AT Beregn_trafikkarbeid_fra_innlest MAPPER TIL NYE FYLKER
  summarise(trafikkarbeid = sum(trafikkarbeid) / 1e9,
            veglengde = sum(parsellengde_m) / 1e3,
            #adt_snitt = mean(adt_total),
            adt_arbeid_per_m =
              sum(as.numeric(adt_total * parsellengde_m)) /
              (veglengde * 1e3)) %>%
  ungroup() %>%
  # complete(fylkenr, nesting(vegkat, fartsgrense, aar),
  #          fill = list(trafikkarbeid = 0,
  #                      veglengde = 0,
  #                      adt_snitt = 0,
  #                      adt_snitt_vektet = 0)) %>%
  left_join(fylker, by = c("fylkenr" = "fylkenr")) %>%
  select(fylkenr, navn, aar, vegkat,
         #fartsgrense,
         veglengde, trafikkarbeid,
         adt_arbeid_per_m) %>%
  arrange(fylkenr, desc(vegkat)#, fartsgrense)
  )

trafikkarbeid_E_R <- trafikkarbeid_nvdb %>%
  dplyr::group_by(vegkat, aar) %>%
  dplyr::summarise(trafikkarbeid = sum(trafikkarbeid))

# Intermesso: gjenskaper region og landsdel
region_landsdel <- read.csv2("trafikkarbeidstall_2017.csv") %>%
  select(Fylkenr, Region, Landsdel) %>%
  distinct(Fylkenr, .keep_all = T)

# Trafikkarbeidstall for opplasting til  ####
trafikkarbeid_datainn <- trafikkarbeid_nvdb %>%
  filter(aar == "2018") %>%
  select(navn, fylkenr, vegkat,
         #fartsgrense,
         trafikkarbeid) %>%
  #left_join(region_landsdel, by = c("fylkenr" = "Fylkenr")) %>%
  rename(Fylke = navn,
         Fylkenr = fylkenr,
         Vegkategori = vegkat#,
         #Fartsgrense = fartsgrense
         ) %>%
  select(Fylke, Fylkenr,
         #Region, Landsdel,
         Vegkategori,
         #Fartsgrense,
         trafikkarbeid)

jsonlite::write_json(trafikkarbeid_datainn,
           path = "trafikkarbeid_2019.json",
           prettify = TRUE)

# Framskriving ####
# TODO: Framskriv 2018-trafikkarbeid med 2019-indeks
# Framskrive for gamle fylker? Midle indeks for sammenslåtte fylker?
# Les inn indeks for 2019
indeks_2019 <- read.csv2("Vegtrafikkindeksen-2019-12_2018-12.csv") %>%
  dplyr::filter(lengdeklasse == "Alle",
                periode == "Hittil i år",
                døgn == "Alle") %>%
  dplyr::select(Område, Vegkategori, indeks) %>%
  dplyr::mutate(indeks = as.numeric(str_replace(indeks, ",", "."))) %>%
  dplyr::left_join(fylker, by = c("Område" = "navn")) %>%
  dplyr::filter(Vegkategori != "E+R+F") %>%
  dplyr::filter(!is.na(fylkenr)) %>%
  dplyr::filter(!is.na(indeks)) %>%
  dplyr::group_by(Vegkategori, fylkenr) %>%
  dplyr::distinct() %>%
  dplyr::ungroup() %>%
  dplyr::mutate(fylkenr = recode(fylkenr,
                          '1' = 30L, '2' = 30L, '6' = 30L,
                          '4' = 34L, '5' = 34L,
                          '7' = 38L, '8' = 38L,
                          '9' = 42L, '10' = 42L,
                          '12' = 46L, '14' = 46L,
                          '19' = 54L, '20' = 54L)) %>%
  dplyr::group_by(fylkenr, Vegkategori) %>%
  dplyr::summarise(indeksmiddel = mean(indeks) / 100 + 1)

trafikkarbeid_datainn_framskrevet <- trafikkarbeid_datainn %>%
  dplyr::left_join(indeks_2019,
                   by = c("Fylkenr" = "fylkenr",
                          "Vegkategori" = "Vegkategori")) %>%
  dplyr::mutate(trafikkarbeid_framskrevet =
                  trafikkarbeid * indeksmiddel) %>%
  dplyr::select(-trafikkarbeid, -indeksmiddel) %>%
  dplyr::rename(trafikkarbeid = trafikkarbeid_framskrevet)

jsonlite::write_json(trafikkarbeid_datainn_framskrevet,
           path = "trafikkarbeid_2019.json",
           prettify = TRUE)

# Skriver ut csv-fil som skal lastes opp i Datainn:
 write.csv2(trafikkarbeid_datainn,
            file = "trafikkarbeid_2018.csv")

trafikkarbeid <- read.csv2("trafikkarbeid_2018.csv") %>%
  dplyr::group_by(Vegkategori) %>%
  dplyr::summarise(trafikkarbeid = sum(trafikkarbeid))

# Histogram
trafikkarbeid_nvdb %>%
  group_by(navn, aar, vegkat) %>%
  summarise(trafikkarbeid = sum(trafikkarbeid)) %>%
  ggplot(aes(aar, trafikkarbeid, color = vegkat)) +
  geom_histogram(stat = "identity") +
  #geom_line() +
  facet_wrap(. ~ navn) +
  scale_x_discrete(breaks = c("2001", "2010", "2017")) +
  theme(axis.text.x = element_text(angle = 90))

# Beregner trafikkarbeid
# per fylke,
# per vegkategori,
# alle fartsgrenser
trafikkarbeid_veg_fylke <- adt_nvdb %>%
  mutate(vegkat = str_replace(vegkat, "E", "R")) %>%
  group_by(fylkenr, vegkat, aar) %>%
  summarise(trafikkarbeid = sum(trafikkarbeid) / 1e9,
            veglengde = sum(parsellengde_m) / 1e3,
            #adt_snitt = mean(adt_total),
            adt_arbeid_per_m =
              sum(as.numeric(adt_total * parsellengde_m)) /
              (veglengde * 1e3)) %>%
  left_join(fylker, by = c("fylkenr" = "fylkenr")) %>%
  select(fylkenr, navn, aar, vegkat, veglengde, trafikkarbeid,
         adt_arbeid_per_m) %>%
  arrange(fylkenr, desc(vegkat))

# write.csv2(trafikkarbeid_veg_fylke,
#            file = "trafikkarbeid_veg_fylke_2017.csv",
#            row.names = F)
#
# trafikkarbeid_veg_fylke %>%
#   ungroup() %>%
#   ggplot(aes(aar, trafikkarbeid, color = vegkat, group = vegkat)) +
#   #geom_area(position = "stack") +
#   geom_histogram(stat = "identity") +
#   scale_x_discrete(name = "År", breaks = c("2001", "2010", "2017")) +
#   facet_wrap(. ~ navn)

# Beregner trafikkarbeid
# per fylke,
# alle vegkategorier,
# alle fartsgrenser
trafikkarbeid_fylke <- adt_nvdb %>%
  mutate(vegkat = str_replace(vegkat, "E", "R")) %>%
  group_by(fylkenr, aar) %>%
  summarise(trafikkarbeid = sum(trafikkarbeid) / 1e9,
            veglengde = sum(parsellengde_m) / 1e3,
            #adt_snitt = mean(adt_total),
            adt_arbeid_per_m =
              sum(as.numeric(adt_total * parsellengde_m)) /
              (veglengde * 1e3)) %>%
  left_join(fylker, by = c("fylkenr" = "fylkenr")) %>%
  select(fylkenr, navn, aar, veglengde, trafikkarbeid,
         adt_arbeid_per_m) %>%
  arrange(fylkenr)

# Beregner trafikkarbeid
# alle fylker,
# alle vegkategorier,
# alle fartsgrenser
trafikkarbeid_aar <- adt_nvdb %>%
  group_by(aar) %>%
  summarise(trafikkarbeid = sum(trafikkarbeid) / 1e9,
            veglengde = sum(parsellengde_m) / 1e3,
            #adt_snitt = mean(adt_total),
            adt_arbeid_per_m =
              sum(as.numeric(adt_total * parsellengde_m)) /
              (veglengde * 1e3))

trafikkarbeid_aar %>%
  ggplot(aes(aar, trafikkarbeid)) +
  geom_bar(stat = "identity")

# Summen av alt
# sum(trafikkarbeid_nvdb$trafikkarbeid)

# Lager et Excelark med fire faner
# Må først lage en navgitt liste
excelfaner <- list(fart = trafikkarbeid_nvdb,
                   vegkategori = trafikkarbeid_veg_fylke,
                   fylke = trafikkarbeid_fylke,
                   land = trafikkarbeid_aar)

write_xlsx(excelfaner, path = "trafikkarbeid.xlsx")

#
# Gamle tall i Trøndelag - hva har økt så mye? ####

trondelag <- adt_nvdb %>%
  filter(fylkenr == 50) %>%
  filter(aar %in% c(2016, 2017)) %>%
  mutate(vegkat = str_replace(vegkat, "E", "R")) %>%
  #mutate(vegkat = str_replace(vegkat, "R", "E+R")) %>%
  group_by(fylkenr, aar, vegkat) %>% #, vegstat, vegnr, aar, fra_hp) %>%
  summarise(trafikkarbeid = round(sum(trafikkarbeid, na.rm = T) / 1e9,
                                  digits = 3),
            veglengde = round(sum(parsellengde_m, na.rm = T) / 1e3,
                              digits = 3),
            adt_snitt = mean(adt_total),
            adt_snitt_vektet =
              sum(as.numeric(adt_total * parsellengde_m)) /
              (veglengde * 1e3))

# Leser inn gamle filer hentet fra NVDB i 2017.
adt_st <- read.csv2("adt_16_2016.csv")
adt_nt <- read.csv2("adt_17_2016.csv")

adt_snt <- bind_rows(adt_st, adt_nt) %>%
  select(-2, -3, -4, -16)

adt_snt_veg <- adt_snt %>%
  filter(Vegstatus == "V") %>%
  group_by(Vegkategori, Vegstatus, Vegnummer, Frahp) %>%
  summarise(trafikkarbeid_2016 = round(sum(Trafikkarbeidstall, na.rm = T) / 1e6,
                                  digits = 3),
            veglengde_2016 = round(sum(Parsell.Lengde, na.rm = T) / 1e3,
                              digits = 3))

smnlign <- trondelag %>%
  ungroup() %>%
  select(vegkat, vegstat, vegnr, fra_hp, trafikkarbeid, veglengde) %>%
  full_join(adt_snt_veg, by = c("vegkat" = "Vegkategori",
                                "vegstat" = "Vegstatus",
                                "vegnr" = "Vegnummer",
                                "fra_hp" = "Frahp")) %>%
  mutate(ta_diff = trafikkarbeid - trafikkarbeid_2016,
         vl_dill = veglengde - veglengde_2016)

trondelag_e6 <- adt_nvdb %>%
  filter(fylkenr == 50,
         aar == 2017,
         vegkat == "E",
         vegstat == "V",
         vegnr == 6,
         fra_hp < 4,
         til_hp < 4,
         adt_total < 7000) %>%
  arrange(fra_hp, fra_m) %>%
  mutate(parsellengde_beregnet = til_m - fra_m,
         lengdeavvik = (parsellengde_m)/
           parsellengde_beregnet)

sum(trondelag_e6$parsellengde_m)

adt_snt_e6 <- adt_snt %>%
  filter(Vegkategori == "E",
         Vegstatus == "V",
         Vegnummer == 6,
         Frahp == 4) %>%
  arrange(Frameter)

# Ser ut til at parsellengden er feil
parsellengder <- adt_innlest %>%
  mutate(vegkat = str_replace(vegkat, "=", "")) %>%
  mutate(vegstat = str_replace(vegstat, "=", "")) %>%
  filter(vegstat == "V") %>%
  filter(vegkat %in% c("E", "R", "F")) %>%
  mutate(samme_hp = (fra_hp == til_hp),
         parsellengde_beregnet = til_m - fra_m,
         lengdeavvik = (parsellengde_m)/
           parsellengde_beregnet ) %>%
  filter(samme_hp == TRUE) %>%
  filter(parsellengde_beregnet > 100) %>%
  filter(lengdeavvik != 1) %>%
  group_by(fylkenr, aar) %>%
  summarise(ant = n(),
            median = median(lengdeavvik),
            max = max(lengdeavvik),
            min = min(lengdeavvik))

#
# Sammenligner tall mellom år ####
adt_2016 <- adt_2016 %>%
  mutate(fylkenr = replace(fylkenr, 16:17, 50))

adt_2016_2017 <- full_join(adt_2017, adt_2016,
                           by = c("fylkenr", "vegkat", "vegstat", "vegnr",
                                  "fra_hp", "fra_m"),
                           suffix = c("_2017", "_2016")) %>%
  filter(fylkenr == 19) %>%
  arrange(vegkat, vegnr, fra_hp, fra_m)

adt_2016_2017_sum <- adt_2016_2017 %>%
  group_by(fylkenr, vegkat, vegnr) %>%
  summarise(trafikkarbeid_2016 = sum(trafikkarbeid_2016, na.rm = T),
            trafikkarbeid_2017 = sum(trafikkarbeid_2017, na.rm = T),
            forskjell = (trafikkarbeid_2017/trafikkarbeid_2016 - 1) * 100)

# Behov for antall punkt i indeks ####
# Leser inn for et gitt år og legger på ÅDT-klasse
adt_2018 <- les_sdv("adt_belagt_2018.sdv") %>%
  mutate(vegkat = str_replace(vegkat, "=", "")) %>%
  mutate(vegstat = str_replace(vegstat, "=", "")) %>%
  filter(vegstat == "V") %>%
  filter(vegkat %in% c("E", "R", "F")) %>%
  filter(!is.na(adt_total)) %>%
  filter(!is.na(fartsgrense)) %>%
  filter(adt_total >= 500) %>%
  mutate(trafikkarbeid = (adt_total * parsellengde_m * 365),
         adt_klasse = cut(adt_total,
                           breaks = c(0, 1000, 4000, 8000, 12000, Inf),
                           labels = c("1", "2", "3", "4", "5"),
                           right = F))

# Summerer for hver klasse
adt_klasse_summert <- adt_2018 %>%
  mutate(vegkat = str_replace(vegkat, "E", "R")) %>%
  group_by(fylkenr, vegkat, adt_klasse) %>%
  summarise(trafikkarbeid_mill_km = sum(trafikkarbeid) / 1e9,
            veglengde_km = sum(parsellengde_m) / 1e3,
            parsellengde_m_snitt = mean(parsellengde_m),
            adt_snitt_vektet = round(
              sum(as.numeric(adt_total * parsellengde_m)) /
              (veglengde_km * 1e3),
              digits = 0),
            punkttrafikkarbeid = parsellengde_m_snitt *
              adt_snitt_vektet * 365 * 1E-9)

# Må summere per fylke og veg for å kunne regne relativ andel
adt_summert <- adt_klasse_summert %>%
  group_by(fylkenr, vegkat) %>%
  summarise(trafikkarbeid_fylke_veg_mill_km = sum(trafikkarbeid_mill_km))

# Beregner relativ andel trafikkarbeid
adt_klasse_rel <- left_join(adt_klasse_summert, adt_summert,
                            by = c("fylkenr" = "fylkenr",
                                   "vegkat" = "vegkat")) %>%
  mutate(trafikkarbeid_relandel =
           round(trafikkarbeid_mill_km / trafikkarbeid_fylke_veg_mill_km,
                 digits = 3))

# Henter inn antall punkter per klasse
adt_klasse_rel_pkt <- left_join(adt_klasse_rel, ant_per_fylke_veg,
                            by = c("fylkenr" = "fylke",
                                   "vegkat" = "vegkategori",
                                   "adt_klasse" = "adt_klasse")) %>%
  mutate(antall = if_else(is.na(antall), 0, antall)) %>%
  mutate(trafikkarbeid_maalt = punkttrafikkarbeid * antall)

# Må summere per fylke og veg for å kunne regne relativ andel
adt_klasse_rel_pkt_summert <- adt_klasse_rel_pkt %>%
  group_by(fylkenr, vegkat) %>%
  summarise(trafikkarbeid_maalt_sum = sum(trafikkarbeid_maalt))

# Beregner relativ andel målt trafikkarbeid
antall_punkt_relativt <- left_join(adt_klasse_rel_pkt,
                                   adt_klasse_rel_pkt_summert,
                                   by = c("fylkenr" = "fylkenr",
                                          "vegkat" = "vegkat")) %>%
  mutate(trafikkarbeid_maalt_relandel =
           round(trafikkarbeid_maalt / trafikkarbeid_maalt_sum,
                 digits = 3))

andelsplott("R")

antall_punkt_fylke_veg <- antall_punkt_relativt %>%
  ungroup() %>%
  summarise(ant = sum(ant_punkt))

# Plotter de relative fordelingene for å visuelt sjekke hvor godt
# de ligner
andelsplott <- function(kat) {
  plott <- antall_punkt_relativt %>%
  filter(vegkat == kat) %>%
  select(fylkenr, vegkat, adt_klasse, trafikkarbeid_relandel,
         trafikkarbeid_maalt_relandel) %>%
  gather(variabel, andel, trafikkarbeid_relandel:
         trafikkarbeid_maalt_relandel) %>%
  ggplot() +
  geom_point(aes(adt_klasse, andel, color = variabel)) +
  facet_wrap(~fylkenr)

  return(plott)
}

andelsplott("F")
andelsplott("R")


#
# Vegavd ####
adt_halogaland <- read.csv2("vegavd_halogaland_2017.sdv", skip = 1) %>%
  select(1, 3:9, 11, 13, 14) %>%
  mutate(vegavd = "Midtre Hålogaland")

vegavdkolonnenavn <- c("fylkenr", "vegkat", "vegstat", "vegnr", "fra_hp",
                        "fra_m", "til_hp", "til_m", "adt_total", "fartsgrense",
                        "parsellengde_m", "vegavd")

colnames(adt_halogaland) <- vegavdkolonnenavn

adt_nordland <- read.csv2("vegavd_nordland_2017.sdv", skip = 1) %>%
  select(1, 3:9, 11, 13, 14)%>%
  mutate(vegavd = "Nordland")

colnames(adt_nordland) <- vegavdkolonnenavn

adt_troms <- read.csv2("vegavd_troms_2017.sdv", skip = 1) %>%
  select(1, 3:9, 11, 13, 14)%>%
  mutate(vegavd = "Troms")

colnames(adt_troms) <- vegavdkolonnenavn


adt_vegavd <- bind_rows(adt_halogaland, adt_nordland, adt_troms) %>%
  mutate(vegkat = str_replace(vegkat, "=", "")) %>%
  mutate(vegstat = str_replace(vegstat, "=", "")) %>%
  filter(vegstat == "V") %>%
  filter(vegkat %in% c("E", "R", "F")) %>%
  filter(!is.na(adt_total)) %>%
  filter(!is.na(fartsgrense)) %>%
  mutate(trafikkarbeid = (adt_total * parsellengde_m * 365))

trafikkarbeid_vegavd <- adt_vegavd %>%
  mutate(vegkat = str_replace(vegkat, "E", "R")) %>%
  group_by(vegavd, vegkat) %>%
  summarise(trafikkarbeid_summert = sum(trafikkarbeid) / 1e9,
            veglengde = sum(parsellengde_m) / 1e3,
            adt_snitt = mean(adt_total),
            adt_snitt_vektet =
              sum(adt_total * parsellengde_m) /
              (veglengde * 1e3))

####

#
# Slutt.
#

# Motorveg ####
# Skal beregne trafikkarbeidstall med og uten motorveg.

# Leser inne filene for alle nedlastede år
adt_filer_motorveg <- list.files(pattern = "adt_motorv*")
adt_motorveg <- do.call(rbind, lapply(adt_filer_motorveg, les_sdv)) %>%
  filter(!is.na(adt_total)) %>%
  Beregn_trafikkarbeid_fra_innlest()

## Gammel måte
adt_motorveg <- les_sdv("adt_motorv_2017.sdv") %>%
  filter(!is.na(adt_total)) %>%
  Beregn_trafikkarbeid_fra_innlest()
##

trafikkarbeid_motorveg <- adt_motorveg %>%
  mutate(vegkat = str_replace(vegkat, "E", "R")) %>%
  group_by(fylkenr, vegkat, aar) %>%
  summarise(trafikkarbeid = sum(trafikkarbeid) / 1e9,
            veglengde = sum(parsellengde_m) / 1e3,
            #adt_snitt = mean(adt_total),
            adt_arbeid_per_m =
              sum(as.numeric(adt_total * parsellengde_m)) /
              (veglengde * 1e3)) %>%
  select(fylkenr, vegkat, aar, veglengde, trafikkarbeid,
         adt_arbeid_per_m) %>%
  arrange(fylkenr, desc(vegkat))

# Må gjøre det samme for ikke-motorveg for at
# årsdøgntrafikkarbeidet per meter skal blir korrekt.
adt_filer_ikkemotorveg <- list.files(pattern = "adt_ikkemv*")
adt_ikkemotorveg <- do.call(rbind, lapply(adt_filer_ikkemotorveg, les_sdv)) %>%
  filter(!is.na(adt_total)) %>%
  Beregn_trafikkarbeid_fra_innlest()

# adt_ikkemotorveg <- les_sdv("adt_ikkemv_2017.sdv") %>%
#   filter(!is.na(adt_total))%>%
#   Beregn_trafikkarbeid_fra_innlest()

trafikkarbeid_ikkemotorveg <- adt_ikkemotorveg %>%
  mutate(vegkat = str_replace(vegkat, "E", "R")) %>%
  group_by(fylkenr, vegkat, aar) %>%
  summarise(trafikkarbeid = sum(trafikkarbeid) / 1e9,
            veglengde = sum(parsellengde_m) / 1e3,
            adt_snitt = mean(adt_total),
            adt_arbeid_per_m =
              sum(as.numeric(adt_total * parsellengde_m)) /
              (veglengde * 1e3)) %>%
  select(fylkenr, vegkat, aar, veglengde, trafikkarbeid,
         adt_arbeid_per_m) %>%
  arrange(fylkenr, desc(vegkat))

# Slår sammen:
trafikkarbeid_med_og_uten_motorveg <- trafikkarbeid_veg_fylke %>%
  left_join(trafikkarbeid_motorveg,
            by = c("fylkenr", "vegkat", "aar"),
            suffix = c("_total", "_motorveg")) %>%
  left_join(trafikkarbeid_ikkemotorveg,
            by = c("fylkenr", "vegkat", "aar"),
            suffix = c("", "_ikke_motorveg")) %>%
  mutate_if(is.numeric, coalesce, 0) %>%
  mutate_if(is.numeric, myRounding) %>%
  mutate(veglengde_kontrollsum =
           veglengde_total - veglengde_motorveg - veglengde,
         trafikkarbeid_kontrollsum =
           trafikkarbeid_total - trafikkarbeid_motorveg -
           trafikkarbeid) %>%
  filter(aar %in% c("2017", "2016"))

write.csv2(trafikkarbeid_med_og_uten_motorveg,
           file = "trafikkarbeid_med_og_uten_motorveg.csv",
           row.names = F)

##
