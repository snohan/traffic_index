#
# Ser på timeverdier fra et punkt.
#

# Pakker ####
library(tidyverse)
library(lubridate)
library(PerformanceAnalytics)

fil <- "500117-volumes-2017.csv"
fil <- choose.files()
mstr <- read.csv2("stasjoner_og_ip.csv")

# Leser inn mstr-info ####

# Innlesing av timeverdier ####
les_inn_timeverdier <- function(fil) {

  timeverdier <- read_delim(fil, delim = ";") %>%
    select(1, 4:13) %>%
    rename(msnr = measure_point_number,
           felt = lane,
           volum = '[..,..)',
           korte = '[..,5.6)',
           lange = '[5.6,..)',
           fem_til_sju = '[5.6,7.6)',
           sju_til_tolv = '[7.6,12.5)',
           tolv_seksten = '[12.5,16.0)',
           over_seksten = '[16.0,..)') %>%
    mutate(interval_start = str_sub(interval_start, 1, 22)) %>%
    mutate(interval_start = str_replace(interval_start, ":00", ":00:00")) %>%
    mutate(volum = as.numeric(volum),
           korte = as.numeric(korte),
           lange = as.numeric(lange),
           fem_til_sju = as.numeric(fem_til_sju),
           sju_til_tolv = as.numeric(sju_til_tolv),
           tolv_seksten = as.numeric(tolv_seksten),
           over_seksten = as.numeric(over_seksten))

  timeverdier$interval_start <-
    with_tz(ymd_hms(timeverdier$interval_start), "CET")

  timeverdier_summert <- timeverdier %>%
    filter(completeness > 0.99) %>%
    filter(!is.na(korte)) %>%
    mutate(aar = year(interval_start),
           maaned = month(interval_start),
           dag = day(interval_start),
           time = hour(interval_start),
           feltnr_odde = (felt %% 2 != 0),
           retning = case_when(feltnr_odde == T ~ "medretning",
                               feltnr_odde == F ~ "motretning")) %>%
    mutate(felt = as.character(felt)) %>%
    group_by(msnr, felt, retning,
             aar, maaned, dag, time) %>%
    summarise(volum = sum(volum, na.rm = T),
              under_12.5 = sum(korte, na.rm = T) +
                sum(fem_til_sju, na.rm = T) +
                sum(sju_til_tolv, na.rm = T),
              over_12.5 = sum(tolv_seksten, na.rm = T) +
                sum(over_seksten, na.rm = T),
              ant = n(),
              ant0 = sum(volum == 0, na.rm = T)
              )

  return(timeverdier_summert)
}

# Summering til ÅDT ####
summer_til_adt <- function(dt){
  # Tar inn en gruppert dt.

  dt_ut <- dt %>%
  summarise(adt_alle =
              round(sum(volum) / sum(ant_timer) * 24,
                    digits = 0),
            adt_under_12.5_m =
              round(sum(under_12.5_m) / sum(ant_timer) * 24,
                    digits = 0),
            adt_over_12.5_m =
              round(sum(over_12.5_m) / sum(ant_timer) * 24,
                    digits = 0),
            andel_over_12.5_m =
              round(sum(over_12.5_m) / sum(volum) * 100,
                    digits = 0),
            standardavvik_alle = round(sd(volum), digits = 0),
            standardavvik_under_12.5_m = round(sd(under_12.5_m), digits = 0),
            standardavvik_over_12.5_m = round(sd(over_12.5_m), digits = 0),
            ant_timer = sum(ant_timer),
            dekningsgrad = round((sum(ant_timer) / 8760 ) * 100, digits = 0),
            andel_nulltimer_felt = round(
              (sum(ant_nulltimer_felt) /
                 (sum(ant_timer) * median(ant_felt))) * 100,
              digits = 0),
            andel_nulltimer = round(
              sum(ant_nulltimer) / sum(ant_timer) * 100, digits = 0))

  return(dt_ut)
}

# Innlesing av enkeltkjøretøy ####
les_inn_vbv <- function(fil) {

  vbv <- read_csv2(fil) %>%
    filter(equipment_local_timestamp != "Alle passeringer er med i filen") %>%
    arrange(equipment_local_timestamp) %>%
    rename(length = 'length_(m)',
           speed = 'speed_(km/h)') #%>%
    #filter(length >= 1.8)

  vbv$equipment_local_timestamp <-
    with_tz(ymd_hms(vbv$equipment_local_timestamp), "CET")

  vbv_tblt <- as_tbl_time(vbv, index = equipment_local_timestamp) %>%
    mutate(equipment_local_timestamp =
             collapse_index(equipment_local_timestamp, "hourly")) %>%
    group_by(equipment_local_timestamp) %>%
    summarise(tvolum = n())

  return(vbv_tblt)
}

#####
# timetrafikk_100190_2017 <-
#   les_inn_vbv("100190_20171001000000-20171201000000.csv")
#
# tp <- timetrafikk_100190_2017 %>%
#   filter(equipment_local_timestamp < "2017-10-24 00:00:00") #%>%
#  filter(equipment_local_timestamp >= "2017-11-01 00:00:00")

sum(tp$tvolum)

# Leser inn Nortraffil ####
nortraf_100190 <- read_csv2("nortraf_100190-10-11-2017.csv", col_names = F) %>%
  select(1:4)

colnames(nortraf_100190) <- c("dag", "time", "felt", "volum")

nortraf_100190$dag <- dmy(nortraf_100190$dag)

nortraf_100190_tblt <- as_tbl_time(nortraf_100190, index = dag)%>%
  mutate(dag = collapse_index(dag, "daily")) %>%
  group_by(dag) %>%
  summarise(dvolum = sum(volum))

# Kongsvinger V jan 2018 ####
fil1 <- "400021_20170101000000-20170201000000.csv"
fil2 <- "400021_20180101000000-20180201000000.csv"

kongsvingerv2017 <- les_inn_vbv(fil1) %>%
  filter(equipment_local_timestamp > "2017-01-01")
kongsvingerv2018 <- les_inn_vbv(fil2) %>%
  filter(equipment_local_timestamp > "2018-01-01")

basis_600176 <- les_inn_timeverdier("volumes-2017_600176.csv", "monthly")
bereg_600176 <- les_inn_timeverdier("volumes-2018_600176.csv", "monthly")

bereg_400021_enkeltkj <- vbv %>%
  as_tbl_time(index = equipment_local_timestamp) %>%
  filter_time('2018-01-02' ~ '2018-01-02')

# ÅDT-beregning ####
beregn_adt <- function(fil) {

  # Leser inn timeverdiene summert på lengdeklasser.
  timeverdier <- les_inn_timeverdier(fil)

  # Kjørefelt
  dt_felt <- timeverdier %>%
    group_by(msnr, felt, retning, aar, maaned, dag) %>%
    summarise(ant_felt = 1,
              volum = sum(volum),
              under_12.5_m = sum(under_12.5),
              over_12.5_m = sum(over_12.5),
              ant_timer = n(),
              ant_nulltimer_felt = sum(ant0),
              ant_nulltimer = sum(ant0))

  adt_felt <- dt_felt %>%
    group_by(msnr, felt, ant_felt, aar) %>%
    summer_til_adt()

  # Finner hvilke felter som fins
  dt_felter <- timeverdier %>%
    group_by(retning) %>%
    filter(volum != 0) %>%
    select(felt) %>%
    distinct(felt)

  dt_felter_retning <- dt_felter %>%
    summarise(ant_felt_fasit = n())

  # Kjøreretning
  retning_time <- timeverdier %>%
    group_by(msnr, retning, aar, maaned, dag, time) %>%
    summarise(volum = sum(volum),
              under_12.5_m = sum(under_12.5),
              over_12.5_m = sum(over_12.5),
              ant_nulltimer = sum(ant0),
              ant_felt = n()) %>%
    left_join(dt_felter_retning, by = "retning") %>%
    filter(ant_felt == ant_felt_fasit)

  dt_retning <- retning_time %>%
    group_by(msnr, retning, ant_felt, aar, maaned, dag) %>%
    summarise(volum = sum(volum),
              under_12.5_m = sum(under_12.5_m),
              over_12.5_m = sum(over_12.5_m),
              ant_timer = n(),
              ant_nulltimer_felt = sum(ant_nulltimer),
              ant_nulltimer = sum(volum == 0))

  adt_retning <- dt_retning %>%
    group_by(msnr, retning, ant_felt, aar) %>%
    summer_til_adt() %>%
    rename(felt = retning)

  # Hele snittet.
  snittet_time <- timeverdier %>%
    group_by(msnr, aar, maaned, dag, time) %>%
    summarise(ant_felt = n(),
              volum = sum(volum),
              under_12.5_m = sum(under_12.5),
              over_12.5_m = sum(over_12.5),
              ant_nulltimer = sum(ant0)) %>%
    filter(ant_felt == nrow(dt_felter))

  dt_snitt <- snittet_time %>%
    group_by(msnr, ant_felt, aar, maaned, dag) %>%
    summarise(volum = sum(volum),
              under_12.5_m = sum(under_12.5_m),
              over_12.5_m = sum(over_12.5_m),
              ant_timer = n(),
              ant_nulltimer_felt = sum(ant_nulltimer),
              ant_nulltimer = sum(volum == 0))

  adt_snittet <- dt_snitt %>%
    group_by(msnr, ant_felt, aar) %>%
    summer_til_adt() %>%
    mutate(felt = "alle") %>%
    select(msnr, felt, ant_felt:andel_nulltimer)

  # Setter sammen felt, retning og hele.
  adt <- bind_rows(adt_felt,
                   adt_retning,
                   adt_snittet)

  return(adt)
}

hent_metainfo <- function(adt){
  # Legger på målestasjonsinfo
  adt_mstr <- left_join(adt, mstr, by = "msnr") %>%
    select(msnr, navn, vegkategori, vegstatus, parsell, meter,
           igangsatt, felt:andel_nulltimer) %>%
    ungroup() %>%
    mutate(msnr = as.numeric(msnr)) %>%
    arrange(msnr)

  return(adt_mstr)
}

# Utførende kode ÅDT ####

setwd("./lane_volumes_apr")
filer <- list.files(".")

#adt_test <- beregn_adt(fil)

adt4 <- do.call(rbind, lapply(filer[80:120], beregn_adt))
adt <- rbind(adt, adt2, adt3, adt4)
adt_mstr <- adt %>% hent_metainfo()

write.csv2(adt_mstr, file = "adt_2017_apr.csv", row.names = F)

test <- les_inn_timeverdier(fil)
#
# ÅDT-testing ####

adt_gammel <- read.csv2("adt_2017.csv") %>%
  select(-adt_nortraf)
adt_nortraf <- read.csv2("adt_2017_nortraf.csv") %>%
  mutate(Felt = str_replace(Felt, "R0" , "alle")) %>%
  mutate(Felt = str_replace(Felt, "R1" , "medretning")) %>%
  mutate(Felt = str_replace(Felt, "R2" , "motretning")) %>%
  select(1, 7, 9, 10)

adt_r_nortraf <- left_join(adt_gammel, adt_nortraf,
                           by = c("msnr" = "Tellepunkt",
                                  "felt" = "Felt")) %>%
  rename(adt_nortraf = ADT,
         std_nortraf = ÅDTavvik,
         adt_r = adt_alle,
         std_r = standardavvik_alle) %>%
  select(msnr:aar, adt_nortraf, adt_r, std_nortraf, std_r,
         ant_timer:andel_nulltimer) %>%
  mutate(reldiff_adt_r_nortraf = 100 * (adt_r - adt_nortraf) / adt_nortraf,
         diff_std_r_nortraf = std_nortraf - std_r)

adt_r_nortraf %>%
  filter(!is.na(diff_adt_r_nortraf)) %>%
  ggplot(aes(dekningsgrad, diff_adt_r_nortraf)) +
  geom_point()

adt_r_nortraf %>%
  filter(!is.na(diff_adt_r_nortraf)) %>%
  ggplot(aes(andel_nulltimer_felt, diff_adt_r_nortraf)) +
  geom_point()

# Leser inn aggregeringer gjort direkte fra ES

adt_es <- read.csv2("adt-es.csv") %>%
  rename(adt_es = adt_alle,
         std_es = standardavvik)

adt_es_hour <- read.csv2("adt-es-hour.csv") %>%
  rename(adt_es_hour = adt_alle,
         std_es_hour = standardavvik)

adt_es_filt <- read.csv2("adt-es-hour-filtered.csv") %>%
  rename(adt_es_filt = adt_alle,
         std_es_filt = standardavvik)

adt_r_nortraf_es <- inner_join(adt_r_nortraf, adt_es,
                               by = c("msnr" = "msnr",
                                      "felt" = "felt")) %>%
  mutate(reldiff_adt_es_nortraf = 100 * (adt_es - adt_nortraf) / adt_nortraf)

adt_r_nortraf_es_hour <- inner_join(adt_r_nortraf_es, adt_es_hour,
                               by = c("msnr" = "msnr",
                                      "felt" = "felt")) %>%
  mutate(reldiff_adt_es_hour_nortraf = 100 * (adt_es_hour - adt_nortraf) / adt_nortraf)

adt_alle <- inner_join(adt_r_nortraf_es_hour, adt_es_filt,
                                    by = c("msnr" = "msnr",
                                           "felt" = "felt")) %>%
  mutate(reldiff_adt_es_filt_nortraf = 100 * (adt_es_filt - adt_nortraf) / adt_nortraf)

adt_sammenligning <- adt_alle %>%
  filter(!is.na(adt_nortraf)) %>%
  #filter(adt_nortraf > 500) %>%
  select(msnr, felt, dekningsgrad, andel_nulltimer_felt, adt_nortraf,
         adt_es, adt_es_hour, adt_es_filt, adt_r,
         reldiff_adt_r_nortraf, reldiff_adt_es_nortraf,
         reldiff_adt_es_hour_nortraf, reldiff_adt_es_filt_nortraf)

write.csv2(adt_sammenligning, file = "adt_sammenligning_2.csv", row.names = F)

adt_alle %>%
  filter(!is.na(adt_nortraf)) %>%
  #filter(adt_nortraf > 500) %>%
  filter(felt != "alle") %>%
  #filter(dekningsgrad > 50) %>%
  #filter(andel_nulltimer_felt < 10) %>%
  select(dekningsgrad, andel_nulltimer_felt,
         reldiff_adt_r_nortraf, reldiff_adt_es_nortraf,
         reldiff_adt_es_hour_nortraf, reldiff_adt_es_filt_nortraf) %>%
  chart.Correlation()

adt_alle %>%
  filter(!is.na(adt_nortraf)) %>%
  filter(adt_nortraf > 500) %>%
  filter(felt == "alle") %>%
  select(dekningsgrad, andel_nulltimer_felt,
         reldiff_adt_r_nortraf, reldiff_adt_es_nortraf,
         reldiff_adt_es_hour_nortraf, reldiff_adt_es_filt_nortraf) %>%
  chart.Correlation()

adt_alle %>%
  filter(!is.na(adt_nortraf)) %>%
  #filter(adt_nortraf > 500) %>%
  filter(felt != "alle") %>%
  filter(dekningsgrad < 110) %>%
  #filter(andel_nulltimer_felt > 20) %>%
  select(dekningsgrad, andel_nulltimer_felt,
         adt_nortraf, adt_es, adt_es_hour, adt_es_filt, adt_r) %>%
  chart.Correlation()

#adt_linjeplott <-
  adt_r_nortraf_es_hour %>%
  filter(felt != "alle") %>%
  filter(!is.na(adt_nortraf)) %>%
  filter(dekningsgrad < 20) %>%
  select(msnr, felt, adt_nortraf, adt_r, adt_es, adt_es_hour) %>%
  gather(kilde, adt, adt_nortraf:adt_es_hour) %>%
  unite(msnr_felt, msnr:felt) %>%
  ggplot() +
  geom_point(aes(msnr_felt, adt, colour = kilde, group = 1))

  sum_avvik_abs <-
  adt_alle %>%
    summarise(sum_avvik_r = sum(abs(reldiff_adt_r_nortraf), na.rm = T),
              sum_avvik_es = sum(abs(reldiff_adt_es_nortraf), na.rm = T),
              sum_avvik_es_hour = sum(abs(reldiff_adt_es_hour_nortraf), na.rm = T),
              sum_avvik_es_filt = sum(abs(reldiff_adt_es_filt_nortraf), na.rm = T))

  sum_avvik <-
    adt_alle %>%
    filter(felt != "alle") %>%
    summarise(sum_avvik_r = sum(reldiff_adt_r_nortraf, na.rm = T),
              sum_avvik_es = sum(reldiff_adt_es_nortraf, na.rm = T),
              sum_avvik_es_hour = sum(reldiff_adt_es_hour_nortraf, na.rm = T),
              sum_avvik_es_filt = sum(reldiff_adt_es_filt_nortraf, na.rm = T))

sum_avvik2 <- adt_alle %>%
  filter(felt != "alle") %>%
  filter(adt_nortraf > 500) %>%
  filter(!is.na(adt_nortraf)) %>%
  select(starts_with("reldiff")) %>%
  summarise_all(funs(xmin = min,
                     xq25 = quantile(., 0.25),
                     xmedian = median,
                     xq75 = quantile(., 0.75),
                     xmax = max,
                     xmean = mean,
                     xsd = sd)) %>%
  gather(stat, val) %>%
  mutate(val = round(val, 2)) %>%
  separate(stat, into = c("var", "stat"), sep = "_x") %>%
  spread(stat, val) %>%
  select(var, min, q25, median, q75, max, mean, sd)

#avvik_fordeling <-
  adt_alle %>%
  filter(felt != "alle") %>%
  filter(!is.na(adt_nortraf)) %>%
  select(starts_with("reldiff")) %>%
  gather(metode, avvik) %>%
  ggplot() +
  geom_density(aes(avvik, color = metode))


##

# EMU3 Stengelsrud ####
januar2018_600176 <- vbv

ggplot(januar2018_600176,
       aes(equipment_local_timestamp, length, colour = lane_number)) +
  geom_point()

# ... ####

#

#
# Slutt.
#