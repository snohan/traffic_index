#
# Samler sammen punktindeksene og beregner aggregerte indekser.
#

# Pakker ####
library(tidyverse)
library(ggthemes)
library(wrapr)
#library(Hmisc)

# Test av før og etter sykkelvolumer ####
punktindeks_before <- read.csv2("pointindex-2018-03_2017-03_before.csv",
                                stringsAsFactors = F) %>%
  select(msnr, døgn:trafikkmengde.basisår)

punktindeks_after <- read.csv2("pointindex-2018-03_2017-03_after.csv",
                                stringsAsFactors = F)%>%
  select(msnr, døgn:trafikkmengde.basisår)

punktindekser_sammenlignet <- full_join(punktindeks_before,
                                        punktindeks_after,
                                        by = c("msnr", "døgn",
                                               "lengdeklasse", "periode"),
                                        suffix = c("_before", "_after")) %>%
  mutate(indeks_forskjell = indeks_before == indeks_after)

punktindekser_sammenlignet_mangler <- anti_join(punktindeks_before,
                                        punktindeks_after,
                                        by = c("msnr", "døgn",
                                               "lengdeklasse", "periode"))


punktindeks_before %>% filter(msnr == 400220,
                              indeks != "-")

# Funksjoner ####

lesInn <- function(fil) {

  innlest <- read.csv2(fil)

return(innlest)
}

csvFormat <- function(df) {
  df2 <- ungroup(df)
  df3 <-
    select(df2,
           navn, vegkat, lengdeklasse, dogn, maaned,
           trafikkmengde.basisaar,
           trafikkmengde.indeksaar, indeks,
           timer, kalendertimer, dekningsgrad)
  return(df3)
}

aggreger.indeks <- function(df) {
  aggDf <- summarise(df,
                     sum.ant_punkter = sum(ant_punkter),
                     sum.trafikkmengde_basisaar =
                       sum(trafikkmengde_basisaar),
                     sum.trafikkmengde_indeksaar =
                       sum(trafikkmengde_indeksaar),
                     indeksen = round(sum(indeks * trafikkarbeid)/
                       sum(trafikkarbeid),
                       digits = 3),
                     stden = round(
                       sqrt(
                       sum(
                         (ant_punkter - 1) * std ^ 2
                         )
                       /
                         sum(ant_punkter - 1)
                       ),
                       digits = 3),
                     kfi = round(
                       qt(0.975, sum.ant_punkter) * stden /
                       sqrt(sum.ant_punkter),
                       digits = 3),
                     sum.trafikkarbeid = sum(trafikkarbeid)) %>%
    rename(ant_punkter = sum.ant_punkter,
           trafikkmengde_basisaar = sum.trafikkmengde_basisaar,
           trafikkmengde_indeksaar = sum.trafikkmengde_indeksaar,
           indeks = indeksen,
           std = stden,
           trafikkarbeid = sum.trafikkarbeid)
  #timer = sum(timer),
  #kalendertimer = sum(kalendertimer),
  #dekningsgrad = (timer / kalendertimer) * 100)
  return(aggDf)
}

byttKomma <- function(v) {
  bytta <- gsub(",", ".", v)
}

# Mars 2018: Langformat på csv  ####
# Trøndelag er fylke nr. 50
# map <- qc("16", "17") :=
#       qc("50", "50")

perioderangering <- c("Januar", "Februar", "Mars", "April", "Mai",
                      "Juni", "Juli", "August", "September", "Oktober",
                      "November", "Desember", "Hittil i år", "Siste 12 måneder")
fylkerangering <- c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10",
                    "11", "12", "14", "15", "16", "17",
                    "50", "18", "19", "20")

punktindekser <- read.csv2("pointindex-2018-01_2017-01.csv",
                           stringsAsFactors = F) %>%
  select(1:4, 11:17) %>%
  filter(døgn == "Alle") %>%
  filter(lengdeklasse == "Alle") %>%
  #filter(periode == "Januar") %>%
  filter(indeks != "-") %>%
  arrange(msnr) %>%
  mutate(vegkategori = str_replace(Vegkategori, "E", "R"),
         indeks = as.numeric(byttKomma(indeks)),
         trafikkmengde_basisaar =
           as.numeric(byttKomma(trafikkmengde.basisår)),
         trafikkmengde_indeksaar =
           as.numeric(byttKomma(trafikkmengde.indeksår))) %>%
  select(-Vegkategori, -trafikkmengde.basisår, -trafikkmengde.indeksår) %>%
  mutate(fylkenr = as.character(fylkenr),
         fylkenr = str_replace(fylkenr, "16", "50"),
         fylkenr = str_replace(fylkenr, "17", "50")) %>%
  mutate(fylkenr = factor(fylkenr, levels = fylkerangering),
         periode = factor(periode, levels = perioderangering))

indeks.fylke.veg <- punktindekser %>%
  group_by(fylkenr, vegkategori, periode) %>%
  summarise(ant_punkter = n(),
            sum.trafikkmengde_basisaar = sum(trafikkmengde_basisaar),
            sum.trafikkmengde_indeksaar = sum(trafikkmengde_indeksaar),
            indeksen = round(
              (sum.trafikkmengde_indeksaar / sum.trafikkmengde_basisaar - 1)
              * 100, digits = 3),
            vekt = 1 / (1 - sum(
              (trafikkmengde_basisaar /
                          sum.trafikkmengde_basisaar) ^ 2)
              ),
            std = round(
              sqrt(vekt * sum(
                (trafikkmengde_basisaar /
                                sum.trafikkmengde_basisaar) *
                               (indeks - indeksen) ^ 2)
                ),
                               digits = 3),
            #std_uvektet = round(sd(indeks), digits = 1),
            kfi = round(qt(0.975, ant_punkter) * std /
                          sqrt(ant_punkter), digits = 3)) %>%
  select(-vekt) %>%
  rename(trafikkmengde_basisaar = sum.trafikkmengde_basisaar,
         trafikkmengde_indeksaar = sum.trafikkmengde_indeksaar,
         indeks = indeksen)

write.csv2(indeks.fylke.veg, file = "indeks_fylke_vegkat_snorre.csv",
           row.names = F)
#

# Minitest ####
# Leser inn alle punktindekser og setter de sammen i en tabell:
filer <- list.files(pattern = "maanedstall_?")
punktindekser <- do.call(rbind, lapply(filer, lesInn))

punktindekser.hittil <- filter(punktindekser, maaned == 11 | maaned == 12) %>%
  group_by(punktnr, basisaar, indeksaar, dogn, lengdeklasse) %>%
  summarise(dager = sum(dager),
            timer = sum(timer),
            kalendertimer = sum(kalendertimer),
            trafikkmengde.basisaar = sum(trafikkmengde.basisaar),
            trafikkmengde.indeksaar = sum(trafikkmengde.indeksaar),
            indeks = (trafikkmengde.indeksaar / trafikkmengde.basisaar - 1)
            * 100,
            dekningsgrad = (timer / kalendertimer) * 100,
          maaned = "hittil") %>%
  select(punktnr:indeksaar, maaned, dogn:dekningsgrad)

punktindekser <- rbind(punktindekser, ungroup(punktindekser.hittil))

# Bredformat.
punktindekser.bred <- dcast(
  as.data.table(
  filter(punktindekser, maaned == "11" | maaned == "12" | maaned == "hittil")),
  punktnr + basisaar + indeksaar + dogn + lengdeklasse ~
  maaned,
  value.var = colnames(select(punktindekser, dager:dekningsgrad)),
  fun.aggregate = identity,
  fill = NA)

punktindekser.bred %<>% select(punktnr:lengdeklasse,
                              ends_with("_11"),
                              ends_with("_12"),
                              ends_with("_hittil"))

# Skriver ut som punktindeks-csv:
write.csv2(punktindekser.bred,
           file = "punktindeks_mini_s_26sep2017.csv",
           row.names = F)

# Leser inn metadata om punktene:
punktmeta <- read.csv2("datainn-vti-test.csv")
punktmeta <- select(punktmeta, 1:7)

punktindekser.meta <- left_join(punktindekser, punktmeta,
                                by = c("punktnr" = "mnr"))

punktindekser.meta %<>% filter(maaned == "12" |
                               maaned == "11" |
                               maaned == "hittil")

# Fylkesindeks per vegkategori ####
punktindekser.gruppert <- group_by(punktindekser.meta,
                                   maaned, lengdeklasse, dogn,
                                   fylke, vegkat)

fylkesindekser.vegkategori <-
  summarise(punktindekser.gruppert,
            sum.trafikkmengde.basisaar =
              sum(trafikkmengde.basisaar),
            trafikkmengde.indeksaar =
              sum(trafikkmengde.indeksaar),
            indeks.ny = 100 *
              (trafikkmengde.indeksaar/
                 sum.trafikkmengde.basisaar - 1),
            timer = sum(timer),
            kalendertimer = sum(kalendertimer),
            dekningsgrad = (timer / kalendertimer) * 100,
            standardavvik = sqrt(wtd.var(x = indeks,
                    weights = trafikkmengde.basisaar/sum.trafikkmengde.basisaar,
                    normwt = TRUE)))

colnames(fylkesindekser.vegkategori)[6] <-
  "trafikkmengde.basisaar"
colnames(fylkesindekser.vegkategori)[8] <-
  "indeks"

#
# Standardavvik ####


# Trafikkarbeid, region, landsdel - sammenheng ####
# Må ha trafikkarbeidstall til vekting:
# Henter tabell fra NVDB og legger til fylkenr
fylker <- read.csv2("fylker.csv")

trafikkarbeid.2017 <- left_join(trafikkarbeid_nvdb,
                                   fylker,
                                   by = c("navn" = "navn")) %>%
  select(-navn) %>%
  group_by(fylkenr, vegkat) %>%
  summarise(trafikkarbeid = sum(trafikkarbeid)) %>%
  ungroup() %>%
  mutate(fylkenr = factor(fylkenr, levels = fylkerangering))

# Må ha sammenhengen mellom fylke og region:
fylke.region.landsdel <- read.csv2("fylke_region_landsdel_2018.csv") %>%
  mutate(fylkenr = factor(fylkenr, levels = fylkerangering))

trafikkarbeid.f.r.l <- left_join(trafikkarbeid.2017,
                                 fylke.region.landsdel,
                                 by = c("fylkenr" = "fylkenr"))

indeks.fylke.veg.trafikkarbeid.region.landsdel <-
  left_join(indeks.fylke.veg, trafikkarbeid.f.r.l,
            by = c("fylkenr" = "fylkenr", "vegkategori" = "vegkat"))

# Fylkesindeks ####
indeks.fylke <- indeks.fylke.veg.trafikkarbeid.region.landsdel %>%
  group_by(fylkenr, periode) %>%
  aggreger.indeks() %>%
  mutate(vegkategori = "R+F") %>%
  select(fylkenr, vegkategori, periode:trafikkarbeid) %>%
  filter(periode %in% c("Januar", "Hittil i år", "Siste 12 måneder"))
#

# Aggregert per vegkategori ####
# Region
fylkesindekser.gruppert.for.region <-
  group_by(fylkesindekser.vegkategori.trafikkarbeid.region.landsdel,
           maaned, lengdeklasse, dogn, vegkat, regionnr)

regionindeks.vegkategori <- aggIndeks(fylkesindekser.gruppert.for.region)

# Landsdel
fylkesindekser.gruppert.for.landsdel <-
  group_by(fylkesindekser.vegkategori.trafikkarbeid.region.landsdel,
           maaned, lengdeklasse, dogn, vegkat, landsdelnr)

landsdelindeks.vegkategori <- aggIndeks(fylkesindekser.gruppert.for.landsdel)

# Land
trafikkarbeid.region.vegkategori <- summarise(group_by(
  trafikkarbeid.region.landsdel,
  Vegkategori, regionnr),
  trafikkarbeid_2015 = sum(trafikkarbeid_2015))

regionindeks.vegkategori.trafikkarbeid <-
  left_join(regionindeks.vegkategori,
            trafikkarbeid.region.vegkategori,
            by = c("regionnr" = "regionnr",
                   "vegkat" = "Vegkategori"))

regionindeks.vegkategori.trafikkarbeid.gruppert <-
  group_by(regionindeks.vegkategori.trafikkarbeid,
           maaned, lengdeklasse, dogn, vegkat)

landsindeks.vegkategori <-
  aggIndeks(regionindeks.vegkategori.trafikkarbeid.gruppert)
#

# Aggregert over vegkategori ####
# trafikkarbeid.fylke <- summarise(group_by(trafikkarbeid, Fylkenr),
#                                   trafikkarbeid_2015 = sum(trafikkarbeid_2015))
#
# trafikkarbeid.fylke.region.landsdel <- left_join(trafikkarbeid.fylke,
#                                                  fylke.region.landsdel,
#                                            by = c("Fylkenr" = "fylkenr"))
#
# fylkesindekser.trafikkarbeid <- left_join(fylkesindekser,
#                                           trafikkarbeid.fylke.region.landsdel,
#                                           by = c("fylke" = "Fylkenr"))

# Region
indeks.region <- indeks.fylke %>%
  left_join(fylke.region.landsdel, by = c("fylkenr" = "fylkenr")) %>%
  group_by(regionnr, vegkategori, periode) %>%
  aggreger.indeks()

indeks.norge <- indeks.region %>%
  group_by(vegkategori, periode) %>%
  aggreger.indeks()

indeks.norge %>%
  ggplot(aes(periode, indeks)) +
  geom_point(size = 4) +
  geom_linerange(aes(ymax = indeks + kfi, ymin = indeks - kfi)) +
  theme_tufte() +
  annotate("text", label = "test")

indeks.region %>%
  ggplot(aes(periode, indeks)) +
  geom_point(size = 4) +
  geom_linerange(aes(ymax = indeks + kfi, ymin = indeks - kfi)) +
  facet_wrap(~ regionnr, scales = "free") +
  theme_minimal()

indeks.fylke %>%
  ggplot(aes(periode, indeks)) +
  geom_point(size = 4) +
  geom_linerange(aes(ymax = indeks + kfi, ymin = indeks - kfi)) +
  facet_wrap(~ fylkenr, scales = "free") +
  theme_minimal()

##
# Gammel aggregering uten SD ####
fylkesindeks.trafikkarbeid.gruppert.region <-
  group_by(fylkesindekser.trafikkarbeid,
           maaned, lengdeklasse, dogn, regionnr)

regionindeks <- aggIndeks(fylkesindeks.trafikkarbeid.gruppert.region)

# Landsdel
fylkesindeks.trafikkarbeid.gruppert.landsdel <-
  group_by(fylkesindekser.trafikkarbeid,
           maaned, lengdeklasse, dogn, landsdelnr)

landsdelindeks <- aggIndeks(fylkesindeks.trafikkarbeid.gruppert.landsdel)

# Land
trafikkarbeid.region <- summarise(group_by(
  trafikkarbeid.region.landsdel,
  regionnr),
  trafikkarbeid_2015 = sum(trafikkarbeid_2015))

regionindeks.trafikkarbeid <-
  left_join(regionindeks,
            trafikkarbeid.region,
            by = c("regionnr" = "regionnr"))

regionindeks.trafikkarbeid.gruppert <-
  group_by(regionindeks.trafikkarbeid,
           maaned, lengdeklasse, dogn)

landsindeks <- aggIndeks(regionindeks.trafikkarbeid.gruppert)

# Setter sammen til punktindeks.csv ####
alle_vegkat <- "E+R+F"
fylker <- read.csv2("fylker.csv")

fylkesindekser.vegkategori.navn <- left_join(fylkesindekser.vegkategori,
                                             fylker,
                                             by = c("fylke" = "nr"))

fylkesindekser.vegkategori.csv <- csvFormat(fylkesindekser.vegkategori.navn)

fylkesindekser$vegkat <- alle_vegkat
fylkesindekser.navn <- left_join(fylkesindekser,
                                 fylker,
                                 by = c("fylke" = "nr"))

fylkesindekser.csv <- csvFormat(fylkesindekser.navn)

regioner <- read.csv2("regioner.csv")
regionindekser.vegkategori.navn <- left_join(regionindeks.vegkategori,
                                             regioner,
                                             by = c("regionnr" = "nr"))

regionindekser.vegkategori.csv <- csvFormat(regionindekser.vegkategori.navn)

regionindeks$vegkat <- alle_vegkat
regionindeks.navn <- left_join(regionindeks,
                               regioner,
                               by = c("regionnr" = "nr"))

regionindeks.csv <- csvFormat(regionindeks.navn)

landsdeler <- read.csv2("landsdeler.csv")
landsdelindeks.vegkategori.navn <- left_join(landsdelindeks.vegkategori,
                                             landsdeler,
                                             by = c("landsdelnr" = "nr"))

landsdelindeks.vegkategori.csv <- csvFormat(landsdelindeks.vegkategori.navn)

landsdelindeks$vegkat <- alle_vegkat
landsdelindeks.navn <- left_join(landsdelindeks,
                                 landsdeler,
                                 by = c("landsdelnr" = "nr"))

landsdelindeks.csv <- csvFormat(landsdelindeks.navn)

landsindeks.vegkategori$navn <- "Norge"
landsindeks.vegkategori.csv <- csvFormat(landsindeks.vegkategori)

landsindeks$navn <- "Norge"
landsindeks$vegkat <- alle_vegkat
landsindeks.csv <- csvFormat(landsindeks)

vegtrafikkindeks <- rbind(fylkesindekser.vegkategori.csv,
                          fylkesindekser.csv,
                          regionindekser.vegkategori.csv,
                          regionindeks.csv,
                          landsdelindeks.vegkategori.csv,
                          landsdelindeks.csv,
                          landsindeks.vegkategori.csv,
                          landsindeks.csv)
vegtrafikkindeks <- rename(vegtrafikkindeks, omraade = navn)

vegtrafikkindeks$vegkat <- as.character(vegtrafikkindeks$vegkat)
vegtrafikkindeks$vegkat[vegtrafikkindeks$vegkat == "R"] <- "E+R"
vegtrafikkindeks$vegkat <- as.factor(vegtrafikkindeks$vegkat)

vegtrafikkindeks$dogn <- as.character(vegtrafikkindeks$dogn)
vegtrafikkindeks$dogn[vegtrafikkindeks$dogn == "alle"] <- "Alle"
vegtrafikkindeks$dogn[vegtrafikkindeks$dogn == "yrke"] <- "Yrkesdøgn"
vegtrafikkindeks$dogn[vegtrafikkindeks$dogn == "helg"] <- "Helgedøgn"
vegtrafikkindeks$dogn <- as.factor(vegtrafikkindeks$dogn)

# Bredformat.
vegtrafikkindeks.bred <- dcast(
  as.data.table(vegtrafikkindeks),
  omraade + vegkat + lengdeklasse + dogn ~
    maaned,
  value.var = colnames(
    select(vegtrafikkindeks, trafikkmengde.basisaar:dekningsgrad)),
  fun.aggregate = identity,
  fill = NA)

vegtrafikkindeks.bred %<>% select(omraade:dogn,
                               ends_with("_11"),
                               ends_with("_12"),
                               ends_with("_hittil"))

write.csv2(vegtrafikkindeks.bred, file = "vti_mini_s_26sep2017.csv")

# Leser inn datainn.csv ####
datainnpkti <- read.csv2("datainn-minitest/pointindex-2016-12.csv",
                         stringsAsFactors = F)
datainnpkti %<>% select(msnr, lengdeklasse, November, Desember) %>% filter (November != "-")
datainnpkti$November <- gsub(",", ".", datainnpkti$November)
datainnpkti$November %<>% as.numeric()

lengdeklasser <- read.csv2("lengdeklasser.csv")
datainnpkti <- left_join(datainnpkti, lengdeklasser,
                         by = c("lengdeklasse" = "lengdeklasse_ord"))

punktindekser.alle %<>% filter(dogn == "alle")
punktindekser.alle$indeks_11 %<>% round(digits = 3)
punktindekser.alle$indeks_12 %<>% round(digits = 3)
punktindekser.alle$indeks_hittil %<>% round(digits = 3)
pi.sammenlign <- left_join(punktindekser.alle, datainnpkti,
                           by = c("lengdeklasse" = "lengdeklasse_tall",
                                  "punktnr" = "msnr"))
pi.sammenlign <- mutate(pi.sammenlign,
                        diff_nov = round(indeks_11 - November, digits = 3),
                        diff_des = round(indeks_12 - Desember, digits = 3))

write.csv2(pi.sammenlign,
           file = "punktindeks_minitest_sammenlignet_20170331.csv",
           row.names = F)

#
datainnvti <- read.csv2("datainn-minitest/vegtrafikkindex-2016-12.csv",
                        stringsAsFactors = F)
datainnvti %<>% filter(Desember != "-")
datainnvti$November <- gsub(",", ".", datainnvti$November)
datainnvti$Desember <- gsub(",", ".", datainnvti$Desember)
datainnvti$Siste_12_mnd <- gsub(",", ".", datainnvti$Siste_12_mnd)
datainnvti$Hittil.i.år <- gsub(",", ".", datainnvti$Hittil.i.år)

datainnvti$November %<>% as.numeric()
datainnvti$Desember %<>% as.numeric()
datainnvti$Siste_12_mnd %<>% as.numeric()
datainnvti$Hittil.i.år %<>% as.numeric()

datainnvti <- left_join(datainnvti, lengdeklasser,
                        by = c("Lengdeklasse" = "lengdeklasse_ord"))

vegtrafikkindeks.bred$indeks_11 %<>% as.numeric() %>% round(digits = 3)
vegtrafikkindeks.bred$indeks_12 %<>% round(digits = 3)
vegtrafikkindeks.bred$indeks_hittil %<>% round(digits = 3)
vegtrafikkindeks.bred$dogn %<>% as.character()
str(vegtrafikkindeks.bred)
str(datainnvti)
vegtrafikkindeks.bred$dogn <- gsub("døgn", "", vegtrafikkindeks.bred$dogn)
datainnvti$Døgn <- gsub("døgn", "", datainnvti$Døgn)

vti.sammenlign <- left_join(vegtrafikkindeks.bred, datainnvti,
                            by = c("omraade" = "Område",
                                   "vegkat" = "Vegkategori",
                                   "lengdeklasse" = "lengdeklasse_tall",
                                   "dogn" = "Døgn"))

vti.sammenlign <- mutate(vti.sammenlign,
                         diff_11 = round(indeks_11 - November, digits = 3),
                         diff_12 = round(indeks_12 - Desember, digits = 3),
                         diff_hittil =
                           round(indeks_hittil - Hittil.i.år, digits = 3))

vti.sammenlign.barediff <- select(vti.sammenlign,
                                  omraade:dogn,
                                  indeks_11, indeks_12, indeks_hittil,
                                  November:diff_hittil) %>%
  filter(!is.na(diff_11))

write.csv2(vti.sammenlign,
           file = "vti_minitest_sammenlignet_20170404.csv")

# Byindeks minitest ####
# Per vegkategori
bypunktindekser.meta <- punktindekser.meta

# Skal her skille på E og R, og Ålesund aust er R
bypunktindekser.meta$vegkat <-
  gsub("R", "E", bypunktindekser.meta$vegkat)

bypunktindekser.meta[,"vegkat"][bypunktindekser.meta[,"punktnr"] == 1500009 ] <- "R"

bypunktindekser.gruppert <- group_by(bypunktindekser.meta,
                                     maaned, lengdeklasse, dogn,
                                     vegkat)

byindekser.vegkategori <-
  summarise(bypunktindekser.gruppert,
            sum.trafikkmengde.basisaar =
              sum(trafikkmengde.basisaar),
            trafikkmengde.indeksaar =
              sum(trafikkmengde.indeksaar),
            indeks.ny = 100 *
              (trafikkmengde.indeksaar/
                 sum.trafikkmengde.basisaar - 1),
            timer = sum(timer),
            kalendertimer = sum(kalendertimer),
            dekningsgrad = (timer / kalendertimer) * 100,
            standardavvik = sqrt(wtd.var(x = indeks,
              weights = trafikkmengde.basisaar/sum.trafikkmengde.basisaar,
                                         normwt = TRUE)))

colnames(byindekser.vegkategori)[5] <-
  "trafikkmengde.basisaar"
colnames(byindekser.vegkategori)[7] <-
  "indeks"

byindekser.ER <- byindekser.vegkategori %>%
  filter(vegkat == "E" | vegkat == "R") %>%
  group_by(maaned, lengdeklasse, dogn) %>%
  summarise(sum.trafikkmengde.basisaar =
              sum(trafikkmengde.basisaar),
            trafikkmengde.indeksaar =
              sum(trafikkmengde.indeksaar),
            indeks.ny = 100 *
              (trafikkmengde.indeksaar/
                 sum.trafikkmengde.basisaar - 1),
            timer = sum(timer),
            kalendertimer = sum(kalendertimer),
            dekningsgrad = (timer / kalendertimer) * 100,
            standardavvik = sqrt(wtd.var(x = indeks,
              weights = trafikkmengde.basisaar/sum.trafikkmengde.basisaar,
                                         normwt = TRUE))) %>%
  mutate(vegkat = "E+R") %>%
  rename(trafikkmengde.basisaar = sum.trafikkmengde.basisaar,
         indeks = indeks.ny) %>%
  select(maaned:dogn, vegkat, trafikkmengde.basisaar:standardavvik)

byindekser.ERF <- byindekser.vegkategori %>%
  group_by(maaned, lengdeklasse, dogn) %>%
  summarise(sum.trafikkmengde.basisaar =
              sum(trafikkmengde.basisaar),
            trafikkmengde.indeksaar =
              sum(trafikkmengde.indeksaar),
            indeks.ny = 100 *
              (trafikkmengde.indeksaar/
                 sum.trafikkmengde.basisaar - 1),
            timer = sum(timer),
            kalendertimer = sum(kalendertimer),
            dekningsgrad = (timer / kalendertimer) * 100,
            standardavvik = sqrt(wtd.var(x = indeks,
              weights = trafikkmengde.basisaar/sum.trafikkmengde.basisaar,
                                         normwt = TRUE))) %>%
  mutate(vegkat = "E+R+F") %>%
  rename(trafikkmengde.basisaar = sum.trafikkmengde.basisaar,
         indeks = indeks.ny) %>%
  select(maaned:dogn, vegkat, trafikkmengde.basisaar:standardavvik)

byindekser <- rbind(byindekser.vegkategori, byindekser.ER, byindekser.ERF)

write.csv2(byindekser, file = "byindekser_minitest_snorre.csv")

#
# slutt.
#