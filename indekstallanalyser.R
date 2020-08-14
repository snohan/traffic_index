#
# Sjekker om punktindeksene er normalfordelte, og om de bør stratifiseres på
# vegkategori og fylke.
#

# Pakker ####


# Leser inn ####
indeksfil <- choose.files()
pindeks <- read.csv2(indeksfil, stringsAsFactors = F, dec = ",")

pindeks$Januar <- as.numeric(sub(",", ".", pindeks$Januar))
pindeks$Februar <- as.numeric(sub(",", ".", pindeks$Februar))
pindeks$Mars <- as.numeric(sub(",", ".", pindeks$Mars))
pindeks$April <- as.numeric(sub(",", ".", pindeks$April))
pindeks$Mai <- as.numeric(sub(",", ".", pindeks$Mai))
pindeks$Juni <- as.numeric(sub(",", ".", pindeks$Juni))
pindeks$Juli <- as.numeric(sub(",", ".", pindeks$Juli))
pindeks$August <- as.numeric(sub(",", ".", pindeks$August))
pindeks$September <- as.numeric(sub(",", ".", pindeks$September))
# pindeks$Oktober <- as.numeric(sub(",", ".", pindeks$Oktober))
# pindeks$November <- as.numeric(sub(",", ".", pindeks$November))
# pindeks$Desember <- as.numeric(sub(",", ".", pindeks$Desember))
# pindeks$Hittil_i_år <- as.numeric(sub(",", ".", pindeks$Hittil_i_år))
# pindeks$Siste_12_mnd <- as.numeric(sub(",", ".", pindeks$Siste_12_mnd))

pindeks.alt <- pindeks

# Januar ####
pindeks %<>% filter(Januar != "-", lengdeklasse == "Alle")
pindeks %<>% filter(Januar >= -50, Januar <= 50)

# Spredning ####
summary(pindeks$Januar)
sd(pindeks$Januar)
hist(pindeks$Januar, breaks = 100, probability = T)
lines(density(pindeks$Januar), col = "blue")
tail(sort(pindeks$Januar))
head(sort(pindeks$Januar))

normal <- dnorm(Januar, mean = mean(Januar), sd = sd(Januar))
plot(Januar, normal, type = "l")
lines(Januar, normal)

shapiro.test(pindeks$Januar)
qqnorm(pindeks$Januar)

# Ser på variansen:
pindeks.var <- pindeks
pindeks.var$vegkategori <- gsub("E", "R", pindeks.var$vegkategori)
pindeks.var %<>% group_by(fylkenr, vegkategori) %>%
  summarise(var = var(Januar), sd = sd(Januar), n = n(),
            psd = sum((n - 1) * sqrt(sd)) / sum(n - 1))


# Deler inn i vegkategori ####
pR <- filter(pindeks, vegkategori != "F")
pF <- filter(pindeks, vegkategori == "F")
summary(pR$Januar)
summary(pF$Januar)
sd(pR$Januar)
sd(pF$Januar)

hist(pR$Januar, breaks = 100, probability = T)
lines(density(pR$Januar), col = "blue")
lines(density(pF$Januar), col = "red")

ggplot(pindeks, aes(vegkategori, Januar)) +
  geom_boxplot(fill = "grey80", colour = "blue") +
  scale_x_discrete()

# Deler inn i vegkategori og fylke ####
pR1 <- filter(pR, fylkenr == 1)
summary(pR1$Januar)
sd(pR1$Januar)
hist(pR1$Januar, breaks = 100, probability = T)
lines(density(pR1$Januar), col = "blue")
shapiro.test(pR1$Januar)
qqnorm(pR1$Januar)

# Punktindekser ####
pindeks <- pindeks.alt %>% select(fylkenr:meter, døgn, lengdeklasse,
                                  Januar, Februar, Mars, April, Mai,
                                  Juni, Juli, August, September) %>%
  filter(døgn == "Alle", lengdeklasse == "Alle") %>%
  arrange(msnr)

pindeks %<>% mutate(veg = paste(vegkategori, vegnr, sep = "")) %>%
  select(fylkenr:vegkategori, veg, vegnr:September)

pindeks$vegkategori <- gsub("E", "R", pindeks$vegkategori)

pindeksfylkesantall <- pindeks %>% group_by(fylkenr, vegkategori) %>%
  summarise(antall = n(),
            ant_jan = sum(!is.na(Januar)),
            ant_feb = sum(!is.na(Februar)),
            ant_mar = sum(!is.na(Mars)),
            ant_apr = sum(!is.na(April)),
            ant_mai = sum(!is.na(Mai)),
            ant_jun = sum(!is.na(Juni)),
            ant_jul = sum(!is.na(Juli)),
            ant_aug = sum(!is.na(August)),
            ant_sep = sum(!is.na(September)))

# Sammenligner med tidligere versjon ####
pindeks.sammenlign <- left_join(pindeks, pindeks.ny,
                                by = c("msnr" = "msnr",
                                       "lengdeklasse" = "lengdeklasse"),
                                suffix = c(".0", ".1"))

pindeks.sammenlign <- select(pindeks.sammenlign, msnr, msnavn.0, lengdeklasse,
                             Januar.0, Januar.1,
                             Februar.0, Februar.1,
                             Mars.0, Mars.1,
                             April.0, April.1,
                             Mai.0, Mai.1,
                             Juni.0, Juni.1,
                             Juli.0, Juli.1,
                             August.0, August.1)

# Lager boksplott ####
pindeks.lang <- gather(pindeks, Maaned, Indeks, Januar:September)

pindeks.lang %>%
  filter(lengdeklasse == "Alle") %>%
  filter(Maaned == "September") %>%
  ggplot() +
  geom_boxplot(aes(x = vegkategori, y = Indeks)) +
  facet_wrap(~fylkenr)

# Linjeplott for gitt fylke og vegkategori ####
# For å se utvikling per stasjon over tid.
stasjonsnumre <- unique(pindeks.lang$msnr)

maaneder <- c("Januar", "Februar", "Mars", "April", "Mai", "Juni", "Juli",
              "August", "September", "Oktober", "November", "Desember")

stasjonsplott_over_tid <- pindeks.lang %>%
  filter(vegkategori == "R" & fylkenr == 20) %>%
  mutate(Maaned = factor(Maaned, levels = maaneder),
         msnr = factor(msnr, levels = stasjonsnumre)) %>%
  ggplot(aes(x = Maaned, y = Indeks, group = msnr, color = msnr)) +
  geom_line() +
  geom_point()
#  facet_wrap(~fylkenr)
stasjonsplott_over_tid

# Ser på fordelinga ####
fylke_og_veg <- pindeks.lang %>%
  filter(vegkategori == "R" & fylkenr == 2 & Maaned == "September") %>%
  filter(!is.na(Indeks)) %>%
  mutate(Maaned = factor(Maaned, levels = maaneder),
         msnr = factor(msnr, levels = stasjonsnumre))

# Sammenligner med t-fordeling ####
tparametre <- fitdistr(fylke_og_veg$Indeks, "t")
tparametre

tkurve <- function(x, mu, nu, df) {
  dt((x-mu)/nu,df)/nu
}

ggplot(fylke_og_veg, aes(Indeks)) +
  geom_histogram(aes(y = ..density..), color = "black", fill = "white") +
  geom_density(alpha = 0.2, fill = "red") +
  stat_function(fun = tkurve,
                args = list(mu = tparametre$estimate[1],
                            nu = tparametre$estimate[2],
                            df = tparametre$estimate[3]),
                color = "green") +
  stat_function(fun = dnorm,
                args = list(mean = mean(fylke_og_veg$Indeks, na.rm = TRUE),
                            sd = sd(fylke_og_veg$Indeks, na.rm = TRUE)),
                color = "blue")


# Finner standaradavvik ####
pindeks.sd <- pindeks.lang %>%
  filter(!is.na(Indeks)) %>%
  #group_by(fylkenr, vegkategori, Maaned) %>%
  summarise(antall = n(),
            sigma = round(sd(Indeks, na.rm = T), digits = 2),
            persentil25 = quantile(Indeks, probs = c(0.25)),
            persentil50 = quantile(Indeks, probs = c(0.75)))

pindeks.sd %>%
  filter(Maaned != "Mars", Maaned != "April") %>%
  ungroup() %>%
  summarise(meansd = mean(sigma))

quantile(pindeks.sd$sigma, probs = c(0.1, 0.9))

# Ser på tunge kjøretøy ####
pindeks.tunge <- pindeks.alt %>% select(fylkenr:meter, døgn, lengdeklasse,
                                  Januar, Februar, Mars, April, Mai,
                                  Juni, Juli, August, September) %>%
  filter(døgn == "Alle", lengdeklasse == "Større eller lik 5,6m") %>%
  arrange(msnr)

pindeks.tunge %<>% mutate(veg = paste(vegkategori, vegnr, sep = "")) %>%
  select(fylkenr:vegkategori, veg, vegnr:August)

pindeks.tunge$vegkategori <- gsub("E", "R", pindeks.tunge$vegkategori)

pindeks.tunge.lang <- gather(pindeks.tunge, Maaned, Indeks, Januar:September)

pindeks.tunge.lang %>%
  filter(vegkategori == "R" & fylkenr == 20) %>%
  mutate(Maaned = factor(Maaned, levels = maaneder),
         msnr = factor(msnr, levels = stasjonsnumre)) %>%
  ggplot(aes(x = Maaned, y = Indeks, group = msnr, color = msnr)) +
  geom_line() +
  geom_point()


#
pindeks.alle.tunge <-
  filter(pindeks.lang,
         lengdeklasse == "Alle" | lengdeklasse == "Større eller lik 5,6m") %>%
  spread(lengdeklasse, Indeks) %>%
  mutate(Maaned = factor(Maaned, levels = maaneder)) %>%
  arrange(msnr, Maaned)

pindeks.aug.ekstreme <- filter(pindeks.aug.lang, Indeks > 30 | Indeks < -30)

pindeks.aug.ekstreme.msnr <- unique(pindeks.aug.ekstreme$msnr)

write.csv2(pindeks.aug.ekstreme.msnr,
           file = "msnr_med_ekstreme_punktindekser.csv")

write.csv2(pindeks.sammenlign,
           file = "punktindeks_sammelign_etter_filtrerte_lengder.csv")

# Vegtrafikkindeks ####
vtifil <- choose.files()
vti <- read.csv2(vtifil, stringsAsFactors = F, dec = ",")

vti$Januar <- as.numeric(sub(",", ".", vti$Januar))
vti$Februar <- as.numeric(sub(",", ".", vti$Februar))
vti$Mars <- as.numeric(sub(",", ".", vti$Mars))
vti$April <- as.numeric(sub(",", ".", vti$April))
vti$Mai <- as.numeric(sub(",", ".", vti$Mai))
vti$Juni <- as.numeric(sub(",", ".", vti$Juni))
vti$Juli <- as.numeric(sub(",", ".", vti$Juli))
vti$August <- as.numeric(sub(",", ".", vti$August))
vti$September <- as.numeric(sub(",", ".", vti$September))
vti$Oktober <- as.numeric(sub(",", ".", vti$Oktober))
vti$November <- as.numeric(sub(",", ".", vti$November))
vti$Desember <- as.numeric(sub(",", ".", vti$Desember))
vti$Hittil.i.år <- as.numeric(sub(",", ".", vti$Hittil.i.år))
vti$Siste_12_mnd <- as.numeric(sub(",", ".", vti$Siste_12_mnd))

vti.norge <- vti %>% select(-Siste_12_mnd) %>%
  filter(Døgn == "Alle", Lengdeklasse == "Alle", Område == "Norge")

vti.fylker <- left_join(fylker, vti, by = c("navn" = "Område")) %>%
  rename(Fylkenr = nr, Fylkenavn = navn) %>%
  select(-Siste_12_mnd, -Hittil.i.år) %>%
  filter(Døgn == "Alle", Lengdeklasse == "Alle") %>%
  filter(!is.na(Januar))

vti.fylker.lang <- gather(vti.fylker, Maaned, Indeks, Januar:September)
fylkene <- fylker$navn

vti.fylker.lang %>%
  filter(Vegkategori == "E+R+F") %>%
  mutate(Maaned = factor(Maaned, levels = maaneder),
         Fylkenavn = factor(Fylkenavn, levels = fylkene)) %>%
  ggplot(aes(x = Maaned, y = Indeks, group = Fylkenavn, color = Fylkenavn)) +
  geom_line() +
  geom_point() +
  ggtitle("Fylkesindekser 2017, E+R+F")

#
# Slutt.
#