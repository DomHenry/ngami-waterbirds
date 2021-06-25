## ________________________________________________________________________

## Code to recreate boral analysis and associated plots of Ngami waterbird community data
## Authors: Dominic A.W. Henry & Graeme S. Cumming
## Date: 25/06/2021
## Citation: XXX

## Required libraries
# install.packages("tidyverse")
# install.packages("boral")
# install.packages("mvabund")
# install.packages("gghighlight")
# install.packages("corrplot")
# install.packages("glue")

library(tidyverse)
library(boral)
library(mvabund)
library(gghighlight)
library(corrplot)

## ________________________________________________________________________

# Import data -------------------------------------------------------------

## Waterbird community data (mean counts at each site during each sampling mission)
nga_data <- read_delim("data/NgamiCountData2.txt", delim = "\t")

## Create bird count data frame
nga_birds <- nga_data %>%
  select(-(Mission:Ycoord))

## Isolate count metadata and add lake_area (hectares) to each mission
nga_meta <- nga_data %>%
  select(Mission, Year, Month) %>%
  mutate(lake_area = case_when(
    Mission == "NGA1" ~ 50,
    Mission == "NGA2" ~ 30,
    Mission == "NGA3" ~ 10,
    Mission == "NGA4" ~ 39,
    Mission == "NGA5" ~ 30,
    Mission == "NGA6" ~ 20,
    TRUE ~ 0
  ))

# Foraging guilds ---------------------------------------------------------

## Read in foraging guild data
nga_fg <- read_delim("data/NgamiGuilds.txt", delim = "\t")

# 1	Short vegetation/mud/grass
# 2	Emergent vegetation
# 3	Shallow water
# 4	In or over deep water

## Subset count data to only include waterbirds in the first four forgaging guilds
spp_keep <- nga_fg %>%
  filter(guild %in% c(1, 2, 3, 4)) %>%
  select(Bird) %>%
  pull()

nga_birds <- nga_birds %>%
  select(all_of(spp_keep)) %>%
  mutate_all(list(~ round(., 0)))

# boral analysis ----------------------------------------------------------

## MCMC settings - testing
# mod_mcmc_control <- list(n.burnin = 100, n.iteration = 200, n.thin = 1)

## MCMC settings - actual model run
mod_mcmc_control <- list(n.burnin = 10000, n.iteration = 50000, n.thin = 30)

## Specify negative binomal boral model
birdfit_nbX <- boral(
  y = nga_birds, X = scale(nga_meta$lake_area),
  family = "negative.binomial", lv.control = list(num.lv = 2),
  row.eff = "fixed", mcmc.control = mod_mcmc_control, model.name = "jagsboral.txt",
  save.model = TRUE
)

## Reformat output from boral fit
tb <- tidyboral(birdfit_nbX)

# Diagnostics -------------------------------------------------------------
plot(birdfit_nbX)

# Biplot ------------------------------------------------------------------
lvsplot(birdfit_nbX, ind.spp = 10, biplot = TRUE)

# Ordinations -------------------------------------------------------------

miss_labs <- c(
  "NGA1" = "Oct 2007",
  "NGA2" = "Feb 2008",
  "NGA3" = "June 2008",
  "NGA4" = "Oct 2008",
  "NGA5" = "Feb 2009",
  "NGA6" = "June 2009"
)

guild_labs <- c(
  "1" = "Foraging guild 1",
  "2" = "Foraging guild 2",
  "3" = "Foraging guild 3",
  "4" = "Foraging guild 4"
)

nga_fg <- nga_fg %>%
  rename(species = Bird)

## Extract latent variable score data
plotdata <- lvsplot(birdfit_nbX, main = "Residual biplot", alpha = 0.55, biplot = FALSE, return.vals = TRUE)

## Figure 3
p1 <- tibble(lv1 = plotdata$scaled.lvs[, 1],
  lv2 = plotdata$scaled.lvs[, 2]) %>%
  mutate(mission = nga_meta$Mission) %>%
  ggplot(aes(x = lv1, y = lv2)) +
  geom_point(aes(color = mission), size = 2.8) +
  geom_hline(yintercept = 0, size = 0.5) +
  geom_vline(xintercept = 0, size = 0.5) +
  facet_wrap(~mission, labeller = labeller(mission = miss_labs)) +
  gghighlight(unhighlighted_params = list(size = 2)) +
  theme_bw() +
  theme(
    axis.text = element_text(size = 12, colour = "black"),
    axis.title = element_text(size = 12),
    strip.text = element_text(size = 14)
  )

p1

## Figure 4
p2 <- tibble(lv1 = plotdata$scaled.lv.coefs[, 1],
  lv2 = plotdata$scaled.lv.coefs[, 2]) %>%
  mutate(species = spp_keep) %>%
  left_join(nga_fg, by = "species") %>%
  mutate(guild = as_factor(guild)) %>%
  ggplot(aes(x = lv1, y = lv2)) +
  geom_point(aes(color = guild), size = 2.8) +
  geom_hline(yintercept = 0, size = 0.5) +
  geom_vline(xintercept = 0, size = 0.5) +
  facet_wrap(~guild, labeller = labeller(guild = guild_labs)) +
  gghighlight(unhighlighted_params = list(size = 2), use_direct_label = FALSE) +
  theme_bw() +
  theme(
    axis.text = element_text(size = 12, colour = "black"),
    axis.title = element_text(size = 12),
    strip.text = element_text(size = 14),
    legend.position = "none"
  )

p2

## END _________________________________________________________________
