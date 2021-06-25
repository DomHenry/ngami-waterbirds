## ________________________________________________________________________

## Code to recreate mvabund analysis and associated plots of Ngami waterbird community data
## Authors: Dominic A.W. Henry & Graeme S. Cumming
## Date: 25/06/2021
## Citation: XXX

## Required libraries
# install.packages("tidyverse")
# install.packages("boral")
# install.packages("mvabund")
# install.packages("gghighlight")
# install.packages("corrplot")
# install.packages("gridExtra")

library(tidyverse)
library(boral)
library(mvabund)
library(gghighlight)
library(corrplot)
library(gridExtra)
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

# Mean variance relationship ----------------------------------------------

## Create mvabund object
birds <- mvabund(nga_birds)

## Construct mean-mariance plots for multivariate abundance data
meanvar_data <- meanvar.plot(birds, table = TRUE)

meanvar_data <- rownames_to_column(as.data.frame(meanvar_data)) %>%
  as_tibble() %>%
  rename(species = rowname)

meanvar_data %>%
  ggplot(aes(x = log(Mean), y = log(Variance))) +
  geom_point() +
  theme_bw() +
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 12)
  ) +
  labs(title = guildlabel)


# manyGLM modelling -------------------------------------------------------

## Fit model (multivariate abunadance as a function of Ngami lake level)
mod <- manyglm(birds ~ nga_meta$lake_area, family = "negative_binomial")

## Check output
plot(mod)

## Check model summary
mod_sum <- summary(mod)

## Anova
mod_aov <- anova.manyglm(mod, p.uni = "adjusted")
mod_aov$table
mod_aov$uni.test
mod_aov$uni.p

# manyGLM plotting --------------------------------------------------------

nga_fg <- nga_fg %>%
  rename(species = Bird)

species <- names(mod$coefficients[2, ])

## Create dataframe to hold plot data from manyglm object
plotdata <- tibble(
  coval = mod$coefficients[2, ],
  se = mod$stderr.coefficients[2, ], # standard error of coefficients
  upper = coval + se,
  lower = coval - se
) %>%
  mutate(species = species) %>%
  left_join(nga_fg, by = "species") %>%
  mutate(species = as_factor(species)) %>%
  arrange(guild, species)

## Set colors
gg_color <- function(n) {
  hues <- seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

plotcols <- gg_color(4)

## Neaten species names
plotdata <- plotdata %>%
  mutate(species = str_replace_all(species, "\\.", " "))

## Figure 2

p1 <- plotdata %>%
  mutate(sig = ifelse(upper < 0 & lower < 0 | upper > 0 & lower > 0, "sig", "ns")) %>%
  filter(sig == "sig" & guild %in% c(1, 4)) %>%
  ggplot(aes(x = fct_reorder(species, guild), y = coval, ymin = lower, ymax = upper)) +
  geom_pointrange(aes(color = as_factor(guild)), size = 0.9) +
  scale_color_manual(values = plotcols[c(1, 4)]) +
  labs(color = "Foraging guild") +
  coord_flip(ylim = c(-0.5, 0.5)) +
  geom_hline(yintercept = 0) +
  theme_bw() +
  xlab("") +
  ylab("Coefficient") +
  theme(
    axis.text = element_text(size = 16, colour = "black"),
    axis.title = element_text(size = 15),
    legend.position = "top",
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 16)
  )

p1

p2 <- plotdata %>%
  mutate(sig = ifelse(upper < 0 & lower < 0 | upper > 0 & lower > 0, "sig", "ns")) %>%
  filter(sig == "sig" & guild %in% c(2, 3)) %>%
  ggplot(aes(x = fct_reorder(species, guild), y = coval, ymin = lower, ymax = upper)) +
  geom_pointrange(aes(color = as_factor(guild)), size = 0.9) +
  scale_color_manual(values = plotcols[c(2, 3)]) +
  labs(color = "Foraging guild") +
  coord_flip(ylim = c(-0.5, 0.5)) +
  geom_hline(yintercept = 0) +
  theme_bw() +
  xlab("") +
  ylab("Coefficient") +
  theme(
    axis.text = element_text(size = 16, colour = "black"),
    axis.title = element_text(size = 15),
    legend.position = "top",
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 16)
  )

p2

p2panel <- grid.arrange(p1, p2, nrow = 1, ncol = 2)
