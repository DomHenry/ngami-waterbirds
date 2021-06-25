## ________________________________________________________________________

## Code to recreate biodiversity indices, SACs and associated plots of Ngami waterbird community data
## Authors: Dominic A.W. Henry & Graeme S. Cumming
## Date: 25/06/2021
## Citation: XXX

## Required libraries
# install.packages("tidyverse")
# install.packages("vegan")
# install.packages("ggrepel")
# install.packages("sars")

library(tidyverse)
library(vegan)
library(ggrepel)
library(sars)
## ________________________________________________________________________

# Import data -------------------------------------------------------------

## Waterbird community data (mean counts at each site during each sampling mission)
nga_data <- read_delim("data/NgamiCountData2.txt", delim = "\t")

## Create count metadata dataframe
nga_meta <- nga_data %>%
  select(Mission, Year, Month) %>%
  group_by(Mission, Year, Month) %>%
  filter(row_number() == 1) %>%
  filter(Month != 1) %>%
  ungroup() %>%
  mutate(waterlev = factor(c("October", "February", "June", "October", "February", "June"),
    levels = c("October", "February", "June"), ordered = TRUE
  ))

## Create vector of missions
missions <- distinct(nga_data, Mission) %>% pull()

# Diversity indices -------------------------------------------------------

## Function to calculate mean diversity, richness, and evenness for each mission
calc_div_indices <- function(x) {
  mission_data <- nga_data %>%
    filter(Mission %in% x) %>%
    select(-(Mission:Ycoord))

  H <- diversity(mission_data)
  H_mean <- mean(H)
  H_sd <- sd(H)

  richness <- specnumber(mission_data)
  richness_mean <- mean(richness)
  richness_sd <- sd(richness)

  eveness <- H / log(richness)
  eveness_mean <- mean(eveness)
  eveness_sd <- sd(eveness)

  tibble(mission = x, H_mean, H_sd, richness_mean, richness_sd, eveness_mean, eveness_sd)
}

## Create plot dataframe
plot_data <- map_df(.x = missions, .f = calc_div_indices)

## Add labels and lake level
plot_data <- plot_data %>%
  mutate(
    lakelev = c(50, 30, 10, 39, 30, 20),
    monthnum = c(1, 5, 9, 13, 17, 21),
    misslab = c("Oct 2007", "Feb 2008", "Jun 2008", "Oct 2008", "Feb 2009", "Jun 2009")
  )

## Define colors and axis limits
point_colors <- c("#F8766D", "#00BA38", "#619CFF", "#F8766D", "#00BA38", "#619CFF")
xlims <- c(0, 20.9)

## Shannon diversity
plot_div <- plot_data %>%
  ggplot(aes(x = monthnum, y = H_mean)) +
  theme_bw() +
  geom_line(size = 1) +
  geom_pointrange(aes(ymin = H_mean - H_sd, ymax = H_mean + H_sd), cex = 0.8, color = point_colors) +
  labs(y = "Shannon Diversity (H)", x = "") +
  theme(axis.text = element_text(size = 16, color = "black"), axis.title = element_text(size = 16, color = "black")) +
  geom_text_repel(aes(label = misslab), hjust = -0.1, direction = "y", xlim = xlims, size = 5)

plot_div

## Species richness
plot_rich <- plot_data %>%
  ggplot(aes(x = monthnum, y = richness_mean)) +
  theme_bw() +
  geom_line(size = 1) +
  geom_pointrange(aes(ymin = richness_mean - richness_sd, ymax = richness_mean + richness_sd), cex = 0.8, color = point_colors) +
  labs(y = "Species Richness", x = "") +
  theme(axis.text = element_text(size = 16, color = "black"), axis.title = element_text(size = 16, color = "black")) +
  geom_text_repel(aes(label = misslab), hjust = -0.1, direction = "y", xlim = xlims, size = 5)

plot_rich

## Species eveness
plot_even <- plot_data %>%
  ggplot(aes(x = monthnum, y = eveness_mean)) +
  theme_bw() +
  geom_line(size = 1) +
  geom_pointrange(aes(ymin = eveness_mean - eveness_sd, ymax = eveness_mean + eveness_sd), cex = 0.8, color = point_colors) +
  labs(y = "Species Evenness", x = "Months since start") +
  theme(axis.text = element_text(size = 16, color = "black"), axis.title = element_text(size = 16, color = "black")) +
  geom_text_repel(aes(label = misslab), hjust = -0.1, direction = "y", xlim = xlims, size = 5)

plot_even

# Appendix 3 plot
ggpubr::ggarrange(plot_div, plot_rich, plot_even, ncol = 1, nrow = 3)


# Species accumulation curves ---------------------------------------------

## Function to calculate SACs for each mission
calc_sac <- function(x) {
  mission_data <- nga_data %>%
    filter(Mission %in% x) %>%
    select(-(Mission:Ycoord))

  sac <- specaccum(mission_data)

  p <- predict(sac)
  nr <- length(p)
  s <- cbind(1:nr, p)
  fit <- sar_loga(data = s)
  t <- summary(fit)
  cvals <- t$Parameters[1, ]
  zvals <- t$Parameters[2, ]

  sac_data <- tibble(
    sr = cvals["Estimate"] + zvals["Estimate"] * log(seq(1, 13, by = 0.5)),
    upper = cvals["97.5%"] + zvals["97.5%"] * log(seq(1, 13, by = 0.5)),
    lower = cvals["2.5%"] + zvals["2.5%"] * log(seq(1, 13, by = 0.5)),
    area = seq(1, 13, by = 0.5),
    Mission = x
  )
}

## Create plotting dataframe
sac_plot_data <- map_df(.x = missions, .f = calc_sac)

sac_plot_data <- sac_plot_data %>%
  left_join(nga_meta, by = "Mission")

## Appendix 4 plot
plot_sac <- sac_plot_data %>%
  ggplot(aes(x = area, y = sr, col = as_factor(Year))) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "grey", alpha = 0.6) +
  geom_line(size = 1.1) +
  theme_bw() +
  labs(color = "Year") +
  coord_cartesian(xlim = c(0, 13)) +
  scale_x_continuous(breaks = c(0, 2, 4, 6, 8, 10, 12)) +
  scale_y_continuous(breaks = c(20, 40, 60, 80, 100, 120)) +
  labs(x = "Point count", y = "Species richness") +
  theme(
    axis.title = element_text(size = 15),
    axis.text = element_text(size = 15, colour = "black"),
    strip.text = element_text(size = 15),
    legend.title = element_text(size = 15),
    legend.text = element_text(size = 15)
  ) +
  facet_wrap(vars(waterlev))

plot_sac

# SAC slope plots ---------------------------------------------------------

## Function to calculate and extract slopes of SACs
calc_sac_slope <- function(x) {

  mission_data <- nga_data %>%
    filter(Mission %in% x) %>%
    select(-(Mission:Ycoord))

  sac <- specaccum(mission_data)

  p <- predict(sac)
  nr <- length(p)
  s <- cbind(1:nr, p)
  fit <- sar_loga(data = s)
  t <- summary(fit)

  tibble(
    zest = t$Parameters[2, 1],
    zsd = t$Parameters[2, 2],
    Mission = x
  )

}

## Run function for each mission to extract plotting data
slope_plot_data <- map_df(.x = missions, .f = calc_sac_slope)

slope_plot_data <- slope_plot_data %>%
  left_join(nga_meta, by = "Mission") %>%
  mutate(
    lakelev = c(50, 30, 10, 39, 30, 20),
    misslab = c("Oct 2007", "Feb 2008", "Jun 2008", "Oct 2008", "Feb 2009", "Jun 2009"),
    monthnum = c(1, 5, 9, 13, 17, 21)
  )

point_colors <- c("#F8766D", "#00BA38", "#619CFF", "#F8766D", "#00BA38", "#619CFF")

## Figure 1a
z_vs_lakelev <- ggplot(slope_plot_data, aes(x = lakelev, y = zest)) +
  theme_bw() +
  geom_pointrange(aes(ymin = zest - zsd, ymax = zest + zsd), cex = 0.8, color = point_colors) +
  labs(y = "Slope of SAC (SE)", x = "Lake area (km2)") +
  theme(
    axis.text = element_text(size = 16, colour = "black"),
    axis.title = element_text(size = 16)
  ) +
  geom_text_repel(aes(label = misslab), hjust = -0.1, xlim = c(10, 50), size = 5)
z_vs_lakelev

## Figure 1b
z_vs_month <- ggplot(slope_plot_data, aes(x = monthnum, y = zest)) +
  theme_bw() +
  geom_line(size = 1) +
  geom_pointrange(aes(ymin = zest - zsd, ymax = zest + zsd), cex = 0.8, color = point_colors) +
  labs(y = "Slope of SAC (SE)", x = "Months since start") +
  theme(axis.text = element_text(size = 16, color = "black"), axis.title = element_text(size = 16, color = "black")) +
  geom_text_repel(aes(label = misslab), hjust = -0.1, xlim = c(0, 25), size = 5)
z_vs_month
