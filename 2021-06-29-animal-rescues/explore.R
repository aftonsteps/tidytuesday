# Get the Data
library(magrittr)
library(ggplot2)
library(dplyr)
library(scales)
library(ggimage)
library(ggpattern)

## Credit for images (I didn't end up using them though)
## https://pixabay.com/photos/cat-pet-yawning-yawn-animal-339400/
## https://pixabay.com/photos/cat-kitten-tree-curious-tabby-1647775/

# Read in data manually
animal_rescues <- 
  readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-06-29/animal_rescues.csv') %>%
  dplyr::mutate(animal_group_parent = tolower(animal_group_parent))

## Filter to just cats
cats <-
  animal_rescues %>%
  dplyr::filter(animal_group_parent == "cat")

## Get cat costs per year
cat_cost_yearly <-
  cats %>%
  dplyr::filter(cal_year < 2021) %>%
  dplyr::mutate(incident_notional_cost = 
                  ifelse(test = incident_notional_cost == "NULL",
                         yes = 0,
                         no = incident_notional_cost)) %>%
  dplyr::mutate(incident_notional_cost = 
                  as.numeric(incident_notional_cost)) %>%
  dplyr::group_by(cal_year) %>%
  dplyr::summarize(incident_notional_cost = 
                     sum(incident_notional_cost))

## Get total cat cost over the last few years
cat_cost_total <-
  cat_cost_yearly %>% 
  dplyr::ungroup() %>%
  dplyr::summarize(incident_notional_cost = 
                     sum(incident_notional_cost))

## Plot setup
font_choice <- "Rockwell"
title_size <- 18
subtitle_size <- 10
w <- 10
h <- 4
filepath <- function(img) {
  return(paste0("2021-06-29-animal-rescues/charts/", img))
}

## First graph: total cat debt
g1 <- ggplot(data = cat_cost_total,
             mapping = aes(x = 1, 
                           y = incident_notional_cost)) +
  geom_bar(stat = "identity",
           fill = "orange") +
  geom_text(x = 1, 
            y = 1250000, 
            label = dollar_format(prefix = "£")(cat_cost_total$incident_notional_cost[1]),
            family = font_choice) + 
  scale_y_continuous(labels = dollar_format(prefix = "£"),
                     expand = expansion(mult = c(0.06, 0.1))) +
  labs(title = "These Cats Owe the City of London Over £1 Million.",
       subtitle = "Total cost of cat rescues by the London Fire Brigade between 2009 and 2020") +
  theme_bw(base_family = font_choice) +
  theme(title = element_text(size = title_size),
        plot.subtitle = element_text(size = subtitle_size),
        axis.text.x = element_blank(),
        axis.title = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid = element_blank())

g1
ggsave(filename = filepath("total_cat_debt.png"), 
       plot = g1, 
       width = w, 
       height = h)

## Second graph: cat debt over time
g2 <- ggplot(data = cat_cost_yearly,
             mapping = aes(x = cal_year, 
                           y = incident_notional_cost)) +
  geom_bar(stat = "identity",
           fill = "mediumorchid2") +
  geom_line(data = 
              cat_cost_yearly %>% dplyr::filter(cal_year %in% c(2009, 2020)),
            mapping = aes(x = cal_year, y = incident_notional_cost * 1.25),
            color = "mediumvioletred",
            size = 1.8,
            arrow = arrow(ends = "last")) +
  scale_y_continuous(labels = dollar_format(prefix = "£"),
                     expand = expansion(mult = c(0.06, 0.1))) +
  scale_x_continuous(breaks  = seq(2009, 2020),
                     labels = as.character(seq(2009, 2020))) +
  labs(title = "Annual Cat Debt Is Increasing.",
       subtitle = "Yearly cost of cat rescues by the London Fire Brigade") +
  theme_bw(base_family = font_choice) +
  theme(title = element_text(size = title_size),
        plot.subtitle = element_text(size = subtitle_size),
        axis.title = element_blank(),
        panel.grid = element_blank())
g2
ggsave(filename = filepath("cat_debt_by_year.png"), 
       plot = g2, 
       width = w, 
       height = h)


## Third graph: Cat image
g3 <- ggplot(data = cat_cost_total,
             mapping = aes(x = 1, 
                           y = incident_notional_cost)) +
  geom_bar_pattern(stat = "identity",
                   pattern = 'placeholder',
                   pattern_type = 'kitten',
                   fill = 'white',
                   colour = 'black',
                   pattern_spacing = 0.025) +
  scale_y_continuous(labels = dollar_format(prefix = "£"),
                     expand = expansion(mult = c(0.06, 0.1))) +
  labs(title = "They Are Never Going to Pay This Back.",
       subtitle = "They will absolutely never pay this back.") +
  theme_bw(base_family = font_choice) +
  theme(title = element_text(size = title_size),
        plot.subtitle = element_text(size = subtitle_size),
        axis.text.x = element_blank(),
        axis.title = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid = element_blank())
g3
ggsave(filename = filepath("cat_debt_total_cat.png"), 
       plot = g3, 
       width = w, 
       height = h)


## Fourth graph: yearly with cats
g4 <- ggplot(data = cat_cost_yearly,
             mapping = aes(x = cal_year, 
                           y = incident_notional_cost)) +
  geom_bar_pattern(stat = "identity",
                   pattern = 'placeholder',
                   pattern_type = 'kitten',
                   fill = 'white',
                   colour = 'black',
                   pattern_spacing = 0.025) +
  scale_y_continuous(labels = dollar_format(prefix = "£"),
                     expand = expansion(mult = c(0.06, 0.1))) +
  scale_x_continuous(breaks  = seq(2009, 2020),
                     labels = as.character(seq(2009, 2020))) +
  labs(title = "They Have No Respect for Financial Institutions.",
       subtitle = "They don't even know what money is.") +
  theme_bw(base_family = font_choice) +
  theme(title = element_text(size = title_size),
        plot.subtitle = element_text(size = subtitle_size),
        axis.title = element_blank(),
        panel.grid = element_blank())
g4
ggsave(filename = filepath("cat_debt_by_year_cat.png"), 
       plot = g4, 
       width = w, 
       height = h)
