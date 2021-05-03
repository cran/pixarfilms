## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, warning=FALSE-----------------------------------------------------
library(pixarfilms)
library(dplyr)
library(tidyr)
library(forcats)
library(ggplot2)

## ----fig.height=5, fig.width=6------------------------------------------------
public_response %>%
  select(film, rotten_tomatoes, metacritic) %>%
  mutate(film = fct_inorder(film)) %>%
  pivot_longer(cols = c("rotten_tomatoes", "metacritic"),
               names_to = "ratings",
               values_to = "value") %>%
  mutate(ratings = case_when(
    ratings == "metacritic" ~ "Metacritic",
    ratings == "rotten_tomatoes" ~ "Rotten Tomatoes"
  )) %>%
  ggplot(aes(x = film, y = value, col = ratings)) +
  geom_point() +
  geom_line(aes(group = ratings)) +
  scale_color_brewer(palette = "Dark2") +
  labs(x = "Pixar film", y = "Rating value") +
  guides(col = guide_legend(title = "Ratings")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90),
        legend.position = "bottom") 

## ----fig.height=6, fig.width=4------------------------------------------------
public_response %>%
  select(film, rotten_tomatoes, metacritic) %>%
  pivot_longer(cols = c("rotten_tomatoes", "metacritic"),
               names_to = "ratings",
               values_to = "value") %>%
  mutate(ratings = case_when(
    ratings == "metacritic" ~ "Metacritic",
    ratings == "rotten_tomatoes" ~ "Rotten Tomatoes"
  )) %>%
  ggplot(aes(x = ratings, y = value, col = ratings)) +
  geom_boxplot() +
  ggbeeswarm::geom_beeswarm() +
  ggrepel::geom_text_repel(data = . %>%
                             filter(film == "Cars 2" ) %>%
                             filter(ratings == "Rotten Tomatoes"),
                           aes(label = film),
                           point.padding = 0.4) +
  scale_color_brewer(palette = "Dark2") +
  guides(col = guide_legend(title = "Ratings")) +
  labs(x = "Rating group", y = "Rating value") +
  ylim(c(30, 100)) +
  theme_minimal() +
  theme(legend.position = "bottom") 

## -----------------------------------------------------------------------------
sessionInfo()

