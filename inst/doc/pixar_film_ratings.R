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
library(irr)

## -----------------------------------------------------------------------------
df <-
  public_response %>%
  select(-cinema_score) %>%
  mutate(film = fct_inorder(film)) %>%
  pivot_longer(cols = c("rotten_tomatoes", "metacritic", "critics_choice"),
               names_to = "ratings",
               values_to = "value") %>%
  mutate(ratings = case_when(
    ratings == "metacritic" ~ "Metacritic",
    ratings == "rotten_tomatoes" ~ "Rotten Tomatoes",
    ratings == "critics_choice" ~ "Critics Choice"
  )) %>%
  drop_na()

## ----fig.height=5, fig.width=6------------------------------------------------
df %>%
  ggplot(aes(x = film, y = value, col = ratings)) +
  geom_point() +
  geom_line(aes(group = ratings)) +
  scale_color_brewer(palette = "Dark2") +
  labs(x = "Pixar film", y = "Rating value") +
  guides(col = guide_legend(title = "Ratings")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        legend.position = "bottom") 

## ----fig.height=6, fig.width=5------------------------------------------------
df %>%
  ggplot(aes(x = ratings, y = value, col = ratings)) +
  geom_boxplot(width = 1.75 / length(unique(df$ratings))) +
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
public_response %>%
  select(-c(cinema_score, film)) %>%
  drop_na() %>%
  icc(model = "twoway", type = "consistency")

## -----------------------------------------------------------------------------
sessionInfo()

