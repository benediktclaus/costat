# Pakete ------------------------------------------------------------------
library(tidyverse)
library(haven)


# t-Tests -----------------------------------------------------------------
# Einstichproben-t-Test
set.seed(20200422)
tibble("intelligence" = round(rnorm(48, 125, 15))) %>% 
  rownames_to_column(var = "id") %>% 
  write_sav(path = "data/ravenclav.sav")

# t-Test für unabhängige Stichproben
set.seed(20200422)
tibble(
  house = as_factor(c("Gryffindor", "Slytherin")),
  chivalry = list(round(rnorm(n = 42, mean = 95, sd = 8)), round(rnorm(n = 38, mean = 45, sd = 9)))
) %>% 
  unnest(chivalry) %>% 
  rownames_to_column(var = "id") %>% 
  write_sav(path = "data/chivalry.sav")

# t-Test für verbundene Stichproben
set.seed(20200422)
tibble(
  measurement = as_factor(c("Begin", "End")),
  patience = list(round(rnorm(53, 44, 8)), round(rnorm(53, 46, 9)))
) %>% 
  unnest(patience) %>% 
  add_column(id = as.character(rep(1:53, 2)), .before = "measurement") %>% 
  write_sav(path = "data/patience.sav")


# ANOVAs ------------------------------------------------------------------
# Einfaktorielle ANOVA
set.seed(20200428)
tibble(
  backpack = as_factor(c("None", "Light", "Heavy", "Handcart")),
  joy = list(round(rnorm(42, 9, 1.3)), round(rnorm(53, 7, 1.5)), round(rnorm(56, 6.5, 1.5)), round(rnorm(35, 3, 1.7)))
) %>% 
  unnest(joy) %>% 
  mutate(joy = pmin(joy, 10),
         joy = pmax(joy, 0)) %>% 
  sample_n(size = nrow(.)) %>% 
  rownames_to_column(var = "id") %>% 
  write_sav(path = "data/phantasialand.sav")

# Faktorielle ANOVA
set.seed(20200504)
crossing(flavor = as_factor(c("Vanilla", "Chocolate", "Cookie", "Champagne")),
         sex = as_factor(c("Female", "Male"))) %>% 
  add_column(yumminess = list(
    round(rnorm(36, 17, 2)),
    round(rnorm(38, 16, 2.1)),
    round(rnorm(41, 17.5, 1.9)),
    round(rnorm(40, 17.3, 1.7)),
    round(rnorm(45, 16, 2.3)),
    round(rnorm(35, 13, 2.4)),
    round(rnorm(40, 15.5, 2.2)),
    round(rnorm(20, 8, 2.4))
    )) %>% 
  unnest(yumminess) %>% 
  mutate(yumminess = pmin(yumminess, 20)) %>% 
  sample_n(size = nrow(.)) %>% 
  rownames_to_column(var = "id") %>% 
  write_sav("data/ice_cream.sav")

# ANOVA mit Messwiederholungen
set.seed(20200518)
tibble(
  "bad" = rnorm(51, mean = 10, sd = 3),
  "neutral" = rnorm(51, mean = 16, sd = 3),
  "favorite" = rnorm(51, mean = 25, sd = 3)
) %>% 
  rownames_to_column(var = "id") %>% 
  pivot_longer(
    cols = -id,
    names_to = "music_type",
    values_to = "endurance"
  ) %>% 
  mutate(endurance = round(endurance),
         endurance = pmax(endurance, 0),
         endurance = pmin(endurance, 30)) %>% 
  write_sav(path = "data/runners.sav")
