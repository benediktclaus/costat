# Pakete ------------------------------------------------------------------
library(tidyverse)
library(haven)


# t-Tests -----------------------------------------------------------------
# Einstichproben-t-Test
set.seed(20200422)
ravenclaw <- tibble("intelligence" = round(rnorm(48, 125, 15))) %>% 
  rownames_to_column(var = "id") %>% 
  write_sav(path = "data/ravenclav.sav")

# t-Test für unabhängige Stichproben
set.seed(20200422)
chivalry <- tibble(
  house = as_factor(c("Gryffindor", "Slytherin")),
  chivalry = list(round(rnorm(n = 42, mean = 95, sd = 8)), round(rnorm(n = 38, mean = 45, sd = 9)))
) %>% 
  unnest(chivalry) %>% 
  rownames_to_column(var = "id") %>% 
  write_sav(path = "data/chivalry.sav")

# t-Test für verbundene Stichproben
set.seed(20200422)
patience <- tibble(
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
  bagpack = as_factor(c("None", "Light", "Heavy", "Handcart")),
  joy = list(round(rnorm(42, 9, 1.3)), round(rnorm(53, 7, 1.5)), round(rnorm(56, 6.5, 1.5)), round(rnorm(35, 3, 1.7)))
) %>% 
  unnest(joy) %>% 
  mutate(joy = pmin(joy, 10),
         joy = pmax(joy, 0)) %>% 
  sample_n(size = nrow(.)) %>% 
  rownames_to_column(var = "id") %>% 
  write_sav(path = "data/phantasialand.sav")
