# Sablefish fishery and survey biological data summaries 
# Data request from K. Fenske 2017-12-26
# Jane Sullivan (jane.sullivan1@alaska.gov)
# 2018-01-08

# Includes age and length compositions, proportions mature at age, length and
# weight-at-age for sablefish fishery and survey data in Southeast Alaska.

# Libraries and ggplot theme ----

library(tidyverse)
library(mosaic)

# Data ----

# length: fork length, cm 
# weight: kg 
# Matuirty: "0" = immature, "1" = mature
# Gear: indicates gear used (pot or longline)
# Source: fishery or survey
# Mgmt_area: NSEI is Chatham Strait, SSEI is Clarence Strait

read_csv("data/biological/survey/detailed_llsrvbio_nsei_ssei_1985_2017.csv",
         guess_max = 50000) -> srv_bio

read_csv("data/biological/survey/detailed_potsrvbio_nsei_ssei_1979_2017.csv",
         guess_max = 50000) -> potsrv_bio

read_csv("data/biological/fishery/detailed_fisherybio_nsei_ssei_1988_2017.csv", 
         guess_max = 50000) -> fsh_bio

bind_rows(srv_bio, potsrv_bio, fsh_bio) -> bio

#Summary of data available:
bio %>% 
  group_by(Gear, Source, Mgmt_area) %>% 
  summarise(min_year = min(year),
          max_year = max(year),
          no_of_years = n_distinct(year))

# Age comps ----

# Analysis conducted at various levels of detail, new column "description"
# describes what factors are used to obtain age comps. 

# Get subset. 42+ = plus group
bio %>% select(year, Gear, Source, Mgmt_area, Sex, age) %>% 
  filter(Sex %in% c('Female', 'Male') & !is.na(age)) %>% 
  droplevels() %>% 
  mutate(age = ifelse(age >= 42, 42, age)) -> agedat   

agedat %>% 
  # Age comps by Source, Gear, year, area, and Sex 
  count(Gear, Source, Mgmt_area, Sex, year, age) %>%
  group_by(Gear, Source, Mgmt_area, Sex, year) %>% 
  mutate(proportion = round( n / sum(n), 4),
         description = "agecompby_gear_source_area_sex") %>% 
  bind_rows(
    # Age comps by Source, Gear, year, area (sexes combined)
    agedat %>% 
      count(Source, Gear, Mgmt_area, year, age) %>%
      group_by(Source, Gear, Mgmt_area, year) %>% 
      mutate(proportion = round( n / sum(n), 4),
             Sex = "Sex_combined",
             description = "agecompby_gear_source_area"),
    # Age comps by Source, Gear, year, and sex (areas combined)
    agedat %>% 
      count(Source, Gear, Sex, year, age) %>%
      group_by(Source, Gear, Sex, year) %>% 
      mutate(proportion = round( n / sum(n), 4),
             Mgmt_area = "NSEI_SSEI_combined",
             description = "agecompby_gear_source_sex"),
    # Age comps by Source, Gear, and year (areas and sexes combined)
    agedat %>% 
      count(Source, Gear, year, age) %>%
      group_by(Source, Gear, year) %>% 
      mutate(proportion = round( n / sum(n), 4),
             Mgmt_area = "NSEI_SSEI_combined",
             Sex = "Sex_combined",
             description = "agecompby_gear_source")) %>% 
  arrange(description, Source, Gear, Mgmt_area, Sex, year, age) %>% 
  # Fill in the blanks with 0's
  complete(description, Source, Gear, Mgmt_area, Sex, year, age,
           fill = list(n = 0, proportion = 0)) %>% 
  write_csv("data/biological/allsable_agecomps.csv")

# Length comps ----

# Pers. comm. K. Fenske 2018-01-05: NMFS uses length bins 41, 43, 45 ... 99.
# These bins represent the center of the bin, so a 43 bin represents fish
# 42-43.9 cm. They omit fish smaller than 40 or larger than 100 cm for the
# length comp analysis. I've maintained these conventions for easy comparison:
bio %>% 
  filter(!c(length < 40 | length > 100)) %>% 
  mutate(length2 = ifelse(length < 41, 41,
                           ifelse(length > 99, 99, length)),
         length_bin = cut(length2, breaks = seq(39.9, 99.9, 2),
                          labels = paste(seq(41, 99, 2)))) %>% 
  select(-length2) -> lendat

# Similar to the age comps, the length comp analyses are conducted at various
# levels of detail. See new "description" column for more details.
lendat %>% 
  # Length comps by Source, Gear, year, area, and Sex 
  count(Gear, Source, Mgmt_area, Sex, year, length_bin) %>%
  group_by(Gear, Source, Mgmt_area, Sex, year) %>% 
  mutate(proportion = round( n / sum(n), 4),
         description = "lengthcompby_gear_source_area_sex") %>% 
  bind_rows(
    # Length comps by Source, Gear, year, area (sexes combined)
    lendat %>% 
      count(Source, Gear, Mgmt_area, year, length_bin) %>%
      group_by(Source, Gear, Mgmt_area, year) %>% 
      mutate(proportion = round( n / sum(n), 4),
             Sex = "Sex_combined",
             description = "lengthcompby_gear_source_area"),
    # Length comps by Source, Gear, year, and sex (areas combined)
    lendat %>% 
      count(Source, Gear, Sex, year, length_bin) %>%
      group_by(Source, Gear, Sex, year) %>% 
      mutate(proportion = round( n / sum(n), 4),
             Mgmt_area = "NSEI_SSEI_combined",
             description = "lengthcompby_gear_source_sex"),
    # Length comps by Source, Gear, and year (areas and sexes combined)
    lendat %>% 
      count(Source, Gear, year, length_bin) %>%
      group_by(Source, Gear, year) %>% 
      mutate(proportion = round( n / sum(n), 4),
             Mgmt_area = "NSEI_SSEI_combined",
             Sex = "Sex_combined",
             description = "lengthcompby_gear_source")) %>% 
  arrange(description, Source, Gear, Mgmt_area, Sex, year, length_bin) %>% 
  # Fill in the blanks with 0's
  complete(description, Source, Gear, Mgmt_area, Sex, year, length_bin,
           fill = list(n = 0, proportion = 0)) %>% 
  write_csv("data/biological/allsable_lengthcomps.csv")


# Maturity proportions ----

# Pers. comm. K. Fenske 2018-01-05: Just provide proportion mature by age (not
# length). I treat these similarly to the age and length comps, except I don't
# combine sexes.

bio %>% 
  filter(!is.na(Maturity)) -> matdat

matdat %>% 
  # Proportion mature at age by Source, Gear, year, area, Sex
  count(Gear, Source, Mgmt_area, Sex, year, Maturity, age) %>%
  group_by(Gear, Source, Mgmt_area, Sex, year) %>% 
  mutate(proportion = round( n / sum(n), 4),
         description = "propmatureby_gear_source_area_sex_age") %>% 
  bind_rows(
    # Proportion mature at age by Source, Gear, year, Sex (areas combined)
    matdat %>% 
      count(Gear, Source, Sex, year, Maturity, age) %>%
      group_by(Gear, Source, Sex, year) %>% 
      mutate(proportion = round( n / sum(n), 4),
             Mgmt_area = "NSEI_SSEI_combined",
             description = "propmatureby_gear_source_sex_age")) %>% 
  # Filter out immature rows for clarity 
  filter(Maturity == 1) %>% select(-Maturity) %>% 
  arrange(description, Source, Gear, Mgmt_area, Sex, year, age) %>%
  # Fill in the blanks with 0's
  # ungroup() %>%
  complete(description, Source, Gear, Mgmt_area, Sex, year, age,
           fill = list(n = 0, proportion = 0))  %>% 
  write_csv("data/biological/allsable_proportionmature.csv")

# Length-at-age ----

bio %>% filter(!is.na(age)) -> laa

laa %>% 
  group_by(Gear, Source, Mgmt_area, Sex, year, age) %>% 
  summarise(mean_length = round(mean(length),1),
            sd_length = round(sd(length),1),
            n = length(length),
            description = "lengthageby_gear_source_area_year_sex") %>%
  bind_rows(
    # Length-at-age by Source, Gear, year, Sex (areas combined)
    laa %>% 
      group_by(Gear, Source, Sex, year, age) %>% 
      summarise(mean_length = round(mean(length),1),
                sd_length = round(sd(length),1),
                n = length(length),
                Mgmt_area = "NSEI_SSEI_combined",
                description = "lengthageby_gear_source_year_sex") 
  ) %>% arrange(description, Gear, Source, Mgmt_area, Sex, year, age) %>% 
  select(description, Gear, Source, Mgmt_area, Sex, year, age, mean_length, sd_length, n) %>% 
  write_csv("data/biological/allsable_lengthage.csv")

# Weight-at-age ----

bio %>% filter(!c(is.na(weight) | is.na(age))) -> waa

waa %>% 
  group_by(Gear, Source, Mgmt_area, Sex, year, age) %>% 
  summarise(mean_weight = round(mean(weight),1),
            sd_weight = round(sd(weight),1),
            n = length(weight),
            description = "weightageby_gear_source_area_year_sex") %>%
  bind_rows(
    # Weight-at-age by Source, Gear, year, Sex (areas combined)
    waa %>% 
      group_by(Gear, Source, Sex, year, age) %>% 
      summarise(mean_weight = round(mean(weight),1),
                sd_weight = round(sd(weight),1),
                n = length(weight),
                Mgmt_area = "NSEI_SSEI_combined",
                description = "weightageby_gear_source_year_sex") 
  ) %>% arrange(description, Gear, Source, Mgmt_area, Sex, year, age) %>% 
  select(description, Gear, Source, Mgmt_area, Sex, year, age, mean_weight, sd_weight, n) %>% 
  write_csv("data/biological/allsable_weightage.csv")
