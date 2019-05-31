
# Libraries, themes, and user-defined functions
# Jane Sullivan
# Last updated 2019-05-31

if(!require("mosaic"))   install.packages("mosaic") # derivedFactor, derivedVariable. Masks over a lot of fxns, but generally only improves their utility
if(!require("tidyverse"))   install.packages("tidyverse") # dplyr, ggplot, etc.
if(!require("lubridate"))   install.packages("lubridate") # dates functions like yday, dmy/mdy
if(!require("data.table"))   install.packages("data.table") # dcast, foverlaps
if(!require("ROracle"))   install.packages("ROracle") # database access through R

windowsFonts(Times=windowsFont("Times New Roman"))

theme_sleek <- function(base_size = 12, base_family = "Times") {
  half_line <- base_size/2
  theme_light(base_size = 12, base_family = "Times") +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.ticks.length = unit(half_line / 2.2, "pt"),
      strip.background = element_rect(fill = NA, colour = NA),
      strip.text.x = element_text(colour = "black"),
      strip.text.y = element_text(colour = "black"),
      panel.border = element_rect(fill = NA),#, colour = "grey70", size = 1),
      legend.key.size = unit(0.9, "lines"),
      legend.key = element_rect(colour = NA, fill = NA),
      legend.background = element_rect(colour = NA, fill = NA)#,
    )
}

theme_set(theme_sleek())

# Format ggplot figures with ticked axes (especially good for marking year and
# age) 

# Depends on dplyr
tickr <- function(
  data, # dataframe
  var, # column of interest
  to # break point definition 
){
  
  VAR <- enquo(var) # makes VAR a dynamic variable
  
  data %>% 
    distinct(!!VAR) %>%
    ungroup(!!VAR) %>% 
    mutate(labels = ifelse(!!VAR %in% seq(to * round(min(!!VAR) / to), max(!!VAR), to),
                           !!VAR, "")) %>%
    select(breaks = UQ(VAR), labels)
}
