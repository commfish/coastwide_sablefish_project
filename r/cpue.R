# Fishery and survey effort

# Data request from K. Fenske 2017-12-26
# Jane Sullivan (jane.sullivan1@alaska.gov)
# 2018-04-24

# Fishery CPUE is in lbs/hook.
# Survey CPUE is number/hook.

# Libraries----

library(tidyverse)

read_csv("data/effort/llsrv_cpue_nsei_ssei_raw.csv",
         guess_max = 500000) -> srv_cpue

srv_cpue  %>% 
  filter(# Mike Vaughn 2018-03-06: Sets (aka subsets with 12 or more invalid hooks are subset condition code "02" or invalid)
    subset_condition_cde != "02") %>% 
  mutate(Year = factor(year),
         Stat = factor(Stat),
         #standardize hook spacing (Sigler & Lunsford 2001, CJFAS) changes in 
         #hook spacing. pers. comm. with aaron.baldwin@alaska.gov: 1995 & 1996 -
         #118 in; 1997 - 72 in.; 1998 & 1999 - 64; 2000-present - 78". This is
         #different from KVK's code (he assumed 3 m before 1997, 2 m in 1997 and
         #after)
         hooks_bare = ifelse(is.na(hooks_bare), 0, hooks_bare),
         hooks_bait = ifelse(is.na(hooks_bait), 0, hooks_bait),
         hook_invalid = ifelse(is.na(hook_invalid), 0, hook_invalid),
         no_hooks = no_hooks - hook_invalid,
         # 1 inch = 0.0254 meters
         std_hooks = ifelse(year <= 1996, 2.2 * no_hooks * (1 - exp(-0.57 * (118 * 0.0254))),
                            ifelse(year == 1997, 2.2 * no_hooks * (1 - exp(-0.57 * (72 * 0.0254))),
                                   ifelse( year %in% c(1998, 1999), 2.2 * no_hooks * (1 - exp(-0.57 * (64 * 0.0254))),
                                           2.2 * no_hooks * (1 - exp(-0.57 * (78 * 0.0254)))))),
         sablefish_retained = replace(hooks_sablefish, is.na(hooks_sablefish), 0), # make any NAs 0 values
         # Soak time in the surveys was 1 hour prior to 1997, and 3 hours in
         # 1997 to present. According to Carlile et al. (2002), 1-hour soak time
         # CPUEs were about 43% lower than those associated with 3-hour soak
         # times (see README for more information)
         std_cpue = ifelse(year <= 1996, 1.43 * sablefish_retained/std_hooks, sablefish_retained/std_hooks)
  ) -> srv_cpue

# # Get NPUE and RPN by strata *DON'T HAVE STRATA AREAS YET*
# srv_cpue %>% 
#   group_by(Mgmt_area, Stat, year) %>% 
#   summarise(npue = round(mean(std_cpue, na.rm = TRUE), 2),
#             n = length(std_cpue),
#             CV = sd(std_cpue) / npue) %>% 
#   left_join(srv_cpue %>%
#               distinct(Stat, stat_km2, by = "Stat")) %>%
#   mutate(rpn = npue * stat_km2) -> srv_sum
# 
# # Sum RPNs by year and management area
# srv_sum %>% 
#   group_by(year, Mgmt_area) %>% 
#   summarize(RPN = round(sum(rpn), 2)) %>% 
#   left_join(srv_cpue %>% 
#               group_by(year, Mgmt_area) %>% 
#               summarise(NPUE = round(mean(std_cpue, na.rm = TRUE), 2)),
#             by = c("year", "Mgmt_area")) -> srv_npue_rpn

srv_cpue %>% 
  group_by(year, Mgmt_area) %>% 
  summarise(NPUE = round(mean(std_cpue, na.rm = TRUE), 2),
            sd = round(sd(std_cpue, na.rm = TRUE), 2),
            n = length(std_cpue),
            se = round(sd / sqrt(n), 4),
            CV = round(sd / NPUE, 2),
            RSE = round(se / NPUE, 2)) %>% 
  filter(year >= 1997) %>% 
  mutate(description = paste0("ADFG_LLsrv_NPUE_", Mgmt_area)) %>% 
  select(Year = year, Value = NPUE, CV = RSE, description) %>% 
  arrange(description) -> srv_sum

# Fishery cpue

# Legacy values from \\dfg.alaska.local\DCF\Sitka\GROUNDFISH\SABLEFISH\NSEI & SSEI summary files
rbind(data.frame(Mgmt_area = "NSEI",
                 year = 1980:1996,
                 WPUE = c(0.719, 0.980, 1.240, 1.521, 1.304, 1.880, 1.581,
                          1.704, 1.579, 1.156, 1.402, 1.267, 1.468, 1.587,
                          0.993, 0.838, 0.730)),
      data.frame(Mgmt_area = "SSEI",
                 year = 1985:1996,
                 WPUE = c(0.232,0.280,0.185, 0.228, 0.242, 0.248, 0.211,
                          0.269, 0.219, 0.210, 0.227, 0.171))) -> legacy

# 1997 - present                                   
read_csv(paste0("data/effort/fishery_cpue_nsei_ssei_CONFIDENTIAL.csv"), 
         guess_max = 50000) %>% 
   mutate(std_hooks = 2.2 * no_hooks * (1 - exp(-0.57 * (hook_space / 39.37))), 
         std_cpue = sable_lbs_set / std_hooks) -> fsh_cpue

bind_rows(legacy,
          fsh_cpue %>% 
            group_by(year, Mgmt_area) %>% 
            summarise(WPUE = round(mean(std_cpue, na.rm = TRUE), 2),
                      sd = round(sd(std_cpue, na.rm = TRUE), 2),
                      n = length(std_cpue),
                      se = round(sd / sqrt(n), 4),
                      CV = round(sd / WPUE, 2),
                      RSE = round(se / WPUE, 2))) %>% 
            mutate(description = paste0("ADFG_LLfishery_WPUE_", Mgmt_area)) %>% 
  select(Year = year, Value = WPUE, CV = RSE, description) -> fsh_sum

bind_rows(srv_sum, fsh_sum) %>% 
  write_csv("data/effort/all_cpue_indices.csv")
