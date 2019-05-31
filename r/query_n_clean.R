# Query and data cleaning file

# Data request from K. Fenske 2017-12-26
# Jane Sullivan (jane.sullivan1@alaska.gov)
# Last updated 2019-05-31-08

# Set-up ----

source("r/helper.r")

YEAR <- 2018 # use to update data files (most recent year of data)

# Oracle connections ----

# Database usernames and passwords (this file and file path are user-specific)
ora <- read_csv("my_oracle/database.csv") 

# Connection strings in T:/Toolbox/TNS/tnsnames.ora

# IFDB aka ALEX. Region I database, ultimately will be replaced by Zander
ifdb <- "(DESCRIPTION =
(ADDRESS = (PROTOCOL = TCP)(HOST = 10.209.0.83)(PORT = 1521))
(CONNECT_DATA = (SERVER = DEDICATED)
(SERVICE_NAME = DFGCFR1P.500040564.us1.internal)))"

ifdb_channel <- dbConnect(drv = dbDriver('Oracle'), 
                          username = ora$ifdb_user, 
                          password = ora$ifdb_pw, 
                          dbname = ifdb)

# Zander. Region I database (update 2019-05-31: Zander and IFDB have merged into
# the same db, but are in separate schemas. Queries haven't changed.
zprod <- "(DESCRIPTION =
(ADDRESS = (PROTOCOL = TCP)(HOST = 10.209.0.83)(PORT = 1521))
(CONNECT_DATA = (SERVER = DEDICATED)
(SERVICE_NAME = DFGCFR1P.500040564.us1.internal)))"

zprod_channel <- dbConnect(drv = dbDriver('Oracle'), 
                           username = ora$zprod_user, 
                           password = ora$zprod_pw, 
                           dbname = zprod)

# Data warehouse. eLandings, fish tickets, GEF
dwprod <- "(DESCRIPTION =
(ADDRESS = (PROTOCOL = TCP)(HOST = 10.209.2.34)(PORT = 1521))
(CONNECT_DATA = (SERVER = DEDICATED)
(SERVICE_NAME = DFGDWP.us1.ocm.s7134325.oraclecloudatcustomer.com)))"

dwprod_channel <- dbConnect(drv = dbDriver('Oracle'), 
                            username = ora$dwprod_user, 
                            password = ora$dwprod_pw, 
                            dbname = dwprod)

# IFDB fishery removals ----

# Harvest from IFDB (the Region 1 SEAK database). This source is what managers
# are using and what I've used for the NSEI stock assessments.  Landings
# are always entered as POUNDS and then converted to ROUND_POUNDS using a
# conversion factor related to the disposition code. Prior to 1985 there was no
# disposition code for landings, which is why ROUND_POUNDS is not populated
# prior to 1985. Here we assume fish were delivered whole prior to 1985, because
# that's the best we have.

query <-
  " select  year, adfg_no, trip_no, vessel_name, port_code, gear,
         catch_date, sell_date, harvest_code, harvest, g_stat_area,
         g_management_area_code, species_code, pounds, round_pounds,
         delivery_code, g_cfec_fishery_group, g_cfec_fishery
    from    out_g_cat_ticket
    where   species_code = '710' and g_management_area_code in ('NSEI','SSEI') "

dbGetQuery(ifdb_channel, query) %>% 
  rename(Mgmt_area = G_MANAGEMENT_AREA_CODE) %>% 
  mutate(ROUND_POUNDS = ifelse(ROUND_POUNDS == 0, POUNDS, ROUND_POUNDS)) -> ifdb_catch

write_csv(ifdb_catch, paste0("data/catch/region1_ifdb/region1_sablecatch_", 
                      min(ifdb_catch$YEAR), "_", max(ifdb_catch$YEAR), "_CONFIDENTIAL.csv"))

ifdb_catch %>% group_by(YEAR, Mgmt_area) %>% summarise(sum(ROUND_POUNDS)) %>% View()

# GEF fishery removals ----

# gef = gross earnings file. This was pitched to me by J. Shriver as the
# definitive source for historical landings records. The detailed file includes
# all sablefish (species code 710) landings, including as bycatch. If you're
# interested in which fishery landings came from, use the CFEC permit code (cfec
# group code = 'C' = Sablefish, code descriptions at
# https://www.cfec.state.ak.us/misc/FshyDesC.htm

query <-
  " select  adfg_b_batch_year as year, adfg_h_date_landed as date_landed,
            adfg_h_gear_code as gear_code, adfg_h_port as port_code, 
            cfec_harvest_area, adfg_h_permit_fishery as cfec_permit, 
            adfg_i_delivery_code as delivery_code, adfg_i_harvest_code as harvest_code, 
            adfg_h_mgt_program_id as mgt_program_code, adfg_i_species_code as species_code, 
            adfg_i_stat_area as stat_area, adfg_h_stat_area_type as stat_area_type, 
            cfec_landing_status as landing_status, adfg_i_pounds as pounds, 
            adfg_i_whole_pounds as whole_pounds
    from    dwgross.ge_gross_earnings 
    where   adfg_i_species_code = '710' "

system.time(dbGetQuery(dwprod_channel, query) -> gef ) 

gef %>% mutate(YEAR = as.numeric(YEAR)) -> gef

# gef %>% group_by(YEAR) %>% summarize(n()) %>% View()

# Stat area information by database type (includes additional info like state vs
# fed waters, effective dates for stat area codes, ADFG/NMFS mgt areas, etc.)
query <- 
  " select  stat_area, database_type as stat_area_type, region, waters, 
            start_date, end_date, g_mgt_area_district, g_nmfs_area 
    from    dwgross.ge_stat_area "

dbGetQuery(dwprod_channel, query) -> stat_areas

merge(gef, stat_areas, by = c("STAT_AREA", "STAT_AREA_TYPE")) -> gef

# Full gef in case it becomes of interest for use in the project later. There's
# an Excel file with detailed variable descriptions in the gross earning file
# folder
write_csv(gef, paste0("data/catch/gross_earnings_file/allak_sablecatch_gef_", min(gef$YEAR), "_", max(gef$YEAR), "_CONFIDENTIAL.csv"))

# Pers. comm. with K. Fenske 2019-01-03 indicates that she's already obtained
# Aluetian Is. data and only needs state data for NSEI, SSEI, and PWS (I include
# Lower Cook Inlet b/c it's also in Region 2 with PWS. S. Larson, who is
# currently working with historical stat areas for an SK project, helped to
# update NAs in the waters column. 

# File I sent to S. Larsen:
# gef %>% filter(G_MGT_AREA_DISTRICT %in% 
#            c("PWS", "PWSE", "PWSI", "PWSI", "PWSW", "LOWCI", "NA")) %>% 
#   distinct(G_MGT_AREA_DISTRICT, STAT_AREA, WATERS, START_DATE, END_DATE) %>% 
# write_csv("region2_statareas_districts.csv")  

# Once SK project is complete these fields will be updated in the source
# database. For now pull these updates from S. Larsen in separately:

read_csv("data/catch/gross_earnings_file/SJL_region2_statareas_districts.csv") %>% 
  select(STAT_AREA, START_DATE, END_DATE, UPDATED_WATERS) -> updated_waters

left_join(gef, updated_waters, by = c("STAT_AREA", "START_DATE", "END_DATE"))  %>% 
  mutate(WATERS = ifelse(is.na(UPDATED_WATERS), WATERS, UPDATED_WATERS)) %>%
  select(-UPDATED_WATERS) -> gef

# Heads up: This summary will include IFQ caught in state waters. Let me know and we can
# filter that using the management program code field.

gef %>% 
  filter(c(G_MGT_AREA_DISTRICT %in% 
             c("NSEI","SSEI","PWS", "PWSE", "PWSI", "PWSI", "PWSW", "LOWCI") &
             WATERS %in% c("STATE")) ) %>% 
  droplevels() %>% 
  mutate(
    # BEWARE: Landings are always entered as POUNDS and then converted to
    # WHOLE_POUNDS using a conversion factor related to the disposition code.
    # Prior to 1985 there was no disposition code for landings, which is why
    # WHOLE_POUNDS is not populated prior to 1985. Here we assume fish were
    # delivered whole prior to 1985, because that's the best we have.
    WHOLE_POUNDS = ifelse(WHOLE_POUNDS == 0, POUNDS, WHOLE_POUNDS),
    # Lump all PWS areas into one for clarity. Lookup table in
    # adfg_mgt_area_metadata_slarsen.shp, search under G_SEL_AREA
    Mgmt_area = derivedFactor("PWS" = G_MGT_AREA_DISTRICT %in% 
                                    c("PWS", "PWSE", "PWSI", "PWSI", "PWSW"),
                             "NSEI" = G_MGT_AREA_DISTRICT == "NSEI",
                             "SSEI" = G_MGT_AREA_DISTRICT == "SSEI", 
                             "LOWCI" = G_MGT_AREA_DISTRICT == "LOWCI"),
    # Aggregate gear field codes (lookup table in  GEF_Variable_Descriptions....xls)
    GEAR_CODE = derivedFactor("HAL" = GEAR_CODE %in% c("61", "06"), #hook-and-line aka longline
                              "POT" = GEAR_CODE %in% c("91", "09"),
                              "TRW" = GEAR_CODE %in% c("07", "47", "27", "17"), # non-pelagic/bottom trawl, pelagic/mid-water trawl, otter trawl, or beam trawl
                              "JIG/TRL" = GEAR_CODE %in% c("26", "05", "25", "15"), # mechanical jigs, power troll, hand troll, dinglebar troll
                              "GNT" = GEAR_CODE %in% c("03", "04", "41"), # set, drift, or sunken gillnet
                              "UNK" = GEAR_CODE %in% c("99"))) %>% # unknown
  # Per request: catch by gear and adfg mangament area
  group_by(YEAR, Mgmt_area, GEAR_CODE) %>% 
  summarise(WHOLE_POUNDS = sum(WHOLE_POUNDS)) %>%
  ungroup() %>% 
  complete(YEAR, Mgmt_area, GEAR_CODE,
           fill = list(WHOLE_POUNDS = 0)) %>%
  write_csv(paste0("data/catch/gross_earnings_file/regions1n2_statewater_sablecatch_gef_", 
                   min(gef$YEAR), "_", max(gef$YEAR), ".csv")) -> state_reg1n2

ggplot(state_reg1n2 %>%
         droplevels()) +
  geom_line(aes(as.numeric(YEAR), WHOLE_POUNDS, 
                colour = Mgmt_area, group = Mgmt_area), size = 1) +
  facet_wrap(~ GEAR_CODE, scales = "free") +
  scale_x_continuous(breaks = c(seq(min(state_reg1n2$YEAR), max(state_reg1n2$YEAR), 5))) 
  
# Historical fishery removals ----

# I found a file with historical catches going from 1907-2000 in a folder from
# D. Carlile to S. Dressel. I include it for comparison but would not recommend
# using it for anything until it's been vetted (and I don't know if that's possible). 

read_csv("data/catch/nsei_historicalsablecatch_nosource_carlile_1907_2000.csv") %>% 
  mutate(data_source = "Historical (Carlile et al. 2002)",
         Mgmt_area = "NSEI") -> historical

# Compare fishery removals ----

state_reg1n2 %>% 
  group_by(Mgmt_area, YEAR) %>% 
  summarise(WHOLE_POUNDS = sum(WHOLE_POUNDS)) %>%
  mutate(data_source = "Gross Earnings File") -> gef_sum

ifdb_catch %>% 
  group_by(Mgmt_area, YEAR) %>% 
  summarise(WHOLE_POUNDS = sum(ROUND_POUNDS)) %>%
  mutate(data_source = "IFDB (Region I fish ticket database)") -> ifdb_sum

bind_rows(historical,  filter(gef_sum, Mgmt_area %in% c("NSEI", "SSEI")), ifdb_sum) %>% 
  arrange(Mgmt_area, YEAR, data_source) %>%  
  # convert to metric tons
  mutate(whole_mt = WHOLE_POUNDS * 0.000453592) %>% 
  select(year = YEAR, Mgmt_area, whole_mt, data_source) -> compare_catch

axis <- tickr(compare_catch, year, 5)

ggplot(compare_catch) + 
  geom_rect(aes(xmin = 1975, xmax = 1985, ymin = -Inf, ymax = Inf), 
            fill = "grey95", colour = NA, alpha = 0.9, show.legend = FALSE) +
  geom_line(aes(as.numeric(year), whole_mt, 
                colour = data_source, linetype = data_source, group = data_source), size = 1) +
  scale_x_continuous(breaks = axis$breaks, labels = axis$labels) +
  scale_colour_grey() +
  facet_wrap(~Mgmt_area, ncol = 1, scales = "free_y") +
  labs(x = "Year", y = "Total catch (round mt)", 
       colour = "Data source", linetype = "Data source",
       title = "Comparison of landings in NSEI and SSEI by data source") +
  theme(legend.position = "top") 

ggsave(paste0("catch_comparison_", min(compare_catch$year), "_", max(compare_catch$year), ".png"), dpi=300,  height=6, width=8,  units="in")

write_csv(compare_catch, paste0("data/catch/catch_comparison_summary_", min(compare_catch$year), "_", max(compare_catch$year), ".csv"))

state_reg1n2 %>%
  rename(GEAR = GEAR_CODE) %>% 
  filter(Mgmt_area == "NSEI" & YEAR %in% c(1975:1985)) %>% 
  group_by(YEAR, GEAR) %>% 
  summarise(WHOLE_POUNDS = sum(WHOLE_POUNDS)) %>% 
  mutate(data_source = "Gross Earnings File") %>% 
  bind_rows(ifdb_catch %>%
              select(YEAR, Mgmt_area, GEAR, WHOLE_POUNDS = ROUND_POUNDS) %>% 
              filter(Mgmt_area == "NSEI" & YEAR %in% c(1975:1985)) %>% 
              mutate(data_source = "IFDB (Region I fish ticket database)",
                     GEAR = derivedFactor(
                       "HAL" = GEAR %in% c("Longline-keel < 60 ft.", "Longline"), #hook-and-line aka longline
                       "POT" = GEAR %in% c("Pot"),
                       "TRW" = GEAR %in% c("Beam Trawl"),
                       "JIG/TRL" = GEAR %in% c("Power Troll", "Hand Line/Jig/Troll"), # all jigs and troll gear
                       "UNK" = GEAR %in% c("Other/Unknown/Missing"))) %>% 
              group_by(YEAR, GEAR) %>% 
              summarise(WHOLE_POUNDS = sum(WHOLE_POUNDS)) %>% 
              mutate(data_source = "IFDB (Region I fish ticket database)")) %>% 
  mutate(whole_mt = WHOLE_POUNDS * 0.000453592) %>% 
  select(-WHOLE_POUNDS) -> discrepancy

axis <- tickr(discrepancy, YEAR, 2)
ggplot(discrepancy) + 
 geom_line(aes(as.numeric(YEAR), whole_mt, 
                colour = data_source, linetype = data_source, group = data_source), 
           size = 1) +
  scale_x_continuous(breaks = axis$breaks, labels = axis$labels) +
  scale_colour_grey() +
  facet_wrap(~GEAR, ncol = 1, scales = "free_y") +
  labs(x = "Year", y = "Total catch (round mt)", 
       colour = "Data source", linetype = "Data source",
       title = "Comparison of NSEI landings by gear and data source") +
  theme(legend.position = "top") 

# Final summary of fishery removals ----

# B. Williams, J. Sullivan, and S. Larsen reviewed the available landings data
# 2019-05-31. Because of the discrepancies in GEF and IFDB between 1975 and 1984
# (see catch_comparison_summary_1907_2018.csv), we decided for NSEI to use the
# Carlile et al. 2002 historical catch time series from 1907-1984 and GEF
# 1985-present. For SSEI, we use GEF for 1975-present. Also use GEF for PWS.

gef_sum %>%
  filter(Mgmt_area == "PWS") %>% 
  mutate(whole_mt = WHOLE_POUNDS * 0.000453592) %>% 
  select(year = YEAR, Mgmt_area, whole_mt, data_source) %>% 
  bind_rows(compare_catch) -> final_catch

final_catch %>% 
  filter( (Mgmt_area == "NSEI" & year <= 1984 & data_source == "Historical (Carlile et al. 2002)") | 
            (Mgmt_area == "NSEI" & year >= 1985 & data_source == "Gross Earnings File") |
            (Mgmt_area == "SSEI" & data_source == "Gross Earnings File") |
            (Mgmt_area == "PWS") ) -> final_catch

write_csv(final_catch, paste0("data/catch/catch_nsei_ssei_pws_1907_", YEAR, "_USEME.csv"))

axis <- tickr(final_catch, year, 10)
ggplot(final_catch, aes(x = year, y = whole_mt, 
                        colour = Mgmt_area, linetype = Mgmt_area, group = Mgmt_area)) +
  geom_line() +
  scale_colour_grey() +
  scale_x_continuous(breaks = axis$breaks, labels = axis$labels) +
  labs(x = "\nYear", y = "Total catch (round mt)\n", linetype = NULL, colour = NULL) +
  theme(legend.position = c(0.9, 0.8)) 

ggsave(paste0("total_catch_1907_", YEAR, ".png"), dpi=300, height=3.5, width=5, units="in")

# Fishery biological data ----

# Fishery and pot survey bio data still come from IFDB, ZPROD official source
# for longline survey data

# project codes: query zprod: "select new_project_code, project_code, project from
# lookup.project_conversion where category_code = 'g'"

# new (Zander) = old (IFDB) = description
# 601 = 01 = Clarence sablefish longline survey
# 602 = 02 = commercial longline trip
# 603 = 03 = Chatham sablefish longline survey
# 607 = 07 = atypical sample (unknown gear)
# 608 = 08 = atypical longline sample
# 617 = 17 = commercial pot trip
# 610 = 10 = Clarence sablefish pot survey
# 611 = 11 = Chatham sabelfish pot survey
# 623 = 23 = Canadian commercial longline
# 624 = 24 = Canadian commercial pot
# 625 = 25 = Canadian commercial trawl
# 626 = 26 = Canadian scientific survey

# length: fork length, cm
# weight: kg
# Maturity: "0" = immature, "1" = mature

query <-
  " select  year, project_code, g_management_area_code, species_code, 
            length_millimeters / 10 as length, weight_kilograms as weight, 
            age, sex_code, maturity_code, maturity
    from    out_g_bio_age_sex_size
    where   species_code = '710' and
            project_code in ('02', '17') and
            g_management_area_code in ('NSEI', 'SSEI')"

dbGetQuery(ifdb_channel, query) %>%  
  filter(!is.na(LENGTH)) %>% 
  mutate(Sex = derivedFactor("Male" = SEX_CODE == "01",
                             "Female" = SEX_CODE == "02",
                             .default = NA),
         Maturity = derivedFactor("0" = MATURITY_CODE %in% c("01", "02"), 
                                  "1" = MATURITY_CODE %in% c("03", "04", "05", "06", "07"),
                                  .default = NA),
         Gear = derivedFactor("Pot" = PROJECT_CODE == "17",
                              "Longline" = PROJECT_CODE == "02"),
         Source = "Fishery") %>% 
  select(year = YEAR, Gear, Source, Mgmt_area = G_MANAGEMENT_AREA_CODE, 
         Spp_cde = SPECIES_CODE, length = LENGTH, weight = WEIGHT,
         age = AGE, Sex, Maturity)  %>% 
  # Omit two records that look like errors
  filter(! c(length < 20 | length > 120)) -> fsh_bio

write_csv(fsh_bio, paste0("data/biological/fishery/detailed_fisherybio_nsei_ssei_", min(fsh_bio$year), "_", max(fsh_bio$year), ".csv")) 

# Pot survey biological data ----

# These are the Clarence and Chatham tagging survey and do not have much bio
# data compared to the longline surveys. There are no fish weight data, and no
# maturity or sex data for Clarence Strait. Could get some simple length comps
# for Clarence though.

query <-
  " select  year, project_code, management_area, species_code, 
            length_millimeters / 10 as length, weight_kilograms as weight,
            age, sex_code, maturity_code
    from    out_g_bio_effort_age_sex_size
    where   species_code = '710' and
            project_code in ('10', '11') and
            management_area in ('NSEI', 'SSEI') "

dbGetQuery(ifdb_channel, query) %>% 
  filter(!is.na(LENGTH)) %>% 
  mutate(Sex = derivedFactor("Male" = SEX_CODE == "01",
                             "Female" = SEX_CODE == "02",
                             .default = NA),
         Maturity = derivedFactor("0" = MATURITY_CODE %in% c("01", "02"), 
                                  "1" = MATURITY_CODE %in% c("03", "04", "05", "06", "07"),
                                  .default = NA),
         Gear = "Pot",
         Source = "Survey") %>% 
  select(year = YEAR, Gear, Source, Mgmt_area = MANAGEMENT_AREA, 
         Spp_cde = SPECIES_CODE, length = LENGTH, weight = WEIGHT, age = AGE, Sex, Maturity) -> pot_bio

pot_bio %>% group_by(year, Mgmt_area) %>% summarise(n()) %>% View()

write_csv(pot_bio, paste0("data/biological/survey/detailed_potsrvbio_nsei_ssei_", min(pot_bio$year), "_", max(pot_bio$year), ".csv"))

# Longline survey biological data ----

# These are stored in the modern database, Zander (aka ZPROD)

query <-
  " select  year, project_code, g_stat_area as stat, species_code, 
            length_millimeters / 10 as length, weight_kilograms as weight, 
            age, sex_code, maturity_code
    from    output.out_g_sur_longline_specimen
    where   species_code = '710' and
            project_code in ('601', '603')"

dbGetQuery(zprod_channel, query) -> srv_bio

# View doesn't have management areas, join with stat_area look up as a check
# (the project code should be enough because they're area-specific)

query <- 
  " select  g_stat_area as stat, g_management_area_code
    from    lookup.g_stat_area"

dbGetQuery(zprod_channel, query) -> stat_areas

merge(srv_bio, stat_areas, by = "STAT") %>% 
  filter(!is.na(LENGTH)) %>% 
  mutate(Sex = derivedFactor("Male" = SEX_CODE == "01",
                             "Female" = SEX_CODE == "02",
                             .default = NA),
         Maturity = derivedFactor("0" = MATURITY_CODE %in% c("01", "02"), 
                                  "1" = MATURITY_CODE %in% c("03", "04", "05", "06", "07"),
                                  .default = NA),
         Gear = "Longline",
         Source = "Survey") %>% 
  select(year = YEAR, Gear, Source, Mgmt_area = G_MANAGEMENT_AREA_CODE, 
         Spp_cde = SPECIES_CODE, length = LENGTH, weight = WEIGHT, age = AGE, Sex, Maturity) -> srv_bio

write_csv(srv_bio, paste0("data/biological/survey/detailed_llsrvbio_nsei_ssei_", min(srv_bio$year), "_", max(srv_bio$year), ".csv"))

# Fishery CPUE/logbook ----

# Longline survey CPUE ----

# Physical areas of stat areas. From S. Larsen 2018-04-23. Will use this to
# calculate relative population numbers/weight.

read_csv("data/groundfish_ssei_nsei_statarea_larsen_20180423.csv") %>% 
  select(Mgmt_area = SUBDISTRICT_CODE, Stat = STAT_AREA, stat_km2 = Area_Kilometers) %>% 
  mutate(Stat = as.character(Stat)) %>% 
  arrange(Mgmt_area) -> stat_km2

# 601 = 01 = Clarence sablefish longline survey
# 603 = 03 = Chatham sablefish longline survey
query <- 
  " select  year, project_code, trip_no, target_species_code, adfg_no, vessel_name, 
            time_first_buoy_onboard, number_of_stations, hooks_per_set, hook_size, 
            hook_spacing_inches, sample_freq, last_skate_sampled, effort_no, station_no,
            g_stat_area as stat, start_latitude_decimal_degrees as start_lat,
            start_longitude_decimal_degree as start_lon, end_latitude_decimal_degrees as end_lat,
            end_longitude_decimal_degrees as end_lon, avg_depth_fathoms * 1.8288 as depth_meters, 
            number_hooks, bare, bait, invalid, sablefish, 
            subset_condition_code

from    output.out_g_sur_longline_hook_acc_bi

where   project_code in ('603', '03', '601', '01')" 

dbGetQuery(zprod_channel, query) -> srv_eff

srv_eff %>% 
  filter(YEAR < max(YEAR)) %>% #the programmers have some dummy data in the db for the upcoming year
  mutate(date = ymd(as.Date(TIME_FIRST_BUOY_ONBOARD)), #ISO 8601 format
         julian_day = yday(date)) %>% 
  select(year = YEAR, Project_cde = PROJECT_CODE, Station_no = STATION_NO,
         trip_no = TRIP_NO, Adfg = ADFG_NO, Vessel = VESSEL_NAME, date, julian_day,
         Stat = STAT,  set = EFFORT_NO, start_lat = START_LAT, start_lon = START_LON, end_lat = END_LAT,
         end_lon = END_LON, depth = DEPTH_METERS, no_hooks = NUMBER_HOOKS, hooks_bare = BARE,
         hooks_bait = BAIT, hook_invalid = INVALID, hooks_sablefish = SABLEFISH,
         subset_condition_cde = SUBSET_CONDITION_CODE) %>%
  # join in stat area phys. area info
  left_join(stat_km2, by = "Stat") -> srv_eff

srv_eff %>% 
  group_by(Project_cde) %>% 
  summarize(n = length(trip_no),
             min_yr = min(year),
             max_yr = max(year))

write_csv(srv_eff, paste0("data/effort/llsrv_cpue_nsei_ssei_raw_", min(srv_eff$year), "_", max(srv_eff$year), ".csv"))

# Fishery cpue ----

# Kamala Carroll pulls the IFDB view out_g_log_longline_c_e, populates new
# columns effort_target_species_code and effort_comments, and sends to Scott
# Johnson. Scott runs a series of Martina Kallenburger's sql scripts (which I
# don't have access to) that match fish tickets pounds back to set based on
# Kamala's set target designation based on some undocumented methodology
# (proportional to numbers/pounds in logbook?). It lives in a view called
# sablefish_cpue_final_view in scottj's IFDB schema, though he has made a public
# synonym for it. This output doesn't contain information on sets designated as
# halibut targets. Note that it is missing effort_no's (effort_no's = individual
# sets).

query <- 
  " select  year, project_code, trip_no, adfg_no, longline_system_code, sell_date, 
            hook_size, hook_spacing, number_of_skates, number_of_hooks,
            average_depth_meters, g_management_area_code, g_stat_area, trip_target, set_target,
            effort_no, sable_lbs_per_set, time_set, time_hauled, 
            start_latitude_decimal_degrees, start_longitude_decimal_degree

    from    sablefish_cpue_final_view

    where   g_management_area_code in ('NSEI', 'SSEI')"

dbGetQuery(ifdb_channel, query) -> fsh_eff

fsh_eff %>% 
  # rename, define factors, remove mixed hook sizes
  mutate(date = ymd(as.Date(TIME_SET)), #ISO 8601 format
         julian_day = yday(date),
         soak = difftime(TIME_HAULED, TIME_SET, units = "hours"),
         Gear = factor(LONGLINE_SYSTEM_CODE),
         Hook_size = HOOK_SIZE, 
         hook_space = HOOK_SPACING, #*FLAG* - check that hook_space is in inches
         Size = factor(as.numeric(gsub("[^0-9]", "", Hook_size))),
         no_hooks = NUMBER_OF_HOOKS,
         sable_lbs_set = SABLE_LBS_PER_SET) %>% 
  select(year = YEAR, Mgmt_area = G_MANAGEMENT_AREA_CODE, trip_no = TRIP_NO, 
         Adfg = ADFG_NO, Spp_cde = TRIP_TARGET, date, julian_day, 
         soak, Gear = LONGLINE_SYSTEM_CODE, Hook_size, Size, 
         hook_space, Stat = G_STAT_AREA, no_hooks, depth = AVERAGE_DEPTH_METERS, 
         sets = EFFORT_NO, sable_lbs_set, start_lat = START_LATITUDE_DECIMAL_DEGREES,
         start_lon = START_LONGITUDE_DECIMAL_DEGREE) -> fsh_eff

fsh_eff %>% 
  group_by(Mgmt_area) %>% 
  summarize(n = length(trip_no),
            min_yr = min(year),
            max_yr = max(year))

write_csv(fsh_eff, paste0("data/effort/fishery_cpue_nsei_ssei_CONFIDENTIAL_", min(fsh_eff$year), "_", max(fsh_eff$year), ".csv"))
