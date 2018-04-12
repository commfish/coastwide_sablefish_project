# Query and data cleaning file
# Data request from K. Fenske 2017-12-26
# Jane Sullivan (jane.sullivan1@alaska.gov)
# 2018-01-08

# Libraries and ggplot theme ----

library(ROracle)
library(tidyverse)
library(mosaic)

theme_set(theme_bw(base_size = 12) + 
            theme(panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  axis.text.x=element_text(angle = 45, hjust = 1)))

# Oracle connections ----

# Database usernames and passwords (this file and file path are user-specific)
ora <- read_csv("~/my_oracle/database.csv") 

# Connection strings in T:/Toolbox/TNS/tnsnames.ora

# IFDB aka ALEX. Region I database, ultimately will be replaced by Zander
ifdb <- "(DESCRIPTION =
(ADDRESS = (PROTOCOL = TCP)(HOST = db-ifdb.dfg.alaska.local)(PORT = 1521))
(CONNECT_DATA = (SERVER = DEDICATED)
(SERVICE_NAME = ifdb.dfg.alaska.local)))"

ifdb_channel <- dbConnect(drv = dbDriver('Oracle'), 
                          username = ora$ifdb_user, 
                          password = ora$ifdb_pw, 
                          dbname = ifdb)

# Zander. Region I database
zprod <- "(DESCRIPTION =
(ADDRESS = (PROTOCOL = TCP)(HOST = db-zprod.dfg.alaska.local)(PORT = 1521))
(CONNECT_DATA = (SERVER = DEDICATED)
(SERVICE_NAME = zprod.dfg.alaska.local)))"

zprod_channel <- dbConnect(drv = dbDriver('Oracle'), 
                           username = ora$zprod_user, 
                           password = ora$zprod_pw, 
                           dbname = zprod)

# Data warehouse. eLandings, fish tickets, GEF
dwprod <- "(DESCRIPTION =
(ADDRESS = (PROTOCOL = TCP)(HOST = db-dwprod.dfg.alaska.local)(PORT = 1521))
(CONNECT_DATA = (SERVER = DEDICATED)
(SERVICE_NAME = dwprod.dfg.alaska.local)))"

dwprod_channel <- dbConnect(drv = dbDriver('Oracle'), 
                            username = ora$dwprod_user, 
                            password = ora$dwprod_pw, 
                            dbname = dwprod)

# Fishery removals ----

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
write_csv(gef, "data/catch/gross_earnings_file/allak_sablecatch_gef_1975_2016_CONFIDENTIAL.csv")

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
    Mgt_area = derivedFactor("PWS" = G_MGT_AREA_DISTRICT %in% 
                                    c("PWS", "PWSE", "PWSI", "PWSI", "PWSW"),
                                  "NSEI" = G_MGT_AREA_DISTRICT == "NSEI",
                                  "SSEI" = G_MGT_AREA_DISTRICT == "SSEI", 
                                  "LOWCI" = G_MGT_AREA_DISTRICT == "LOWCI"),
    # Aggregate gear field codes (lookup table in GEF_Variable_Descriptions....xls)
    GEAR_CODE = derivedFactor("HAL" = GEAR_CODE %in% c("61", "06"), #hook-and-line aka longline
                              "POT" = GEAR_CODE %in% c("91", "09"),
                              "TRW" = GEAR_CODE %in% c("07", "47", "27", "17"), # non-pelagic/bottom trawl, pelagic/mid-water trawl, otter trawl, or beam trawl
                              "JIG" = GEAR_CODE %in% c("26"), # mechanical jigs
                              "TRL" = GEAR_CODE %in% c("05", "25", "15"), # hand troll, dinglebar troll
                              "GNT" = GEAR_CODE %in% c("03", "04", "41"), # set, drift, or sunken gillnet
                              "UNK" = GEAR_CODE %in% c("99"))) %>% # unknown
  # Per request: catch by gear and adfg mangament area
  group_by(YEAR, Mgt_area, GEAR_CODE) %>% 
  summarise(WHOLE_POUNDS = sum(WHOLE_POUNDS)) %>%
  ungroup() %>% 
  complete(YEAR, Mgt_area, GEAR_CODE,
           fill = list(WHOLE_POUNDS = 0)) %>%
  write_csv("data/catch/gross_earnings_file/regions1n2_statewater_sablecatch_gef_1975_2016.csv") ->
  state_reg1n2

ggplot(state_reg1n2 %>%
         droplevels()) +
  geom_line(aes(as.numeric(YEAR), WHOLE_POUNDS, 
                colour = Mgt_area, group = Mgt_area), size = 1) +
  facet_wrap(~ GEAR_CODE, scales = "free") +
  scale_x_continuous(breaks = c(seq(min(state_reg1n2$YEAR), max(state_reg1n2$YEAR), 5))) 
  
# I found a file with historical catches going from 1907-2000 in a folder from
# D. Carlile to S. Dressel. I include it for comparison but would not recommend
# using it for anything until it's been vetted (and I don't know if that's possible). 

read_csv("data/catch/nsei_historicalsablecatch_nosource_carlile_1907_2000.csv") %>% 
  mutate(data_source = "Historical (unknown)") -> nsei_historical

state_reg1n2 %>% 
  filter(Mgt_area == "NSEI") %>% 
  select(-Mgt_area) %>% 
  group_by(YEAR) %>% 
  summarise(WHOLE_POUNDS = sum(WHOLE_POUNDS)) %>%
  mutate(data_source = "Gross Earnings File") -> nsei_modern

rbind(nsei_historical,  nsei_modern) %>% 
  arrange(YEAR, data_source) %>%  
  mutate(thous_lbs = WHOLE_POUNDS/1000) -> nsei

ggplot(nsei) + 
  geom_line(aes(as.numeric(YEAR), thous_lbs, 
                colour = data_source, group = data_source), size = 1) +
  scale_x_continuous(breaks = c(seq(min(nsei$YEAR), max(nsei$YEAR), 5))) +
  labs(x = " ", y = "Round Weight (1000 lbs)", 
       title = "Harvest in NSEI (Chatham Strait)")

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
  filter(! c(length < 20 | length > 120)) %>% 
  write_csv("data/biological/fishery/detailed_fisherybio_nsei_ssei_1988_2017.csv") 

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
         Spp_cde = SPECIES_CODE, length = LENGTH, weight = WEIGHT, age = AGE, Sex, Maturity)  %>% 
  write_csv("data/biological/survey/detailed_potsrvbio_nsei_ssei_1979_2017.csv")

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
         Spp_cde = SPECIES_CODE, length = LENGTH, weight = WEIGHT, age = AGE, Sex, Maturity)  %>% 
  write_csv("data/biological/survey/detailed_llsrvbio_nsei_ssei_1985_2017.csv")

