# Coastwide Sablefish Project

Queries, data prep, and analysis for ADFG data requested by K. Fenske. 2017-12-26. Contact: Jane Sullivan (jane.sullivan1@alaska.gov)

All queries and data prep can be found in r/query_n_clean.r. 

## Catch by gear and ADFG management area, 1960-present, or other dates (going back in time) as available. 

Pers. comm. with K. Fenske 2018-01-03: she already has Aluetian Is. data from AKFIN and only needs state data for NSEI, SSEI, and PWS. 

I suggested using the Gross Earnings File (GEF) as the definitive source for statewide catch reconstructions based on the recommendation of J. Shriver. Column descriptions for GEF in data/catch/gross_earnings_file/GEF_Variable_Descriptions_Working_JShriver_2014_12_08.xlsx. Descriptions for ADFG management areas can be found in adfg_mgt_area_metadata_slarsen.shp. S. Larsen also helped me update some of the stat area NAs in Region 1 and 2, source file data/catch/gross_earnings_file/SJL_region2_statareas_districts

I supplied three data files for catch:

1.  data/catch/gross_earnings_file/regions1n2_statewater_sablecatch_gef_1975_2016.csv -- Just catch in statewaters in Regions 1 and 2, which include the follow management areas: Northern Southeast Inside (NSEI), Southern Southeast Inside (SSEI), Prince William Sound (PWS), and Cook Inlet (CI).

2.  data/catch/gross_earnings_file/allak_sablecatch_gef_1975_2016_CONFIDENTIAL.csv -- These are all records for sablefish from the GEF, in case Kari wants to use the GEF instead of a combination of AKFIN and GEF. 

3.  data/catch/nsei_historicalsablecatch_nosource_carlile_1907_2000.csv -- An interesting but undocumented time series of catch in Chatham Strait going back to 1907.

## Abundance index estimates (or absolute abundance) from survey, mark-recapture, or fishery data, by year, standardized if possible or noted as unstandardized if they aren’t. Biomass and abundance only available for Chatham Strait.

These estimates are only currently available for Chatham Strait (NSEI). I included abundance (age-2+), biomass (age-4+), and ABC/corresponding harvest policy when available. I used Franz's age-structured assessment to populate 1980-2002, but this model was not used for management. We are working on CPUE, and can provide those estimates at a later date.

## Natural mortality used or estimated. 

All recent assessments used for management have assumed M = 0.1. 

## Biological data: Maturation at age proportions, Age composition data for survey and/or fisheries (by survey/gear, if available), Length composition data for survey and/or fisheries (by survey/gear, if available), length and weight at age from survey(s) and fisheries

To my knowledge ADFG only has sablefish biological data for NSEI and SSEI. Code for biological summaries in r/biodata_analysis.r

Column name descriptions:
length: fork length, cm 
weight: kg 
Matuirty: "0" = immature, "1" = mature
Gear: indicates gear used (pot or longline)
Source: fishery or survey
Mgmt_area: NSEI is Chatham Strait, SSEI is Clarence Strait

Biological data are available for the following Gear/Source/Mgmt_area combinations as follows:

|      Gear|  Source| Mgmt_area| min_year| max_year|
|----------|--------|----------|---------|---------|
|  Longline| Fishery|      NSEI|     2000|     2017|
|  Longline| Fishery|      SSEI|     2001|     2017|
|  Longline|  Survey|      NSEI|     1985|     2017|
|  Longline|  Survey|      SSEI|     1986|     2017|
|       Pot| Fishery|      SSEI|     1998|     2017|
|       Pot|  Survey|      NSEI|     1981|     2017|
|       Pot|  Survey|      SSEI|     1979|     1983|

The requested summaries are available in data/biological: allsable_proportionmature.csv, allsable_agecomps.csv, allsable_lengthcomps.csv, allsable_lengthage.csv, and allsable_lengthweight.csv. Analysis was conducted at various levels of detail, specified by the "description" column. For example, I provided output for NSEI/SSEI separately and combined.

The detailed version of the fishery data are available in data/biological/fishery/detailed_fisherybio_nsei_ssei_1988_2017.csv.  The pot and longline survey data are available in data/biological/survey: detailed_potsrvbio_nsei_ssei_1979_2017.csv and detailed_llsrvbio_nsei_ssei_1985_2017.csv.

## Von Bertalanffy growth rate parameters

I provided parameter estimates for Chatham Strait in data/biological/compare_vonb_adfg_noaa.csv. This was from an earlier analysis, and the code is available at https://github.com/commfish/seak_sablefish/blob/master/r_code/biological.R 
