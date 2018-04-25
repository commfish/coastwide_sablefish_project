# Coastwide Sablefish Project

Queries, data prep, and analysis for ADFG data requested by K. Fenske. 2017-12-26. Contact: Jane Sullivan (jane.sullivan1@alaska.gov)

All queries and data prep can be found in r/query_n_clean.r. 

## Catch by gear and ADFG management area, 1960-present, or other dates (going back in time) as available. 

**Update 2018-04-24: I've struggled getting the IFDB database and Gross Earnings File to match up to agency reports. I've uploaded a new harvest file called data/catch/landings_nseisseicombined_1980_2016_USEME.csv. These numbers come straight from ADFG Annual Harvest Objective Memos. Because they're the only values that have been published, I want to use them until I can thoroughly review these other data sources. They represent reported landings in round lbs in the NSEI from 1980-2016 and SSEI from 1985-2016. I left 1907-1979 with Dave Carlile's historical catch reconstruction, but these numbers are for NSEI only.**

Pers. comm. with K. Fenske 2018-01-03: she already has Aluetian Is. data from AKFIN and only needs state data for NSEI, SSEI, and PWS. 

Reserve for later use:

I suggested using the Gross Earnings File (GEF) as the definitive source for statewide catch reconstructions based on the recommendation of J. Shriver. Column descriptions for GEF in data/catch/gross_earnings_file/GEF_Variable_Descriptions_Working_JShriver_2014_12_08.xlsx. Descriptions for ADFG management areas can be found in adfg_mgt_area_metadata_slarsen.shp. S. Larsen also helped me update some of the stat area NAs in Region 1 and 2, source file data/catch/gross_earnings_file/SJL_region2_statareas_districts

I supplied three data files for catch:

1.  data/catch/gross_earnings_file/regions1n2_statewater_sablecatch_gef_1975_2016.csv -- Just catch in statewaters in Regions 1 and 2, which include the follow management areas: Northern Southeast Inside (NSEI), Southern Southeast Inside (SSEI), Prince William Sound (PWS), and Cook Inlet (CI).

2.  data/catch/gross_earnings_file/allak_sablecatch_gef_1975_2016_CONFIDENTIAL.csv -- These are all records for sablefish from the GEF, in case Kari wants to use the GEF instead of a combination of AKFIN and GEF. 

3.  data/catch/nsei_historicalsablecatch_nosource_carlile_1907_2000.csv -- An interesting but undocumented time series of catch in Chatham Strait going back to 1907.

## Abundance index estimates (or absolute abundance) from survey, mark-recapture, or fishery data, by year, standardized if possible or noted as unstandardized if they aren’t. Biomass and abundance only available for Chatham Strait.

These estimates are only currently available for Chatham Strait (NSEI). I included abundance (age-2+), biomass (age-4+), and ABC/corresponding harvest policy when available. I used Franz's age-structured assessment to populate 1980-2002, but this model was not used for management. 

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

## Effort in the Longline Survey and Fishery

Results for the following analyses can be found in data/effort/all_cpue_indices.csv. Effort data are available for the following Source/Mgmt_area combinations as follows:

|      Gear|  Source| Mgmt_area| min_year| max_year|
|----------|--------|----------|---------|---------|
|  Longline|  Survey|      NSEI|    1985*|     2017|         
|  Longline|  Survey|      SSEI|    1988*|     2017|
|  Longline| Fishery|      NSEI|     1997|     2017|
|  Longline| Fishery|      SSEI|     1997|     2017|

* Soak time in the surveys was 1 hour prior to 1997, and 3 hours in 1997 to present. According to Carlile et al. (2002), 1-hour soak time CPUEs were about 43% lower than those associated with 3-hour soak times (Page 10 http://www.sf.adfg.state.ak.us/fedaidpdfs/RIR.1J.2002.02.pdf). One could correct pre-1997 survey data by dividing by 0.43. If absolutely necessary, I suggest using a higher CV pre-1997, or omit these years entirely. Experiments (Sigler 1993, UW PhD dissertation) and summaries (Sigler 2000, Tech Report NMFS 130) indicate that initial capture rates vary without trend in the first hour and thus appear to have no relationship with density of sablefish. Consequently, I chose not to include pre-1997 longline survey effort indices, although the raw data are available in data/effort/llsrv_cpue_nsei_ssei_raw.csv if desired.

Hook spacing in the fishery and survey was standardized following Sigler and Lunsford (2001, CJFAS).

We use numbers per unit effort (NPUE; number per hook) as the measure of effort in the longline survey. Note that the CV reported is actually the relative standard error (RSE), such that

$$CV=RSE=(s/\sqrt{n})/\bar{x}.$$

**NOT COMPLETED** - Let me know if and when you want to do this.

The relative population number (RPN) in year $y$ is given by

$$RPN_{y}=\sum_{s=1}^S{NPUE_{s,y}*Area_{s,y}},$$

where the product of NPUE in each depth stratum $s$ and the physical area (km^2) of each stratum summed across all strata in the management area. Depth strata (from NMFS): 3 = 201-300 m; 4 = 301-400 m; 5 =401-600 m; 6 =601-800 m; and 7 =801-1,000 m (Sigler 2000).

We use weight per unit effort (WPUE; round lbs per hook) as the measure of effort in the ADFG longline fishery in Chatham and Clarence Straits (these are all averaged across all depths). The relative population weight (RPW) is given by the same equation as RPN, subsituting WPUE for NPUE. Fishery CPUE information was collected through skipper interview and voluntary logbook programs prior to 1997 and through mandatory logbook program beginning in 1997 (Carlile et al. 2002). I include the "official" legacy WPUE values for your reference, but I don't have any methods to offer for them. Similar to the survey NPUE, I report the RSE as the CV for 1997 to present. For legacy WPUE values, I set the CV equal to 0.05 in the Google Drive (not in the code though).