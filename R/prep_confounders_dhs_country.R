#prep_confounders_dhs_country.R
library(dplyr)
library(tidyr)
library(purrr)
#install.packages("countrycode")
library(countrycode)
library(ggplot2)
rm(list=ls())


################################################
### Load general data
################################################
#get Africa ISO codes
africa_isos_df <- read.csv("./data/interim/africa_isos.csv")

################################################
### Load DHS points
################################################
dhs_df <- read.csv("./data/interim/dhs_treat_control_5k_raster.csv") %>% 
  select(dhs_id, lat, lon, country, iso3)
  
################################################
### Load Polity V data
################################################
polity_df <- readxl::read_excel("./data/PolityV/p5v2018.xls") %>% 
  filter(year %in% 1999:2013) %>% 
  mutate(iso3=countrycode(sourcevar=country,origin="country.name",destination="iso3c")) %>% 
  filter(iso3 %in% africa_isos_df$iso3) %>% 
  select(country, iso3, year, polity2) 

#remove extra Sudan 2011 record that collides with south/north specific measures
polity_df <- polity_df %>% 
  filter(!(year==2011 & country=="Sudan-North")) 

#####################################################
### GDP per capita and GINI coefficient of inequality
#####################################################
wdi_df <- readxl::read_excel("./data/WorldDevelopmentIndicators/P_Data_Extract_From_World_Development_Indicators.xlsx",
                             .name_repair="universal") %>% 
  filter(Country.Code %in% africa_isos_df$iso3) %>% 
  rename(country="Country.Name",
         iso3="Country.Code")

#reshape to have year records for each variable
wdi_long_df <- wdi_df %>% 
  pivot_longer(cols=starts_with(".."), names_to="year", values_to="value",
               names_pattern="\\.{2}(\\d+)\\..+") %>% 
  mutate(year=as.integer(year),
         Series.Code=case_match(Series.Code,
                                "NY.GDP.PCAP.KD" ~ "gdp_per_cap_USD2015",
                                "SI.POV.GINI" ~ "country_gini"
                                )) %>% 
  select(-Series.Name) %>% 
  pivot_wider(names_from = Series.Code, values_from = value)
       

#exclude countries that don't have polity scores nor dhs points
wdi_long_df <-  wdi_long_df %>% 
  filter(!iso3 %in% c("STP", "SYC", "SSD")) 

#join the two datasets, using country name from wdi_long_df
country_confounder_df <- wdi_long_df %>% 
  left_join(polity_df[,!(names(polity_df) %in% "country")], by=join_by(iso3,year))

#exclude countries that don't have a country_gini measure nor dhs
country_confounder_df <- country_confounder_df %>% 
  filter(!iso3 %in% c("GNQ","ERI","LBY","SOM"))

#fill in missing year gini's with the most recent year's value
#or, for early years, set to first known value
country_confounder_gini_fill_df <- country_confounder_df %>%
  group_by(iso3) %>%
  mutate(
    country_gini = if_else(
      country_gini == ".." | is.na(country_gini),
      NA_character_, # Replace ".." or NA values with NA
      country_gini
    )
  ) %>%
  tidyr::fill(country_gini, .direction = "downup") %>%
  ungroup()


#fill in missing gdp_per_cap_USD2015 values using same approach
country_confounder_gdp_fill_df <- country_confounder_gini_fill_df %>%
  group_by(iso3) %>%
  mutate(
    gdp_per_cap_USD2015 = if_else(
      gdp_per_cap_USD2015 == ".." | is.na(gdp_per_cap_USD2015),
      NA_character_, # Replace ".." or NA values with NA
      gdp_per_cap_USD2015
    )
  ) %>%
  tidyr::fill(gdp_per_cap_USD2015, .direction = "downup") %>%
  ungroup()

#convert gdp and gini scores to numbers, log the gdp & scale gini
country_confounder_step1_df <- country_confounder_gdp_fill_df %>% 
  mutate(gdp_per_cap_USD2015 = as.double(gdp_per_cap_USD2015),
         country_gini = as.double(country_gini) / 100,
         log_gdp_per_cap_USD2015 = log(gdp_per_cap_USD2015 + 1)) 


#####################################################
### World Bank Governance indicators
#####################################################
sheets_v <- c("ControlofCorruption","GovernmentEffectiveness",
            "Political StabilityNoViolence","RegulatoryQuality",
            "RuleofLaw","VoiceandAccountability")

#read and process each sheet, store in output list
output <- lapply(sheets_v, function(one_sheet) {
  wgi_df <- readxl::read_excel("./data/WorldGovernanceIndicators/wgidataset.xlsx",
                              sheet=one_sheet,
                              skip=13,
                              col_names=FALSE)

  #column names are in the first two rows
  names(wgi_df) <- paste(wgi_df[2,], wgi_df[1, ],sep="_")
  wgi_df <- wgi_df[-c(1,2),]
  
  #select estimates, make a long format
  wgi_est_df <- wgi_df %>%
    rename(iso3=Code_NA) %>% 
    filter(iso3 %in% africa_isos_df$iso3) %>% 
    select(iso3,starts_with("Estimate")) %>% 
    pivot_longer(cols=starts_with("Estimate"),
                 names_to = "year",
                 names_prefix="Estimate_",
                 values_to=one_sheet) %>% 
    filter(year %in% 1999:2013)
  
  return(wgi_est_df)
})

#join the dataframes in the output into a single one
all_wgi_df <- output %>%
  reduce(function(x, y) left_join(x, y, by = c("iso3", "year"))) %>% 
  mutate(across(all_of(sheets_v),as.numeric),
         year=as.integer(year)) %>% 
  filter(complete.cases(.))  
#remove incomplete SSD, CPV, REU which are not in our sample anyway.

#generate and save density plot
wgi_density <- all_wgi_df %>%
  pivot_longer(all_of(sheets_v), names_to = "measure", values_to = "score") %>%
  mutate(measure=case_match(measure,
                            "ControlofCorruption" ~ "Control of Corruption",
                            "GovernmentEffectiveness" ~ "Government Effectiveness",
                            "Political StabilityNoViolence" ~ "Political Stability No Violence",
                            "RegulatoryQuality" ~ "Regulatory Quality",
                            "RuleofLaw" ~ "Rule of Law",
                            "VoiceandAccountability" ~ "Voice and Accountability")) %>% 
  ggplot(aes(score, color=as.factor(year))) +
  geom_density() +
  labs(x = "Score", y = "Density across countries",
       title="World Governance Indicators across African countries",color="Year")  +
  facet_wrap(~measure) +
  theme_bw()

ggsave("./figures/wgi_density.png", wgi_density, width = 7, height = 6, dpi = 300, bg = "white", units = "in")

#since there are no 2001 values, impute as average of 2000 and 2002
imputed_2001 <- all_wgi_df %>%
  filter(year %in% c(2000,2002)) %>% 
  group_by(iso3) %>% 
  summarize(across(all_of(sheets_v), ~ mean(., na.rm=TRUE))) %>% 
  mutate(year = as.integer(2001)) %>% 
  select(iso3, year,all_of(sheets_v))

#bind both, and rename columns
all_wgi_plus_imputed_df <- bind_rows(all_wgi_df,imputed_2001) %>% 
  #rename columns using my variable style
  rename(political_stability=`Political StabilityNoViolence`,
         corruption_control="ControlofCorruption",
         gov_effectiveness="GovernmentEffectiveness",
         reg_quality="RegulatoryQuality",
         rule_of_law="RuleofLaw",                   
         voice_accountability="VoiceandAccountability"   
  )

#join the governance indicators into country_confounder_complete_df
country_confounder_step2_df <- country_confounder_step1_df %>%
  left_join(all_wgi_plus_imputed_df,  by = c("iso3", "year"))

################################################################
### Election indicators from Database of Political Institutions
################################################################
dpi_df <- readxl::read_excel("./data/DPI/Database DPI2017/DPI2017_basefile_Jan2018.xlsx",
                             col_names=TRUE) %>% 
  select(countryname, year,yrcurnt) %>% 
  filter(year %in% 1999:2014) %>% 
  filter(countryname %in% africa_isos_df$name |
         countryname %in% c("Cent. Af. Rep.","Comoro Is.",
                            "Congo (DRC)","Cote d'Ivoire","Eq. Guinea",
                            "Swaziland","S. Africa","Tanzania")) %>% 
  #update countrynames to match my source
  mutate(countryname=case_match(countryname,
                               "Cent. Af. Rep." ~ "Central African Republic",
                               "Comoro Is." ~ "Comoros",
                               "Congo (DRC)" ~ "Congo, Democratic Republic of the",
                               "Cote d'Ivoire" ~ "Côte d'Ivoire",
                               "Eq. Guinea" ~ "Equatorial Guinea",
                               "Swaziland" ~ "Eswatini",
                               "S. Africa" ~ "South Africa",
                               "Tanzania" ~ "Tanzania, United Republic of",
                               .default=countryname)) %>% 
  #join to africa_isos_df to pick up iso codes
  left_join(africa_isos_df,by=join_by(countryname==name)) %>% 
  #remove South Sudan, which has incomplete data and isn't in my sample anyway
  filter(iso3 != "SSD") %>% 
  #add election year indicator, yrcurnt==0 during election years
  #set to non-election near if yrcurnt is NA or has -999
  mutate(election_year = if_else(is.na(yrcurnt),0,
                                 if_else(yrcurnt==0,1,0))) %>% 
  select(iso3,year,election_year)

#join to rest of confounders
country_confounder_step3_df <- country_confounder_step2_df %>%
  left_join(dpi_df,  by = c("iso3", "year"))

################################################################
### Temporary UN Security Council Membership
################################################################
unsc_df <- readxl::read_excel("./data/Dreher/UNSCdata.xls",
                              sheet="data",
                             col_names=TRUE) %>% 
  filter(year %in% 1999:2014 &
          code %in% africa_isos_df$iso3) %>% 
  mutate(unsc=as.integer(unsc)) %>% 
  select(code,year,unsc) %>% 
  rename(iso3=code)

################################################################
### Alignment with US in UNSC voting 
################################################################
unsc_us_align_df <- readxl::read_excel("./data/Dreher/UNSC_voting_update_Nov2022.xlsx",
                              sheet="data",
                              col_names=TRUE) 

#set column names to values in first row and then delete it, 
# which have iso3 codes instead of country names
names(unsc_us_align_df) <- as.character(unsc_us_align_df[1, ])
unsc_us_align_df <- unsc_us_align_df[-1,]

#create a df with UNSC temporary members and the years they served
unsc_temp_mems_df <- unsc_df %>% 
  filter(unsc==1) %>% 
  select(iso3,year) 

#make a long format only with rows and columns we need
unsc_us_align_mems_df <- unsc_us_align_df %>%  
  #limit votes to study years
  filter(Year %in% 1999:2014) %>%
  rename(US_vote = USA,
         year = Year) %>% 
  mutate(year = as.integer(year)) %>% 
  #limit to unsc temp members in Africa
  select(Res_count, year, US_vote, all_of(unsc_temp_mems_df$iso3)) %>% 
  pivot_longer(cols = !!unsc_temp_mems_df$iso3,
             names_to = "iso3",
             values_to = "vote") %>% 
  #keep only rows for years each country was on the security council
  semi_join(unsc_temp_mems_df, by=c("iso3","year")) %>% 
  mutate(aligned_US = if_else(vote==US_vote,1,0))

#summarize by country and year, with unsc_full_US_aligned = 2 if all votes match US, 
# 1 otherwise.  Will use 0 for country-years where country wasn't a member of UNSC.
unsc_us_full_align_df <- unsc_us_align_mems_df %>% 
  group_by(iso3,year) %>% 
  summarize(unsc_full_US_aligned=if_else(all(aligned_US==1),2,1),.groups="drop") %>% 
  select(iso3,year,unsc_full_US_aligned)

#join to rest of confounders
country_confounder_step4_df <- country_confounder_step3_df  %>%
  left_join(unsc_us_full_align_df, by = c("iso3", "year")) %>% 
  #set unsc_full_US_aligned to 0 (not member of UNSC) where it is NA
  mutate(unsc_full_US_aligned=if_else(is.na(unsc_full_US_aligned),0,
                                      unsc_full_US_aligned))

#####################################################
### Annual temperature and precipitation
#####################################################
#temperature
temp_df <- readxl::read_excel("./data/ClimateChangeKnowledgePortal/temperature.xlsx",
                             .name_repair="universal") %>% 
  rename(country=name,
         iso3=code) %>% 
  filter(iso3 %in% africa_isos_df$iso3) %>% 
  pivot_longer(cols=grep("^\\.\\.\\d{4}.07$", names(.),value=TRUE),
               names_to="year_long",values_to="temp_avg_c") %>% 
  mutate(year = as.integer(sub(".*(\\d{4}).*", "\\1", year_long))) %>% 
  filter(year %in% 1999:2014) %>% 
  select(iso3, year, temp_avg_c)


#join data to rest of confounders
country_confounder_step5_df <- country_confounder_step4_df  %>%
  left_join(temp_df, by = c("iso3", "year")) 

#precipitation
precip_df <- readxl::read_excel("./data/ClimateChangeKnowledgePortal/precipitation.xlsx",
                            .name_repair="universal") %>% 
  rename(country=name,
         iso3=code) %>% 
  filter(iso3 %in% africa_isos_df$iso3) %>% 
  pivot_longer(cols=grep("^\\.\\.\\d{4}.07$", names(.),value=TRUE),
               names_to="year_long",values_to="precip_avg_mm") %>% 
  mutate(year = as.integer(sub(".*(\\d{4}).*", "\\1", year_long))) %>% 
  filter(year %in% 1999:2014) %>% 
  select(iso3, year, precip_avg_mm)

country_confounder_complete_df <-  country_confounder_step5_df %>%
  left_join(precip_df, by = c("iso3", "year")) 

#####################################################
### Write file used later in the analysis
#####################################################
write.csv(country_confounder_complete_df,"./data/interim/country_confounders.csv",row.names=FALSE)  
#country_confounder_complete_df <- read.csv("./data/interim/country_confounders.csv")  
