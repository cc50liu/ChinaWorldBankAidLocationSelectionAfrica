#prep_confounders_dhs_country.R
library(dplyr)
library(tidyr)
library(purrr)
#install.packages("countrycode")
library(countrycode)
rm(list=ls())


################################################
### Load general data
################################################
#get Africa ISO codes
africa_isos_df <- read.csv("./data/interim/africa_isos.csv")

################################################
### Load DHS points
################################################
dhs_df <- read.csv("./data/interim/dhs_treat_control_raster.csv") %>% 
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
#using the same iso code, record 7 below
polity_df %>% 
  filter(year %in% 2011:2013 & iso3 %in% c("SDN","SSD"))
# 1 Sudan-North SDN    2011      -4
# 2 Sudan-North SDN    2012      -4
# 3 Sudan-North SDN    2013      -4
# 4 South Sudan SSD    2011       0
# 5 South Sudan SSD    2012       0
# 6 South Sudan SSD    2013       0
# 7 Sudan       SDN    2011      -2

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

#check results
# country_confounder_gini_fill_df %>% 
#   filter(iso3=="ZMB")
# 
# country_confounder_df %>% 
#   filter(iso3=="ZMB")


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

#verify results
# country_confounder_gini_fill_df %>%
#   filter(iso3=="DJI")
# 
# country_confounder_gdp_fill_df %>%
#   filter(iso3=="DJI")

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
country_confounder_complete_df <- country_confounder_step1_df %>%
  left_join(all_wgi_plus_imputed_df,  by = c("iso3", "year"))

#####################################################
### Write file used later in the analysis
#####################################################
write.csv(country_confounder_complete_df,"./data/interim/country_confounders.csv",row.names=FALSE)  
