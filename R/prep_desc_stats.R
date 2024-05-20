#prep_desc_stats.R
library(dplyr)
library(tidyr)

rm(list=ls())

#############################################################
#### Projects by funder
#############################################################
oda_sect_group_df <- read.csv("./data/interim/africa_oda_sector_group.csv") %>% 
  filter(transactions_start_year >= 2002 &
           transactions_start_year <= 2013)

donor_precision_count <- oda_sect_group_df %>%
  group_by(funder, precision_code) %>% 
  summarize(n = n_distinct(project_location_id)) %>%
  pivot_wider(names_from = funder, values_from = n, values_fill = 0) %>% 
  mutate(precision_code = paste("project_precision",precision_code)) %>% 
  rename(description = precision_code)

donor_vars_df <- oda_sect_group_df %>%
  select(funder, project_id, project_location_id, site_iso3, ad_sector_codes) %>%
  group_by(funder) %>%
  summarize(across(everything(), ~n_distinct(.))) %>%
  pivot_longer(cols = -funder, names_to = "description", values_to = "distinct_count") %>%
  pivot_wider(names_from = funder, values_from = distinct_count, values_fill = 0)

# donor_regional_unspecified_df <- oda_sect_group_df %>%
#   select(funder, project_location_id, recipients_iso3) %>%
#   group_by(funder) %>%
#   filter(grepl("regional|Unspecified",recipients_iso3)) %>% 
#   summarize(regional_count= n_distinct(project_location_id)) %>%
#   pivot_longer(cols = -funder, names_to = "description", values_to = "distinct_count") %>%
#   pivot_wider(names_from = funder, values_from = distinct_count, values_fill = 0)
#   
# donor_recipient_site_mismatch_df <- oda_sect_group_df %>%
#   select(funder, project_location_id, recipients_iso3, site_iso3) %>%
#   group_by(funder) %>%
#   filter(!grepl("regional|Unspecified",recipients_iso3) &
#          recipients_iso3 != site_iso3) %>% 
#   summarize(mismatch_count= n_distinct(project_location_id)) %>%
#   pivot_longer(cols = -funder, names_to = "description", values_to = "distinct_count") %>%
#   pivot_wider(names_from = funder, values_from = distinct_count, values_fill = 0)

no_end_date_df <- oda_sect_group_df %>%
  select(funder,end_actual_isodate) %>%
  group_by(funder) %>% 
  summarize(portion_no_end_date = mean(end_actual_isodate=="")) %>% 
  mutate(portion_no_end_date = round(portion_no_end_date,2)) %>% 
  pivot_longer(cols = -funder, names_to = "description", values_to = "distinct_count") %>%
  pivot_wider(names_from = funder, values_from = distinct_count, values_fill = 0)

no_funding_df <- oda_sect_group_df %>%
  select(funder,total_disbursements) %>%
  group_by(funder) %>% 
  summarize(portion_no_funding = mean(is.na(total_disbursements))) %>% 
  mutate(portion_no_funding = round(portion_no_funding,2)) %>% 
  pivot_longer(cols = -funder, names_to = "description", values_to = "distinct_count") %>%
  pivot_wider(names_from = funder, values_from = distinct_count, values_fill = 0)

multisector_df <- oda_sect_group_df %>%
  select(funder, geoname_id, transactions_start_year, ad_sector_codes) %>%
  group_by(funder, geoname_id, transactions_start_year, ad_sector_codes) %>% 
  count() %>% 
  group_by(funder) %>% 
  summarize(portion_multisector = mean(n > 1)) %>% 
  mutate(portion_multisector = round(portion_multisector,2)) %>% 
  pivot_longer(cols = -funder, names_to = "description", values_to = "distinct_count") %>%
  pivot_wider(names_from = funder, values_from = distinct_count, values_fill = 0)

desired_order <- c(3, 4, 1, 2, 5, 6, 7, 8, 9, 10)
donor_comparison_df <- rbind(donor_vars_df,donor_precision_count, no_end_date_df, 
                             no_funding_df, multisector_df) %>%
  slice(match(desired_order, row_number())) %>% 
  mutate(description = case_match(description,
                                "site_iso3" ~ "Countries hosting projects count",
                                "ad_sector_codes" ~ "Sectors funded",
                                "project_id" ~ "Aid project count",
                                "project_location_id" ~ "Aid project location count",
                                "project_precision 1" ~ "Exact locations available (precision 1)", 
                                "project_precision 2" ~ "Near (<25km) locations available (precision 2)", 
                                "project_precision 3" ~ "ADM2 locations available (precision 3)", 
                                "portion_no_end_date" ~ "Portion lacking end date",
                                "portion_no_funding" ~ "Portion lacking funding information",
                                "portion_multisector" ~ "Portion with concurrent, co-located, multi-sector projects",
                                .default = description))

# Table 1:  Funder Comparison: China and World Bank
# 1 Countries hosting projects count                             50      44   
# 2 Sectors funded                                               22      13   
# 3 Aid project count                                           722     513   
# 4 Aid project location count                                 1373    7115   
# 5 Exact locations available (precision 1)                     987    4149   
# 6 Near (<25km) locations available (precision 2)              169     258   
# 7 ADM2 locations available (precision 3)                      217    2708   
# 8 Portion lacking end date                                      0.68    0.15
# 9 Portion lacking funding information                           1       0.28
# 10 Portion with concurrent, co-located, multi-sector projects    0.03    0.02

#higher than Gehring et al, because they exclude countries with less than 1 million people

write.csv(donor_comparison_df,"./tables/funder_comparison.csv",row.names = FALSE)

#to do: make a sector level table similar to this, but divided by funders
#consider limiting to sectors actually included in analysis?
oda_sect_group_df %>% 
  filter(precision_code < 4) %>% 
  group_by(funder, ad_sector_codes, ad_sector_names) %>% 
  count()


#############################################################
#### DHS Units of Analysis 
#############################################################
#read file with all DHS points for one survey round per country
dhs_est_iwi_df <- read.csv("./data/interim/dhs_est_iwi.csv")

#read file that contains confounder data (lower n due to missing confounders)
dhs_confounders_df <- read.csv("./data/interim/dhs_5k_confounders.csv")

#identify locations excluded due to missing confounders, group by country
excluded_dhs_df <- anti_join(dhs_est_iwi_df,dhs_confounders_df,by="dhs_id") %>% 
  group_by(iso3) %>% count() %>% ungroup() %>% 
  rename(n_excluded=n)

#get formatted country names
africa_isos_df <- read.csv("./data/interim/africa_isos.csv")

#get treated information
dhs_t_df <- read.csv("./data/interim/dhs_treated_sector_3yr.csv") %>% 
  filter(year_group!="2014:2016") %>% 
  #exclude DHS points where confounder data not available 
  inner_join(dhs_confounders_df %>% 
               select(dhs_id, ID_adm2), by = join_by(dhs_id)) 

#identify locations that were never treated, group by country
dhs_never_treated_df <- anti_join(dhs_confounders_df,dhs_t_df,by="dhs_id") %>% 
  group_by(iso3) %>% count() %>% rename(n_never_treated=n) %>% ungroup()

#create display version of neighborhood descriptive stats
neighborhoods_df <- dhs_confounders_df %>% 
  select(iso3,survey_start_year,households,rural.x,dhs_id) %>% 
  group_by(iso3,survey_start_year) %>% 
  summarize(n_cluster_locations=n_distinct(dhs_id),
            n_households=sum(households),
            portion_rural=round(mean(rural.x),2)) %>% 
  ungroup() %>% 
  #join to get locations that were never treated
  left_join(dhs_never_treated_df, by="iso3") %>% 
  mutate(n_never_treated=ifelse(is.na(n_never_treated),0,n_never_treated)) %>%   
  mutate(portion_never_treated=round(n_never_treated/n_cluster_locations,2)) %>% 
  #join to get excluded count
  left_join(excluded_dhs_df,by="iso3") %>% 
  mutate(n_excluded=ifelse(is.na(n_excluded),0,n_excluded)) %>%  
  #join to get country names
  left_join(africa_isos_df %>% select(-iso2), by="iso3") %>% 
    rename(country=name) %>% 
    select(country,survey_start_year,n_cluster_locations,n_households,
           portion_rural,portion_never_treated,n_excluded) %>% 
  arrange(country)

# country                           survey_start_year n_cluster_l…¹ n_hou…² porti…³ porti…⁴ n_exc…⁵
# <chr>                                         <int>         <int>   <int>   <dbl>   <dbl>   <dbl>
#   1 Angola                                         2006            62    1377    0.35    0.4        0
# 2 Benin                                          1996           190    3152    0.53    0.03       0
# 3 Burkina Faso                                   1998            81     754    0.62    0.25       0
# 4 Burundi                                        2010           307    7032    0.88    0.49       0
# 5 Cameroon                                       2004           464    9254    0.48    0.22       0
# 6 Central African Republic                       1994            66    1142    0.65    0.86       0
# 7 Chad                                           2014           235    6499    0.59    0.26       0
# 8 Comoros                                        2012           242    4286    0.56    0          0
# 9 Congo, Democratic Republic of the              2007           286    7947    0.57    0          5
# 10 Côte d'Ivoire                                  1998            63     833    0.49    0.1        0
# 11 Egypt                                          1995             7     111    0.14    1          0
# 12 Eswatini                                       2006           210    3689    0.64    0.52       0
# 13 Ethiopia                                       2000           506    8623    0.73    0          0
# 14 Gabon                                          2012           332   11151    0.45    0.46       0
# 15 Ghana                                          1998           144    1373    0.65    0          0
# 16 Guinea                                         1999           290    3567    0.6     0.06       0
# 17 Kenya                                          2003           389    7640    0.67    0.45       0
# 18 Lesotho                                        2004           344    5350    0.72    0.1        0
# 19 Liberia                                        2009            30     855    0.1     0          0
# 20 Madagascar                                     1997           260    5610    0.59    0.02       0
# 21 Malawi                                         2000           554   14057    0.8     0.6        1
# 22 Mali                                           1995           167    2238    0.57    0.01       0
# 23 Morocco                                        2003           343    7530    0.42    0.95       1
# 24 Mozambique                                     2011           609   13303    0.58    0.12       0
# 25 Namibia                                        2000           258    6236    0.6     0.88       2
# 26 Niger                                          1998           184    2189    0.51    0.12       0
# 27 Nigeria                                        2003           356    5599    0.54    0.42       0
# 28 Rwanda                                         2005           447    8504    0.76    0          0
# 29 Senegal                                        1997           270    2518    0.63    0.04       0
# 30 Sierra Leone                                   2008           349    7138    0.59    0          0
# 31 South Africa                                   2016           745   11035    0.38    0.85       1
# 32 Tanzania, United Republic of                   1999           171    2831    0.66    0.05       0
# 33 Togo                                           1998           271    5332    0.52    0.03       0
# 34 Uganda                                         2000           137    2192    0.55    0.19       0
# 35 Zambia                                         2007           318    7552    0.64    0.08       1
# 36 Zimbabwe                                       1999           212    3469    0.64    0.5        0

write.csv(neighborhoods_df,"./tables/neighborhoods.csv",row.names = FALSE)

#get total counts for use in Pipeline Figure
total_dhs_n_df <- dhs_confounders_df %>% 
  select(iso3,survey_start_year,households,dhs_id) %>% 
  summarize(n_cluster_locations=n_distinct(dhs_id),
            n_households=sum(households),
            n_countries=n_distinct(iso3),
            n_start_year=n_distinct(survey_start_year)
  ) 
write.csv(total_dhs_n_df,"./tables/total_dhs_n.csv",row.names = FALSE)




  