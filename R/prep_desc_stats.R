#prep_desc_stats.R
library(dplyr)
library(tidyr)

rm(list=ls())

#############################################################
#### Projects by funder
#oda_sect_group_df <- read.csv("./data/interim/africa_oda_sector_group.csv")
oda_sect_group_df <- read.csv("./data/interim/africa_oda_sector_group_v2.csv")


excluded_precision_count <- oda_sect_group_df %>%
  filter(precision_code >= 4) %>% 
  group_by(funder) %>% 
  summarize(n = n_distinct(project_location_id)) %>%
  pivot_wider(names_from = funder, values_from = n, values_fill = 0) %>% 
  mutate(description = "project_precision >=4")

donor_precision_count <- oda_sect_group_df %>%
  filter(precision_code < 4) %>% 
  group_by(funder, precision_code) %>% 
  summarize(n = n_distinct(project_location_id)) %>%
  pivot_wider(names_from = funder, values_from = n, values_fill = 0) %>% 
  mutate(precision_code = paste("project_precision",precision_code)) %>% 
  rename(description = precision_code)

donor_vars_df <- oda_sect_group_df %>%
  filter(precision_code < 4) %>% 
  select(funder, project_id, project_location_id, site_iso3, ad_sector_codes) %>%
  group_by(funder) %>%
  summarize(across(everything(), ~n_distinct(.))) %>%
  pivot_longer(cols = -funder, names_to = "description", values_to = "distinct_count") %>%
  pivot_wider(names_from = funder, values_from = distinct_count, values_fill = 0)

oda_sect_group_df %>% 
  filter(precision_code < 4) %>% 
  filter(funder=="CH") %>% 
  distinct(ad_sector_codes) %>% 
  arrange(ad_sector_codes)

donor_regional_unspecified_df <- oda_sect_group_df %>%
  filter(precision_code < 4) %>% 
  select(funder, project_location_id, recipients_iso3) %>%
  group_by(funder) %>%
  filter(grepl("regional|Unspecified",recipients_iso3)) %>% 
  summarize(regional_count= n_distinct(project_location_id)) %>%
  pivot_longer(cols = -funder, names_to = "description", values_to = "distinct_count") %>%
  pivot_wider(names_from = funder, values_from = distinct_count, values_fill = 0)
  

donor_recipient_site_mismatch_df <- oda_sect_group_df %>%
  filter(precision_code < 4) %>% 
  select(funder, project_location_id, recipients_iso3, site_iso3) %>%
  group_by(funder) %>%
  filter(!grepl("regional|Unspecified",recipients_iso3) &
         recipients_iso3 != site_iso3) %>% 
  summarize(mismatch_count= n_distinct(project_location_id)) %>%
  pivot_longer(cols = -funder, names_to = "description", values_to = "distinct_count") %>%
  pivot_wider(names_from = funder, values_from = distinct_count, values_fill = 0)


no_end_date_df <- oda_sect_group_df %>%
  filter(precision_code < 4) %>% 
  select(funder,end_actual_isodate) %>%
  group_by(funder) %>% 
  summarize(portion_no_end_date = mean(end_actual_isodate=="")) %>% 
  mutate(portion_no_end_date = round(portion_no_end_date,2)) %>% 
  pivot_longer(cols = -funder, names_to = "description", values_to = "distinct_count") %>%
  pivot_wider(names_from = funder, values_from = distinct_count, values_fill = 0)

# description            CH    WB
# <chr>               <dbl> <dbl>
#   1 portion_no_end_date  0.69  0.17

desired_order <- c(3, 4, 1, 2, 8, 9, 5, 6, 7, 10,11)
donor_comparison_df <- rbind(donor_vars_df,donor_precision_count,donor_regional_unspecified_df,
                             donor_recipient_site_mismatch_df,excluded_precision_count,
                             no_end_date_df) %>%
  slice(match(desired_order, row_number())) %>% 
  mutate(description = case_match(description,
                                "site_iso3" ~ "Countries hosting projects count",
                                "ad_sector_codes" ~ "Sectors funded",
                                "project_id" ~ "Aid project count",
                                "project_location_id" ~ "Aid project location count",
                                "regional_count" ~ "Locations labeled `Regional` or `Unspecified` country",
                                "mismatch_count" ~ "Locations where recipient and site country differ",
                                "project_precision 1" ~ "Exact locations available (precision 1)", 
                                "project_precision 2" ~ "Near (<25km) locations available (precision 2)", 
                                "project_precision 3" ~ "ADM2 locations available (precision 3)", 
                                "project_precision >=4" ~ "Excluded Less precise locations (precision 4-8)",
                                "portion_no_end_date" ~ "Portion lacking end date (precision <=3)",
                                .default = description))

# Table 1:  Funder Comparison: China and World Bank
# description                                              CH    WB
# <chr>                                                   <dbl>   <dbl>
# 1 Countries hosting projects count                        50      44   
# 2 Sectors funded                                          22      13   
# 3 Aid project count                                      819     594   
# 4 Aid project location count                            1529    8248   
# 5 Locations labeled `Regional` or `Unspecified` country   71     644   
# 6 Locations where recipient and site country differ       23       1   
# 7 Exact locations available (precision 1)               1088    4722   
# 8 Near (<25km) locations available (precision 2)         189     288   
# 9 ADM2 locations available (precision 3)                 252    3238   
# 10 Excluded Less precise locations (precision 4-8)        291    3662   
# 11 Portion lacking end date (precision <=3)                0.69    0.17

#higher than Gehring et al, because they exclude countries with less than 1 million people

write.csv(donor_comparison_df,"./tables/funder_comparison.csv",row.names = FALSE)

#to do: make a sector level table similar to this, but divided by funders
#consider limiting to sectors actually included in analysis?
oda_sect_group_df %>% 
  filter(precision_code < 4) %>% 
  group_by(funder, ad_sector_codes, ad_sector_names) %>% 
  count()


#############################################################
#### Treatments and controls by sector
dhs_df <- read.csv("./data/interim/dhs_treat_control_confounders.csv")
names(dhs_df)

communities_df <- dhs_df %>% 
  mutate(never_treated = if_all(all_of(sector_t_c), ~ .==0)) %>% 
  select(dhs_id,sector_t_c, country, never_treated,survey_start_year,rural,households) %>% 
  group_by(country, survey_start_year) %>% 
  summarize(
    clusters = n(),
    perc_rural = round(mean(rural),2)*100,
    mean_hhlds = round(mean(households),2),
    sd_hhlds = round(sd(households),2),
    never_treated_count = sum(never_treated),
    ever_treated_count = sum(!never_treated),
    perc_never_treated=round(never_treated_count/(ever_treated_count + never_treated_count),2)*100
  ) %>% 
  mutate(country = gsub("_"," ",country),
         country = stringr::str_to_title(country)) %>% 
  select(-never_treated_count, -ever_treated_count)

# country                      survey_start_year clusters perc_rural mean_hhlds sd_hhlds perc_neve…¹
# <chr>                                    <int>    <int>      <dbl>      <dbl>    <dbl>       <dbl>
#   1 Angola                                    2006       62         35      22.2      3.57          40
# 2 Benin                                     1996      190         53      16.6     10.5            3
# 3 Burkina Faso                              1998       81         62       9.31     8.24          25
# 4 Burundi                                   2010      307         88      22.9      1.25          41
# 5 Cameroon                                  2004      464         48      19.9      9.05           9
# 6 Central African Republic                  1994       66         65      17.3      9.74          86
# 7 Chad                                      2014      235         59      27.7      4.41          20
# 8 Comoros                                   2012      242         56      17.7      1.96           0
# 9 Democratic Republic Of Congo              2007      286         57      27.8      4.81           0
# 10Egypt                                     1995        7         14      15.9      7.54         100

# %>% 
#   rename("Survey Year" = survey_start_year,
#          "Cluster Locations"=clusters,
#          "Rural %"=perc_rural,
#          "Households (mean)"=mean_hhlds,
#          "Households (sd)"=sd_hhlds)

write.csv(communities_df,"./tables/communities.csv",row.names = FALSE)

sector_t_c <- names(dhs_df)[grep("^(?:wb|ch)_\\d{3}$",names(dhs_df))]
            
#get treatment and control counts by sector            
sector_treat_control_df <- dhs_df %>% 
  select(dhs_id,sector_t_c) %>% 
  pivot_longer(cols = sector_t_c,names_to="original_col_name") %>% 
  separate_wider_regex(cols=original_col_name,patterns=c(funder="^(?:wb|ch)_",
                                            sector="\\d{3}")) %>% 
  group_by(funder, sector, value) %>% 
  count() %>% 
  arrange(sector) %>% 
  mutate(funder=ifelse(value %in% c(0,2),"both",stringr::str_remove(funder,"_$"))) %>% 
  filter(value!=-1) %>%  #used to keep track of other funder
  unique() %>%  #remove duplicate "both" rows 
  mutate(situation=case_when(
    (funder=="both" & value==0) ~ "control_n",
    (funder=="ch" & value==1) ~ "ch_treat_n",
    (funder=="wb" & value==1) ~ "wb_treat_n",
    (funder=="both" & value==2) ~ "both_treat_n"
  )) %>% 
  select(-funder,-value) %>% 
  pivot_wider(id_cols=sector,names_from = situation,values_from=n, values_fill = 0) %>% 
  ungroup()

sector_min_years <- names(dhs_df)[grep("^(?:wb|ch)_\\d{3}_min_oda_year",names(dhs_df))]

#get the earliest project year by sector      
sector_min_years_df <- dhs_df %>% 
  summarize(across(all_of(sector_min_years), ~ min(.,na.rm=TRUE))) %>% 
  mutate(across(everything(), ~ replace(., is.infinite(.), NA))) %>% 
  pivot_longer(everything()) %>% 
  separate_wider_regex(cols=name,patterns=c(funder="^(?:wb|ch)_",
                                            sector="\\d{3}",
                                            "_min_oda_year")) %>% 
  pivot_wider(names_from=funder, values_from=value,names_prefix="first_oda_year_") %>% 
  rename(wb_first_oda_year = first_oda_year_wb_, 
         ch_first_oda_year = first_oda_year_ch_ )


#get sector names
sector_names_df <- read.csv("./data/interim/sector_group_names.csv") %>% 
  mutate(sector_name = paste0(ad_sector_names," (",ad_sector_codes,")")) %>% 
  select(ad_sector_codes, sector_name) %>% 
  mutate(ad_sector_codes = as.character(ad_sector_codes))


#join the three
sector_stats_df <-  sector_treat_control_df %>% 
  left_join(sector_min_years_df,by="sector") %>% 
  left_join(sector_names_df,join_by(sector==ad_sector_codes)) %>% 
  select(sector_name, ch_treat_n, ch_first_oda_year, wb_treat_n,wb_first_oda_year,both_treat_n,control_n,-sector)

write.csv(sector_stats_df,"./tables/sector_treat_control.csv",row.names = FALSE)

# sector_name                                        ch_tr…¹ ch_fi…² wb_tr…³ wb_fi…⁴ both_…⁵ contr…⁶
# <chr>                                                <int>   <dbl>   <int>   <dbl>   <int>   <int>
#   1 Education (110)                                        318    2001    2477    2001     603    6506
# 2 Health (120)                                           613    2001    2506    2001    1013    5772
# 3 Population Policies / Programmes and Reproductive…      44    2003       0      NA       0    9860
# 4 Water Supply and Sanitation (140)                      217    2002    4243    2001     216    5228
# 5 Government and Civil Society (150)                     136    2001    4782    2001    1051    3935
# 6 Other Social Infrastructure and Services (160)         188    2001    3890    2001     478    5348
# 7 Transport and Storage (210)                            223    2001    4612    2001     785    4284
# 8 Communications (220)                                   431    2001    1460    2001     205    7808
# 9 Energy Generation and Supply (230)                     279    2002    2679    2001     283    6663
# 10Banking and Financial Services (240)                     0      NA    1562    2001       0    8342
# 



  