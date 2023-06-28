#prep_desc_stats.R
library(dplyr)
library(tidyr)

rm(list=ls())

oda_sect_group_df <- read.csv("./data/interim/africa_oda_sector_group.csv")

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

donor_regional_unspecified_df <- oda_sect_group_df %>%
  select(funder, project_location_id, recipients_iso3) %>%
  group_by(funder) %>%
  filter(grepl("regional|Unspecified",recipients_iso3)) %>% 
  summarize(regional_count= n_distinct(project_location_id)) %>%
  pivot_longer(cols = -funder, names_to = "description", values_to = "distinct_count") %>%
  pivot_wider(names_from = funder, values_from = distinct_count, values_fill = 0)
  

donor_recipient_site_mismatch_df <- oda_sect_group_df %>%
  select(funder, project_location_id, recipients_iso3, site_iso3) %>%
  group_by(funder) %>%
  filter(!grepl("regional|Unspecified",recipients_iso3) &
         recipients_iso3 != site_iso3) %>% 
  summarize(mismatch_count= n_distinct(project_location_id)) %>%
  pivot_longer(cols = -funder, names_to = "description", values_to = "distinct_count") %>%
  pivot_wider(names_from = funder, values_from = distinct_count, values_fill = 0)



desired_order <- c(3, 4, 1, 2, 9, 10, 5, 6, 7, 8)
donor_comparison_df <- rbind(donor_vars_df,donor_precision_count,donor_regional_unspecified_df,
                             donor_recipient_site_mismatch_df) %>%
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
                                "project_precision 4" ~ "ADM1 locations available (precision 4)",
                                .default = description))

# description                                              CH    WB
# <chr>                                                 <int> <int>
# 1 Countries hosting projects count                         49    44
# 2 Sectors funded                                           22    14
# 3 Aid project count                                       890   786
# 4 Aid project location count                             1839 11273
# 5 Locations labeled `Regional` or `Unspecified` country    71   749
# 6 Locations where recipient and site country differ        24     1
# 7 Exact locations available (precision 1)                1117  5106
# 8 Near (<25km) locations available (precision 2)          192   296
# 9 ADM2 locations available (precision 3)                  255  3462
# 10 ADM1 locations available (precision 4)                  275  2409

#higher than Gehring et al, because they exclude countries with less than 1 million people


#to do: make a sector level table similar to this, but divided by funders
oda_sect_group_df %>% 
  group_by(funder, ad_sector_codes, ad_sector_names) %>% 
  count()

#descriptive stats about treatments and controls by sector
dhs_df <- read.csv("./data/interim/dhs_treat_control_sector_vector_year.csv")
names(dhs_df)


columns_of_interest <- c("rural", "iwi_2017_2019_est", "wb_110", "wb_110_min_oda_year",
                         "log_avg_nl_1996_1998", "log_avg_min_to_city",
                         "log_avg_pop_dens_2000", "log_deaths1995_1999",
                         "leader_birthplace")

treat_control_stats <- dhs_df %>%
  group_by(country, year) %>%
  summarise(across(all_of(columns_of_interest), list(mean, sd, min, max))) %>% 
  rename_with(~ gsub("_1$", "_mean", .), ends_with("_1")) %>%
  rename_with(~ gsub("_2$", "_sd", .), ends_with("_2")) %>%
  rename_with(~ gsub("_3$", "_min", .), ends_with("_3")) %>%
  rename_with(~ gsub("_4$", "_max", .), ends_with("_4")) 

treat_control_stats
write.csv(treat_control_stats,"./data/interim/treat_control_stats.csv",row.names = FALSE)
