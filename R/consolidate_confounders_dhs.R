#consolidate_confounders_dhs.R
library(dplyr)

rm(list=ls())

#read only the dhs_id and the columns each file is "responsible" for
dhs_vector_df <- read.csv("./data/interim/dhs_treat_control_vector.csv") %>% 
  select(dhs_id, log_deaths1995_1999, leader_birthplace)

dhs_raster_df <- read.csv("./data/interim/dhs_treat_control_raster.csv") %>% 
  select(dhs_id, starts_with("log"))

#join the two
dhs_intermediate_df <- inner_join(dhs_vector_df, dhs_raster_df, by="dhs_id")

#get all columns here, which include iwi estimates, t/c and min_oda_year by sector
dhs_treat_control_df <- read.csv("./data/interim/dhs_treat_control_sector_year.csv") 

#join the two for the final consolidated file, n=9601
dhs_confounders_df <- inner_join(dhs_intermediate_df, dhs_treat_control_df, by="dhs_id")

write.csv(dhs_confounders_df,"./data/interim/dhs_treat_control_confounders.csv",row.names=FALSE)



