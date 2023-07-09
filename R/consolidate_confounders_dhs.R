#consolidate_confounders_dhs.R
library(dplyr)

rm(list=ls())

#read only the dhs_id and the columns each file is "responsible" for
dhs_vector_df <- read.csv("./data/interim/dhs_treat_control_vector.csv") %>% 
  select(dhs_id, starts_with("log_"), starts_with("leader_"))

dhs_raster_df <- read.csv("./data/interim/dhs_treat_control_raster.csv") %>% 
  select(dhs_id, starts_with("log"))

dhs_natl_res_df <-  read.csv("./data/interim/dhs_natl_res.csv") 

dhs_loan_transp_df <- read.csv("./data/interim/dhs_loan_projs.csv") %>% 
  select(dhs_id, starts_with("log"))

#get all columns here, which include iwi estimates, t/c and min_oda_year by sector
dhs_treat_control_df <- read.csv("./data/interim/dhs_treat_control_sector_year.csv") 

#join them all into a consolidated df
dhs_confounders_df <- dhs_treat_control_df %>% 
  inner_join(dhs_vector_df, by="dhs_id") %>% 
  inner_join(dhs_raster_df, by="dhs_id") %>% 
  inner_join(dhs_natl_res_df, by="dhs_id") %>%  
  inner_join(dhs_loan_transp_df, by="dhs_id") 

write.csv(dhs_confounders_df,"./data/interim/dhs_treat_control_confounders.csv",row.names=FALSE)



