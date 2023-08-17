#consolidate_confounders_wide_dhs.R
library(dplyr)

rm(list=ls())

#read only the dhs_id and the columns each file is "responsible" for
dhs_vector_df <- read.csv("./data/interim/dhs_treat_control_vector.csv") %>% 
  select(dhs_id, starts_with("log_"), starts_with("leader_"))

dhs_raster_df <- read.csv("./data/interim/dhs_treat_control_raster.csv") %>% 
  select(dhs_id, starts_with("log"))

dhs_natl_res_df <-  read.csv("./data/interim/dhs_natl_res.csv") %>% 
  select(dhs_id, starts_with("log"))

dhs_loan_transp_df <- read.csv("./data/interim/dhs_loan_projs.csv") %>% 
  select(dhs_id, starts_with("log"))

#get iwi estimate and all other attributes here
dhs_iwi_df <- read.csv("./data/interim/dhs_est_iwi.csv")

#join them all into a consolidated df
dhs_confounders_df <- dhs_iwi_df %>% 
  inner_join(dhs_vector_df, by="dhs_id") %>% 
  inner_join(dhs_raster_df, by="dhs_id") %>% 
  inner_join(dhs_natl_res_df, by="dhs_id") %>%  
  inner_join(dhs_loan_transp_df, by="dhs_id") 

write.csv(dhs_confounders_df,"./data/interim/dhs_confounders.csv",row.names=FALSE)



