# call_ridge_one_sector.R
#
# Read input files for sector_funder CNN analysiss and run ridge regression.  
# Use to predict ATE to use as comparison for ATE with images.
# Bootstrap 100 times and use the standard deviation as estimated standard errors.
#
library(dplyr)
library(glmnet)
library(data.table)

rm(list=ls())
setwd("/mimer/NOBACKUP/groups/globalpoverty1/cindy/eoml_ch_wb")

setwd("/mimer/NOBACKUP/groups/globalpoverty1/cindy/eoml_ch_wb")
args <- commandArgs(trailingOnly = TRUE)

#####################################################################
#function to estimate ATE and standard error with ridge regression
#####################################################################
est_ate_with_se_ridge <- function(X, obsW, obsY, nBoot = 100) { 
  ate_vec <- c(); 
  for (i in 1L:(nBoot+1L)) {
    if(nBoot > 0L){ print(paste0("Bootstrap iteration ",i-1L," of ",nBoot) ) } 
    
    # shuffle indices for training
    if(i != (nBoot+1L)){ boot_indices <- sample(1:length( obsY ), length( obsY ), replace = T) }
    # use input order for last iteration, whose results we'll return as ate estimate
    if(i == (nBoot+1L)){ boot_indices <- 1:length( obsY ) }
    
    #estimate ridge model then use to predict estimated treatment probs
    ridge_model <- glmnet::cv.glmnet(
      x = as.matrix(X[boot_indices,]),
      y = as.matrix(obsW[boot_indices]),
      nfolds = 5,
      alpha = 0, # alpha = 0 is the ridge penalty
      type.measure = "auc", 
      family = "binomial")
    obs_treated <- obsW[boot_indices] 
    obs_outcome <- obsY[boot_indices]
    est_pr_treated <- predict(ridge_model, s = "lambda.min",
                        newx = as.matrix(X[boot_indices,]), type = "response")
    
    # compute ate
    ate_vec[i] <- ate <- sum(  obs_outcome*prop.table(obs_treated/c(est_pr_treated))) -
      sum(obsY*prop.table((1-obs_treated)/c(1-est_pr_treated) ))
    
  } #end of for loop
  
  return(list(
    "ate" = ate_vec[length(ate_vec)],
    "ate_se" = sd(ate_vec[-length(ate_vec)])
  ) )
} #end of est_ate_with_se_ridge


#####################################################################
#control starts here
#####################################################################
# The command line argument should be funder_sector (like both_110, wb_110, ch_110)
fund_sect_param <- args[1]
#remove comment to test
#fund_sect_param <- "ch_700"
sector <- sub(".*_(\\d+).*", "\\1", fund_sect_param)
funder <- sub("(wb|ch).*", "\\1", fund_sect_param)

#read funder_sector input file from the cnn run 
input_df <-  read.csv(paste0("./data/interim/input_cnn_5k_3yr_",fund_sect_param,".csv")

#create covariate matrix with adm2 fixed effects
conf_matrix <- scale(cbind(
as.matrix(data.frame(
  "first_year_group"           =input_df$first_year_group - 2001,
  "log_pc_nl_pre_oda"          =input_df$log_pc_nl_pre_oda,           #scene level
  "log_avg_min_to_city"        =input_df$log_avg_min_to_city,         #scene level
  "log_avg_pop_dens"           =input_df$log_avg_pop_dens,            #scene level
  "log_dist_km_to_gold"        =input_df$log_dist_km_to_gold,         #scene level
  "log_dist_km_to_gems"        =input_df$log_dist_km_to_gems,         #scene level
  "log_dist_km_to_dia"         =input_df$log_dist_km_to_dia,          #scene level
  "log_dist_km_to_petro"       =input_df$log_dist_km_to_petro,        #scene level
  "log_treated_other_funder_n" =input_df$log_treated_other_funder_n,  #inherited from ADM2
  "log_ch_loan_proj_n"         =input_df$log_ch_loan_proj_n,          #inherited from ADM1, ADM2
  "log_other_sect_n"           =input_df$log_other_sect_n,            #inherited from ADM2
  "log_3yr_pre_conflict_deaths"=input_df$log_3yr_pre_conflict_deaths, #inherited from ADM1
  "log_disasters"              =input_df$log_disasters,               #inherited from ADM1,2,or3
  "leader_birthplace"          =input_df$leader_birthplace,           #inherited from ADM1
  "election_year"              =input_df$election_year,               #country level
  "unsc_aligned_us"            =input_df$unsc_aligned_us,             #country level
  "unsc_non_aligned_us"        =input_df$unsc_non_aligned_us,         #country level
  "country_gini"               =input_df$country_gini,                #country level
  "corruption_control"         =input_df$corruption_control,          #country level
  "gov_effectiveness"          =input_df$gov_effectiveness,           #country level
  "political_stability"        =input_df$political_stability,         #country level
  "reg_quality"                =input_df$reg_quality,                 #country level
  "rule_of_law"                =input_df$rule_of_law,                 #country level
  "voice_accountability"       =input_df$voice_accountability,        #country level
  "landsat578"                 =input_df$landsat578,                  #pre-treat image
  "log_total_neighbor_projs"   =input_df$log_total_neighbor_projs     #neighbor ADM2s
)),
#multiple columns for adm2 fixed effects variables
model.matrix(~ adm2 - 1, input_df)
))

#remove any columns that have 0 standard deviation before passing to function
conf_matrix <- conf_matrix[,which(apply(conf_matrix,2,sd)>0)] 

#returns list of ate and se
print(paste0("[",format(Sys.time(), "%Y-%m-%d %H:%M:%S"),"]",
		   " Processing funder: ", funder, " sector: ",sector))
output <- est_ate_with_se_ridge(X=conf_matrix, 
					obsW=input_df$treated, 
					obsY=input_df$iwi_est_post_oda)  

print(paste0("funder: ", funder, " sector: ",sector, " output=",output ))
write.csv(as.data.frame("funder: ", funder, " sector: ",sector,output),
          paste0("./results/ridge_only_",funder,"_",sector,".csv"), row.names=FALSE) 

