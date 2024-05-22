#consolidate_confounders_wide_dhs.R
library(dplyr)
library(stringr)

rm(list=ls())

#read only the dhs_id and the columns each file is "responsible" for
dhs_vector_df <- read.csv("./data/interim/dhs_treat_control_vector.csv") %>% 
  select(dhs_id, starts_with("log_"), starts_with("leader_"), starts_with("disasters"))

#calculate annual agglomeration index here
dhs_raster_df <- read.csv("./data/interim/dhs_treat_control_5k_raster.csv") %>%
  rowwise() %>%
  mutate(
    across(starts_with("avg_pop_dens_"),
           ~ ifelse(avg_min_to_city <= 60 && . >= 150, 1, 0),
           .names = "agglom_{str_extract(.col,'[0-9]{4}')}")
  ) %>%
  ungroup() %>% 
  select(dhs_id, rural, starts_with("log"), starts_with("agglom"), starts_with("avg_pop_dens_"))

dhs_natl_res_df <-  read.csv("./data/interim/dhs_natl_res.csv") %>% 
  select(dhs_id, starts_with("log"), starts_with("dist_km_to_petro"))

dhs_ch_loan_df <- read.csv("./data/interim/dhs_loan_projs.csv") %>% 
  select(dhs_id, starts_with("ch_loan_proj"), starts_with("log"))

#get iwi estimate and all other attributes here
dhs_iwi_df <- read.csv("./data/interim/dhs_est_iwi.csv") 

################################
# Process per-capita nightlights 
################################
dhs_nl_df <- read.csv("./data/GEE/per_cap_nl_harmonized_5k_WorldPop.csv") %>%  
  #exclude locations with 0 pop_counts, so they don't show high per capita nightlights 
  filter(!if_any(starts_with("pop_count"), ~ . == 0))

#initialize pc_dhs_nl_df and then loop over it, calculating per capita nightlights each year
pc_dhs_nl_df <- dhs_nl_df
for (year in 2000:2013) {
  pc_dhs_nl_df <- pc_dhs_nl_df %>% 
    mutate(!!paste0("pc_nl_",year) := get(paste0("nl",year)) / get(paste0("pop_count",year)))
}

#create logged variables
pc_nl_log_df <- pc_dhs_nl_df %>% 
  mutate(
    across(starts_with("pc_nl_"), ~ log(. + 1), .names = "log_{.col}")
  )

#remove the variables used to construct this
rm(dhs_nl_df,pc_dhs_nl_df)

# pc_nl_log_df %>%
#   summarise(across(starts_with("log_pc_nl"), ~ max(.)))


############################################
# join all into a consolidated df
############################################
dhs_confounders_df <- dhs_iwi_df %>% 
  inner_join(dhs_vector_df, by="dhs_id") %>% 
  inner_join(dhs_raster_df, by="dhs_id") %>% 
  inner_join(dhs_natl_res_df, by="dhs_id") %>%  
  inner_join(dhs_ch_loan_df, by="dhs_id") %>% 
  inner_join(pc_nl_log_df %>%  select(dhs_id, starts_with("log_"), starts_with("pc_nl_")), by="dhs_id")

#group them into 3-year sets matching 3-year-image years
dhs_5k_3yr_confounders <- dhs_confounders_df %>% 
  mutate(leader_1999_2001 = if_else((leader_1999==1 | leader_2000==1 |leader_2001==1),1,0),
         leader_2002_2004 = if_else((leader_2002==1 | leader_2003==1 |leader_2004==1),1,0),
         leader_2005_2007 = if_else((leader_2005==1 | leader_2006==1 |leader_2007==1),1,0),
         leader_2008_2010 = if_else((leader_2008==1 | leader_2009==1 |leader_2010==1),1,0),
         leader_2011_2013 = if_else((leader_2011==1 | leader_2012==1 |leader_2013==1),1,0),
         #new petroleum deposits discovered in 2000 and 2003, take weighted averages of distances
         #for year ranges that span the discoveries
         log_dist_km_to_petro_1999_2001 = log(1 + ((dist_km_to_petro_1999 +         #1999
                                                    dist_km_to_petro_1999 +         #2000
                                                    dist_km_to_petro_2000_2002)/3)),#2001
         log_dist_km_to_petro_2002_2004 = log(1 + ((dist_km_to_petro_2000_2002 +    #2002
                                                    dist_km_to_petro_2000_2002 +    #2003
                                                    dist_km_to_petro_2003)/3)),     #2004
         log_dist_km_to_petro_2005_2007 = log_dist_km_to_petro_2003,
         log_dist_km_to_petro_2008_2010 = log_dist_km_to_petro_2003,
         log_dist_km_to_petro_2011_2013 = log_dist_km_to_petro_2003,
         log_avg_pop_dens_2000_2001 = log(1 + rowMeans(select(.,avg_pop_dens_2000,avg_pop_dens_2001), na.rm=TRUE)),
         log_avg_pop_dens_2002_2004 = log(1 + rowMeans(select(.,avg_pop_dens_2002,avg_pop_dens_2003,avg_pop_dens_2004), na.rm=TRUE)),
         log_avg_pop_dens_2005_2007 = log(1 + rowMeans(select(.,avg_pop_dens_2005,avg_pop_dens_2006,avg_pop_dens_2007), na.rm=TRUE)),
         log_avg_pop_dens_2008_2010 = log(1 + rowMeans(select(.,avg_pop_dens_2008,avg_pop_dens_2009,avg_pop_dens_2010), na.rm=TRUE)),
         log_avg_pop_dens_2011_2013 = log(1 + rowMeans(select(.,avg_pop_dens_2011,avg_pop_dens_2012,avg_pop_dens_2013), na.rm=TRUE)),
         agglom_2000_2001 = if_else((agglom_2000==1 | agglom_2001==1),1,0),
         agglom_2002_2004 = if_else((agglom_2002==1 | agglom_2003==1 | agglom_2004==1),1,0),
         agglom_2005_2007 = if_else((agglom_2005==1 | agglom_2006==1 | agglom_2007==1),1,0),
         agglom_2008_2010 = if_else((agglom_2008==1 | agglom_2009==1 | agglom_2010==1),1,0),
         agglom_2011_2013 = if_else((agglom_2011==1 | agglom_2012==1 | agglom_2013==1),1,0),
         log_pc_nl_2000_2001 = log(1 + rowMeans(select(.,pc_nl_2000,pc_nl_2001), na.rm=TRUE)),
         log_pc_nl_2002_2004 = log(1 + rowMeans(select(.,pc_nl_2002,pc_nl_2003,pc_nl_2004), na.rm=TRUE)),
         log_pc_nl_2005_2007 = log(1 + rowMeans(select(.,pc_nl_2005,pc_nl_2006,pc_nl_2007), na.rm=TRUE)),
         log_pc_nl_2008_2010 = log(1 + rowMeans(select(.,pc_nl_2008,pc_nl_2009,pc_nl_2010), na.rm=TRUE)),
         log_pc_nl_2011_2013 = log(1 + rowMeans(select(.,pc_nl_2011,pc_nl_2012,pc_nl_2013), na.rm=TRUE)),
         #sum first, then take the log
         log_ch_loan_proj_n_1999_2001 = log(rowSums(select(.,ch_loan_proj_n_1999,ch_loan_proj_n_2000,ch_loan_proj_n_2001), na.rm=TRUE) + .01),
         log_ch_loan_proj_n_2002_2004 = log(rowSums(select(.,ch_loan_proj_n_2002,ch_loan_proj_n_2003,ch_loan_proj_n_2004), na.rm=TRUE) + .01),
         log_ch_loan_proj_n_2005_2007 = log(rowSums(select(.,ch_loan_proj_n_2005,ch_loan_proj_n_2006,ch_loan_proj_n_2007), na.rm=TRUE) + .01),
         log_ch_loan_proj_n_2008_2010 = log(rowSums(select(.,ch_loan_proj_n_2008,ch_loan_proj_n_2009,ch_loan_proj_n_2010), na.rm=TRUE) + .01),
         log_ch_loan_proj_n_2011_2013 = log(rowSums(select(.,ch_loan_proj_n_2011,ch_loan_proj_n_2012,ch_loan_proj_n_2013), na.rm=TRUE) + .01),
         log_ch_loan_proj_n_2014_2016 = log_ch_loan_proj_n_2014,
         #sum first, then take the log
         log_disasters1999_2001 = log(rowSums(select(.,disasters1999,disasters2000,disasters2001), na.rm=TRUE) + .01),
         log_disasters2002_2004 = log(rowSums(select(.,disasters2002,disasters2003,disasters2004), na.rm=TRUE) + .01),
         log_disasters2005_2007 = log(rowSums(select(.,disasters2005,disasters2006,disasters2007), na.rm=TRUE) + .01),
         log_disasters2008_2010 = log(rowSums(select(.,disasters2008,disasters2009,disasters2010), na.rm=TRUE) + .01),
         log_disasters2011_2013 = log(rowSums(select(.,disasters2011,disasters2012,disasters2013), na.rm=TRUE) + .01),
         log_disasters2014 = log_disasters2014)

write.csv(dhs_5k_3yr_confounders,"./data/interim/dhs_5k_confounders.csv",row.names=FALSE)
#dhs_5k_3yr_confounders  <- read.csv("./data/interim/dhs_5k_confounders.csv")

############################################
# Descriptive stats for per capita nightlights
############################################
library(ggplot2)
#plot the distribution of nightlights       
pc_nl_density <- pc_nl_log_df %>% 
  tidyr::pivot_longer(cols = starts_with("pc_nl_"), names_to = "pc_nl_year", values_to = "density") %>%
  ggplot(aes(density, color=pc_nl_year)) +
  geom_density() +
  labs(x="Nightlights per capita", y="Density across DHS clusters",
       title="Nightlights per capita across DHS Clusters (5km square)",
       color="Years") +
  scale_color_discrete(labels = function(x) gsub(".*?_(\\d{4})$", "\\1", x)) +
  theme_bw()

pc_nl_density

ggsave("./figures/nl_pc_density_5k.png",pc_nl_density, width=6, height = 4, dpi=300,
       bg="white", units="in")

#density plot of log nightlights
log_pc_nl_density <-  pc_nl_log_df %>% 
  tidyr::pivot_longer(cols = starts_with("log_pc_nl_"), names_to = "log_pc_nl_year", values_to = "density") %>%
  ggplot(aes(density, color=log_pc_nl_year)) +
  geom_density() +
  labs(x="Nightlights per capita (log)", y="Density across DHS clusters",
       title="Nightlights per capita (log) across DHS clusters",
       color="Years") +
  scale_color_discrete(labels = function(x) gsub(".*?_(\\d{4})$", "\\1", x)) +
  theme_bw()

log_pc_nl_density
ggsave("./figures/nl_pc_log_density_5k.png",log_pc_nl_density, width=6, height = 4, dpi=300,
       bg="white", units="in")
