#consolidate_confounders_wide_dhs.R
library(dplyr)
library(stringr)

rm(list=ls())

#read only the dhs_id and the columns each file is "responsible" for
dhs_vector_df <- read.csv("./data/interim/dhs_treat_control_vector.csv") %>% 
  select(dhs_id, starts_with("log_"), starts_with("leader_"))

#calculate annual agglomeration index here
dhs_raster_df <- read.csv("./data/interim/dhs_treat_control_5k_raster.csv") %>%
  rowwise() %>%
  mutate(
    across(starts_with("avg_pop_dens_"),
           ~ ifelse(avg_min_to_city <= 60 && . >= 150, 1, 0),
           .names = "agglom_{str_extract(.col,'[0-9]{4}')}")
  ) %>%
  ungroup() %>% 
  select(dhs_id, rural, starts_with("log"), starts_with("agglom"))

dhs_natl_res_df <-  read.csv("./data/interim/dhs_natl_res.csv") %>% 
  select(dhs_id, starts_with("log"))

dhs_loan_transp_df <- read.csv("./data/interim/dhs_loan_projs.csv") %>% 
  select(dhs_id, starts_with("log"))

#get iwi estimate and all other attributes here
dhs_iwi_df <- read.csv("./data/interim/dhs_est_iwi.csv") %>% 
  select(-image_file)  #remove deprecated column to ensure not used downstream

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
  inner_join(dhs_loan_transp_df, by="dhs_id") %>% 
  inner_join(pc_nl_log_df %>%  select(dhs_id, starts_with("log_")), by="dhs_id")
              
#group them into 3-year sets matching 3-year-image years
dhs_5k_3yr_confounders <- dhs_confounders_df %>% 
  mutate(leader_1999_2001 = rowMeans(select(.,leader_1999,leader_2000,leader_2001), na.rm=TRUE),
         leader_2002_2004 = rowMeans(select(.,leader_2002,leader_2003,leader_2004), na.rm=TRUE),
         leader_2005_2007 = rowMeans(select(.,leader_2005,leader_2006,leader_2007), na.rm=TRUE),
         leader_2008_2010 = rowMeans(select(.,leader_2008,leader_2009,leader_2010), na.rm=TRUE),
         leader_2011_2013 = rowMeans(select(.,leader_2011,leader_2012,leader_2013), na.rm=TRUE),
         log_avg_pop_dens_2000_2001 = rowMeans(select(.,log_avg_pop_dens_2000,log_avg_pop_dens_2001), na.rm=TRUE),
         log_avg_pop_dens_2002_2004 = rowMeans(select(.,log_avg_pop_dens_2002,log_avg_pop_dens_2003,log_avg_pop_dens_2004), na.rm=TRUE),
         log_avg_pop_dens_2005_2007 = rowMeans(select(.,log_avg_pop_dens_2005,log_avg_pop_dens_2006,log_avg_pop_dens_2007), na.rm=TRUE),
         log_avg_pop_dens_2008_2010 = rowMeans(select(.,log_avg_pop_dens_2008,log_avg_pop_dens_2009,log_avg_pop_dens_2010), na.rm=TRUE),
         log_avg_pop_dens_2011_2013 = rowMeans(select(.,log_avg_pop_dens_2011,log_avg_pop_dens_2012,log_avg_pop_dens_2013), na.rm=TRUE),
         agglom_2000_2001 = rowMeans(select(.,agglom_2000,agglom_2001), na.rm=TRUE),
         agglom_2002_2004 = rowMeans(select(.,agglom_2002,agglom_2003,agglom_2004), na.rm=TRUE),
         agglom_2005_2007 = rowMeans(select(.,agglom_2005,agglom_2006,agglom_2007), na.rm=TRUE),
         agglom_2008_2010 = rowMeans(select(.,agglom_2008,agglom_2009,agglom_2010), na.rm=TRUE),
         agglom_2011_2013 = rowMeans(select(.,agglom_2011,agglom_2012,agglom_2013), na.rm=TRUE),         
         log_dist_km_to_petro_2002_2004 = rowMeans(select(.,log_dist_km_to_petro_2000_2002,
                                                          log_dist_km_to_petro_2003,
                                                          log_dist_km_to_petro_2003), na.rm=TRUE),  
         log_pc_nl_2000_2001 = rowMeans(select(.,log_pc_nl_2000,log_pc_nl_2001), na.rm=TRUE),
         log_pc_nl_2002_2004 = rowMeans(select(.,log_pc_nl_2002,log_pc_nl_2003,log_pc_nl_2004), na.rm=TRUE),
         log_pc_nl_2005_2007 = rowMeans(select(.,log_pc_nl_2005,log_pc_nl_2006,log_pc_nl_2007), na.rm=TRUE),
         log_pc_nl_2008_2010 = rowMeans(select(.,log_pc_nl_2008,log_pc_nl_2009,log_pc_nl_2010), na.rm=TRUE),
         log_pc_nl_2011_2013 = rowMeans(select(.,log_pc_nl_2011,log_pc_nl_2012,log_pc_nl_2013), na.rm=TRUE))

write.csv(dhs_5k_3yr_confounders,"./data/interim/dhs_5k_confounders.csv",row.names=FALSE)

names(dhs_5k_3yr_confounders)

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


############################################
# Descriptive stats for agglomeration index
############################################
agglomeration_plot <- dhs_raster_df %>% 
  select(dhs_id, rural,starts_with("agglom")) %>% 
  tidyr::pivot_longer(cols=starts_with("agglom"),names_to="Year",names_prefix = "agglom_",
               values_to = "Agglomeration") %>% 
ggplot(aes(factor(Agglomeration,labels=c("False","True")), fill = factor(rural))) +
  geom_bar(position = "fill") +
  labs(title = "Agglomeration versus DHS urban/rural labels for DHS locations (5km square)",
       x = "Agglomeration Index",
       y = "Proportion",
       fill = "DHS Label") +
  scale_fill_manual(values=c("darkgrey","darkgreen"),
                    labels=c("Urban","Rural")) +
  facet_wrap(~Year, scales="free_x") +
  theme_bw()


ggsave("./figures/agglomeration_5k.png",agglomeration_plot, width=6, height = 6, dpi=300,
       bg="white", units="in")

agglomeration_time_plot <- dhs_raster_df %>% 
  select(rural,starts_with("agglom")) %>% 
  tidyr::pivot_longer(cols=starts_with("agglom"),names_to="Year",names_prefix = "agglom_",
               values_to = "Agglomeration") %>% 
  group_by(Year) %>% 
  summarize(proportion=mean(Agglomeration)) %>% 
  ggplot(aes(x=Year,y=proportion)) +
  geom_point() +
  ylim(0,.25) +
  labs(title = "Agglomeration of DHS locations over time",
       x = "Year",
       y = "% Agglomerations") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Rotate x-axis labels by 45 degrees

ggsave("./figures/agglomeration_5k_annual.png",agglomeration_time_plot, width=6, height = 6, dpi=300,
       bg="white", units="in")
