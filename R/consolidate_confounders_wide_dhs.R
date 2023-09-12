#consolidate_confounders_wide_dhs.R
library(dplyr)
library(stringr)

rm(list=ls())

#read only the dhs_id and the columns each file is "responsible" for
dhs_vector_df <- read.csv("./data/interim/dhs_treat_control_vector.csv") %>% 
  select(dhs_id, starts_with("log_"), starts_with("leader_"))

#calculate annual agglomeration index here
dhs_raster_df <- read.csv("./data/interim/dhs_treat_control_raster.csv") %>%
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
dhs_iwi_df <- read.csv("./data/interim/dhs_est_iwi.csv")

################################
# Process per-capita nightlights 
################################
dhs_nl_df <- read.csv("./data/GEE/per_cap_nl_dhs_WorldPop.csv") %>%  
  #exclude three points with 0 pop_counts, so they don't show high per capita nightlights 
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

#since the data is highly right skewed, trim it at the 99th percentile
pc_nl_log_trim_df <- pc_nl_log_df 
for (year in 2000:2013) {
  #uncomment to test
  #year <- 2000
  
  log_col <- paste0("log_pc_nl_",year)
  q99 <- quantile(pc_nl_log_df[[log_col]],.99)
  
  pc_nl_log_trim_df <- pc_nl_log_trim_df %>% 
    mutate(!!sym(log_col) := ifelse(!!sym(log_col) > q99,q99,
                                    !!sym(log_col)))
}

#remove the variables used to construct this
rm(dhs_nl_df,pc_dhs_nl_df)

# pc_nl_log_df %>% 
#   summarise(across(starts_with("log_pc_nl"), ~ max(.)))
# pc_nl_log_trim_df %>% 
#   summarise(across(starts_with("log_pc_nl"), ~ max(.)))

############################################
# join all into a consolidated df
############################################
dhs_confounders_df <- dhs_iwi_df %>% 
  inner_join(dhs_vector_df, by="dhs_id") %>% 
  inner_join(dhs_raster_df, by="dhs_id") %>% 
  inner_join(dhs_natl_res_df, by="dhs_id") %>%  
  inner_join(dhs_loan_transp_df, by="dhs_id") %>% 
  inner_join(pc_nl_log_trim_df %>%  select(dhs_id, starts_with("log_")), by="dhs_id")
              

write.csv(dhs_confounders_df,"./data/interim/dhs_confounders.csv",row.names=FALSE)

############################################
# Descriptive stats for per capita nightlights
############################################
#plot the distribution of nightlights       
pc_nl_density <- pc_nl_log_df %>% 
  tidyr::pivot_longer(cols = starts_with("pc_nl_"), names_to = "pc_nl_year", values_to = "density") %>%
  ggplot(aes(density, color=pc_nl_year)) +
  geom_density() +
  labs(x="Nightlights per capita", y="Density across DHS clusters",
       title="Nightlights per capita across DHS Clusters",
       color="Years") +
  scale_color_discrete(labels = function(x) gsub(".*?_(\\d{4})$", "\\1", x)) +
  theme_bw()

pc_nl_density

ggsave("./figures/nl_pc_density.png",pc_nl_density, width=6, height = 4, dpi=300,
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
ggsave("./figures/nl_pc_log_density.png",log_pc_nl_density, width=6, height = 4, dpi=300,
       bg="white", units="in")

#density plot of trimmed log nightlights
log_pc_nl_trim_density <-  pc_nl_log_trim_df %>% 
  tidyr::pivot_longer(cols = starts_with("log_pc_nl_"), names_to = "log_pc_nl_year", values_to = "density") %>%
  ggplot(aes(density, color=log_pc_nl_year)) +
  geom_density() +
  labs(x="Nightlights per capita (log, trimmed at 99th percentile)", y="Density across DHS clusters",
       title="Nightlights per capita (log, trimmed at 99th percentile) across DHS clusters",
       color="Years") +
  scale_color_discrete(labels = function(x) gsub(".*?_(\\d{4})$", "\\1", x)) +
  theme_bw()

log_pc_nl_trim_density
ggsave("./figures/nl_pc_log_trim99_density.png",log_pc_nl_trim_density, width=6, height = 4, dpi=300,
       bg="white", units="in")

  
############################################
# Descriptive stats for agglomeration index
############################################
agglomeration_plot <- dhs_raster_df %>% 
  select(dhs_id, rural,starts_with("agglom")) %>% 
  pivot_longer(cols=starts_with("agglom"),names_to="Year",names_prefix = "agglom_",
               values_to = "Agglomeration") %>% 
ggplot(aes(factor(Agglomeration,labels=c("False","True")), fill = factor(rural))) +
  geom_bar(position = "fill") +
  labs(title = "Agglomeration versus DHS urban/rural labels for study locations",
       x = "Agglomeration Index",
       y = "Proportion",
       fill = "DHS Label") +
  scale_fill_manual(values=c("darkgrey","darkgreen"),
                    labels=c("Urban","Rural")) +
  facet_wrap(~Year, scales="free_x") +
  theme_bw()


ggsave("./figures/agglomeration.png",agglomeration_plot, width=6, height = 6, dpi=300,
       bg="white", units="in")

agglomeration_time_plot <- dhs_raster_df %>% 
  select(rural,starts_with("agglom")) %>% 
  pivot_longer(cols=starts_with("agglom"),names_to="Year",names_prefix = "agglom_",
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

ggsave("./figures/agglomeration_annual.png",agglomeration_time_plot, width=6, height = 6, dpi=300,
       bg="white", units="in")
