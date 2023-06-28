#chart_treatment_assignment.R
library(dplyr)
library(ggplot2)
library(tidyr)

rm(list=ls())
names(dhs_df)
dhs_vector_df <-  read.csv("./data/interim/dhs_treat_control_vector.csv") %>% 
  select(dhs_id,log_avg_nl_1996_1998, log_avg_min_to_city, log_avg_pop_dens_2000, log_deaths1995_1999, leader_birthplace)

dhs_df <- read.csv("./data/interim/dhs_treat_control_sector_year.csv") %>% 
  left_join(dhs_vector_df, by="dhs_id")

write.csv(dhs_df,"./data/interim/dhs_treat_control_sector_vector_year.csv",row.names = FALSE)

names(dhs_df)

sector_names_df <- read.csv("./data/interim/sector_group_names.csv") %>% 
  mutate(sec_pre_name = paste0(ad_sector_names," (",sector_group,")"))


#identify columns that hold treatment/control variables
selected_columns <- grep("^(wb|ch)_\\d+$", names(dhs_df), value = TRUE)

# Convert to long format for charts
long_data <- dhs_df %>%
  gather(variable_name, value, log_avg_nl_1996_1998, log_avg_min_to_city, log_avg_pop_dens_2000, log_deaths1995_1999, leader_birthplace)
  
for (treat_control_variable in selected_columns) {
  #treat_control_variable <- selected_columns[1]
  sector <- unique(sub(".*_(\\d+).*", "\\1", treat_control_variable))
  sector_name <- sector_names_df %>% 
    filter(ad_sector_codes==sector) %>% 
    pull(sec_pre_name)
  
  combined_boxplot <- ggplot(long_data, aes(x = factor(.data[[treat_control_variable]]), y = value)) +
    geom_boxplot() +
    labs(title = paste(treat_control_variable, "Confounder Distribution over Treated and Control DHS Points"),
         subtitle = sector_name,
         x = "Treatment/Control: -1:other funder, 0:control, 1:treated (this funder), 2:treated (both funders)",
         y = "Value") +
    facet_wrap(~ variable_name, scales = "free") +
    theme_void()
  
  combined_boxplot
  
  ggsave(paste0("./figures/",treat_control_variable,"_confound_boxplots.png"),
         combined_boxplot,
         width=8, height = 6, dpi=300,
         bg="white", units="in")
  
}


dhs_df %>% 
  group_by(ch_130) %>% 
  count()
