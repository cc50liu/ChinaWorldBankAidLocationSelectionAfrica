#chart_treatment_assignment.R
library(dplyr)
library(ggplot2)
library(tidyr)

rm(list=ls())

dhs_df <-  read.csv("./data/interim/dhs_treat_control_vector.csv") 

#identify columns that hold treatment/control variables
selected_columns <- grep("^(wb|ch)_\\d+_p\\d+$", names(dhs_df), value = TRUE)

# Convert to long format for charts
long_data <- dhs_df %>%
  gather(variable_name, value, log_avg_nl_1996_1998, log_avg_min_to_city, log_avg_pop_dens_2000, log_deaths1995_1999, leader_birthplace)
  
for (treat_control_variable in selected_columns) {
  #treat_control_variable <- selected_columns[1]
  combined_boxplot <- ggplot(long_data, aes(x = factor(.data[[treat_control_variable]]), y = value)) +
    geom_boxplot() +
    labs(title = paste(treat_control_variable, "Confounder Distribution over Treated and Control DHS Points"), x = "Treatment/Control") +
    facet_wrap(~ variable_name, scales = "free")
  
  combined_boxplot
  
  ggsave(paste0("./figures/",treat_control_variable,"_confound_boxplots.png"),
         combined_boxplot,
         width=8, height = 6, dpi=300,
         bg="white", units="in")
  
}

