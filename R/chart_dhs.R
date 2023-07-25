################################################################################
# chart_dhs.R:  charts and descriptive stats on dhs points and their intersection
# with projects by sector 
################################################################################
library(dplyr)
library(ggplot2)

rm(list=ls())

#read file that contains sector-level treatment and control variables
dhs_df <- read.csv("./data/interim/dhs_treat_control_confounders.csv")

names(dhs_df)

# Create a dataframe containing the name and average of the treatment/control
# binary variables
selected_columns <- grep("^(wb|ch)_\\d+_p\\d+$", names(dhs_df), value = TRUE)
mean_values <- colMeans(dhs_df[selected_columns])
mean_df <- data.frame(FunderSectorPrecision = selected_columns, 
                      MeanValue = mean_values)

#modify it so we can sort by Funder Sector Precision and interleave projects by funders
mean_df <- mean_df %>%
  mutate(SortValue = sub("^(wb|ch)_(.*)", "\\2", FunderSectorPrecision),
         FunderSectorPrecision = factor(FunderSectorPrecision, 
                    levels = unique(FunderSectorPrecision[order(SortValue,decreasing=TRUE)])))
#Portion treated for Funder-Sector-Precision
dhs_sector_treated_portion <- mean_df %>% 
  filter(mean_values > 0) %>% 
  ggplot(aes(x = MeanValue, y = FunderSectorPrecision )) +
  geom_bar(stat = "identity", aes(fill = grepl("^ch_", FunderSectorPrecision))) +
  labs(x = "Portion treated", y = "Funder-Sector-Precision", fill="Funder",
       title="Portion of DHS points treated, by aid funder, sector, and precision") +
  scale_fill_manual(values = c("blue", "red"),
                    breaks = c(FALSE, TRUE),
                    labels = c("World Bank", "China")) +
  theme_bw()

ggsave("./figures/dhs_sector_treated_portion.png",dhs_sector_treated_portion, 
       width=8, height = 6, dpi=300,
       bg="white", units="in")


# calculate the mean nightlight, travel time, conflict deaths, and leader birthplaces
# for treated and controls
desc_stats <- data.frame(matrix(NA,nrow=1,ncol=9))
colnames(desc_stats) <- c('ColName','Mean_NL','SD_NL','mean_trvl','sd_trvl',
                          'mean_deaths','sd_deaths','mean_ldrbrth','sd_ldrbrth')

mean_sd_list <- list()
for (col in selected_columns) {
  for (i in 0:1) {
    colnm <- paste0(col, "_", i)
    subset_data <- subset(dhs_df, subset = (dhs_df[[col]] == i),
                          select=c(avg_nl_1996_1998,avg_min_to_city,
                                   deaths1995_1999,leader_birthplace))
    mean_nl   <- mean(as.numeric(subset_data$avg_nl_1996_1998), na.rm = TRUE)
    sd_nl     <- sd(as.numeric(subset_data$avg_nl_1996_1998), na.rm = TRUE)    
    mean_trvl <- mean(as.numeric(subset_data$avg_min_to_city), na.rm = TRUE)
    sd_trvl   <- sd(as.numeric(subset_data$avg_min_to_city), na.rm = TRUE)  
    mean_deaths <- mean(as.numeric(subset_data$deaths1995_1999), na.rm = TRUE)
    sd_deaths   <- sd(as.numeric(subset_data$deaths1995_1999), na.rm = TRUE)  
    mean_ldrbrth <- mean(as.numeric(subset_data$leader_birthplace), na.rm = TRUE)
    sd_ldrbrth   <- sd(as.numeric(subset_data$leader_birthplace), na.rm = TRUE)    
   
names(dhs_df)    
     
    desc_stats[1,1] <- colnm
    desc_stats[1,2] <- mean_nl
    desc_stats[1,3] <- sd_nl
    desc_stats[1,4] <- mean_trvl
    desc_stats[1,5] <- sd_trvl
    desc_stats[1,6] <- mean_deaths
    desc_stats[1,7] <- sd_deaths
    desc_stats[1,8] <- mean_ldrbrth
    desc_stats[1,9] <- sd_ldrbrth
    
    mean_sd_list[[colnm]] <- desc_stats
    }
}
combined_mean_sd_df <- bind_rows(mean_sd_list)

library(tidyverse)

data_plot <- combined_mean_sd_df %>%
  mutate(sector_precision = gsub("^.*_(\\d+_p\\d)_.*$", "\\1", ColName),
         funder = gsub("^(.*?)_.*$", "\\1", ColName),
         sector = as.integer(gsub("^.*_(\\d+)_.*$", "\\1", ColName)),
         precision = as.integer(gsub("^.*_p(\\d).*$", "\\1", ColName)),
         treated = as.integer(gsub("^.*_(\\d)$", "\\1", ColName)),
         funder_treated = paste0(funder,"_", treated)
         # ,
         # Mean_NL = Mean_NL,
         # SD_NL = SD_NL,
         # mean_trvl = mean_trvl,
         # sd_trvl = sd_trvl
         ) %>%
  select(sector_precision, funder, sector, precision, treated, funder_treated, 
         Mean_NL, SD_NL,mean_trvl,sd_trvl,mean_deaths,sd_deaths,mean_ldrbrth,
         sd_ldrbrth) %>% 
  na.omit()

#get sector names and group for labels
sector_names_df <- read.csv("./data/interim/sector_group_names.csv")
data_plot_complete <- data_plot %>% 
    mutate(sector = as.integer(sector)) %>% 
    left_join(sector_names_df,by=join_by(sector==ad_sector_codes)) %>% 
    mutate(sec_pre_name = paste0(sector_precision," ",
                                 substr(ad_sector_names,1,30),
                                 " (",sector_group,")"))

#Nightlight Figure
pre_nl_ct_sector <- ggplot(data_plot_complete, aes(y = reorder(sec_pre_name, -Mean_NL), x = Mean_NL)) +
  geom_errorbarh(aes(xmin = pmax(Mean_NL - SD_NL,0), xmax = Mean_NL + SD_NL, color = funder_treated),
                 height = 0.1, 
                 position = position_nudge(y = ifelse(data_plot_complete$funder_treated == "wb_0", 0.1,
                                                      ifelse(data_plot_complete$funder_treated == "wb_1", 0.2,
                                                             ifelse(data_plot_complete$funder_treated == "ch_0", 0.3,
                                                                    ifelse(data_plot_complete$funder_treated == "ch_1", 0.4, 0.4)))))) +  
  geom_jitter(aes(shape = factor(treated), color = funder_treated), 
              position = position_nudge(y = ifelse(data_plot_complete$funder_treated == "wb_0", 0.1,
                                            ifelse(data_plot_complete$funder_treated == "wb_1", 0.2,
                                            ifelse(data_plot_complete$funder_treated == "ch_0", 0.3,
                                            ifelse(data_plot_complete$funder_treated == "ch_1", 0.4, 0.4)))))) +
  labs(y = "Sector_ProjectPrecision SectorName (Group)", x = "Nighlights 1996-1998", 
       title = "Average Pre-Treatment Nightlight for Treated and Control DHS points", 
       subtitle = "By Sector and Project Precision, lines +/- one standard deviation",
       color = "Funder & Treated") +
  scale_color_manual(values = c("wb_0" = "lightblue", "wb_1" = "blue", "ch_0" = "pink", "ch_1" = "red"),
                     breaks = c("wb_0", "wb_1", "ch_0", "ch_1"),
                     labels = c("WB Control", "WB Treated", "CH Control", "CH Treated")) +
  guides(color = guide_legend(override.aes = list(shape = c(21, 24)),
                               title = "Funder & Treated")) +
  theme_bw()

pre_nl_ct_sector

ggsave("./figures/pre_nl_ct_sector_sd.png",pre_nl_ct_sector, 
       width=8, height = 8, dpi=300,
       bg="white", units="in")

#Travel Time Figure
pre_trvl_tc_sector <- ggplot(data_plot_complete, aes(y = reorder(sec_pre_name, -mean_trvl), x = mean_trvl)) +
  geom_errorbarh(aes(xmin = pmax(mean_trvl - sd_trvl,0), xmax = mean_trvl + sd_trvl, color = funder_treated),
                 height = 0.1, 
                 position = position_nudge(y = ifelse(data_plot_complete$funder_treated == "wb_0", 0.1,
                                               ifelse(data_plot_complete$funder_treated == "wb_1", 0.2,
                                               ifelse(data_plot_complete$funder_treated == "ch_0", 0.3,
                                               ifelse(data_plot_complete$funder_treated == "ch_1", 0.4, 0.4)))))) +  
  geom_jitter(aes(shape = factor(treated), color = funder_treated), 
              position = position_nudge(y = ifelse(data_plot_complete$funder_treated == "wb_0", 0.1,
                                            ifelse(data_plot_complete$funder_treated == "wb_1", 0.2,
                                            ifelse(data_plot_complete$funder_treated == "ch_0", 0.3,
                                            ifelse(data_plot_complete$funder_treated == "ch_1", 0.4, 0.4)))))) +
  labs(y = "Sector_ProjectPrecision SectorName (Group)", x = "Minutes travel to >50K city in 2000", 
       title = "Average Travel Minutes to >50K City for Treated and Control DHS points", 
       subtitle = "By Sector and Project Precision, lines +/- one standard deviation",
       color = "Funder & Treated") +
  scale_color_manual(values = c("wb_0" = "lightblue", "wb_1" = "blue", "ch_0" = "pink", "ch_1" = "red"),
                     breaks = c("wb_0", "wb_1", "ch_0", "ch_1"),
                     labels = c("WB Control", "WB Treated", "CH Control", "CH Treated")) +
  guides(color = guide_legend(override.aes = list(shape = c(21, 24)),
                              title = "Funder & Treated"))

pre_trvl_tc_sector

ggsave("./figures/pre_trvl_tc_sector.png",pre_trvl_tc_sector, 
       width=8, height = 8, dpi=300,
       bg="white", units="in")

#Battle-related deaths Figure
pre_deaths_tc_sector <- ggplot(data_plot_complete, aes(y = reorder(sec_pre_name, -mean_deaths), x = mean_deaths)) +
  geom_errorbarh(aes(xmin = pmax(mean_deaths - sd_deaths,0), xmax = mean_deaths + sd_deaths, color = funder_treated),
                 height = 0.1, 
                 position = position_nudge(y = ifelse(data_plot_complete$funder_treated == "wb_0", 0.1,
                                                      ifelse(data_plot_complete$funder_treated == "wb_1", 0.2,
                                                             ifelse(data_plot_complete$funder_treated == "ch_0", 0.3,
                                                                    ifelse(data_plot_complete$funder_treated == "ch_1", 0.4, 0.4)))))) +  
  geom_jitter(aes(shape = factor(treated), color = funder_treated), 
              position = position_nudge(y = ifelse(data_plot_complete$funder_treated == "wb_0", 0.1,
                                                   ifelse(data_plot_complete$funder_treated == "wb_1", 0.2,
                                                          ifelse(data_plot_complete$funder_treated == "ch_0", 0.3,
                                                                 ifelse(data_plot_complete$funder_treated == "ch_1", 0.4, 0.4)))))) +
  labs(y = "Sector_ProjectPrecision SectorName (Group)", x = "Average Battle-Related deaths 1995-1999", 
       title = "Average Battle-Related Deaths for Treated and Control DHS points", 
       subtitle = "By Sector and Project Precision, lines +/- one standard deviation",
       color = "Funder & Treated") +
  scale_color_manual(values = c("wb_0" = "lightblue", "wb_1" = "blue", "ch_0" = "pink", "ch_1" = "red"),
                     breaks = c("wb_0", "wb_1", "ch_0", "ch_1"),
                     labels = c("WB Control", "WB Treated", "CH Control", "CH Treated")) +
  guides(color = guide_legend(override.aes = list(shape = c(21, 24)),
                              title = "Funder & Treated")) +
  theme_bw()

pre_deaths_tc_sector

ggsave("./figures/pre_deaths_tc_sector.png",pre_deaths_tc_sector, 
       width=8, height = 8, dpi=300,
       bg="white", units="in")


#Leader birthplace Figure
pr_ldrbrthtc_sector <- ggplot(data_plot_complete, aes(y = reorder(sec_pre_name, -mean_ldrbrth), x = mean_ldrbrth)) +
  geom_errorbarh(aes(xmin = pmax(mean_ldrbrth - sd_ldrbrth,0), xmax = pmin(mean_ldrbrth + sd_ldrbrth,1), color = funder_treated),
                 height = 0.1, 
                 position = position_nudge(y = ifelse(data_plot_complete$funder_treated == "wb_0", 0.1,
                                                      ifelse(data_plot_complete$funder_treated == "wb_1", 0.2,
                                                             ifelse(data_plot_complete$funder_treated == "ch_0", 0.3,
                                                                    ifelse(data_plot_complete$funder_treated == "ch_1", 0.4, 0.4)))))) +  
  geom_jitter(aes(shape = factor(treated), color = funder_treated), 
              position = position_nudge(y = ifelse(data_plot_complete$funder_treated == "wb_0", 0.1,
                                                   ifelse(data_plot_complete$funder_treated == "wb_1", 0.2,
                                                          ifelse(data_plot_complete$funder_treated == "ch_0", 0.3,
                                                                 ifelse(data_plot_complete$funder_treated == "ch_1", 0.4, 0.4)))))) +
  labs(y = "Sector_ProjectPrecision SectorName (Group)", x = "Portion in Leader Birthplace ADM1", 
       title = "Portion in Leader Birthplace ADM1 for Treated and Control DHS points", 
       subtitle = "By Sector and Project Precision, lines +/- one standard deviation",
       color = "Funder & Treated") +
  scale_color_manual(values = c("wb_0" = "lightblue", "wb_1" = "blue", "ch_0" = "pink", "ch_1" = "red"),
                     breaks = c("wb_0", "wb_1", "ch_0", "ch_1"),
                     labels = c("WB Control", "WB Treated", "CH Control", "CH Treated")) +
  guides(color = guide_legend(override.aes = list(shape = c(21, 24)),
                              title = "Funder & Treated")) +
  theme_bw()

pr_ldrbrthtc_sector

ggsave("./figures/pr_ldrbrthtc_sector.png",pr_ldrbrthtc_sector, 
       width=8, height = 8, dpi=300,
       bg="white", units="in")

#create a pairs plot of confounder variables
#install.packages("GGally")
library(GGally)
pairs_plot <- ggpairs(dhs_df[ ,c(119:122,125,126)])
ggsave("./figures/pairs_plot.png",pairs_plot,width = 10, height=10)

names(dhs_df)

#understand points that had a high number of NA's in the population density tif
#read file that didn't lose lon/lat
dhs_df <- read.csv("./data/interim/dhs_treat_control_raster.csv")
summary(dhs_df$pop_dens_na_count)
dhs_df %>% 
  filter(pop_dens_na_count > 150) %>% 
  select(lat,lon,pop_dens_na_count) %>% 
  arrange(pop_dens_na_count)
# lat        lon pop_dens_na_count
# 1    9.5314133 -13.680595               151
# 2  -16.4876900  34.680510               153
# 3    9.5297189 -13.685380               155
# 4    9.5374772 -13.673128               157
# 5   33.2620117  -8.509358               158
# 6    9.5217961 -13.691675               159
# 7    9.5091229 -13.722758               160
# 8    5.0951909  -3.063661               161 near coast Jomoro, Ghana
# 9    9.5384804 -13.687556               165 peninsula: Conakry, Guinea
# 10 -34.1829270  22.150469               172 peninsula: 28 Marsh St, Mossel Bay, 6500, South Africa
# 11  -8.7232502  29.088988               173 Lake Mweru
# 12  -0.9619578  34.071090               178 Lake Victoria
# 13   8.1298022 -13.201233               181 Banana Islands, Sierra Leone
# 14 -15.3025198  35.827076               200 Mecanhelas, Mozambique (lake and natl border near)
# 15 -12.0216432  34.618376               224  Chizumulu Island, Malawi
