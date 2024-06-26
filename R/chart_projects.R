################################################################################
# chart_projects.R:  charts and descriptive stats on projects
################################################################################
library(dplyr)
library(ggplot2)

rm(list=ls())

#read consolidated project list
oda_df <- read.csv("./data/interim/africa_oda_sector_group.csv") %>% 
  filter(transactions_start_year >= 2002 &
         transactions_start_year <= 2013  )

### Project counts by year and funder
proj_year_count <- oda_df %>% 
  group_by(funder, transactions_start_year) %>% 
  count() %>% 
ggplot(aes(x = transactions_start_year, y = n, fill = funder)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "African aid by start year and funder",
       x = "Transaction Start Year", y = "Project Count") +
  theme_bw() + 
  theme(panel.grid = element_blank()) +
  scale_x_continuous(breaks = unique(oda_df$transactions_start_year), 
                     labels = unique(oda_df$transactions_start_year)) + 
  guides(fill = guide_legend(title = "Funder")) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("CH" = "indianred1", "WB" = "mediumblue"),
                    labels = c("China","World Bank")) 

ggsave("./figures/proj_year_counts.png",proj_year_count, width=6, height = 4, dpi=300,
       bg="white", units="in")


# proj_year_prec_count <- oda_df %>% 
#   group_by(funder, transactions_start_year, precision_code) %>% 
#   count() %>% 
#   ggplot(aes(x = transactions_start_year, y = n, fill = funder, alpha=precision_code/4)) +
#   geom_bar(stat = "identity", position = "dodge",color = "black", width = 0.7) +
#   labs(title = "Count of African aid projects by start year",
#        x = "Transaction Start Year", y = "Count") +
#   theme_minimal() + 
#   scale_x_continuous(breaks = unique(oda_df$transactions_start_year), 
#                      labels = unique(oda_df$transactions_start_year)) + 
#   guides(fill = guide_legend(title = "Funder"),
#          alpha = guide_legend(title= "Precision Code")) + 
#   scale_alpha_continuous(breaks=c(.25,.5,.75,1),labels = c("1 Exact", "2 Near", "3 ADM2", "4 ADM1")) +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))

### Project counts by year, funder, and precision
proj_year_prec_count <- oda_df %>% 
  group_by(funder, transactions_start_year, precision_code) %>% 
  count() %>% 
  ggplot(aes(x = transactions_start_year, y = n, fill = funder, alpha = factor(precision_code/4))) +
  geom_bar(stat = "identity", position = position_dodge(width = .9), width = 0.7) +
  labs(title = "African aid project location counts by start year and precision",
       x = "Transaction Start Year", y = "Count") +
  theme_bw() + 
  theme(panel.grid = element_blank()) +
  scale_x_continuous(breaks = unique(oda_df$transactions_start_year), 
                     labels = unique(oda_df$transactions_start_year)) + 
  guides(fill = guide_legend(title = "Funder"),
         alpha = guide_legend(title = "Precision Code")) + 
  scale_alpha_manual(values = c(1, 0.75, 0.5, 0.25),
                     labels = c("1 Exact", "2 Near", "3 ADM2", "4 ADM1")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("CH" = "indianred1", "WB" = "mediumblue"),
                    labels = c("China","World Bank")) 


ggsave("./figures/proj_year_prec_counts.png",proj_year_prec_count, width=6, height = 4, dpi=300,
       bg="white", units="in")

### Project Precision Counts
proj_prec_count <- oda_df %>% 
  group_by(funder, precision_code) %>% 
  count() %>% 
  ggplot(aes(x = factor(precision_code), y = n, fill = funder)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "African aid project location counts by precision and funder",
       x = "Precision Code", y = "Project location count") +
  theme_bw() + 
  theme(panel.grid = element_blank()) +
  guides(fill = guide_legend(title = "Funder"),
         alpha = guide_legend(title = "Precision Code")) + 
  scale_x_discrete(labels = c("1 Exact", "2 Near", "3 ADM2", "4 ADM1")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("CH" = "indianred1", "WB" = "mediumblue"),
                    labels = c("China","World Bank")) 

ggsave("./figures/proj_prec_counts.png", proj_prec_count, width = 6, height = 4, dpi = 300,
       bg = "white", units = "in")


oda_df %>% 
  filter(precision_code %in% c(1,3)) %>% 
  group_by(funder, location_type_name, location_type_code, geographic_exactness) %>% 
  count() %>% 
  filter(geographic_exactness==2)
#The only records with "approximate" are Chinese ADM2, 3, or 4 projects.
# funder location_type_name                   location_type_code geographic_exactness     n
# <chr>  <chr>                                <chr>                             <int> <int>
# 1 CH     fourth-order administrative division ADM4                                  2     2
# 2 CH     second-order administrative division ADM2                                  2    45
# 3 CH     third-order administrative division  ADM3                                  2     4


### Location Type Codes
#plot top location type codes
loc_type_plot <- oda_df %>% 
  filter(precision_code %in% c(1,3)) %>% 
  group_by(funder, location_type_name, location_type_code, geographic_exactness) %>% 
  count() %>% 
  filter(n > 10) %>% 
  mutate(geographic_exactness = factor(geographic_exactness / 2)) %>% 
ggplot(aes(y = reorder(location_type_name,n), x = n, fill = funder, alpha=geographic_exactness)) +
  #geom_bar(stat = "identity", position = "dodge") +
  geom_bar(stat = "identity", position = position_dodge(width = .9), width = 0.7) +
  labs(title = "Most Frequent Location Types (n>10)",
       subtitle = "Aid Project Precision 1 or 3",
       y = "Location Type", x = "Count") +
  theme_bw() + 
  theme(panel.grid = element_blank()) +
  guides(fill = guide_legend(title = "Funder"),
         alpha = guide_legend(title = "Geographic Exactness")) + 
  scale_alpha_manual(values = c(.5, 1),
                     labels = c("1 Exact", "2 Approximate")) +
  scale_fill_manual(values = c("CH" = "indianred1", "WB" = "mediumblue"),
                    labels = c("China", "World Bank")) 

ggsave("./figures/top_loc_types.png",loc_type_plot, width=6, height = 4, dpi=300,
       bg="white", units="in")

### Count by Country 
country_plot <- oda_df %>% 
  filter(precision_code %in% c(1,2,3)) %>% 
  group_by(funder, recipients) %>% 
  count() %>% 
  ggplot(aes(y = recipients, x = n, fill = funder)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Aid projects by recipients and funder",
       subtitle = "Aid Project Precision 1, 2, and 3",
       y = "Recipient(s)", x = "Count") +
  theme_bw() + 
  theme(panel.grid = element_blank()) + 
  guides(fill = guide_legend(title = "Funder")) +
  scale_fill_manual(values = c("CH" = "indianred1", "WB" = "mediumblue"),
                    labels = c("China", "World Bank")) 

ggsave("./figures/country_counts.png",country_plot, width=6, height = 8, dpi=300,
       bg="white", units="in")

### Count by Sector 
sector_plot <- oda_df %>% 
  filter(precision_code %in% c(1,2,3)) %>% 
  group_by(funder, ad_sector_names) %>% 
  mutate(ad_sector_names = paste0(substr(ad_sector_names, 1, 30),
                                 " (",ad_sector_codes,")")) %>%
  count() %>% 
  ggplot(aes(y = ad_sector_names, x = n, fill = funder)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "African Aid projects 2002-2013 by sector and funder",
       subtitle = "Aid Project Precisions: Exact, Near, and ADM2",
       y = "Sector", x = "Count") +
  theme_bw() + 
  theme(panel.grid = element_blank()) +
  guides(fill = guide_legend(title = "Funder")) +
  scale_fill_manual(values = c("CH" = "indianred1", "WB" = "mediumblue"),
                    labels = c("China", "World Bank")) 

ggsave("./figures/sector_counts.png",sector_plot, width=8, height = 8, dpi=300,
       bg="white", units="in")


### Project length by sector
sector_length_plot <- oda_df %>% 
  filter(precision_code %in% c(1,2,3)) %>%
  mutate(start_year=as.integer(sub("^(\\d{4})-.*","\\1",start_actual_isodate)),
         end_year=as.integer(sub("^(\\d{4})-.*","\\1",end_actual_isodate)),
         proj_length = ifelse(is.na(end_year) | is.na(start_year),-1,
                              end_year - start_year)) %>% 
  mutate(ad_sector_names = paste0(substr(ad_sector_names, 1, 30),
                                  " (",ad_sector_codes,")"))  %>% 
  group_by(funder, ad_sector_names, proj_length) %>%
  ggplot(aes(y = ad_sector_names, x = proj_length, color = funder)) +
  geom_boxplot(outlier.color=NULL) +
  geom_vline(xintercept=0,color="gray80") +
  labs(title = "African aid project length (years) by Sector and Funder (2002-2013)",
       subtitle = "Includes only projects of precisions: Exact, Near, and ADM2",
       y = "Sector", x = "Project Length (Years, -1 = Unknown end date)") +
  theme_bw() + 
  theme(panel.grid = element_blank()) +
  guides(color = guide_legend(title = "Funder")) +
  scale_color_manual(values = c("CH" = "indianred1", "WB" = "mediumblue"),
                    labels = c("China", "World Bank")) +
  scale_x_continuous(n.breaks=14)

ggsave("./figures/sector_proj_length.png",sector_length_plot, width=10, height = 8, dpi=300,
       bg="white", units="in")

