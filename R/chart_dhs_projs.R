################################################################################
# chart_dhs.R:  charts and descriptive stats on dhs points and their intersection
# with projects by sector 
################################################################################
library(dplyr)
library(tidyr)
library(ggplot2)

rm(list=ls())

#get dhs treatment/control counts for actual (not estimated) DHS points
treat_control_actual_dhs_df <- read.csv("./data/interim/dhs_treat_control_3yr_actual_counts.csv") 

###########################################################
#calc dhs treatment/control counts for estimated DHS points
###########################################################
##### read confounder and treatment data from files
dhs_confounders_df <- read.csv("./data/interim/dhs_5k_confounders.csv") %>% 
  select(-year)  #remove survey year column that could be confused with oda year

#get list of all dhs_id's and their iso3 for use below from confounder set
#since those without confounder data are not usable
dhs_iso3_df <- dhs_confounders_df %>% 
  distinct(dhs_id,iso3) 

#get treated for all funders and sectors
dhs_t_df <- read.csv("./data/interim/dhs_treated_sector_3yr.csv") %>% 
  #exclude DHS points where confounder data not available 
  inner_join(dhs_confounders_df %>% 
               select(dhs_id, ID_adm2), by = join_by(dhs_id)) %>% 
  filter(year_group!="2014:2016")

##### calculate control points #############################
#identify countries where each funder is operating in each sector
funder_sector_iso3 <- dhs_t_df %>% 
  #join to dhs_confounders to get iso3 and limit to dhs points with confounder data
  inner_join(dhs_confounders_df, by="dhs_id") %>%  
  distinct(funder,sector,iso3)
  
#create a record for each year_group for panel data
year_group_v <- c('2002:2004', '2005:2007', '2008:2010', '2011:2013')

#generate dataframe of all dhs points for all year groups in operating countries 
all_t_c_df <- funder_sector_iso3 %>%
  #create a row for each year group
  crossing(year_group = year_group_v) %>% 
  #create a row for each dhs_id
  left_join(dhs_iso3_df,by="iso3",
            multiple = "all")

#remove treated funder/sector/dhs/year_group observations to construct controls 
dhs_c_df <- all_t_c_df %>% 
  #exclude dhs_points treated in each year_group
  anti_join(dhs_t_df,by=c("sector","funder","dhs_id","year_group"))

treat_dhs_count_df  <- dhs_t_df %>%   
  group_by(funder,sector) %>%
  summarize(treat_n = n()) %>% 
  ungroup()

control_dhs_count_df  <- dhs_c_df %>%   
  group_by(funder,sector) %>%
  summarize(control_n = n()) %>% 
  ungroup()

#join control and treated into a single dataframe
treat_control_est_dhs_df <- treat_dhs_count_df %>% 
  left_join(control_dhs_count_df, by=c("funder","sector")) %>% 
  rename(est_iwi_treat_n = treat_n,
         est_iwi_control_n = control_n)

#join with t/c counts from actual IWI DHS locations rather than estimates
t_c_est_act_df <- treat_control_est_dhs_df %>% 
  left_join(treat_control_actual_dhs_df, by=c("funder","sector")) %>% 
  rename(act_iwi_treat_n = treat_n,
         act_iwi_control_n = control_n)


write.csv(t_c_est_act_df,"./tables/dhs_treat_control_est_act_compare.csv",row.names=FALSE)

names(t_c_est_act_df)
#################################################################################
#xy plots to compare treated and control n for actual and estimated wealth 
#################################################################################

#determine the limits of the plot
max_abs_value <- max(abs(t_c_est_act_df$est_iwi_treat_n),
                     abs(t_c_est_act_df$act_iwi_treat_n),
                     na.rm=T)

treated_est_act <- t_c_est_act_df %>% 
  ggplot(aes(x = act_iwi_treat_n, y = est_iwi_treat_n, color=funder, 
             label=sector)) +
  geom_point() +
  ggrepel::geom_text_repel(box.padding = .1,max.overlaps=Inf,show.legend=FALSE) +
  scale_color_manual(name = "Funder",
                     values = c("ch"="indianred1","wb"="mediumblue"),
                     labels = c("China","World Bank")) +
  geom_abline(intercept=0, slope=1, linetype="dashed",color="gray80") +
  labs(title = "Treated Observations using Actual versus Estimated Wealth Outcomes\nBy Funder and Sector Number",
       x = "Actual DHS wealth data treated n (Cross-Sectional Data)",
       y = "Estimated wealth data treated n (Panel Data)",
       legend = "Funder") +   
  coord_cartesian(xlim=c(0,max_abs_value),
                  ylim=c(0,max_abs_value)) +
  theme_bw()  +
  theme(panel.grid = element_blank())

#save
ggsave("./figures/xy_treated_n_est_act.pdf",
       treated_est_act,
       width=8, height = 6, dpi=300,
       bg="white", units="in")


#determine the limits of the plot
max_abs_value_tc <- max(abs(t_c_est_act_df$est_iwi_treat_n),
                     abs(t_c_est_act_df$act_iwi_treat_n),
                     abs(t_c_est_act_df$est_iwi_control_n),
                     abs(t_c_est_act_df$act_iwi_control_n),
                     na.rm=T)

t_c_est_act_fig <- t_c_est_act_df %>% 
  pivot_longer(cols=starts_with("est"),names_to="var_est",values_to="estimated_n") %>% 
  mutate(var=ifelse(grepl("_iwi_treat_n",var_est),"Treated","Control")) %>% 
  select(-var_est) %>% 
  mutate(actual_n=ifelse(var=="Treated",act_iwi_treat_n,act_iwi_control_n)) %>% 
  select(-act_iwi_treat_n,-act_iwi_control_n ) %>% 
  ggplot(aes(x = actual_n, y = estimated_n, color=funder, 
             label=sector)) +
  facet_wrap(var ~ funder,scales="free",
             labeller=labeller(funder=c("ch"="China","wb"="World Bank"))) + 
  geom_point(show.legend=FALSE) +
  ggrepel::geom_text_repel(box.padding = .1,max.overlaps=Inf,show.legend=FALSE) +
  scale_color_manual(values = c("ch"="indianred1","wb"="mediumblue"),
                     labels = c("China","World Bank")) +
  geom_abline(intercept=0, slope=1, linetype="dashed",color="gray80") +
  labs(title = "Observation n using Actual DHS versus Estimated Wealth Outcomes\nBy Funder and Sector Number",
       x = "Actual DHS wealth data observations (Cross-Sectional Data)",
       y = "Estimated wealth data observations (Panel Data)",
       legend = "Funder") +   
  theme_bw()  +
  theme(panel.grid = element_blank())

#save
ggsave("./figures/xy_cntrl_treated_n_est_act.pdf",
       t_c_est_act_fig,
       width=8, height = 8, dpi=300,
       bg="white", units="in")
