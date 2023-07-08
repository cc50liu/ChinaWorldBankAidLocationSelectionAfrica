#prep_confounders_dhs_loan_projects.R
library(dplyr)
rm(list=ls())

#get Africa ISO codes
africa_isos_df <- read.csv("./data/interim/africa_isos.csv")

#read projects and limit to those with location details and study years
ch_oof_df <- read.csv("./data/AiddataChinav1.1.1/GeoCoded_China_Data_Merged_Files/oof-like_flows.csv") 

#n=267 #safe to limit to africa isos because rows with multiple recipients not in Africa
ch_oof_df <- ch_oof_df%>% 
  filter(precision_code<=3 &  # 1=exact, 2=up to 25km, or 3=dist/muni/commune
           umbrella==FALSE &
           year <= 2014 &
           !is.na(latitude) &
           recipients_iso3 %in% africa_isos_df$iso3)

summary(ch_oof_df)

ch_oof_df %>% 
  group_by(precision_code) %>% 
  count()
# precision_code     n
# <int> <int>
# 1              1   186
# 2              2    41
# 3              3    40

ch_oof_df %>% 
  group_by(location_class) %>% 
  count()
# location_class     n
# <int> <int>
# 1              1    41
# 2              2   171
# 3              3    54
# 4              4     1

ch_oof_df %>% 
  group_by(geographic_exactness) %>% 
  count()
# geographic_exactness     n
# <int> <int>
# 1                    1   203
# 2                    2    64

ch_oof_df %>% 
  group_by(start_actual_isodate) %>% 
  count()
# <chr>                <int>
#   1 ""                     183
# 2 "2003-01-01"             2
# 3 "2005-07-05"             1
# 4 "2005-07-28"             3
# 5 "2005-11-08"             7
# 6 "2005-11-19"             2
# 7 "2005-12-06"             4
# 8 "2005-12-13"             4
# 9 "2006-04-06"             1
# 10 "2006-04-18"             1


ch_oof_df %>% 
  group_by(status) %>% 
  count()
# status             n
# <chr>          <int>
# 1 Completion       174
# 2 Implementation    93

ch_oof_df %>% 
  group_by(transactions_start_year, year) %>% 
  count()
# transactions_start_year  year     n
# <int> <int> <int>
# 1                    2001  2001     2
# 2                    2002  2002     2
# 3                    2003  2003    20
# 4                    2004  2004     6
# 5                    2005  2005    46
# 6                    2006  2006    38
# 7                    2007  2007    26
# 8                    2008  2008    28
# 9                    2009  2009    23
# 10                    2010  2010    16
# 11                    2011  2011    16
# 12                    2012  2012    18
# 13                    2013  2013     9
# 14                    2014  2014    17

ch_oof_df %>% 
  group_by(field_completeness) %>% 
  count()
# 1                  6    33
# 2                  7    24
# 3                  8   112
# 4                  9    98

ch_oof_df %>% 
  group_by(ad_sector_names) %>% 
  count()
# 1 Agriculture, Forestry and Fishing           10
# 2 Communications                               7
# 3 Education                                   74
# 4 Energy Generation and Supply                52
# 5 Government and Civil Society                 6
# 6 Health                                      15
# 7 Industry, Mining, Construction               9
# 8 Other Multisector                            1
# 9 Other Social infrastructure and services    25
# 10 Transport and Storage                       56
# 11 Water Supply and Sanitation                 12



write.csv(country_confounder_complete_df,"./data/interim/country_confounders.csv",row.names=FALSE)  
