library(dplyr)

dhs_nl_df <- read.csv("./data/GEE/per_cap_nl_dhs_5k_WorldPop.csv") %>%  
  #exclude locations with 0 pop_counts, so they don't show high per capita nightlights 
  filter(!if_any(starts_with("pop_count"), ~ . == 0))

dhs_nl_harmonized_df <- read.csv("./data/GEE/per_cap_nl_harmonized_5k_WorldPop.csv") %>%  
  #exclude locations with 0 pop_counts, so they don't show high per capita nightlights 
  filter(!if_any(starts_with("pop_count"), ~ . == 0))

for (current_year in 2000:2013) {
  col_name <- paste0("nl",current_year)
  print(paste(current_year,cor(dhs_nl_harmonized_df[[col_name]],dhs_nl_df[[col_name]])))
}

# [1] "2000 0.992896906518665"
# [1] "2001 0.992553620457069"
# [1] "2002 0.992266670219821"
# [1] "2003 0.987133954865717"
# [1] "2004 0.980282196603104"
# [1] "2005 0.983599382531392"
# [1] "2006 0.986038543677659"
# [1] "2007 0.984988996516684"
# [1] "2008 0.986814839384792"
# [1] "2009 0.990427672664215"
# [1] "2010 0.998467117070647"
# [1] "2011 0.99797165949474"
# [1] "2012 0.994301849844765"
# [1] "2013 0.994735594980626"