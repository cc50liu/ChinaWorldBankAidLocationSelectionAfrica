dhs_nl_bnu_df <- read.csv("./data/GEE/per_cap_nl_dhs_WorldPop_BNU.csv") %>%  
  #exclude three points with 0 pop_counts, so they don't show high per capita nightlights 
  filter(!if_any(starts_with("pop_count"), ~ . == 0))

#exploration of old and new values
compare_df <- dhs_nl_bnu_df %>% 
  mutate(across(starts_with("nl"), ~.x, .names="{col}_BNU")) %>% 
  select(dhs_id, matches("nl.*_BNU")) %>% 
  inner_join(dhs_nl_df,by="dhs_id") %>% 
  select(dhs_id, starts_with("nl")) 

compare_longer_df <- compare_df %>% 
  pivot_longer(cols=starts_with("nl")) %>% 
  mutate(year = substr(name,3,6),
         dataset = ifelse(grepl("BNU",name),"BNU","Non-corrected"))


compare_longer_df %>% 
  filter(dhs_id==48830) %>% 
  print(n=38)
#problem!!! some of these have negative values

names(compare_df)