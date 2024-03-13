#prep_confounders_dhs_raster.R
#uses terra and sf libraries
library(dplyr)
library(ggplot2)
rm(list=ls())

# Get DHS points
dhs_df <- read.csv("./data/interim/dhs_est_iwi.csv") 

# Create empty columns to store results 
dhs_df$avg_min_to_city   <- NA
pop_dens_years <- 2000:2013
for (pop_dens_year in pop_dens_years) {
  column_name <- paste0("avg_pop_dens_", pop_dens_year)
  dhs_df[[column_name]] <- NA
}

#################################################
#Travel time (minutes) to city of >=50k in 2000
#################################################
acc_50k <- terra::rast("./data/EU-GlobalAccessiblilityMap/acc_50k.tif")

#initialize current_iso3, used to open each country's pop density tif
current_iso3 = "initial"

# Iterate over dhs points
for (i in 1:nrow(dhs_df)) {
  #i=1 #uncomment to test

  # Read the file path from the image_file_5k_3yr column
  file_path <- dhs_df$image_file_5k_3yr[i]

  #get the extent of the image over the dhs point from the first layer
  layer1 <- terra::rast(file_path, lyrs = 1)
  extent_dhs <- terra::ext(layer1)

  #calc and store average travel minutes
  travel_r <- terra::crop(acc_50k,extent_dhs)
  average_travel_min_50k <- base::mean(terra::as.matrix(travel_r),na.rm=TRUE)
  dhs_df$avg_min_to_city[i] <- average_travel_min_50k
  
  #read country's population density tifs, if not already in memory
  if (current_iso3 != dhs_df$iso3[i]) {
    current_iso3 <- dhs_df$iso3[i]
    #initialize empty variables for each year
    for (pop_dens_year in pop_dens_years) {
      raster_var_name <- paste0("cntry_pop_dens", pop_dens_year, "_r")
      assign(raster_var_name, NULL)
      file_var_name <- paste0("cntry_pop_dens", pop_dens_year, "_filename")
      assign(file_var_name, NULL)        
    }
    #read tifs and store rasters in the year-specific variables initialized above
    for (pop_dens_year in pop_dens_years) {
      cntry_pop_dens_filename <- paste0("./data/WorldPop/", 
                                        tolower(current_iso3), 
                                        "_pd_", pop_dens_year, "_1km_UNadj.tif")
      pop_dens_r <- terra::rast(cntry_pop_dens_filename)
      assign(paste0("cntry_pop_dens", pop_dens_year, "_filename"), cntry_pop_dens_filename)
      assign(paste0("cntry_pop_dens", pop_dens_year, "_r"), pop_dens_r)
    }
  }

  # Loop over the years, calculating and saving the pop density to df
  for (pop_dens_year in pop_dens_years) {
    tryCatch({
      avg_pop_dens <- NA  # Reinitialize in case of failure
      pop_dens_r <- terra::crop(get(paste0("cntry_pop_dens",pop_dens_year,"_r")), extent_dhs)
      avg_pop_dens <- base::mean(terra::as.matrix(pop_dens_r), na.rm = TRUE)
    }, error = function(e) {
      if (grepl("extents do not overlap", e$message)) {
        print(paste("Skipping crop error i:",i,"dhs_id",dhs_df$dhs_id[i],
                    "lat",dhs_df$lat[i],"lon",dhs_df$lon[i],file_path,
                    get(paste0("cntry_pop_dens",pop_dens_year,"_filename"))))
      #  "Skipping crop error i: 9561 dhs_id 48270 lat 27.157745012716 lon -13.1897010778989 ./data/dhs_tifs/morocco_2003/00118.tif ./data/WorldPop/mar_pd_2013_1km_UNadj.tif"
        }
    })
    
    # Save to the dataframe
    column_name <- paste0("avg_pop_dens_", pop_dens_year)
    dhs_df[[column_name]][i] <- avg_pop_dens
  }
  
  if (i %% 100 == 0) {
    print(paste("iteration",i,"Avg trav min",round(average_travel_min_50k,1), 
                "Last avg pd",round(avg_pop_dens,1)))
  }
}

#write to file for later use
dhs_df %>% 
  write.csv(.,"./data/interim/dhs_treat_control_raster_no_log.csv",row.names=FALSE)  
#dhs_df <- read.csv("./data/interim/dhs_treat_control_raster_no_log.csv")


#look at points where couldn't determine pop density
dhs_df %>%
  select(dhs_id,country,starts_with("avg_pop_dens_"),image_file) %>% 
  filter(!complete.cases(.))
  
#exclude points where pop density could not be determined, n now 9909
dhs_df <- dhs_df %>%
  filter(complete.cases(.))

#any rows with 0 population density?
dhs_df %>%
  filter(if_any(starts_with("avg_pop_dens_"), ~.x == 0))

#exclude 5 rows in DR congo where pop density is 0, n now 9904
#dhs_id's 8359 8361 8364 8369 8376
dhs_df <- dhs_df %>%
  filter(!if_any(starts_with("avg_pop_dens_"), ~.x == 0))  

#create logged versions of variables
dhs_log_df <- dhs_df %>%
  mutate(
    across(starts_with("avg_"), ~ log(. + 1), .names = "log_{.col}")
  )
names(dhs_log_df)

#write to file for later use
dhs_log_df %>% 
  write.csv(.,"./data/interim/dhs_treat_control_raster.csv",row.names=FALSE)    
#dhs_log_df <- read.csv("./data/interim/dhs_treat_control_raster.csv")    


#plot the distribution of travel minutes         
minutes_travel_density <- dhs_log_df %>% 
  ggplot(aes(avg_min_to_city)) +
  geom_density() +
  labs(x="Avg min to >50K City", y="Density across DHS clusters",
       title="Travel minutes to >50k City in 2000")  +
  theme_bw()

minutes_travel_density

ggsave("./figures/minutes_travel_density.png",minutes_travel_density, width=6, height = 4, dpi=300,
       bg="white", units="in")

#density plot of the log travel minutes
log_minutes_travel_density <-  dhs_log_df %>% 
  ggplot(aes(log_avg_min_to_city)) +
  geom_density() +
  labs(x="Log Avg min to >50K City", y="Density across DHS clusters",
       title="Log Travel minutes to >50k City in 2000") +
  theme_bw()

log_minutes_travel_density
ggsave("./figures/log_minutes_travel_density.png",log_minutes_travel_density, width=6, height = 4, dpi=300,
       bg="white", units="in")

#plot the distribution of population density        
population_density <- dhs_log_df %>%
  tidyr::pivot_longer(cols = starts_with("avg_pop_dens_"), names_to = "pop_dens_year", values_to = "density") %>%
  ggplot(aes(density, color = pop_dens_year)) +
  geom_density() +
  labs(x = "Average population density per square km", y = "Density across DHS clusters",
       title = "Population density across DHS clusters", color="Year") +
  scale_color_discrete(labels = function(x) gsub(".*?(\\d{4})$", "\\1", x)) +
  theme_bw()

population_density
ggsave("./figures/population_density.png",population_density, width=6, height = 4, dpi=300,
       bg="white", units="in")

log_avg_pop_dens_density <-  dhs_log_df %>% 
  tidyr::pivot_longer(cols = starts_with("log_avg_pop_dens_"), names_to = "pop_dens_year", values_to = "density") %>%
  ggplot(aes(density, color = pop_dens_year)) +
  geom_density() +
  labs(x = "Average(log) population density per square km", y = "Density (log) across DHS clusters",
       title = "Population density (log) across DHS clusters", color="Year") +
  scale_color_discrete(labels = function(x) gsub(".*?(\\d{4})$", "\\1", x)) +
  theme_bw()


log_avg_pop_dens_density
ggsave("./figures/log_avg_pop_dens_density.png",log_avg_pop_dens_density, width=6, height = 4, dpi=300,
       bg="white", units="in")





