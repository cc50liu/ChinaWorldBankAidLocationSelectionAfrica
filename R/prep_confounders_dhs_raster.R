#prep_confounders_dhs_raster.R
#uses terra and sf libraries
library(dplyr)
library(ggplot2)
rm(list=ls())

####################################
# Nightlights
dhs_df <- read.csv("./data/interim/dhs_est_iwi.csv") 

  # Create empty columns to store results 
  dhs_df$avg_nl_1996_1998  <- NA
  dhs_df$avg_nl_1999_2001  <- NA
  dhs_df$avg_nl_2002_2004  <- NA
  dhs_df$avg_nl_2005_2007  <- NA
  dhs_df$avg_nl_2008_2010  <- NA
  dhs_df$avg_nl_2011_2013  <- NA
  dhs_df$avg_min_to_city   <- NA
  
  pop_dens_years <- 2000:2013
  for (pop_dens_year in pop_dens_years) {
    column_name <- paste0("avg_pop_dens_", pop_dens_year)
    dhs_df[[column_name]] <- NA
  }
  
  #construct map of bands to the nl year columns they relate to
  nl_bands <- c("_24", "_32", "_40", "_48", "_56", "_64")
  nl_columns <- c("avg_nl_1996_1998",
                    "avg_nl_1999_2001",
                    "avg_nl_2002_2004",
                    "avg_nl_2005_2007",
                    "avg_nl_2008_2010",
                    "avg_nl_2011_2013")
  nl_bands_to_columns <- data.frame(nl_bands,nl_columns)
  
  #Travel time (minutes) to city of >=50k in 2000
  acc_50k <- terra::rast("./data/EU-GlobalAccessiblilityMap/acc_50k.tif")

  #initialize current_iso3, used to open each country's pop density tif
  current_iso3 = "initial"

  # Iterate over dhs points
  for (i in 1:nrow(dhs_df)) {
    #i=1 #uncomment to test
    # Read the file path from the image_file column
    file_path <- dhs_df$image_file[i]
    for (j in 1:nrow(nl_bands_to_columns)) {
      #j=1 #uncomment to test
      layer_name <- paste0(gsub(pattern=".*/(\\d{5})\\.tif$","\\1",
                                x=file_path),
                                nl_bands_to_columns$nl_bands[j])  #layer is preceeded by filename
  
      # Calculate average nighlights
      nl <- terra::rast(file_path, lyrs = layer_name)
      if (j==1) {
        #save the extent to crop travel time and density rasters
        extent_nl <- terra::ext(nl)
      }
      average_nl <- base::mean(terra::as.matrix(nl),na.rm=TRUE)
      dhs_df[[nl_bands_to_columns$nl_columns[j]]][i] <- average_nl
    }
    
    #calc and store average travel minutes
    travel_r <- terra::crop(acc_50k,extent_nl)
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
        pop_dens_r <- terra::crop(get(paste0("cntry_pop_dens",pop_dens_year,"_r")), extent_nl)
        avg_pop_dens <- base::mean(terra::as.matrix(pop_dens_r), na.rm = TRUE)
      }, error = function(e) {
        if (grepl("extents do not overlap", e$message)) {
          print(paste("Skipping crop error i:",i,"dhs_id",dhs_df$dhs_id,
                      "lat",dhs_df$lat[i],"lon",dhs_df$lon[i],file_path,
                      get(paste0("cntry_pop_dens",pop_dens_year,"_filename"))))
        #   [1] "Skipping crop error i: 5051 lat 27.157745012716 lon -13.1897010778989 ./data/dhs_tifs/morocco_2003/00118.tif ./data/WorldPop/mar_pd_2000_1km_UNadj.tif"
        #   [1] "Skipping crop error i: 5051 lat 27.157745012716 lon -13.1897010778989 ./data/dhs_tifs/morocco_2003/00118.tif ./data/WorldPop/mar_pd_2003_1km_UNadj.tif"
        #   [1] "Skipping crop error i: 5051 lat 27.157745012716 lon -13.1897010778989 ./data/dhs_tifs/morocco_2003/00118.tif ./data/WorldPop/mar_pd_2006_1km_UNadj.tif"
        #   [1] "Skipping crop error i: 5051 lat 27.157745012716 lon -13.1897010778989 ./data/dhs_tifs/morocco_2003/00118.tif ./data/WorldPop/mar_pd_2009_1km_UNadj.tif"
        #   [1] "Skipping crop error i: 5051 lat 27.157745012716 lon -13.1897010778989 ./data/dhs_tifs/morocco_2003/00118.tif ./data/WorldPop/mar_pd_2012_1km_UNadj.tif"
          }
      })
      
      # Save to the dataframe
      column_name <- paste0("avg_pop_dens_", pop_dens_year)
      dhs_df[[column_name]][i] <- avg_pop_dens
    }
    
    if (i %% 100 == 0) {
      print(paste("iteration",i,"Avg nl",round(average_nl,1),"Avg trav min",
                  round(average_travel_min_50k,1), "Last avg pd",
                  round(avg_pop_dens,1)))
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
    
  #exclude points where pop density could not be determined, n now 9606
  dhs_df <- dhs_df %>%
    filter(complete.cases(.))
  
  #any rows with 0 population density?
  dhs_df %>%
    filter(if_any(starts_with("avg_pop_dens_"), ~.x == 0))
  
  #exclude 5 rows in DR congo where pop density is 0, n now 9601
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
  
#plot the distribution of nightlights       
nl_density <- dhs_log_df %>% 
  ggplot(aes(avg_nl_1996_1998)) +
  geom_density() +
  labs(x="Avg nightlights 1996-1998", y="Density per DHS point",
       title="Avg nightlights 1996-1998")

nl_density

ggsave("./figures/nl_density_1996_1998.png",nl_density, width=6, height = 4, dpi=300,
       bg="white", units="in")

#density plot of the log
log_nl_density <-  dhs_log_df %>% 
  ggplot(aes(log_avg_nl_1996_1998)) +
  geom_density() +
  labs(x="Log avg nightlights 1996-1998", y="Density per DHS point",
       title="Log avg nightlights 1996-1998")

log_nl_density
ggsave("./figures/log_nl_density.png",log_nl_density, width=6, height = 4, dpi=300,
       bg="white", units="in")


#plot the distribution of travel minutes         
minutes_travel_density <- dhs_log_df %>% 
  ggplot(aes(avg_min_to_city)) +
  geom_density() +
  labs(x="Avg min to >50K City", y="Density across DHS points",
       title="Travel minutes to >50k City in 2000")

minutes_travel_density

ggsave("./figures/minutes_travel_density.png",minutes_travel_density, width=6, height = 4, dpi=300,
       bg="white", units="in")

#density plot of the log travel minutes
log_minutes_travel_density <-  dhs_log_df %>% 
  ggplot(aes(log_avg_min_to_city)) +
  geom_density() +
  labs(x="Log Avg min to >50K City", y="Density across DHS points",
       title="Log Travel minutes to >50k City in 2000")

log_minutes_travel_density
ggsave("./figures/log_minutes_travel_density.png",log_minutes_travel_density, width=6, height = 4, dpi=300,
       bg="white", units="in")

#plot the distribution of population density        
population_density <- dhs_log_df %>%
  pivot_longer(cols = starts_with("avg_pop_dens_"), names_to = "pop_dens_year", values_to = "density") %>%
  ggplot(aes(density, color = pop_dens_year)) +
  geom_density() +
  labs(x = "Average population density", y = "Density across DHS points",
       title = "Population density across DHS points", color="Year") +
  scale_color_discrete(labels = function(x) gsub(".*?(\\d{4})$", "\\1", x))

population_density
ggsave("./figures/population_density.png",population_density, width=6, height = 4, dpi=300,
       bg="white", units="in")

log_avg_pop_dens_density <-  dhs_log_df %>% 
  pivot_longer(cols = starts_with("log_avg_pop_dens_"), names_to = "pop_dens_year", values_to = "density") %>%
  ggplot(aes(density, color = pop_dens_year)) +
  geom_density() +
  labs(x = "Average(log) population density", y = "Density (log) across DHS points",
       title = "Population density (log) across DHS points", color="Year") +
  scale_color_discrete(labels = function(x) gsub(".*?(\\d{4})$", "\\1", x))


log_avg_pop_dens_density
ggsave("./figures/log_avg_pop_dens_density.png",log_avg_pop_dens_density, width=6, height = 4, dpi=300,
       bg="white", units="in")





