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
  
  dhs_df$percap_nl_1996_1998  <- NA
  dhs_df$percap_nl_1999_2001  <- NA
  dhs_df$percap_nl_2002_2004  <- NA
  dhs_df$percap_nl_2005_2007  <- NA
  dhs_df$percap_nl_2008_2010  <- NA
  dhs_df$percap_nl_2011_2013  <- NA
  
  dhs_df$avg_min_to_city   <- NA
  
  pop_dens_years <- 2000:2013
  for (pop_dens_year in pop_dens_years) {
    column_name <- paste0("avg_pop_dens_", pop_dens_year)
    dhs_df[[column_name]] <- NA
    column_name <- paste0("sum_pop_dens_", pop_dens_year)
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
  
  #some example tifs
  # dhs_id      country                                  image_file        lat      lon
  # 1   48830 south_africa ./data/dhs_tifs/south_africa_2016/00743.tif -34.463232 19.54247
  # 2   48781 south_africa ./data/dhs_tifs/south_africa_2016/00694.tif -34.418873 19.18893
  # 3   48237 south_africa ./data/dhs_tifs/south_africa_2016/00150.tif -27.402460 26.64820
  # 4   48607 south_africa ./data/dhs_tifs/south_africa_2016/00520.tif -26.910471 26.59503
  # 5   48316 south_africa ./data/dhs_tifs/south_africa_2016/00229.tif -26.281482 27.88798
  # 6   55822     zimbabwe     ./data/dhs_tifs/zimbabwe_1999/00028.tif -20.213279 28.52031
  # 7   55800     zimbabwe     ./data/dhs_tifs/zimbabwe_1999/00006.tif -20.207657 28.55544
  # 8    8158      comoros      ./data/dhs_tifs/comoros_2012/00149.tif -11.491992 43.38610
  # 9    8161      comoros      ./data/dhs_tifs/comoros_2012/00152.tif -11.437967 43.39200
  # 10     44       angola       ./data/dhs_tifs/angola_2006/00044.tif  -8.757972 13.50009

  b1_sa32 <- terra::rast("./data/dhs_tifs/south_africa_2016/00743.tif", lyrs = "00743_1")
  
  nl_sa32 <- terra::rast("./data/dhs_tifs/south_africa_2016/00743.tif", lyrs = "00743_32")
  nl_32 <- terra::rast("./data/dhs_tifs/south_africa_2016/00694.tif", lyrs = "00694_32" )
  nl_40 <- terra::rast("./data/dhs_tifs/south_africa_2016/00150.tif", lyrs = "00150_40")
  nl_48 <- terra::rast("./data/dhs_tifs/south_africa_2016/00520.tif", lyrs = "00520_48")
  nl_56 <- terra::rast("./data/dhs_tifs/zimbabwe_1999/00028.tif", lyrs = "00028_56")
  nl_64 <- terra::rast("./data/dhs_tifs/comoros_2012/00149.tif", lyrs = "00149_64")
  nl_ang <- terra::rast("./data/dhs_tifs/angola_2006/00044.tif", lyrs = "00044_24")
  
  cntry_pop_dens_filename <- paste0("./data/WorldPop/zaf_pd_2000_1km_UNadj.tif")
  pop_dens_sa_32_r <- terra::rast(cntry_pop_dens_filename)
  terra::plot(pop_dens_sa_32_r)
  
  
  nl_bands <- c("_24", "_32", "_40", "_48", "_56", "_64")
  nl_columns <- c("avg_nl_1996_1998",
                  "avg_nl_1999_2001",
                  "avg_nl_2002_2004",
                  "avg_nl_2005_2007",
                  "avg_nl_2008_2010",
                  "avg_nl_2011_2013")
  
  
  #one problem with per capita nightlights is that I don't have population density prior to 2000.
  #so I can't calculate per capita nightlights for projects starting in 2001 because the nightlights
  #are between 1996-1998 for that year
  
  #another challenge is the varying resolution of the population density and nightlight tiffs
  
  
  terra::plot(nl_sa32)
  nl_sa32
  extent_nl_sa32 <- terra::ext(nl_24)
  pop_dens__sa32 <- terra::crop(pop_dens_sa_32_r, extent_nl_sa32)
  terra::plot(pop_dens__sa32)
  
  dev.off()
  terra::plot(nl_32)
  dev.off()
  
  terra::plot(nl_40)
  dev.off()
  terra::plot(nl_48)
  nl_48
  dev.off()
  terra::plot(nl_56)  #this seems to have a higher resolution than the others
  nl_56
  dev.off()
  terra::plot(nl_64)
  terra::plot(nl_ang)

  
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
        sum_pop_dens <- base::sum(terra::as.matrix(pop_dens_r), na.rm = TRUE)
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
      column_name <- paste0("sum_pop_dens_", pop_dens_year)
      dhs_df[[column_name]][i] <- sum_pop_dens
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
  
#plot the distribution of nightlights       
nl_density <- dhs_log_df %>% 
  tidyr::pivot_longer(cols = starts_with("avg_nl_"), names_to = "avg_nl_year", values_to = "density") %>%
  ggplot(aes(density, color=avg_nl_year)) +
  geom_density() +
  labs(x="Avg nightlights", y="Density across DHS clusters",
       title="Nightlights across DHS Clusters (avg)",
       color="Years") +
  scale_color_discrete(labels = function(x) gsub(".*?(\\d{4}_\\d{4})$", "\\1", x)) +
  theme_bw()

nl_density

ggsave("./figures/nl_density.png",nl_density, width=6, height = 4, dpi=300,
       bg="white", units="in")

#density plot of log nightlights
log_nl_density <-  dhs_log_df %>% 
  tidyr::pivot_longer(cols = starts_with("log_avg_nl_"), names_to = "log_avg_nl_year", values_to = "density") %>%
  ggplot(aes(density, color=log_avg_nl_year)) +
  geom_density() +
  labs(x="Nightlights (avg, log)", y="Density per DHS cluster",
       title="Average nightlights (log) across DHS clusters",
       color="Years") +
  scale_color_discrete(labels = function(x) gsub(".*?(\\d{4}_\\d{4})$", "\\1", x)) +
  theme_bw()

log_nl_density
ggsave("./figures/log_nl_density.png",log_nl_density, width=6, height = 4, dpi=300,
       bg="white", units="in")


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
  labs(x = "Average population density", y = "Density across DHS clusters",
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
  labs(x = "Average(log) population density", y = "Density (log) across DHS clusters",
       title = "Population density (log) across DHS clusters", color="Year") +
  scale_color_discrete(labels = function(x) gsub(".*?(\\d{4})$", "\\1", x)) +
  theme_bw()


log_avg_pop_dens_density
ggsave("./figures/log_avg_pop_dens_density.png",log_avg_pop_dens_density, width=6, height = 4, dpi=300,
       bg="white", units="in")





