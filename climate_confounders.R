install.packages("ncdf4")

library(ncdf4)
library(ggplot2)

rm(list=ls())

# Open the netCDF file
nc <- nc_open("./data/ClimateResearchUnit/cru_ts4.07.1991.2000.pre.dat.nc")

# Read the data
data <- ncvar_get(nc, "lat","lon","time","var")

# Close the netCDF file
nc_close(nc)

# Plot the data
ggplot(data = data.frame(value = data)) +
  geom_histogram(aes(x = value), bins = 30, fill = "blue", color = "black") +
  labs(x = "Values", y = "Frequency", title = "Histogram of netCDF data")
