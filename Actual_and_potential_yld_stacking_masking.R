#clipping the potential and actual yield to global south
setwd("H:\\Prioritization areas for Agronomic Investment\\Prioritization_Cereals_2")

library(terra)
library(sf)


#read non oecd countries shapefile
Global_south_countries <- terra::vect("H:\\Prioritization areas for Agronomic Investment\\Prioritization_Cereals\\Non_OECD_countries_farmsys.shp")

#potential
mylist <- list.files(pattern="._.tif$")
mylist

#Read all potential crops
potential_cereals <- rast(mylist)


# Crop potential yield in the global south 
potential_yield_cereals <- terra::mask(potential_cereals, Global_south_countries)

# Save the clipped potential yield
writeRaster(potential_yield_cereals, "potential_yield_cereals_stacked.tif", overwrite = TRUE)


#clipping actual yield
#High input
highlist_actual <- list.files(pattern="._high.tif$")
highlist_actual

#Read all actual crops
high_Actual_cereals <- rast(highlist_actual)

# Crop potential yield in the global south 
high_Actual_yield_cereals <- terra::mask(high_Actual_cereals, Global_south_countries)
high_Actual_yield_cereals

# Convert values of actual yield from kg/ha to MT/ha
high_Actual_yld_mt_per_ha <- high_Actual_yield_cereals / 1000

# Save the result to a new raster file
writeRaster(high_Actual_yld_mt_per_ha, "high_Actual_yield_cereals_stacked.tif", overwrite = TRUE)

#Low input
lowlist_actual <- list.files(pattern="._low.tif$")
lowlist_actual

#Read all actual crops
low_Actual_cereals <- rast(lowlist_actual)
low_Actual_cereals

# Crop potential yield in the global south 
low_Actual_yield_cereals <- terra::mask(low_Actual_cereals, Global_south_countries)
low_Actual_yield_cereals

# Convert values of actual yield from kg/ha to MT/ha
low_Actual_yld_mt_per_ha <- low_Actual_yield_cereals / 1000

# Save the result to a new raster file
writeRaster(low_Actual_yld_mt_per_ha, "low_Actual_yield_cereals_stacked.tif", overwrite = TRUE)



