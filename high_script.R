setwd("H:\\Prioritization areas for Agronomic Investment\\Prioritization_Cereals_2")

library(terra)
library(sf)

#read potential and actual yld datasets (stacked)
potential_yield <- rast("potential_yield_cereals_stacked.tif")
potential_yield
plot(potential_yield)

#read actual yield data
#High input
high_actual_yield <- rast("high_Actual_yield_cereals_stacked.tif")
high_actual_yield
plot(high_actual_yield)

# Subtract actual yield from potential yield
#Yield gap
high_yield_gap <- potential_yield - high_actual_yield
plot(high_yield_gap)

writeRaster(high_yield_gap, "high_yield_gap.tif", overwrite = TRUE)

# Calculate 10% of the high yield gap
yield_gap_10_percent_high <- high_yield_gap * 0.1
yield_gap_10_percent_high
plot(yield_gap_10_percent_high)

writeRaster(yield_gap_10_percent_high, "high_yield_gap_10_percent.tif", overwrite = TRUE)


#Convert additional production to kcal for all the crops
# factors - Calories per tonne for each crop from FAO
high_Maize_KCAL <- yield_gap_10_percent_high$Maize_*3580802.60

high_Rice_KCAL <- yield_gap_10_percent_high$Rice_*2800000

high_Sorghum_KCAL <- yield_gap_10_percent_high$Sorghum_*3430000.40

high_Wheat_KCAL <- yield_gap_10_percent_high$Wheat_*3284000.00


writeRaster(high_Maize_KCAL, "high_Maize_KCAL.tif", overwrite = TRUE)
writeRaster(high_Rice_KCAL, "high_Rice_KCAL.tif", overwrite = TRUE)
writeRaster(high_Sorghum_KCAL, "high_Sorghum_KCAL.tif", overwrite = TRUE)
writeRaster(high_Wheat_KCAL, "high_Wheat_KCAL.tif", overwrite = TRUE)

calories_per_ha_high <- list.files(pattern="._KCAL.tif$")
r <- rast(calories_per_ha_high)
plot(r)

writeRaster(r, "calories_per_ha_high_stacked.tif", overwrite = TRUE)


# Sum the kcal across N crops
Total_KCAL_Per_Pixel_high <- high_Maize_KCAL + high_Rice_KCAL + high_Sorghum_KCAL + high_Wheat_KCAL
Total_KCAL_Per_Pixel_high
plot(Total_KCAL_Per_Pixel_high)

# read the now oecd countries shapefile
countries <- st_read("Global_south_countries.shp")

plot(r[[4]])

# Visualize with adjusted text size
plot(r[[4]], 
     breaks=c(0, 500, 1000, 10000, 50000, 100000, 500000, Inf), 
     main="Calories per Hectare for Wheat (high input)", 
     cex.axis=0.01, cex.lab=0.01)

# Add country borders without filling color
plot(countries, add=TRUE, col=NA, lwd=1)

#sum of all crops plot
plot(Total_KCAL_Per_Pixel_high, main="Sum of Calories per Hectare for Cereals (high input)")

plot(Total_KCAL_Per_Pixel_high, 
     breaks=c(0, 500, 1000, 10000, 50000, 100000, 500000, Inf), 
     main="Sum of Calories per Hectare for Cereals (high input)", 
     cex.axis=0.01, cex.lab=0.01)

