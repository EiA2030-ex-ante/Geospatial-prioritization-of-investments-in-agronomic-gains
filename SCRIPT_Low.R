setwd("H:\\Prioritization areas for Agronomic Investment\\Prioritization_Cereals_2")

library(terra)
library(sf)

#read potential and actual yld datasets (stacked)
potential_yield <- rast("potential_yield_cereals_stacked.tif")
potential_yield

#read actual yield data
#low
low_actual_yield <- rast("low_Actual_yield_cereals_stacked.tif")
low_actual_yield

# Subtract actual yield from potential yield
#Yield gain 
low_yield_gap <- potential_yield - low_actual_yield

writeRaster(low_yield_gap, "low_yield_gap.tif", overwrite = TRUE)

# Calculate 10% of the low yield gap
yield_gap_10_percent_low <- low_yield_gap * 0.1
yield_gap_10_percent_low


writeRaster(yield_gap_10_percent_low, "yield_gap_10_percent_low.tif", overwrite = TRUE)


#Convert additional production to kcal for all the crops
# factors - Calories per tonne for each crop from FAO
low_Maize_KCAL <- yield_gap_10_percent_low$maize_*3580802.60

low_Rice_KCAL <- yield_gap_10_percent_low$rice_*2800000

low_Sorghum_KCAL <- yield_gap_10_percent_low$sorghum_*3430000.40

low_Wheat_KCAL <- yield_gap_10_percent_low$wheat_*3284000.00


writeRaster(low_Maize_KCAL, "low_Maize_KCAL_L.tif", overwrite = TRUE)
writeRaster(low_Rice_KCAL, "low_Rice_KCAL_L.tif", overwrite = TRUE)
writeRaster(low_Sorghum_KCAL, "low_Sorghum_KCAL_L.tif", overwrite = TRUE)
writeRaster(low_Wheat_KCAL, "low_Wheat_KCAL_L.tif", overwrite = TRUE)

calories_per_ha_low <- list.files(pattern="._L.tif$")
rr <- rast(calories_per_ha_low)

writeRaster(rr, "calories_per_ha_low_stacked.tif", overwrite = TRUE)


# Sum the kcal across N crops
Total_KCAL_Per_Pixel_low <- low_Maize_KCAL + low_Rice_KCAL + low_Sorghum_KCAL + low_Wheat_KCAL
writeRaster(Total_KCAL_Per_Pixel_low, "Total_KCAL_Per_Pixel_low.tif", overwrite = TRUE)

plot(rr[[4]])

# read the now oecd countries shapefile
countries <- st_read("non_oecd_countries.shp")



# Visualize with adjusted text size
plot(rr[[4]], 
     breaks=c(0, 10000, 50000, 100000, 500000, 1000000, Inf), 
     main="Calories per Hectare for wheat (low input)", 
     cex.axis=0.01, cex.lab=0.01)

# Add country borders without filling color
plot(countries, add=TRUE, col=NA, lwd=1)

#sum of all crops plot
plot(Total_KCAL_Per_Pixel_low)

plot(Total_KCAL_Per_Pixel_low, 
     breaks=c(0, 10000, 50000, 100000, 500000, 1000000, Inf), 
     main="Calories per Hectare for Cereals (low input)", 
     cex.axis=0.01, cex.lab=0.01)


