setwd("H:\\Prioritization areas for Agronomic Investment\\Prioritization_Cereals_2")

library(terra)
library(sf)

#read potential and actual yld datasets (stacked)
#low input
low_potential_yield <- rast("low_potential_yield_cereals_stacked.tif")
low_potential_yield
plot(low_potential_yield)

#read actual yield data
actual_yield <- rast("Actual_yield_cereals_stacked.tif")
actual_yield
plot(actual_yield)


# Subtract actual yield from potential yield
#Yield gap 
low_yield_gap <- low_potential_yield - actual_yield
low_yield_gap
plot(low_yield_gap)

writeRaster(low_yield_gap, "low_yield_gap.tif", overwrite = TRUE)

# Calculate 10% of the low yield gap
yield_gap_10_percent_low <- low_yield_gap * 0.1
yield_gap_10_percent_low
plot(yield_gap_10_percent_low)

writeRaster(yield_gap_10_percent_low, "low_yield_gap_10_percent.tif", overwrite = TRUE)


#Convert additional production to kcal for all the crops
# factors - Calories per tonne for each crop from FAO
low_Maize_KCAL <- yield_gap_10_percent_low$maiz200b_yld_low*3580802.60

low_Rice_KCAL <- yield_gap_10_percent_low$ricw200b_yld_low*2800000

low_Sorghum_KCAL <- yield_gap_10_percent_low$sorg200b_yld_low*3430000.40

low_Wheat_KCAL <- yield_gap_10_percent_low$whea200b_yld_low*3284000.00


writeRaster(low_Maize_KCAL, "low_Maize_KCAL_L.tif", overwrite = TRUE)
writeRaster(low_Rice_KCAL, "low_Rice_KCAL_L.tif", overwrite = TRUE)
writeRaster(low_Sorghum_KCAL, "low_Sorghum_KCAL_L.tif", overwrite = TRUE)
writeRaster(low_Wheat_KCAL, "low_Wheat_KCAL_L.tif", overwrite = TRUE)

calories_per_ha_low <- list.files(pattern="._L.tif$")
rr <- rast(calories_per_ha_low)
plot(rr)

writeRaster(rr, "calories_per_ha_low_stacked.tif", overwrite = TRUE)

# Replace missing values crops_KCAL with zeros
low_Maize_KCAL[is.na(low_Maize_KCAL)] <- 0
low_Rice_KCAL[is.na(low_Rice_KCAL)] <- 0
low_Sorghum_KCAL[is.na(low_Sorghum_KCAL)] <- 0
low_Wheat_KCAL[is.na(low_Wheat_KCAL)] <- 0

# Sum the kcal across N crops
Total_KCAL_Per_Pixel_low <- low_Maize_KCAL + low_Rice_KCAL + low_Sorghum_KCAL + low_Wheat_KCAL  
plot(Total_KCAL_Per_Pixel_low, main="Sum of Calories per Hectare for Cereals (low input)")


writeRaster(Total_KCAL_Per_Pixel_low, "low_Total_KCAL_Per_Pixel.tif", overwrite = TRUE)

plot(rr[[4]], main="Calories per Hectare for Maize (low input)")

# read the now oecd countries shapefile
countries <- st_read("Global_south_countries.shp")


# Visualize with adjusted text size
plot(rr[[1]], 
     breaks=c(-Inf, 0, 1000, 10000, 100000, 500000, 1000000,  Inf), 
     main="Calories per Hectare for Maize (low input)", 
     cex.axis=0.01, cex.lab=0.01)

# Add country borders without filling color
plot(countries, add=TRUE, col=NA, lwd=1)



plot(Total_KCAL_Per_Pixel_low, 
     breaks=c(-Inf, 0, 10000, 50000, 100000, 500000, 1000000, Inf), 
     main="Sum of Calories per Hectare for Cereals (low input)", 
     cex.axis=0.01, cex.lab=0.01)

# Add country borders without filling color
plot(countries, add=TRUE, col=NA, lwd=1)
