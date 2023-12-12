setwd("H:\\Prioritization areas for Agronomic Investment\\Prioritization_Cereals_2")

library(terra)
library(sf)

#read potential yield data
#High input
high_potential_yield <- rast("high_Potential_yield_cereals_stacked.tif")
high_potential_yield
plot(high_potential_yield)

#read potential and actual yld datasets (stacked)
actual_yield <- rast("Actual_yield_cereals_stacked.tif")
actual_yield
plot(actual_yield)

# Subtract actual yield from potential yield
#Yield gap
high_yield_gap <- high_potential_yield - actual_yield 
plot(high_yield_gap)

writeRaster(high_yield_gap, "high_yield_gap.tif", overwrite = TRUE)

# Calculate 10% of the high yield gap
yield_gap_10_percent_high <- high_yield_gap * 0.1
yield_gap_10_percent_high
plot(yield_gap_10_percent_high)

writeRaster(yield_gap_10_percent_high, "high_yield_gap_10_percent.tif", overwrite = TRUE)


#Convert additional production to kcal for all the crops
# factors - Calories per tonne for each crop from FAO
high_Maize_KCAL <- yield_gap_10_percent_high$maiz200b_yld_high*3580802.60

high_Rice_KCAL <- yield_gap_10_percent_high$ricw200b_yld_high*2800000

high_Sorghum_KCAL <- yield_gap_10_percent_high$sorg200b_yld_high*3430000.40

high_Wheat_KCAL <- yield_gap_10_percent_high$whea200b_yld_high*3284000.00

writeRaster(high_Maize_KCAL, "high_Maize_KCAL.tif", overwrite = TRUE)
writeRaster(high_Rice_KCAL, "high_Rice_KCAL.tif", overwrite = TRUE)
writeRaster(high_Sorghum_KCAL, "high_Sorghum_KCAL.tif", overwrite = TRUE)
writeRaster(high_Wheat_KCAL, "high_Wheat_KCAL.tif", overwrite = TRUE)

calories_per_ha_high <- list.files(pattern="._KCAL.tif$")
r <- rast(calories_per_ha_high)
plot(r)

writeRaster(r, "calories_per_ha_high_stacked.tif", overwrite = TRUE)


# Replace missing values crops_KCAL with zeros
high_Maize_KCAL[is.na(high_Maize_KCAL)] <- 0
high_Rice_KCAL[is.na(high_Rice_KCAL)] <- 0
high_Sorghum_KCAL[is.na(high_Sorghum_KCAL)] <- 0
high_Wheat_KCAL[is.na(high_Wheat_KCAL)] <- 0


# calculate Total_KCAL_Per_Pixel_high
Total_KCAL_Per_Pixel_high <- high_Maize_KCAL + high_Rice_KCAL + high_Sorghum_KCAL + high_Wheat_KCAL
# Plot the updated Total_KCAL_Per_Pixel_high
plot(Total_KCAL_Per_Pixel_high)


# Sum the kcal across N crops
Total_KCAL_Per_Pixel_high <- high_Maize_KCAL + high_Rice_KCAL + high_Sorghum_KCAL
Total_KCAL_Per_Pixel_high
plot(Total_KCAL_Per_Pixel_high)

# read the now oecd countries shapefile
countries <- st_read("Global_south_countries.shp")

#plotting individual crops
plot(r[[1]], main="Calories per Hectare for Maize (high input)")
# Add country borders without filling color
plot(countries, add=TRUE, col=NA, lwd=1)

# Visualize with adjusted text size
plot(r[[4]], 
     breaks=c(-Inf ,0, 50000, 100000, 500000, Inf), 
     main="Calories per Hectare for Wheat (high input)", 
     cex.axis=0.01, cex.lab=0.01)

# Add country borders without filling color
plot(countries, add=TRUE, col=NA, lwd=1)

#sum of all crops plot
plot(Total_KCAL_Per_Pixel_high, main="Sum of Calories per Hectare for Cereals (high input)")

plot(Total_KCAL_Per_Pixel_high, 
     breaks=c(-Inf, 0, 10000, 50000, 100000, 500000, Inf), 
     main="Sum of Calories per Hectare for Cereals (high input)", 
     cex.axis=0.01, cex.lab=0.01)

