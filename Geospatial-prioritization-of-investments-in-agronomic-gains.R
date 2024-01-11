# Prioritization of investments in agronomic gains
# Lavinia Madaga and Chris Mwungu
# 20th December 2023
rm(list = ls())

library(terra)
library(sf)
library(scales)
#library(viridis)
library(tmap)
library(tmaptools)
#library(stars)
#library(rnaturalearth)
library(gridExtra)
library(grid)
library(dplyr)

dir <- "/cluster01/workspace/common/fao/gaez4/data/"
outdir <- "/home/cmiyinzi/prioritization/"

# read Non-OECD countries shape file
countries <- vect("/home/cmiyinzi/prioritization/country_farming_system_filtered_2.shp")
# read CGIAR shape file
cgair <-  vect("/home/cmiyinzi/prioritization/cgiar_regions.shp")

# Importing potential yield
dir2 <- file.path(dir, "res02")
py <- list.files(dir2, "yld", recursive = TRUE, full.names = TRUE)

# historical yield
pyh <- grep("CRUTS32", py, value = TRUE)
pyhl <- grep("8110L", pyh, value = TRUE)
# low yield
crops_pr <- c("barley","maize","millet","rice","sorghum","wheat") 
pyhl <- grep("*200b_yld.tif$", pyhl, value = TRUE)
pr <- grep("maiz|barl|mllt|ricw|sorg|whea", pyhl, value = TRUE)
potyld  <- rast(pr)

# Actual yield
dir6 <- file.path(dir, "res06")
ay <- list.files(dir6, "2010_yld", recursive = TRUE, full.names = TRUE)
crops_ay <- c("barley","millet","maize","rice","sorghum","wheat")
ay2 <- grep("mze|brl|mlt|rcw|srg|whe", ay, value = TRUE)
ay3 <- grep("T", ay2, value = TRUE)
actualyld <- rast(ay3)

# Renaming tiff files with consistent crops names
names(potyld) <- crops_pr 
names(actualyld) <- crops_ay

#Computing yield gap difference and ratio
getYldGap <- function(crop_i){
  actualyldcrop <- subset(actualyld, crop_i)
  potyldcrop <- subset(potyld, crop_i)
  actualyldcrop <- terra::app(actualyldcrop, function(x) ifelse(is.na(x), 0, x))
  potyldcrop <- terra::app(potyldcrop, function(x) ifelse(is.na(x), 0, x))
  yldgapdiff <- (potyldcrop - actualyldcrop) * 0.1
  yldgapratio <- actualyldcrop / potyldcrop
  or <- c(yldgapdiff, yldgapratio)
  names(or) <- paste0(crop_i, "_", c("yldgapdiff", "yldgapratio"))
  for (i in 1:nlyr(or)) {
    single_layer <- or[[i]]
    filename <- paste0(outdir, "/", names(or)[i], ".tif")
    writeRaster(single_layer, filename, overwrite = TRUE)
  }
}
# Apply function to each crop
lapply(crops_pr, getYldGap)

#Testing the Tiff files 
#Barley 
barleyyieldgap_10percent_low <- rast("/home/cmiyinzi/prioritization/barley_yldgapdiff.tif")
plot(barleyyieldgap_10percent_low)
#Millet
milletyieldgap_10percent_low <- rast("/home/cmiyinzi/prioritization/millet_yldgapdiff.tif")
plot(milletyieldgap_10percent_low)
#Maize
maizeyieldgap_10percent_low <- rast("/home/cmiyinzi/prioritization/maize_yldgapdiff.tif")
plot(maizeyieldgap_10percent_low)
#Sorghum
sorghumyieldgap_10percent_low <- rast("/home/cmiyinzi/prioritization/sorghum_yldgapdiff.tif")
plot(sorghumyieldgap_10percent_low)
#Rice 
riceyieldgap_10percent_low <- rast("/home/cmiyinzi/prioritization/rice_yldgapdiff.tif")
plot(riceyieldgap_10percent_low)
#Wheat 
wheatyieldgap_10percent_low <- rast("/home/cmiyinzi/prioritization/wheat_yldgapdiff.tif")
plot(wheatyieldgap_10percent_low)

#Computing Calories
low_Barley_KCAL <- barleyyieldgap_10percent_low * ((3.55)*1000000)
low_Maize_KCAL <- maizeyieldgap_10percent_low * ((2.73)*1000000)
low_Rice_KCAL <- riceyieldgap_10percent_low * ((3.82)*1000000)
low_Sorghum_KCAL <- sorghumyieldgap_10percent_low * ((0.91)*1000000)
low_Wheat_KCAL <- wheatyieldgap_10percent_low * ((3.59)*1000000)
# Total kcal for all crops
allspat <- c(low_Barley_KCAL, low_Maize_KCAL, low_Rice_KCAL, low_Sorghum_KCAL, low_Wheat_KCAL)
Total_KCAL_Per_Pixel_low <- app(allspat, fun = function(x) sum(x, na.rm = TRUE))

#Cropping to only remain with Global South and Non OECD Countries 
masked_Barley_KCAL <- mask(low_Barley_KCAL, countries)
masked_Maize_KCAL <- mask(low_Maize_KCAL, countries)
masked_Rice_KCAL <- mask(low_Rice_KCAL, countries)
masked_Sorghum_KCAL <- mask(low_Sorghum_KCAL, countries)
masked_Wheat_KCAL <- mask(low_Wheat_KCAL, countries)
masked_Total_KCAL_Per_Pixel_low <- mask(Total_KCAL_Per_Pixel_low, countries)

countries_sf <- st_as_sf(countries)
invalid <- !st_is_valid(countries_sf)
if (any(invalid)) {
  countries_sf <- st_make_valid(countries_sf)
}
#####################################

# Define the list of crops and their respective data
crops <- list(
  Barley = masked_Barley_KCAL,
  Maize = masked_Maize_KCAL,
  Rice = masked_Rice_KCAL,
  Sorghum = masked_Sorghum_KCAL,
  Wheat = masked_Wheat_KCAL,
  "All Crops" = masked_Total_KCAL_Per_Pixel_low
)
# Loop through each crop and generate a plot
for (crop_name in names(crops)) {
  # Apply the NA transformation
  crop_data <- app(crops[[crop_name]], function(x) ifelse(x == 0, NA, x))
  
  # Create the plot
  tm <- tm_shape(crop_data, raster.downsample = FALSE) +
    tm_raster(style = "cont", midpoint = NA, palette = "-RdYlBu", 
              title = "KCAL Per Pixel (Low Input)") +
    tm_shape(countries_sf) +
    tm_borders(col = "gray70", lwd = 0.5) +
    tm_layout(
      main.title = paste(crop_name, "KCAL Per Pixel (Low)"),
      main.title.position = "center",
      main.title.size = 1.0,  # Adjust main title size
      legend.title.size = 0.8,  # Adjust legend title size
      frame = FALSE,
      outer.margins = c(0,0,0,0),
      inner.margins = c(0,0,0.05,0.05),
      fontfamily = "Arial",
      legend.position = c("left", "bottom"),
      legend.text.size = 0.6  # Adjust legend text size
    )
  
  # Save the plot
  file_name <- paste("/home/cmiyinzi/prioritization/", crop_name, "_KCAL_Plot.png", sep = "")
  tmap_save(tm, filename = file_name, dpi = 300)
}
#################

# Farming Systems Analysis #####################################################
#Total_KCAL_Per_Pixel_low
#CGIR REGIONS
#FARMING SYTEMS (NON OECD)
# RANKING 


# Define CGIAR regions
lac <- data.frame(cgregion = "LAC", ISO3 = c("MEX", "COL", "GTM", "BLZ", "SLV", "NIC", "HND", "CRI", "PAN", "CUB", "HTI", "JAM", "DOM", "BHS", "PRI", "VIR", "VCT", "DMA", "BRB", "TTO", "GRD", "TCA", "COL", "ECU", "VEN", "GUY", "SUR", "BRA", "PER", "ARG", "BOL", "CHL", "PRY", "URY"))
wca <- data.frame(cgregion = "WCA", ISO3 = c("MRT", "SEN", "MLI", "NER", "NGA", "TCD", "GNB", "GIN", "SLE", "LBR", "CIV", "TGO", "GHA", "BEN", "CMR", "GNQ", "GAB", "COG", "COD", "AGO", "BFA", "CAF", "GMB"))
esa <- data.frame(cgregion = "ESA", ISO3 = c("TZA", "KEN", "SSD", "ERI", "ETH", "SOM", "DJI", "RWA", "BDI", "UGA", "ZMB", "MOZ", "MWI", "MDG", "ZWE", "NAM", "BWA", "SWZ", "LSO", "ZAF", "SYC", "COM", "SLB"))
cwana <- data.frame(cgregion = "CWANA", ISO3 = c("SDN", "EGY", "YEM", "MAR", "ESH", "DZA", "TUN", "LBY", "SAU", "OMN", "ARE", "QAT", "KWT", "IRQ", "IRN", "JOR", "LBN", "SYR", "ISR", "PSE", "TUR", "GEO", "AZE", "ARM", "TKM", "UZB", "KGZ", "KAZ", "TJK", "AFG"))
sa <- data.frame(cgregion = "SA", ISO3 = c("IND", "PAK", "BGD", "NPL", "LKA", "BTN"))
sea <- data.frame(cgregion = "SEA", ISO3 = c("CHN", "MMR", "THA", "LAO", "VNM", "KHM", "IDN", "PHL", "MYS", "TLS", "PNG", "BRN"))

# Filter data for each CGIAR region (e.g., LAC)
shp_lac <- countries[countries$iso3 %in% lac$ISO3, ]
shp_wca <- countries[countries$iso3 %in% wca$ISO3, ]
shp_esa <- countries[countries$iso3 %in% esa$ISO3, ]
shp_cwana <- countries[countries$iso3 %in% cwana$ISO3, ]
shp_sa <- countries[countries$iso3 %in% sa$ISO3, ]
shp_sea <- countries[countries$iso3 %in% sea$ISO3, ]

#Extract raster values for each farming system
kcal_values_lac <- terra::extract(Total_KCAL_Per_Pixel_low, shp_lac, fun = mean, na.rm = TRUE)
kcal_values_wca <- extract(Total_KCAL_Per_Pixel_low, shp_wca, fun = mean, na.rm = TRUE)
kcal_values_esa <- extract(Total_KCAL_Per_Pixel_low, shp_esa, fun = mean, na.rm = TRUE)
kcal_values_cwana <- extract(Total_KCAL_Per_Pixel_low, shp_cwana, fun = mean, na.rm = TRUE)
kcal_values_sa <- extract(Total_KCAL_Per_Pixel_low, shp_sa, fun = mean, na.rm = TRUE)
kcal_values_sea <- extract(Total_KCAL_Per_Pixel_low, shp_sea, fun = mean, na.rm = TRUE)


# Merge the Kcal summary with shp based on the common "ID" column
merge_kcal_with_shp <- function(region_name, shp_df, kcal_values) {
  sf_result <- 
    st_as_sf(shp_df) %>% 
    mutate(ID := seq_len(nrow(.))) %>% 
    left_join(., kcal_values, by = "ID") %>% 
    mutate(cgregion = region_name)  # Add region column
  
  return(sf_result)
}

#LAC
Kcal_farmsys_lac_sf <- merge_kcal_with_shp("LAC", shp_lac, kcal_values_lac)
#WCA
Kcal_farmsys_wca_sf <- merge_kcal_with_shp("WCA", shp_wca, kcal_values_wca)
#ESA
Kcal_farmsys_esa_sf <- merge_kcal_with_shp("ESA", shp_esa, kcal_values_esa)
#CWANA
Kcal_farmsys_cwana_sf <- merge_kcal_with_shp("CWANA", shp_cwana, kcal_values_cwana)
#SA
Kcal_farmsys_sa_sf <- merge_kcal_with_shp("SA", shp_sa, kcal_values_sa)
#SEA
Kcal_farmsys_sea_sf <- merge_kcal_with_shp("SEA", shp_sea, kcal_values_sea)

plot(Kcal_farmsys_sea_sf[13])

#rank farming systems within each of the CGIAR’s 6 regions
#LAC
LAC_ranking <- Kcal_farmsys_lac_sf %>%
  group_by(DESCRIPTIO) %>%
  summarise(mean_Kcal = mean(lyr.1, na.rm=TRUE)) %>% 
  arrange(desc(mean_Kcal))

#WCA
WCA_ranking <- Kcal_farmsys_wca_sf %>%
  group_by(DESCRIPTIO) %>%
  summarise(mean_Kcal = mean(lyr.1, na.rm=TRUE)) %>% 
  arrange(desc(mean_Kcal))

# Check and fix invalid geometries
if (!all(st_is_valid(Kcal_farmsys_esa_sf))) {
  Kcal_farmsys_esa_sf <- st_make_valid(Kcal_farmsys_esa_sf)
}

#ESA
ESA_ranking <- Kcal_farmsys_esa_sf %>%
  group_by(DESCRIPTIO) %>%
  summarise(mean_Kcal = mean(lyr.1, na.rm=TRUE)) %>% 
  arrange(desc(mean_Kcal))

#CWANA
CWANA_ranking <- Kcal_farmsys_cwana_sf %>%
  group_by(DESCRIPTIO) %>%
  summarise(mean_Kcal = mean(lyr.1, na.rm=TRUE)) %>% 
  arrange(desc(mean_Kcal))

#SA
SA_ranking <- Kcal_farmsys_sa_sf %>%
  group_by(DESCRIPTIO) %>%
  summarise(mean_Kcal = mean(lyr.1, na.rm=TRUE)) %>% 
  arrange(desc(mean_Kcal))

# Check and fix invalid geometries
if (!all(st_is_valid(Kcal_farmsys_sea_sf))) {
  Kcal_farmsys_sea_sf <- st_make_valid(Kcal_farmsys_sea_sf)
}

#SEA
SEA_ranking <- Kcal_farmsys_sea_sf %>%
  group_by(DESCRIPTIO) %>%
  summarise(mean_Kcal = mean(lyr.1, na.rm=TRUE)) %>% 
  arrange(desc(mean_Kcal))

#save each region as csv 
SEA_ranking_df <- as.data.frame(SEA_ranking)
SA_ranking_df <- as.data.frame(SA_ranking)
CWANA_ranking_df <- as.data.frame(CWANA_ranking)
WCA_ranking_df <- as.data.frame(WCA_ranking)
LAC_ranking_df <- as.data.frame(LAC_ranking)
ESA_ranking_df <- as.data.frame(ESA_ranking)

# Set the file name for the CSV file
csv_file <- "SEA_ranking.csv"
csv_file1 <- "SA_ranking.csv"
csv_file2 <- "CWANA_ranking.csv"
csv_file3 <- "WCA_ranking.csv"
csv_file4 <- "LAC_ranking.csv"
csv_file5 <- "ESA_ranking.csv"

# Write the data frame to a CSV file without the geometry column in the working directory
write.csv(SEA_ranking_df[, -ncol(SEA_ranking_df)], file.path(getwd(), csv_file), row.names = FALSE)
write.csv(SA_ranking_df[, -ncol(SA_ranking_df)], file.path(getwd(), csv_file1), row.names = FALSE)
write.csv(CWANA_ranking_df[, -ncol(CWANA_ranking_df)], file.path(getwd(), csv_file2), row.names = FALSE)
write.csv(WCA_ranking_df[, -ncol(WCA_ranking_df)], file.path(getwd(), csv_file3), row.names = FALSE)
write.csv(LAC_ranking_df[, -ncol(LAC_ranking_df)], file.path(getwd(), csv_file4), row.names = FALSE)
write.csv(ESA_ranking_df[, -ncol(ESA_ranking_df)], file.path(getwd(), csv_file5), row.names = FALSE)








