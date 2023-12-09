setwd("H:\\Prioritization areas for Agronomic Investment\\Datasets")

# Load necessary libraries
library(sf)

# Read the shapefile
country_farming_system <- st_read("Potential_yld_cereals.shp")

# List of OECD countries
oecd_countries <- c(
  "Australia", "Austria", "Belgium", "Canada", "Chile", "Colombia", "Costa Rica", 
  "Czech Republic", "Denmark", "Estonia", "Finland", "France", "Germany", "Greece", 
  "Hungary", "Iceland", "Ireland", "Israel", "Italy", "Japan", "South Korea", 
  "Latvia", "Lithuania", "Luxembourg", "Mexico", "Netherlands", "New Zealand", 
  "Norway", "Poland", "Portugal", "Slovakia", "Slovenia", "Spain", "Sweden", 
  "Switzerland", "Turkey", "United Kingdom", "United States"
)

# Remove OECD countries and China
country_farming_system_filtered <- country_farming_system[!(country_farming_system$country %in% c(oecd_countries, "China")), ]

# Save the filtered shapefile
st_write(country_farming_system_filtered, "country_farming_system_filtered.shp")


