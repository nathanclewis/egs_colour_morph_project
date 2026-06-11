##### Range-wide projection for probability of melanism for the squirrel colour morphs project

#### Script Info/Instructions -----

{
  ## Start-up Instructions
  # 1. Pull from github first
  # 2. Read libraries second (after downloading them on first use)
  # 3. Read all datasets third
  # 4. Push changes to github regularly and before closing RStudio
  
  ## Script Format
  ##### Script Description
  #### Script Instructions/Details
  ### Section Header
  ## Description of single code chunk
  # Description of single line
}

### Read libraries -----

library(tidyverse)
library(geodata)
library(terra)
library(sf)
library(tidyterra)

### Read datasets -----

{
## Full model dataset
df_4_top_model <- read_csv("Data/data_4model.csv")

## Range map
native_range <- read_sf("Data/EGS_nativerange.shp")%>%
  mutate(native = "Y") %>%
  dplyr::select(native)

## Model dataset as sf with Albers Equal Area projection
df_projected <- df_4_top_model %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
  st_filter(native_range, .predicate = st_intersects) %>%
  st_transform("ESRI:102008") %>%
  mutate(
    lon_albers = st_coordinates(st_transform(geometry, "ESRI:102008"))[, 1],
    lat_albers = st_coordinates(st_transform(geometry, "ESRI:102008"))[, 2])

## Load human population density data
pop_den_2020 <- rast("Data/NA_PopulationDensity_2020.tif")

## Average winter daily low temperature
jan_2020 <- rast('Data/wc2.1_cruts4.06_2.5m_tmin_2020-2021/wc2.1_2.5m_tmin_2020-01.tif')
feb_2020 <- rast('Data/wc2.1_cruts4.06_2.5m_tmin_2020-2021/wc2.1_2.5m_tmin_2020-02.tif')
jan_2021 <- rast('Data/wc2.1_cruts4.06_2.5m_tmin_2020-2021/wc2.1_2.5m_tmin_2021-01.tif')
feb_2021 <- rast('Data/wc2.1_cruts4.06_2.5m_tmin_2020-2021/wc2.1_2.5m_tmin_2021-02.tif')

## Load land cover
rast_lc <- rast("C:/Users/Benson-Amram Lab/Desktop/Nathan/NA_NALCMS_landcover_2020v2_30m.tif") 

}

### Create model for prediction -----

## Create model
mod <- glm(melanic_binary ~ pop_den_scaled + winter_temp_scaled + prop_forest_scaled + 
             introduced + pop_den_scaled:introduced + winter_temp_scaled:introduced + 
             pop_den_scaled:winter_temp_scaled + pop_den_scaled:prop_forest_scaled + RAC_20km,
           family = binomial(link = "logit"),
           data = df_4_top_model,
           na.action = "na.fail")

### Create grid cells -----

## Project the MCP to Albers Equal Area 
native_projected <- st_transform(native_range, "ESRI:102008")

## Make the MCP a SpatVector
native_vect <- vect(native_projected)

## Create a template raster
grid_template <- rast(native_vect, res = 1000) # 1000m = 1km

### Extract human population density values -----

pop_1km <- project(pop_den_2020, grid_template, method = "bilinear")
pop_grid <- mask(pop_1km, native_vect)

# Dynamically set the name to match downstream data frames
names(pop_grid) <- "population_density" 

## Convert population raster to a data frame
df_pred_melanism <- as.data.frame(pop_grid, xy = TRUE, na.rm = TRUE)

## Get Lat/Lon coordinates
coords_projected <- df_pred_melanism[, c("x", "y")]
v <- vect(coords_projected, geom = c("x", "y"), crs = crs(pop_grid))
v_latlon <- project(v, "EPSG:4326")

df_pred_melanism$longitude <- crds(v_latlon)[, 1]
df_pred_melanism$latitude  <- crds(v_latlon)[, 2]

colnames(df_pred_melanism)[1:2] <- c("albers_x", "albers_y")


### Extract average winter daily minimum temperature -----

# Correct way to stack multiple SpatRasters in terra is using c()
tmin_stack <- c(jan_2020, feb_2020, jan_2021, feb_2021)

# Match CRS of native range to the temperature stack for early cropping
native_wgs84 <- vect(native_range)
winter_stack_cropped <- crop(tmin_stack, native_wgs84)

# Calculate the mean across the layers (January & February across both years)
winter_low_daily_avg <- mean(winter_stack_cropped)

# Project to your 1km Albers template grid
temp_1km <- project(winter_low_daily_avg, pop_grid, method = "bilinear")

# Extract to dataframe
v_points <- vect(df_pred_melanism, geom = c("albers_x", "albers_y"), crs = crs(pop_grid))
temp_values <- terra::extract(temp_1km, v_points)

# terra::extract returns a matrix/df where column 1 is the ID and column 2 is the value
df_pred_melanism$avg_winter_daily_low <- temp_values[, 2]


### Extract Forest Cover (30m to 1km) -----

# 1. Transform native range to landcover projection to crop before doing heavy math
native_lc_crs <- project(native_vect, crs(rast_lc))
rast_lc_cropped <- crop(rast_lc, native_lc_crs)

# 2. Reclassify the NALCMS landcover map into binary Forest (1) vs Non-forest (0)
# NALCMS Forest codes generally range from 1 to 6 (Coniferous, Deciduous, Mixed)
# Check your specific metadata, but 1:6 is standard for NALCMS forest classes.
m <- c(0, 0, 0,
       1, 6, 1,   # Classes 1 through 6 become 1 (Forest)
       6, 20, 0)  # Everything else becomes 0
rcl_matrix <- matrix(m, ncol=3, byrow=TRUE)
forest_mask <- classify(rast_lc_cropped, rcl_matrix, right=TRUE)

# 3. Aggregate 30m to ~1km to massively scale down size before projecting
# 1000m / 30m ≈ 33. We will use a factor of 33 and take the mean to calculate "proportion"
forest_prop_30m <- aggregate(forest_mask, fact = 33, fun = "mean")

# 4. Project the lightweight proportion raster to your target template
forest_1km <- project(forest_prop_30m, pop_grid, method = "bilinear")
forest_values <- terra::extract(forest_1km, v_points)
df_pred_melanism$prop_forest <- forest_values[, 2]

### Set RAC to mean -----

# Calculate mean RAC from model df
mean_RAC <- mean(df_4_top_model$RAC_20km)

# Set all values to mean in projection dataset
df_pred_melanism$RAC_20km <- mean_RAC

### Re-scale grid data to match the model scaling -----

## Extract mean and SD from the complete squirrel dataset

pop_mean <- mean(df_4_top_model$weighted_pop_density, na.rm = TRUE)
pop_sd <- sd(df_4_top_model$weighted_pop_density, na.rm = TRUE)

temp_mean <- mean(df_4_top_model$avg_winter_low_temp, na.rm = TRUE)
temp_sd <- sd(df_4_top_model$avg_winter_low_temp, na.rm = TRUE)

forest_mean <- mean(df_4_top_model$prop_forest, na.rm = TRUE)
forest_sd <- sd(df_4_top_model$prop_forest, na.rm = TRUE)

## Apply scaling to the grid variables

df_pred_melanism$pop_den_scaled <- (df_pred_melanism$population_density - pop_mean) / pop_sd
df_pred_melanism$winter_temp_scaled <- (df_pred_melanism$avg_winter_daily_low - temp_mean) / temp_sd
df_pred_melanism$prop_forest_scaled <- (df_pred_melanism$prop_forest - forest_mean) / forest_sd

## Add introduced status
df_pred_melanism$introduced <- "N"

## Save dataset
#write_csv(df_pred_melanism, "C:/Users/Benson-Amram Lab/Desktop/Nathan/native_proj_dataset.csv")

### Project probability of melanism -----

## Predict probabilities

df_pred_melanism$prob_melanic <- predict(mod, newdata = df_pred_melanism, type = "response")

## Create a SpatVector from the predicted data
pred_v <- vect(df_pred_melanism, geom = c("albers_x", "albers_y"), crs = "ESRI:102008")

## Rasterize the probabilities
prob_raster <- rasterize(pred_v, pop_grid, field = "prob_melanic")

## Save raster of probabilities
writeRaster(prob_raster, 
            filename = "Data/melanism_probability_1km.tif", 
            gdal = c("COMPRESS=LZW", "TFW=YES"), # LZW compression + creates a world file
            overwrite = TRUE)

### Visualize the projection -----

## Read raster
prob_raster <- rast("Data/melanism_probability_1km.tif")

## Plot
proj_map <- ggplot() +
  geom_spatraster(data = prob_raster) +
  scale_fill_gradient(
    low = "grey95",
    high = "black",
    limits = c(0,1),
    na.value = "transparent"
  ) +
  theme_bw() +
  theme(axis.text = element_text(size = 20),
        axis.title = element_text(size = 20),
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 20)) +
  labs(
    x = "Longitude",
    y = "Latitude",
    fill = "Probability of Melanism"); proj_map

  # Save plot
  ggsave("Figures/melanism_projection_June10_2026.tiff", proj_map, dpi = "retina")
  