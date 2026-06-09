### Read libraries -----
library(tidyverse)
library(geodata)
library(terra)   
library(sf)
library(tidyterra)

### Read datasets -----

## Full model dataset
df_4_top_model <- read_csv("Data/data_4model.csv")

## Model dataset as sf - Local Texas Centric Albers Projection (EPSG:3081)
df_projected <- df_4_top_model %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
  st_transform("EPSG:3081") %>%
  mutate(
    lon_tx = st_coordinates(geometry)[, 1],
    lat_tx = st_coordinates(geometry)[, 2]
  )

## Load SpatRasters
pop_den_2020 <- rast("Data/NA_PopulationDensity_2020.tif")

jan_2020 <- rast('Data/wc2.1_cruts4.06_2.5m_tmin_2020-2021/wc2.1_2.5m_tmin_2020-01.tif')
feb_2020 <- rast('Data/wc2.1_cruts4.06_2.5m_tmin_2020-2021/wc2.1_2.5m_tmin_2020-02.tif')
jan_2021 <- rast('Data/wc2.1_cruts4.06_2.5m_tmin_2020-2021/wc2.1_2.5m_tmin_2021-01.tif')
feb_2021 <- rast('Data/wc2.1_cruts4.06_2.5m_tmin_2020-2021/wc2.1_2.5m_tmin_2021-02.tif')

## Load land cover
rast_lc <- rast("C:/Users/Benson-Amram Lab/Desktop/Nathan/NA_NALCMS_landcover_2020v2_30m.tif") 

### Create model for prediction -----

mod <- glm(melanic_binary ~ pop_den_scaled + winter_temp_scaled + prop_forest_scaled + 
             introduced + pop_den_scaled:introduced + winter_temp_scaled:introduced + 
             pop_den_scaled:winter_temp_scaled + pop_den_scaled:prop_forest_scaled + RAC_20km,
           family = binomial(link = "logit"),
           data = df_4_top_model,
           na.action = "na.fail")


### Project across Cactus, Texas Region -----

## Define bounding box centered around Cactus, Texas (Moore County & Surrounding High Plains)
# Longitude range: -102.3 (West of Dumas/Cactus) to -101.7 (East of Cactus toward Stratford plains)
# Latitude range:  35.8 (South of Dumas) to 36.3 (North of Cactus up to the Sherman County line)
cactus_bbox <- st_bbox(c(xmin = -102.3, ymin = 35.8, xmax = -101.7, ymax = 36.3), 
                       crs = st_crs(4326))

# Convert bounding box to local Texas Centric Albers (EPSG:3081)
region_poly_projected <- st_as_sfc(cactus_bbox) %>% 
  st_transform("EPSG:3081")

# Turn it into a terra SpatVector for clipping
region_vect <- vect(region_poly_projected)

## Create the regional template raster (1km grid)
grid_template <- rast(region_vect, res = 1000) 

### Extract human population density values -----

# 1. Project the Texas bounding box back to the exact CRS of the global population raster
pop_crs_vect <- project(region_vect, crs(pop_den_2020))

# 2. Crop the global population raster FIRST while it is still in its native format.
# This prevents interpolation artifacts over small areas.
pop_cropped_native <- crop(pop_den_2020, pop_crs_vect)

# 3. Project the small, cropped chunk into your Texas Albers template grid.
# Using 'bilinear' or 'near' ensures cells fill smoothly without generating NAs.
pop_grid <- project(pop_cropped_native, grid_template, method = "bilinear")
names(pop_grid) <- "population_density" 

## Convert population raster to data frame
df_pred_melanism <- as.data.frame(pop_grid, xy = TRUE, na.rm = TRUE)

# Double Check: If your data frame is completely empty, it means 'na.rm = TRUE' 
# dropped everything because the raster was all NAs. If it has rows now, it worked!
print(head(df_pred_melanism))


### Extract average winter daily minimum temperature for the Texas Panhandle -----

tmin_stack <- c(jan_2020, feb_2020, jan_2021, feb_2021)

# Convert WGS84 bounding box directly to a SpatVector for early tmin cropping
region_vect_wgs84 <- vect(st_as_sfc(cactus_bbox))
winter_stack_cropped <- crop(tmin_stack, region_vect_wgs84)

# Calculate the mean and project to the 1km regional grid
winter_low_daily_avg <- mean(winter_stack_cropped)
temp_1km <- project(winter_low_daily_avg, pop_grid, method = "bilinear")

# Extract to dataframe
v_points <- vect(df_pred_melanism, geom = c("albers_x", "albers_y"), crs = crs(pop_grid))
temp_values <- terra::extract(temp_1km, v_points)
df_pred_melanism$avg_winter_daily_low <- temp_values[, 2]


### Extract Localized Forest Cover -----

# Transform Texas vector to landcover projection before cropping
tx_lc_crs <- project(region_vect, crs(rast_lc))
rast_lc_cropped <- crop(rast_lc, ext(tx_lc_crs))

# Break the file pointer link to clear RAM
values(rast_lc_cropped) <- values(rast_lc_cropped)

# Reclassify (1:6 are NALCMS forest types)
m <- c(0, 0, 0,
       1, 6, 1,   
       6, 20, 0)  
rcl_matrix <- matrix(m, ncol=3, byrow=TRUE)
forest_mask <- classify(rast_lc_cropped, rcl_matrix, right=TRUE)

# Aggregate from 30m up to ~1km and project
forest_prop_30m <- aggregate(forest_mask, fact = 33, fun = "mean")
forest_1km <- project(forest_prop_30m, pop_grid, method = "bilinear")
forest_values <- terra::extract(forest_1km, v_points)
df_pred_melanism$prop_forest <- forest_values[, 2]


### Re-scale grid data to match baseline model scaling safely -----

# Scaling locked to the original RAW un-scaled variables from df_4_top_model
pop_mean <- mean(df_4_top_model$population_density, na.rm = TRUE) 
pop_sd   <- sd(df_4_top_model$population_density, na.rm = TRUE)

temp_mean <- mean(df_4_top_model$avg_winter_low_temp, na.rm = TRUE) 
temp_sd   <- sd(df_4_top_model$avg_winter_low_temp, na.rm = TRUE)

forest_mean <- mean(df_4_top_model$prop_forest, na.rm = TRUE)
forest_sd   <- sd(df_4_top_model$prop_forest, na.rm = TRUE)

df_pred_melanism$pop_den_scaled     <- (df_pred_melanism$population_density - pop_mean) / pop_sd
df_pred_melanism$winter_temp_scaled <- (df_pred_melanism$avg_winter_daily_low - temp_mean) / temp_sd
df_pred_melanism$prop_forest_scaled <- (df_pred_melanism$prop_forest - forest_mean) / forest_sd

# Introduced settings
df_pred_melanism$introduced <- "Y"
df_pred_melanism$RAC_20km <- 0


### Project probability of melanism -----

df_pred_melanism$prob_melanic <- predict(mod, newdata = df_pred_melanism, type = "response")

pred_v <- vect(df_pred_melanism, geom = c("albers_x", "albers_y"), crs = "EPSG:3081")
prob_raster <- rasterize(pred_v, pop_grid, field = "prob_melanic")


### Generate Plot Map -----

# Filter training points down to the local Texas Panhandle bounding box
df_regional_points <- df_projected %>% 
  st_filter(region_poly_projected)

proj_map <- ggplot() +
  geom_spatraster(data = prob_raster) + 
  scale_fill_gradient(
    low = "lightgrey",
    high = "black",
    limits = c(0, 1),
    na.value = "transparent"
  ) + 
  theme_bw() + 
  labs(
    x = "Easting (Texas Albers)",
    y = "Northing (Texas Albers)",
    fill = "Probability of Melanism",
    title = "Projected Probability of Melanism: Cactus, Texas & Vicinity"
  ) + 
  geom_point(data = df_regional_points, aes(x = lon_tx, y = lat_tx, col = as.factor(melanic_binary))) + 
  scale_color_manual(
    values = c("0" = "lightgreen",
               "1" = "black"),
    labels = c("0" = "Grey",
               "1" = "Black"),
    name = "Colour Morph"
  ); proj_map

# Save plot
ggsave("Figures/melanism_projection_CactusTX_no_RAC_2026.tiff", proj_map, dpi = "retina")