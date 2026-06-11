### Project across the Greater Toronto Area (GTA) 

### Read libraries -----
library(tidyverse)
library(geodata)
library(terra)   
library(sf)
library(tidyterra)

### Read datasets -----

## Full model dataset
df_4_top_model <- read_csv("Data/data_4model.csv")

## Model dataset as sf - CHANGED to ESRI:102008 
df_projected <- df_4_top_model %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
  st_transform("ESRI:102008") %>%
  mutate(
    lon_albers = st_coordinates(geometry)[, 1],
    lat_albers = st_coordinates(geometry)[, 2]
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

## Create model
mod <- glm(melanic_binary ~ pop_den_scaled + winter_temp_scaled + prop_forest_scaled + 
             introduced + pop_den_scaled:introduced + winter_temp_scaled:introduced + 
             pop_den_scaled:winter_temp_scaled + pop_den_scaled:prop_forest_scaled + RAC_20km,
           family = binomial(link = "logit"),
           data = df_4_top_model,
           na.action = "na.fail")

### GTA Bounding Box (WGS84 Coordinates) -----
gta_bbox <- st_bbox(c(xmin = -79.75, ymin = 43.5, xmax = -79, ymax = 44), 
                    crs = st_crs(4326))

# Convert the bounding box into a spatial polygon and project it to Albers Equal Area
region_poly_albers <- st_as_sfc(gta_bbox) %>% 
  st_transform("ESRI:102008")

# Turn it into a terra SpatVector for clipping
region_vect <- vect(region_poly_albers)

## Create the regional template raster (1km grid)
grid_template <- rast(region_vect, res = 1000) 

### Extract human population density values
pop_1km <- project(pop_den_2020, grid_template, method = "bilinear")
pop_grid <- mask(pop_1km, region_vect)
names(pop_grid) <- "population_density" 

## Convert population raster to the localized data frame
df_pred_melanism <- as.data.frame(pop_grid, xy = TRUE, na.rm = TRUE)

## Get Lat/Lon coordinates for the localized grid
coords_projected <- df_pred_melanism[, c("x", "y")]
v <- vect(coords_projected, geom = c("x", "y"), crs = crs(pop_grid))
v_latlon <- project(v, "EPSG:4326")

df_pred_melanism$longitude <- crds(v_latlon)[, 1]
df_pred_melanism$latitude  <- crds(v_latlon)[, 2]
colnames(df_pred_melanism)[1:2] <- c("albers_x", "albers_y")


### Extract average winter daily minimum temperature for the GTA -----

tmin_stack <- c(jan_2020, feb_2020, jan_2021, feb_2021)

# Convert WGS84 bounding box directly to a SpatVector for early tmin cropping
region_vect_wgs84 <- vect(st_as_sfc(gta_bbox))
winter_stack_cropped <- crop(tmin_stack, region_vect_wgs84)

# Calculate the mean and project to the 1km regional grid
winter_low_daily_avg <- mean(winter_stack_cropped)
temp_1km <- project(winter_low_daily_avg, pop_grid, method = "bilinear")

# Extract to dataframe
v_points <- vect(df_pred_melanism, geom = c("albers_x", "albers_y"), crs = crs(pop_grid))
temp_values <- terra::extract(temp_1km, v_points)
df_pred_melanism$avg_winter_daily_low <- temp_values[, 2]

### Extract Localized Forest Cover (High Efficiency) -----

# Transform NYC vector to landcover projection before cropping
gta_lc_crs <- project(region_vect, crs(rast_lc))
rast_lc_cropped <- crop(rast_lc, ext(gta_lc_crs))

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


### Re-scale grid data to match the model scaling -----

pop_mean <- mean(df_4_top_model$weighted_pop_density, na.rm = TRUE) 
pop_sd   <- sd(df_4_top_model$weighted_pop_density, na.rm = TRUE)

temp_mean <- mean(df_4_top_model$avg_winter_low_temp, na.rm = TRUE) 
temp_sd   <- sd(df_4_top_model$avg_winter_low_temp, na.rm = TRUE)

forest_mean <- mean(df_4_top_model$prop_forest, na.rm = TRUE)
forest_sd   <- sd(df_4_top_model$prop_forest, na.rm = TRUE)

df_pred_melanism$pop_den_scaled     <- (df_pred_melanism$population_density - pop_mean) / pop_sd
df_pred_melanism$winter_temp_scaled <- (df_pred_melanism$avg_winter_daily_low - temp_mean) / temp_sd
df_pred_melanism$prop_forest_scaled <- (df_pred_melanism$prop_forest - forest_mean) / forest_sd

### Set RAC and native status -----

# Calculate mean RAC from model df
mean_RAC <- mean(df_4_top_model$RAC_20km)

# Set all values to mean in projection dataset
df_pred_melanism$RAC_20km <- mean_RAC

# Set all as native
df_pred_melanism$introduced <- "N"


### Project probability of melanism -----

df_pred_melanism$prob_melanic <- predict(mod, newdata = df_pred_melanism, type = "response")

pred_v <- vect(df_pred_melanism, geom = c("albers_x", "albers_y"), crs = "ESRI:102008")
prob_raster <- rasterize(pred_v, pop_grid, field = "prob_melanic")

### Visualize the projection -----

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
ggsave("Figures/melanism_projection_GTA_June10_2026.tiff", proj_map, dpi = "retina")
