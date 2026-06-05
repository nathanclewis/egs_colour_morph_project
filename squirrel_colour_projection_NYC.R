### Project across New York City Metro Area

### Create model for prediction -----

## Create model
mod <- glm(melanic_binary ~ pop_den_scaled + winter_temp_scaled + prop_forest_scaled + 
             introduced + pop_den_scaled:introduced + winter_temp_scaled:introduced + 
             pop_den_scaled:winter_temp_scaled + pop_den_scaled:prop_forest_scaled + RAC_20km,
           family = binomial(link = "logit"),
           data = df_4_top_model,
           na.action = "na.fail")

### Define a bounding box for NYC and surrounding towns (NY, NJ, CT suburbs) -----
# Longitude range: approx -74.5 (West NJ) to -73.3 (East Long Island/CT)
# Latitude range: approx 40.4 (South NJ/Staten Island) to 41.3 (North Hudson Valley)
nyc_bbox_wgs84 <- st_bbox(c(xmin = -75.6, ymin = 39.8, xmax = -73.3, ymax = 41.5), 
                          crs = st_crs(4326))

# Convert the bounding box into a spatial polygon and project it to Albers Equal Area
nyc_poly_albers <- st_as_sfc(nyc_bbox_wgs84) %>% 
  st_transform("ESRI:102008")

# Turn it into a terra SpatVector for clipping
nyc_vect <- vect(nyc_poly_albers)

## Create the regional template raster (1km grid)
grid_template <- rast(nyc_vect, res = 1000) 

### Extract human population density values -----
pop_1km <- project(pop_den_2020, grid_template, method = "sum")
pop_grid <- mask(pop_1km, nyc_vect)
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


### Extract average winter daily minimum temperature for NYC -----

tmin_stack <- c(jan_2020, feb_2020, jan_2021, feb_2021)

# Convert WGS84 bounding box directly to a SpatVector for early tmin cropping
nyc_vect_wgs84 <- vect(st_as_sfc(nyc_bbox_wgs84))
winter_stack_cropped <- crop(tmin_stack, nyc_vect_wgs84)

# Calculate the mean and project to the 1km regional grid
winter_low_daily_avg <- mean(winter_stack_cropped)
temp_1km <- project(winter_low_daily_avg, pop_grid, method = "bilinear")

# Extract to dataframe
v_points <- vect(df_pred_melanism, geom = c("albers_x", "albers_y"), crs = crs(pop_grid))
temp_values <- terra::extract(temp_1km, v_points)
df_pred_melanism$avg_winter_daily_low <- temp_values[, 2]


### Extract Localized Forest Cover (High Efficiency) -----

# Transform NYC vector to landcover projection before cropping
nyc_lc_crs <- project(nyc_vect, crs(rast_lc))
rast_lc_cropped <- crop(rast_lc, ext(nyc_lc_crs))

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

# CRITICAL: Keep scaling calculations locked to the original baseline model data frame (df_4_top_model)
# Do not calculate mean/sd from the NYC subset, or it will alter the meaning of your coefficients!
pop_mean <- mean(df_4_top_model$pop_den_scaled, na.rm = TRUE) 
pop_sd   <- sd(df_4_top_model$pop_den_scaled, na.rm = TRUE)

temp_mean <- mean(df_4_top_model$avg_winter_low_temp, na.rm = TRUE) 
temp_sd   <- sd(df_4_top_model$avg_winter_low_temp, na.rm = TRUE)

forest_mean <- mean(df_4_top_model$prop_forest_scaled, na.rm = TRUE)
forest_sd   <- sd(df_4_top_model$prop_forest_scaled, na.rm = TRUE)

df_pred_melanism$pop_den_scaled     <- (df_pred_melanism$population_density - pop_mean) / pop_sd
df_pred_melanism$winter_temp_scaled <- (df_pred_melanism$avg_winter_daily_low - temp_mean) / temp_sd
df_pred_melanism$prop_forest_scaled <- (df_pred_melanism$prop_forest - forest_mean) / forest_sd

df_pred_melanism$RAC_20km <- 0 
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
    low = "lightgrey",
    high = "black",
    limits = c(0,1),
    na.value = "transparent"
  ) +
  theme_bw() +
  labs(
    x = "Longitude",
    y = "Latitude",
    fill = "Probability of Melanism"
    #) +
    #geom_point(data = df_projected, aes(x = lon_albers, y = lat_albers, col = as.factor(melanic_binary))) +
    #scale_color_manual(
    #  values = c("0" = "lightgreen",
    #             "1" = "black"),
    #  labels = c("0" = "Grey",
    #             "1" = "Black"),
    #  name = "Colour Morph"
  ); proj_map

# Save plot
#ggsave("Figures/melanism_projection_NYC_June5_2026.tiff", proj_map, dpi = "retina")

