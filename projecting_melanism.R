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

### Read datasets -----

{
## Full model dataset
df_4_top_model <- read_csv("Data/data_4model.csv")

## Human population density
pop_den_2020 <- population(2020, res = 0.5, path = tempdir())

## Average winter daily low temperature
jan_2020 <- raster('Data/wc2.1_cruts4.06_2.5m_tmin_2020-2021/wc2.1_2.5m_tmin_2020-01.tif')
feb_2020 <- raster('Data/wc2.1_cruts4.06_2.5m_tmin_2020-2021/wc2.1_2.5m_tmin_2020-02.tif')
jan_2021 <- raster('Data/wc2.1_cruts4.06_2.5m_tmin_2020-2021/wc2.1_2.5m_tmin_2021-01.tif')
feb_2021 <- raster('Data/wc2.1_cruts4.06_2.5m_tmin_2020-2021/wc2.1_2.5m_tmin_2021-02.tif')

## Range map
native_range <- read_sf("Data/EGS_nativerange.shp")%>%
  mutate(native = "Y") %>%
  dplyr::select(native)
}

### Create most parsimonious model for prediction -----

## Create most parsimonious model
mod_parsimonious <- glm(melanic_binary ~ pop_den_scaled + winter_temp_scaled + introduced +
                        pop_den_scaled:introduced + winter_temp_scaled:introduced + RAC,
                        family = binomial(link = "logit"),
                        data = df_4_top_model,
                        na.action = "na.fail")

## Evaluate most parsimonious model
summary(mod_parsimonious)
r2(mod_parsimonious)

### Create minimum convex polygon (MCP) of all squirrel reports -----

## Convert to a spatial sf object
sf_final_dataset <- st_as_sf(df_4_top_model, coords = c("longitude", "latitude"), crs = 4326)

## Create the MCP
mcp <- sf_final_dataset %>%
  st_combine() %>% #count all points as one group to make a single polygon
  st_convex_hull() #create MCP

## Visualize MCP
plot(st_geometry(sf_final_dataset), col = "blue", pch = 16)
plot(mcp, add = TRUE, border = "red", lwd = 2)

### Create grid cells -----

## Project the MCP to Albers Equal Area (as established)
mcp_projected <- st_transform(mcp, "ESRI:102008")

## Make the MCP a SpatVector
mcp_vect <- vect(mcp_projected)

## Create a template raster
grid_template <- rast(mcp_vect, res = 1000) # 1000m = 1km

### Extract human population density values -----

## Project the Population Data
pop_1km <- project(pop_den_2020, grid_template, method = "sum")

## Clip to the MCP
pop_grid <- mask(pop_1km, mcp_vect)

## Visualize
plot(pop_grid, main = "Population Density (1km Grid)")

## Convert the final population raster to a data frame
df_pred_melanism <- as.data.frame(pop_grid, xy = TRUE, na.rm = TRUE)

## To get Lat/Lon, treat the X/Y points as a spatial object and transform them back to WGS84 (EPSG: 4326)
coords_projected <- df_pred_melanism[, c("x", "y")]

## Create a temporary SpatVector of the grid centers
v <- vect(coords_projected, geom = c("x", "y"), crs = crs(pop_grid))

## Transform to Lat/Lon
v_latlon <- project(v, "EPSG:4326")

## Add the Lat/Lon back to the main data frame
df_pred_melanism$longitude <- crds(v_latlon)[, 1]
df_pred_melanism$latitude  <- crds(v_latlon)[, 2]

## Rename the Albers columns to avoid confusion
colnames(df_pred_melanism)[1:2] <- c("albers_x", "albers_y")

### Extract average winter daily minimum temperature -----

## Convert the 'raster' objects to 'terra' objects
tmin_list <- list(jan_2020, feb_2020, jan_2021, feb_2021)
tmin_stack <- rast(lapply(tmin_list, rast))

## Crop the stack to the MCP
winter_stack_cropped <- crop(tmin_stack, mcp_vect)

## Calculate the mean on only the cropped area
winter_low_daily_avg <- mean(winter_stack_cropped)

## Project the temperature average to match your 1km Albers grid
# Use 'bilinear' here because temperature is a continuous gradient
temp_1km <- project(winter_low_daily_avg, pop_grid, method = "bilinear")

## Create a SpatVector of the points in df_pred_melanism
v_points <- vect(df_pred_melanism, geom = c("albers_x", "albers_y"), crs = crs(pop_grid))

## Extract the temperature values exactly at those point locations
temp_values <- terra::extract(temp_1km, v_points)

## Add the values to your data frame
df_pred_melanism$avg_winter_daily_low <- temp_values[, 2]

## Visualize
plot(temp_1km, main = "Average Winter Daily Minimum Temperature (C)")

### Classify cells as native/introduced -----

## Project range map to Albers as a terra object
range_proj <- st_transform(native_range, "ESRI:102008")

## Convert range map and data to terra objects
range_v <- vect(range_proj)
points_v <- vect(df_pred_melanism, geom = c("albers_x", "albers_y"), crs = "ESRI:102008")

## Classify the points as native/non-native
joined_status <- terra::extract(range_v, points_v)

## Added classifications to df
df_pred_melanism$introduced <- ifelse(is.na(joined_status$native), "Y", "N")

## Clean up to free up RAM
rm(points_v, range_v, joined_status)

### Calculate RAC -----

## Make a SpatVector from the training dataset
train_v <- vect(df_4_top_model, geom = c("longitude", "latitude"), crs = "EPSG:4326")

## Project train_v to Albers grid
train_v_proj <- project(train_v, "ESRI:102008")

## Interpolate the RAC values across the entire grid

RAC_raster <- interpIDW(pop_grid, train_v_proj, field = "RAC", near = 8, power = 4, radius = 10000)

## Extract RAC values for all cells
RAC_extracted_all <- values(RAC_raster, na.rm = FALSE)

## Extract the population values for the grid
all_pop_values <- values(pop_grid, na.rm = FALSE)

## Filter the RAC values using the same mask as for the pop data
RAC_extracted_subset <- RAC_extracted_all[!is.na(all_pop_values)]

## Add to the df
df_pred_melanism$RAC_extracted <- RAC_extracted_subset

### Re-scale grid data to match the model scaling -----

## Extract mean and SD from the complete squirrel dataset

pop_mean <- mean(df_4_top_model$population_density, na.rm = TRUE)
pop_sd <- sd(df_4_top_model$population_density, na.rm = TRUE)

temp_mean <- mean(df_4_top_model$avg_winter_low_temp, na.rm = TRUE)
temp_sd <- sd(df_4_top_model$avg_winter_low_temp, na.rm = TRUE)

RAC_mean <- mean(df_4_top_model$RAC, na.rm = TRUE)
RAC_sd <- sd(df_4_top_model$RAC, na.rm = TRUE)

## Apply scaling to the grid variables

df_pred_melanism$pop_den_scaled <- (df_pred_melanism$population_density - pop_mean) / pop_sd
df_pred_melanism$winter_temp_scaled <- (df_pred_melanism$avg_winter_daily_low - temp_mean) / temp_sd
df_pred_melanism$RAC <- (df_pred_melanism$RAC_extracted - RAC_mean) / RAC_sd

## Set all RAC values equal to 0
df_pred_melanism$RAC[is.na(df_pred_melanism$RAC)] <- 0
df_pred_melanism$RAC <- 0


### Project probability of melanism -----

## Predict probabilities

df_pred_melanism$prob_melanic <- predict(mod_parsimonious, newdata = df_pred_melanism, type = "response")

## Create a SpatVector from the predicted data
pred_v <- vect(df_pred_melanism, geom = c("albers_x", "albers_y"), crs = "ESRI:102008")

## Rasterize the probabilities
prob_raster <- rasterize(pred_v, pop_grid, field = "prob_melanic")

## Visualize
colours <- colorRampPalette(c("lightgrey","black"))(100)
plot(prob_raster, col = colours)
