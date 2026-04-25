##### Analyses and data visualizations for the squirrel colour morphs project

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
  
  ## df naming convention
  #Example df name: df_1_2_3_4
    #Position 1 = year
    #Position 2 = position in year's file of first image included
    #Position 3 = position in year's file of last image included
    #Position 4 = includes RGB colour data (if "col", RGBs are included, if
    #             missing, RGBs are not included)
  
  ## Instructions for clicking on squirrel photos
  # 1. When a squirrel pops up, determine whether the photo is an eastern grey 
  #    squirrel with a clear patch of fur for you to click a bounding box onto.
  # 2. If the squirrel does not match the above criteria, hit escape without 
  #    clicking on the squirrel. If you accidentally click on it, click 3+  more
  #    times and then hit escape to nullify the bounding box.
  # 3. If the squirrel does meet the criteria, click two points to form opposite
  #    corners on the bounding box in which the pixels will have their RGB 
  #    values extracted, then hit escape to proceed to the next photo.
  
  ## Data links
  # winter temps: https://www.worldclim.org/data/monthlywth.html
}

### Download packages -----

## For EBImage
{
  install.packages("BiocManager")
  BiocManager::install("EBImage")
}

### Load Packages and Settings -----

{ #run this line to read all packages
  library(tidyverse) #for tidyverse structure of coding using pipes
  library(furrr) #contains future_map_lgl, which speeds up rowwise check of URLs
  plan(multisession) #enable parallel processing to speed up certain processes
  library(EBImage) #for image processing
  library(ggplot2) #for tidy plot generation
  library(geodata) #for generating human pop den data
  library("raster") #for reading temperature data
  library(randomForest) #for randomForest
  library(caret) #for confusion matrices
  library(car) #for Anova
  library(visreg) #for visualizing model fit
  library(lme4) #for mixed models
  library(leaflet) #for mapping
  library(MuMIn) #for dredge
  library(performance) #for r^2
  library(mgcv) #for gam
  library(sf) #for working with shapefiles
  library(terra) #for working with spatial rasters
  library(spdep) #for Moran's I test of spatial autocorrelation
  }

### Read Squirrel Data -----

{ #run this line to read all data files
  
  df_full <- read_csv("Data/full_dataset_2019_2021.csv")
  
  df_final_dataset <- read_csv("Data/final_dataset.csv")
  
  ## Complete dataset (including RGBs) from 2019
  df_2019_completed <- read_csv("Data/sq_RGB_2019_1_17000.csv")
  
  ## Completed dataset (including RGBs) with 20,594 usable records from 2020
  df_2020_completed <- read_csv("Data/sq_RGB_2020_df_1_31535.csv") %>%
    dplyr::select(inat_id, observed_on, image_url, latitude.y, longitude.y, color_max_x, color_min_x, color_max_y, color_min_y, red, green, blue) %>%
    #remove records from outside North America
    filter(latitude.y > 13 & longitude.y < -51) %>%
    rename(id = inat_id, latitude = latitude.y, longitude = longitude.y)
  
  ## Complete dataset (including RGBs) from 2021
  df_2021_completed <- read_csv("Data/sq_RGB_2021_1_31413.csv") %>%
    dplyr::select(id, observed_on, image_url, latitude, longitude, color_max_x, color_min_x, color_max_y, color_min_y, red, green, blue) %>%
    #remove records from outside North America
    filter(latitude > 13 & longitude < -51)
  
  ## All 16,993 records from 2019
  df_2019 <- read_csv("Data/observations_2019.csv") %>%
    dplyr::select(id, observed_on, image_url, latitude, longitude) %>%
    #remove records from outside North America
    filter(latitude > 13 & longitude < -51)
  
  ## All 31,535 records from 2020
  df_2020 <- read_csv("Data/df_2020_complete_data.csv") %>%
    dplyr::select(inat_id, observed_on, image_url, latitude.y, longitude.y) %>%
    #remove records from outside North America
    filter(latitude.y > 13 & longitude.y < -51) %>%
    rename(id = inat_id, latitude = latitude.y, longitude = longitude.y)
  
  ## All 31,413 records from 2021
  df_2021 <- read_csv("Data/df_2021_complete_data.csv") %>%
    dplyr::select(id, observed_on, image_url, latitude, longitude) %>%
    #remove records from outside North America
    filter(latitude > 13 & longitude < -51)
  
  ## Squirrel Mapper data
  df_sq_mpr <- read_csv("Data/squirrelMapper_observations.csv") %>%
    rename(id = inat.id, sq_mpr_col = morph.class) %>%
    #remove images with >1 squirrel
    group_by(id) %>%
    mutate(num_sq = n()) %>%
    ungroup() %>%
    filter(num_sq == 1 & !is.na(id)) %>%
    dplyr::select(id, sq_mpr_col)
  }

### Test Image URLs and remove rows with invalid URLs -----

## Create a function that identifies invalid image URLs
url_check = function(url_in,t=2){
  con <- url(url_in)
  check <- suppressWarnings(try(open.connection(con,open="rt",timeout=t),silent=T)[1])
  suppressWarnings(try(close.connection(con),silent=T))
  ifelse(is.null(check),TRUE,FALSE)
  }  

## Choose a subset to process and remove invalid URLs (also removes invalid file type: .gif)
df_2019_noerrors <- df_2019 %>%
  slice(16751:17000) %>%
  filter(!str_detect(image_url, "gif$")) %>%
  mutate(valid_url = future_map_lgl(image_url, url_check)) %>%
  filter(valid_url == TRUE)

### Coordinate extraction -----

## Function that extracts coordinates from a picture
locate_box = function(image_url){
  display(readImage(image_url), 
          temp(),
          method = "raster", all = TRUE)
  locator()
}

## Apply it to a short list
df_2019_16751_17000 = df_2019_noerrors %>%
  #slice() %>%
  rowwise() %>%
  mutate(picture_info = list(locate_box(image_url))) %>%
  #remove images without two clicks
  filter(length(picture_info$x) == 2) %>%
  ungroup() %>%
  mutate(sq_location = map(picture_info,
                           ~ c(sort(.[[1]], decreasing = T), 
                               sort(.[[2]], decreasing = T)))) %>% 
  unnest_wider(sq_location, names_sep = "_") %>% 
  # keeps the sq_location to check if code is correct
  dplyr::rename(color_max_x = sq_location_1,
                color_min_x = sq_location_2,
                color_max_y = sq_location_3,
                color_min_y = sq_location_4) %>% 
  mutate(across(starts_with("color"), round))

### Extract RGB values -----

## Function to extract mean RGBs from an image given the coordinates
extract_mean_colour = function(image, xmin, xmax, ymin, ymax){
  readImage(image)[xmin:xmax, ymin:ymax, ] %>% 
    apply(3, mean)
}

## Apply extract colour functions and create columns for red, green, and blue values
df_2019_16751_17000_col <- df_2019_16751_17000 %>%
  mutate(mean_rgb = future_pmap(
    list(image_url, color_min_x, color_max_x, color_min_y, color_max_y),
    ~ extract_mean_colour(..1, ..2, ..3, ..4, ..5)
  )) %>% 
  unnest_wider(mean_rgb, names_sep = "_") %>%
  rename(red = mean_rgb_1,
         green = mean_rgb_2,
         blue = mean_rgb_3) %>%
  dplyr::select(-c(valid_url, picture_info, mean_rgb_4))

### Add new df to existing df -----

## Generate complete dataset
df_2019_new <- df_2019_completed %>%
  rbind(df_2019_16751_17000_col) #insert name of newly created df here

## Write new csv. Always change the last number in the name to match the highest
## number clicked through to date before writing
write_csv(df_2019_new, "Data/sq_RGB_2019_1_17000.csv")

## Compile data from all years into master df
df_colour <- df_2019_completed %>%
  rbind(df_2020_completed) %>%
  rbind(df_2021_completed) %>%
  #include a new ID column to match with the popden and temp dfs
  mutate(ID = row_number())

### Generate human population density data -----

{
## Set spatial boundaries
max_lat <- 72.04
min_lat <- 13.89
max_lon <- -51.58
min_lon <- -160.60

## Load human population density data
pop_den_2020 <- population(2020, res = 0.5, path = tempdir())

## Map data
plot(pop_den_2020^0.1,
     xlim = c(min_lon, max_lon),
     ylim = c(min_lat, max_lat),
     axes = TRUE)

#Add points to the map
points(df_colour$longitude, df_colour$latitude, pch=19, cex=0.01, col=1)

#Extracting points from method directly above
df_popden <- raster::extract(pop_den_2020,
                                    df_colour[c("longitude","latitude")],
                                    df=TRUE)
}

### Generate winter temperature data -----

{
## Load average daily minimum temp data for each month
jan_2020 <- raster('Data/wc2.1_cruts4.06_2.5m_tmin_2020-2021/wc2.1_2.5m_tmin_2020-01.tif')
feb_2020 <- raster('Data/wc2.1_cruts4.06_2.5m_tmin_2020-2021/wc2.1_2.5m_tmin_2020-02.tif')
jan_2021 <- raster('Data/wc2.1_cruts4.06_2.5m_tmin_2020-2021/wc2.1_2.5m_tmin_2021-01.tif')
feb_2021 <- raster('Data/wc2.1_cruts4.06_2.5m_tmin_2020-2021/wc2.1_2.5m_tmin_2021-02.tif')

## Extracting points from method directly above
df_temps_jan20 <- raster::extract(jan_2020,
                                  df_colour[c("longitude","latitude")],
                                  df=TRUE) %>%
  rename(temp_jan20 = wc2.1_2.5m_tmin_01)
df_temps_jan21 <- raster::extract(jan_2021,
                                  df_colour[c("longitude","latitude")],
                                  df=TRUE) %>%
  rename(temp_jan21 = wc2.1_2.5m_tmin_01)
df_temps_feb20 <- raster::extract(feb_2020,
                                  df_colour[c("longitude","latitude")],
                                  df=TRUE) %>%
  rename(temp_feb20 = wc2.1_2.5m_tmin_02)
df_temps_feb21 <- raster::extract(feb_2020,
                                  df_colour[c("longitude","latitude")],
                                  df=TRUE) %>%
  rename(temp_feb21 = wc2.1_2.5m_tmin_02)
}

### Identify reports in native range -----

## Load range map
range <- read_sf("Data/EGS_nativerange.shp")

## Plot map
ggplot(range) +
  geom_sf() +
  geom_point(data = df_col_class, aes(x = longitude, y = latitude, col = col_class)) +
  theme_bw()

## Assign native status to range
range_native <- range %>%
  mutate(native = "Y")

## Create sf for reports
sf_reports <- st_as_sf(
  df_colour,
  coords = c("longitude", "latitude"),
  crs = 4326,   # WGS84
  remove = FALSE
)

## Combine reports and native range objects
df_reports_native <- st_join(sf_reports, range_native, join = st_within) %>%
  st_drop_geometry() %>%
  mutate(native = ifelse(is.na(native), "N", native)) %>%
  as.data.frame() %>%
  dplyr::select(1:13,42)

### Upload land cover data -----

## Read land cover dataset
df_LC <- read_csv("Data/NL_sq_LC_data.csv") %>%
  rename(id = ID) %>%
  mutate(forest = TEMPERATE_OR_SUBPOLAR_NEEDLEAF_FOREST + SUBPOLAR_TAIGA_NEEDLELEAF_FOREST + TROPICAL_OR_SUBTROPICAL_BROADLEAF_EVERGREEN_FOREST + TROPICAL_OR_SUBTROPICAL_BROADLEAF_DECIDUOUS_FOREST + TEMPERATE_OR_SUBPOLAR_BROADLEAF_DECIDUOUS_FOREST + MIXED_FOREST,
         shrubland = TROPICAL_OR_SUBTROPICAL_SHRUBLAND + TEMPERATE_OR_SUBPOLAR_SHRUBLAND + SUBPOLAR_OR_POLAR_SHRUBLAND_LICHEN_MOSS,
         grassland = TROPICAL_OR_SUBTROPICAL_GRASSLAND + TEMPERATE_OR_SUBPOLAR_GRASSLAND + SUBPOLAR_OR_POLAR_GRASSLAND_LICHEN_MOSS,
         barren = BARREN_LAND + SUBPOLAR_OR_POLAR_BARREN_LICHEN_MOSS,
         wetland = WETLAND,
         cropland = CROPLAND,
         developed = URBAN_AND_BUILT_UP
         ) %>%
  dplyr::select(-c(A_))

### Compile complete dataset -----

df_colour_popden_temp <- df_colour %>%
  full_join(c(df_popden, df_temps_jan20, df_temps_feb20, df_temps_jan21, df_temps_feb21, df_reports_native), copy = TRUE) %>%
  dplyr::select(-c(ID.1,ID.2,ID.3,ID.4)) %>%
  mutate(avg_winter_low_temp = rowMeans(across(c(temp_jan20, temp_feb20, temp_jan21, temp_feb21)), na.rm = TRUE)) %>%
  left_join(df_LC) %>%
  left_join(df_sq_mpr)

#write_csv(df_colour_popden_temp, "Data/full_dataset_2019_2021.csv")

### Train and test random forest (k-fold cv) -----

## Create filtered df
df_4ml <- df_full %>%
  dplyr::select(red, green, blue, sq_mpr_col) %>%
  na.omit() %>%
  #filter(sq_mpr_col %in% c('gray', 'melanic')) %>%
  mutate(sq_mpr_col = factor(sq_mpr_col))

## Define cross-validation method
train_control <- trainControl(
  method = "cv",
  number = 10, # k = 10 folds
  #sampling = "down", # Downsample to balance classes within each fold
  verboseIter = TRUE # Shows progress
)

## Train the model
set.seed(1234) # for reproducibility
rf_model_cv <- train(
  sq_mpr_col ~ red + green + blue,
  data = df_4ml,
  method = "rf", # "rf" is the code for the randomForest package
  trControl = train_control,
  na.action = na.pass,
  # Add importance for variable importance plots later
  importance = TRUE
)

## Results
print(rf_model_cv)
confusionMatrix(rf_model_cv)

### Predict colour morphs -----

## Keep only rgb columns
df_rgb <- df_full %>%
  dplyr::select(red, green, blue)

## Predict all values
predicted_col_morphs <- predict(rf_model_cv, newdata = df_rgb)

## Add predictions back into the original data
df_col_class <- df_full %>%
  mutate(
    col_class = as.character(predicted_col_morphs)) %>%
  filter(col_class %in% c('melanic', 'gray'))

## Check full set of predictions vs. sq mpr
table(df_col_class$sq_mpr_col, df_col_class$col_class)

### Logistic regression -----

## Reduce df
df_4model <- df_col_class %>%
  mutate(melanic_binary = ifelse(col_class == "melanic", 1, ifelse(col_class == "gray", 0, NA)),
         melanic_binary = factor(melanic_binary),
         introduced = ifelse(native == "Y", "N", "Y"),
         total_LC = forest + shrubland + grassland + barren + wetland + cropland + developed,
         prop_forest = forest/total_LC,
         prop_developed = developed/total_LC) %>%
  dplyr::select(id, latitude, longitude, population_density, avg_winter_low_temp, introduced, prop_forest, prop_developed, col_class, melanic_binary) %>%
  na.omit()

## Add log-centred versions of each predictor
df_4model$pop_den_scaled <- scale(df_4model$population_density) %>% as.vector
df_4model$winter_temp_scaled <- scale(df_4model$avg_winter_low_temp) %>% as.vector
df_4model$prop_forest_scaled <- scale(df_4model$prop_forest) %>% as.vector
df_4model$prop_developed_scaled <- scale(df_4model$prop_developed) %>% as.vector

#write_csv(df_4model, "Data/final_dataset.csv")

## Create model
mod <- glm(melanic_binary ~ pop_den_scaled + winter_temp_scaled + introduced +
             prop_forest_scaled + prop_developed_scaled + pop_den_scaled:introduced +
             winter_temp_scaled:introduced + pop_den_scaled:winter_temp_scaled +
             pop_den_scaled:prop_developed_scaled + prop_forest_scaled:prop_developed_scaled +
             RAC,
                family = binomial(link = "logit"),
                data = df_final_dataset,
                na.action = "na.fail")

## Evaluate model
summary(mod)
Anova(mod)
confint(mod)
r2(mod)
visreg(mod, scale = "response")
vif(mod)
cor(df_4model[11:14], method = "pearson")

# Calculate model residuals
mod_resid <- residuals(mod, type = "deviance")
mod_resid_rac <- residuals(mod, type = "deviance")

## Evaluate spatial autocorrelation with Moran's I

# Find all neighbors within the specified distance range (currently within 1 lat/lon)
coords <- as.matrix(df_final_dataset[,c("longitude", "latitude")])
nb <- dnearneigh(coords, d1 = 0, d2 = 0.25)

# Convert the neighbor object to a spatial weights list
# The style="W" argument row-standardizes the weights (each row sums to 1)
# 'zero.policy=TRUE' allows for data points that might have no neighbors
listw <- nb2listw(nb, style = "W", zero.policy = TRUE)

# Calculate the spatially lagged residuals (the autocovariate)
rac_term <- lag.listw(listw, mod_resid)

# Add RAC term to final dataset
df_final_dataset$RAC <- rac_term

# Run Moran's I test on the residuals 
moran_result <- moran.test(mod_resid_rac, 
                           listw = listw, 
                           zero.policy = TRUE)
print(moran_result)

### Plot raw data -----

## Human pop den
df_4model %>%
  mutate(col_class_binary = ifelse(col_class == "gray", 0,
                                   ifelse(col_class == "melanic", 1,
                                          NA))) %>%
  filter(!is.na(col_class_binary)) %>%
  ggplot(aes(x = population_density, y = col_class_binary)) +
  geom_point() +
  geom_smooth(method = glm) +
  labs(x = "Population Density", y = "Probability of Melanism") +
  theme_bw()

## Winter temperature
df_4model %>%
  mutate(col_class_binary = ifelse(col_class == "gray", 0,
                                   ifelse(col_class == "melanic", 1,
                                          NA))) %>%
  filter(!is.na(col_class_binary)) %>%
  ggplot(aes(x = avg_winter_low_temp, y = col_class_binary)) +
  geom_point() +
  geom_smooth(method = "glm") +
  labs(x = "Average Winter Temperature (C)", y = "Probability of Melanism") +
  theme_bw()

## Developed land
df_4model %>%
  mutate(col_class_binary = ifelse(col_class == "gray", 0,
                                   ifelse(col_class == "melanic", 1,
                                          NA))) %>%
  filter(!is.na(col_class_binary)) %>%
  ggplot(aes(x = prop_developed, y = col_class_binary)) +
  geom_point() +
  geom_smooth(method = "glm") +
  labs(x = "Developed Land Cover", y = "Probability of Melanism") +
  theme_bw()

## Forest cover
df_4model %>%
  mutate(col_class_binary = ifelse(col_class == "gray", 0,
                                   ifelse(col_class == "melanic", 1,
                                          NA))) %>%
  filter(!is.na(col_class_binary)) %>%
  ggplot(aes(x = forest, y = col_class_binary)) +
  geom_point() +
  geom_smooth(method = "glm") +
  labs(x = "Forest Cover", y = "Probability of Melanism") +
  theme_bw()

### Plot confidence intervals -----

## Plot confidence intervals
{
  # List of variables in each model
  variables <- c("Intercept", "Human Population Density", "Average Winter Temperature",
                         "Non-Native Range", "Forest Cover", "Developed Land Cover", "Residual Autocovariate",
                         "Population Density x Non-Native Range", "Winter Temperature x Non-Native Range",
                         "Population Density x Winter Temperature", "Population Density x Developed Land",
                         "Forest Cover x Developed Land")
  
  # List of coefficients
  coefficients <- c(-2.13, 0.19, -0.88, 0.98, -0.01, 0.04, 2.36, -0.20, 0.44, 0.09, 0.03, 0.09)
  
  # Desired plotting order
  var_levels <- c("Population Density x Developed Land",
                  "Population Density x Winter Temperature",
                  "Forest Cover x Developed Land",
                  "Population Density x Non-Native Range",
                  "Winter Temperature x Non-Native Range",
                  "Forest Cover",
                  "Developed Land Cover",
                  "Human Population Density",
                  "Average Winter Temperature",
                  "Non-Native Range",
                  "Residual Autocovariate",
                  "Intercept")
  
  # Build tidy CI table and plot
  p_conf_ints <- bind_rows(
    as_tibble(confint(mod)) %>%
      cbind(Variable = variables,
            Coefficient = coefficients) %>%
      pivot_longer(`2.5 %`:`97.5 %`,
                   names_to = "Level",
                   values_to = "CL")) %>%
    mutate(
      Variable = factor(Variable, levels = var_levels),
      y   = as.numeric(Variable)) %>%
    group_by(Variable, y) %>%
    summarise(
      Lower = min(CL),
      Upper = max(CL),
      Coefficient = unique(Coefficient),
      .groups = "drop"
    ) %>%
    ggplot(aes(y = y)) +
    geom_segment(aes(x = Lower, xend = Upper, yend = y),
                 linewidth = 2, lineend = "square") +
    geom_point(aes(x = Coefficient),
               size = 5, alpha = 0.5) +
    geom_vline(xintercept = 0, linetype = "dashed") +
    geom_hline(yintercept = c(5.5, 10.5)) +
    geom_text(
      data = tibble(
        x = c(-2.5, -2.5, -2.5),
        y = c(5.3, 10.3, 12.5),
        label = c("Interactions",
                  "Main Effects",
                  "Intercept and RAC")
      ),
      aes(x = x, y = y, label = label),
      inherit.aes = FALSE,
      size = 5,
      hjust = 0
    ) +
    scale_y_continuous(
      breaks = unique(as.numeric(factor(var_levels, levels = var_levels))),
      labels = var_levels
    ) +
    scale_x_continuous(breaks = c(-6, -4, -2, 0, 2, 4, 6)) +
    labs(x = "95% Confidence Intervals", y = "") +
    theme_classic() +
    theme(
      legend.position = "right",
      axis.text = element_text(size = 18),
      axis.title = element_text(size = 18),
      legend.text = element_text(size = 18),
      legend.title = element_blank()); p_conf_ints
  }

### Map data -----

# Create the base map
leaflet(df_final_dataset) %>%
  addTiles() %>%
  # Add MELANIC squirrels as one group
  addCircles(
    data = subset(df_final_dataset, col_class == "melanic"),
    ~longitude, ~latitude,
    color = "black",
    group = "Melanic",
    popup = "Melanic Squirrel",
    fillOpacity = 0.5, radius = 10, stroke = FALSE
  ) %>%
  # Add GREY squirrels as another group
  addCircles(
    data = subset(df_final_dataset, col_class == "gray"),
    ~longitude, ~latitude,
    color = "grey",
    group = "Grey",
    popup = "Grey Squirrel",
    fillOpacity = 0.5, radius = 10, stroke = FALSE
  ) %>%
  # Add OTHER squirrels as another group
  addCircles(
    data = subset(df_final_dataset, col_class == "other"),
    ~longitude, ~latitude,
    color = "green",
    group = "Other",
    popup = "Other Squirrel",
    fillOpacity = 0.5, radius = 10, stroke = FALSE
  ) %>%
  # Add a controller to turn layers on and off
  addLayersControl(
    overlayGroups = c("Melanic", "Grey", "Other"),
    options = layersControlOptions(collapsed = FALSE)
  )

### Predict North America-wide probability of melanism -----

## Identify most parsimonious model for prediction
mod_dredged <- dredge(mod, rank = "BIC")

## Create most parsimonious model
mod_parsimonious <- glm(melanic_binary ~ pop_den_scaled + winter_temp_scaled + introduced +
                          pop_den_scaled:introduced + winter_temp_scaled:introduced + RAC,
                        family = binomial(link = "logit"),
                        data = df_final_dataset,
                        na.action = "na.fail")

## Evaluate most parsimonious model
summary(mod_parsimonious)
r2(mod_parsimonious)

## Create minimum convex polygon (MCP) of all squirrel reports

# Convert to a spatial sf object
sf_final_dataset <- st_as_sf(df_final_dataset, coords = c("longitude", "latitude"), crs = 4326)

# Create the MCP
mcp <- sf_final_dataset %>%
  st_combine() %>% #count all points as one group to make a single polygon
  st_convex_hull() #create MCP

# Visualize MCP
plot(st_geometry(sf_final_dataset), col = "blue", pch = 16)
plot(mcp, add = TRUE, border = "red", lwd = 2)

## Create grid cells

# Project your MCP to Albers Equal Area (as established)
mcp_projected <- st_transform(mcp, "ESRI:102008")

# Make the MCP a SpatVector
mcp_vect <- vect(mcp_projected)

# Create a "Template Raster"
grid_template <- rast(mcp_vect, res = 1000) # 1000m = 1km

## Extract human population density values

# Project the Population Data
# Instead of extracting points, this projects the whole population raster to match the 1km Albers grid
# 'method = "sum"' ensures that if multiple pixels fit in one cell, their population is added together as a weighted average
pop_1km <- project(pop_den_2020, grid_template, method = "sum")

# Clip to the MCP
pop_grid <- mask(pop_1km, mcp_vect)

# Visualize
plot(pop_grid, main = "Population Density (1km Grid)")

# Convert the final population raster to a data frame
df_pred_melanism <- as.data.frame(pop_grid, xy = TRUE, na.rm = TRUE)

# To get Lat/Lon, treat the X/Y points as a spatial object 
# and transform them back to WGS84 (EPSG: 4326)
coords_projected <- df_pred_melanism[, c("x", "y")]

# Create a temporary SpatVector of your grid centers
v <- vect(coords_projected, geom = c("x", "y"), crs = crs(pop_grid))

# Transform to Lat/Lon
v_latlon <- project(v, "EPSG:4326")

# Add the Lat/Lon back to the main data frame
df_pred_melanism$longitude <- crds(v_latlon)[, 1]
df_pred_melanism$latitude  <- crds(v_latlon)[, 2]

# Rename the Albers columns to avoid confusion
colnames(df_pred_melanism)[1:2] <- c("albers_x", "albers_y")

## Extract average winter daily minimum temperature

# Convert the 'raster' objects to 'terra' objects
tmin_list <- list(jan_2020, feb_2020, jan_2021, feb_2021)
tmin_stack <- rast(lapply(tmin_list, rast))

# Crop the stack to the MCP
winter_stack_cropped <- crop(tmin_stack, mcp_vect)

# Calculate the mean on only the cropped area
winter_low_daily_avg <- mean(winter_stack_cropped)

# Project the temperature average to match your 1km Albers grid
# Use 'bilinear' here because temperature is a continuous gradient
temp_1km <- project(winter_low_daily_avg, pop_grid, method = "bilinear")

# Create a SpatVector of the points in df_pred_melanism
v_points <- vect(df_pred_melanism, geom = c("albers_x", "albers_y"), crs = crs(pop_grid))

# Extract the temperature values exactly at those point locations
temp_values <- terra::extract(temp_1km, v_points)

# Add the values to your data frame
df_pred_melanism$avg_winter_daily_low <- temp_values[, 2]
