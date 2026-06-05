##### Squirrel colour morph classification process for the squirrel colour morphs project

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

{
  install.packages(c("BiocManager", "tidyverse", "furrr", "geodata", "raster", "randomForest", "caret", "car", "visreg", "lme4", "leaflet",
                     "MuMIn", "performance", "sf", "terra", "spdep"))
  BiocManager::install("EBImage")
}

### Read packages and settings -----

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
  library(sf) #for working with shapefiles
  library(terra) #for working with spatial rasters
  library(spdep) #for Moran's I test of spatial autocorrelation
}

### Read Squirrel Data -----

{ #run this line to read all data files
  
  df_full <- read_csv("Data/full_dataset_2019_2021.csv")
  
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
  
  ## All 16,993 records from 2019 (no RGBs)
  df_2019 <- read_csv("Data/observations_2019.csv") %>%
    dplyr::select(id, observed_on, image_url, latitude, longitude) %>%
    #remove records from outside North America
    filter(latitude > 13 & longitude < -51)
  
  ## All 31,535 records from 2020 (no RGBs)
  df_2020 <- read_csv("Data/df_2020_complete_data.csv") %>%
    dplyr::select(inat_id, observed_on, image_url, latitude.y, longitude.y) %>%
    #remove records from outside North America
    filter(latitude.y > 13 & longitude.y < -51) %>%
    rename(id = inat_id, latitude = latitude.y, longitude = longitude.y)
  
  ## All 31,413 records from 2021 (no RGBs)
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
  slice(1:10) %>%
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
df_2019_1_10 = df_2019_noerrors %>%
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
df_2019_1_10_col <- df_2019_1_10 %>%
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
  rbind(df_2019_1_10_col) #insert name of newly created df here

## Write new csv. Always change the last number in the name to match the highest
## number clicked through to date before writing
#write_csv(df_2019_new, "Data/sq_RGB_2019_1_17000.csv")

## Compile data from all years into master df
df_colour <- df_2019_completed %>%
  rbind(df_2020_completed) %>%
  rbind(df_2021_completed) %>%
  #include a new ID column to match with the popden and temp dfs
  mutate(ID = row_number())

### Upload land cover data -----

# 1. Read the land cover file pointer
rast_lc <- rast("C:/Users/Benson-Amram Lab/Desktop/Nathan/NA_NALCMS_landcover_2020v2_30m.tif")

# Convert your original data frame points to an sf spatial object (WGS84)
sf_squirrels <- st_as_sf(df_colour, coords = c("longitude", "latitude"), crs = 4326)

# Transform to metric CRS (EPSG:2163) so 'dist = 1000' equals exactly 1 kilometer
sf_squirrels_projected <- st_transform(sf_squirrels, crs = 2163)

# Generate the 1km buffer zones
sf_squirrel_buffers <- st_buffer(sf_squirrels_projected, dist = 1000)

# 2. Project your buffer circles to match the Land Cover CRS
# (We do the work in the raster's native coordinate system to prevent errors)
sf_buffers_lc_crs <- st_transform(sf_squirrel_buffers, crs = crs(rast_lc))
num_buffers <- nrow(sf_buffers_lc_crs)

# 3. Create our blank results collector matrix
categories <- c("forest", "shrubland", "grassland", "barren", "wetland", "cropland", "developed", "other")
results_matrix <- matrix(0, nrow = num_buffers, ncol = length(categories))
colnames(results_matrix) <- categories

# 4. Loop through every single squirrel buffer individually
for (i in 1:num_buffers) {
  
  if (i %% 100 == 0 || i == 1) {
    message(paste0("Processing squirrel ", i, " of ", num_buffers, "..."))
  }
  
  # Isolate exactly ONE buffer polygon
  single_sf <- sf_buffers_lc_crs[i, ]
  
  # Generate a rapid internal grid of points inside this 1km buffer.
  # Spacing them 30 meters apart perfectly mimics the raster grid layout!
  internal_points <- st_sample(single_sf, size = 3500, type = "regular")
  
  if (length(internal_points) == 0) next
  
  # Extract raw coordinates matrix [X, Y]
  coords <- st_coordinates(internal_points)
  
  # BYPASS FUNCTION: Convert X/Y coordinates directly into Raster Cell Numbers
  # This acts as a direct memory lookup bypass
  cell_numbers <- cellFromXY(rast_lc, coords)
  
  # Pull the raw values directly from the hard drive stream by cell index
  raw_values <- rast_lc[cell_numbers][[1]]
  
  # Filter missing data out
  raw_values <- raw_values[!is.na(raw_values)]
  if (length(raw_values) == 0) next
  
  # Count the frequencies of land cover codes
  val_counts <- table(raw_values)
  single_props <- as.data.frame(val_counts)
  names(single_props) <- c("lc_code", "cell_count")
  
  # Categorize and aggregate
  single_props <- single_props %>%
    mutate(
      lc_code = as.numeric(as.character(lc_code)),
      category = case_when(
        lc_code %in% 1:6 ~ "forest",
        lc_code %in% c(7, 8, 11) ~ "shrubland",
        lc_code %in% c(9, 10, 12) ~ "grassland",
        lc_code %in% c(16, 19) ~ "barren",
        lc_code == 14 ~ "wetland",
        lc_code == 15 ~ "cropland",
        lc_code == 17 ~ "developed",
        TRUE ~ "other"
      )
    ) %>%
    group_by(category) %>%
    summarize(total_cells = sum(cell_count), .groups = "drop") %>%
    mutate(proportion = total_cells / sum(total_cells))
  
  # Pop the results into our matrix row
  if (nrow(single_props) > 0) {
    for (j in 1:nrow(single_props)) {
      results_matrix[i, single_props$category[j]] <- single_props$proportion[j]
    }
  }
}

# 5. Bind data seamlessly back together
df_lc_proportions <- as.data.frame(results_matrix)
df_colour_landcover <- cbind(df_colour, df_lc_proportions)

### Generate human population density data -----

{
  ## Set spatial boundaries
  max_lat <- 72.04
  min_lat <- 13.89
  max_lon <- -51.58
  min_lon <- -160.60
  
  ## Load human population density data
  pop_den_2020 <- rast("Data/NA_PopulationDensity_2020.tif")
  
  # Match the buffer projections to the raster's native CRS before building the crop box
  sf_buffers_raster_crs <- st_transform(sf_squirrel_buffers, crs = crs(pop_den_2020))
  
  # Turn the buffers into a terra SpatVector for optimized geometric operations
  vect_buffers <- vect(sf_buffers_raster_crs)
  
  # Compute the exact tight bounding box around your buffers and slice the raster
  buffer_extent <- ext(vect_buffers)
  cropped_pop_den <- crop(pop_den_2020, buffer_extent)
  
  # This extracts cell values intersecting the buffers along with their overlap coverage fractions
  extracted_popden <- terra::extract(cropped_pop_den, vect_buffers, exact = TRUE, ID = TRUE)
  
  # Safely isolate the exact name of the population density data layer
  layer_name <- names(cropped_pop_den)[1]
  
  # Calculate the final weighted averages
  df_popden <- extracted_popden %>%
    filter(!is.na(.data[[layer_name]])) %>%
    group_by(ID) %>%
    summarize(
      weighted_pop_density = sum(.data[[layer_name]] * fraction) / sum(fraction)
    )
  
  # Bind the resulting metrics vector back onto your original df_colour dataframe
  df_colour_landcover_popden <- df_colour_landcover %>%
    left_join(df_popden)
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
                                    df_colour_landcover_popden[c("longitude","latitude")],
                                    df=TRUE) %>%
    rename(temp_jan20 = wc2.1_2.5m_tmin_01)
  df_temps_jan21 <- raster::extract(jan_2021,
                                    df_colour_landcover_popden[c("longitude","latitude")],
                                    df=TRUE) %>%
    rename(temp_jan21 = wc2.1_2.5m_tmin_01)
  df_temps_feb20 <- raster::extract(feb_2020,
                                    df_colour_landcover_popden[c("longitude","latitude")],
                                    df=TRUE) %>%
    rename(temp_feb20 = wc2.1_2.5m_tmin_02)
  df_temps_feb21 <- raster::extract(feb_2020,
                                    df_colour_landcover_popden[c("longitude","latitude")],
                                    df=TRUE) %>%
    rename(temp_feb21 = wc2.1_2.5m_tmin_02)
  
  df_colour_landcover_popden_temp <- df_colour_landcover_popden %>%
    left_join(c(df_temps_jan20, df_temps_feb20, df_temps_jan21, df_temps_feb21), copy = TRUE) %>%
    mutate(avg_winter_low_temp = rowMeans(across(c(temp_jan20, temp_feb20, temp_jan21, temp_feb21)), na.rm = TRUE)) %>%
    dplyr::select(-c(ID.1, ID.2, ID.3))
}

### Identify reports in native range -----

## Load range map
range <- read_sf("Data/EGS_nativerange.shp")

## Assign native status to range
range_native <- range %>%
  mutate(native = "Y")

## Create sf for reports
sf_reports <- st_as_sf(
  df_colour_landcover_popden_temp,
  coords = c("longitude", "latitude"),
  crs = 4326,   # WGS84
  remove = FALSE
)

## Combine reports and native range objects
df_reports_native <- st_join(sf_reports, range_native, join = st_within) %>%
  st_drop_geometry() %>%
  mutate(native = ifelse(is.na(native), "N", native)) %>%
  as.data.frame() %>%
  dplyr::select(id, native)

df_colour_landcover_popden_temp_range <- df_colour_landcover_popden_temp %>%
  left_join(df_reports_native) %>%
  left_join(df_sq_mpr)

write_csv(df_colour_landcover_popden_temp_range, "Data/full_dataset_2019_2021.csv")

### Train and test random forest (k-fold cv) -----

## Create filtered df and add squirrel mapper data for training
df_4ml <- df_colour_landcover_popden_temp_range %>%
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
df_rgb <- df_colour_landcover_popden_temp_range %>%
  dplyr::select(red, green, blue)

## Predict all values
predicted_col_morphs <- predict(rf_model_cv, newdata = df_rgb)

## Add predictions back into the original data
df_col_class <- df_colour_landcover_popden_temp_range %>%
  mutate(
    col_class = as.character(predicted_col_morphs)) %>%
  filter(col_class %in% c('melanic', 'gray'))

## Check full set of predictions vs. sq mpr
table(df_col_class$sq_mpr_col, df_col_class$col_class)

### Save complete dataset for analyses -----

write_csv(df_col_class, "Data/complete_data_4_analyses.csv")
