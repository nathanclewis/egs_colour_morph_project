##### Analyses and data visualizations for the squirrel colour morphs project
## Liz's copy - 2019 photo set

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
}

### Read Data -----

{ #run this line to read all data files
  
  ## Completed dataset (including RGBs) with 20,594 usable records from 2020
  df_2020_completed <- read_csv("Data/sq_RGB_2020_df_1_31535.csv") %>%
    dplyr::select(inat_id, observed_on, image_url, latitude.y, longitude.y, color_max_x, color_min_x, color_max_y, color_min_y, red, green, blue) %>%
    #remove records from outside North America
    filter(latitude.y > 13 & longitude.y < -51) %>%
    rename(id = inat_id, latitude = latitude.y, longitude = longitude.y)
  
  ## Partially complete dataset (including RGBs) from 2021
  df_2021_completed <- read_csv("Data/sq_RGB_2021_df_1_20000.csv") %>%
    dplyr::select(id, observed_on, image_url, latitude, longitude, color_max_x, color_min_x, color_max_y, color_min_y, red, green, blue) %>%
    #remove records from outside North America
    filter(latitude > 13 & longitude < -51)
  
  ## All 31,535 records from 2020
  df_2020 <- read_csv("Data/df_2020_complete_data.csv") %>%
    dplyr::select(inat_id, observed_on, image_url, latitude.y, longitude.y) %>%
    #remove records from outside North America
    filter(latitude.y > 13 & longitude.y < -51) %>%
    rename(id = inat_id, latitude = latitude.y, longitude = longitude.y)
  
  ## All 31413 records from 2021
  df_2021 <- read_csv("Data/df_2021_complete_data.csv") %>%
    dplyr::select(id, observed_on, image_url, latitude, longitude) %>%
    #remove records from outside North America
    filter(latitude > 13 & longitude < -51)
}

### Read Data for 2019 ----

## Records from 2019 (n = 16,993)
df_2019 = read_csv("Data/observations_2019.csv") %>% 
  dplyr::select(id, observed_on, image_url, latitude, longitude) %>% 
  #remove records from outside North America
  filter(latitude > 13 & longitude < -51)

df_2019_completed = read_csv("Data/sq_RGB_2019_1_4750.csv") 
#use most recently created sq_RGB_2019_1_XXXX

df_2019 %>% view()

### Test Image URLs and remove rows with invalid URLs -----

## Create a function that identifies invalid image URLs
url_check = function(url_in,t=2){
  con <- url(url_in)
  check <- suppressWarnings(try(open.connection(con,open="rt",timeout=t),silent=T)[1])
  suppressWarnings(try(close.connection(con),silent=T))
  ifelse(is.null(check),TRUE,FALSE)
}  

## Choose a subset to process and remove invalid URLs
df_2019_noerrors = df_2019 %>%
  slice(4751:5250) %>%
  filter(!str_detect(image_url, "gif$")) %>%
  mutate(valid_url = future_map_lgl(image_url, url_check)) %>%
  filter(valid_url == TRUE)

df_2019_noerrors %>% view()
### Coordinate extraction -----

## Function that extracts coordinates from a picture
locate_box = function(image_url){
  display(readImage(image_url), 
          temp(),
          method = "raster", all = TRUE)
  locator()
}

## Apply it to a short list
df_2019_4751_5250 = df_2019_noerrors %>%
  #slice(1:3) %>% 
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

#df_2019_1001_1500 %>% view()
### Extract RGB values -----

## Function to extract mean RGBs from an image given the coordinates
extract_mean_colour = function(image, xmin, xmax, ymin, ymax){
  readImage(image)[xmin:xmax, ymin:ymax, ] %>% 
    apply(3, mean)
}

## Apply extract colour functions and create columns for red, green, and blue values
df_2019_4751_5250_col = df_2019_4751_5250 %>%
  mutate(mean_rgb = future_pmap(
    list(image_url, color_min_x, color_max_x, color_min_y, color_max_y),
    ~ extract_mean_colour(..1, ..2, ..3, ..4, ..5)
  )) %>% 
  unnest_wider(mean_rgb, names_sep = "_") %>%
  rename(red = mean_rgb_1,
         green = mean_rgb_2,
         blue = mean_rgb_3) %>%
  dplyr::select(-c(valid_url, picture_info, mean_rgb_4))
#, mean_rgb_4


df_2019_4751_5250_col %>% view()
### Add new df to existing master df -----

## Generate complete dataset
df_2019_new = df_2019_completed %>%
  rbind(df_2019_4751_5250_col) #insert name of newly created df here

df_2019_new %>% view()

## Write new csv. Always change the last number in the name to match the highest
## number clicked through to date before writing
write_csv(df_2019_new, "Data/sq_RGB_2019_1_5250.csv")
#write_csv(df_2019_4501_4750, "Data/df_2019_4501_4750.csv")
#df_2019_4501_4750 = read_csv("Data/df_2019_4501_4750.csv")

## check error
safe_extract <- purrr::safely(extract_mean_colour)
results <- future_pmap(
  list(df_2019_1001_1500$image_url,
       df_2019_1001_1500$color_min_x,
       df_2019_1001_1500$color_max_x,
       df_2019_1001_1500$color_min_y,
       df_2019_1001_1500$color_max_y),
  ~ safe_extract(..1, ..2, ..3, ..4, ..5)
)

df_2019_3751_4000$mean_rgb <- map(results, "result")
df_2019_3751_4000$error    <- map(results, "error")

problem_rows <- df_2019_3751_4000 %>%
  filter(!map_lgl(error, is.null))

print(problem_rows)