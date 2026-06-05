##### Analyses for the squirrel colour morphs project

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
  library(car) #for check_collinearity()
  library(visreg) #for visualizing model fit
  library(lme4) #for mixed models
  library(leaflet) #for mapping
  library(MuMIn) #for dredge
  library(performance) #for r^2
  library(sf) #for working with shapefiles
  library(terra) #for working with spatial rasters
  library(spdep) #for Moran's I test of spatial autocorrelation
  library(AICcmodavg) #for model selection
  library(broom.mixed) #for extracting model estimates and confidence intervals from mixed models
}

### Read and prepare dataset -----

## Read dataset as generated with the squirrel_colour_classification.R script
df_full <- read_csv("Data/complete_data_4_analyses.csv")

## Reduce df_full to usable and necessary data
df_4model <- df_full %>%
  mutate(melanic_binary = ifelse(col_class == "melanic", 1, ifelse(col_class == "gray", 0, NA)),
         melanic_binary = factor(melanic_binary),
         introduced = ifelse(native == "Y", "N", "Y"),
         total_LC = forest + shrubland + grassland + barren + wetland + cropland + developed,
         prop_forest = forest/total_LC) %>%
  dplyr::select(id, latitude, longitude, weighted_pop_density, avg_winter_low_temp, introduced, prop_forest, col_class, melanic_binary) %>%
  na.omit()

## Add log-centred versions of each predictor
df_4model$pop_den_scaled <- scale(df_4model$weighted_pop_density) %>% as.vector
df_4model$winter_temp_scaled <- scale(df_4model$avg_winter_low_temp) %>% as.vector
df_4model$prop_forest_scaled <- scale(df_4model$prop_forest) %>% as.vector


### Calculate residual autocorrelation -----

## Ensure coordinates are a matrix for spdep
coords <- as.matrix(df_4model[, c("longitude", "latitude")])

# Fit the baseline global model (no RAC)
mod_baseline <- glm(melanic_binary ~ pop_den_scaled + winter_temp_scaled + introduced +
                      prop_forest_scaled + pop_den_scaled:introduced +
                      winter_temp_scaled:introduced + pop_den_scaled:winter_temp_scaled +
                      prop_forest_scaled:pop_den_scaled,
                    family = binomial(link = "logit"),
                    data = df_4model,
                    na.action = "na.fail")

## Extract the raw baseline deviance residuals to generate all downstream RAC terms
base_resid <- residuals(mod_baseline, type = "deviance")

## Generate RAC covariates for 1 km scale
nb_1km <- dnearneigh(coords, d1 = 0, d2 = 1, longlat = TRUE)
listw_1km <- nb2listw(nb_1km, style = "W", zero.policy = TRUE)
df_4model$RAC_1km <- lag.listw(listw_1km, base_resid, zero.policy = TRUE)

## Generate RAC covariates for 10 km scale
nb_10km <- dnearneigh(coords, d1 = 0, d2 = 10, longlat = TRUE)
listw_10km <- nb2listw(nb_10km, style = "W", zero.policy = TRUE)
df_4model$RAC_10km <- lag.listw(listw_10km, base_resid, zero.policy = TRUE)

## Generate RAC covariates for 20 km scale
nb_20km <- dnearneigh(coords, d1 = 0, d2 = 20, longlat = TRUE)
listw_20km <- nb2listw(nb_20km, style = "W", zero.policy = TRUE)
df_4model$RAC_20km <- lag.listw(listw_20km, base_resid, zero.policy = TRUE)

## Generate RAC covariate for 30 km scale
nb_30km <- dnearneigh(coords, d1 = 0, d2 = 30, longlat = TRUE)
listw_30km <- nb2listw(nb_30km, style = "W", zero.policy = TRUE)
df_4model$RAC_30km <- lag.listw(listw_30km, base_resid, zero.policy = TRUE)

## Generate RAC covariate for 40 km scale
nb_40km <- dnearneigh(coords, d1 = 0, d2 = 40, longlat = TRUE)
listw_40km <- nb2listw(nb_40km, style = "W", zero.policy = TRUE)
df_4model$RAC_40km <- lag.listw(listw_40km, base_resid, zero.policy = TRUE)

## Generate RAC covariates for 50 km scale
nb_50km <- dnearneigh(coords, d1 = 0, d2 = 50, longlat = TRUE)
listw_50km <- nb2listw(nb_50km, style = "W", zero.policy = TRUE)
df_4model$RAC_50km <- lag.listw(listw_50km, base_resid, zero.policy = TRUE)

## Fit models for each RAC scale

mod_RAC_1km  <- update(mod_baseline, . ~ . + RAC_1km,  data = df_4model)
mod_RAC_10km <- update(mod_baseline, . ~ . + RAC_10km, data = df_4model)
mod_RAC_20km <- update(mod_baseline, . ~ . + RAC_20km, data = df_4model)
mod_RAC_30km <- update(mod_baseline, . ~ . + RAC_30km, data = df_4model)
mod_RAC_40km <- update(mod_baseline, . ~ . + RAC_40km, data = df_4model)
mod_RAC_50km <- update(mod_baseline, . ~ . + RAC_50km, data = df_4model)

## Perform model selection

# Pack models into a named list for a tidy printout
models_list <- list(
  "No Spatial Control (Baseline)" = mod_baseline,
  "1 km RAC"  = mod_RAC_1km,
  "10 km RAC" = mod_RAC_10km,
  "20 km RAC" = mod_RAC_20km,
  "30 km RAC" = mod_RAC_30km,
  "40 km RAC" = mod_RAC_40km,
  "50 km RAC" = mod_RAC_50km
)

# Generate and print the selection table
aic_table <- aictab(cand.set = models_list)
print(aic_table)

## Evaluate Residuals of the Winning Model

# Extract the residuals from the top model
winning_resids <- residuals(mod_RAC_20km, type = "deviance")

# Run the Moran's I test using the 20 km spatial weights matrix we made earlier
moran_result <- moran.test(winning_resids, listw = listw_20km, zero.policy = TRUE)
print(moran_result)

### Save model-ready dataset -----

#Save df
write_csv(df_4model, "Data/data_4model.csv")

#Read saved df with RAC
df_4model <- read_csv("Data/data_4model.csv")

### Assess predictors for correlation -----

## Pearson's r
cor(df_4model[c(10:12)], method = "pearson")

### Perform complete logistic regression -----

## Create model
mod <- glm(melanic_binary ~ pop_den_scaled + winter_temp_scaled + prop_forest_scaled + 
             introduced + pop_den_scaled:introduced + winter_temp_scaled:introduced + 
             pop_den_scaled:winter_temp_scaled + pop_den_scaled:prop_forest_scaled + RAC_20km,
               family = binomial(link = "logit"),
               data = df_4model,
               na.action = "na.fail")

## Evaluate model
summary(mod)
confint(mod)
r2(mod)
visreg(mod, scale = "response")
check_collinearity(mod)

### Plot raw data -----

## Human pop den
df_4model %>%
  mutate(col_class_binary = ifelse(col_class == "gray", 0,
                                   ifelse(col_class == "melanic", 1,
                                          NA))) %>%
  filter(!is.na(col_class_binary)) %>%
  ggplot(aes(x = weighted_pop_density, y = col_class_binary, col = introduced)) +
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
  ggplot(aes(x = avg_winter_low_temp, y = col_class_binary, col = introduced)) +
  geom_point() +
  geom_smooth(method = "glm") +
  labs(x = "Average Winter Temperature (C)", y = "Probability of Melanism") +
  theme_bw()

## Forest cover
df_4model %>%
  mutate(col_class_binary = ifelse(col_class == "gray", 0,
                                   ifelse(col_class == "melanic", 1,
                                          NA))) %>%
  filter(!is.na(col_class_binary)) %>%
  ggplot(aes(x = prop_forest, y = col_class_binary, col = introduced)) +
  geom_point() +
  geom_smooth(method = "glm") +
  labs(x = "Forest Cover", y = "Probability of Melanism") +
  theme_bw()

### Plot confidence intervals -----

## Create df with confidence intervals

df_plot_data <- tidy(mod, conf.int = TRUE) %>%
    # Rename components for cleaner facet labels
    mutate(term = fct_recode(term,
                             `Intercept` = "(Intercept)",
                             `Population density` = "pop_den_scaled",
                             `Winter temperature` = "winter_temp_scaled",
                             `Forest cover` = "prop_forest_scaled",
                             `Non-native` = "introducedY",
                             `Residual autocovariate` = "RAC_20km",
                             `Population density x Non-native` = "pop_den_scaled:introducedY",
                             `Winter temperature x Non-native` = "winter_temp_scaled:introducedY",
                             `Population density x Winter temperature` = "pop_den_scaled:winter_temp_scaled",
                             `Population density x Forest cover` = "pop_den_scaled:prop_forest_scaled")) %>%
  filter(!term %in% c("Intercept", "Residual autocovariate"))


#Create plot
CI_plot <- ggplot(df_plot_data, aes(x = estimate, y = term)) +
  # Add a vertical line at 0 for reference
  geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.5) +
  # The "dodging" happens here to prevent overlap
  geom_pointrange(aes(xmin = conf.low, xmax = conf.high),
                  position = position_dodge(width = 0.6),
                  size = 1) +
  labs(x = "Coefficient Estimate (with 95% CI)",
       y = NULL) +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 22),
        axis.text.y = element_text(size = 22),
        axis.title.x = element_text(size = 22),
        legend.text = element_text(size = 22),
        legend.title = element_text(size = 22)) +
  theme(panel.grid.minor = element_blank(),
        strip.text = element_text(face = "bold", size = 22),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1));CI_plot

#ggsave("CI_plot.png")

### Map reported sightings -----

# Create the base map
leaflet(df_4model) %>%
  addTiles() %>%
  # Add MELANIC squirrels as one group
  addCircles(
    data = subset(df_4model, col_class == "melanic"),
    ~longitude, ~latitude,
    color = "black",
    group = "Melanic",
    popup = "Melanic Squirrel",
    fillOpacity = 0.5, radius = 10, stroke = FALSE
  ) %>%
  # Add GREY squirrels as another group
  addCircles(
    data = subset(df_4model, col_class == "gray"),
    ~longitude, ~latitude,
    color = "grey",
    group = "Grey",
    popup = "Grey Squirrel",
    fillOpacity = 0.5, radius = 10, stroke = FALSE
  ) %>%
  # Add OTHER squirrels as another group
  addCircles(
    data = subset(df_4model, col_class == "other"),
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
