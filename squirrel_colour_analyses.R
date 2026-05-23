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

### Read and prepare dataset -----

## Read dataset as generated with the squirrel_colour_classification.R script
df_full <- read_csv("Data/complete_data_4_analyses.csv")

## Reduce df_full to usable and necessary data
df_4model <- df_full %>%
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


### Calculate residual autocorrelation -----

## Create model
mod4RAC <- glm(melanic_binary ~ pop_den_scaled + winter_temp_scaled + introduced +
             prop_forest_scaled + prop_developed_scaled + pop_den_scaled:introduced +
             winter_temp_scaled:introduced + pop_den_scaled:winter_temp_scaled +
             pop_den_scaled:prop_developed_scaled + prop_forest_scaled:prop_developed_scaled,
           family = binomial(link = "logit"),
           data = df_4model,
           na.action = "na.fail")

# Calculate model residuals
mod_resid <- residuals(mod4RAC, type = "deviance")
mod_resid_rac <- residuals(mod4RAC, type = "deviance")

## Evaluate spatial autocorrelation with Moran's I

# Find all neighbors within the specified distance range
coords <- as.matrix(df_4model[,c("longitude", "latitude")])
nb <- dnearneigh(coords, d1 = 0, d2 = 0.25)

# Convert the neighbor object to a spatial weights list
# The style="W" argument row-standardizes the weights (each row sums to 1)
# 'zero.policy=TRUE' allows for data points that might have no neighbors
listw <- nb2listw(nb, style = "W", zero.policy = TRUE)

# Calculate the spatially lagged residuals (the autocovariate)
rac_term <- lag.listw(listw, mod_resid)

# Add RAC term to final dataset
df_4model$RAC <- rac_term

# Run Moran's I test on the residuals 
moran_result <- moran.test(mod_resid_rac, 
                           listw = listw, 
                           zero.policy = TRUE)
print(moran_result)

### Save model-ready dataset -----

write_csv(df_4model, "Data/data_4model.csv")

### Perform complete logistic regression -----

## Create model
mod <- glm(melanic_binary ~ pop_den_scaled + winter_temp_scaled + introduced +
                 prop_forest_scaled + prop_developed_scaled + pop_den_scaled:introduced +
                 winter_temp_scaled:introduced + pop_den_scaled:winter_temp_scaled +
                 pop_den_scaled:prop_developed_scaled + prop_forest_scaled:prop_developed_scaled + RAC,
               family = binomial(link = "logit"),
               data = df_4model,
               na.action = "na.fail")

## Evaluate model
summary(mod)
Anova(mod)
confint(mod)
r2(mod)
visreg(mod, scale = "response")
vif(mod)
cor(df_4model[11:14], method = "pearson")

### Identify most parsimonious sub-model for prediction (to be used in projecting_melanism.R) -----

## Create model without 

mod_dredged <- dredge(mod, rank = "BIC")
View(mod_dredged)

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
  ggplot(aes(x = prop_forest, y = col_class_binary)) +
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
