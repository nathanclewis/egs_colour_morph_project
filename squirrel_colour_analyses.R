##### Analyses for the squirrel colour morphs project

##### Script Info/Instructions -----

{
  ## Start-up Instructions
  # 1. Pull from github first
  # 2. Read libraries second (after downloading them on first use)
  # 3. Read all datasets third
  # 4. Push changes to github regularly and before closing RStudio
  
  ## Script Format
  ##### Script Description
  #### Script Instructions/Details
  ##########Section header
  ### Subsection Header
  ## Description of single code chunk
  # Description of single line
}

########## Setup -----
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
  library(broom.mixed) #for extracting model estimates and confidence intervals from mixed models
  library(AICcmodavg) #for model selection
  library(pROC) #for ROC curve
  
  set.seed(123)
}

### Read full dataset -----

## Read dataset as generated with the squirrel_colour_classification.R script
df_full <- read_csv("Data/complete_data_4_analyses.csv") %>%
  mutate(melanic_binary = ifelse(col_class == "melanic", 1, ifelse(col_class == "gray", 0, NA)),
         melanic_binary = factor(melanic_binary),
         introduced = ifelse(native == "Y", "N", "Y"),
         total_LC = forest + shrubland + grassland + barren + wetland + cropland + developed,
         prop_forest = forest/total_LC) %>%
  dplyr::select(id, latitude, longitude, weighted_pop_density, avg_winter_low_temp, introduced, prop_forest, col_class, melanic_binary) %>%
  filter(introduced == "N" | longitude < -101.366) %>%
  na.omit()

########## Full native dataset (no out-of-sample subset removed) -----
### Prepare dataset -----

## Prepare native-only dataset
df_4model_fullnative <- df_full %>%
  filter(introduced == "N") %>%
  mutate(
    pop_den_scaled = (weighted_pop_density - mean(df_4model_fullnative$weighted_pop_density))/sd(df_4model_fullnative$weighted_pop_density),
    winter_temp_scaled = (avg_winter_low_temp - mean(df_4model_fullnative$avg_winter_low_temp))/sd(df_4model_fullnative$avg_winter_low_temp),
    prop_forest_scaled = (prop_forest - mean(df_4model_fullnative$prop_forest))/sd(df_4model_fullnative$prop_forest))

### Calculate residual autocorrelation -----

## Ensure coordinates are a matrix for spdep
coords_fullnative <- as.matrix(df_4model_fullnative[, c("longitude", "latitude")])

# Fit the baseline global model (no RAC)
mod_baseline_fullnative <- glm(melanic_binary ~ pop_den_scaled + winter_temp_scaled +
                      prop_forest_scaled + pop_den_scaled:winter_temp_scaled +
                      prop_forest_scaled:pop_den_scaled,
                    family = binomial(link = "logit"),
                    data = df_4model_fullnative,
                    na.action = "na.fail")

## Extract the raw baseline deviance residuals to generate all downstream RAC terms
base_resid_fullnative <- residuals(mod_baseline_fullnative, type = "deviance")

## Generate RAC covariates for 1 km scale
nb_1km_fullnative <- dnearneigh(coords_fullnative, d1 = 0, d2 = 1, longlat = TRUE)
listw_1km_fullnative <- nb2listw(nb_1km_fullnative, style = "W", zero.policy = TRUE)
df_4model_fullnative$RAC_1km <- lag.listw(listw_1km_fullnative, base_resid_fullnative, zero.policy = TRUE)

## Generate RAC covariates for 10 km scale
nb_10km_fullnative <- dnearneigh(coords_fullnative, d1 = 0, d2 = 10, longlat = TRUE)
listw_10km_fullnative <- nb2listw(nb_10km_fullnative, style = "W", zero.policy = TRUE)
df_4model_fullnative$RAC_10km <- lag.listw(listw_10km_fullnative, base_resid_fullnative, zero.policy = TRUE)

## Generate RAC covariates for 20 km scale
nb_20km_fullnative <- dnearneigh(coords_fullnative, d1 = 0, d2 = 20, longlat = TRUE)
listw_20km_fullnative <- nb2listw(nb_20km_fullnative, style = "W", zero.policy = TRUE)
df_4model_fullnative$RAC_20km <- lag.listw(listw_20km_fullnative, base_resid_fullnative, zero.policy = TRUE)

## Generate RAC covariate for 30 km scale
nb_30km_fullnative <- dnearneigh(coords_fullnative, d1 = 0, d2 = 30, longlat = TRUE)
listw_30km_fullnative <- nb2listw(nb_30km_fullnative, style = "W", zero.policy = TRUE)
df_4model_fullnative$RAC_30km <- lag.listw(listw_30km_fullnative, base_resid_fullnative, zero.policy = TRUE)

## Generate RAC covariate for 40 km scale
nb_40km_fullnative <- dnearneigh(coords_fullnative, d1 = 0, d2 = 40, longlat = TRUE)
listw_40km_fullnative <- nb2listw(nb_40km_fullnative, style = "W", zero.policy = TRUE)
df_4model_fullnative$RAC_40km <- lag.listw(listw_40km_fullnative, base_resid_fullnative, zero.policy = TRUE)

## Generate RAC covariates for 50 km scale
nb_50km_fullnative <- dnearneigh(coords_fullnative, d1 = 0, d2 = 50, longlat = TRUE)
listw_50km_fullnative <- nb2listw(nb_50km_fullnative, style = "W", zero.policy = TRUE)
df_4model_fullnative$RAC_50km <- lag.listw(listw_50km_fullnative, base_resid_fullnative, zero.policy = TRUE)

## Fit models for each RAC scale

mod_RAC_1km_fullnative  <- update(mod_baseline_fullnative, . ~ . + RAC_1km,  data = df_4model_fullnative)
mod_RAC_10km_fullnative <- update(mod_baseline_fullnative, . ~ . + RAC_10km, data = df_4model_fullnative)
mod_RAC_20km_fullnative <- update(mod_baseline_fullnative, . ~ . + RAC_20km, data = df_4model_fullnative)
mod_RAC_30km_fullnative <- update(mod_baseline_fullnative, . ~ . + RAC_30km, data = df_4model_fullnative)
mod_RAC_40km_fullnative <- update(mod_baseline_fullnative, . ~ . + RAC_40km, data = df_4model_fullnative)
mod_RAC_50km_fullnative <- update(mod_baseline_fullnative, . ~ . + RAC_50km, data = df_4model_fullnative)

## Perform model selection

# Pack models into a named list for a tidy printout
models_list_fullnative <- list(
  "No Spatial Control (Baseline)" = mod_baseline_fullnative,
  "1 km RAC"  = mod_RAC_1km_fullnative,
  "10 km RAC" = mod_RAC_10km_fullnative,
  "20 km RAC" = mod_RAC_20km_fullnative,
  "30 km RAC" = mod_RAC_30km_fullnative,
  "40 km RAC" = mod_RAC_40km_fullnative,
  "50 km RAC" = mod_RAC_50km_fullnative
)

# Generate and print the selection table
aic_table_fullnative <- aictab(cand.set = models_list_fullnative)
print(aic_table_fullnative)

## Evaluate Residuals of the Winning Model

# Extract the residuals from the top model
winning_resids_fullnative <- residuals(mod_RAC_20km_fullnative, type = "deviance")

# Run the Moran's I test using the 20 km spatial weights matrix we made earlier
moran_result_fullnative <- moran.test(winning_resids_fullnative, listw = listw_20km_fullnative, zero.policy = TRUE)
print(moran_result_fullnative)

### Save and read model-ready datasets -----

#Save df
write_csv(df_4model_fullnative, "Data/data_4model_fullnative.csv")

#Read saved df with RAC
df_4model_fullnative <- read_csv("Data/data_4model_fullnative.csv")

### Assess predictors for correlation -----

#Pearson's r
cor(df_4model_fullnative[c(10:12)], method = "pearson")

### Perform logistic regression -----

## Create model
mod_fullnative <- glm(melanic_binary ~ pop_den_scaled + winter_temp_scaled + prop_forest_scaled + 
                    pop_den_scaled:winter_temp_scaled + pop_den_scaled:prop_forest_scaled + RAC_20km,
                  family = binomial(link = "logit"),
                  data = df_4model_fullnative,
                  na.action = "na.fail")

## Evaluate model
summary(mod_fullnative)
confint(mod_fullnative)
r2(mod_fullnative)
visreg(mod_fullnative, scale = "response")
check_collinearity(mod_fullnative)

########## Native training dataset (out-of-sample subset removed) -----
### Identify subset to remove -----

## Set the seed for reproducibility
set.seed(12345)

## Slice a random subset
df_native_slice <- df_4model_fullnative %>%
  #Make the random subset equal in length to the introduced sample
  slice_sample(n = length(df_full$id[df_full$introduced == "Y"]))

### Prepare training set -----

df_4model_trainnative <- df_4model_fullnative %>%
  anti_join(df_native_slice, by = "id")

### Save and read model-ready datasets -----

#Save df
write_csv(df_4model_trainnative, "Data/data_4model_trainnative.csv")

#Read saved df with RAC
df_4model_trainnative <- read_csv("Data/data_4model_trainnative.csv")

### Assess predictors for correlation -----

#Pearson's r
cor(df_4model_trainnative[c(10:12)], method = "pearson")

### Perform logistic regression -----

## Create model
mod_trainnative <- glm(melanic_binary ~ pop_den_scaled + winter_temp_scaled + prop_forest_scaled + 
                    pop_den_scaled:winter_temp_scaled + pop_den_scaled:prop_forest_scaled + RAC_20km,
                  family = binomial(link = "logit"),
                  data = df_4model_trainnative,
                  na.action = "na.fail")

## Evaluate model
summary(mod_trainnative)
confint(mod_trainnative)
r2(mod_trainnative)
visreg(mod_trainnative, scale = "response")
check_collinearity(mod_trainnative)

########## Native out-of-sample subset -----
### Prepare dataset -----

## Create subset df
df_4model_testnative <- df_native_slice %>%
  mutate(
    pop_den_scaled = (weighted_pop_density - mean(df_4model_fullnative$weighted_pop_density))/sd(df_4model_fullnative$weighted_pop_density),
    winter_temp_scaled = (avg_winter_low_temp - mean(df_4model_fullnative$avg_winter_low_temp))/sd(df_4model_fullnative$avg_winter_low_temp),
    prop_forest_scaled = (prop_forest - mean(df_4model_fullnative$prop_forest))/sd(df_4model_fullnative$prop_forest),
    RAC_20km = mean(df_4model_testnative$RAC_20km))

### Save and read model-ready datasets -----

#Save df
write_csv(df_4model_testnative, "Data/data_4model_testnative.csv")

#Read saved df with RAC
df_4model_testnative <- read_csv("Data/data_4model_testnative.csv")

### Assess predictors for correlation -----

#Pearson's r
cor(df_4model_testnative[c(10:12)], method = "pearson")


### Predict native test morphs with native model -----

## Predict probabilities of melanism in the introduced dataset using native coefficients
df_4model_testnative$pred_prob <- predict(
  mod_trainnative, 
  newdata = df_4model_testnative, 
  type = "response"
)

## Discrimination (AUC / ROC Curve)
roc_obj_testnative <- roc(df_4model_testnative$melanic_binary, df_4model_testnative$pred_prob)
auc(roc_obj_testnative)
plot(roc_obj_testnative) #ROC Curve: Native Model Predicting Introduced Range

## Mean Predicted Probability vs. Actual Prevalence
mean_actual_testnative <- mean(df_4model_testnative$melanic_binary, na.rm = TRUE)
mean_predicted_testnative <- mean(df_4model_testnative$pred_prob, na.rm = TRUE)

cat("Actual Prevalence:", mean_actual_testnative, "\n")
cat("Mean Predicted Prevalence:", mean_predicted_testnative, "\n")

### Logistic calibration model -----

## Convert predicted probabilities back to log-odds scale
df_4model_testnative$logit_pred <- logit(df_4model_testnative$pred_prob) 
# Note: logit(p) is log(p / (1 - p))

# Fit calibration model with logit predictions as an offset
calib_model_testnative <- glm(
  melanic_binary ~ logit_pred, 
  family = binomial, 
  data = df_4model_testnative
)

# Inspect the intercept to determine whether the baseline melanism is different between the native and introduced ranges
summary(calib_model_testnative)

########## Introduced subset -----
### Prepare dataset -----

## Prepare west coast introduced-only dataset
df_4model_introduced <- df_full %>%
  filter(introduced == "Y") %>%
  mutate(
    pop_den_scaled = (weighted_pop_density - mean(df_4model_fullnative$weighted_pop_density))/sd(df_4model_fullnative$weighted_pop_density),
    winter_temp_scaled = (avg_winter_low_temp - mean(df_4model_fullnative$avg_winter_low_temp))/sd(df_4model_fullnative$avg_winter_low_temp),
    prop_forest_scaled = (prop_forest - mean(df_4model_fullnative$prop_forest))/sd(df_4model_fullnative$prop_forest),
    RAC_20km = mean(df_4model_testnative$RAC_20km))

### Save and read model-ready datasets -----

#Save df
write_csv(df_4model_introduced, "Data/data_4model_introduced.csv")

#Read saved df with RAC
df_4model_introduced <- read_csv("Data/data_4model_introduced.csv")

### Assess predictors for correlation -----

#Pearson's r
cor(df_4model_introduced[c(10:12)], method = "pearson")

### Predict introduced morphs with native model -----

## Predict probabilities of melanism in the introduced dataset using native coefficients
df_4model_introduced$pred_prob <- predict(
  mod_native, 
  newdata = df_4model_introduced, 
  type = "response"
)

## Discrimination (AUC / ROC Curve)
roc_obj <- roc(df_4model_introduced$melanic_binary, df_4model_introduced$pred_prob)
auc(roc_obj)
plot(roc_obj) #ROC Curve: Native Model Predicting Introduced Range

## Mean Predicted Probability vs. Actual Prevalence
mean_actual <- mean(df_4model_introduced$melanic_binary, na.rm = TRUE)
mean_predicted <- mean(df_4model_introduced$pred_prob, na.rm = TRUE)

cat("Actual Introduced Prevalence:", mean_actual, "\n")
cat("Mean Predicted Prevalence:", mean_predicted, "\n")

### Introduced logistic calibration model -----

## Convert predicted probabilities back to log-odds scale
df_4model_introduced$logit_pred <- logit(df_4model_introduced$pred_prob) 
# Note: logit(p) is log(p / (1 - p))

# Fit calibration model with logit predictions as an offset
calib_model <- glm(
  melanic_binary ~ logit_pred, 
  family = binomial, 
  data = df_4model_introduced
)

# Inspect the intercept to determine whether the baseline melanism is different between the native and introduced ranges
summary(calib_model)

########## Visualizations -----
### Plot raw data -----

## Human pop den
df_full %>%
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
df_full %>%
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
df_full %>%
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

## Function to tidy and label models
tidy_model <- function(model, label) {
  tidy(model, conf.int = TRUE) %>%
    #filter(effect == "fixed") %>%
    mutate(model_type = label)
}

## Create df with confidence intervals
df_plot_data <- tidy_model(mod_native, "Native") %>%
  # Remove intercepts
  filter(!term %in% c("(Intercept)", "RAC_10km", "RAC_20km")) %>%
  # Rename components for cleaner facet labels
  mutate(term = fct_recode(term,
                             `Population density` = "pop_den_scaled",
                             `Winter temperature` = "winter_temp_scaled",
                             `Forest cover` = "prop_forest_scaled",
                             `Population density x\nWinter temperature` = "pop_den_scaled:winter_temp_scaled",
                             `Population density x\nForest cover` = "pop_den_scaled:prop_forest_scaled"),
           term = fct_rev(fct_relevel(term,
                              "Winter temperature",
                              "Population density",
                              "Forest cover",
                              "Population density x\nForest cover",
                              "Population density x\nWinter temperature")))


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
  # Prevent overlap
  geom_pointrange(aes(xmin = conf.low, xmax = conf.high),
                  position = position_dodge(width = 0.6),
                  size = 1) +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 22),
        axis.text.y = element_text(size = 22),
        axis.title.x = element_text(size = 22),
        legend.text = element_text(size = 22),
        legend.title = element_text(size = 22)) +
  theme(panel.grid.minor = element_blank(),
        strip.text = element_text(face = "bold", size = 22),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1)); CI_plot

ggsave("Figures/CI_plot.png")

### Visreg plots -----

## Human population density
visreg(mod, xvar = "pop_den_scaled", by = "introduced", scale = "response", gg = TRUE,
       xlab = "Human population density", ylab = "Probability of melanism") + 
  facet_grid(. ~ introduced, labeller = as_labeller(c("N" = "Native", "Y" = "Non-native"))) +
  theme_bw() +
  ylim(c(0,0.3))

### Map reported sightings -----

## Load range map
range <- read_sf("Data/EGS_nativerange.shp")

## Create the base map
leaflet(df_4model_introduced) %>%
  addTiles() %>%
  # Add MELANIC squirrels as one group
  addCircles(
    data = subset(df_full, col_class == "melanic"),
    ~longitude, ~latitude,
    color = "black",
    group = "Melanic",
    popup = "Melanic Squirrel",
    fillOpacity = 0.5, radius = 10, stroke = FALSE
  ) %>%
  # Add GREY squirrels as another group
  addCircles(
    data = subset(df_full, col_class == "gray"),
    ~longitude, ~latitude,
    color = "grey",
    group = "Grey",
    popup = "Grey Squirrel",
    fillOpacity = 0.5, radius = 10, stroke = FALSE
  ) %>%
  # Add OTHER squirrels as another group
  addCircles(
    data = subset(df_full, col_class == "other"),
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
  ) %>%
  # Add shape for native range
  addPolygons(
    data = range,
    fill = FALSE,
    color = "black",
    weight = 1 #set border thickness
  )
