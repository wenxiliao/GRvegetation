# Remove variables in the global environment
rm(list = ls())

# install the package if have not
# install.packages("ggplot2")
# install.packages("dplyr")
# install.packages("openxlsx")
# install.packages("readxl")
# install.packages("lmtest")
# install.packages("MASS")
# install.packages("lme4")
# install.packages("segmented")
# install.packages("patchwork")
# install.packages("lmerTest")
# install.packages("purr")
# install.packages("broom")
# install.packages("~/data/AlgDesign_1.2.1.1.tar.gz", repos = NULL, type = "source") # package for agricolae
# install.packages("~/data/agricolae_1.3-7.tar.gz", repos = NULL, type = "source") # package for agricolae

# Packages --------------
library(ggplot2)
library(tidyr)
library(dplyr)
library(openxlsx)
library(readxl)
library(lmtest)
library(MASS)
library(lme4)
library(segmented) 
library(patchwork)
library(lmerTest) 
library(purrr)
library(broom)

# Functions -----------
# Function to calculate standard error
get_se <- function(value) {
  se <- sd(value, na.rm = TRUE)/sqrt(length(na.omit(value)))
  return(se)
}

# Function to calculate aspect ratios for green roof modules
aspect_ratio <- function(perimeter, area) {
  length <- (perimeter / 4) - sqrt(perimeter^2 / 16 - area)
  width <- perimeter / 2 - length
  
  # Calculate and return the aspect ratio (width / length)
  return(width / length)
}

# Read Data Files ------------------------------------------------
ndvi_data <- read_excel("~/data/ndvi_data.xlsx", sheet = "Sheet 1")

# Convert the date to the correct format
ndvi_data$DATE <- as.Date(ndvi_data$DATE, origin = "1899-12-30")

# Calculate aspect ratios for green roof modules
# Some NaNs are produced due to negative values in sqrt(). This means that some polygons are not valid rectangles and thus are excluded from aspect ratio analysis.
ndvi_data$AspectRatio = aspect_ratio(ndvi_data$SHAPE_Leng, ndvi_data$SHAPE_Area)

# Calculate the coefficient of variation (COV = STD / mean + 1) 
# The normalization makes NDVI positive without small mean values, while not changing the data distribution.
ndvi_data$COV = ndvi_data$STD / (ndvi_data$MEAN + 1)

# Transform the roof module area
ndvi_data$sqrt_area = sqrt(ndvi_data$SHAPE_Area)

# Transform roof module aspect ratio
ndvi_data$sqrt_AR = sqrt(ndvi_data$AspectRatio)


# Group green roofs into increasing or decreasing trend -----------------------------------------------
# if code below does not worl, reload the packages
detach("package:tidyr", unload = TRUE)
detach("package:dplyr", unload = TRUE)
library(tidyr)
library(dplyr)

# Unique green roof modules with age >= 3 
unique_id_count_with_3yrs <- ndvi_data %>%
  group_by(uniqueID) %>%
  filter(n_distinct(year) >= 3) %>%
  ungroup() %>%
  count(uniqueID)

# Total number of unique green roof modules
uniqueID_atleast_3yrs = nrow(unique_id_count_with_3yrs)
uniqueID_atleast_3yrs

# Fit a linear regression for mean NDVI vs. green roof age for each green roof module
results_3yrs <- ndvi_data %>%
  group_by(uniqueID) %>%
  filter(n_distinct(year) >= 3) %>%
  arrange(year) %>%
  nest() %>%
  mutate(
    model = map(data, ~ lm(MEAN ~ GR_age_of_year, data = .)),
    tidied = map(model, tidy)
  ) %>%
  unnest(tidied) %>% 
  filter(term == "GR_age_of_year") %>%  
  mutate(p_adj = p.adjust(p.value, method = "fdr")) 

results_3yrs
num_GR_3yrs = nrow(results_3yrs)


### Green roof modules decreasing trend (estimate < 0) ###
decrease_trend_3yrs <- results_3yrs %>%
  filter(term != "(Intercept)", estimate < 0) %>%
  select(uniqueID, term, estimate, p.value)

decrease_trend_3yrs
num_decrease_trend = nrow(decrease_trend_3yrs)


### Green roof modules increasing trend (estimate ≥ 0) ###
increase_trend_3yrs <- results_3yrs %>%
  filter(term != "(Intercept)", estimate >= 0) %>%
  select(uniqueID, term, estimate, p.value)

increase_trend_3yrs
num_increase_trend = nrow(increase_trend_3yrs)


# Effects of design factors on green roof modules showing decreasing trends ##### ------------------------------------
# Get the data for green roof modules showing decreasing trends ##
decrease_uniqueID_3yrs = decrease_trend_3yrs$uniqueID
length(decrease_uniqueID_3yrs)

# Filter the data to get the full dataset for the green roof modules in decreasing category ##
decrease_ndvi_3yrs <- ndvi_data %>%
  filter(uniqueID %in% decrease_uniqueID_3yrs)
decrease_ndvi_3yrs
nrow(decrease_ndvi_3yrs)

# Subset 2018 data (pre-growing season)
decrease_ndvi_3yrs_2018 = subset(decrease_ndvi_3yrs, year == 2018)
nrow(decrease_ndvi_3yrs_2018)

# Subset 2017 data (growing season)
decrease_ndvi_3yrs_2017 = subset(decrease_ndvi_3yrs, year == 2017)
nrow(decrease_ndvi_3yrs_2017) 


### Model Selection (pre-growing season) ### ------------------------------------
model_decrease_ndvi_3yrs_2018_GRtype <- lm(MEAN~SHAPE_Area + GR_type + MAX_HEIGHT + AspectRatio, data=na.omit(decrease_ndvi_3yrs_2018))
model_decrease_ndvi_3yrs_2018_VegeType <- lm(MEAN~SHAPE_Area + new_GRtype + MAX_HEIGHT + AspectRatio, data=na.omit(decrease_ndvi_3yrs_2018))

model_decrease_ndvi_3yrs_2018_GRtype <- step(model_decrease_ndvi_3yrs_2018_GRtype, direction = "both") 
model_decrease_ndvi_3yrs_2018_VegeType <- step(model_decrease_ndvi_3yrs_2018_VegeType, direction = "both") 

# Final model grouped by green roof type  
summary(model_decrease_ndvi_3yrs_2018_GRtype)
# Final model grouped by vegetation type  
summary(model_decrease_ndvi_3yrs_2018_VegeType)


### Model Selection (growing season) ### ------------------------------------
model_decrease_ndvi_3yrs_2017_GRtype <- lm(MEAN~SHAPE_Area + GR_type + MAX_HEIGHT + AspectRatio, data=decrease_ndvi_3yrs_2017)
model_decrease_ndvi_3yrs_2017_VegeType <- lm(MEAN~SHAPE_Area + new_GRtype + MAX_HEIGHT + AspectRatio, data=decrease_ndvi_3yrs_2017)

model_decrease_ndvi_3yrs_2017_GRtype <- step(model_decrease_ndvi_3yrs_2017_GRtype, direction = "both") 
model_decrease_ndvi_3yrs_2017_VegeType <- step(model_decrease_ndvi_3yrs_2017_VegeType, direction = "both") 

# Final model grouped by green roof type  
summary(model_decrease_ndvi_3yrs_2017_GRtype)
# Final model grouped by vegetation type  
summary(model_decrease_ndvi_3yrs_2017_VegeType)


# Effects of design factors on green roof modules showing increasing trends ##### ------------------------------------
# Get the data for green roof modules showing increasing trends ##
increase_uniqueID_3yrs = increase_trend_3yrs$uniqueID

# Filter the data to get the full dataset for the green roof modules in decreasing category ##
increase_ndvi_3yrs <- ndvi_data %>%
  filter(uniqueID %in% increase_uniqueID_3yrs)

increase_ndvi_3yrs
nrow(increase_ndvi_3yrs)

# Subset 2018 data (pre-growing season)
increase_ndvi_3yrs_2018 = subset(increase_ndvi_3yrs, year == 2018)
nrow(increase_ndvi_3yrs_2018)

# Subset 2017 data (growing season)
increase_ndvi_3yrs_2017 = subset(increase_ndvi_3yrs, year == 2017)
nrow(increase_ndvi_3yrs_2017) 


### Model Selection (pre-growing season) ### ------------------------------------
model_increase_ndvi_3yrs_2018_GRtype <- lm(MEAN~SHAPE_Area + GR_type + MAX_HEIGHT + AspectRatio, data=na.omit(increase_ndvi_3yrs_2018))
model_increase_ndvi_3yrs_2018_VegeType <- lm(MEAN~SHAPE_Area + new_GRtype + MAX_HEIGHT + AspectRatio, data=na.omit(increase_ndvi_3yrs_2018))

model_increase_ndvi_3yrs_2018_GRtype <- step(model_increase_ndvi_3yrs_2018_GRtype, direction = "both") 
model_increase_ndvi_3yrs_2018_VegeType <- step(model_increase_ndvi_3yrs_2018_VegeType, direction = "both") 

# Final model grouped by green roof type  
summary(model_increase_ndvi_3yrs_2018_GRtype)
# Final model grouped by vegetation type  
summary(model_increase_ndvi_3yrs_2018_VegeType)


### Model Selection (growing season) ### ------------------------------------
model_increase_ndvi_3yrs_2017_GRtype <- lm(MEAN~SHAPE_Area + GR_type + MAX_HEIGHT + AspectRatio, data=increase_ndvi_3yrs_2017)
model_increase_ndvi_3yrs_2017_VegeType <- lm(MEAN~SHAPE_Area + new_GRtype + MAX_HEIGHT + AspectRatio, data=increase_ndvi_3yrs_2017)

model_increase_ndvi_3yrs_2017_GRtype <- step(model_increase_ndvi_3yrs_2017_GRtype, direction = "both") 
model_increase_ndvi_3yrs_2017_VegeType <- step(model_increase_ndvi_3yrs_2017_VegeType, direction = "both") 

# Final model grouped by green roof type  
summary(model_increase_ndvi_3yrs_2017_GRtype)
# Final model grouped by vegetation type  
summary(model_increase_ndvi_3yrs_2017_VegeType)


