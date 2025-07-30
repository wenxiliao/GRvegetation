# Remove variables in the global environment
rm(list = ls())

# install.packages("ggplot2")
# install.packages("dplyr")
# install.packages("knitr")
# install.packages("openxlsx")
# install.packages("MASS")
# install.packages("segmented")
# install.packages("lme4")
# install.packages("lmerTest")
# install.packages("patchwork")
# install.packages("lmerTest")
# install.packages("broom")
# install.packages("purrr")

# Packages --------------
library(ggplot2)
library(dplyr)
library(tidyr)
library(knitr)
library(openxlsx)
library(readxl)
library(lmtest)
library(MASS)
library(lme4)
library(segmented) 
library(patchwork)
library(lmerTest) 
library(broom)
library(purrr)

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

# convert the date to the correct format
ndvi_data$DATE <- as.Date(ndvi_data$DATE, origin = "1899-12-30")

# Calculate aspect ratios for green roof modules
# Some NaNs are produced due to negative values in sqrt(). This means that some polygons are not valid rectangles and thus are excluded from aspect ratio analysis.
ndvi_data$AspectRatio = aspect_ratio(ndvi_data$SHAPE_Leng, ndvi_data$SHAPE_Area)

# Calculate the coefficient of variation (COV = STD / mean + 1) 
# The normalization makes NDVI positive without small mean values, while not changing the data distribution.
ndvi_data$COV = ndvi_data$STD / (ndvi_data$MEAN + 1)

# Number of green roof modules -------------------------------
# total number of unique green roof modules
n_distinct(ndvi_data$uniqueID)

# number of grass green roof modules
grass = subset(ndvi_data, new_GRtype == "grass")
n_distinct(grass$uniqueID)

# number of woody plant green roof modules
woody_plants = subset(ndvi_data, new_GRtype == "woody_plants")
n_distinct(woody_plants$uniqueID)

# number of sedum green roof modules
sedum = subset(ndvi_data, new_GRtype == "sedum_mat")
n_distinct(sedum$uniqueID)

# number of grass-tree mixture green roof modules
mix = subset(ndvi_data, new_GRtype == "mix_grass_tree")
n_distinct(mix$uniqueID)


# Group green roofs into increasing or decreasing trend -----------------------------------------------
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


# Filter data for unique green roof modules with age >= 3 
trend_3yrs_rows <- ndvi_data %>%
  filter(ndvi_data$uniqueID %in% results_3yrs$uniqueID)

deduplicated_trend_3yrs_rows <- trend_3yrs_rows %>%
  distinct(uniqueID, .keep_all = TRUE)

# Number of unique green roof modules with age >= 3 grouped by vegetation type
plyr::count(deduplicated_trend_3yrs_rows$new_GRtype)
kable(plyr::count(deduplicated_trend_3yrs_rows$new_GRtype), col.names = c("UniqueID", "Count"), caption = "Count of vegetation type in increase")


# Category: green roof modules with decreasing trend (estimate < 0) ---------------------------------------
decrease_trend_3yrs <- results_3yrs %>%
  filter(term != "(Intercept)", estimate < 0) %>%
  select(uniqueID, term, estimate, p_adj)

decrease_trend_3yrs
num_decrease_trend = nrow(decrease_trend_3yrs)
num_decrease_trend

# Filter data for unique green roof modules with decreasing trends
decrease_3yrs_rows <- ndvi_data %>%
  filter(ndvi_data$uniqueID %in% decrease_trend_3yrs$uniqueID)

deduplicated_decrease_3yrs_rows <- decrease_3yrs_rows %>%
  distinct(uniqueID, .keep_all = TRUE)

# Number of unique green roof modules with age >= 3 grouped by vegetation type
plyr::count(deduplicated_decrease_3yrs_rows$new_GRtype)
kable(plyr::count(deduplicated_decrease_3yrs_rows$new_GRtype), col.names = c("UniqueID", "Count"), caption = "Count of vegetation type in decrease")


# Category: green roof modules with increasing trend (estimate >= 0) ---------------------------------------
increase_trend_3yrs <- results_3yrs %>%
  filter(term != "(Intercept)", estimate > 0 | estimate == 0) %>%
  select(uniqueID, term, estimate, p_adj)

increase_trend_3yrs
num_increase_trend = nrow(increase_trend_3yrs)
num_increase_trend

# Filter data for unique green roof modules with increasing trends
increase_3yrs_rows <- ndvi_data %>%
  filter(ndvi_data$uniqueID %in% increase_trend_3yrs$uniqueID)

deduplicated_increase_3yrs_rows <- increase_3yrs_rows %>%
  distinct(uniqueID, .keep_all = TRUE)

# Number of unique green roof modules with age >= 3 grouped by vegetation type
plyr::count(deduplicated_increase_3yrs_rows$new_GRtype)
kable(plyr::count(deduplicated_increase_3yrs_rows$new_GRtype), col.names = c("UniqueID", "Count"), caption = "Count of vegetation type in increase")


# Percentage of green roof modules showing increasing trends
percent_increase = num_increase_trend/num_GR_3yrs*100
percent_increase

# Percentage of green roof modules showing decreasing trends
percent_decrease = num_decrease_trend/num_GR_3yrs*100
percent_decrease


# Significant decreasing trends ------------------------------------------------------
# Filter unique green roof modules with significant trends
significant_results_3yrs <- results_3yrs %>%
  filter(term != "(Intercept)", p_adj < 0.05) %>%
  select(uniqueID, term, estimate, p_adj)

# View the green roof modules with significant p-values along with their coefficients and p-values
significant_results_3yrs

# Count number of unique green roof modules with significant trends
num_sig_results_3yrs = nrow(significant_results_3yrs)
num_sig_results_3yrs

# Get data for green roof modules with significant increasing trends
sig_increase_3yrs = subset(significant_results_3yrs, significant_results_3yrs$estimate >= 0)
sig_increase_3yrs
# Get data for green roof modules with significant decreasing trends
sig_decrease_3yrs = subset(significant_results_3yrs, significant_results_3yrs$estimate < 0)
sig_decrease_3yrs

# Get number of green roof modules with significant increasing trends
num_sig_increase_3yrs = nrow(sig_increase_3yrs)
num_sig_increase_3yrs

# Get number of green roof modules with significant decreasing trends
num_sig_decrease_3yrs = nrow(sig_decrease_3yrs)
num_sig_decrease_3yrs


# Filter data for unique green roof modules showing decreasing trend
sig_decrease_3yrs_rows <- ndvi_data %>%
  filter(ndvi_data$uniqueID %in% sig_decrease_3yrs$uniqueID)

deduplicated_sig_decrease_3yrs_rows <- sig_decrease_3yrs_rows %>%
  distinct(uniqueID, .keep_all = TRUE)

# Number of unique green roof modules showing significant decreasing trend grouped by vegetation type
vegetype_sig_decrease = plyr::count(deduplicated_sig_decrease_3yrs_rows$new_GRtype)
vegetype_sig_decrease
kable(plyr::count(deduplicated_sig_decrease_3yrs_rows$new_GRtype), col.names = c("UniqueID", "Count"), caption = "Count of vegetation type in increase")

# Percentage of green roof modules showing decreasing trends grouped by vegetation type
grass_decrease = vegetype_sig_decrease$freq[1]/sum(vegetype_sig_decrease$freq) * 100
grass_decrease
sedum_decrease = vegetype_sig_decrease$freq[2]/sum(vegetype_sig_decrease$freq) * 100
sedum_decrease
woody_decrease = vegetype_sig_decrease$freq[3]/sum(vegetype_sig_decrease$freq) * 100
woody_decrease

# Percentage of green roof modules showing decreasing trends
vegetype_percent_sig_decrease = num_sig_decrease_3yrs/num_sig_results_3yrs*100
vegetype_percent_sig_decrease


# Significant increasing trends ------------------------------------------------------
# Filter data for unique green roof modules showing increasing trend
sig_increase_3yrs_rows <- ndvi_data %>%
  filter(ndvi_data$uniqueID %in% sig_increase_3yrs$uniqueID)

deduplicated_sig_increase_3yrs_rows <- sig_increase_3yrs_rows %>%
  distinct(uniqueID, .keep_all = TRUE)

# Number of unique green roof modules showing significant incrasing trend grouped by vegetation type
vegetype_sig_increase = plyr::count(deduplicated_sig_increase_3yrs_rows$new_GRtype)
vegetype_sig_increase
kable(plyr::count(deduplicated_sig_increase_3yrs_rows$new_GRtype), col.names = c("UniqueID", "Count"), caption = "Count of vegetation type in increase")

# Percentage of green roof modules showing increasing trends grouped by vegetation type
grass_increase = vegetype_sig_increase$freq[1]/sum(vegetype_sig_increase$freq) * 100
grass_increase
mix_increase = vegetype_sig_increase$freq[2]/sum(vegetype_sig_increase$freq) * 100
mix_increase
sedum_increase = vegetype_sig_increase$freq[3]/sum(vegetype_sig_increase$freq) * 100
sedum_increase
woody_increase = vegetype_sig_increase$freq[4]/sum(vegetype_sig_increase$freq) * 100
woody_increase

# Percentage of green roof modules showing increasing trends
vegetype_percent_sig_increase = num_sig_increase_3yrs/num_sig_results_3yrs*100
vegetype_percent_sig_increase



