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

# packages --------------
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
savi_data <- read_excel("~/data/savi_data.xlsx", sheet = "Sheet 1")
gndvi_data <- read_excel("~/data/gndvi_data.xlsx", sheet = "Sheet 1")
cig_data <- read_excel("~/data/cig_data.xlsx", sheet = "Sheet 1")

# Convert the date to the correct format
ndvi_data$DATE <- as.Date(ndvi_data$DATE, origin = "1899-12-30")
savi_data$DATE <- as.Date(savi_data$DATE, origin = "1899-12-30")
gndvi_data$DATE <- as.Date(gndvi_data$DATE, origin = "1899-12-30")
cig_data$DATE <- as.Date(cig_data$DATE, origin = "1899-12-30")

# Calculate aspect ratios for green roof modules
# Some NaNs are produced due to negative values in sqrt(). This means that some polygons are not valid rectangles and thus are excluded from aspect ratio analysis.
ndvi_data$AspectRatio = aspect_ratio(ndvi_data$SHAPE_Leng, ndvi_data$SHAPE_Area)
savi_data$AspectRatio = aspect_ratio(savi_data$SHAPE_Leng, savi_data$SHAPE_Area)
gndvi_data$AspectRatio = aspect_ratio(gndvi_data$SHAPE_Leng, gndvi_data$SHAPE_Area)
cig_data$AspectRatio = aspect_ratio(cig_data$SHAPE_Leng, cig_data$SHAPE_Area)


# Calculate the coefficient of variation (COV = STD / mean + 1) 
# The normalization makes NDVI positive without small mean values, while not changing the data distribution.
ndvi_data$COV = ndvi_data$STD / (ndvi_data$MEAN + 1)
savi_data$COV = savi_data$STD / (savi_data$MEAN + 1)
gndvi_data$COV = gndvi_data$STD / (gndvi_data$MEAN + 1)
cig_data$COV = cig_data$STD / (cig_data$MEAN + 1)


# Count number of green roofs and roof modules -----------------------------
# Number of green roofs
sum(plyr::count(plyr::count(ndvi_data$permit_num)$x)$freq)

# Number of roof modules
sum(plyr::count(plyr::count(ndvi_data$uniqueID)$x)$freq)



# Figure 3: mean NDVI changes with green roof age --------------------------------
###### Mean NDVI #######
# Change order of legend display
ndvi_data$leaf <- factor(ndvi_data$leaf, levels = c("leafy", "leafless"))

# Plot
mean_ndvi.plot <- ggplot(ndvi_data, aes(x=GR_age_of_year,y=MEAN, color = leaf, linetype = leaf))+
  geom_point(aes(shape = leaf), size=3.5, alpha = 0.3) +
  geom_smooth(aes(color = leaf, fill=leaf, linetype = leaf),
              method='lm', formula= y~x, alpha = 0.3, cex=1.3)+
  labs(x="Green roof age (year)", y="Mean NDVI",
       group = "Seasonality", linetype = "Seasonality", pch = "Seasonality")+
  scale_color_manual(name="Seasonality",labels = c("Growing season", "Pre-growing season"),values = c("#28BD3F","#C67B39"))+
  scale_fill_manual(name="Seasonality",labels = c("Growing season", "Pre-growing season"),values = c("#28BD3F","#C67B39"))+
  scale_linetype_manual(name="Seasonality", labels = c("Growing season", "Pre-growing season"), values = c(1,2))+
  scale_shape_manual(name="Seasonality", labels = c("Growing season", "Pre-growing season"), values = c(17,19))+
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8))+
  theme_bw(base_size = 28)+
  theme(
    legend.position = "top",
    legend.title = element_text(size=36),
    legend.text = element_text(size = 32),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.text=element_text(size=28),
    axis.title=element_text(size=32)
  )

mean_ndvi.plot


### Statistics: linear mixed effect models ###
### Overall trends ###
# Include DATE and uniqueID as random factors
model_ndvi <- lmer(MEAN ~ GR_age_of_year + DATE + leaf + (1|uniqueID) + (1|DATE), data = ndvi_data)
summary(model_ndvi)

# Model selection for the best predictors
selected_model_ndvi <- step(model_ndvi, direction = "both")
print(selected_model_ndvi)

# Final model based on the predictor selection
final_model_ndvi <- lmer(MEAN ~ GR_age_of_year + leaf + (1|uniqueID) + (1|DATE), data = ndvi_data)
summary(final_model_ndvi)

### Trend grouped by seasons ###
## Growing seasons 
# Mean NDVI vs. Green roof age 
summary(lmer(MEAN ~ GR_age_of_year + (1|uniqueID) + (1|DATE), data = subset(ndvi_data, leaf == "leafy"))) 

# Mean NDVI vs. Date 
summary(lmer(MEAN ~ DATE + (1|uniqueID) + (1|DATE), data = subset(ndvi_data, leaf == "leafy"))) 

## Pre-growing seasons 
# Mean NDVI vs. Green roof age 
summary(lmer(MEAN ~ GR_age_of_year + (1|uniqueID) + (1|DATE), data = subset(ndvi_data, leaf == "leafless"))) 

# Mean NDVI vs. Date 
summary(lmer(MEAN ~ DATE + (1|uniqueID) + (1|DATE), data = subset(ndvi_data, leaf == "leafless"))) 


###### COV NDVI #######
# Change order of legend display
ndvi_data$leaf <- factor(ndvi_data$leaf, levels = c("leafy", "leafless"))

# Plot
CV_ndvi.plot <- ggplot(ndvi_data, aes(x=GR_age_of_year,y=COV, color = leaf, linetype = leaf))+
  geom_point(aes(shape = leaf), size=3.5, alpha = 0.3) +
  geom_smooth(aes(color = leaf, fill=leaf, linetype = leaf),
              method='lm', formula= y~x, alpha = 0.3, cex=1.3)+
  labs(x="Green roof age (year)", y="CV of NDVI",
       group = "Seasonality", linetype = "Seasonality", pch = "Seasonality")+
  scale_color_manual(name="Seasonality",labels = c("Growing season", "Pre-growing season"),values = c("#28BD3F","#C67B39"))+
  scale_fill_manual(name="Seasonality",labels = c("Growing season", "Pre-growing season"),values = c("#28BD3F","#C67B39"))+
  scale_linetype_manual(name="Seasonality", labels = c("Growing season", "Pre-growing season"), values = c(2,1))+
  scale_shape_manual(name="Seasonality", labels = c("Growing season", "Pre-growing season"), values = c(17,19))+
  scale_x_continuous(breaks = c(1,2,3,4,5, 6,7,8))+
  theme_bw(base_size = 28)+
  theme(
    legend.position = "top",
    legend.title = element_text(size=36),
    legend.text = element_text(size = 32),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.text=element_text(size=28),
    axis.title=element_text(size=32)
  )

CV_ndvi.plot


### Statistics: linear mixed effect models ###
### Overall trends ###
# Include DATE and uniqueID as random factors
model_ndvi <- lmer(COV ~ GR_age_of_year + DATE + leaf + (1|uniqueID) + (1|DATE), data = ndvi_data)
summary(model_ndvi)
step(model_ndvi, direction = "both")

# Final model based on the selection
summary(lmer(COV ~ GR_age_of_year + DATE + leaf + (1|uniqueID) + (1|DATE), data = ndvi_data)) 

### Trend grouped by seasons ###
## Growing seasons 
# COV NDVI vs. Green roof age 
summary(lmer(COV ~ GR_age_of_year + (1|uniqueID) + (1|DATE), data = subset(ndvi_data, leaf == "leafy"))) 

# COV NDVI vs. Date 
summary(lmer(COV ~ DATE + (1|uniqueID) + (1|DATE), data = subset(ndvi_data, leaf == "leafy"))) 

## Pre-growing seasons 
# COV NDVI vs. Green roof age 
summary(lmer(COV ~ GR_age_of_year + (1|uniqueID) + (1|DATE), data = subset(ndvi_data, leaf == "leafless"))) 

# COV NDVI vs. Date 
summary(lmer(COV ~ DATE + (1|uniqueID) + (1|DATE), data = subset(ndvi_data, leaf == "leafless"))) 


###### Combine Figure 3: a. Mean NDVI and b. COV NDVI ######
merge_NDVI_overall = mean_ndvi.plot + CV_ndvi.plot +
  plot_layout(guides = "collect", ncol = 2) & theme(legend.position = "top")

merge_NDVI_overall[[1]] = merge_NDVI_overall[[1]] + theme(legend.position = "none")

merge_NDVI_overall

# ggsave(plot=merge_NDVI_overall, filename = "path/Fig_NDVI_overall.tif", width = 15, height = 7, device='tiff', dpi=600,unit="in")


###### Mean NDVI grouped by green roof type (intensive vs. extensive green roofs) #######
# Change order of legend display
ndvi_data$GR_type <- factor(ndvi_data$GR_type, levels = c("ext", "int"))

# Plot for growing seasons
mean_ndvi_GRtype_leafy.plot <- ggplot(ndvi_data[ndvi_data$leaf == "leafy",], aes(x=GR_age_of_year,y=MEAN, color = GR_type, linetype = GR_type))+
  geom_point(aes(shape = GR_type), size=3, alpha = 0.3) +
  geom_smooth(aes(color = GR_type, fill=GR_type, linetype = GR_type),
              method='lm', formula= y~x, alpha = 0.2, cex=1.3, se = T)+
  labs(x="Green roof age", y="Mean NDVI",
       group = "GR_type", linetype = "GR_type", pch = "GR_type")+
  scale_color_manual(name="Green roof type",labels = c("Extensive","Intensive"),values = c("#FC7307","#236AB9"))+
  scale_fill_manual(name="Green roof type",labels = c("Extensive","Intensive"),values = c("#FC7307","#236AB9"))+
  scale_linetype_manual(name="Green roof type", labels = c("Extensive","Intensive"), values = c(1,1))+
  scale_shape_manual(name="Green roof type", labels = c("Extensive","Intensive"), values = c(19,17))+
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8))+
  theme_bw(base_size = 28)+
  theme(
    legend.position = "top",
    legend.box = "vertical",
    legend.title = element_text(size=36),
    legend.text = element_text(size = 32),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.text=element_text(size=28),
    axis.title=element_text(size=32)
  ) 

mean_ndvi_GRtype_leafy.plot


# Change order of legend display
ndvi_data$GR_type <- factor(ndvi_data$GR_type, levels = c("ext", "int"))

# Plot for pre-growing seasons
mean_ndvi_GRtype_leafless.plot <- ggplot(ndvi_data[ndvi_data$leaf == "leafless",], aes(x=GR_age_of_year,y=MEAN, color = GR_type, linetype = GR_type))+
  geom_point(aes(shape = GR_type), size=3, alpha = 0.3) +
  geom_smooth(aes(color = GR_type, fill=GR_type, linetype = GR_type),
              method='lm', formula= y~x, alpha = 0.2, cex=1.3, se = T)+
  labs(x="Green roof age (year)", y="Mean NDVI",
       group = "GR_type", linetype = "GR_type", pch = "GR_type")+
  scale_color_manual(name="Green roof type",labels = c("Extensive","Intensive"),values = c("#FC7307","#236AB9"))+
  scale_fill_manual(name="Green roof type",labels = c("Extensive","Intensive"),values = c("#FC7307","#236AB9"))+
  scale_linetype_manual(name="Green roof type", labels = c("Extensive","Intensive"), values = c(2,2))+
  scale_shape_manual(name="Green roof type", labels = c("Extensive","Intensive"), values = c(19,17))+
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8))+
  theme_bw(base_size = 28)+
  theme(
    legend.position = "top",
    legend.box = "vertical",
    legend.title = element_text(size=36),
    legend.text = element_text(size = 32),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.text=element_text(size=28),
    axis.title=element_text(size=32)
  ) 

mean_ndvi_GRtype_leafless.plot


### Statistics: linear mixed effect models ###
### Overall trends ###
# Include DATE and uniqueID as random factors
model_ndvi <- lmer(MEAN ~ GR_age_of_year + DATE + leaf + GR_type + (1|uniqueID) + (1|DATE), data = ndvi_data)
summary(model_ndvi)

# Model selection for the best predictors
selected_model_ndvi <- step(model_ndvi, direction = "both") # shows the steps: has AIC
print(selected_model_ndvi)

# Final model based on the selection
final_model_ndvi <- lmer(MEAN ~ GR_age_of_year + leaf + GR_type + (1 | uniqueID) + (1 | DATE), data = ndvi_data)
summary(final_model_ndvi)

### Trend grouped by seasons ###
## Growing seasons 
# Mean NDVI vs. Green roof age 
summary(lmer(MEAN ~ GR_age_of_year + (1|uniqueID) + (1|DATE), data = subset(ndvi_data, leaf == "leafy"))) 
# Mean NDVI vs. Date
summary(lmer(MEAN ~ DATE + (1|uniqueID) + (1|DATE), data = subset(ndvi_data, leaf == "leafy"))) 

## Grouped by green roof type 
# Mean NDVI vs. Green roof age 
# Extensive green roofs
summary(lmer(MEAN ~ GR_age_of_year + (1|uniqueID) + (1|DATE), data = subset(ndvi_data, leaf == "leafy" & GR_type == "ext"))) 
# Intensive green roofs
summary(lmer(MEAN ~ GR_age_of_year + (1|uniqueID) + (1|DATE), data = subset(ndvi_data, leaf == "leafy" & GR_type == "int"))) 

# Mean NDVI vs. Date
# Extensive green roofs
summary(lmer(MEAN ~ DATE + (1|uniqueID) + (1|DATE), data = subset(ndvi_data, leaf == "leafy" & GR_type == "ext"))) 
# Intensive green roofs
summary(lmer(MEAN ~ DATE + (1|uniqueID) + (1|DATE), data = subset(ndvi_data, leaf == "leafy" & GR_type == "int"))) 


## Pre-growing seasons 
# Mean NDVI vs. Green roof age 
summary(lmer(MEAN ~ GR_age_of_year + (1|uniqueID) + (1|DATE), data = subset(ndvi_data, leaf == "leafless"))) 
# Mean NDVI vs. Date
summary(lmer(MEAN ~ DATE + (1|uniqueID) + (1|DATE), data = subset(ndvi_data, leaf == "leafless"))) 

### Grouped by green roof type ###
# Mean NDVI vs. Green roof age 
# Extensive green roofs
summary(lmer(MEAN ~ GR_age_of_year + (1|uniqueID) + (1|DATE), data = subset(ndvi_data, leaf == "leafless" & GR_type == "ext"))) 
# Intensive green roofs
summary(lmer(MEAN ~ GR_age_of_year + (1|uniqueID) + (1|DATE), data = subset(ndvi_data, leaf == "leafless" & GR_type == "int"))) 

# Mean NDVI vs. Date
# Extensive green roofs
summary(lmer(MEAN ~ DATE + (1|uniqueID) + (1|DATE), data = subset(ndvi_data, leaf == "leafless" & GR_type == "ext"))) 
# Intensive green roofs
summary(lmer(MEAN ~ DATE + (1|uniqueID) + (1|DATE), data = subset(ndvi_data, leaf == "leafless" & GR_type == "int"))) 


###### COV NDVI grouped by green roof type (intensive vs. extensive green roofs) #######
# Change order of legend display
ndvi_data$GR_type <- factor(ndvi_data$GR_type, levels = c("ext", "int"))

# Plot for growing seasons
CV_ndvi_GRtype_leafy.plot <- ggplot(ndvi_data[ndvi_data$leaf == "leafy",], aes(x=GR_age_of_year,y=COV, color = GR_type, linetype = GR_type))+
  geom_point(aes(shape = GR_type), size=3, alpha = 0.3) +
  geom_smooth(aes(color = GR_type, fill=GR_type, linetype = GR_type),
              method='lm', formula= y~x, alpha = 0.2, cex=1.3, se = T)+
  labs(x="Green roof age (year)", y="CV of NDVI",
       group = "GR_type", linetype = "GR_type", pch = "GR_type")+
  scale_color_manual(name="Green roof type",labels = c("Extensive","Intensive"),values = c("#FC7307","#236AB9"))+
  scale_fill_manual(name="Green roof type",labels = c("Extensive","Intensive"),values = c("#FC7307","#236AB9"))+
  scale_linetype_manual(name="Green roof type", labels = c("Extensive","Intensive"), values = c(2,2))+
  scale_shape_manual(name="Green roof type", labels = c("Extensive","Intensive"), values = c(19,17))+
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8))+
  theme_bw(base_size = 28)+
  theme(
    legend.position = "top",
    legend.box = "vertical",
    legend.title = element_text(size=36),
    legend.text = element_text(size = 32),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.text=element_text(size=28),
    axis.title=element_text(size=32)
  ) 

CV_ndvi_GRtype_leafy.plot


# Change order of display
ndvi_data$GR_type <- factor(ndvi_data$GR_type, levels = c("ext", "int"))

# Plot for pre-growing seasons
CV_ndvi_GRtype_leafless.plot <- ggplot(ndvi_data[ndvi_data$leaf == "leafless",], aes(x=GR_age_of_year,y=COV, color = GR_type, linetype = GR_type))+
  geom_point(aes(shape = GR_type), size=3, alpha = 0.3) +
  geom_smooth(aes(color = GR_type, fill=GR_type, linetype = GR_type),
              method='lm', formula= y~x, alpha = 0.2, cex=1.3, se = T)+
  labs(x="Green roof age (year)", y="CV of NDVI",
       group = "GR_type", linetype = "GR_type", pch = "GR_type")+
  scale_color_manual(name="Green roof type",labels = c("Extensive","Intensive"),values = c("#FC7307","#236AB9"))+
  scale_fill_manual(name="Green roof type",labels = c("Extensive","Intensive"),values = c("#FC7307","#236AB9"))+
  scale_linetype_manual(name="Green roof type", labels = c("Extensive","Intensive"), values = c(1,1))+
  scale_shape_manual(name="Green roof type", labels = c("Extensive","Intensive"), values = c(19,17))+
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8))+
  theme_bw(base_size = 28)+
  theme(
    legend.position = "top",
    legend.box = "vertical",
    legend.title = element_text(size=36),
    legend.text = element_text(size = 32),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.text=element_text(size=28),
    axis.title=element_text(size=32)
  ) 

CV_ndvi_GRtype_leafless.plot


### Statistics: linear mixed effect models ###
### Overall trends ###
# Include DATE and uniqueID as random factors
model_ndvi <- lmer(COV ~ GR_age_of_year + DATE + leaf + GR_type + (1|uniqueID) + (1|DATE), data = ndvi_data)
summary(model_ndvi)

# Model selection for the best predictors
selected_model_ndvi <- step(model_ndvi, direction = "both") # shows the steps: has AIC
print(selected_model_ndvi)

# Final model based on the selection
final_model_ndvi <- lmer(COV ~ GR_age_of_year + DATE + leaf + GR_type + (1 | uniqueID) + (1 | DATE), data = ndvi_data)
summary(final_model_ndvi)

### Trend grouped by seasons ###
## Growing seasons 
# COV NDVI vs. Green roof age 
summary(lmer(COV ~ GR_age_of_year + (1|uniqueID) + (1|DATE), data = subset(ndvi_data, leaf == "leafy"))) 
# Mean NDVI vs. Date
summary(lmer(COV ~ DATE + (1|uniqueID) + (1|DATE), data = subset(ndvi_data, leaf == "leafy"))) 

## Grouped by green roof type 
# COV NDVI vs. Green roof age 
# Extensive green roofs
summary(lmer(COV ~ GR_age_of_year + (1|uniqueID) + (1|DATE), data = subset(ndvi_data, leaf == "leafy" & GR_type == "ext"))) 
# Intensive green roofs
summary(lmer(COV ~ GR_age_of_year + (1|uniqueID) + (1|DATE), data = subset(ndvi_data, leaf == "leafy" & GR_type == "int"))) 

# COV NDVI vs. Date
# Extensive green roofs
summary(lmer(COV ~ DATE + (1|uniqueID) + (1|DATE), data = subset(ndvi_data, leaf == "leafy" & GR_type == "ext"))) 
# Intensive green roofs
summary(lmer(COV ~ DATE + (1|uniqueID) + (1|DATE), data = subset(ndvi_data, leaf == "leafy" & GR_type == "int"))) 


## Pre-growing seasons 
# COV NDVI vs. Green roof age 
summary(lmer(COV ~ GR_age_of_year + (1|uniqueID) + (1|DATE), data = subset(ndvi_data, leaf == "leafless"))) 
# COV NDVI vs. Date
summary(lmer(COV ~ DATE + (1|uniqueID) + (1|DATE), data = subset(ndvi_data, leaf == "leafless"))) 

## Grouped by green roof type 
# COV NDVI vs. Green roof age 
# Extensive green roofs
summary(lmer(COV ~ GR_age_of_year + (1|uniqueID) + (1|DATE), data = subset(ndvi_data, leaf == "leafless" & GR_type == "ext"))) 
# Intensive green roofs
summary(lmer(COV ~ GR_age_of_year + (1|uniqueID) + (1|DATE), data = subset(ndvi_data, leaf == "leafless" & GR_type == "int"))) 

# COV NDVI vs. Date
# Extensive green roofs
summary(lmer(COV ~ DATE + (1|uniqueID) + (1|DATE), data = subset(ndvi_data, leaf == "leafless" & GR_type == "ext"))) 
# Intensive green roofs
summary(lmer(COV ~ DATE + (1|uniqueID) + (1|DATE), data = subset(ndvi_data, leaf == "leafless" & GR_type == "int"))) 


###### Combine SI Figure S1: a. Mean NDVI leaf-on; b. Mean NDVI leaf-off; c. CV NDVI leaf-on; d. CV NDVI leaf-off ######
merge_NDVI_GR_type = mean_ndvi_GRtype_leafy.plot + mean_ndvi_GRtype_leafless.plot + 
  CV_ndvi_GRtype_leafy.plot + CV_ndvi_GRtype_leafless.plot +
  plot_layout(guides = "collect", ncol = 2) & theme(legend.position = "top")

merge_NDVI_GR_type[[1]] = merge_NDVI_GR_type[[1]] + theme(axis.text.x = element_blank(),axis.title.x = element_blank(),legend.position = "none")
merge_NDVI_GR_type[[2]] = merge_NDVI_GR_type[[2]] + theme(axis.text.x = element_blank(),axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position = "none")
merge_NDVI_GR_type[[3]] = merge_NDVI_GR_type[[3]] + theme(legend.position = "none")
merge_NDVI_GR_type[[4]] = merge_NDVI_GR_type[[4]] + theme(axis.title.y = element_blank())

merge_NDVI_GR_type

# ggsave(plot=merge_NDVI_GR_type, filename = "path/Fig_NDVI_Age_GR_type.tif", width = 14, height = 10, device='tiff', dpi=600,unit="in")

###### Mean NDVI grouped by vegetation type #######
# change order of display
ndvi_data$new_GRtype <- factor(ndvi_data$new_GRtype, levels = c("grass", "sedum_mat", "woody_plants","mix_grass_tree"))

# Plot for growing seasons
mean_ndvi_vege_type_leafy.plot <- ggplot(ndvi_data[ndvi_data$leaf == "leafy",], aes(x=GR_age_of_year,y=MEAN, color = new_GRtype, linetype = new_GRtype))+
  geom_point(aes(shape = new_GRtype), size=3, alpha = 0.3) +
  geom_smooth(aes(color = new_GRtype, fill=new_GRtype, linetype = new_GRtype),
              method='lm', formula= y~x, alpha = 0.2, cex=1.3, se = T)+
  labs(x="Green roof age (year)", y="Mean NDVI",
       group = "new_GRtype", linetype = "new_GRtype", pch = "new_GRtype")+
  scale_color_manual(name="Vegetation type",labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"),values = c("#00BFFF", "#FF762E", "#7CAE00","#cd6ee8"))+
  scale_fill_manual(name="Vegetation type",labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"),values = c("#00BFFF", "#FF762E", "#7CAE00","#cd6ee8"))+
  scale_linetype_manual(name="Vegetation type", labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"), values = c(2,1, 1, 2))+
  scale_shape_manual(name="Vegetation type", labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"), values = c(19,17, 15, 18))+
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8))+
  theme_bw(base_size = 28)+
  theme(
    legend.position = "top",
    legend.box = "vertical",
    legend.title = element_text(size=36),
    legend.text = element_text(size = 32),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.text=element_text(size=28),
    axis.title=element_text(size=32)
  ) +
  guides(
    color = guide_legend(nrow = 2, byrow = TRUE),
    linetype = guide_legend(nrow = 2, byrow = TRUE)
  )

mean_ndvi_vege_type_leafy.plot


# change order of display
ndvi_data$new_GRtype <- factor(ndvi_data$new_GRtype, levels = c("grass", "sedum_mat", "woody_plants","mix_grass_tree"))

# Plot for pre-growing seasons
mean_ndvi_vege_type_leafless.plot <- ggplot(ndvi_data[ndvi_data$leaf == "leafless",], aes(x=GR_age_of_year,y=MEAN, color = new_GRtype, linetype = new_GRtype))+
  geom_point(aes(shape = new_GRtype), size=3, alpha = 0.3) +
  geom_smooth(aes(color = new_GRtype, fill=new_GRtype, linetype = new_GRtype),
              method='lm', formula= y~x, alpha = 0.2, cex=1.3, se = T)+
  labs(x="Green roof age (year)", y="Mean NDVI",
       group = "new_GRtype", linetype = "new_GRtype", pch = "new_GRtype")+
  scale_color_manual(name="Vegetation type",labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"),values = c("#00BFFF", "#FF762E", "#7CAE00","#cd6ee8"))+
  scale_fill_manual(name="Vegetation type",labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"),values = c("#00BFFF", "#FF762E", "#7CAE00","#cd6ee8"))+
  scale_linetype_manual(name="Vegetation type", labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"), values = c(2,1, 1, 2))+
  scale_shape_manual(name="Vegetation type", labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"), values = c(19,17, 15, 18))+
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8))+
  theme_bw(base_size = 28)+
  theme(
    legend.position = "top",
    legend.box = "vertical",
    legend.title = element_text(size=36),
    legend.text = element_text(size = 32),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.text=element_text(size=28),
    axis.title=element_text(size=32)
  ) +
  guides(
    color = guide_legend(nrow = 2, byrow = TRUE),
    linetype = guide_legend(nrow = 2, byrow = TRUE)
  )

mean_ndvi_vege_type_leafless.plot


### Statistics: linear mixed effect models ###
### Overall trends ###
# Include DATE and uniqueID as random factors
model_ndvi <- lmer(MEAN ~ GR_age_of_year + DATE + leaf + new_GRtype + (1|uniqueID) + (1|DATE), data = ndvi_data)
summary(model_ndvi)

# Model selection for the best predictors
selected_model_ndvi <- step(model_ndvi, direction = "both") # shows the steps: has AIC
print(selected_model_ndvi)

# Final model based on the selection
final_model_ndvi <- lmer(MEAN ~ GR_age_of_year + DATE + leaf + new_GRtype + (1 | uniqueID) + (1 | DATE), data = ndvi_data)
summary(final_model_ndvi)

### Trend grouped by seasons ###
## Growing seasons 
# Mean NDVI vs. Green roof age 
summary(lmer(MEAN ~ GR_age_of_year + (1|uniqueID) + (1|DATE), data = subset(ndvi_data, leaf == "leafy"))) 
# Mean NDVI vs. Date
summary(lmer(MEAN ~ DATE + (1|uniqueID) + (1|DATE), data = subset(ndvi_data, leaf == "leafy"))) 

## Grouped by vegetation type 
# Mean NDVI vs. Green roof age 
summary(lmer(MEAN ~ GR_age_of_year + (1|uniqueID) + (1|DATE), data = subset(ndvi_data, leaf == "leafy" & new_GRtype == "grass"))) 
summary(lmer(MEAN ~ GR_age_of_year + (1|uniqueID) + (1|DATE), data = subset(ndvi_data, leaf == "leafy" & new_GRtype == "sedum_mat"))) 
summary(lmer(MEAN ~ GR_age_of_year + (1|uniqueID) + (1|DATE), data = subset(ndvi_data, leaf == "leafy" & new_GRtype == "woody_plants"))) 
summary(lmer(MEAN ~ GR_age_of_year + (1|uniqueID) + (1|DATE), data = subset(ndvi_data, leaf == "leafy" & new_GRtype == "mix_grass_tree"))) 

# Mean NDVI vs. Date
summary(lmer(MEAN ~ DATE + (1|uniqueID) + (1|DATE), data = subset(ndvi_data, leaf == "leafy" & new_GRtype == "grass"))) 
summary(lmer(MEAN ~ DATE + (1|uniqueID) + (1|DATE), data = subset(ndvi_data, leaf == "leafy" & new_GRtype == "sedum_mat"))) 
summary(lmer(MEAN ~ DATE + (1|uniqueID) + (1|DATE), data = subset(ndvi_data, leaf == "leafy" & new_GRtype == "woody_plants"))) 
summary(lmer(MEAN ~ DATE + (1|uniqueID) + (1|DATE), data = subset(ndvi_data, leaf == "leafy" & new_GRtype == "mix_grass_tree"))) 

## Pre-growing seasons 
# Mean NDVI vs. Green roof age 
summary(lmer(MEAN ~ GR_age_of_year + (1|uniqueID) + (1|DATE), data = subset(ndvi_data, leaf == "leafless"))) 
# Mean NDVI vs. Date
summary(lmer(MEAN ~ DATE + (1|uniqueID) + (1|DATE), data = subset(ndvi_data, leaf == "leafless"))) 

## Grouped by vegetation type 
# Mean NDVI vs. Green roof age 
summary(lmer(MEAN ~ GR_age_of_year + (1|uniqueID) + (1|DATE), data = subset(ndvi_data, leaf == "leafless" & new_GRtype == "grass"))) 
summary(lmer(MEAN ~ GR_age_of_year + (1|uniqueID) + (1|DATE), data = subset(ndvi_data, leaf == "leafless" & new_GRtype == "sedum_mat"))) 
summary(lmer(MEAN ~ GR_age_of_year + (1|uniqueID) + (1|DATE), data = subset(ndvi_data, leaf == "leafless" & new_GRtype == "woody_plants"))) 
summary(lmer(MEAN ~ GR_age_of_year + (1|uniqueID) + (1|DATE), data = subset(ndvi_data, leaf == "leafless" & new_GRtype == "mix_grass_tree"))) 

# Mean NDVI vs. Date
summary(lmer(MEAN ~ DATE + (1|uniqueID) + (1|DATE), data = subset(ndvi_data, leaf == "leafless" & new_GRtype == "grass"))) 
summary(lmer(MEAN ~ DATE + (1|uniqueID) + (1|DATE), data = subset(ndvi_data, leaf == "leafless" & new_GRtype == "sedum_mat"))) 
summary(lmer(MEAN ~ DATE + (1|uniqueID) + (1|DATE), data = subset(ndvi_data, leaf == "leafless" & new_GRtype == "woody_plants"))) 
summary(lmer(MEAN ~ DATE + (1|uniqueID) + (1|DATE), data = subset(ndvi_data, leaf == "leafless" & new_GRtype == "mix_grass_tree"))) 


###### COV NDVI group by vegetation type #######
# change order of display
ndvi_data$new_GRtype <- factor(ndvi_data$new_GRtype, levels = c("grass", "sedum_mat", "woody_plants","mix_grass_tree"))

# plot for growing seasons
CV_ndvi_vege_type_leafy.plot <- ggplot(ndvi_data[ndvi_data$leaf == "leafy",], aes(x=GR_age_of_year,y=COV, color = new_GRtype, linetype = new_GRtype))+
  geom_point(aes(shape = new_GRtype), size=3, alpha = 0.3) +
  geom_smooth(aes(color = new_GRtype, fill=new_GRtype, linetype = new_GRtype),
              method='lm', formula= y~x, alpha = 0.2, cex=1.3, se = T)+
  labs(x="Green roof age (year)", y="CV of NDVI",
       group = "new_GRtype", linetype = "new_GRtype", pch = "new_GRtype")+
  scale_color_manual(name="Vegetation type",labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"),values = c("#00BFFF", "#FF762E", "#7CAE00","#cd6ee8"))+
  scale_fill_manual(name="Vegetation type",labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"),values = c("#00BFFF", "#FF762E", "#7CAE00","#cd6ee8"))+
  scale_linetype_manual(name="Vegetation type", labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"), values = c(2,2, 2, 2))+
  scale_shape_manual(name="Vegetation type", labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"), values = c(19,17, 15, 18))+
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8))+
  theme_bw(base_size = 28)+
  theme(
    legend.position = "top",
    legend.box = "vertical",
    legend.title = element_text(size=36),
    legend.text = element_text(size = 32),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.text=element_text(size=28),
    axis.title=element_text(size=32)
  ) +
  guides(
    color = guide_legend(nrow = 2, byrow = TRUE),
    linetype = guide_legend(nrow = 2, byrow = TRUE)
  )

CV_ndvi_vege_type_leafy.plot


# change order of display
ndvi_data$new_GRtype <- factor(ndvi_data$new_GRtype, levels = c("grass", "sedum_mat", "woody_plants","mix_grass_tree"))

# plot for pre-growing seasons
CV_ndvi_vege_type_leafless.plot <- ggplot(ndvi_data[ndvi_data$leaf == "leafless",], aes(x=GR_age_of_year,y=COV, color = new_GRtype, linetype = new_GRtype))+
  geom_point(aes(shape = new_GRtype), size=3, alpha = 0.3) +
  geom_smooth(aes(color = new_GRtype, fill=new_GRtype, linetype = new_GRtype),
              method='lm', formula= y~x, alpha = 0.2, cex=1.3, se = T)+
  labs(x="Green roof age (year)", y="CV of NDVI",
       group = "new_GRtype", linetype = "new_GRtype", pch = "new_GRtype")+
  scale_color_manual(name="Vegetation type",labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"),values = c("#00BFFF", "#FF762E", "#7CAE00","#cd6ee8"))+
  scale_fill_manual(name="Vegetation type",labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"),values = c("#00BFFF", "#FF762E", "#7CAE00","#cd6ee8"))+
  scale_linetype_manual(name="Vegetation type", labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"), values = c(2,2, 1, 2))+
  scale_shape_manual(name="Vegetation type", labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"), values = c(19,17, 15, 18))+
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8))+
  theme_bw(base_size = 28)+
  theme(
    legend.position = "top",
    legend.box = "vertical",
    legend.title = element_text(size=36),
    legend.text = element_text(size = 32),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.text=element_text(size=28),
    axis.title=element_text(size=32)
  ) +
  guides(
    color = guide_legend(nrow = 2, byrow = TRUE),
    linetype = guide_legend(nrow = 2, byrow = TRUE)
  )

CV_ndvi_vege_type_leafless.plot


### Statistics: linear mixed effect models ###
### Overall trends ###
# Include DATE and uniqueID as random factors
model_ndvi <- lmer(COV ~ GR_age_of_year + DATE + leaf + new_GRtype + (1|uniqueID) + (1|DATE), data = ndvi_data)
summary(model_ndvi)

# Model selection for the best predictors
selected_model_ndvi <- step(model_ndvi, direction = "both") # shows the steps: has AIC
print(selected_model_ndvi)

# Final model based on the selection
final_model_ndvi <- lmer(COV ~ GR_age_of_year + DATE + leaf + new_GRtype + (1 | uniqueID) + (1 | DATE), data = ndvi_data)
summary(final_model_ndvi)

### Trend grouped by seasons ###
## Growing seasons 
# COV NDVI vs. Green roof age 
summary(lmer(COV ~ GR_age_of_year + (1|uniqueID) + (1|DATE), data = subset(ndvi_data, leaf == "leafy"))) 
# COV NDVI vs. Date
summary(lmer(COV ~ DATE + (1|uniqueID) + (1|DATE), data = subset(ndvi_data, leaf == "leafy"))) 

## Grouped by vegetation type 
# COV NDVI vs. Green roof age 
summary(lmer(COV ~ GR_age_of_year + (1|uniqueID) + (1|DATE), data = subset(ndvi_data, leaf == "leafy" & new_GRtype == "grass"))) 
summary(lmer(COV ~ GR_age_of_year + (1|uniqueID) + (1|DATE), data = subset(ndvi_data, leaf == "leafy" & new_GRtype == "sedum_mat"))) 
summary(lmer(COV ~ GR_age_of_year + (1|uniqueID) + (1|DATE), data = subset(ndvi_data, leaf == "leafy" & new_GRtype == "woody_plants"))) 
summary(lmer(COV ~ GR_age_of_year + (1|uniqueID) + (1|DATE), data = subset(ndvi_data, leaf == "leafy" & new_GRtype == "mix_grass_tree"))) 

# COV NDVI vs. Date
summary(lmer(COV ~ DATE + (1|uniqueID) + (1|DATE), data = subset(ndvi_data, leaf == "leafy" & new_GRtype == "grass"))) 
summary(lmer(COV ~ DATE + (1|uniqueID) + (1|DATE), data = subset(ndvi_data, leaf == "leafy" & new_GRtype == "sedum_mat"))) 
summary(lmer(COV ~ DATE + (1|uniqueID) + (1|DATE), data = subset(ndvi_data, leaf == "leafy" & new_GRtype == "woody_plants"))) 
summary(lmer(COV ~ DATE + (1|uniqueID) + (1|DATE), data = subset(ndvi_data, leaf == "leafy" & new_GRtype == "mix_grass_tree"))) 


## Pre-growing seasons 
# COV NDVI vs. Green roof age 
summary(lmer(COV ~ GR_age_of_year + (1|uniqueID) + (1|DATE), data = subset(ndvi_data, leaf == "leafless"))) 
# COV NDVI vs. Date
summary(lmer(COV ~ DATE + (1|uniqueID) + (1|DATE), data = subset(ndvi_data, leaf == "leafless"))) 

## Grouped by vegetation type 
# COV NDVI vs. Green roof age 
summary(lmer(COV ~ GR_age_of_year + (1|uniqueID) + (1|DATE), data = subset(ndvi_data, leaf == "leafless" & new_GRtype == "grass"))) 
summary(lmer(COV ~ GR_age_of_year + (1|uniqueID) + (1|DATE), data = subset(ndvi_data, leaf == "leafless" & new_GRtype == "sedum_mat"))) 
summary(lmer(COV ~ GR_age_of_year + (1|uniqueID) + (1|DATE), data = subset(ndvi_data, leaf == "leafless" & new_GRtype == "woody_plants"))) 
summary(lmer(COV ~ GR_age_of_year + (1|uniqueID) + (1|DATE), data = subset(ndvi_data, leaf == "leafless" & new_GRtype == "mix_grass_tree"))) 

# COV NDVI vs. Date
summary(lmer(COV ~ DATE + (1|uniqueID) + (1|DATE), data = subset(ndvi_data, leaf == "leafless" & new_GRtype == "grass"))) 
summary(lmer(COV ~ DATE + (1|uniqueID) + (1|DATE), data = subset(ndvi_data, leaf == "leafless" & new_GRtype == "sedum_mat"))) 
summary(lmer(COV ~ DATE + (1|uniqueID) + (1|DATE), data = subset(ndvi_data, leaf == "leafless" & new_GRtype == "woody_plants"))) 
summary(lmer(COV ~ DATE + (1|uniqueID) + (1|DATE), data = subset(ndvi_data, leaf == "leafless" & new_GRtype == "mix_grass_tree"))) 


###### Combine SI Figure S4: a. Mean NDVI leaf-on; b. Mean NDVI leaf-off; c. CV NDVI leaf-on; d. CV NDVI leaf-off ######
merge_NDVI_vege_type = mean_ndvi_vege_type_leafy.plot + mean_ndvi_vege_type_leafless.plot + 
  CV_ndvi_vege_type_leafy.plot + CV_ndvi_vege_type_leafless.plot +
  plot_layout(guides = "collect", ncol = 2) & theme(legend.position = "top")

merge_NDVI_vege_type[[1]] = merge_NDVI_vege_type[[1]] + theme(axis.text.x = element_blank(),axis.title.x = element_blank(),legend.position = "none")
merge_NDVI_vege_type[[2]] = merge_NDVI_vege_type[[2]] + theme(axis.text.x = element_blank(),axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position = "none")
merge_NDVI_vege_type[[3]] = merge_NDVI_vege_type[[3]] + theme(legend.position = "none")
merge_NDVI_vege_type[[4]] = merge_NDVI_vege_type[[4]] + theme(axis.title.y = element_blank())

merge_NDVI_vege_type

# ggsave(plot=merge_NDVI_vege_type, filename = "path/Fig_NDVI_Age_Vegetation_type.tif", width = 14, height = 10, device='tiff', dpi=600,unit="in")





# Supplementary Figures for other vegetation indices: SAVI, GNDVI, and CIg --------
### Figure S2: Mean VIs and COV VIs #####
###### Mean SAVI #######
# Change order of display
savi_data$leaf <- factor(savi_data$leaf, levels = c("leafy", "leafless"))

# Plot
mean_savi.plot <- ggplot(savi_data, aes(x=GR_age_of_year,y=MEAN, color = leaf, linetype = leaf))+
  geom_point(aes(shape = leaf), size=3.5, alpha = 0.3) +
  geom_smooth(aes(color = leaf, fill=leaf, linetype = leaf),
              method='lm', formula= y~x, alpha = 0.3, cex=1.3)+
  labs(x="Green roof age (year)", y="Mean SAVI",
       group = "Seasonality", linetype = "Seasonality", pch = "Seasonality")+
  scale_color_manual(name="Seasonality",labels = c("Growing season", "Pre-growing season"),values = c("#28BD3F","#C67B39"))+
  scale_fill_manual(name="Seasonality",labels = c("Growing season", "Pre-growing season"),values = c("#28BD3F","#C67B39"))+
  scale_linetype_manual(name="Seasonality", labels = c("Growing season", "Pre-growing season"), values = c(1,1))+
  scale_shape_manual(name="Seasonality", labels = c("Growing season", "Pre-growing season"), values = c(17,19))+
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8))+
  theme_bw(base_size = 28)+
  theme(
    legend.position = "top",
    legend.title = element_text(size=36),
    legend.text = element_text(size = 32),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.text=element_text(size=28),
    axis.title=element_text(size=32)
  )

mean_savi.plot


### Statistics: linear mixed effect models ###
### Overall trends ###
# Include DATE and uniqueID as random factors
model_savi <- lmer(MEAN ~ GR_age_of_year + DATE + leaf + (1|uniqueID) + (1|DATE), data = savi_data)
summary(model_savi)

# Model selection for the best predictors
selected_model_savi <- step(model_savi, direction = "both") # shows the steps: has AIC
print(selected_model_savi)

# Final model based on the selection
final_model_savi <- lmer(MEAN ~ leaf + (1|uniqueID) + (1|DATE), data = savi_data)
summary(final_model_savi)

### Trend grouped by seasons ###
## Growing seasons 
# Mean SAVI vs. Green roof age 
summary(lmer(MEAN ~ GR_age_of_year + (1|uniqueID) + (1|DATE), data = subset(savi_data, leaf == "leafy"))) 

# Mean SAVI vs. Date 
summary(lmer(MEAN ~ DATE + (1|uniqueID) + (1|DATE), data = subset(savi_data, leaf == "leafy"))) 

## Pre-growing seasons 
# Mean SAVI vs. Green roof age 
summary(lmer(MEAN ~ GR_age_of_year + (1|uniqueID) + (1|DATE), data = subset(savi_data, leaf == "leafless"))) 

# Mean SAVI vs. Date 
summary(lmer(MEAN ~ DATE + (1|uniqueID) + (1|DATE), data = subset(savi_data, leaf == "leafless"))) 


###### Mean GNDVI #######
# Change order of display
gndvi_data$leaf <- factor(gndvi_data$leaf, levels = c("leafy", "leafless"))

# Plot
mean_gndvi.plot <- ggplot(gndvi_data, aes(x=GR_age_of_year,y=MEAN, color = leaf, linetype = leaf))+
  geom_point(aes(shape = leaf), size=3.5, alpha = 0.3) +
  geom_smooth(aes(color = leaf, fill=leaf, linetype = leaf),
              method='lm', formula= y~x, alpha = 0.3, cex=1.3)+
  labs(x="Green roof age (year)", y="Mean GNDVI",
       group = "Seasonality", linetype = "Seasonality", pch = "Seasonality")+
  scale_color_manual(name="Seasonality",labels = c("Growing season", "Pre-growing season"),values = c("#28BD3F","#C67B39"))+
  scale_fill_manual(name="Seasonality",labels = c("Growing season", "Pre-growing season"),values = c("#28BD3F","#C67B39"))+
  scale_linetype_manual(name="Seasonality", labels = c("Growing season", "Pre-growing season"), values = c(1,1))+
  scale_shape_manual(name="Seasonality", labels = c("Growing season", "Pre-growing season"), values = c(17,19))+
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8))+
  theme_bw(base_size = 28)+
  theme(
    legend.position = "top",
    legend.title = element_text(size=36),
    legend.text = element_text(size = 32),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.text=element_text(size=28),
    axis.title=element_text(size=32)
  )

mean_gndvi.plot


### Statistics: linear mixed effect models ###
### Overall trends ###
# Include DATE and uniqueID as random factors
model_gndvi <- lmer(MEAN ~ GR_age_of_year + DATE + leaf + (1|uniqueID) + (1|DATE), data = gndvi_data)
summary(model_gndvi)

# Model selection for the best predictors
selected_model_gndvi <- step(model_gndvi, direction = "both") # shows the steps: has AIC
print(selected_model_gndvi)

# Final model based on the selection
final_model_gndvi <- lmer(MEAN ~ DATE + (1|uniqueID) + (1|DATE), data = gndvi_data)
summary(final_model_gndvi)

### Trend grouped by seasons ###
## Growing seasons 
# Mean GNDVI vs. Green roof age 
summary(lmer(MEAN ~ GR_age_of_year + (1|uniqueID) + (1|DATE), data = subset(gndvi_data, leaf == "leafy"))) 

# Mean GNDVI vs. Date 
summary(lmer(MEAN ~ DATE + (1|uniqueID) + (1|DATE), data = subset(gndvi_data, leaf == "leafy"))) 

## Pre-growing seasons 
# Mean GNDVI vs. Green roof age 
summary(lmer(MEAN ~ GR_age_of_year + (1|uniqueID) + (1|DATE), data = subset(gndvi_data, leaf == "leafless"))) 

# Mean GNDVI vs. Date 
summary(lmer(MEAN ~ DATE + (1|uniqueID) + (1|DATE), data = subset(gndvi_data, leaf == "leafless"))) 


###### Mean CIg #######
# Change order of display
cig_data$leaf <- factor(cig_data$leaf, levels = c("leafy", "leafless"))

# Plot
mean_cig.plot <- ggplot(cig_data, aes(x=GR_age_of_year,y=MEAN, color = leaf, linetype = leaf))+
  geom_point(aes(shape = leaf), size=3.5, alpha = 0.3) +
  geom_smooth(aes(color = leaf, fill=leaf, linetype = leaf),
              method='lm', formula= y~x, alpha = 0.3, cex=1.3)+
  labs(x="Green roof age (year)", y="Mean CIg",
       group = "Seasonality", linetype = "Seasonality", pch = "Seasonality")+
  scale_color_manual(name="Seasonality",labels = c("Growing season", "Pre-growing season"),values = c("#28BD3F","#C67B39"))+
  scale_fill_manual(name="Seasonality",labels = c("Growing season", "Pre-growing season"),values = c("#28BD3F","#C67B39"))+
  scale_linetype_manual(name="Seasonality", labels = c("Growing season", "Pre-growing season"), values = c(1,1))+
  scale_shape_manual(name="Seasonality", labels = c("Growing season", "Pre-growing season"), values = c(17,19))+
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8))+
  theme_bw(base_size = 28)+
  theme(
    legend.position = "top",
    legend.title = element_text(size=36),
    legend.text = element_text(size = 32),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.text=element_text(size=28),
    axis.title=element_text(size=32)
  )

mean_cig.plot


### Statistics: linear mixed effect models ###
### Overall trends ###
# Include DATE and uniqueID as random factors
model_cig <- lmer(MEAN ~ GR_age_of_year + DATE + leaf + (1|uniqueID) + (1|DATE), data = cig_data)
summary(model_cig)

# Model selection for the best predictors
selected_model_cig <- step(model_cig, direction = "both") # shows the steps: has AIC
print(selected_model_cig)

# Final model based on the selection
final_model_cig <- lmer(MEAN ~ (1|uniqueID) + (1|DATE), data = cig_data)
summary(final_model_cig)

### Trend grouped by seasons ###
## Growing seasons 
# Mean CIg vs. Green roof age 
summary(lmer(MEAN ~ GR_age_of_year + (1|uniqueID) + (1|DATE), data = subset(cig_data, leaf == "leafy"))) 

# Mean CIg vs. Date 
summary(lmer(MEAN ~ DATE + (1|uniqueID) + (1|DATE), data = subset(cig_data, leaf == "leafy"))) 

## Pre-growing seasons 
# Mean CIg vs. Green roof age 
summary(lmer(MEAN ~ GR_age_of_year + (1|uniqueID) + (1|DATE), data = subset(cig_data, leaf == "leafless"))) 

# Mean CIg vs. Date 
summary(lmer(MEAN ~ DATE + (1|uniqueID) + (1|DATE), data = subset(cig_data, leaf == "leafless"))) 


###### COV SAVI #######
# Change order of display
savi_data$leaf <- factor(savi_data$leaf, levels = c("leafy", "leafless"))

# Plot
CV_savi.plot <- ggplot(savi_data, aes(x=GR_age_of_year,y=COV, color = leaf, linetype = leaf))+
  geom_point(aes(shape = leaf), size=3.5, alpha = 0.3) +
  geom_smooth(aes(color = leaf, fill=leaf, linetype = leaf),
              method='lm', formula= y~x, alpha = 0.3, cex=1.3)+
  labs(x="Green roof age (year)", y="CV of SAVI",
       group = "Seasonality", linetype = "Seasonality", pch = "Seasonality")+
  scale_color_manual(name="Seasonality",labels = c("Growing season", "Pre-growing season"),values = c("#28BD3F","#C67B39"))+
  scale_fill_manual(name="Seasonality",labels = c("Growing season", "Pre-growing season"),values = c("#28BD3F","#C67B39"))+
  scale_linetype_manual(name="Seasonality", labels = c("Growing season", "Pre-growing season"), values = c(1,1))+
  scale_shape_manual(name="Seasonality", labels = c("Growing season", "Pre-growing season"), values = c(17,19))+
  scale_x_continuous(breaks = c(1,2,3,4,5, 6,7,8))+
  theme_bw(base_size = 28)+
  theme(
    legend.position = "top",
    legend.title = element_text(size=36),
    legend.text = element_text(size = 32),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.text=element_text(size=28),
    axis.title=element_text(size=32)
  )

CV_savi.plot


### Statistics: linear mixed effect models ###
### Overall trends ###
# Include DATE and uniqueID as random factors
model_savi <- lmer(COV ~ GR_age_of_year + DATE + leaf + (1|uniqueID) + (1|DATE), data = savi_data)
summary(model_savi)
step(model_savi, direction = "both")

# Final model based on the selection
summary(lmer(COV ~ GR_age_of_year + leaf + (1|uniqueID) + (1|DATE), data = savi_data)) 

### Trend grouped by seasons ###
## Growing seasons 
# COV SAVI vs. Green roof age 
summary(lmer(COV ~ GR_age_of_year + (1|uniqueID) + (1|DATE), data = subset(savi_data, leaf == "leafy"))) 

# COV SAVI vs. Date 
summary(lmer(COV ~ DATE + (1|uniqueID) + (1|DATE), data = subset(savi_data, leaf == "leafy"))) 

## Pre-growing seasons 
# COV SAVI vs. Green roof age 
summary(lmer(COV ~ GR_age_of_year + (1|uniqueID) + (1|DATE), data = subset(savi_data, leaf == "leafless"))) 

# COV SAVI vs. Date 
summary(lmer(COV ~ DATE + (1|uniqueID) + (1|DATE), data = subset(savi_data, leaf == "leafless"))) 


###### COV GNDVI #######
# Change order of display
gndvi_data$leaf <- factor(gndvi_data$leaf, levels = c("leafy", "leafless"))

# Plot
CV_gndvi.plot <- ggplot(gndvi_data, aes(x=GR_age_of_year,y=COV, color = leaf, linetype = leaf))+
  geom_point(aes(shape = leaf), size=3.5, alpha = 0.3) +
  geom_smooth(aes(color = leaf, fill=leaf, linetype = leaf),
              method='lm', formula= y~x, alpha = 0.3, cex=1.3)+
  labs(x="Green roof age (year)", y="CV of GNDVI",
       group = "Seasonality", linetype = "Seasonality", pch = "Seasonality")+
  scale_color_manual(name="Seasonality",labels = c("Growing season", "Pre-growing season"),values = c("#28BD3F","#C67B39"))+
  scale_fill_manual(name="Seasonality",labels = c("Growing season", "Pre-growing season"),values = c("#28BD3F","#C67B39"))+
  scale_linetype_manual(name="Seasonality", labels = c("Growing season", "Pre-growing season"), values = c(1,1))+
  scale_shape_manual(name="Seasonality", labels = c("Growing season", "Pre-growing season"), values = c(17,19))+
  scale_x_continuous(breaks = c(1,2,3,4,5, 6,7,8))+
  theme_bw(base_size = 28)+
  theme(
    legend.position = "top",
    legend.title = element_text(size=36),
    legend.text = element_text(size = 32),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.text=element_text(size=28),
    axis.title=element_text(size=32)
  )

CV_gndvi.plot


### Statistics: linear mixed effect models ###
### Overall trends ###
# Include DATE and uniqueID as random factors
model_gndvi <- lmer(COV ~ GR_age_of_year + DATE + leaf + (1|uniqueID) + (1|DATE), data = gndvi_data)
summary(model_gndvi)
step(model_gndvi, direction = "both")

# Final model based on the selection
summary(lmer(COV ~ GR_age_of_year + (1|uniqueID) + (1|DATE), data = gndvi_data)) 

### Trend grouped by seasons ###
## Growing seasons 
# COV GNDVI vs. Green roof age 
summary(lmer(COV ~ GR_age_of_year + (1|uniqueID) + (1|DATE), data = subset(gndvi_data, leaf == "leafy"))) 

# COV GNDVI vs. Date 
summary(lmer(COV ~ DATE + (1|uniqueID) + (1|DATE), data = subset(gndvi_data, leaf == "leafy"))) 

## Pre-growing seasons
# COV GNDVI vs. Green roof age 
summary(lmer(COV ~ GR_age_of_year + (1|uniqueID) + (1|DATE), data = subset(gndvi_data, leaf == "leafless"))) 

# COV GNDVI vs. Date 
summary(lmer(COV ~ DATE + (1|uniqueID) + (1|DATE), data = subset(gndvi_data, leaf == "leafless"))) 


###### COV CIg #######
# Change order of display
cig_data$leaf <- factor(cig_data$leaf, levels = c("leafy", "leafless"))

# Plot
CV_cig.plot <- ggplot(cig_data, aes(x=GR_age_of_year,y=COV, color = leaf, linetype = leaf))+
  geom_point(aes(shape = leaf), size=3.5, alpha = 0.3) +
  geom_smooth(aes(color = leaf, fill=leaf, linetype = leaf),
              method='lm', formula= y~x, alpha = 0.3, cex=1.3)+
  labs(x="Green roof age (year)", y="CV of CIg",
       group = "Seasonality", linetype = "Seasonality", pch = "Seasonality")+
  scale_color_manual(name="Seasonality",labels = c("Growing season", "Pre-growing season"),values = c("#28BD3F","#C67B39"))+
  scale_fill_manual(name="Seasonality",labels = c("Growing season", "Pre-growing season"),values = c("#28BD3F","#C67B39"))+
  scale_linetype_manual(name="Seasonality", labels = c("Growing season", "Pre-growing season"), values = c(1,1))+
  scale_shape_manual(name="Seasonality", labels = c("Growing season", "Pre-growing season"), values = c(17,19))+
  scale_x_continuous(breaks = c(1,2,3,4,5, 6,7,8))+
  theme_bw(base_size = 28)+
  theme(
    legend.position = "top",
    legend.title = element_text(size=36),
    legend.text = element_text(size = 32),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.text=element_text(size=28),
    axis.title=element_text(size=32)
  )

CV_cig.plot


### Statistics: linear mixed effect models ###
### Overall trends ###
# Include DATE and uniqueID as random factors
model_cig <- lmer(COV ~ GR_age_of_year + DATE + leaf + (1|uniqueID) + (1|DATE), data = cig_data)
summary(model_cig)
step(model_cig, direction = "both")

# Final model based on the selection
summary(lmer(COV ~ GR_age_of_year + (1|uniqueID) + (1|DATE), data = cig_data)) 

### Trend grouped by seasons ###
## Growing seasons 
# COV CIg vs. Green roof age
summary(lmer(COV ~ GR_age_of_year + (1|uniqueID) + (1|DATE), data = subset(cig_data, leaf == "leafy"))) 

# COV CIg vs. Date 
summary(lmer(COV ~ DATE + (1|uniqueID) + (1|DATE), data = subset(cig_data, leaf == "leafy"))) 

## Pre-growing seasons 
# COV CIg vs. Green roof age
summary(lmer(COV ~ GR_age_of_year + (1|uniqueID) + (1|DATE), data = subset(cig_data, leaf == "leafless"))) 

# COV CIg vs. Date 
summary(lmer(COV ~ DATE + (1|uniqueID) + (1|DATE), data = subset(cig_data, leaf == "leafless"))) 


###### Combine SI Figure S2: Mean VIs and COV VIs ######
merge_VIs_overall = mean_savi.plot + CV_savi.plot +
  mean_gndvi.plot + CV_gndvi.plot +
  mean_cig.plot + CV_cig.plot +
  plot_layout(guides = "collect", ncol = 2) & theme(legend.position = "top")

merge_VIs_overall[[1]] = merge_VIs_overall[[1]] + theme(axis.text.x = element_blank(),axis.title.x = element_blank())
merge_VIs_overall[[2]] = merge_VIs_overall[[2]] + theme(axis.text.x = element_blank(),axis.title.x = element_blank())
merge_VIs_overall[[3]] = merge_VIs_overall[[3]] + theme(axis.text.x = element_blank(),axis.title.x = element_blank())
merge_VIs_overall[[4]] = merge_VIs_overall[[4]] + theme(axis.text.x = element_blank(),axis.title.x = element_blank())

merge_VIs_overall

# ggsave(plot=merge_VIs_overall, filename = "path/Fig_VIs_overall.tif", width = 14, height = 13, device='tiff', dpi=600,unit="in")


#### SI Figure S3: Grouped by green roof type (intensive vs. extensive green roofs) #####
###### Mean SAVI grouped by green roof type (intensive vs. extensive green roofs) #######
# Change order of display
savi_data$GR_type <- factor(savi_data$GR_type, levels = c("ext", "int"))

# Plot for growing seasons 
mean_savi_GRtype_leafy.plot <- ggplot(savi_data[savi_data$leaf == "leafy",], aes(x=GR_age_of_year,y=MEAN, color = GR_type, linetype = GR_type))+
  geom_point(aes(shape = GR_type), size=3, alpha = 0.3) +
  geom_smooth(aes(color = GR_type, fill=GR_type, linetype = GR_type),
              method='lm', formula= y~x, alpha = 0.2, cex=1.3, se = T)+
  labs(x="Green roof age (year)", y="Mean SAVI",
       group = "GR_type", linetype = "GR_type", pch = "GR_type")+
  scale_color_manual(name="Green roof type",labels = c("Extensive","Intensive"),values = c("#FC7307","#236AB9"))+
  scale_fill_manual(name="Green roof type",labels = c("Extensive","Intensive"),values = c("#FC7307","#236AB9"))+
  scale_linetype_manual(name="Green roof type", labels = c("Extensive","Intensive"), values = c(1,1))+
  scale_shape_manual(name="Green roof type", labels = c("Extensive","Intensive"), values = c(19,17))+
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8))+
  theme_bw(base_size = 28)+
  theme(
    legend.position = "top",
    legend.box = "vertical",
    legend.title = element_text(size=36),
    legend.text = element_text(size = 32),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.text=element_text(size=28),
    axis.title=element_text(size=32)
  ) 

mean_savi_GRtype_leafy.plot


# change order of display
savi_data$GR_type <- factor(savi_data$GR_type, levels = c("ext", "int"))

# plot for pre-growing seasons 
mean_savi_GRtype_leafless.plot <- ggplot(savi_data[savi_data$leaf == "leafless",], aes(x=GR_age_of_year,y=MEAN, color = GR_type, linetype = GR_type))+
  geom_point(aes(shape = GR_type), size=3, alpha = 0.3) +
  geom_smooth(aes(color = GR_type, fill=GR_type, linetype = GR_type),
              method='lm', formula= y~x, alpha = 0.2, cex=1.3, se = T)+
  labs(x="Green roof age (year)", y="Mean SAVI",
       group = "GR_type", linetype = "GR_type", pch = "GR_type")+
  scale_color_manual(name="Green roof type",labels = c("Extensive","Intensive"),values = c("#FC7307","#236AB9"))+
  scale_fill_manual(name="Green roof type",labels = c("Extensive","Intensive"),values = c("#FC7307","#236AB9"))+
  scale_linetype_manual(name="Green roof type", labels = c("Extensive","Intensive"), values = c(1,2))+
  scale_shape_manual(name="Green roof type", labels = c("Extensive","Intensive"), values = c(19,17))+
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8))+
  theme_bw(base_size = 28)+
  theme(
    legend.position = "top",
    legend.box = "vertical",
    legend.title = element_text(size=36),
    legend.text = element_text(size = 32),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.text=element_text(size=28),
    axis.title=element_text(size=32)
  ) 

mean_savi_GRtype_leafless.plot


### Statistics: linear mixed effect models ###
### Overall trends ###
# Include DATE and uniqueID as random factors
model_savi <- lmer(MEAN ~ GR_age_of_year + DATE + leaf + GR_type + (1|uniqueID) + (1|DATE), data = savi_data)
summary(model_savi)

# Model selection for the best predictors
selected_model_savi <- step(model_savi, direction = "both") # shows the steps: has AIC
print(selected_model_savi)

# Final model based on the selection
final_model_savi <- lmer(MEAN ~ GR_age_of_year + leaf + GR_type + (1 | uniqueID) + (1 | DATE), data = savi_data)
summary(final_model_savi)

### Trend grouped by seasons ###
## Growing seasons 
# Mean SAVI vs. Green roof age 
summary(lmer(MEAN ~ GR_age_of_year + (1|uniqueID) + (1|DATE), data = subset(savi_data, leaf == "leafy"))) 
# Mean SAVI vs. Date
summary(lmer(MEAN ~ DATE + (1|uniqueID) + (1|DATE), data = subset(savi_data, leaf == "leafy"))) 

## Grouped by green roof type 
# Mean SAVI vs. Green roof age 
# Extensive green roofs
summary(lmer(MEAN ~ GR_age_of_year + (1|uniqueID) + (1|DATE), data = subset(savi_data, leaf == "leafy" & GR_type == "ext"))) 
# Intensive green roofs
summary(lmer(MEAN ~ GR_age_of_year + (1|uniqueID) + (1|DATE), data = subset(savi_data, leaf == "leafy" & GR_type == "int"))) 

# Mean SAVI vs. Date
summary(lmer(MEAN ~ DATE + (1|uniqueID) + (1|DATE), data = subset(savi_data, leaf == "leafy" & GR_type == "ext"))) 
summary(lmer(MEAN ~ DATE + (1|uniqueID) + (1|DATE), data = subset(savi_data, leaf == "leafy" & GR_type == "int"))) 


## Pre-growing seasons 
# Mean SAVI vs. Green roof age 
summary(lmer(MEAN ~ GR_age_of_year + (1|uniqueID) + (1|DATE), data = subset(ndvi_data, leaf == "leafless"))) 
# Mean SAVI vs. Date
summary(lmer(MEAN ~ DATE + (1|uniqueID) + (1|DATE), data = subset(ndvi_data, leaf == "leafless"))) 

### Grouped by green roof type ###
# Mean SAVI vs. Green roof age 
# Extensive green roofs
summary(lmer(MEAN ~ GR_age_of_year + (1|uniqueID) + (1|DATE), data = subset(savi_data, leaf == "leafless" & GR_type == "ext"))) 
# Intensive green roofs
summary(lmer(MEAN ~ GR_age_of_year + (1|uniqueID) + (1|DATE), data = subset(savi_data, leaf == "leafless" & GR_type == "int"))) 

# Mean SAVI vs. Date
# Extensive green roofs
summary(lmer(MEAN ~ DATE + (1|uniqueID) + (1|DATE), data = subset(savi_data, leaf == "leafless" & GR_type == "ext"))) 
summary(lmer(MEAN ~ DATE + (1|uniqueID) + (1|DATE), data = subset(savi_data, leaf == "leafless" & GR_type == "int"))) 


###### Mean GNDVI group by green roof type (intensive vs. extensive green roofs) #######
# Change order of display
gndvi_data$GR_type <- factor(gndvi_data$GR_type, levels = c("ext", "int"))

# Plot for growing seasons 
mean_gndvi_GRtype_leafy.plot <- ggplot(gndvi_data[gndvi_data$leaf == "leafy",], aes(x=GR_age_of_year,y=MEAN, color = GR_type, linetype = GR_type))+
  geom_point(aes(shape = GR_type), size=3, alpha = 0.3) +
  geom_smooth(aes(color = GR_type, fill=GR_type, linetype = GR_type),
              method='lm', formula= y~x, alpha = 0.2, cex=1.3, se = T)+
  labs(x="Green roof age (year)", y="Mean GNDVI",
       group = "GR_type", linetype = "GR_type", pch = "GR_type")+
  scale_color_manual(name="Green roof type",labels = c("Extensive","Intensive"),values = c("#FC7307","#236AB9"))+
  scale_fill_manual(name="Green roof type",labels = c("Extensive","Intensive"),values = c("#FC7307","#236AB9"))+
  scale_linetype_manual(name="Green roof type", labels = c("Extensive","Intensive"), values = c(1,1))+
  scale_shape_manual(name="Green roof type", labels = c("Extensive","Intensive"), values = c(19,17))+
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8))+
  theme_bw(base_size = 28)+
  theme(
    legend.position = "top",
    legend.box = "vertical",
    legend.title = element_text(size=36),
    legend.text = element_text(size = 32),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.text=element_text(size=28),
    axis.title=element_text(size=32)
  ) 

mean_gndvi_GRtype_leafy.plot


# change order of display
gndvi_data$GR_type <- factor(gndvi_data$GR_type, levels = c("ext", "int"))

# Plot for pre-growing seasons 
mean_gndvi_GRtype_leafless.plot <- ggplot(gndvi_data[gndvi_data$leaf == "leafless",], aes(x=GR_age_of_year,y=MEAN, color = GR_type, linetype = GR_type))+
  geom_point(aes(shape = GR_type), size=3, alpha = 0.3) +
  geom_smooth(aes(color = GR_type, fill=GR_type, linetype = GR_type),
              method='lm', formula= y~x, alpha = 0.2, cex=1.3, se = T)+
  labs(x="Green roof age (year)", y="Mean SAVI",
       group = "GR_type", linetype = "GR_type", pch = "GR_type")+
  scale_color_manual(name="Green roof type",labels = c("Extensive","Intensive"),values = c("#FC7307","#236AB9"))+
  scale_fill_manual(name="Green roof type",labels = c("Extensive","Intensive"),values = c("#FC7307","#236AB9"))+
  scale_linetype_manual(name="Green roof type", labels = c("Extensive","Intensive"), values = c(1,1))+
  scale_shape_manual(name="Green roof type", labels = c("Extensive","Intensive"), values = c(19,17))+
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8))+
  theme_bw(base_size = 28)+
  theme(
    legend.position = "top",
    legend.box = "vertical",
    legend.title = element_text(size=36),
    legend.text = element_text(size = 32),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.text=element_text(size=28),
    axis.title=element_text(size=32)
  ) 

mean_gndvi_GRtype_leafless.plot


### Statistics: linear mixed effect models ###
### Overall trends ###
# Include DATE and uniqueID as random factors
model_gndvi <- lmer(MEAN ~ GR_age_of_year + DATE + leaf + GR_type + (1|uniqueID) + (1|DATE), data = gndvi_data)
summary(model_gndvi)

# Model selection for the best predictors
selected_model_gndvi <- step(model_gndvi, direction = "both") # shows the steps: has AIC
print(selected_model_gndvi)

# Final model based on the selection
final_model_gndvi <- lmer(MEAN ~ GR_age_of_year + GR_type + (1 | uniqueID) + (1 | DATE), data = gndvi_data)
summary(final_model_gndvi)

### Trend grouped by seasons ###
## Growing seasons 
# Mean GNDVI vs. Green roof age 
summary(lmer(MEAN ~ GR_age_of_year + (1|uniqueID) + (1|DATE), data = subset(gndvi_data, leaf == "leafy"))) 
# Mean GNDVI vs. Date
summary(lmer(MEAN ~ DATE + (1|uniqueID) + (1|DATE), data = subset(gndvi_data, leaf == "leafy"))) 

## Grouped by green roof type 
# Mean GNDVI vs. Green roof age 
# Extensive green roofs
summary(lmer(MEAN ~ GR_age_of_year + (1|uniqueID) + (1|DATE), data = subset(gndvi_data, leaf == "leafy" & GR_type == "ext"))) 
# Intensive green roofs
summary(lmer(MEAN ~ GR_age_of_year + (1|uniqueID) + (1|DATE), data = subset(gndvi_data, leaf == "leafy" & GR_type == "int"))) 

# Mean GNDVI vs. Date
summary(lmer(MEAN ~ DATE + (1|uniqueID) + (1|DATE), data = subset(gndvi_data, leaf == "leafy" & GR_type == "ext"))) 
# Intensive green roofs
summary(lmer(MEAN ~ DATE + (1|uniqueID) + (1|DATE), data = subset(gndvi_data, leaf == "leafy" & GR_type == "int"))) 


## Pre-growing seasons 
# Mean GNDVI vs. Green roof age
summary(lmer(MEAN ~ GR_age_of_year + (1|uniqueID) + (1|DATE), data = subset(gndvi_data, leaf == "leafless"))) 
# Mean GNDVI vs. Date
summary(lmer(MEAN ~ DATE + (1|uniqueID) + (1|DATE), data = subset(gndvi_data, leaf == "leafless"))) 

### Grouped by green roof type ###
# Mean GNDVI vs. Green roof age 
# Extensive green roofs
summary(lmer(MEAN ~ GR_age_of_year + (1|uniqueID) + (1|DATE), data = subset(gndvi_data, leaf == "leafless" & GR_type == "ext"))) 
# Intensive green roofs
summary(lmer(MEAN ~ GR_age_of_year + (1|uniqueID) + (1|DATE), data = subset(gndvi_data, leaf == "leafless" & GR_type == "int"))) 

# Mean GNDVI vs. Date
# Extensive green roofs
summary(lmer(MEAN ~ DATE + (1|uniqueID) + (1|DATE), data = subset(gndvi_data, leaf == "leafless" & GR_type == "ext"))) 
# Intensive green roofs
summary(lmer(MEAN ~ DATE + (1|uniqueID) + (1|DATE), data = subset(gndvi_data, leaf == "leafless" & GR_type == "int"))) 


###### Mean CIg group by green roof type (intensive vs. extensive green roofs) #######
# Change order of display
cig_data$GR_type <- factor(cig_data$GR_type, levels = c("ext", "int"))

# Plot for growing seasons 
mean_cig_GRtype_leafy.plot <- ggplot(cig_data[cig_data$leaf == "leafy",], aes(x=GR_age_of_year,y=MEAN, color = GR_type, linetype = GR_type))+
  geom_point(aes(shape = GR_type), size=3, alpha = 0.3) +
  geom_smooth(aes(color = GR_type, fill=GR_type, linetype = GR_type),
              method='lm', formula= y~x, alpha = 0.2, cex=1.3, se = T)+
  labs(x="Green roof age (year)", y="Mean CIg",
       group = "GR_type", linetype = "GR_type", pch = "GR_type")+
  scale_color_manual(name="Green roof type",labels = c("Extensive","Intensive"),values = c("#FC7307","#236AB9"))+
  scale_fill_manual(name="Green roof type",labels = c("Extensive","Intensive"),values = c("#FC7307","#236AB9"))+
  scale_linetype_manual(name="Green roof type", labels = c("Extensive","Intensive"), values = c(1,1))+
  scale_shape_manual(name="Green roof type", labels = c("Extensive","Intensive"), values = c(19,17))+
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8))+
  theme_bw(base_size = 28)+
  theme(
    legend.position = "top",
    legend.box = "vertical",
    legend.title = element_text(size=36),
    legend.text = element_text(size = 32),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.text=element_text(size=28),
    axis.title=element_text(size=32)
  ) 

mean_cig_GRtype_leafy.plot


# change order of display
cig_data$GR_type <- factor(cig_data$GR_type, levels = c("ext", "int"))

# Plot for pre-growing seasons 
mean_cig_GRtype_leafless.plot <- ggplot(cig_data[cig_data$leaf == "leafless",], aes(x=GR_age_of_year,y=MEAN, color = GR_type, linetype = GR_type))+
  geom_point(aes(shape = GR_type), size=3, alpha = 0.3) +
  geom_smooth(aes(color = GR_type, fill=GR_type, linetype = GR_type),
              method='lm', formula= y~x, alpha = 0.2, cex=1.3, se = T)+
  labs(x="Green roof age (year)", y="Mean CIg",
       group = "GR_type", linetype = "GR_type", pch = "GR_type")+
  scale_color_manual(name="Green roof type",labels = c("Extensive","Intensive"),values = c("#FC7307","#236AB9"))+
  scale_fill_manual(name="Green roof type",labels = c("Extensive","Intensive"),values = c("#FC7307","#236AB9"))+
  scale_linetype_manual(name="Green roof type", labels = c("Extensive","Intensive"), values = c(1,2))+
  scale_shape_manual(name="Green roof type", labels = c("Extensive","Intensive"), values = c(19,17))+
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8))+
  theme_bw(base_size = 28)+
  theme(
    legend.position = "top",
    legend.box = "vertical",
    legend.title = element_text(size=36),
    legend.text = element_text(size = 32),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.text=element_text(size=28),
    axis.title=element_text(size=32)
  ) 

mean_cig_GRtype_leafless.plot


### Statistics: linear mixed effect models ###
### Overall trends ###
# Include DATE and uniqueID as random factors
model_cig <- lmer(MEAN ~ GR_age_of_year + DATE + leaf + GR_type + (1|uniqueID) + (1|DATE), data = cig_data)
summary(model_cig)

# Model selection for the best predictors
selected_model_cig <- step(model_cig, direction = "both") # shows the steps: has AIC
print(selected_model_cig)

# Final model based on the selection
final_model_cig <- lmer(MEAN ~ GR_type + (1 | uniqueID) + (1 | DATE), data = cig_data)
summary(final_model_cig)

### Trend grouped by seasons ###
## Growing seasons 
# Mean CIg vs. Green roof age
summary(lmer(MEAN ~ GR_age_of_year + (1|uniqueID) + (1|DATE), data = subset(cig_data, leaf == "leafy"))) 
# Mean CIg vs. Date
summary(lmer(MEAN ~ DATE + (1|uniqueID) + (1|DATE), data = subset(cig_data, leaf == "leafy"))) 

## Grouped by green roof type 
# Mean CIg vs. Green roof age 
# Extensive green roofs
summary(lmer(MEAN ~ GR_age_of_year + (1|uniqueID) + (1|DATE), data = subset(cig_data, leaf == "leafy" & GR_type == "ext"))) 
# Intensive green roofs
summary(lmer(MEAN ~ GR_age_of_year + (1|uniqueID) + (1|DATE), data = subset(cig_data, leaf == "leafy" & GR_type == "int"))) 

# Mean CIg vs. Date
summary(lmer(MEAN ~ DATE + (1|uniqueID) + (1|DATE), data = subset(cig_data, leaf == "leafy" & GR_type == "ext"))) 
summary(lmer(MEAN ~ DATE + (1|uniqueID) + (1|DATE), data = subset(cig_data, leaf == "leafy" & GR_type == "int"))) 


## Pre-growing seasons 
# Mean CIg vs. Green roof age 
summary(lmer(MEAN ~ GR_age_of_year + (1|uniqueID) + (1|DATE), data = subset(cig_data, leaf == "leafless"))) 
# Mean CIg vs. Date
summary(lmer(MEAN ~ DATE + (1|uniqueID) + (1|DATE), data = subset(cig_data, leaf == "leafless"))) 

### Grouped by green roof type ###
# Mean CIg vs. Green roof age 
# Extensive green roofs
summary(lmer(MEAN ~ GR_age_of_year + (1|uniqueID) + (1|DATE), data = subset(cig_data, leaf == "leafless" & GR_type == "ext"))) 
# Intensive green roofs
summary(lmer(MEAN ~ GR_age_of_year + (1|uniqueID) + (1|DATE), data = subset(cig_data, leaf == "leafless" & GR_type == "int"))) 

# Mean CIg vs. Date
# Extensive green roofs
summary(lmer(MEAN ~ DATE + (1|uniqueID) + (1|DATE), data = subset(cig_data, leaf == "leafless" & GR_type == "ext"))) 
# Intensive green roofs
summary(lmer(MEAN ~ DATE + (1|uniqueID) + (1|DATE), data = subset(cig_data, leaf == "leafless" & GR_type == "int"))) 


###### Group by green roof type: COV SAVI #######
# Change order of display
savi_data$GR_type <- factor(savi_data$GR_type, levels = c("ext", "int"))

# Plot for growing seasons 
CV_savi_GRtype_leafy.plot <- ggplot(savi_data[savi_data$leaf == "leafy",], aes(x=GR_age_of_year,y=COV, color = GR_type, linetype = GR_type))+
  geom_point(aes(shape = GR_type), size=3, alpha = 0.3) +
  geom_smooth(aes(color = GR_type, fill=GR_type, linetype = GR_type),
              method='lm', formula= y~x, alpha = 0.2, cex=1.3, se = T)+
  labs(x="Green roof age (year)", y="CV of SAVI",
       group = "GR_type", linetype = "GR_type", pch = "GR_type")+
  scale_color_manual(name="Green roof type",labels = c("Extensive","Intensive"),values = c("#FC7307","#236AB9"))+
  scale_fill_manual(name="Green roof type",labels = c("Extensive","Intensive"),values = c("#FC7307","#236AB9"))+
  scale_linetype_manual(name="Green roof type", labels = c("Extensive","Intensive"), values = c(1,2))+
  scale_shape_manual(name="Green roof type", labels = c("Extensive","Intensive"), values = c(19,17))+
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8))+
  theme_bw(base_size = 28)+
  theme(
    legend.position = "top",
    legend.box = "vertical",
    legend.title = element_text(size=36),
    legend.text = element_text(size = 32),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.text=element_text(size=28),
    axis.title=element_text(size=32)
  ) 

CV_savi_GRtype_leafy.plot


# Change order of display
savi_data$GR_type <- factor(savi_data$GR_type, levels = c("ext", "int"))

# Plot for pre-growing seasons 
CV_savi_GRtype_leafless.plot <- ggplot(savi_data[savi_data$leaf == "leafless",], aes(x=GR_age_of_year,y=COV, color = GR_type, linetype = GR_type))+
  geom_point(aes(shape = GR_type), size=3, alpha = 0.3) +
  geom_smooth(aes(color = GR_type, fill=GR_type, linetype = GR_type),
              method='lm', formula= y~x, alpha = 0.2, cex=1.3, se = T)+
  labs(x="Green roof age (year)", y="CV of SAVI",
       group = "GR_type", linetype = "GR_type", pch = "GR_type")+
  scale_color_manual(name="Green roof type",labels = c("Extensive","Intensive"),values = c("#FC7307","#236AB9"))+
  scale_fill_manual(name="Green roof type",labels = c("Extensive","Intensive"),values = c("#FC7307","#236AB9"))+
  scale_linetype_manual(name="Green roof type", labels = c("Extensive","Intensive"), values = c(1,1))+
  scale_shape_manual(name="Green roof type", labels = c("Extensive","Intensive"), values = c(19,17))+
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8))+
  theme_bw(base_size = 28)+
  theme(
    legend.position = "top",
    legend.box = "vertical",
    legend.title = element_text(size=36),
    legend.text = element_text(size = 32),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.text=element_text(size=28),
    axis.title=element_text(size=32)
  ) 

CV_savi_GRtype_leafless.plot


### Statistics: linear mixed effect models ###
### Overall trends ###
# Include DATE and uniqueID as random factors
model_savi <- lmer(COV ~ GR_age_of_year + DATE + leaf + GR_type + (1|uniqueID) + (1|DATE), data = savi_data)
summary(model_savi)

# Model selection for the best predictors
selected_model_savi <- step(model_savi, direction = "both") # shows the steps: has AIC
print(selected_model_savi)

# Final model based on the selection
final_model_savi <- lmer(COV ~ GR_age_of_year + leaf + (1 | uniqueID) + (1 | DATE), data = savi_data)
summary(final_model_savi)

### Trend grouped by seasons ###
## Growing seasons 
# COV SAVI vs. Green roof age 
summary(lmer(COV ~ GR_age_of_year + (1|uniqueID) + (1|DATE), data = subset(savi_data, leaf == "leafy"))) 
# COV SAVI vs. Date
summary(lmer(COV ~ DATE + (1|uniqueID) + (1|DATE), data = subset(savi_data, leaf == "leafy"))) 

## Grouped by green roof type 
# COV SAVI vs. Green roof age 
# Extensive green roofs
summary(lmer(COV ~ GR_age_of_year + (1|uniqueID) + (1|DATE), data = subset(savi_data, leaf == "leafy" & GR_type == "ext"))) 
# Intensive green roofs
summary(lmer(COV ~ GR_age_of_year + (1|uniqueID) + (1|DATE), data = subset(savi_data, leaf == "leafy" & GR_type == "int"))) 

# COV SAVI vs. Date
# Extensive green roofs
summary(lmer(COV ~ DATE + (1|uniqueID) + (1|DATE), data = subset(savi_data, leaf == "leafy" & GR_type == "ext"))) 
# Intensive green roofs
summary(lmer(COV ~ DATE + (1|uniqueID) + (1|DATE), data = subset(savi_data, leaf == "leafy" & GR_type == "int"))) 


## Pre-growing seasons 
# COV SAVI vs. Green roof age
summary(lmer(COV ~ GR_age_of_year + (1|uniqueID) + (1|DATE), data = subset(savi_data, leaf == "leafless"))) 
# COV SAVI vs. Date
summary(lmer(COV ~ DATE + (1|uniqueID) + (1|DATE), data = subset(savi_data, leaf == "leafless"))) 

### Grouped by green roof type ###
# COV SAVI vs. Green roof age 
# Extensive green roofs
summary(lmer(COV ~ GR_age_of_year + (1|uniqueID) + (1|DATE), data = subset(savi_data, leaf == "leafless" & GR_type == "ext"))) 
# Intensive green roofs
summary(lmer(COV ~ GR_age_of_year + (1|uniqueID) + (1|DATE), data = subset(savi_data, leaf == "leafless" & GR_type == "int"))) 

# COV SAVI vs. Date
# Extensive green roofs
summary(lmer(COV ~ DATE + (1|uniqueID) + (1|DATE), data = subset(savi_data, leaf == "leafless" & GR_type == "ext"))) 
# Intensive green roofs
summary(lmer(COV ~ DATE + (1|uniqueID) + (1|DATE), data = subset(savi_data, leaf == "leafless" & GR_type == "int"))) 


###### Group by green roof type: COV GNDVI #######
# Change order of display
gndvi_data$GR_type <- factor(gndvi_data$GR_type, levels = c("ext", "int"))

# Plot for growing seasons 
CV_gndvi_GRtype_leafy.plot <- ggplot(gndvi_data[gndvi_data$leaf == "leafy",], aes(x=GR_age_of_year,y=COV, color = GR_type, linetype = GR_type))+
  geom_point(aes(shape = GR_type), size=3, alpha = 0.3) +
  geom_smooth(aes(color = GR_type, fill=GR_type, linetype = GR_type),
              method='lm', formula= y~x, alpha = 0.2, cex=1.3, se = T)+
  labs(x="Green roof age (year)", y="CV of GNDVI",
       group = "GR_type", linetype = "GR_type", pch = "GR_type")+
  scale_color_manual(name="Green roof type",labels = c("Extensive","Intensive"),values = c("#FC7307","#236AB9"))+
  scale_fill_manual(name="Green roof type",labels = c("Extensive","Intensive"),values = c("#FC7307","#236AB9"))+
  scale_linetype_manual(name="Green roof type", labels = c("Extensive","Intensive"), values = c(1,2))+
  scale_shape_manual(name="Green roof type", labels = c("Extensive","Intensive"), values = c(19,17))+
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8))+
  theme_bw(base_size = 28)+
  theme(
    legend.position = "top",
    legend.box = "vertical",
    legend.title = element_text(size=36),
    legend.text = element_text(size = 32),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.text=element_text(size=28),
    axis.title=element_text(size=32)
  ) 

CV_gndvi_GRtype_leafy.plot


# Change order of display
gndvi_data$GR_type <- factor(gndvi_data$GR_type, levels = c("ext", "int"))

# Plot for pre-growing seasons 
CV_gndvi_GRtype_leafless.plot <- ggplot(gndvi_data[gndvi_data$leaf == "leafless",], aes(x=GR_age_of_year,y=COV, color = GR_type, linetype = GR_type))+
  geom_point(aes(shape = GR_type), size=3, alpha = 0.3) +
  geom_smooth(aes(color = GR_type, fill=GR_type, linetype = GR_type),
              method='lm', formula= y~x, alpha = 0.2, cex=1.3, se = T)+
  labs(x="Green roof age (year)", y="CV of GNDVI",
       group = "GR_type", linetype = "GR_type", pch = "GR_type")+
  scale_color_manual(name="Green roof type",labels = c("Extensive","Intensive"),values = c("#FC7307","#236AB9"))+
  scale_fill_manual(name="Green roof type",labels = c("Extensive","Intensive"),values = c("#FC7307","#236AB9"))+
  scale_linetype_manual(name="Green roof type", labels = c("Extensive","Intensive"), values = c(1,1))+
  scale_shape_manual(name="Green roof type", labels = c("Extensive","Intensive"), values = c(19,17))+
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8))+
  theme_bw(base_size = 28)+
  theme(
    legend.position = "top",
    legend.box = "vertical",
    legend.title = element_text(size=36),
    legend.text = element_text(size = 32),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.text=element_text(size=28),
    axis.title=element_text(size=32)
  ) 

CV_gndvi_GRtype_leafless.plot


### Statistics: linear mixed effect models ###
### Overall trends ###
# Include DATE and uniqueID as random factors
model_gndvi <- lmer(COV ~ GR_age_of_year + DATE + leaf + GR_type + (1|uniqueID) + (1|DATE), data = gndvi_data)
summary(model_gndvi)

# Model selection for the best predictors
selected_model_gndvi <- step(model_gndvi, direction = "both") # shows the steps: has AIC
print(selected_model_gndvi)

# Final model based on the selection
final_model_gndvi <- lmer(COV ~ GR_age_of_year + (1 | uniqueID) + (1 | DATE), data = gndvi_data)
summary(final_model_gndvi)

### Trend grouped by seasons ###
## Growing seasons 
# COV GNDVI vs. Green roof age
summary(lmer(COV ~ GR_age_of_year + (1|uniqueID) + (1|DATE), data = subset(gndvi_data, leaf == "leafy"))) 
# COV GNDVI vs. Date
summary(lmer(COV ~ DATE + (1|uniqueID) + (1|DATE), data = subset(gndvi_data, leaf == "leafy"))) 

## Grouped by green roof type 
# COV GNDVI vs. Green roof age 
# Extensive green roofs
summary(lmer(COV ~ GR_age_of_year + (1|uniqueID) + (1|DATE), data = subset(gndvi_data, leaf == "leafy" & GR_type == "ext"))) 
# Intensive green roofs
summary(lmer(COV ~ GR_age_of_year + (1|uniqueID) + (1|DATE), data = subset(gndvi_data, leaf == "leafy" & GR_type == "int"))) 

# COV GNDVI vs. Date
# Extensive green roofs
summary(lmer(COV ~ DATE + (1|uniqueID) + (1|DATE), data = subset(gndvi_data, leaf == "leafy" & GR_type == "ext"))) 
# Intensive green roofs
summary(lmer(COV ~ DATE + (1|uniqueID) + (1|DATE), data = subset(gndvi_data, leaf == "leafy" & GR_type == "int"))) 


## Pre-growing seasons 
# COV GNDVI vs. Green roof age 
summary(lmer(COV ~ GR_age_of_year + (1|uniqueID) + (1|DATE), data = subset(gndvi_data, leaf == "leafless"))) 
# COV GNDVI vs. Date
summary(lmer(COV ~ DATE + (1|uniqueID) + (1|DATE), data = subset(gndvi_data, leaf == "leafless"))) 

### Grouped by green roof type ###
# COV GNDVI vs. Green roof age 
# Extensive green roofs
summary(lmer(COV ~ GR_age_of_year + (1|uniqueID) + (1|DATE), data = subset(gndvi_data, leaf == "leafless" & GR_type == "ext"))) 
# Intensive green roofs
summary(lmer(COV ~ GR_age_of_year + (1|uniqueID) + (1|DATE), data = subset(gndvi_data, leaf == "leafless" & GR_type == "int"))) 

# COV GNDVI vs. Date
# Extensive green roofs
summary(lmer(COV ~ DATE + (1|uniqueID) + (1|DATE), data = subset(gndvi_data, leaf == "leafless" & GR_type == "ext"))) 
# Intensive green roofs
summary(lmer(COV ~ DATE + (1|uniqueID) + (1|DATE), data = subset(gndvi_data, leaf == "leafless" & GR_type == "int"))) 


###### Group by green roof type: COV CIg #######
# Change order of display
cig_data$GR_type <- factor(cig_data$GR_type, levels = c("ext", "int"))

# Plot for growing seasons 
CV_cig_GRtype_leafy.plot <- ggplot(cig_data[cig_data$leaf == "leafy",], aes(x=GR_age_of_year,y=COV, color = GR_type, linetype = GR_type))+
  geom_point(aes(shape = GR_type), size=3, alpha = 0.3) +
  geom_smooth(aes(color = GR_type, fill=GR_type, linetype = GR_type),
              method='lm', formula= y~x, alpha = 0.2, cex=1.3, se = T)+
  labs(x="Green roof age (year)", y="CV of CIg",
       group = "GR_type", linetype = "GR_type", pch = "GR_type")+
  scale_color_manual(name="Green roof type",labels = c("Extensive","Intensive"),values = c("#FC7307","#236AB9"))+
  scale_fill_manual(name="Green roof type",labels = c("Extensive","Intensive"),values = c("#FC7307","#236AB9"))+
  scale_linetype_manual(name="Green roof type", labels = c("Extensive","Intensive"), values = c(1,2))+
  scale_shape_manual(name="Green roof type", labels = c("Extensive","Intensive"), values = c(19,17))+
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8))+
  theme_bw(base_size = 28)+
  theme(
    legend.position = "top",
    legend.box = "vertical",
    legend.title = element_text(size=36),
    legend.text = element_text(size = 32),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.text=element_text(size=28),
    axis.title=element_text(size=32)
  ) 

CV_cig_GRtype_leafy.plot


# Change order of display
cig_data$GR_type <- factor(cig_data$GR_type, levels = c("ext", "int"))

# Plot for pre-growing seasons 
CV_cig_GRtype_leafless.plot <- ggplot(cig_data[cig_data$leaf == "leafless",], aes(x=GR_age_of_year,y=COV, color = GR_type, linetype = GR_type))+
  geom_point(aes(shape = GR_type), size=3, alpha = 0.3) +
  geom_smooth(aes(color = GR_type, fill=GR_type, linetype = GR_type),
              method='lm', formula= y~x, alpha = 0.2, cex=1.3, se = T)+
  labs(x="Green roof age (year)", y="CV of GNDVI",
       group = "GR_type", linetype = "GR_type", pch = "GR_type")+
  scale_color_manual(name="Green roof type",labels = c("Extensive","Intensive"),values = c("#FC7307","#236AB9"))+
  scale_fill_manual(name="Green roof type",labels = c("Extensive","Intensive"),values = c("#FC7307","#236AB9"))+
  scale_linetype_manual(name="Green roof type", labels = c("Extensive","Intensive"), values = c(1,1))+
  scale_shape_manual(name="Green roof type", labels = c("Extensive","Intensive"), values = c(19,17))+
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8))+
  theme_bw(base_size = 28)+
  theme(
    legend.position = "top",
    legend.box = "vertical",
    legend.title = element_text(size=36),
    legend.text = element_text(size = 32),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.text=element_text(size=28),
    axis.title=element_text(size=32)
  ) 

CV_cig_GRtype_leafless.plot


### Statistics: linear mixed effect models ###
### Overall trends ###
# Include DATE and uniqueID as random factors
model_cig <- lmer(COV ~ GR_age_of_year + DATE + leaf + GR_type + (1|uniqueID) + (1|DATE), data = cig_data)
summary(model_cig)

# Model selection for the best predictors
selected_model_cig <- step(model_cig, direction = "both") # shows the steps: has AIC
print(selected_model_cig)

# Final model based on the selection
final_model_cig <- lmer(COV ~ GR_age_of_year + (1 | uniqueID) + (1 | DATE), data = cig_data)
summary(final_model_cig)

### Trend grouped by seasons ###
## Growing seasons 
# COV CIg vs. Green roof age 
summary(lmer(COV ~ GR_age_of_year + (1|uniqueID) + (1|DATE), data = subset(cig_data, leaf == "leafy"))) 
# COV CIg vs. Date 
summary(lmer(COV ~ DATE + (1|uniqueID) + (1|DATE), data = subset(cig_data, leaf == "leafy"))) 

## Grouped by green roof type 
# COV CIg vs. Green roof age 
# Extensive green roofs
summary(lmer(COV ~ GR_age_of_year + (1|uniqueID) + (1|DATE), data = subset(cig_data, leaf == "leafy" & GR_type == "ext"))) 
# Intensive green roofs
summary(lmer(COV ~ GR_age_of_year + (1|uniqueID) + (1|DATE), data = subset(cig_data, leaf == "leafy" & GR_type == "int"))) 

# COV CIg vs. Date
# Extensive green roofs
summary(lmer(COV ~ DATE + (1|uniqueID) + (1|DATE), data = subset(cig_data, leaf == "leafy" & GR_type == "ext"))) 
# Intensive green roofs
summary(lmer(COV ~ DATE + (1|uniqueID) + (1|DATE), data = subset(cig_data, leaf == "leafy" & GR_type == "int"))) 


## Pre-growing seasons 
# COV CIg vs. Green roof age 
summary(lmer(COV ~ GR_age_of_year + (1|uniqueID) + (1|DATE), data = subset(cig_data, leaf == "leafless"))) 
# COV CIg vs. Date
summary(lmer(COV ~ DATE + (1|uniqueID) + (1|DATE), data = subset(cig_data, leaf == "leafless"))) 

### Grouped by green roof type ###
# COV CIg vs. Green roof age 
# Extensive green roofs
summary(lmer(COV ~ GR_age_of_year + (1|uniqueID) + (1|DATE), data = subset(cig_data, leaf == "leafless" & GR_type == "ext"))) 
# Intensive green roofs
summary(lmer(COV ~ GR_age_of_year + (1|uniqueID) + (1|DATE), data = subset(cig_data, leaf == "leafless" & GR_type == "int"))) 

# COV CIg vs. Date
# Extensive green roofs
summary(lmer(COV ~ DATE + (1|uniqueID) + (1|DATE), data = subset(cig_data, leaf == "leafless" & GR_type == "ext"))) 
# Intensive green roofs
summary(lmer(COV ~ DATE + (1|uniqueID) + (1|DATE), data = subset(cig_data, leaf == "leafless" & GR_type == "int"))) 



###### Combine SI Figure S3: Mean VIs and COV VIs grouped by green roof type ######
merge_VIs_GR_type = mean_savi_GRtype_leafy.plot + mean_savi_GRtype_leafless.plot + 
  mean_gndvi_GRtype_leafy.plot + mean_gndvi_GRtype_leafless.plot + 
  mean_cig_GRtype_leafy.plot + mean_cig_GRtype_leafless.plot + 
  CV_savi_GRtype_leafy.plot + CV_savi_GRtype_leafless.plot +
  CV_gndvi_GRtype_leafy.plot + CV_gndvi_GRtype_leafless.plot +
  CV_cig_GRtype_leafy.plot + CV_cig_GRtype_leafless.plot +
  plot_layout(guides = "collect", ncol = 2) & theme(legend.position = "top")

merge_VIs_GR_type[[1]] = merge_VIs_GR_type[[1]] + theme(axis.text.x = element_blank(),axis.title.x = element_blank(),legend.position = "none")
merge_VIs_GR_type[[2]] = merge_VIs_GR_type[[2]] + theme(axis.text.x = element_blank(),axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position = "none")
merge_VIs_GR_type[[3]] = merge_VIs_GR_type[[3]] + theme(axis.text.x = element_blank(),axis.title.x = element_blank(),legend.position = "none")
merge_VIs_GR_type[[4]] = merge_VIs_GR_type[[4]] + theme(axis.text.x = element_blank(),axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position = "none")
merge_VIs_GR_type[[5]] = merge_VIs_GR_type[[5]] + theme(axis.text.x = element_blank(),axis.title.x = element_blank(),legend.position = "none")
merge_VIs_GR_type[[6]] = merge_VIs_GR_type[[6]] + theme(axis.text.x = element_blank(),axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position = "none")
merge_VIs_GR_type[[7]] = merge_VIs_GR_type[[7]] + theme(axis.text.x = element_blank(),axis.title.x = element_blank(),legend.position = "none")
merge_VIs_GR_type[[8]] = merge_VIs_GR_type[[8]] + theme(axis.text.x = element_blank(),axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position = "none")
merge_VIs_GR_type[[9]] = merge_VIs_GR_type[[9]] + theme(axis.text.x = element_blank(),axis.title.x = element_blank(),legend.position = "none")
merge_VIs_GR_type[[10]] = merge_VIs_GR_type[[10]] + theme(axis.text.x = element_blank(),axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position = "none")
merge_VIs_GR_type[[11]] = merge_VIs_GR_type[[11]] + theme(legend.position = "none")
merge_VIs_GR_type[[12]] = merge_VIs_GR_type[[12]] + theme(axis.title.y = element_blank())

merge_VIs_GR_type

# ggsave(plot=merge_VIs_GR_type, filename = "path/Fig_VIsAge_GR_type.tif", width = 13, height = 20, device='tiff', dpi=600,unit="in")




### Figure S5: Grouped by vegetation type #####
###### Mean SAVI grouped by vegetation type #######
# change order of display
savi_data$new_GRtype <- factor(savi_data$new_GRtype, levels = c("grass", "sedum_mat", "woody_plants","mix_grass_tree"))

# Plot for growing seasons 
mean_savi_vege_type_leafy.plot <- ggplot(savi_data[savi_data$leaf == "leafy",], aes(x=GR_age_of_year,y=MEAN, color = new_GRtype, linetype = new_GRtype))+
  geom_point(aes(shape = new_GRtype), size=3, alpha = 0.3) +
  geom_smooth(aes(color = new_GRtype, fill=new_GRtype, linetype = new_GRtype),
              method='lm', formula= y~x, alpha = 0.2, cex=1.3, se = T)+
  # annotate("text", x = 6, y = 0.8, label = "Leafy", size = 10, color = "black", hjust = 0) +
  labs(x="Green roof age (year)", y="Mean SAVI",
       group = "new_GRtype", linetype = "new_GRtype", pch = "new_GRtype")+
  scale_color_manual(name="Vegetation type",labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"),values = c("#00BFFF", "#FF762E", "#7CAE00","#cd6ee8"))+
  scale_fill_manual(name="Vegetation type",labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"),values = c("#00BFFF", "#FF762E", "#7CAE00","#cd6ee8"))+
  scale_linetype_manual(name="Vegetation type", labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"), values = c(2,1, 1, 2))+
  scale_shape_manual(name="Vegetation type", labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"), values = c(19,17, 15, 18))+
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8))+
  theme_bw(base_size = 28)+
  theme(
    legend.position = "top",
    legend.box = "vertical",
    legend.title = element_text(size=36),
    legend.text = element_text(size = 32),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.text=element_text(size=28),
    axis.title=element_text(size=32)
  ) +
  guides(
    color = guide_legend(nrow = 2, byrow = TRUE),
    linetype = guide_legend(nrow = 2, byrow = TRUE)
  )

mean_savi_vege_type_leafy.plot


# change order of display
savi_data$new_GRtype <- factor(savi_data$new_GRtype, levels = c("grass", "sedum_mat", "woody_plants","mix_grass_tree"))

# Plot for pre-growing seasons 
mean_savi_vege_type_leafless.plot <- ggplot(savi_data[savi_data$leaf == "leafless",], aes(x=GR_age_of_year,y=MEAN, color = new_GRtype, linetype = new_GRtype))+
  geom_point(aes(shape = new_GRtype), size=3, alpha = 0.3) +
  geom_smooth(aes(color = new_GRtype, fill=new_GRtype, linetype = new_GRtype),
              method='lm', formula= y~x, alpha = 0.2, cex=1.3, se = T)+
  labs(x="Green roof age (year)", y="Mean SAVI",
       group = "new_GRtype", linetype = "new_GRtype", pch = "new_GRtype")+
  scale_color_manual(name="Vegetation type",labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"),values = c("#00BFFF", "#FF762E", "#7CAE00","#cd6ee8"))+
  scale_fill_manual(name="Vegetation type",labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"),values = c("#00BFFF", "#FF762E", "#7CAE00","#cd6ee8"))+
  scale_linetype_manual(name="Vegetation type", labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"), values = c(2,2, 2, 2))+
  scale_shape_manual(name="Vegetation type", labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"), values = c(19,17, 15, 18))+
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8))+
  theme_bw(base_size = 28)+
  theme(
    legend.position = "top",
    legend.box = "vertical",
    legend.title = element_text(size=36),
    legend.text = element_text(size = 32),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.text=element_text(size=28),
    axis.title=element_text(size=32)
  ) +
  guides(
    color = guide_legend(nrow = 2, byrow = TRUE),
    linetype = guide_legend(nrow = 2, byrow = TRUE)
  )

mean_savi_vege_type_leafless.plot


### Statistics: linear mixed effect models ###
### Overall trends ###
# Include DATE and uniqueID as random factors
model_savi <- lmer(MEAN ~ GR_age_of_year + DATE + leaf + new_GRtype + (1|uniqueID) + (1|DATE), data = savi_data)
summary(model_savi)

# Model selection for the best predictors
selected_model_savi <- step(model_savi, direction = "both") # shows the steps: has AIC
print(selected_model_savi)

# Final model based on the selection
final_model_savi <- lmer(MEAN ~ GR_age_of_year + DATE + leaf + new_GRtype + (1 | uniqueID) + (1 | DATE), data = savi_data)
summary(final_model_savi)

### Trend grouped by seasons ###
## Growing seasons 
# Mean SAVI vs. Green roof age
summary(lmer(MEAN ~ GR_age_of_year + (1|uniqueID) + (1|DATE), data = subset(savi_data, leaf == "leafy"))) 
# Mean SAVI vs. Date
summary(lmer(MEAN ~ DATE + (1|uniqueID) + (1|DATE), data = subset(savi_data, leaf == "leafy"))) 

## Grouped by vegetation type 
# Mean SAVI vs. Green roof age 
summary(lmer(MEAN ~ GR_age_of_year + (1|uniqueID) + (1|DATE), data = subset(savi_data, leaf == "leafy" & new_GRtype == "grass"))) 
summary(lmer(MEAN ~ GR_age_of_year + (1|uniqueID) + (1|DATE), data = subset(savi_data, leaf == "leafy" & new_GRtype == "sedum_mat"))) 
summary(lmer(MEAN ~ GR_age_of_year + (1|uniqueID) + (1|DATE), data = subset(savi_data, leaf == "leafy" & new_GRtype == "woody_plants"))) 
summary(lmer(MEAN ~ GR_age_of_year + (1|uniqueID) + (1|DATE), data = subset(savi_data, leaf == "leafy" & new_GRtype == "mix_grass_tree"))) 

# Mean SAVI vs. Date
summary(lmer(MEAN ~ DATE + (1|uniqueID) + (1|DATE), data = subset(savi_data, leaf == "leafy" & new_GRtype == "grass"))) 
summary(lmer(MEAN ~ DATE + (1|uniqueID) + (1|DATE), data = subset(savi_data, leaf == "leafy" & new_GRtype == "sedum_mat"))) 
summary(lmer(MEAN ~ DATE + (1|uniqueID) + (1|DATE), data = subset(savi_data, leaf == "leafy" & new_GRtype == "woody_plants"))) 
summary(lmer(MEAN ~ DATE + (1|uniqueID) + (1|DATE), data = subset(savi_data, leaf == "leafy" & new_GRtype == "mix_grass_tree"))) 


## Pre-growing seasons 
# Mean SAVI vs. Green roof age 
summary(lmer(MEAN ~ GR_age_of_year + (1|uniqueID) + (1|DATE), data = subset(savi_data, leaf == "leafless"))) 
# Mean SAVI vs. Date
summary(lmer(MEAN ~ DATE + (1|uniqueID) + (1|DATE), data = subset(savi_data, leaf == "leafless"))) 

## Grouped by vegetation type 
# Mean SAVI vs. Green roof age 
summary(lmer(MEAN ~ GR_age_of_year + (1|uniqueID) + (1|DATE), data = subset(savi_data, leaf == "leafless" & new_GRtype == "grass"))) 
summary(lmer(MEAN ~ GR_age_of_year + (1|uniqueID) + (1|DATE), data = subset(savi_data, leaf == "leafless" & new_GRtype == "sedum_mat"))) 
summary(lmer(MEAN ~ GR_age_of_year + (1|uniqueID) + (1|DATE), data = subset(savi_data, leaf == "leafless" & new_GRtype == "woody_plants"))) 
summary(lmer(MEAN ~ GR_age_of_year + (1|uniqueID) + (1|DATE), data = subset(savi_data, leaf == "leafless" & new_GRtype == "mix_grass_tree"))) 

# Mean SAVI vs. Date
summary(lmer(MEAN ~ DATE + (1|uniqueID) + (1|DATE), data = subset(savi_data, leaf == "leafless" & new_GRtype == "grass"))) 
summary(lmer(MEAN ~ DATE + (1|uniqueID) + (1|DATE), data = subset(savi_data, leaf == "leafless" & new_GRtype == "sedum_mat"))) 
summary(lmer(MEAN ~ DATE + (1|uniqueID) + (1|DATE), data = subset(savi_data, leaf == "leafless" & new_GRtype == "woody_plants"))) 
summary(lmer(MEAN ~ DATE + (1|uniqueID) + (1|DATE), data = subset(savi_data, leaf == "leafless" & new_GRtype == "mix_grass_tree"))) 


###### Group by vegetation type: Mean GNDVI #######
# change order of display
gndvi_data$new_GRtype <- factor(gndvi_data$new_GRtype, levels = c("grass", "sedum_mat", "woody_plants","mix_grass_tree"))

# Plot for growing seasons 
mean_gndvi_vege_type_leafy.plot <- ggplot(gndvi_data[gndvi_data$leaf == "leafy",], aes(x=GR_age_of_year,y=MEAN, color = new_GRtype, linetype = new_GRtype))+
  geom_point(aes(shape = new_GRtype), size=3, alpha = 0.3) +
  geom_smooth(aes(color = new_GRtype, fill=new_GRtype, linetype = new_GRtype),
              method='lm', formula= y~x, alpha = 0.2, cex=1.3, se = T)+
  labs(x="Green roof age (year)", y="Mean GNDVI",
       group = "new_GRtype", linetype = "new_GRtype", pch = "new_GRtype")+
  scale_color_manual(name="Vegetation type",labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"),values = c("#00BFFF", "#FF762E", "#7CAE00","#cd6ee8"))+
  scale_fill_manual(name="Vegetation type",labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"),values = c("#00BFFF", "#FF762E", "#7CAE00","#cd6ee8"))+
  scale_linetype_manual(name="Vegetation type", labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"), values = c(2,1, 1, 2))+
  scale_shape_manual(name="Vegetation type", labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"), values = c(19,17, 15, 18))+
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8))+
  theme_bw(base_size = 28)+
  theme(
    legend.position = "top",
    legend.box = "vertical",
    legend.title = element_text(size=36),
    legend.text = element_text(size = 32),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.text=element_text(size=28),
    axis.title=element_text(size=32)
  ) +
  guides(
    color = guide_legend(nrow = 2, byrow = TRUE),
    linetype = guide_legend(nrow = 2, byrow = TRUE)
  )

mean_gndvi_vege_type_leafy.plot


# change order of display
gndvi_data$new_GRtype <- factor(gndvi_data$new_GRtype, levels = c("grass", "sedum_mat", "woody_plants","mix_grass_tree"))

# Plot for pre-growing seasons 
mean_gndvi_vege_type_leafless.plot <- ggplot(gndvi_data[gndvi_data$leaf == "leafless",], aes(x=GR_age_of_year,y=MEAN, color = new_GRtype, linetype = new_GRtype))+
  geom_point(aes(shape = new_GRtype), size=3, alpha = 0.3) +
  geom_smooth(aes(color = new_GRtype, fill=new_GRtype, linetype = new_GRtype),
              method='lm', formula= y~x, alpha = 0.2, cex=1.3, se = T)+
  # geom_hline(yintercept=0.23, linetype="solid", color = "black",size=1)+ # control
  labs(x="Green roof age (year)", y="Mean GNDVI",
       group = "new_GRtype", linetype = "new_GRtype", pch = "new_GRtype")+
  scale_color_manual(name="Vegetation type",labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"),values = c("#00BFFF", "#FF762E", "#7CAE00","#cd6ee8"))+
  scale_fill_manual(name="Vegetation type",labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"),values = c("#00BFFF", "#FF762E", "#7CAE00","#cd6ee8"))+
  scale_linetype_manual(name="Vegetation type", labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"), values = c(2,2, 1, 2))+
  scale_shape_manual(name="Vegetation type", labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"), values = c(19,17, 15, 18))+
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8))+
  theme_bw(base_size = 28)+
  theme(
    #legend.position = c(0.75, 0.85),
    legend.position = "top",
    legend.box = "vertical",
    legend.title = element_text(size=36),
    legend.text = element_text(size = 32),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.text=element_text(size=28),
    axis.title=element_text(size=32)
  ) +
  guides(
    color = guide_legend(nrow = 2, byrow = TRUE),
    linetype = guide_legend(nrow = 2, byrow = TRUE)
  )

mean_gndvi_vege_type_leafless.plot


### Statistics: linear mixed effect models ###
### Overall trends ###
# Include DATE and uniqueID as random factors
model_gndvi <- lmer(MEAN ~ GR_age_of_year + DATE + leaf + new_GRtype + (1|uniqueID) + (1|DATE), data = gndvi_data)
summary(model_gndvi)

# Model selection for the best predictors
selected_model_gndvi <- step(model_gndvi, direction = "both") # shows the steps: has AIC
print(selected_model_gndvi)

# Final model based on the selection
final_model_gndvi <- lmer(MEAN ~ GR_age_of_year + DATE + leaf + new_GRtype + (1 | uniqueID) + (1 | DATE), data = gndvi_data)
summary(final_model_gndvi)

### Trend grouped by seasons ###
## Growing seasons 
# Mean GNDVI vs. Green roof age
summary(lmer(MEAN ~ GR_age_of_year + (1|uniqueID) + (1|DATE), data = subset(gndvi_data, leaf == "leafy"))) 
# Mean GNDVI vs. Date
summary(lmer(MEAN ~ DATE + (1|uniqueID) + (1|DATE), data = subset(gndvi_data, leaf == "leafy"))) 

## Grouped by vegetation type 
# Mean GNDVI vs. Green roof age 
summary(lmer(MEAN ~ GR_age_of_year + (1|uniqueID) + (1|DATE), data = subset(gndvi_data, leaf == "leafy" & new_GRtype == "grass"))) 
summary(lmer(MEAN ~ GR_age_of_year + (1|uniqueID) + (1|DATE), data = subset(gndvi_data, leaf == "leafy" & new_GRtype == "sedum_mat"))) 
summary(lmer(MEAN ~ GR_age_of_year + (1|uniqueID) + (1|DATE), data = subset(gndvi_data, leaf == "leafy" & new_GRtype == "woody_plants"))) 
summary(lmer(MEAN ~ GR_age_of_year + (1|uniqueID) + (1|DATE), data = subset(gndvi_data, leaf == "leafy" & new_GRtype == "mix_grass_tree"))) 

# Mean GNDVI vs. Date
summary(lmer(MEAN ~ DATE + (1|uniqueID) + (1|DATE), data = subset(gndvi_data, leaf == "leafy" & new_GRtype == "grass"))) 
summary(lmer(MEAN ~ DATE + (1|uniqueID) + (1|DATE), data = subset(gndvi_data, leaf == "leafy" & new_GRtype == "sedum_mat"))) 
summary(lmer(MEAN ~ DATE + (1|uniqueID) + (1|DATE), data = subset(gndvi_data, leaf == "leafy" & new_GRtype == "woody_plants"))) 
summary(lmer(MEAN ~ DATE + (1|uniqueID) + (1|DATE), data = subset(gndvi_data, leaf == "leafy" & new_GRtype == "mix_grass_tree"))) 


## Pre-growing seasons 
# Mean GNDVI vs. Green roof age 
summary(lmer(MEAN ~ GR_age_of_year + (1|uniqueID) + (1|DATE), data = subset(gndvi_data, leaf == "leafless"))) 
# Mean GNDVI vs. Date
summary(lmer(MEAN ~ DATE + (1|uniqueID) + (1|DATE), data = subset(gndvi_data, leaf == "leafless"))) 

## Grouped by vegetation type 
# Mean GNDVI vs. Green roof age 
summary(lmer(MEAN ~ GR_age_of_year + (1|uniqueID) + (1|DATE), data = subset(gndvi_data, leaf == "leafless" & new_GRtype == "grass"))) 
summary(lmer(MEAN ~ GR_age_of_year + (1|uniqueID) + (1|DATE), data = subset(gndvi_data, leaf == "leafless" & new_GRtype == "sedum_mat"))) 
summary(lmer(MEAN ~ GR_age_of_year + (1|uniqueID) + (1|DATE), data = subset(gndvi_data, leaf == "leafless" & new_GRtype == "woody_plants"))) 
summary(lmer(MEAN ~ GR_age_of_year + (1|uniqueID) + (1|DATE), data = subset(gndvi_data, leaf == "leafless" & new_GRtype == "mix_grass_tree"))) 

# Mean GNDVI vs. Date
summary(lmer(MEAN ~ DATE + (1|uniqueID) + (1|DATE), data = subset(gndvi_data, leaf == "leafless" & new_GRtype == "grass"))) 
summary(lmer(MEAN ~ DATE + (1|uniqueID) + (1|DATE), data = subset(gndvi_data, leaf == "leafless" & new_GRtype == "sedum_mat"))) 
summary(lmer(MEAN ~ DATE + (1|uniqueID) + (1|DATE), data = subset(gndvi_data, leaf == "leafless" & new_GRtype == "woody_plants"))) 
summary(lmer(MEAN ~ DATE + (1|uniqueID) + (1|DATE), data = subset(gndvi_data, leaf == "leafless" & new_GRtype == "mix_grass_tree"))) 


###### Group by vegetation type: Mean CIg #######
# change order of display
cig_data$new_GRtype <- factor(cig_data$new_GRtype, levels = c("grass", "sedum_mat", "woody_plants","mix_grass_tree"))

# Plot for growing seasons 
mean_cig_vege_type_leafy.plot <- ggplot(cig_data[cig_data$leaf == "leafy",], aes(x=GR_age_of_year,y=MEAN, color = new_GRtype, linetype = new_GRtype))+
  geom_point(aes(shape = new_GRtype), size=3, alpha = 0.3) +
  geom_smooth(aes(color = new_GRtype, fill=new_GRtype, linetype = new_GRtype),
              method='lm', formula= y~x, alpha = 0.2, cex=1.3, se = T)+
  labs(x="Green roof age (year)", y="Mean CIg",
       group = "new_GRtype", linetype = "new_GRtype", pch = "new_GRtype")+
  scale_color_manual(name="Vegetation type",labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"),values = c("#00BFFF", "#FF762E", "#7CAE00","#cd6ee8"))+
  scale_fill_manual(name="Vegetation type",labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"),values = c("#00BFFF", "#FF762E", "#7CAE00","#cd6ee8"))+
  scale_linetype_manual(name="Vegetation type", labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"), values = c(2,1, 1, 2))+
  scale_shape_manual(name="Vegetation type", labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"), values = c(19,17, 15, 18))+
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8))+
  theme_bw(base_size = 28)+
  theme(
    legend.position = "top",
    legend.box = "vertical",
    legend.title = element_text(size=36),
    legend.text = element_text(size = 32),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.text=element_text(size=28),
    axis.title=element_text(size=32)
  ) +
  guides(
    color = guide_legend(nrow = 2, byrow = TRUE),
    linetype = guide_legend(nrow = 2, byrow = TRUE)
  )

mean_cig_vege_type_leafy.plot


# change order of display
cig_data$new_GRtype <- factor(cig_data$new_GRtype, levels = c("grass", "sedum_mat", "woody_plants","mix_grass_tree"))

# Plot for pre-growing seasons 
mean_cig_vege_type_leafless.plot <- ggplot(cig_data[cig_data$leaf == "leafless",], aes(x=GR_age_of_year,y=MEAN, color = new_GRtype, linetype = new_GRtype))+
  geom_point(aes(shape = new_GRtype), size=3, alpha = 0.3) +
  geom_smooth(aes(color = new_GRtype, fill=new_GRtype, linetype = new_GRtype),
              method='lm', formula= y~x, alpha = 0.2, cex=1.3, se = T)+
  # geom_hline(yintercept=0.23, linetype="solid", color = "black",size=1)+ # control
  labs(x="Green roof age (year)", y="Mean CIg",
       group = "new_GRtype", linetype = "new_GRtype", pch = "new_GRtype")+
  scale_color_manual(name="Vegetation type",labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"),values = c("#00BFFF", "#FF762E", "#7CAE00","#cd6ee8"))+
  scale_fill_manual(name="Vegetation type",labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"),values = c("#00BFFF", "#FF762E", "#7CAE00","#cd6ee8"))+
  scale_linetype_manual(name="Vegetation type", labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"), values = c(2,2, 2, 2))+
  scale_shape_manual(name="Vegetation type", labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"), values = c(19,17, 15, 18))+
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8))+
  theme_bw(base_size = 28)+
  theme(
    #legend.position = c(0.75, 0.85),
    legend.position = "top",
    legend.box = "vertical",
    legend.title = element_text(size=36),
    legend.text = element_text(size = 32),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.text=element_text(size=28),
    axis.title=element_text(size=32)
  ) +
  guides(
    color = guide_legend(nrow = 2, byrow = TRUE),
    linetype = guide_legend(nrow = 2, byrow = TRUE)
  )

mean_cig_vege_type_leafless.plot


### Statistics: linear mixed effect models ###
### Overall trends ###
# Include DATE and uniqueID as random factors
model_cig <- lmer(MEAN ~ GR_age_of_year + DATE + leaf + new_GRtype + (1|uniqueID) + (1|DATE), data = cig_data)
summary(model_cig)

# Model selection for the best predictors
selected_model_cig <- step(model_cig, direction = "both") # shows the steps: has AIC
print(selected_model_cig)

# Final model based on the selection
final_model_cig <- lmer(MEAN ~ GR_age_of_year + DATE + leaf + new_GRtype + (1 | uniqueID) + (1 | DATE), data = cig_data)
summary(final_model_cig)

### Trend grouped by seasons ###
## Growing seasons 
# Mean CIg vs. Green roof age 
summary(lmer(MEAN ~ GR_age_of_year + (1|uniqueID) + (1|DATE), data = subset(cig_data, leaf == "leafy"))) 
# Mean CIg vs. Date
summary(lmer(MEAN ~ DATE + (1|uniqueID) + (1|DATE), data = subset(cig_data, leaf == "leafy"))) 

## Grouped by vegetation type 
# Mean CIg vs. Green roof age 
summary(lmer(MEAN ~ GR_age_of_year + (1|uniqueID) + (1|DATE), data = subset(cig_data, leaf == "leafy" & new_GRtype == "grass"))) 
summary(lmer(MEAN ~ GR_age_of_year + (1|uniqueID) + (1|DATE), data = subset(cig_data, leaf == "leafy" & new_GRtype == "sedum_mat"))) 
summary(lmer(MEAN ~ GR_age_of_year + (1|uniqueID) + (1|DATE), data = subset(cig_data, leaf == "leafy" & new_GRtype == "woody_plants"))) 
summary(lmer(MEAN ~ GR_age_of_year + (1|uniqueID) + (1|DATE), data = subset(cig_data, leaf == "leafy" & new_GRtype == "mix_grass_tree"))) 

# Mean CIg vs. Date
summary(lmer(MEAN ~ DATE + (1|uniqueID) + (1|DATE), data = subset(cig_data, leaf == "leafy" & new_GRtype == "grass"))) 
summary(lmer(MEAN ~ DATE + (1|uniqueID) + (1|DATE), data = subset(cig_data, leaf == "leafy" & new_GRtype == "sedum_mat"))) 
summary(lmer(MEAN ~ DATE + (1|uniqueID) + (1|DATE), data = subset(cig_data, leaf == "leafy" & new_GRtype == "woody_plants"))) 
summary(lmer(MEAN ~ DATE + (1|uniqueID) + (1|DATE), data = subset(cig_data, leaf == "leafy" & new_GRtype == "mix_grass_tree"))) 


## Pre-growing seasons 
# Mean CIg vs. Green roof age 
summary(lmer(MEAN ~ GR_age_of_year + (1|uniqueID) + (1|DATE), data = subset(cig_data, leaf == "leafless"))) 
# Mean CIg vs. Date
summary(lmer(MEAN ~ DATE + (1|uniqueID) + (1|DATE), data = subset(cig_data, leaf == "leafless"))) 

## Grouped by vegetation type 
# Mean CIg vs. Green roof age
summary(lmer(MEAN ~ GR_age_of_year + (1|uniqueID) + (1|DATE), data = subset(cig_data, leaf == "leafless" & new_GRtype == "grass"))) 
summary(lmer(MEAN ~ GR_age_of_year + (1|uniqueID) + (1|DATE), data = subset(cig_data, leaf == "leafless" & new_GRtype == "sedum_mat"))) 
summary(lmer(MEAN ~ GR_age_of_year + (1|uniqueID) + (1|DATE), data = subset(cig_data, leaf == "leafless" & new_GRtype == "woody_plants"))) 
summary(lmer(MEAN ~ GR_age_of_year + (1|uniqueID) + (1|DATE), data = subset(cig_data, leaf == "leafless" & new_GRtype == "mix_grass_tree"))) 

# Mean CIg vs. Date
summary(lmer(MEAN ~ DATE + (1|uniqueID) + (1|DATE), data = subset(cig_data, leaf == "leafless" & new_GRtype == "grass"))) 
summary(lmer(MEAN ~ DATE + (1|uniqueID) + (1|DATE), data = subset(cig_data, leaf == "leafless" & new_GRtype == "sedum_mat"))) 
summary(lmer(MEAN ~ DATE + (1|uniqueID) + (1|DATE), data = subset(cig_data, leaf == "leafless" & new_GRtype == "woody_plants"))) 
summary(lmer(MEAN ~ DATE + (1|uniqueID) + (1|DATE), data = subset(cig_data, leaf == "leafless" & new_GRtype == "mix_grass_tree"))) 


###### Group by vegetation type: COV SAVI #######
# change order of display
savi_data$new_GRtype <- factor(savi_data$new_GRtype, levels = c("grass", "sedum_mat", "woody_plants","mix_grass_tree"))

# Plot for growing seasons 
CV_savi_vege_type_leafy.plot <- ggplot(savi_data[savi_data$leaf == "leafy",], aes(x=GR_age_of_year,y=COV, color = new_GRtype, linetype = new_GRtype))+
  geom_point(aes(shape = new_GRtype), size=3, alpha = 0.3) +
  geom_smooth(aes(color = new_GRtype, fill=new_GRtype, linetype = new_GRtype),
              method='lm', formula= y~x, alpha = 0.2, cex=1.3, se = T)+
  labs(x="Green roof age (year)", y="CV of SAVI",
       group = "new_GRtype", linetype = "new_GRtype", pch = "new_GRtype")+
  scale_color_manual(name="Vegetation type",labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"),values = c("#00BFFF", "#FF762E", "#7CAE00","#cd6ee8"))+
  scale_fill_manual(name="Vegetation type",labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"),values = c("#00BFFF", "#FF762E", "#7CAE00","#cd6ee8"))+
  scale_linetype_manual(name="Vegetation type", labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"), values = c(2,2, 2, 2))+
  scale_shape_manual(name="Vegetation type", labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"), values = c(19,17, 15, 18))+
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8))+
  theme_bw(base_size = 28)+
  theme(
    legend.position = "top",
    legend.box = "vertical",
    legend.title = element_text(size=36),
    legend.text = element_text(size = 32),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.text=element_text(size=28),
    axis.title=element_text(size=32)
  ) +
  guides(
    color = guide_legend(nrow = 2, byrow = TRUE),
    linetype = guide_legend(nrow = 2, byrow = TRUE)
  )

CV_savi_vege_type_leafy.plot


# change order of display
savi_data$new_GRtype <- factor(savi_data$new_GRtype, levels = c("grass", "sedum_mat", "woody_plants","mix_grass_tree"))

# Plot for pre-growing seasons 
CV_savi_vege_type_leafless.plot <- ggplot(savi_data[savi_data$leaf == "leafless",], aes(x=GR_age_of_year,y=COV, color = new_GRtype, linetype = new_GRtype))+
  geom_point(aes(shape = new_GRtype), size=3, alpha = 0.3) +
  geom_smooth(aes(color = new_GRtype, fill=new_GRtype, linetype = new_GRtype),
              method='lm', formula= y~x, alpha = 0.2, cex=1.3, se = T)+
  # geom_hline(yintercept=0.23, linetype="solid", color = "black",size=1)+ # control
  labs(x="Green roof age (year)", y="CV of SAVI",
       group = "new_GRtype", linetype = "new_GRtype", pch = "new_GRtype")+
  scale_color_manual(name="Vegetation type",labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"),values = c("#00BFFF", "#FF762E", "#7CAE00","#cd6ee8"))+
  scale_fill_manual(name="Vegetation type",labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"),values = c("#00BFFF", "#FF762E", "#7CAE00","#cd6ee8"))+
  scale_linetype_manual(name="Vegetation type", labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"), values = c(2,2, 1, 2))+
  scale_shape_manual(name="Vegetation type", labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"), values = c(19,17, 15, 18))+
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8))+
  theme_bw(base_size = 28)+
  theme(
    #legend.position = c(0.75, 0.85),
    legend.position = "top",
    legend.box = "vertical",
    legend.title = element_text(size=36),
    legend.text = element_text(size = 32),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.text=element_text(size=28),
    axis.title=element_text(size=32)
  ) +
  guides(
    color = guide_legend(nrow = 2, byrow = TRUE),
    linetype = guide_legend(nrow = 2, byrow = TRUE)
  )

CV_savi_vege_type_leafless.plot


### Statistics: linear mixed effect models ###
### Overall trends ###
# Include DATE and uniqueID as random factors
model_savi <- lmer(COV ~ GR_age_of_year + DATE + leaf + new_GRtype + (1|uniqueID) + (1|DATE), data = savi_data)
summary(model_savi)

# Model selection for the best predictors
selected_model_savi <- step(model_savi, direction = "both") # shows the steps: has AIC
print(selected_model_savi)

# Final model based on the selection
final_model_savi <- lmer(COV ~ GR_age_of_year + DATE + leaf + new_GRtype + (1 | uniqueID) + (1 | DATE), data = savi_data)
summary(final_model_savi)

### Trend grouped by seasons ###
## Growing seasons 
# COV SAVI vs. Green roof age 
summary(lmer(COV ~ GR_age_of_year + (1|uniqueID) + (1|DATE), data = subset(savi_data, leaf == "leafy"))) 
# COV SAVI vs. Date
summary(lmer(COV ~ DATE + (1|uniqueID) + (1|DATE), data = subset(savi_data, leaf == "leafy"))) 

## Grouped by vegetation type 
# COV SAVI vs. Green roof age 
summary(lmer(COV ~ GR_age_of_year + (1|uniqueID) + (1|DATE), data = subset(savi_data, leaf == "leafy" & new_GRtype == "grass"))) 
summary(lmer(COV ~ GR_age_of_year + (1|uniqueID) + (1|DATE), data = subset(savi_data, leaf == "leafy" & new_GRtype == "sedum_mat"))) 
summary(lmer(COV ~ GR_age_of_year + (1|uniqueID) + (1|DATE), data = subset(savi_data, leaf == "leafy" & new_GRtype == "woody_plants"))) 
summary(lmer(COV ~ GR_age_of_year + (1|uniqueID) + (1|DATE), data = subset(savi_data, leaf == "leafy" & new_GRtype == "mix_grass_tree"))) 

# COV SAVI vs. Date
summary(lmer(COV ~ DATE + (1|uniqueID) + (1|DATE), data = subset(savi_data, leaf == "leafy" & new_GRtype == "grass"))) 
summary(lmer(COV ~ DATE + (1|uniqueID) + (1|DATE), data = subset(savi_data, leaf == "leafy" & new_GRtype == "sedum_mat"))) 
summary(lmer(COV ~ DATE + (1|uniqueID) + (1|DATE), data = subset(savi_data, leaf == "leafy" & new_GRtype == "woody_plants"))) 
summary(lmer(COV ~ DATE + (1|uniqueID) + (1|DATE), data = subset(savi_data, leaf == "leafy" & new_GRtype == "mix_grass_tree"))) 


## Pre-growing seasons 
# Mean NDVI vs. Green roof age 
summary(lmer(COV ~ GR_age_of_year + (1|uniqueID) + (1|DATE), data = subset(savi_data, leaf == "leafless"))) 
summary(lmer(COV ~ DATE + (1|uniqueID) + (1|DATE), data = subset(savi_data, leaf == "leafless"))) 

## Grouped by vegetation type 
# COV SAVI vs. Green roof age 
summary(lmer(COV ~ GR_age_of_year + (1|uniqueID) + (1|DATE), data = subset(savi_data, leaf == "leafless" & new_GRtype == "grass"))) 
summary(lmer(COV ~ GR_age_of_year + (1|uniqueID) + (1|DATE), data = subset(savi_data, leaf == "leafless" & new_GRtype == "sedum_mat"))) 
summary(lmer(COV ~ GR_age_of_year + (1|uniqueID) + (1|DATE), data = subset(savi_data, leaf == "leafless" & new_GRtype == "woody_plants"))) 
summary(lmer(COV ~ GR_age_of_year + (1|uniqueID) + (1|DATE), data = subset(savi_data, leaf == "leafless" & new_GRtype == "mix_grass_tree"))) 

# COV SAVI vs. Date
summary(lmer(COV ~ DATE + (1|uniqueID) + (1|DATE), data = subset(savi_data, leaf == "leafless" & new_GRtype == "grass"))) 
summary(lmer(COV ~ DATE + (1|uniqueID) + (1|DATE), data = subset(savi_data, leaf == "leafless" & new_GRtype == "sedum_mat"))) 
summary(lmer(COV ~ DATE + (1|uniqueID) + (1|DATE), data = subset(savi_data, leaf == "leafless" & new_GRtype == "woody_plants"))) 
summary(lmer(COV ~ DATE + (1|uniqueID) + (1|DATE), data = subset(savi_data, leaf == "leafless" & new_GRtype == "mix_grass_tree"))) 


###### Group by vegetation type: COV GNDVI #######
# change order of display
gndvi_data$new_GRtype <- factor(gndvi_data$new_GRtype, levels = c("grass", "sedum_mat", "woody_plants","mix_grass_tree"))

# Plot for growing seasons 
CV_gndvi_vege_type_leafy.plot <- ggplot(gndvi_data[gndvi_data$leaf == "leafy",], aes(x=GR_age_of_year,y=COV, color = new_GRtype, linetype = new_GRtype))+
  geom_point(aes(shape = new_GRtype), size=3, alpha = 0.3) +
  geom_smooth(aes(color = new_GRtype, fill=new_GRtype, linetype = new_GRtype),
              method='lm', formula= y~x, alpha = 0.2, cex=1.3, se = T)+
  labs(x="Green roof age (year)", y="CV of GNDVI",
       group = "new_GRtype", linetype = "new_GRtype", pch = "new_GRtype")+
  scale_color_manual(name="Vegetation type",labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"),values = c("#00BFFF", "#FF762E", "#7CAE00","#cd6ee8"))+
  scale_fill_manual(name="Vegetation type",labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"),values = c("#00BFFF", "#FF762E", "#7CAE00","#cd6ee8"))+
  scale_linetype_manual(name="Vegetation type", labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"), values = c(2,1, 2, 2))+
  scale_shape_manual(name="Vegetation type", labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"), values = c(19,17, 15, 18))+
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8))+
  theme_bw(base_size = 28)+
  theme(
    legend.position = "top",
    legend.box = "vertical",
    legend.title = element_text(size=36),
    legend.text = element_text(size = 32),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.text=element_text(size=28),
    axis.title=element_text(size=32)
  ) +
  guides(
    color = guide_legend(nrow = 2, byrow = TRUE),
    linetype = guide_legend(nrow = 2, byrow = TRUE)
  )

CV_gndvi_vege_type_leafy.plot


# change order of display
gndvi_data$new_GRtype <- factor(gndvi_data$new_GRtype, levels = c("grass", "sedum_mat", "woody_plants","mix_grass_tree"))

# Plot for pre-growing seasons 
CV_gndvi_vege_type_leafless.plot <- ggplot(gndvi_data[gndvi_data$leaf == "leafless",], aes(x=GR_age_of_year,y=COV, color = new_GRtype, linetype = new_GRtype))+
  geom_point(aes(shape = new_GRtype), size=3, alpha = 0.3) +
  geom_smooth(aes(color = new_GRtype, fill=new_GRtype, linetype = new_GRtype),
              method='lm', formula= y~x, alpha = 0.2, cex=1.3, se = T)+
  labs(x="Green roof age (year)", y="CV of GNDVI",
       group = "new_GRtype", linetype = "new_GRtype", pch = "new_GRtype")+
  scale_color_manual(name="Vegetation type",labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"),values = c("#00BFFF", "#FF762E", "#7CAE00","#cd6ee8"))+
  scale_fill_manual(name="Vegetation type",labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"),values = c("#00BFFF", "#FF762E", "#7CAE00","#cd6ee8"))+
  scale_linetype_manual(name="Vegetation type", labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"), values = c(2,2, 1, 2))+
  scale_shape_manual(name="Vegetation type", labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"), values = c(19,17, 15, 18))+
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8))+
  theme_bw(base_size = 28)+
  theme(
    legend.position = "top",
    legend.box = "vertical",
    legend.title = element_text(size=36),
    legend.text = element_text(size = 32),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.text=element_text(size=28),
    axis.title=element_text(size=32)
  ) +
  guides(
    color = guide_legend(nrow = 2, byrow = TRUE),
    linetype = guide_legend(nrow = 2, byrow = TRUE)
  )

CV_gndvi_vege_type_leafless.plot


### Statistics: linear mixed effect models ###
### Overall trends ###
# Include DATE and uniqueID as random factors
model_gndvi <- lmer(COV ~ GR_age_of_year + DATE + leaf + new_GRtype + (1|uniqueID) + (1|DATE), data = gndvi_data)
summary(model_gndvi)

# Model selection for the best predictors
selected_model_gndvi <- step(model_gndvi, direction = "both") # shows the steps: has AIC
print(selected_model_gndvi)

# Final model based on the selection
final_model_gndvi <- lmer(COV ~ GR_age_of_year + DATE + leaf + new_GRtype + (1 | uniqueID) + (1 | DATE), data = gndvi_data)
summary(final_model_gndvi)

### Trend grouped by seasons ###
## Growing seasons 
# COV GNDVI vs. Green roof age 
summary(lmer(COV ~ GR_age_of_year + (1|uniqueID) + (1|DATE), data = subset(gndvi_data, leaf == "leafy"))) 
# COV GNDVI vs. Date
summary(lmer(COV ~ DATE + (1|uniqueID) + (1|DATE), data = subset(gndvi_data, leaf == "leafy"))) 

## Grouped by vegetation type 
# COV GNDVI vs. Green roof age 
summary(lmer(COV ~ GR_age_of_year + (1|uniqueID) + (1|DATE), data = subset(gndvi_data, leaf == "leafy" & new_GRtype == "grass"))) 
summary(lmer(COV ~ GR_age_of_year + (1|uniqueID) + (1|DATE), data = subset(gndvi_data, leaf == "leafy" & new_GRtype == "sedum_mat"))) 
summary(lmer(COV ~ GR_age_of_year + (1|uniqueID) + (1|DATE), data = subset(gndvi_data, leaf == "leafy" & new_GRtype == "woody_plants"))) 
summary(lmer(COV ~ GR_age_of_year + (1|uniqueID) + (1|DATE), data = subset(gndvi_data, leaf == "leafy" & new_GRtype == "mix_grass_tree"))) 

# COV GNDVI vs. Date
summary(lmer(COV ~ DATE + (1|uniqueID) + (1|DATE), data = subset(gndvi_data, leaf == "leafy" & new_GRtype == "grass"))) 
summary(lmer(COV ~ DATE + (1|uniqueID) + (1|DATE), data = subset(gndvi_data, leaf == "leafy" & new_GRtype == "sedum_mat"))) 
summary(lmer(COV ~ DATE + (1|uniqueID) + (1|DATE), data = subset(gndvi_data, leaf == "leafy" & new_GRtype == "woody_plants"))) 
summary(lmer(COV ~ DATE + (1|uniqueID) + (1|DATE), data = subset(gndvi_data, leaf == "leafy" & new_GRtype == "mix_grass_tree"))) 


## Pre-growing seasons 
# COV GNDVI vs. Green roof age
summary(lmer(COV ~ GR_age_of_year + (1|uniqueID) + (1|DATE), data = subset(gndvi_data, leaf == "leafless"))) 
# COV GNDVI vs. Date
summary(lmer(COV ~ DATE + (1|uniqueID) + (1|DATE), data = subset(gndvi_data, leaf == "leafless"))) 

## Grouped by vegetation type 
# COV GNDVI vs. Green roof age 
summary(lmer(COV ~ GR_age_of_year + (1|uniqueID) + (1|DATE), data = subset(gndvi_data, leaf == "leafless" & new_GRtype == "grass"))) 
summary(lmer(COV ~ GR_age_of_year + (1|uniqueID) + (1|DATE), data = subset(gndvi_data, leaf == "leafless" & new_GRtype == "sedum_mat"))) 
summary(lmer(COV ~ GR_age_of_year + (1|uniqueID) + (1|DATE), data = subset(gndvi_data, leaf == "leafless" & new_GRtype == "woody_plants"))) 
summary(lmer(COV ~ GR_age_of_year + (1|uniqueID) + (1|DATE), data = subset(gndvi_data, leaf == "leafless" & new_GRtype == "mix_grass_tree"))) 

# COV GNDVI vs. Date
summary(lmer(COV ~ DATE + (1|uniqueID) + (1|DATE), data = subset(gndvi_data, leaf == "leafless" & new_GRtype == "grass"))) 
summary(lmer(COV ~ DATE + (1|uniqueID) + (1|DATE), data = subset(gndvi_data, leaf == "leafless" & new_GRtype == "sedum_mat"))) 
summary(lmer(COV ~ DATE + (1|uniqueID) + (1|DATE), data = subset(gndvi_data, leaf == "leafless" & new_GRtype == "woody_plants"))) 
summary(lmer(COV ~ DATE + (1|uniqueID) + (1|DATE), data = subset(gndvi_data, leaf == "leafless" & new_GRtype == "mix_grass_tree"))) 


###### Group by vegetation type: COV CIg #######
# change order of display
cig_data$new_GRtype <- factor(cig_data$new_GRtype, levels = c("grass", "sedum_mat", "woody_plants","mix_grass_tree"))

# Plot for growing seasons 
CV_cig_vege_type_leafy.plot <- ggplot(cig_data[cig_data$leaf == "leafy",], aes(x=GR_age_of_year,y=COV, color = new_GRtype, linetype = new_GRtype))+
  geom_point(aes(shape = new_GRtype), size=3, alpha = 0.3) +
  geom_smooth(aes(color = new_GRtype, fill=new_GRtype, linetype = new_GRtype),
              method='lm', formula= y~x, alpha = 0.2, cex=1.3, se = T)+
  labs(x="Green roof age (year)", y="CV of CIg",
       group = "new_GRtype", linetype = "new_GRtype", pch = "new_GRtype")+
  scale_color_manual(name="Vegetation type",labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"),values = c("#00BFFF", "#FF762E", "#7CAE00","#cd6ee8"))+
  scale_fill_manual(name="Vegetation type",labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"),values = c("#00BFFF", "#FF762E", "#7CAE00","#cd6ee8"))+
  scale_linetype_manual(name="Vegetation type", labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"), values = c(2,2, 2, 2))+
  scale_shape_manual(name="Vegetation type", labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"), values = c(19,17, 15, 18))+
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8))+
  theme_bw(base_size = 28)+
  theme(
    legend.position = "top",
    legend.box = "vertical",
    legend.title = element_text(size=36),
    legend.text = element_text(size = 32),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.text=element_text(size=28),
    axis.title=element_text(size=32)
  ) +
  guides(
    color = guide_legend(nrow = 2, byrow = TRUE),
    linetype = guide_legend(nrow = 2, byrow = TRUE)
  )

CV_cig_vege_type_leafy.plot


# change order of display
cig_data$new_GRtype <- factor(cig_data$new_GRtype, levels = c("grass", "sedum_mat", "woody_plants","mix_grass_tree"))

# Plot for pre-growing seasons 
CV_cig_vege_type_leafless.plot <- ggplot(cig_data[cig_data$leaf == "leafless",], aes(x=GR_age_of_year,y=COV, color = new_GRtype, linetype = new_GRtype))+
  geom_point(aes(shape = new_GRtype), size=3, alpha = 0.3) +
  geom_smooth(aes(color = new_GRtype, fill=new_GRtype, linetype = new_GRtype),
              method='lm', formula= y~x, alpha = 0.2, cex=1.3, se = T)+
  labs(x="Green roof age (year)", y="CV of CIg",
       group = "new_GRtype", linetype = "new_GRtype", pch = "new_GRtype")+
  scale_color_manual(name="Vegetation type",labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"),values = c("#00BFFF", "#FF762E", "#7CAE00","#cd6ee8"))+
  scale_fill_manual(name="Vegetation type",labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"),values = c("#00BFFF", "#FF762E", "#7CAE00","#cd6ee8"))+
  scale_linetype_manual(name="Vegetation type", labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"), values = c(2,2, 1, 2))+
  scale_shape_manual(name="Vegetation type", labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"), values = c(19,17, 15, 18))+
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8))+
  theme_bw(base_size = 28)+
  theme(
    legend.position = "top",
    legend.box = "vertical",
    legend.title = element_text(size=36),
    legend.text = element_text(size = 32),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.text=element_text(size=28),
    axis.title=element_text(size=32)
  ) +
  guides(
    color = guide_legend(nrow = 2, byrow = TRUE),
    linetype = guide_legend(nrow = 2, byrow = TRUE)
  )

CV_cig_vege_type_leafless.plot


### Statistics: linear mixed effect models ###
### Overall trends ###
# Include DATE and uniqueID as random factors
model_cig <- lmer(COV ~ GR_age_of_year + DATE + leaf + new_GRtype + (1|uniqueID) + (1|DATE), data = cig_data)
summary(model_cig)

# Model selection for the best predictors
selected_model_cig <- step(model_gndvi, direction = "both") # shows the steps: has AIC
print(selected_model_cig)

# Final model based on the selection
final_model_cig <- lmer(COV ~ GR_age_of_year + DATE + leaf + new_GRtype + (1 | uniqueID) + (1 | DATE), data = cig_data)
summary(final_model_cig)

### Trend grouped by seasons ###
## Growing seasons 
# COV CIg vs. Green roof age 
summary(lmer(COV ~ GR_age_of_year + (1|uniqueID) + (1|DATE), data = subset(cig_data, leaf == "leafy"))) 
# COV CIg vs. Date
summary(lmer(COV ~ DATE + (1|uniqueID) + (1|DATE), data = subset(cig_data, leaf == "leafy"))) 

## Grouped by vegetation type 
# COV CIg vs. Green roof age 
summary(lmer(COV ~ GR_age_of_year + (1|uniqueID) + (1|DATE), data = subset(cig_data, leaf == "leafy" & new_GRtype == "grass"))) 
summary(lmer(COV ~ GR_age_of_year + (1|uniqueID) + (1|DATE), data = subset(cig_data, leaf == "leafy" & new_GRtype == "sedum_mat"))) 
summary(lmer(COV ~ GR_age_of_year + (1|uniqueID) + (1|DATE), data = subset(cig_data, leaf == "leafy" & new_GRtype == "woody_plants"))) 
summary(lmer(COV ~ GR_age_of_year + (1|uniqueID) + (1|DATE), data = subset(cig_data, leaf == "leafy" & new_GRtype == "mix_grass_tree"))) 

# COV CIg vs. Date
summary(lmer(COV ~ DATE + (1|uniqueID) + (1|DATE), data = subset(cig_data, leaf == "leafy" & new_GRtype == "grass"))) 
summary(lmer(COV ~ DATE + (1|uniqueID) + (1|DATE), data = subset(cig_data, leaf == "leafy" & new_GRtype == "sedum_mat"))) 
summary(lmer(COV ~ DATE + (1|uniqueID) + (1|DATE), data = subset(cig_data, leaf == "leafy" & new_GRtype == "woody_plants"))) 
summary(lmer(COV ~ DATE + (1|uniqueID) + (1|DATE), data = subset(cig_data, leaf == "leafy" & new_GRtype == "mix_grass_tree"))) 


## Pre-growing seasons 
# COV CIg vs. Green roof age 
summary(lmer(COV ~ GR_age_of_year + (1|uniqueID) + (1|DATE), data = subset(cig_data, leaf == "leafless"))) 
# COV CIg vs. Date
summary(lmer(COV ~ DATE + (1|uniqueID) + (1|DATE), data = subset(cig_data, leaf == "leafless"))) 

## Grouped by vegetation type 
# COV CIg vs. Green roof age  
summary(lmer(COV ~ GR_age_of_year + (1|uniqueID) + (1|DATE), data = subset(cig_data, leaf == "leafless" & new_GRtype == "grass"))) 
summary(lmer(COV ~ GR_age_of_year + (1|uniqueID) + (1|DATE), data = subset(cig_data, leaf == "leafless" & new_GRtype == "sedum_mat"))) 
summary(lmer(COV ~ GR_age_of_year + (1|uniqueID) + (1|DATE), data = subset(cig_data, leaf == "leafless" & new_GRtype == "woody_plants"))) 
summary(lmer(COV ~ GR_age_of_year + (1|uniqueID) + (1|DATE), data = subset(cig_data, leaf == "leafless" & new_GRtype == "mix_grass_tree"))) 

# COV CIg vs. Date
summary(lmer(COV ~ DATE + (1|uniqueID) + (1|DATE), data = subset(cig_data, leaf == "leafless" & new_GRtype == "grass"))) 
summary(lmer(COV ~ DATE + (1|uniqueID) + (1|DATE), data = subset(cig_data, leaf == "leafless" & new_GRtype == "sedum_mat"))) 
summary(lmer(COV ~ DATE + (1|uniqueID) + (1|DATE), data = subset(cig_data, leaf == "leafless" & new_GRtype == "woody_plants"))) 
summary(lmer(COV ~ DATE + (1|uniqueID) + (1|DATE), data = subset(cig_data, leaf == "leafless" & new_GRtype == "mix_grass_tree"))) 


###### Combine SI Figure S5: Group by GR Type: Mean VIs and CV VIs ######
merge_VIs_Vegetation_type = mean_savi_vege_type_leafy.plot + mean_savi_vege_type_leafless.plot + 
  mean_gndvi_vege_type_leafy.plot + mean_gndvi_vege_type_leafless.plot + 
  mean_cig_vege_type_leafy.plot + mean_cig_vege_type_leafless.plot + 
  CV_savi_vege_type_leafy.plot + CV_savi_vege_type_leafless.plot +
  CV_gndvi_vege_type_leafy.plot + CV_gndvi_vege_type_leafless.plot +
  CV_cig_vege_type_leafy.plot + CV_cig_vege_type_leafless.plot +
  plot_layout(guides = "collect", ncol = 2) & theme(legend.position = "top")

merge_VIs_Vegetation_type[[1]] = merge_VIs_Vegetation_type[[1]] + theme(axis.text.x = element_blank(),axis.title.x = element_blank(),legend.position = "none")
merge_VIs_Vegetation_type[[2]] = merge_VIs_Vegetation_type[[2]] + theme(axis.text.x = element_blank(),axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position = "none")
merge_VIs_Vegetation_type[[3]] = merge_VIs_Vegetation_type[[3]] + theme(axis.text.x = element_blank(),axis.title.x = element_blank(),legend.position = "none")
merge_VIs_Vegetation_type[[4]] = merge_VIs_Vegetation_type[[4]] + theme(axis.text.x = element_blank(),axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position = "none")
merge_VIs_Vegetation_type[[5]] = merge_VIs_Vegetation_type[[5]] + theme(axis.text.x = element_blank(),axis.title.x = element_blank(),legend.position = "none")
merge_VIs_Vegetation_type[[6]] = merge_VIs_Vegetation_type[[6]] + theme(axis.text.x = element_blank(),axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position = "none")
merge_VIs_Vegetation_type[[7]] = merge_VIs_Vegetation_type[[7]] + theme(axis.text.x = element_blank(),axis.title.x = element_blank(),legend.position = "none")
merge_VIs_Vegetation_type[[8]] = merge_VIs_Vegetation_type[[8]] + theme(axis.text.x = element_blank(),axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position = "none")
merge_VIs_Vegetation_type[[9]] = merge_VIs_Vegetation_type[[9]] + theme(axis.text.x = element_blank(),axis.title.x = element_blank(),legend.position = "none")
merge_VIs_Vegetation_type[[10]] = merge_VIs_Vegetation_type[[10]] + theme(axis.text.x = element_blank(),axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position = "none")
merge_VIs_Vegetation_type[[11]] = merge_VIs_Vegetation_type[[11]] + theme(legend.position = "none")
merge_VIs_Vegetation_type[[12]] = merge_VIs_Vegetation_type[[12]] + theme(axis.title.y = element_blank())

merge_VIs_Vegetation_type

# ggsave(plot=merge_VIs_Vegetation_type, filename = "path/Fig_VIsAge_Vegetation_type.tif", width = 13, height = 20, device='tiff', dpi=600,unit="in")



