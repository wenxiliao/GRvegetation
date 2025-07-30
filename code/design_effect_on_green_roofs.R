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
# install.packages("~/data/AlgDesign_1.2.1.1.tar.gz", repos = NULL, type = "source") # package for agricolae
# install.packages("~/data/agricolae_1.3-7.tar.gz", repos = NULL, type = "source") # package for agricolae

# Packages --------------
library(ggplot2)
library(dplyr)
library(openxlsx)
library(readxl)
library(lmtest)
library(MASS)
library(lme4)
library(segmented) 
library(patchwork)
library(lmerTest)
library(agricolae)

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
ndvi_data <- read_excel('~/data/ndvi_data.xlsx', sheet = "Sheet 1")
savi_data <- read_excel('~/data/savi_data.xlsx', sheet = "Sheet 1")
gndvi_data <- read_excel('~/data/gndvi_data.xlsx', sheet = "Sheet 1")
cig_data <- read_excel('~/data/cig_data.xlsx', sheet = "Sheet 1")

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



# Subset 2018 files for NDVI (pre-growing season) -------------------------
# Subset green roofs in 2018
GR_ndvi_2018 = subset(ndvi_data, year == 2018)

# Transform the roof module area
GR_ndvi_2018$sqrt_area = sqrt(GR_ndvi_2018$SHAPE_Area)

# Transform roof module aspect ratio
GR_ndvi_2018$sqrt_AR = sqrt(GR_ndvi_2018$AspectRatio)


# Subset 2017 files for NDVI (growing season) -----------------------------
# Subset green roofs in 2017
GR_ndvi_2017 = subset(ndvi_data, year == 2017)

# Transform the roof module area
GR_ndvi_2017$sqrt_area = sqrt(GR_ndvi_2017$SHAPE_Area)

# Transform roof module aspect ratio
GR_ndvi_2017$sqrt_AR = sqrt(GR_ndvi_2017$AspectRatio)



# NDVI: Linear model to select key design factors that influence mean NDVI --------
### 2018 (pre-growing season) ###
# model selection
model_ndvi_test_2018_GRtype <- lm(MEAN~SHAPE_Area + GR_type + MAX_HEIGHT + AspectRatio, data=na.omit(GR_ndvi_2018))
model_ndvi_test_2018_VegeType <- lm(MEAN~SHAPE_Area + new_GRtype + MAX_HEIGHT + AspectRatio, data=na.omit(GR_ndvi_2018))

selected_model_ndvi_2018_GRtype <- step(model_ndvi_test_2018_GRtype, direction = "both") 
selected_model_ndvi_2018_VegeType <- step(model_ndvi_test_2018_VegeType, direction = "both") 

# final model
summary(selected_model_ndvi_2018_GRtype)
summary(selected_model_ndvi_2018_VegeType)

### 2017 (growing season) ###
# model selection
model_ndvi_test_2017_GRtype <- lm(MEAN~SHAPE_Area + GR_type + MAX_HEIGHT + AspectRatio, data=GR_ndvi_2017)
model_ndvi_test_2017_VegeType <- lm(MEAN~SHAPE_Area + new_GRtype + MAX_HEIGHT + AspectRatio, data=GR_ndvi_2017)

selected_model_ndvi_2017_GRtype <- step(model_ndvi_test_2017_GRtype, direction = "both") 
selected_model_ndvi_2017_VegeType <- step(model_ndvi_test_2017_VegeType, direction = "both") 

# final model
summary(selected_model_ndvi_2017_GRtype)
summary(selected_model_ndvi_2017_VegeType)

# NDVI: Linear model to select key design factors that influence COV of NDVI --------
### 2018 (pre-growing season) ###
# model selection
model_CV_ndvi_test_2018_GRtype <- lm(COV~SHAPE_Area + GR_type + MAX_HEIGHT + AspectRatio, data=na.omit(GR_ndvi_2018))
model_CV_ndvi_test_2018_VegeType <- lm(COV~SHAPE_Area + new_GRtype + MAX_HEIGHT + AspectRatio, data=na.omit(GR_ndvi_2018))

selected_model_CV_ndvi_2018_GRtype <- step(model_CV_ndvi_test_2018_GRtype, direction = "both")
selected_model_CV_ndvi_2018_VegeType <- step(model_CV_ndvi_test_2018_VegeType, direction = "both")

# final model 
summary(selected_model_CV_ndvi_2018_GRtype)
summary(selected_model_CV_ndvi_2018_VegeType)

### 2017 (growing season) ###
# model selection
model_CV_ndvi_test_2017_GRtype <- lm(COV~SHAPE_Area + GR_type + MAX_HEIGHT + AspectRatio, data=GR_ndvi_2017)
model_CV_ndvi_test_2017_VegeType <- lm(COV~SHAPE_Area + new_GRtype + MAX_HEIGHT + AspectRatio, data=GR_ndvi_2017)

selected_model_CV_ndvi_2017_GRtype <- step(model_CV_ndvi_test_2017_GRtype, direction = "both")
selected_model_CV_ndvi_2017_VegeType <- step(model_CV_ndvi_test_2017_VegeType, direction = "both")

# final model
summary(selected_model_CV_ndvi_2017_GRtype)
summary(selected_model_CV_ndvi_2017_VegeType)



# Figure 2: (a) Mean NDVI vs. SQRT area (grouped by green roof type)  --------
###### 2018 (pre-growing season) ######
# Change the order of legend display
GR_ndvi_2018$GR_type <- factor(GR_ndvi_2018$GR_type, levels = c("ext","int"))

# Plot
ndvi_area_2018.plot <- ggplot(GR_ndvi_2018, aes(x=sqrt_area,y=MEAN, color = GR_type, linetype = GR_type))+
  geom_point(aes(shape = GR_type), size=2.5, alpha = 0.3) +
  geom_smooth(aes(color = GR_type, fill=GR_type, linetype = GR_type),
              method='lm', formula= y~x, alpha = 0.3, cex=1.5)+
  labs(x=expression(sqrt("Green roof area")~(m)), y="Mean NDVI",
       group = "GR_type", linetype = "GR_type", pch = "GR_type")+
  scale_color_manual(name="Green roof type",labels = c("Extensive","Intensive"),values = c("#FC7307","#236AB9"))+
  scale_fill_manual(name="Green roof type",labels = c("Extensive","Intensive"),values = c("#FC7307","#236AB9"))+
  scale_linetype_manual(name="Green roof type", labels = c("Extensive","Intensive"), values = c(1,1))+
  scale_shape_manual(name="Green roof type", labels = c("Extensive","Intensive"), values = c(19,17))+
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

ndvi_area_2018.plot

# Linear regression
summary(lm(MEAN ~ sqrt_area, data = GR_ndvi_2018))

# Linear regression grouped by green roof type
summary(lm(MEAN ~ sqrt_area, data = subset(GR_ndvi_2018, GR_type =="int")))
summary(lm(MEAN ~ sqrt_area, data = subset(GR_ndvi_2018, GR_type =="ext")))


###### 2017 (growing season) ######
# Change the order of legend display
GR_ndvi_2017$GR_type <- factor(GR_ndvi_2017$GR_type, levels = c("ext","int"))

# Plot
ndvi_area_2017.plot <- ggplot(GR_ndvi_2017, aes(x=sqrt_area,y=MEAN, color = GR_type, linetype = GR_type))+
  geom_point(aes(shape = GR_type), size=2.5, alpha = 0.3) +
  geom_smooth(aes(color = GR_type, fill=GR_type, linetype = GR_type),
              method='lm', formula= y~x, alpha = 0.3, cex=1.5)+
  labs(x=expression(sqrt("Green roof area")~(m)), y="Mean NDVI",
       group = "GR_type", linetype = "GR_type", pch = "GR_type")+
  scale_color_manual(name="Green roof type",labels = c("Extensive","Intensive"),values = c("#FC7307","#236AB9"))+
  scale_fill_manual(name="Green roof type",labels = c("Extensive","Intensive"),values = c("#FC7307","#236AB9"))+
  scale_linetype_manual(name="Green roof type", labels = c("Extensive","Intensive"), values = c(1,1))+
  scale_shape_manual(name="Green roof type", labels = c("Extensive","Intensive"), values = c(19,17))+
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

ndvi_area_2017.plot

# Linear regression
summary(lm(MEAN ~ sqrt_area, data = GR_ndvi_2017))

# Linear regression grouped by green roof type
summary(lm(MEAN ~ sqrt_area, data = subset(GR_ndvi_2017, GR_type =="int")))
summary(lm(MEAN ~ sqrt_area, data = subset(GR_ndvi_2017, GR_type =="ext")))


# Figure 2: (b) Mean NDVI vs. Building height (grouped by green roof type) --------
###### 2018 (pre-growing season)  ######
# Change the order of legend display
GR_ndvi_2018$GR_type <- factor(GR_ndvi_2018$GR_type, levels = c("ext","int"))

# Plot
ndvi_height_2018.plot <- ggplot(GR_ndvi_2018, aes(x=MAX_HEIGHT,y=MEAN, color = GR_type, linetype = GR_type))+
  geom_point(aes(shape = GR_type), size=2.5, alpha = 0.3) +
  geom_smooth(aes(color = GR_type, fill=GR_type, linetype = GR_type),
              method='lm', formula= y~x, alpha = 0.3, cex=1.5)+
  labs(x="Building height (m)", y="Mean NDVI",
       group = "GR_type", linetype = "GR_type", pch = "GR_type")+
  scale_color_manual(name="Green roof type",labels = c("Extensive","Intensive"),values = c("#FC7307","#236AB9"))+
  scale_fill_manual(name="Green roof type",labels = c("Extensive","Intensive"),values = c("#FC7307","#236AB9"))+
  scale_linetype_manual(name="Green roof type", labels = c("Extensive","Intensive"), values = c(1,1))+
  scale_shape_manual(name="Green roof type", labels = c("Extensive","Intensive"), values = c(19,17))+
  scale_x_continuous(limits = c(0, 180))+
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

ndvi_height_2018.plot

# Linear regression
summary(lm(MEAN ~ MAX_HEIGHT, data = GR_ndvi_2018))

# Linear regression grouped by green roof type
summary(lm(MEAN ~ MAX_HEIGHT, data = subset(GR_ndvi_2018, GR_type =="int")))
summary(lm(MEAN ~ MAX_HEIGHT, data = subset(GR_ndvi_2018, GR_type =="ext")))


###### 2017 (growing season)  ######
# Change the order of legend display
GR_ndvi_2017$GR_type <- factor(GR_ndvi_2017$GR_type, levels = c("ext","int"))

# Plot
ndvi_height_2017.plot <- ggplot(GR_ndvi_2017, aes(x=MAX_HEIGHT,y=MEAN, color = GR_type, linetype = GR_type))+
  geom_point(aes(shape = GR_type), size=2.5, alpha = 0.3) +
  geom_smooth(aes(color = GR_type, fill=GR_type, linetype = GR_type),
              method='lm', formula= y~x, alpha = 0.3, cex=1.5)+
  labs(x="Building height (m)", y="Mean NDVI",
       group = "GR_type", linetype = "GR_type", pch = "GR_type")+
  scale_color_manual(name="Green roof type",labels = c("Extensive","Intensive"),values = c("#FC7307","#236AB9"))+
  scale_fill_manual(name="Green roof type",labels = c("Extensive","Intensive"),values = c("#FC7307","#236AB9"))+
  scale_linetype_manual(name="Green roof type", labels = c("Extensive","Intensive"), values = c(1,1))+
  scale_shape_manual(name="Green roof type", labels = c("Extensive","Intensive"), values = c(19,17))+
  scale_x_continuous(limits = c(0, 180))+
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

ndvi_height_2017.plot

# Linear regression
summary(lm(MEAN ~ MAX_HEIGHT, data = GR_ndvi_2017))

# Linear regression grouped by green roof type
summary(lm(MEAN ~ MAX_HEIGHT, data = subset(GR_ndvi_2017, GR_type =="int")))
summary(lm(MEAN ~ MAX_HEIGHT, data = subset(GR_ndvi_2017, GR_type =="ext")))


# Figure 2: (c) Mean NDVI vs. Green roof type --------
###### 2018 (pre-growing season)  ######
# Summarize data
summary_GR_ndvi_2018 <- GR_ndvi_2018 %>%
  group_by(GR_type) %>%
  summarise(mean_NDVI = mean(MEAN, na.rm = TRUE),
            se_NDVI = get_se(MEAN))

# Change the level order
summary_GR_ndvi_2018$GR_type <- factor(summary_GR_ndvi_2018$GR_type, levels = c("ext","int"))

# Plot
ndvi_GRtype_barplot_2018 <- ggplot(summary_GR_ndvi_2018, aes(x = GR_type, y = mean_NDVI, fill = GR_type)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin = mean_NDVI - se_NDVI, ymax = mean_NDVI + se_NDVI),
                position = position_dodge(0.9), width = 0.3) +
  scale_x_discrete(name="Green roof type",labels = c("Extensive", "Intensive"))+
  scale_fill_manual(name="Green roof type",labels = c("Extensive", "Intensive"),values = c("#FC7307","#236AB9"))+
  labs(x = "Green roof type", y = "Mean NDVI",
       group = "GR_type")+
  coord_cartesian(ylim=c(0,0.2))+
  theme_bw(base_size = 28) + 
  theme(
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text=element_text(size=28, color = "black"),
    axis.title=element_text(size=32, color = "black"),
    legend.position = "none",
    legend.title = element_text(size = 32)
  )

print(ndvi_GRtype_barplot_2018)

# t-test
t.test(MEAN ~ GR_type, data = GR_ndvi_2018, var.equal = TRUE)


###### 2017 (growing season)  ######
# Summarize data
summary_GR_ndvi_2017 <- GR_ndvi_2017 %>%
  group_by(GR_type) %>%
  summarise(mean_NDVI = mean(MEAN, na.rm = TRUE),
            se_NDVI = get_se(MEAN))

# Change the level order
summary_GR_ndvi_2017$GR_type <- factor(summary_GR_ndvi_2017$GR_type, levels = c("ext","int"))

# Plot
ndvi_GRtype_barplot_2017 <- ggplot(summary_GR_ndvi_2017, aes(x = GR_type, y = mean_NDVI, fill = GR_type)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin = mean_NDVI - se_NDVI, ymax = mean_NDVI + se_NDVI),
                position = position_dodge(0.9), width = 0.3) +
  scale_x_discrete(name="Green roof type",labels = c("Extensive", "Intensive"))+
  scale_fill_manual(name="Green roof type",labels = c("Extensive", "Intensive"),values = c("#FC7307","#236AB9"))+
  labs(x = "Green roof type", y = "Mean NDVI",
       group = "GR_type")+
  coord_cartesian(ylim=c(0,0.25))+
  theme_bw(base_size = 28) + 
  theme(
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text=element_text(size=28, color = "black"),
    axis.title=element_text(size=32, color = "black"),
    legend.position = "none",
    legend.title = element_text(size = 32)
  )

print(ndvi_GRtype_barplot_2017)

# t-test
t.test(MEAN ~ GR_type, data = GR_ndvi_2017, var.equal = TRUE)


# Figure 2: (d) Mean NDVI vs. SQRT Aspect ratio (grouped by green roof type) --------
###### 2018 (pre-growing season)  ######
# Change order of display
GR_ndvi_2018$GR_type <- factor(GR_ndvi_2018$GR_type, levels = c("ext", "int"))

# Plot
ndvi_sqrtAR_GRtype_2018.plot <- ggplot(GR_ndvi_2018, aes(x=sqrt_AR,y=MEAN, color = GR_type, linetype = GR_type))+
  geom_point(aes(shape = GR_type), size=2.5, alpha = 0.3) +
  geom_smooth(aes(color = GR_type, fill=GR_type, linetype = GR_type),
              method='lm', formula= y~x, alpha = 0.3, cex=1.5)+
  labs(x=expression(sqrt("Aspect ratio")), y="Mean NDVI",
       group = "GR_type", linetype = "GR_type", pch = "GR_type")+
  scale_color_manual(name="Green roof type",labels = c("Extensive", "Intensive"),values = c("#FC7307","#236AB9"))+
  scale_fill_manual(name="Green roof type",labels = c("Extensive", "Intensive"),values = c("#FC7307","#236AB9"))+
  scale_linetype_manual(name="Green roof type", labels = c("Extensive", "Intensive"), values = c(1,1))+
  scale_shape_manual(name="Green roof type", labels = c("Extensive", "Intensive"), values = c(19,17)) +
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

ndvi_sqrtAR_GRtype_2018.plot

# Linear regression
summary(lm(MEAN ~ sqrt_AR, data = GR_ndvi_2018))

# Linear regression grouped by green roof type
summary(lm(MEAN ~ sqrt_AR, data = subset(GR_ndvi_2018, GR_type =="int")))
summary(lm(MEAN ~ sqrt_AR, data = subset(GR_ndvi_2018, GR_type =="ext")))


###### 2017 (growing season)  ######
# change order of display
GR_ndvi_2017$GR_type <- factor(GR_ndvi_2017$GR_type, levels = c("ext", "int"))

# Plot
ndvi_sqrtAR_GRtype_2017.plot <- ggplot(GR_ndvi_2017, aes(x=sqrt_AR,y=MEAN, color = GR_type, linetype = GR_type))+
  geom_point(aes(shape = GR_type), size=2.5, alpha = 0.3) +
  geom_smooth(aes(color = GR_type, fill=GR_type, linetype = GR_type),
              method='lm', formula= y~x, alpha = 0.3, cex=1.5)+
  labs(x=expression(sqrt("Aspect ratio")), y="Mean NDVI",
       group = "GR_type", linetype = "GR_type", pch = "GR_type")+
  scale_color_manual(name="Green roof type",labels = c("Extensive", "Intensive"),values = c("#FC7307","#236AB9"))+
  scale_fill_manual(name="Green roof type",labels = c("Extensive", "Intensive"),values = c("#FC7307","#236AB9"))+
  scale_linetype_manual(name="Green roof type", labels = c("Extensive", "Intensive"), values = c(2,1))+
  scale_shape_manual(name="Green roof type", labels = c("Extensive", "Intensive"), values = c(19,17)) +
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

ndvi_sqrtAR_GRtype_2017.plot

# Linear regression
summary(lm(MEAN ~ sqrt_AR, data = GR_ndvi_2017))

# Linear regression grouped by green roof type
summary(lm(MEAN ~ sqrt_AR, data = subset(GR_ndvi_2017, GR_type =="int")))
summary(lm(MEAN ~ sqrt_AR, data = subset(GR_ndvi_2017, GR_type =="ext")))


# Combine Figure 2 grouped by green roof type (growing season) -------------
merge_mean_NDVI_design_2017 = ndvi_area_2017.plot + ndvi_height_2017.plot +
  ndvi_GRtype_barplot_2017 + ndvi_sqrtAR_GRtype_2017.plot +
  plot_layout(guides = "collect", ncol = 2) & theme(legend.position = "top")

merge_mean_NDVI_design_2017[[1]] = merge_mean_NDVI_design_2017[[1]] + theme(legend.position = "none")
merge_mean_NDVI_design_2017[[2]] = merge_mean_NDVI_design_2017[[2]] + theme(legend.position = "none", axis.title.y = element_blank())
merge_mean_NDVI_design_2017[[3]] = merge_mean_NDVI_design_2017[[3]] + theme(legend.position = "none")
merge_mean_NDVI_design_2017[[4]] = merge_mean_NDVI_design_2017[[4]] + theme(axis.title.y = element_blank())

merge_mean_NDVI_design_2017

# ggsave(plot=merge_mean_NDVI_design_2017, filename = "path/NDVI_DesignFactors_GRtype_2017.tif", width = 12, height = 10, device='tiff', dpi=600,unit="in")


# Combine Supplementary Information Figure S6 grouped by green roof type (pre-growing season) -----------------
merge_mean_NDVI_design_2018 = ndvi_area_2018.plot + ndvi_height_2018.plot +
  ndvi_GRtype_barplot_2018 + ndvi_sqrtAR_GRtype_2018.plot +
  plot_layout(guides = "collect", ncol = 2) & theme(legend.position = "top")

merge_mean_NDVI_design_2018[[1]] = merge_mean_NDVI_design_2018[[1]] + theme(legend.position = "none")
merge_mean_NDVI_design_2018[[2]] = merge_mean_NDVI_design_2018[[2]] + theme(legend.position = "none", axis.title.y = element_blank())
merge_mean_NDVI_design_2018[[3]] = merge_mean_NDVI_design_2018[[3]] + theme(legend.position = "none")
merge_mean_NDVI_design_2018[[4]] = merge_mean_NDVI_design_2018[[4]] + theme(axis.title.y = element_blank())

merge_mean_NDVI_design_2018

# ggsave(plot=merge_mean_NDVI_design_2018, filename = "path/NDVI_DesignFactors_GRtype_2018.tif", width = 12, height = 10, device='tiff', dpi=600,unit="in")



# Supplementary Information Figure S7: Mean NDVI vs. SQRT area (grouped by vegetation type) -------------------
###### 2018 (pre-growing season) ######
# Change the level order
GR_ndvi_2018$new_GRtype <- factor(GR_ndvi_2018$new_GRtype, levels = c("grass", "sedum_mat", "woody_plants","mix_grass_tree"))

# Plot
ndvi_area_2018_vegetype.plot <- ggplot(GR_ndvi_2018, aes(x=sqrt_area,y=MEAN, color = new_GRtype, linetype = new_GRtype))+
  geom_point(aes(shape = new_GRtype), size=2.5, alpha = 0.2) +
  geom_smooth(aes(color = new_GRtype, fill=new_GRtype, linetype = new_GRtype),
              method='lm', formula= y~x, alpha = 0.2, cex=1.5)+
  # geom_smooth(aes(color = GR_type, fill = GR_type), method="lm", formula=y ~ log(x + 1e-6), se=TRUE, alpha = 0.3) +
  labs(x=expression(sqrt("Green roof area")~(m)), y="Mean NDVI",
       group = "new_GRtype", linetype = "new_GRtype", pch = "new_GRtype")+
  scale_color_manual(name="Vegetation type",labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"),values = c("#00BFFF", "#FF762E", "#7CAE00","#cd6ee8"))+
  scale_fill_manual(name="Vegetation type",labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"),values = c("#00BFFF", "#FF762E", "#7CAE00","#cd6ee8"))+
  scale_shape_manual(name="Vegetation type", labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"), values = c(19,17, 15, 18))+
  scale_linetype_manual(name="Vegetation type", labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"), values = c(1,1,1,2))+
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
  )+
  guides(
    color = guide_legend(nrow = 2, byrow = TRUE),
    linetype = guide_legend(nrow = 2, byrow = TRUE)
  )

ndvi_area_2018_vegetype.plot

# Linear regression grouped by vegetation type
summary(lm(MEAN ~ sqrt_area, data = subset(GR_ndvi_2018, new_GRtype =="grass")))
summary(lm(MEAN ~ sqrt_area, data = subset(GR_ndvi_2018, new_GRtype =="sedum_mat")))
summary(lm(MEAN ~ sqrt_area, data = subset(GR_ndvi_2018, new_GRtype =="woody_plants")))
summary(lm(MEAN ~ sqrt_area, data = subset(GR_ndvi_2018, new_GRtype =="mix_grass_tree")))


###### 2017 (growing season)  ######
# Change the level order
GR_ndvi_2017$new_GRtype <- factor(GR_ndvi_2017$new_GRtype, levels = c("grass", "sedum_mat", "woody_plants","mix_grass_tree"))

# Plot
ndvi_area_2017_vegetype.plot <- ggplot(GR_ndvi_2017, aes(x=sqrt_area,y=MEAN, color = new_GRtype, linetype = new_GRtype))+
  geom_point(aes(shape = new_GRtype), size=2.5, alpha = 0.2) +
  geom_smooth(aes(color = new_GRtype, fill=new_GRtype, linetype = new_GRtype),
              method='lm', formula= y~x, alpha = 0.2, cex=1.5)+
  labs(x=expression(sqrt("Green roof area")~(m)), y="Mean NDVI",
       group = "new_GRtype", linetype = "new_GRtype", pch = "new_GRtype")+
  scale_color_manual(name="Vegetation type",labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"),values = c("#00BFFF", "#FF762E", "#7CAE00","#cd6ee8"))+
  scale_fill_manual(name="Vegetation type",labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"),values = c("#00BFFF", "#FF762E", "#7CAE00","#cd6ee8"))+
  scale_shape_manual(name="Vegetation type", labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"), values = c(19,17, 15, 18))+
  scale_linetype_manual(name="Vegetation type", labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"), values = c(1,1,1,2))+
  theme_bw(base_size = 28)+
  theme(
    legend.position = "top",
    legend.title = element_text(size=32),
    legend.text = element_text(size = 28),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.text=element_text(size=28),
    axis.title=element_text(size=32)
  )+
  guides(
    color = guide_legend(nrow = 2, byrow = TRUE),
    linetype = guide_legend(nrow = 2, byrow = TRUE)
  )

ndvi_area_2017_vegetype.plot

# Linear regression grouped by vegetation type
summary(lm(MEAN ~ sqrt_area, data = subset(GR_ndvi_2017, new_GRtype =="grass")))
summary(lm(MEAN ~ sqrt_area, data = subset(GR_ndvi_2017, new_GRtype =="sedum_mat")))
summary(lm(MEAN ~ sqrt_area, data = subset(GR_ndvi_2017, new_GRtype =="woody_plants")))
summary(lm(MEAN ~ sqrt_area, data = subset(GR_ndvi_2017, new_GRtype =="mix_grass_tree")))


# Supplementary Information Figure S7: Mean NDVI vs. Building height (grouped by vegetation type) -------------------
###### 2018 (pre-growing season)  ######
# Change the level order
GR_ndvi_2018$new_GRtype <- factor(GR_ndvi_2018$new_GRtype, levels = c("grass", "sedum_mat", "woody_plants","mix_grass_tree"))

# Plot
ndvi_height_2018_vegetype.plot <- ggplot(GR_ndvi_2018, aes(x=MAX_HEIGHT,y=MEAN, color = new_GRtype, linetype = new_GRtype))+
  geom_point(aes(shape = new_GRtype), size=2.5, alpha = 0.2) +
  geom_smooth(aes(color = new_GRtype, fill=new_GRtype, linetype = new_GRtype),
              method='lm', formula= y~x, alpha = 0.2, cex=1.5)+
  # geom_smooth(aes(color = GR_type, fill = GR_type), method="lm", formula=y ~ log(x + 1e-6), se=TRUE, alpha = 0.3) +
  labs(x="Building height (m)", y="Mean NDVI",
       group = "new_GRtype", linetype = "new_GRtype", pch = "new_GRtype")+
  scale_color_manual(name="Vegetation type",labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"),values = c("#00BFFF", "#FF762E", "#7CAE00","#cd6ee8"))+
  scale_fill_manual(name="Vegetation type",labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"),values = c("#00BFFF", "#FF762E", "#7CAE00","#cd6ee8"))+
  scale_shape_manual(name="Vegetation type", labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"), values = c(19,17, 15, 18))+
  scale_linetype_manual(name="Vegetation type", labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"), values = c(2,1,1,2))+
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
  )+
  guides(
    color = guide_legend(nrow = 2, byrow = TRUE),
    linetype = guide_legend(nrow = 2, byrow = TRUE)
  )

ndvi_height_2018_vegetype.plot

# Linear regression grouped by vegetation type
summary(lm(MEAN ~ MAX_HEIGHT, data = subset(GR_ndvi_2018, new_GRtype =="grass")))
summary(lm(MEAN ~ MAX_HEIGHT, data = subset(GR_ndvi_2018, new_GRtype =="sedum_mat")))
summary(lm(MEAN ~ MAX_HEIGHT, data = subset(GR_ndvi_2018, new_GRtype =="woody_plants")))
summary(lm(MEAN ~ MAX_HEIGHT, data = subset(GR_ndvi_2018, new_GRtype =="mix_grass_tree")))


###### 2017 (growing season)  ######
# Change the level order
GR_ndvi_2017$new_GRtype <- factor(GR_ndvi_2017$new_GRtype, levels = c("grass", "sedum_mat", "woody_plants","mix_grass_tree"))

# Plot
ndvi_height_2017_vegetype.plot <- ggplot(GR_ndvi_2017, aes(x=MAX_HEIGHT,y=MEAN, color = new_GRtype, linetype = new_GRtype))+
  geom_point(aes(shape = new_GRtype), size=2.5, alpha = 0.2) +
  geom_smooth(aes(color = new_GRtype, fill=new_GRtype, linetype = new_GRtype),
              method='lm', formula= y~x, alpha = 0.2, cex=1.5)+
  labs(x="Building height (m)", y="Mean NDVI",
       group = "new_GRtype", linetype = "new_GRtype", pch = "new_GRtype")+
  scale_color_manual(name="Vegetation type",labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"),values = c("#00BFFF", "#FF762E", "#7CAE00","#cd6ee8"))+
  scale_fill_manual(name="Vegetation type",labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"),values = c("#00BFFF", "#FF762E", "#7CAE00","#cd6ee8"))+
  scale_shape_manual(name="Vegetation type", labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"), values = c(19,17, 15, 18))+
  scale_linetype_manual(name="Vegetation type", labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"), values = c(2,1,2,2))+
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
  )+
  guides(
    color = guide_legend(nrow = 2, byrow = TRUE),
    linetype = guide_legend(nrow = 2, byrow = TRUE)
  )

ndvi_height_2017_vegetype.plot


# Linear regression grouped by vegetation type
summary(lm(MEAN ~ MAX_HEIGHT, data = subset(GR_ndvi_2017, new_GRtype =="grass")))
summary(lm(MEAN ~ MAX_HEIGHT, data = subset(GR_ndvi_2017, new_GRtype =="sedum_mat")))
summary(lm(MEAN ~ MAX_HEIGHT, data = subset(GR_ndvi_2017, new_GRtype =="woody_plants")))
summary(lm(MEAN ~ MAX_HEIGHT, data = subset(GR_ndvi_2017, new_GRtype =="mix_grass_tree")))


# Supplementary Information Figure S7: Mean NDVI vs. vegetation type -------------------
###### 2018 (pre-growing season)  ######
# Summarize the data
summary_vegetype_ndvi_2018 <-
  do.call(data.frame,
          aggregate(MEAN ~ new_GRtype,
                    data = GR_ndvi_2018,
                    FUN = function(x) c(mean_value = mean(x),se_value = get_se(x))))

# Change the level order
summary_vegetype_ndvi_2018$new_GRtype <- factor(summary_vegetype_ndvi_2018$new_GRtype, levels = c("grass","sedum_mat","woody_plants","mix_grass_tree"))

# plot
ndvi_vegetype_barplot_2018 <- ggplot(summary_vegetype_ndvi_2018, aes(x = new_GRtype, y = MEAN.mean_value, fill = new_GRtype)) +
  geom_bar(stat="identity", colour = "black", size = 0.3) +
  scale_fill_manual(name="Vegetation type",labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"),values = c("#00BFFF", "#FF762E", "#7CAE00","#cd6ee8"))+
  scale_x_discrete(name="Vegetation type",labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"))+
  geom_errorbar(aes(ymin = MEAN.mean_value - MEAN.se_value,
                    ymax = MEAN.mean_value + MEAN.se_value),
                width = 0.3, linetype = 1)+
  labs(x = "Green roof type", y = "Mean NDVI",
       group = "GR_type")+
  coord_cartesian(ylim=c(0,0.2))+
  theme_bw(base_size = 28) + 
  theme(
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text=element_text(size=22, color = "black"),
    axis.title=element_text(size=32, color = "black"),
    axis.text.x = element_text(angle = 15,hjust=1),
    legend.title = element_blank(),
    legend.position = "none"
  )

print(ndvi_vegetype_barplot_2018)

# One-way ANOVA
summary(aov(MEAN~new_GRtype, data = GR_ndvi_2018))
print(HSD.test(aov(MEAN~new_GRtype, data = GR_ndvi_2018), "new_GRtype"))


###### 2017 (growing season)  ######
# Summarize the data
summary_vegetype_ndvi_2017 <-
  do.call(data.frame,
          aggregate(MEAN ~ new_GRtype,
                    data = GR_ndvi_2017,
                    FUN = function(x) c(mean_value = mean(x),se_value = get_se(x))))

# Change the level order
summary_vegetype_ndvi_2017$new_GRtype <- factor(summary_vegetype_ndvi_2017$new_GRtype, levels = c("grass","sedum_mat","woody_plants","mix_grass_tree"))

# plot
ndvi_vegetype_barplot_2017 <- ggplot(summary_vegetype_ndvi_2017, aes(x = new_GRtype, y = MEAN.mean_value, fill = new_GRtype)) +
  geom_bar(stat="identity", colour = "black", size = 0.3) +
  scale_fill_manual(name="Vegetation type",labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"),values = c("#00BFFF", "#FF762E", "#7CAE00","#cd6ee8"))+
  scale_x_discrete(name="Vegetation type",labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"))+
  geom_errorbar(aes(ymin = MEAN.mean_value - MEAN.se_value,
                    ymax = MEAN.mean_value + MEAN.se_value),
                width = 0.3, linetype = 1)+
  labs(x = "Green roof type", y = "Mean NDVI",
       group = "GR_type")+
  coord_cartesian(ylim=c(0,0.27))+
  theme_bw(base_size = 28) + 
  theme(
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text=element_text(size=22, color = "black"),
    axis.title=element_text(size=32, color = "black"),
    axis.text.x = element_text(angle = 15,hjust=1),
    legend.title = element_blank(),
    legend.position = "none"
  )

print(ndvi_vegetype_barplot_2017)

# One-way ANOVA
summary(aov(MEAN~new_GRtype, data = GR_ndvi_2017))
print(HSD.test(aov(MEAN~new_GRtype, data = GR_ndvi_2017), "new_GRtype"))


# Supplementary Information Figure S7: Mean NDVI vs. SQRT Aspect ratio (grouped by vegetation type) -------------------
###### 2018 (pre-growing season)  ######
# change order of display
GR_ndvi_2018$new_GRtype <- factor(GR_ndvi_2018$new_GRtype, levels = c("grass", "sedum_mat", "woody_plants","mix_grass_tree"))

# Plot
ndvi_sqrt_AR_vegetype_2018.plot <- ggplot(GR_ndvi_2018, aes(x=sqrt_AR,y=MEAN, color = new_GRtype, linetype = new_GRtype))+
  geom_point(aes(shape = new_GRtype), size=2.5, alpha = 0.2) +
  geom_smooth(aes(color = new_GRtype, fill=new_GRtype, linetype = new_GRtype),
              method='lm', formula= y~x, alpha = 0.2, cex=1.5)+
  labs(x=expression(sqrt("Aspect ratio")), y="Mean NDVI",
       group = "new_GRtype", linetype = "new_GRtype", pch = "new_GRtype")+
  scale_color_manual(name="Vegetation type",labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"),values = c("#00BFFF", "#FF762E", "#7CAE00","#cd6ee8"))+
  scale_fill_manual(name="Vegetation type",labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"),values = c("#00BFFF", "#FF762E", "#7CAE00","#cd6ee8"))+
  scale_linetype_manual(name="Vegetation type", labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"), values = c(2,2, 1, 2))+
  scale_shape_manual(name="Vegetation type", labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"), values = c(19,17, 15, 18)) +
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
  )+
  guides(
    color = guide_legend(nrow = 2, byrow = TRUE),
    linetype = guide_legend(nrow = 2, byrow = TRUE)
  )

ndvi_sqrt_AR_vegetype_2018.plot

# Linear regression grouped by vegetation type
summary(lm(MEAN ~ sqrt_AR, data = subset(GR_ndvi_2018, new_GRtype =="grass")))
summary(lm(MEAN ~ sqrt_AR, data = subset(GR_ndvi_2018, new_GRtype =="sedum_mat")))
summary(lm(MEAN ~ sqrt_AR, data = subset(GR_ndvi_2018, new_GRtype =="woody_plants")))
summary(lm(MEAN ~ sqrt_AR, data = subset(GR_ndvi_2018, new_GRtype =="mix_grass_tree")))


###### 2017 (growing season)  ######
# change order of display
GR_ndvi_2017$new_GRtype <- factor(GR_ndvi_2017$new_GRtype, levels = c("grass", "sedum_mat", "woody_plants","mix_grass_tree"))

# Plot
ndvi_sqrt_AR_vegetype_2017.plot <- ggplot(GR_ndvi_2017, aes(x=sqrt_AR,y=MEAN, color = new_GRtype, linetype = new_GRtype))+
  geom_point(aes(shape = new_GRtype), size=2.5, alpha = 0.2) +
  geom_smooth(aes(color = new_GRtype, fill=new_GRtype, linetype = new_GRtype),
              method='lm', formula= y~x, alpha = 0.2, cex=1.5)+
  labs(x=expression(sqrt("Aspect ratio")), y="Mean NDVI",
       group = "new_GRtype", linetype = "new_GRtype", pch = "new_GRtype")+
  scale_color_manual(name="Vegetation type",labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"),values = c("#00BFFF", "#FF762E", "#7CAE00","#cd6ee8"))+
  scale_fill_manual(name="Vegetation type",labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"),values = c("#00BFFF", "#FF762E", "#7CAE00","#cd6ee8"))+
  scale_linetype_manual(name="Vegetation type", labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"), values = c(2,1, 1, 2))+
  scale_shape_manual(name="Vegetation type", labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"), values = c(19,17, 15, 18)) +
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
  )+
  guides(
    color = guide_legend(nrow = 2, byrow = TRUE),
    linetype = guide_legend(nrow = 2, byrow = TRUE)
  )

ndvi_sqrt_AR_vegetype_2017.plot

# Linear regression by vegetation type
summary(lm(MEAN ~ sqrt_AR, data = subset(GR_ndvi_2017, new_GRtype =="grass")))
summary(lm(MEAN ~ sqrt_AR, data = subset(GR_ndvi_2017, new_GRtype =="sedum_mat")))
summary(lm(MEAN ~ sqrt_AR, data = subset(GR_ndvi_2017, new_GRtype =="woody_plants")))
summary(lm(MEAN ~ sqrt_AR, data = subset(GR_ndvi_2017, new_GRtype =="mix_grass_tree")))


# Combine Supplementary Information Figure S7: Mean NDVI grouped by vegetation type (pre-growing season) --------
merge_mean_NDVI_design_VegeType_FULL = ndvi_area_2017_vegetype.plot + ndvi_area_2018_vegetype.plot + 
  ndvi_height_2017_vegetype.plot + ndvi_height_2018_vegetype.plot +
  ndvi_vegetype_barplot_2017 + ndvi_vegetype_barplot_2018 +
  ndvi_sqrt_AR_vegetype_2017.plot + ndvi_sqrt_AR_vegetype_2018.plot +
  plot_layout(guides = "collect", ncol = 2) & theme(legend.position = "top")

# merge_CV_NDVI_design_FULL[[1]] = merge_CV_NDVI_design_FULL[[1]] + theme(legend.position = "none")
merge_mean_NDVI_design_VegeType_FULL[[2]] = merge_mean_NDVI_design_VegeType_FULL[[2]] + theme(legend.position = "none", axis.title.y = element_blank())
merge_mean_NDVI_design_VegeType_FULL[[3]] = merge_mean_NDVI_design_VegeType_FULL[[3]] + theme(legend.position = "none")
merge_mean_NDVI_design_VegeType_FULL[[4]] = merge_mean_NDVI_design_VegeType_FULL[[4]] + theme(legend.position = "none", axis.title.y = element_blank())
merge_mean_NDVI_design_VegeType_FULL[[5]] = merge_mean_NDVI_design_VegeType_FULL[[5]] + theme(legend.position = "none")
merge_mean_NDVI_design_VegeType_FULL[[6]] = merge_mean_NDVI_design_VegeType_FULL[[6]] + theme(legend.position = "none", axis.title.y = element_blank())
merge_mean_NDVI_design_VegeType_FULL[[7]] = merge_mean_NDVI_design_VegeType_FULL[[7]] + theme(legend.position = "none")
merge_mean_NDVI_design_VegeType_FULL[[8]] = merge_mean_NDVI_design_VegeType_FULL[[8]] + theme(legend.position = "none", axis.title.y = element_blank())

merge_mean_NDVI_design_VegeType_FULL

# ggsave(plot=merge_mean_NDVI_design_VegeType_FULL, filename = "path/mean_NDVI_DesignFactors_VegeType.tif", width = 12, height = 18, device='tiff', dpi=600,unit="in")



# Supplementary Information Figure S8: CV of NDVI vs. SQRT area (grouped by green roof type) -------
###### CV NDVI: 2018 (pre-growing season)  ######
# Change the level order
GR_ndvi_2018$GR_type <- factor(GR_ndvi_2018$GR_type, levels = c("ext","int"))

# Plot
CV_ndvi_area_2018.plot <- ggplot(GR_ndvi_2018, aes(x=sqrt_area,y=COV, color = GR_type, linetype = GR_type))+
  geom_point(aes(shape = GR_type), size=2.5, alpha = 0.3) +
  geom_smooth(aes(color = GR_type, fill=GR_type, linetype = GR_type),
              method='lm', formula= y~x, alpha = 0.3, cex=1.5)+
  labs(x=expression(sqrt("Green roof area")~(m)), y="CV of NDVI",
       group = "GR_type", linetype = "GR_type", pch = "GR_type")+
  scale_color_manual(name="Green roof type",labels = c("Extensive","Intensive"),values = c("#FC7307","#236AB9"))+
  scale_fill_manual(name="Green roof type",labels = c("Extensive","Intensive"),values = c("#FC7307","#236AB9"))+
  scale_linetype_manual(name="Green roof type", labels = c("Extensive","Intensive"), values = c(1,1))+
  scale_shape_manual(name="Green roof type", labels = c("Extensive","Intensive"), values = c(19,17))+
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

CV_ndvi_area_2018.plot

# Linear regression
summary(lm(COV ~ sqrt_area, data = GR_ndvi_2018))

# Linear regression by green roof type
summary(lm(COV ~ sqrt_area, data = subset(GR_ndvi_2018, GR_type =="int")))
summary(lm(COV ~ sqrt_area, data = subset(GR_ndvi_2018, GR_type =="ext")))


###### CV: 2017 (growing season)  ######
# Change the level order
GR_ndvi_2017$GR_type <- factor(GR_ndvi_2017$GR_type, levels = c("ext","int"))

# Plot
CV_ndvi_area_2017.plot <- ggplot(GR_ndvi_2017, aes(x=sqrt_area,y=COV, color = GR_type, linetype = GR_type))+
  geom_point(aes(shape = GR_type), size=2.5, alpha = 0.3) +
  geom_smooth(aes(color = GR_type, fill=GR_type, linetype = GR_type),
              method='lm', formula= y~x, alpha = 0.3, cex=1.5)+
  labs(x=expression(sqrt("Green roof area")~(m)), y="CV of NDVI",
       group = "GR_type", linetype = "GR_type", pch = "GR_type")+
  scale_color_manual(name="Green roof type",labels = c("Extensive","Intensive"),values = c("#FC7307","#236AB9"))+
  scale_fill_manual(name="Green roof type",labels = c("Extensive","Intensive"),values = c("#FC7307","#236AB9"))+
  scale_linetype_manual(name="Green roof type", labels = c("Extensive","Intensive"), values = c(1,1))+
  scale_shape_manual(name="Green roof type", labels = c("Extensive","Intensive"), values = c(19,17))+
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

CV_ndvi_area_2017.plot

# Linear regression
summary(lm(COV ~ sqrt_area, data = GR_ndvi_2017))

# Linear regression by green roof type
summary(lm(COV ~ sqrt_area, data = subset(GR_ndvi_2017, GR_type =="int")))
summary(lm(COV ~ sqrt_area, data = subset(GR_ndvi_2017, GR_type =="ext")))


# Supplementary Information Figure S8: CV of NDVI vs. Building height (grouped by green roof type) -------
###### CV: 2018 (pre-growing season)  ######
# Change the level order
GR_ndvi_2018$GR_type <- factor(GR_ndvi_2018$GR_type, levels = c("ext","int"))

# Plot
CV_ndvi_height_2018.plot <- ggplot(GR_ndvi_2018, aes(x=MAX_HEIGHT,y=COV, color = GR_type, linetype = GR_type))+
  geom_point(aes(shape = GR_type), size=2.5, alpha = 0.3) +
  geom_smooth(aes(color = GR_type, fill=GR_type, linetype = GR_type),
              method='lm', formula= y~x, alpha = 0.3, cex=1.5)+
  labs(x="Building height (m)", y="CV of NDVI",
       group = "GR_type", linetype = "GR_type", pch = "GR_type")+
  scale_color_manual(name="Green roof type",labels = c("Extensive","Intensive"),values = c("#FC7307","#236AB9"))+
  scale_fill_manual(name="Green roof type",labels = c("Extensive","Intensive"),values = c("#FC7307","#236AB9"))+
  scale_linetype_manual(name="Green roof type", labels = c("Extensive","Intensive"), values = c(1,2))+
  scale_shape_manual(name="Green roof type", labels = c("Extensive","Intensive"), values = c(19,17))+
  scale_x_continuous(limits = c(0, 180))+
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

CV_ndvi_height_2018.plot

# Linear regression
summary(lm(COV ~ MAX_HEIGHT, data = GR_ndvi_2018))

# Linear regression by green roof type
summary(lm(COV ~ MAX_HEIGHT, data = subset(GR_ndvi_2018, GR_type =="int")))
summary(lm(COV ~ MAX_HEIGHT, data = subset(GR_ndvi_2018, GR_type =="ext")))


###### CV: 2017 (growing season)  ######
# Change the level order
GR_ndvi_2017$GR_type <- factor(GR_ndvi_2017$GR_type, levels = c("ext","int"))

# Plot
CV_ndvi_height_2017.plot <- ggplot(GR_ndvi_2017, aes(x=MAX_HEIGHT,y=COV, color = GR_type, linetype = GR_type))+
  geom_point(aes(shape = GR_type), size=2.5, alpha = 0.3) +
  geom_smooth(aes(color = GR_type, fill=GR_type, linetype = GR_type),
              method='lm', formula= y~x, alpha = 0.3, cex=1.5)+
  labs(x="Building height (m)", y="CV of NDVI",
       group = "GR_type", linetype = "GR_type", pch = "GR_type")+
  scale_color_manual(name="Green roof type",labels = c("Extensive","Intensive"),values = c("#FC7307","#236AB9"))+
  scale_fill_manual(name="Green roof type",labels = c("Extensive","Intensive"),values = c("#FC7307","#236AB9"))+
  scale_linetype_manual(name="Green roof type", labels = c("Extensive","Intensive"), values = c(2,2))+
  scale_shape_manual(name="Green roof type", labels = c("Extensive","Intensive"), values = c(19,17))+
  scale_x_continuous(limits = c(0, 180))+
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

CV_ndvi_height_2017.plot

# Linear regression
summary(lm(COV ~ MAX_HEIGHT, data = GR_ndvi_2017))

# Linear regression by green roof type
summary(lm(COV ~ MAX_HEIGHT, data = subset(GR_ndvi_2017, GR_type =="int")))
summary(lm(COV ~ MAX_HEIGHT, data = subset(GR_ndvi_2017, GR_type =="ext")))


# Supplementary Information Figure S8: CV of NDVI vs. Green roof type -------
###### CV: 2018 (pre-growing season)  ######
summary_GR_CV_ndvi_2018 <- GR_ndvi_2018 %>%
  group_by(GR_type) %>%
  summarise(mean_NDVI_CV = mean(COV, na.rm = TRUE),
            se_NDVI_CV = get_se(COV))

# Change the level order
summary_GR_CV_ndvi_2018$GR_type <- factor(summary_GR_CV_ndvi_2018$GR_type, levels = c("ext","int"))

# plot
CV_ndvi_GRtype_barplot_2018 <- ggplot(summary_GR_CV_ndvi_2018, aes(x = GR_type, y = mean_NDVI_CV, fill = GR_type)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin = mean_NDVI_CV - se_NDVI_CV, ymax = mean_NDVI_CV + se_NDVI_CV),
                position = position_dodge(0.9), width = 0.3) +
  scale_x_discrete(name="Green roof type",labels = c("Extensive", "Intensive"))+
  scale_fill_manual(name="Green roof type",labels = c("Extensive", "Intensive"),values = c("#FC7307","#236AB9"))+
  labs(x = "Green roof type", y = "CV of NDVI",
       group = "GR_type")+
  coord_cartesian(ylim=c(0,0.05))+
  theme_bw(base_size = 28) + 
  theme(
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text=element_text(size=28, color = "black"),
    axis.title=element_text(size=32, color = "black"),
    legend.position = "none",
    legend.title = element_text(size = 32)
  )

CV_ndvi_GRtype_barplot_2018

# t-test
t.test(COV ~ GR_type, data = GR_ndvi_2018, var.equal = TRUE)


###### CV: 2017 (growing season) ######
summary_GR_CV_ndvi_2017 <- GR_ndvi_2017 %>%
  group_by(GR_type) %>%
  summarise(mean_NDVI_CV = mean(COV, na.rm = TRUE),
            se_NDVI_CV = get_se(COV))

# Change the level order
summary_GR_CV_ndvi_2017$GR_type <- factor(summary_GR_CV_ndvi_2017$GR_type, levels = c("ext","int"))

# plot
CV_ndvi_GRtype_barplot_2017 <- ggplot(summary_GR_CV_ndvi_2017, aes(x = GR_type, y = mean_NDVI_CV, fill = GR_type)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin = mean_NDVI_CV - se_NDVI_CV, ymax = mean_NDVI_CV + se_NDVI_CV),
                position = position_dodge(0.9), width = 0.3) +
  scale_x_discrete(name="Green roof type",labels = c("Extensive", "Intensive"))+
  scale_fill_manual(name="Green roof type",labels = c("Extensive", "Intensive"),values = c("#FC7307","#236AB9"))+
  labs(x = "Green roof type", y = "CV of NDVI",
       group = "GR_type")+
  coord_cartesian(ylim=c(0,0.05))+
  theme_bw(base_size = 28) + 
  theme(
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text=element_text(size=28, color = "black"),
    axis.title=element_text(size=32, color = "black"),
    legend.position = "none",
    legend.title = element_text(size = 32)
  )

CV_ndvi_GRtype_barplot_2017

# t-test
t.test(COV ~ GR_type, data = GR_ndvi_2017, var.equal = TRUE)


# Supplementary Information Figure S8: CV of NDVI vs. SQRT Aspect ratio (grouped by green roof type) -------
###### CV: 2018 (pre-growing season)  ######
# change order of display
GR_ndvi_2018$GR_type <- factor(GR_ndvi_2018$GR_type, levels = c("ext", "int"))

# Square root aspect ratio
CV_ndvi_sqrtAR_GRtype_2018.plot <- ggplot(GR_ndvi_2018, aes(x=sqrt_AR,y=COV, color = GR_type, linetype = GR_type))+
  geom_point(aes(shape = GR_type), size=2.5, alpha = 0.3) +
  geom_smooth(aes(color = GR_type, fill=GR_type, linetype = GR_type),
              method='lm', formula= y~x, alpha = 0.3, cex=1.5)+
  labs(x=expression(sqrt("Aspect ratio")), y="CV of NDVI",
       group = "GR_type", linetype = "GR_type", pch = "GR_type")+
  scale_color_manual(name="Green roof type",labels = c("Extensive", "Intensive"),values = c("#FC7307","#236AB9"))+
  scale_fill_manual(name="Green roof type",labels = c("Extensive", "Intensive"),values = c("#FC7307","#236AB9"))+
  scale_linetype_manual(name="Green roof type", labels = c("Extensive", "Intensive"), values = c(1,1))+
  scale_shape_manual(name="Green roof type", labels = c("Extensive", "Intensive"), values = c(19,17)) +
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

CV_ndvi_sqrtAR_GRtype_2018.plot

# Linear regression
summary(lm(COV ~ sqrt_AR, data = GR_ndvi_2018))

# Linear regression by green roof type
summary(lm(COV ~ sqrt_AR, data = subset(GR_ndvi_2018, GR_type =="int")))
summary(lm(COV ~ sqrt_AR, data = subset(GR_ndvi_2018, GR_type =="ext")))


###### CV: 2017 (growing season)  ######
# change order of display
GR_ndvi_2017$GR_type <- factor(GR_ndvi_2017$GR_type, levels = c("ext", "int"))

# Square root aspect ratio
CV_ndvi_sqrtAR_GRtype_2017.plot <- ggplot(GR_ndvi_2017, aes(x=sqrt_AR,y=COV, color = GR_type, linetype = GR_type))+
  geom_point(aes(shape = GR_type), size=2.5, alpha = 0.3) +
  geom_smooth(aes(color = GR_type, fill=GR_type, linetype = GR_type),
              method='lm', formula= y~x, alpha = 0.3, cex=1.5)+
  labs(x=expression(sqrt("Aspect ratio")), y="CV of NDVI",
       group = "GR_type", linetype = "GR_type", pch = "GR_type")+
  scale_color_manual(name="Green roof type",labels = c("Extensive", "Intensive"),values = c("#FC7307","#236AB9"))+
  scale_fill_manual(name="Green roof type",labels = c("Extensive", "Intensive"),values = c("#FC7307","#236AB9"))+
  scale_linetype_manual(name="Green roof type", labels = c("Extensive", "Intensive"), values = c(1,1))+
  scale_shape_manual(name="Green roof type", labels = c("Extensive", "Intensive"), values = c(19,17)) +
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

CV_ndvi_sqrtAR_GRtype_2017.plot

# Linear regression
summary(lm(COV ~ sqrt_AR, data = GR_ndvi_2017))

# Linear regression by green roof type
summary(lm(COV ~ sqrt_AR, data = subset(GR_ndvi_2017, GR_type =="int")))
summary(lm(COV ~ sqrt_AR, data = subset(GR_ndvi_2017, GR_type =="ext")))


# Combine Supplementary Information Figure S8: CV of NDVI grouped by green roof type --------
merge_CV_NDVI_design_FULL = CV_ndvi_area_2017.plot + CV_ndvi_area_2018.plot + 
  CV_ndvi_height_2017.plot + CV_ndvi_height_2018.plot +
  CV_ndvi_GRtype_barplot_2017 + CV_ndvi_GRtype_barplot_2018 + 
  CV_ndvi_sqrtAR_GRtype_2017.plot + CV_ndvi_sqrtAR_GRtype_2018.plot +
  plot_layout(guides = "collect", ncol = 2) & theme(legend.position = "top")

merge_CV_NDVI_design_FULL[[2]] = merge_CV_NDVI_design_FULL[[2]] + theme(legend.position = "none", axis.title.y = element_blank())
merge_CV_NDVI_design_FULL[[3]] = merge_CV_NDVI_design_FULL[[3]] + theme(legend.position = "none")
merge_CV_NDVI_design_FULL[[4]] = merge_CV_NDVI_design_FULL[[4]] + theme(legend.position = "none", axis.title.y = element_blank())
merge_CV_NDVI_design_FULL[[5]] = merge_CV_NDVI_design_FULL[[5]] + theme(legend.position = "none")
merge_CV_NDVI_design_FULL[[6]] = merge_CV_NDVI_design_FULL[[6]] + theme(legend.position = "none", axis.title.y = element_blank())
merge_CV_NDVI_design_FULL[[7]] = merge_CV_NDVI_design_FULL[[7]] + theme(legend.position = "none")
merge_CV_NDVI_design_FULL[[8]] = merge_CV_NDVI_design_FULL[[8]] + theme(legend.position = "none", axis.title.y = element_blank())

merge_CV_NDVI_design_FULL

# ggsave(plot=merge_CV_NDVI_design_FULL, filename = "path/CV_NDVI_DesignFactors_GRtype.tif", width = 12, height = 18, device='tiff', dpi=600,unit="in")




# Supplementary Information Figure S9: CV of NDVI vs. SQRT area (grouped by vegetation type) -------
###### 2018 (pre-growing season)  ######
# Change the level order
GR_ndvi_2018$new_GRtype <- factor(GR_ndvi_2018$new_GRtype, levels = c("grass", "sedum_mat", "woody_plants","mix_grass_tree"))

# Plot
CV_ndvi_area_2018_vegetype.plot <- ggplot(GR_ndvi_2018, aes(x=sqrt_area,y=COV, color = new_GRtype, linetype = new_GRtype))+
  geom_point(aes(shape = new_GRtype), size=2.5, alpha = 0.2) +
  geom_smooth(aes(color = new_GRtype, fill=new_GRtype, linetype = new_GRtype),
              method='lm', formula= y~x, alpha = 0.2, cex=1.5)+
  labs(x=expression(sqrt("Green roof area")~(m)), y="CV of NDVI",
       group = "new_GRtype", linetype = "new_GRtype", pch = "new_GRtype")+
  scale_color_manual(name="Vegetation type",labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"),values = c("#00BFFF", "#FF762E", "#7CAE00","#cd6ee8"))+
  scale_fill_manual(name="Vegetation type",labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"),values = c("#00BFFF", "#FF762E", "#7CAE00","#cd6ee8"))+
  scale_shape_manual(name="Vegetation type", labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"), values = c(19,17, 15, 18))+
  scale_linetype_manual(name="Vegetation type", labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"), values = c(1,1,1,2))+
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
  )+
  guides(
    color = guide_legend(nrow = 2, byrow = TRUE),
    linetype = guide_legend(nrow = 2, byrow = TRUE)
  )

CV_ndvi_area_2018_vegetype.plot

# Linear regression by vegetation type
summary(lm(COV ~ sqrt_area, data = subset(GR_ndvi_2018, new_GRtype =="grass")))
summary(lm(COV ~ sqrt_area, data = subset(GR_ndvi_2018, new_GRtype =="sedum_mat")))
summary(lm(COV ~ sqrt_area, data = subset(GR_ndvi_2018, new_GRtype =="woody_plants")))
summary(lm(COV ~ sqrt_area, data = subset(GR_ndvi_2018, new_GRtype =="mix_grass_tree")))


###### 2017 (growing season)  ######
# Change the level order
GR_ndvi_2017$new_GRtype <- factor(GR_ndvi_2017$new_GRtype, levels = c("grass", "sedum_mat", "woody_plants","mix_grass_tree"))

# Plot
CV_ndvi_area_2017_vegetype.plot <- ggplot(GR_ndvi_2017, aes(x=sqrt_area,y=COV, color = new_GRtype, linetype = new_GRtype))+
  geom_point(aes(shape = new_GRtype), size=2.5, alpha = 0.2) +
  geom_smooth(aes(color = new_GRtype, fill=new_GRtype, linetype = new_GRtype),
              method='lm', formula= y~x, alpha = 0.2, cex=1.5)+
  labs(x=expression(sqrt("Green roof area")~(m)), y="CV of NDVI",
       group = "new_GRtype", linetype = "new_GRtype", pch = "new_GRtype")+
  scale_color_manual(name="Vegetation type",labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"),values = c("#00BFFF", "#FF762E", "#7CAE00","#cd6ee8"))+
  scale_fill_manual(name="Vegetation type",labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"),values = c("#00BFFF", "#FF762E", "#7CAE00","#cd6ee8"))+
  scale_shape_manual(name="Vegetation type", labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"), values = c(19,17, 15, 18))+
  scale_linetype_manual(name="Vegetation type", labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"), values = c(1,2,1,2))+
  theme_bw(base_size = 28)+
  theme(
    legend.position = "top",
    legend.title = element_text(size=32),
    legend.text = element_text(size = 28),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.text=element_text(size=28),
    axis.title=element_text(size=32)
  )+
  guides(
    color = guide_legend(nrow = 2, byrow = TRUE),
    linetype = guide_legend(nrow = 2, byrow = TRUE)
  )

CV_ndvi_area_2017_vegetype.plot

# Linear regression by vegetation type
summary(lm(COV ~ sqrt_area, data = subset(GR_ndvi_2017, new_GRtype =="grass")))
summary(lm(COV ~ sqrt_area, data = subset(GR_ndvi_2017, new_GRtype =="sedum_mat")))
summary(lm(COV ~ sqrt_area, data = subset(GR_ndvi_2017, new_GRtype =="woody_plants")))
summary(lm(COV ~ sqrt_area, data = subset(GR_ndvi_2017, new_GRtype =="mix_grass_tree")))


# Supplementary Information Figure S9: CV of NDVI vs. Building height (grouped by vegetation type) -------
###### 2018 (pre-growing season)  ######
# Change the level order
GR_ndvi_2018$new_GRtype <- factor(GR_ndvi_2018$new_GRtype, levels = c("grass", "sedum_mat", "woody_plants","mix_grass_tree"))

# Plot
CV_ndvi_height_2018_vegetype.plot <- ggplot(GR_ndvi_2018, aes(x=MAX_HEIGHT,y=COV, color = new_GRtype, linetype = new_GRtype))+
  geom_point(aes(shape = new_GRtype), size=2.5, alpha = 0.2) +
  geom_smooth(aes(color = new_GRtype, fill=new_GRtype, linetype = new_GRtype),
              method='lm', formula= y~x, alpha = 0.2, cex=1.5)+
  labs(x="Building height (m)", y="CV of NDVI",
       group = "new_GRtype", linetype = "new_GRtype", pch = "new_GRtype")+
  scale_color_manual(name="Vegetation type",labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"),values = c("#00BFFF", "#FF762E", "#7CAE00","#cd6ee8"))+
  scale_fill_manual(name="Vegetation type",labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"),values = c("#00BFFF", "#FF762E", "#7CAE00","#cd6ee8"))+
  scale_shape_manual(name="Vegetation type", labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"), values = c(19,17, 15, 18))+
  scale_linetype_manual(name="Vegetation type", labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"), values = c(2,2,2,2))+
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
  )+
  guides(
    color = guide_legend(nrow = 2, byrow = TRUE),
    linetype = guide_legend(nrow = 2, byrow = TRUE)
  )

CV_ndvi_height_2018_vegetype.plot

# Linear regression by vegetation type
summary(lm(COV ~ MAX_HEIGHT, data = subset(GR_ndvi_2018, new_GRtype =="grass")))
summary(lm(COV ~ MAX_HEIGHT, data = subset(GR_ndvi_2018, new_GRtype =="sedum_mat")))
summary(lm(COV ~ MAX_HEIGHT, data = subset(GR_ndvi_2018, new_GRtype =="woody_plants")))
summary(lm(COV ~ MAX_HEIGHT, data = subset(GR_ndvi_2018, new_GRtype =="mix_grass_tree")))


###### 2017 (growing season)  ######
# Change the level order
GR_ndvi_2017$new_GRtype <- factor(GR_ndvi_2017$new_GRtype, levels = c("grass", "sedum_mat", "woody_plants","mix_grass_tree"))

# Plot
CV_ndvi_height_2017_vegetype.plot <- ggplot(GR_ndvi_2017, aes(x=MAX_HEIGHT,y=COV, color = new_GRtype, linetype = new_GRtype))+
  geom_point(aes(shape = new_GRtype), size=2.5, alpha = 0.2) +
  geom_smooth(aes(color = new_GRtype, fill=new_GRtype, linetype = new_GRtype),
              method='lm', formula= y~x, alpha = 0.2, cex=1.5)+
  labs(x="Building height (m)", y="CV of NDVI",
       group = "new_GRtype", linetype = "new_GRtype", pch = "new_GRtype")+
  scale_color_manual(name="Vegetation type",labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"),values = c("#00BFFF", "#FF762E", "#7CAE00","#cd6ee8"))+
  scale_fill_manual(name="Vegetation type",labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"),values = c("#00BFFF", "#FF762E", "#7CAE00","#cd6ee8"))+
  scale_shape_manual(name="Vegetation type", labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"), values = c(19,17, 15, 18))+
  scale_linetype_manual(name="Vegetation type", labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"), values = c(2,2,2,2))+
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
  )+
  guides(
    color = guide_legend(nrow = 2, byrow = TRUE),
    linetype = guide_legend(nrow = 2, byrow = TRUE)
  )

CV_ndvi_height_2017_vegetype.plot

# Linear regression by vegetation type
summary(lm(COV ~ MAX_HEIGHT, data = subset(GR_ndvi_2017, new_GRtype =="grass")))
summary(lm(COV ~ MAX_HEIGHT, data = subset(GR_ndvi_2017, new_GRtype =="sedum_mat")))
summary(lm(COV ~ MAX_HEIGHT, data = subset(GR_ndvi_2017, new_GRtype =="woody_plants")))
summary(lm(COV ~ MAX_HEIGHT, data = subset(GR_ndvi_2017, new_GRtype =="mix_grass_tree")))


# Supplementary Information Figure S9: CV of NDVI vs. vegetation type -------
###### 2018 (pre-growing season)  ######
# Summarize the data
CV_summary_vegetype_ndvi_2018 <-
  do.call(data.frame,
          aggregate(COV ~ new_GRtype,
                    data = GR_ndvi_2018,
                    FUN = function(x) c(mean_value = mean(x),se_value = get_se(x))))

# Change the level order
CV_summary_vegetype_ndvi_2018$new_GRtype <- factor(CV_summary_vegetype_ndvi_2018$new_GRtype, levels = c("grass","sedum_mat","woody_plants","mix_grass_tree"))

# plot
CV_ndvi_vegetype_barplot_2018 <- ggplot(CV_summary_vegetype_ndvi_2018, aes(x = new_GRtype, y = COV.mean_value, fill = new_GRtype)) +
  geom_bar(stat="identity", colour = "black", size = 0.3) +
  scale_fill_manual(name="Vegetation type",labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"),values = c("#00BFFF", "#FF762E", "#7CAE00","#cd6ee8"))+
  scale_x_discrete(name="Vegetation type",labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"))+
  geom_errorbar(aes(ymin = COV.mean_value - COV.se_value,
                    ymax = COV.mean_value + COV.se_value),
                width = 0.3, linetype = 1)+
  labs(x = "Green roof type", y = "CV of NDVI",
       group = "GR_type")+
  coord_cartesian(ylim=c(0,0.07))+
  theme_bw(base_size = 28) + 
  theme(
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text=element_text(size=22, color = "black"),
    axis.title=element_text(size=32, color = "black"),
    axis.text.x = element_text(angle = 15,hjust=1),
    legend.title = element_blank(),
    legend.position = "none"
  )

CV_ndvi_vegetype_barplot_2018

# One-way ANOVA
summary(aov(COV~new_GRtype, data = GR_ndvi_2018))
print(HSD.test(aov(COV~new_GRtype, data = GR_ndvi_2018), "new_GRtype"))


###### 2017 (growing season)  ######
# Summarize the data
CV_summary_vegetype_ndvi_2017 <-
  do.call(data.frame,
          aggregate(COV ~ new_GRtype,
                    data = GR_ndvi_2017,
                    FUN = function(x) c(mean_value = mean(x),se_value = get_se(x))))

# Change the level order
CV_summary_vegetype_ndvi_2017$new_GRtype <- factor(CV_summary_vegetype_ndvi_2017$new_GRtype, levels = c("grass","sedum_mat","woody_plants","mix_grass_tree"))

# plot
CV_ndvi_vegetype_barplot_2017 <- ggplot(CV_summary_vegetype_ndvi_2017, aes(x = new_GRtype, y = COV.mean_value, fill = new_GRtype)) +
  geom_bar(stat="identity", colour = "black", size = 0.3) +
  scale_fill_manual(name="Vegetation type",labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"),values = c("#00BFFF", "#FF762E", "#7CAE00","#cd6ee8"))+
  scale_x_discrete(name="Vegetation type",labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"))+
  geom_errorbar(aes(ymin = COV.mean_value - COV.se_value,
                    ymax = COV.mean_value + COV.se_value),
                width = 0.3, linetype = 1)+
  labs(x = "Green roof type", y = "CV of NDVI",
       group = "GR_type")+
  coord_cartesian(ylim=c(0,0.08))+
  theme_bw(base_size = 28) + 
  theme(
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text=element_text(size=22, color = "black"),
    axis.title=element_text(size=32, color = "black"),
    axis.text.x = element_text(angle = 15,hjust=1),
    legend.title = element_blank(),
    legend.position = "none"
  )

CV_ndvi_vegetype_barplot_2017

# One-way ANOVA
summary(aov(COV~new_GRtype, data = GR_ndvi_2017))
print(HSD.test(aov(COV~new_GRtype, data = GR_ndvi_2017), "new_GRtype"))


# Supplementary Information Figure S9: CV of NDVI vs. SQRT Aspect ratio (grouped by vegetation type) -------
###### 2018 (pre-growing season)  ######
# change order of display
GR_ndvi_2018$new_GRtype <- factor(GR_ndvi_2018$new_GRtype, levels = c("grass", "sedum_mat", "woody_plants","mix_grass_tree"))

# Plot
CV_ndvi_sqrt_AR_vegetype_2018.plot <- ggplot(GR_ndvi_2018, aes(x=sqrt_AR,y=COV, color = new_GRtype, linetype = new_GRtype))+
  geom_point(aes(shape = new_GRtype), size=2.5, alpha = 0.2) +
  geom_smooth(aes(color = new_GRtype, fill=new_GRtype, linetype = new_GRtype),
              method='lm', formula= y~x, alpha = 0.2, cex=1.5)+
  labs(x=expression(sqrt("Aspect ratio")), y="CV of NDVI",
       group = "new_GRtype", linetype = "new_GRtype", pch = "new_GRtype")+
  scale_color_manual(name="Vegetation type",labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"),values = c("#00BFFF", "#FF762E", "#7CAE00","#cd6ee8"))+
  scale_fill_manual(name="Vegetation type",labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"),values = c("#00BFFF", "#FF762E", "#7CAE00","#cd6ee8"))+
  scale_linetype_manual(name="Vegetation type", labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"), values = c(1,1, 1, 2))+
  scale_shape_manual(name="Vegetation type", labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"), values = c(19,17, 15, 18)) +
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
  )+
  guides(
    color = guide_legend(nrow = 2, byrow = TRUE),
    linetype = guide_legend(nrow = 2, byrow = TRUE)
  )

CV_ndvi_sqrt_AR_vegetype_2018.plot

# Linear regression by vegetation type
summary(lm(COV ~ sqrt_AR, data = subset(GR_ndvi_2018, new_GRtype =="grass")))
summary(lm(COV ~ sqrt_AR, data = subset(GR_ndvi_2018, new_GRtype =="sedum_mat")))
summary(lm(COV ~ sqrt_AR, data = subset(GR_ndvi_2018, new_GRtype =="woody_plants")))
summary(lm(COV ~ sqrt_AR, data = subset(GR_ndvi_2018, new_GRtype =="mix_grass_tree")))


###### 2017 (growing season)  ######
# change order of display
GR_ndvi_2017$new_GRtype <- factor(GR_ndvi_2017$new_GRtype, levels = c("grass", "sedum_mat", "woody_plants","mix_grass_tree"))

# Plot
CV_ndvi_sqrt_AR_vegetype_2017.plot <- ggplot(GR_ndvi_2017, aes(x=sqrt_AR,y=COV, color = new_GRtype, linetype = new_GRtype))+
  geom_point(aes(shape = new_GRtype), size=2.5, alpha = 0.2) +
  geom_smooth(aes(color = new_GRtype, fill=new_GRtype, linetype = new_GRtype),
              method='lm', formula= y~x, alpha = 0.2, cex=1.5)+
  labs(x=expression(sqrt("Aspect ratio")), y="CV of NDVI",
       group = "new_GRtype", linetype = "new_GRtype", pch = "new_GRtype")+
  scale_color_manual(name="Vegetation type",labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"),values = c("#00BFFF", "#FF762E", "#7CAE00","#cd6ee8"))+
  scale_fill_manual(name="Vegetation type",labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"),values = c("#00BFFF", "#FF762E", "#7CAE00","#cd6ee8"))+
  scale_linetype_manual(name="Vegetation type", labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"), values = c(1,2, 1, 2))+
  scale_shape_manual(name="Vegetation type", labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"), values = c(19,17, 15, 18)) +
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
  )+
  guides(
    color = guide_legend(nrow = 2, byrow = TRUE),
    linetype = guide_legend(nrow = 2, byrow = TRUE)
  )

CV_ndvi_sqrt_AR_vegetype_2017.plot

# Linear regression by vegetation type
summary(lm(COV ~ sqrt_AR, data = subset(GR_ndvi_2017, new_GRtype =="grass")))
summary(lm(COV ~ sqrt_AR, data = subset(GR_ndvi_2017, new_GRtype =="sedum_mat")))
summary(lm(COV ~ sqrt_AR, data = subset(GR_ndvi_2017, new_GRtype =="woody_plants")))
summary(lm(COV ~ sqrt_AR, data = subset(GR_ndvi_2017, new_GRtype =="mix_grass_tree")))


# Combine Supplementary Information Figure S9: CV of NDVI grouped by vegetation type --------
merge_CV_NDVI_design_VegeType_FULL = CV_ndvi_area_2017_vegetype.plot + CV_ndvi_area_2018_vegetype.plot +
  CV_ndvi_height_2017_vegetype.plot + CV_ndvi_height_2018_vegetype.plot +
  CV_ndvi_vegetype_barplot_2017 + CV_ndvi_vegetype_barplot_2018 +
  CV_ndvi_sqrt_AR_vegetype_2017.plot + CV_ndvi_sqrt_AR_vegetype_2018.plot +
  plot_layout(guides = "collect", ncol = 2) & theme(legend.position = "top")

merge_CV_NDVI_design_VegeType_FULL[[2]] = merge_CV_NDVI_design_VegeType_FULL[[2]] + theme(legend.position = "none", axis.title.y = element_blank())
merge_CV_NDVI_design_VegeType_FULL[[3]] = merge_CV_NDVI_design_VegeType_FULL[[3]] + theme(legend.position = "none")
merge_CV_NDVI_design_VegeType_FULL[[4]] = merge_CV_NDVI_design_VegeType_FULL[[4]] + theme(legend.position = "none", axis.title.y = element_blank())
merge_CV_NDVI_design_VegeType_FULL[[5]] = merge_CV_NDVI_design_VegeType_FULL[[5]] + theme(legend.position = "none")
merge_CV_NDVI_design_VegeType_FULL[[6]] = merge_CV_NDVI_design_VegeType_FULL[[6]] + theme(legend.position = "none", axis.title.y = element_blank())
merge_CV_NDVI_design_VegeType_FULL[[7]] = merge_CV_NDVI_design_VegeType_FULL[[7]] + theme(legend.position = "none")
merge_CV_NDVI_design_VegeType_FULL[[8]] = merge_CV_NDVI_design_VegeType_FULL[[8]] + theme(legend.position = "none", axis.title.y = element_blank())

merge_CV_NDVI_design_VegeType_FULL

# ggsave(plot=merge_CV_NDVI_design_VegeType_FULL, filename = "path/CV_NDVI_DesignFactors_VegeType.tif", width = 12, height = 18, device='tiff', dpi=600,unit="in")




# Figure 3 and Figure S18: (a) Mean NDVI vs. SQRT Area (grouped by Building Height) (Breakpoint Function)  --------
###### 2018 (pre-growing season) ######
# Add buidling categories to the data
GR_ndvi_2018$building_cat <- ifelse(GR_ndvi_2018$MAX_HEIGHT <= 14.4, "low",
                                    ifelse(GR_ndvi_2018$MAX_HEIGHT < 20, "mid", "high"))

# Save the NDVI vs. Area models for low, mid, and high-rises
linear_model_low_ndvi = lm(MEAN ~ sqrt_area, data = subset(GR_ndvi_2018, building_cat == "low"))
linear_model_mid_ndvi = lm(MEAN ~ sqrt_area, data = subset(GR_ndvi_2018, building_cat == "mid"))
linear_model_high_ndvi = lm(MEAN ~ sqrt_area, data = subset(GR_ndvi_2018, building_cat == "high"))

# Model summary
summary(linear_model_low_ndvi)
summary(linear_model_mid_ndvi)
summary(linear_model_high_ndvi)

# Fit segmented regression: segmented areas for suset building height
seg_model_low_ndvi <- segmented(linear_model_low_ndvi, seg.Z = ~ sqrt_area, psi = list(sqrt_area = 5)) 
seg_model_mid_ndvi <- segmented(linear_model_mid_ndvi, seg.Z = ~ sqrt_area, psi = list(sqrt_area = 5)) 
seg_model_high_ndvi <- segmented(linear_model_high_ndvi, seg.Z = ~ sqrt_area, psi = list(sqrt_area = 5)) 

# Summary of the segmented model
summary(seg_model_low_ndvi)
summary(seg_model_mid_ndvi)
summary(seg_model_high_ndvi)

# compare AIC for linear vs. break point functions
# low-rise
AIC(linear_model_low_ndvi)
AIC(seg_model_low_ndvi)
# mid-rise
AIC(linear_model_mid_ndvi)
AIC(seg_model_mid_ndvi)
# high-rise
AIC(linear_model_high_ndvi)
AIC(seg_model_high_ndvi)

# break points
break_point_low_ndvi = 5.565
break_point_mid_ndvi = 6.967
break_point_high_ndvi = 1.057

# split the breakpoints into 2 datasets for each green roof type
# low-rise
before_bp_low_ndvi = subset(GR_ndvi_2018, sqrt_area <= break_point_low_ndvi)
after_bp_low_ndvi = subset(GR_ndvi_2018, sqrt_area > break_point_low_ndvi)
# mid-rise
before_bp_mid_ndvi = subset(GR_ndvi_2018, sqrt_area <= break_point_mid_ndvi)
after_bp_mid_ndvi = subset(GR_ndvi_2018, sqrt_area > break_point_mid_ndvi)
# high-rise
before_bp_high_ndvi = subset(GR_ndvi_2018, sqrt_area <= break_point_high_ndvi)
after_bp_high_ndvi = subset(GR_ndvi_2018, sqrt_area > break_point_high_ndvi)

# fit the segmented datasets
# low-rise
lm_before_bp_low_ndvi = lm(MEAN ~ sqrt_area, data = before_bp_low_ndvi)
lm_after_bp_low_ndvi = lm(MEAN ~ sqrt_area, data = after_bp_low_ndvi)
# mid-rise
lm_before_bp_mid_ndvi = lm(MEAN ~ sqrt_area, data = before_bp_mid_ndvi)
lm_after_bp_mid_ndvi = lm(MEAN ~ sqrt_area, data = after_bp_mid_ndvi)
# high-rise
lm_before_bp_high_ndvi = lm(MEAN ~ sqrt_area, data = before_bp_high_ndvi)
lm_after_bp_high_ndvi = lm(MEAN ~ sqrt_area, data = after_bp_high_ndvi)

# View summaries of the models
# low-rise
summary(lm_before_bp_low_ndvi) 
summary(lm_after_bp_low_ndvi)
# mid-rise
summary(lm_before_bp_mid_ndvi) 
summary(lm_after_bp_mid_ndvi)
# high-rise
summary(lm_before_bp_high_ndvi)
summary(lm_after_bp_high_ndvi) 


# Change the level order
GR_ndvi_2018$building_cat <- factor(GR_ndvi_2018$building_cat, levels = c("low","mid","high"))

# plot the break point fitting
height_ndvi_2018.plot_bp <- ggplot(GR_ndvi_2018, aes(x=sqrt_area,y=MEAN, color = building_cat, linetype = building_cat))+
  geom_point(aes(shape = building_cat), size=3.5, alpha = 0.2) +
  geom_smooth(data = subset(GR_ndvi_2018, sqrt_area <= break_point_mid_ndvi & building_cat == "mid"), method = "lm",
              aes(x = sqrt_area, y = MEAN, fill = "#A17DB4"), se = TRUE, size = 1.5, linetype = "solid", alpha = 0.3) +
  geom_smooth(data = subset(GR_ndvi_2018, sqrt_area > break_point_mid_ndvi & building_cat == "mid"), method = "lm",
              aes(x = sqrt_area, y = MEAN, fill = "#A17DB4"), se = TRUE, size = 1.5, linetype = "solid", alpha = 0.3) +
  geom_smooth(data = subset(GR_ndvi_2018, sqrt_area <= break_point_high_ndvi & building_cat == "high"), method = "lm",
              aes(x = sqrt_area, y = MEAN, fill = "#ff8533"), se = TRUE, size = 1.5, linetype = "solid", alpha = 0.3) +
  geom_smooth(data = subset(GR_ndvi_2018, sqrt_area > break_point_high_ndvi & building_cat == "high"), method = "lm",
              aes(x = sqrt_area, y = MEAN, fill = "#ff8533"), se = TRUE, size = 1.5, linetype = "solid", alpha = 0.3) +
  geom_smooth(data = subset(GR_ndvi_2018, sqrt_area <= break_point_low_ndvi & building_cat == "low"), method = "lm",
              aes(x = sqrt_area, y = MEAN, fill = "#8FB943"), se = TRUE, size = 1.5, linetype = "solid", alpha = 0.3) +
  geom_smooth(data = subset(GR_ndvi_2018, sqrt_area > break_point_low_ndvi & building_cat == "low"), method = "lm",
              aes(x = sqrt_area, y = MEAN, fill = "#8FB943"), se = TRUE, size = 1.5, linetype = "solid", alpha = 0.3) +
  labs(x=expression(sqrt("Green roof area")~(m)), y="NDVI",
       group = "building_cat", linetype = "building_cat", pch = "building_cat")+
  scale_color_manual(name="Building height",labels = c("Low-rise","Mid-rise","High-rise"),values = c("#8FB943","#A17DB4","#329ED4"))+
  scale_fill_manual(name="Building height",labels = c("Low-rise","Mid-rise","High-rise"),values = c("#8FB943","#A17DB4","#329ED4"))+
  scale_shape_manual(name="Building height", labels = c("Low-rise","Mid-rise","High-rise"), values = c(17,19,15))+
  theme_bw(base_size = 28)+
  theme(
    legend.position = "top",
    legend.title = element_text(size=28),
    legend.text = element_text(size = 26),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.text=element_text(size=26),
    axis.title=element_text(size=30)
  ) +
  guides(linetype = "none", fill = guide_legend(override.aes = list(alpha = 0.2)))

print(height_ndvi_2018.plot_bp)

# ggsave(plot=height_ndvi_2018.plot_bp, filename = "path/NDVI_Area_height_breakpoint_2018.tif", width = 10, height = 8, device='tiff', dpi=300,unit="in")


###### 2017 (growing season) ######
# Add buidling categories to the data
GR_ndvi_2017$building_cat <- ifelse(GR_ndvi_2017$MAX_HEIGHT <= 14.4, "low",
                                    ifelse(GR_ndvi_2017$MAX_HEIGHT < 20, "mid", "high"))

# Save the NDVI vs. Area models for low, mid, and high-rises
linear_model_low_ndvi = lm(MEAN ~ sqrt_area, data = subset(GR_ndvi_2017, building_cat == "low"))
linear_model_mid_ndvi = lm(MEAN ~ sqrt_area, data = subset(GR_ndvi_2017, building_cat == "mid"))
linear_model_high_ndvi = lm(MEAN ~ sqrt_area, data = subset(GR_ndvi_2017, building_cat == "high"))

# Model summary
summary(linear_model_low_ndvi)
summary(linear_model_mid_ndvi)
summary(linear_model_high_ndvi)

# Fit segmented regression: segmented areas for suset building height
seg_model_low_ndvi <- segmented(linear_model_low_ndvi, seg.Z = ~ sqrt_area, psi = list(sqrt_area = 5)) 
seg_model_mid_ndvi <- segmented(linear_model_mid_ndvi, seg.Z = ~ sqrt_area, psi = list(sqrt_area = 5)) 
seg_model_high_ndvi <- segmented(linear_model_high_ndvi, seg.Z = ~ sqrt_area, psi = list(sqrt_area = 5)) 

# Summary of the segmented model
summary(seg_model_low_ndvi)
summary(seg_model_mid_ndvi)
summary(seg_model_high_ndvi)

# compare AIC for linear vs. break point functions
# low-rise
AIC(linear_model_low_ndvi)
AIC(seg_model_low_ndvi)
# mid-rise
AIC(linear_model_mid_ndvi)
AIC(seg_model_mid_ndvi)
# high-rise
AIC(linear_model_high_ndvi)
AIC(seg_model_high_ndvi)

# break points
break_point_low_ndvi = 3.225
break_point_mid_ndvi = 4.802
break_point_high_ndvi = 6.024

# split the breakpoints into 2 datasets for each green roof type
# low-rise
before_bp_low_ndvi = subset(GR_ndvi_2017, sqrt_area <= break_point_low_ndvi)
after_bp_low_ndvi = subset(GR_ndvi_2017, sqrt_area > break_point_low_ndvi)
# mid-rise
before_bp_mid_ndvi = subset(GR_ndvi_2017, sqrt_area <= break_point_mid_ndvi)
after_bp_mid_ndvi = subset(GR_ndvi_2017, sqrt_area > break_point_mid_ndvi)
# high-rise
before_bp_high_ndvi = subset(GR_ndvi_2017, sqrt_area <= break_point_high_ndvi)
after_bp_high_ndvi = subset(GR_ndvi_2017, sqrt_area > break_point_high_ndvi)

# fit the 2 segmented datasets
# low-rise
lm_before_bp_low_ndvi = lm(MEAN ~ sqrt_area, data = before_bp_low_ndvi)
lm_after_bp_low_ndvi = lm(MEAN ~ sqrt_area, data = after_bp_low_ndvi)
# mid-rise
lm_before_bp_mid_ndvi = lm(MEAN ~ sqrt_area, data = before_bp_mid_ndvi)
lm_after_bp_mid_ndvi = lm(MEAN ~ sqrt_area, data = after_bp_mid_ndvi)
# high-rise
lm_before_bp_high_ndvi = lm(MEAN ~ sqrt_area, data = before_bp_high_ndvi)
lm_after_bp_high_ndvi = lm(MEAN ~ sqrt_area, data = after_bp_high_ndvi)

# View summaries of the models
# low-rise
summary(lm_before_bp_low_ndvi)
summary(lm_after_bp_low_ndvi)
# mid-rise
summary(lm_before_bp_mid_ndvi) 
summary(lm_after_bp_mid_ndvi)
# high
summary(lm_before_bp_high_ndvi)
summary(lm_after_bp_high_ndvi) 


# Change the level order
GR_ndvi_2017$building_cat <- factor(GR_ndvi_2017$building_cat, levels = c("low","mid","high"))

# plot the break point fitting
height_ndvi_2017.plot_bp <- ggplot(GR_ndvi_2017, aes(x=sqrt_area,y=MEAN, color = building_cat, linetype = building_cat))+
  geom_point(aes(shape = building_cat), size=2.5, alpha = 0.2) +
  geom_smooth(data = subset(GR_ndvi_2017, sqrt_area <= break_point_mid_ndvi & building_cat == "mid"), method = "lm",
              aes(x = sqrt_area, y = MEAN, fill = "#A17DB4"), se = TRUE, size = 1.5, linetype = "solid", alpha = 0.3) +
  geom_smooth(data = subset(GR_ndvi_2017, sqrt_area > break_point_mid_ndvi & building_cat == "mid"), method = "lm",
              aes(x = sqrt_area, y = MEAN, fill = "#A17DB4"), se = TRUE, size = 1.5, linetype = "solid", alpha = 0.3) +
  geom_smooth(data = subset(GR_ndvi_2017, sqrt_area <= break_point_high_ndvi & building_cat == "high"), method = "lm",
              aes(x = sqrt_area, y = MEAN, fill = "#ff8533"), se = TRUE, size = 1.5, linetype = "solid", alpha = 0.3) +
  geom_smooth(data = subset(GR_ndvi_2017, sqrt_area > break_point_high_ndvi & building_cat == "high"), method = "lm",
              aes(x = sqrt_area, y = MEAN, fill = "#ff8533"), se = TRUE, size = 1.5, linetype = "solid", alpha = 0.3) +
  geom_smooth(data = subset(GR_ndvi_2017, sqrt_area <= break_point_low_ndvi & building_cat == "low"), method = "lm",
              aes(x = sqrt_area, y = MEAN, fill = "#8FB943"), se = TRUE, size = 1.5, linetype = "dashed", alpha = 0.3) +
  geom_smooth(data = subset(GR_ndvi_2017, sqrt_area > break_point_low_ndvi & building_cat == "low"), method = "lm",
              aes(x = sqrt_area, y = MEAN, fill = "#8FB943"), se = TRUE, size = 1.5, linetype = "solid", alpha = 0.3) +
  labs(x=expression(sqrt("Green roof area")~(m)), y="NDVI",
       group = "building_cat", linetype = "building_cat", pch = "building_cat")+
  scale_color_manual(name="Building height category",labels = c("Low-rise","Mid-rise","High-rise"),values = c("#8FB943","#A17DB4","#329ED4"))+
  scale_fill_manual(name="Building height category",labels = c("Low-rise","Mid-rise","High-rise"),values = c("#8FB943","#A17DB4","#329ED4"))+
  scale_shape_manual(name="Building height category", labels = c("Low-rise","Mid-rise","High-rise"), values = c(17,19,15))+
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
  ) +
  guides(linetype = "none", fill = guide_legend(override.aes = list(alpha = 0.2)))

print(height_ndvi_2017.plot_bp)

# ggsave(plot=height_ndvi_2017.plot_bp, filename = "path/NDVI_Area_height_breakpoint_2017.tif", width = 10, height = 8, device='tiff', dpi=300,unit="in")


# Figure 3 and Figure S18: (b) Mean NDVI vs. SQRT Area (gouped by Building Height) (Bar Graph)  --------
###### 2018 (pre-growing season) ######
# Add buidling categories to the data
GR_ndvi_2018$building_cat <- ifelse(GR_ndvi_2018$MAX_HEIGHT <= 14.4, "low",
                                    ifelse(GR_ndvi_2018$MAX_HEIGHT < 20, "mid", "high"))

# Summarize the data
height_summary_vegetype_ndvi_2018 <-
  do.call(data.frame,
          aggregate(MEAN ~ building_cat,
                    data = GR_ndvi_2018,
                    FUN = function(x) c(mean_value = mean(x),se_value = get_se(x))))

# Change the level order
height_summary_vegetype_ndvi_2018$building_cat <- factor(height_summary_vegetype_ndvi_2018$building_cat, levels = c("low","mid","high"))

# plot
height_ndvi_vegetype_barplot_2018 <- ggplot(height_summary_vegetype_ndvi_2018, aes(x = building_cat, y = MEAN.mean_value, fill = building_cat)) +
  geom_bar(stat="identity", colour = "black", size = 0.3) +
  scale_fill_manual(name="Building height",labels = c("Low-rise", "Mid-rise","High-rise"),values = c("#8FB943","#A17DB4","#329ED4"))+
  scale_x_discrete(name="Building height category",labels = c("Low-rise", "Mid-rise","High-rise"))+
  geom_errorbar(aes(ymin = MEAN.mean_value - MEAN.se_value,
                    ymax = MEAN.mean_value + MEAN.se_value),
                width = 0.3, linetype = 1)+
  labs(x = "Building height", y = "Mean NDVI",
       group = "building_cat")+
  coord_cartesian(ylim=c(0,0.22))+
  theme_bw(base_size = 28) + 
  theme(
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text=element_text(size=26, color = "black"),
    axis.title=element_text(size=32, color = "black"),
    axis.text.x = element_text(angle = 15,hjust=1),
    legend.title = element_blank(),
    legend.position = "none"
  )

height_ndvi_vegetype_barplot_2018

# ggsave(plot=ndvi_vegetype_barplot_2018, filename = "path/NDVI_GR_type_2018.tif", width = 9, height = 8, device='tiff', dpi=600,unit="in")

# One-way ANOVA
summary(aov(MEAN~building_cat, data = GR_ndvi_2018))
print(HSD.test(aov(MEAN~building_cat, data = GR_ndvi_2018), "building_cat"))


###### 2017 (growing season)  ######
# Add buidling categories to the data
GR_ndvi_2017$building_cat <- ifelse(GR_ndvi_2017$MAX_HEIGHT <= 14.4, "low",
                                    ifelse(GR_ndvi_2017$MAX_HEIGHT < 20, "mid", "high"))

# Summarize the data
height_summary_vegetype_ndvi_2017 <-
  do.call(data.frame,
          aggregate(MEAN ~ building_cat,
                    data = GR_ndvi_2017,
                    FUN = function(x) c(mean_value = mean(x),se_value = get_se(x))))

# Change the level order
height_summary_vegetype_ndvi_2017$building_cat <- factor(height_summary_vegetype_ndvi_2017$building_cat, levels = c("low","mid","high"))

# plot
height_ndvi_vegetype_barplot_2017 <- ggplot(height_summary_vegetype_ndvi_2017, aes(x = building_cat, y = MEAN.mean_value, fill = building_cat)) +
  geom_bar(stat="identity", colour = "black", size = 0.3) +
  scale_fill_manual(name="Building height",labels = c("Low-rise", "Mid-rise","High-rise"),values = c("#8FB943","#A17DB4","#329ED4"))+
  scale_x_discrete(name="Building height category",labels = c("Low-rise", "Mid-rise","High-rise"))+
  geom_errorbar(aes(ymin = MEAN.mean_value - MEAN.se_value,
                    ymax = MEAN.mean_value + MEAN.se_value),
                width = 0.3, linetype = 1)+
  labs(x = "Building height", y = "Mean NDVI",
       group = "building_cat")+
  coord_cartesian(ylim=c(0,0.3))+
  theme_bw(base_size = 28) + 
  theme(
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text=element_text(size=26, color = "black"),
    axis.title=element_text(size=32, color = "black"),
    axis.text.x = element_text(angle = 15,hjust=1),
    legend.title = element_blank(),
    legend.position = "none"
  )

height_ndvi_vegetype_barplot_2017

# ggsave(plot=ndvi_vegetype_barplot_2018, filename = "path/NDVI_GR_type_2017.tif", width = 9, height = 8, device='tiff', dpi=600,unit="in")

# One-way ANOVA
summary(aov(MEAN~building_cat, data = GR_ndvi_2017))
print(HSD.test(aov(MEAN~building_cat, data = GR_ndvi_2017), "building_cat"))


# Combine Figure 3 and Figure S18: segmented functions for area and building height --------
###### 2018 (pre-growing season) ######
merge_mean_NDVI_height_2018 = height_ndvi_2018.plot_bp + height_ndvi_vegetype_barplot_2018 +
  plot_layout(guides = "collect", ncol = 2) & theme(legend.position = "top")

merge_mean_NDVI_height_2018[[2]] = merge_mean_NDVI_height_2018[[2]] + theme(legend.position = "none")

merge_mean_NDVI_height_2018

# ggsave(plot=merge_mean_NDVI_height_2018, filename = "path/NDVI_segmented_2018.tif", width = 16, height = 8, device='tiff', dpi=600,unit="in")

###### 2017 (growing season)  ######
merge_mean_NDVI_height_2017 = height_ndvi_2017.plot_bp + height_ndvi_vegetype_barplot_2017 +
  plot_layout(guides = "collect", ncol = 2) & theme(legend.position = "top")

merge_mean_NDVI_height_2017[[2]] = merge_mean_NDVI_height_2017[[2]] + theme(legend.position = "none")

merge_mean_NDVI_height_2017

# ggsave(plot=merge_mean_NDVI_height_2017, filename = "path/NDVI_segmented_2017.tif", width = 16, height = 8, device='tiff', dpi=600,unit="in")





################### Supplementary Figures: SAVI, GNDVI, CIg ######################
# Subset 2018 files for SAVI, GNDVI, and CIg (pre-growing season) --------
# subset green roofs in 2018
GR_savi_2018 = subset(savi_data, year == 2018)
GR_gndvi_2018 = subset(gndvi_data, year == 2018)
GR_cig_2018 = subset(cig_data, year == 2018)

# transform the area data
GR_savi_2018$sqrt_area = sqrt(GR_savi_2018$SHAPE_Area)
GR_gndvi_2018$sqrt_area = sqrt(GR_gndvi_2018$SHAPE_Area)
GR_cig_2018$sqrt_area = sqrt(GR_cig_2018$SHAPE_Area)

# transform the aspect ratio data
GR_savi_2018$sqrt_AR = sqrt(GR_savi_2018$AspectRatio)
GR_gndvi_2018$sqrt_AR = sqrt(GR_gndvi_2018$AspectRatio)
GR_cig_2018$sqrt_AR = sqrt(GR_cig_2018$AspectRatio)

# Subset 2017 files for SAVI, GNDVI, and CIg (growing season) --------
# subset green roofs in 2017
GR_savi_2017 = subset(savi_data, year == 2017)
GR_gndvi_2017 = subset(gndvi_data, year == 2017)
GR_cig_2017 = subset(cig_data, year == 2017)

# transform the area data
GR_savi_2017$sqrt_area = sqrt(GR_savi_2017$SHAPE_Area)
GR_gndvi_2017$sqrt_area = sqrt(GR_gndvi_2017$SHAPE_Area)
GR_cig_2017$sqrt_area = sqrt(GR_cig_2017$SHAPE_Area)

# transform the aspect ratio data
GR_savi_2017$sqrt_AR = sqrt(GR_savi_2017$AspectRatio)
GR_gndvi_2017$sqrt_AR = sqrt(GR_gndvi_2017$AspectRatio)
GR_cig_2017$sqrt_AR = sqrt(GR_cig_2017$AspectRatio)



########### VIs: Linear model to select key design factors that influence mean VIs ###########
# SAVI --------------------------------------------------------------------
### 2018 (pre-growing season) ###
# model selection
model_savi_test_2018_GRtype <- lm(MEAN~SHAPE_Area + GR_type + MAX_HEIGHT + AspectRatio, data=na.omit(GR_savi_2018))
model_savi_test_2018_VegeType <- lm(MEAN~SHAPE_Area + new_GRtype + MAX_HEIGHT + AspectRatio, data=na.omit(GR_savi_2018))

selected_model_savi_2018_GRtype <- step(model_savi_test_2018_GRtype, direction = "both")
selected_model_savi_2018_VegeType <- step(model_savi_test_2018_VegeType, direction = "both")

# final model
summary(selected_model_savi_2018_GRtype)
summary(selected_model_savi_2018_VegeType)

### 2017 (growing season) ###
# model selection
model_savi_test_2017_GRtype <- lm(MEAN~SHAPE_Area + GR_type + MAX_HEIGHT + AspectRatio, data=GR_savi_2017)
model_savi_test_2017_VegeType <- lm(MEAN~SHAPE_Area + new_GRtype + MAX_HEIGHT + AspectRatio, data=GR_savi_2017)

selected_model_savi_2017_GRtype <- step(model_savi_test_2017_GRtype, direction = "both") 
selected_model_savi_2017_VegeType <- step(model_savi_test_2017_VegeType, direction = "both") 

# final model 
summary(selected_model_savi_2017_GRtype)
summary(selected_model_savi_2017_VegeType)

# GNDVI --------------------------------------------------------------------
### 2018 (pre-growing season) ###
# model selection
model_gndvi_test_2018_GRtype <- lm(MEAN~SHAPE_Area + GR_type + MAX_HEIGHT + AspectRatio, data=na.omit(GR_gndvi_2018))
model_gndvi_test_2018_VegeType <- lm(MEAN~SHAPE_Area + new_GRtype + MAX_HEIGHT + AspectRatio, data=na.omit(GR_gndvi_2018))

selected_model_gndvi_2018_GRtype <- step(model_gndvi_test_2018_GRtype, direction = "both")
selected_model_gndvi_2018_VegeType <- step(model_gndvi_test_2018_VegeType, direction = "both")

# final model
summary(selected_model_gndvi_2018_GRtype)
summary(selected_model_gndvi_2018_VegeType)

### 2017 (growing season) ###
# model selection
model_gndvi_test_2017_GRtype <- lm(MEAN~SHAPE_Area + GR_type + MAX_HEIGHT + AspectRatio, data=GR_gndvi_2017)
model_gndvi_test_2017_VegeType <- lm(MEAN~SHAPE_Area + new_GRtype + MAX_HEIGHT + AspectRatio, data=GR_gndvi_2017)

selected_model_gndvi_2017_GRtype <- step(model_gndvi_test_2017_GRtype, direction = "both")
selected_model_gndvi_2017_VegeType <- step(model_gndvi_test_2017_VegeType, direction = "both")

# final model
summary(selected_model_gndvi_2017_GRtype)
summary(selected_model_gndvi_2017_VegeType)

# CIg --------------------------------------------------------------------
### 2018 (pre-growing season) ###
# model selection
model_cig_test_2018_GRtype <- lm(MEAN~SHAPE_Area + GR_type + MAX_HEIGHT + AspectRatio, data=na.omit(GR_cig_2018))
model_cig_test_2018_VegeType <- lm(MEAN~SHAPE_Area + new_GRtype + MAX_HEIGHT + AspectRatio, data=na.omit(GR_cig_2018))

selected_model_cig_2018_GRtype <- step(model_cig_test_2018_GRtype, direction = "both") 
selected_model_cig_2018_VegeType <- step(model_cig_test_2018_VegeType, direction = "both")

# final model
summary(selected_model_cig_2018_GRtype)
summary(selected_model_cig_2018_VegeType)

### 2017 (growing season) ###
# model selection
model_cig_test_2017_GRtype <- lm(MEAN~SHAPE_Area + GR_type + MAX_HEIGHT + AspectRatio, data=GR_cig_2017)
model_cig_test_2017_VegeType <- lm(MEAN~SHAPE_Area + new_GRtype + MAX_HEIGHT + AspectRatio, data=GR_cig_2017)

selected_model_cig_2017_GRtype <- step(model_cig_test_2017_GRtype, direction = "both") 
selected_model_cig_2017_VegeType <- step(model_cig_test_2017_VegeType, direction = "both")

# final model
summary(selected_model_cig_2017_GRtype)
summary(selected_model_cig_2017_VegeType)

########### VIs: Linear model to select key design factors that influence CV of VIs ###########
# SAVI --------------------------------------------------------------------
### 2018 (pre-growing season) ###
# model selection
model_CV_savi_test_2018_GRtype <- lm(COV~SHAPE_Area + GR_type + MAX_HEIGHT + AspectRatio, data=na.omit(GR_savi_2018))
model_CV_savi_test_2018_VegeType <- lm(COV~SHAPE_Area + new_GRtype + MAX_HEIGHT + AspectRatio, data=na.omit(GR_savi_2018))

selected_model_CV_savi_2018_GRtype <- step(model_CV_savi_test_2018_GRtype, direction = "both") 
selected_model_CV_savi_2018_VegeType <- step(model_CV_savi_test_2018_VegeType, direction = "both")

# final model
summary(selected_model_CV_savi_2018_GRtype)
summary(selected_model_CV_savi_2018_VegeType)

### 2017 (growing season) ###
# model selection
model_CV_savi_test_2017_GRtype <- lm(COV~SHAPE_Area + GR_type + MAX_HEIGHT + AspectRatio, data=GR_savi_2017)
model_CV_savi_test_2017_VegeType <- lm(COV~SHAPE_Area + new_GRtype + MAX_HEIGHT + AspectRatio, data=GR_savi_2017)

selected_model_CV_savi_2017_GRtype <- step(model_CV_savi_test_2017_GRtype, direction = "both") 
selected_model_CV_savi_2017_VegeType <- step(model_CV_savi_test_2017_VegeType, direction = "both")

# final model
summary(selected_model_CV_savi_2017_GRtype)
summary(selected_model_CV_savi_2017_VegeType)

# GNDVI --------------------------------------------------------------------
### 2018 (pre-growing season) ###
# model selection
model_CV_gndvi_test_2018_GRtype <- lm(COV~SHAPE_Area + GR_type + MAX_HEIGHT + AspectRatio, data=na.omit(GR_gndvi_2018))
model_CV_gndvi_test_2018_VegeType <- lm(COV~SHAPE_Area + new_GRtype + MAX_HEIGHT + AspectRatio, data=na.omit(GR_gndvi_2018))

selected_model_CV_gndvi_2018_GRtype <- step(model_CV_gndvi_test_2018_GRtype, direction = "both") 
selected_model_CV_gndvi_2018_VegeType <- step(model_CV_gndvi_test_2018_VegeType, direction = "both") 

# final model
summary(selected_model_CV_gndvi_2018_GRtype)
summary(selected_model_CV_gndvi_2018_VegeType)

### 2017 (growing season) ###
# model selection
model_CV_gndvi_test_2017_GRtype <- lm(COV~SHAPE_Area + GR_type + MAX_HEIGHT + AspectRatio, data=GR_gndvi_2017)
model_CV_gndvi_test_2017_VegeType <- lm(COV~SHAPE_Area + new_GRtype + MAX_HEIGHT + AspectRatio, data=GR_gndvi_2017)

selected_model_CV_gndvi_2017_GRtype <- step(model_CV_gndvi_test_2017_GRtype, direction = "both")
selected_model_CV_gndvi_2017_VegeType <- step(model_CV_gndvi_test_2017_VegeType, direction = "both")

# final model
summary(selected_model_CV_gndvi_2017_GRtype)
summary(selected_model_CV_gndvi_2017_VegeType)

# CIg --------------------------------------------------------------------
### 2018 (pre-growing season) ###
# model selection
model_CV_cig_test_2018_GRtype <- lm(COV~SHAPE_Area + GR_type + MAX_HEIGHT + AspectRatio, data=na.omit(GR_cig_2018))
model_CV_cig_test_2018_VegeType <- lm(COV~SHAPE_Area + new_GRtype + MAX_HEIGHT + AspectRatio, data=na.omit(GR_cig_2018))

selected_model_CV_cig_2018_GRtype <- step(model_CV_cig_test_2018_GRtype, direction = "both")
selected_model_CV_cig_2018_VegeType <- step(model_CV_cig_test_2018_VegeType, direction = "both")

# final model 
summary(selected_model_CV_cig_2018_GRtype)
summary(selected_model_CV_cig_2018_VegeType)

### 2017 (growing season) ###
# model selection
model_CV_cig_test_2017_GRtype <- lm(COV~SHAPE_Area + GR_type + MAX_HEIGHT + AspectRatio, data=GR_cig_2017)
model_CV_cig_test_2017_VegeType <- lm(COV~SHAPE_Area + new_GRtype + MAX_HEIGHT + AspectRatio, data=GR_cig_2017)

selected_model_CV_cig_2017_GRtype <- step(model_CV_cig_test_2017_GRtype, direction = "both") 
selected_model_CV_cig_2017_VegeType <- step(model_CV_cig_test_2017_VegeType, direction = "both")

# final model
summary(selected_model_CV_cig_2017_GRtype)
summary(selected_model_CV_cig_2017_VegeType)


# Supplementary Figures S10 and S11: Mean SAVI, Mean GNDVI, Mean CIg vs. SQRT area (grouped by green roof type)  --------
# SAVI --------------------------------------------------------------------
### 2018 (pre-growing season) ###
# Change the level order
GR_savi_2018$GR_type <- factor(GR_savi_2018$GR_type, levels = c("ext","int"))

# plot
savi_area_2018.plot <- ggplot(GR_savi_2018, aes(x=sqrt_area,y=MEAN, color = GR_type, linetype = GR_type))+
  geom_point(aes(shape = GR_type), size=2.5, alpha = 0.3) +
  geom_smooth(aes(color = GR_type, fill=GR_type, linetype = GR_type),
              method='lm', formula= y~x, alpha = 0.3, cex=1.5)+
  # geom_smooth(aes(color = GR_type, fill = GR_type), method="lm", formula=y ~ log(x + 1e-6), se=TRUE, alpha = 0.3) +
  labs(x=expression(sqrt("Green roof area")~(m)), y="Mean SAVI",
       group = "GR_type", linetype = "GR_type", pch = "GR_type")+
  scale_color_manual(name="Green roof type",labels = c("Extensive","Intensive"),values = c("#FC7307","#236AB9"))+
  scale_fill_manual(name="Green roof type",labels = c("Extensive","Intensive"),values = c("#FC7307","#236AB9"))+
  scale_linetype_manual(name="Green roof type", labels = c("Extensive","Intensive"), values = c(1,1))+
  scale_shape_manual(name="Green roof type", labels = c("Extensive","Intensive"), values = c(19,17))+
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

savi_area_2018.plot

# Linear regression 
summary(lm(MEAN ~ sqrt_area, data = GR_savi_2018))

# Linear regression by green roof type
summary(lm(MEAN ~ sqrt_area, data = subset(GR_savi_2018, GR_type =="int")))
summary(lm(MEAN ~ sqrt_area, data = subset(GR_savi_2018, GR_type =="ext")))


### 2017 (growing season) ###
# Change the level order
GR_savi_2017$GR_type <- factor(GR_savi_2017$GR_type, levels = c("ext","int"))

# plot
savi_area_2017.plot <- ggplot(GR_savi_2017, aes(x=sqrt_area,y=MEAN, color = GR_type, linetype = GR_type))+
  geom_point(aes(shape = GR_type), size=2.5, alpha = 0.3) +
  geom_smooth(aes(color = GR_type, fill=GR_type, linetype = GR_type),
              method='lm', formula= y~x, alpha = 0.3, cex=1.5)+
  # geom_smooth(aes(color = GR_type, fill = GR_type), method="lm", formula=y ~ log(x + 1e-6), se=TRUE, alpha = 0.3) +
  labs(x=expression(sqrt("Green roof area")~(m)), y="Mean SAVI",
       group = "GR_type", linetype = "GR_type", pch = "GR_type")+
  scale_color_manual(name="Green roof type",labels = c("Extensive","Intensive"),values = c("#FC7307","#236AB9"))+
  scale_fill_manual(name="Green roof type",labels = c("Extensive","Intensive"),values = c("#FC7307","#236AB9"))+
  scale_linetype_manual(name="Green roof type", labels = c("Extensive","Intensive"), values = c(1,1))+
  scale_shape_manual(name="Green roof type", labels = c("Extensive","Intensive"), values = c(19,17))+
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

savi_area_2017.plot

# Linear regression 
summary(lm(MEAN ~ sqrt_area, data = GR_savi_2017))

# Linear regression by green roof type
summary(lm(MEAN ~ sqrt_area, data = subset(GR_savi_2017, GR_type =="int")))
summary(lm(MEAN ~ sqrt_area, data = subset(GR_savi_2017, GR_type =="ext")))


# GNDVI --------------------------------------------------------------------
### 2018 (pre-growing season) ###
# Change the level order
GR_gndvi_2018$GR_type <- factor(GR_gndvi_2018$GR_type, levels = c("ext","int"))

# plot
gndvi_area_2018.plot <- ggplot(GR_gndvi_2018, aes(x=sqrt_area,y=MEAN, color = GR_type, linetype = GR_type))+
  geom_point(aes(shape = GR_type), size=2.5, alpha = 0.3) +
  geom_smooth(aes(color = GR_type, fill=GR_type, linetype = GR_type),
              method='lm', formula= y~x, alpha = 0.3, cex=1.5)+
  labs(x=expression(sqrt("Green roof area")~(m)), y="Mean GNDVI",
       group = "GR_type", linetype = "GR_type", pch = "GR_type")+
  scale_color_manual(name="Green roof type",labels = c("Extensive","Intensive"),values = c("#FC7307","#236AB9"))+
  scale_fill_manual(name="Green roof type",labels = c("Extensive","Intensive"),values = c("#FC7307","#236AB9"))+
  scale_linetype_manual(name="Green roof type", labels = c("Extensive","Intensive"), values = c(1,1))+
  scale_shape_manual(name="Green roof type", labels = c("Extensive","Intensive"), values = c(19,17))+
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

gndvi_area_2018.plot

# Linear regression 
summary(lm(MEAN ~ sqrt_area, data = GR_gndvi_2018))

# Linear regression by green roof type
summary(lm(MEAN ~ sqrt_area, data = subset(GR_gndvi_2018, GR_type =="int")))
summary(lm(MEAN ~ sqrt_area, data = subset(GR_gndvi_2018, GR_type =="ext")))

### 2017 (growing season) ###
# Change the level order
GR_gndvi_2017$GR_type <- factor(GR_gndvi_2017$GR_type, levels = c("ext","int"))

# plot
gndvi_area_2017.plot <- ggplot(GR_gndvi_2017, aes(x=sqrt_area,y=MEAN, color = GR_type, linetype = GR_type))+
  geom_point(aes(shape = GR_type), size=2.5, alpha = 0.3) +
  geom_smooth(aes(color = GR_type, fill=GR_type, linetype = GR_type),
              method='lm', formula= y~x, alpha = 0.3, cex=1.5)+
  labs(x=expression(sqrt("Green roof area")~(m)), y="Mean GNDVI",
       group = "GR_type", linetype = "GR_type", pch = "GR_type")+
  scale_color_manual(name="Green roof type",labels = c("Extensive","Intensive"),values = c("#FC7307","#236AB9"))+
  scale_fill_manual(name="Green roof type",labels = c("Extensive","Intensive"),values = c("#FC7307","#236AB9"))+
  scale_linetype_manual(name="Green roof type", labels = c("Extensive","Intensive"), values = c(1,1))+
  scale_shape_manual(name="Green roof type", labels = c("Extensive","Intensive"), values = c(19,17))+
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

gndvi_area_2017.plot

# Linear regression 
summary(lm(MEAN ~ sqrt_area, data = GR_gndvi_2017))

# Linear regression by green roof type
summary(lm(MEAN ~ sqrt_area, data = subset(GR_gndvi_2017, GR_type =="int")))
summary(lm(MEAN ~ sqrt_area, data = subset(GR_gndvi_2017, GR_type =="ext")))


# CIg --------------------------------------------------------------------
### 2018 (pre-growing season) ###
# Change the level order
GR_cig_2018$GR_type <- factor(GR_cig_2018$GR_type, levels = c("ext","int"))

# plot
cig_area_2018.plot <- ggplot(GR_cig_2018, aes(x=sqrt_area,y=MEAN, color = GR_type, linetype = GR_type))+
  geom_point(aes(shape = GR_type), size=2.5, alpha = 0.3) +
  geom_smooth(aes(color = GR_type, fill=GR_type, linetype = GR_type),
              method='lm', formula= y~x, alpha = 0.3, cex=1.5)+
  # geom_smooth(aes(color = GR_type, fill = GR_type), method="lm", formula=y ~ log(x + 1e-6), se=TRUE, alpha = 0.3) +
  labs(x=expression(sqrt("Green roof area")~(m)), y="Mean CIg",
       group = "GR_type", linetype = "GR_type", pch = "GR_type")+
  scale_color_manual(name="Green roof type",labels = c("Extensive","Intensive"),values = c("#FC7307","#236AB9"))+
  scale_fill_manual(name="Green roof type",labels = c("Extensive","Intensive"),values = c("#FC7307","#236AB9"))+
  scale_linetype_manual(name="Green roof type", labels = c("Extensive","Intensive"), values = c(1,1))+
  scale_shape_manual(name="Green roof type", labels = c("Extensive","Intensive"), values = c(19,17))+
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

cig_area_2018.plot

# Linear regression 
summary(lm(MEAN ~ sqrt_area, data = GR_cig_2018))

# Linear regression by green roof type
summary(lm(MEAN ~ sqrt_area, data = subset(GR_cig_2018, GR_type =="int")))
summary(lm(MEAN ~ sqrt_area, data = subset(GR_cig_2018, GR_type =="ext")))

### 2017 (growing season) ###
# Change the level order
GR_cig_2017$GR_type <- factor(GR_cig_2017$GR_type, levels = c("ext","int"))

# plot
cig_area_2017.plot <- ggplot(GR_cig_2017, aes(x=sqrt_area,y=MEAN, color = GR_type, linetype = GR_type))+
  geom_point(aes(shape = GR_type), size=2.5, alpha = 0.3) +
  geom_smooth(aes(color = GR_type, fill=GR_type, linetype = GR_type),
              method='lm', formula= y~x, alpha = 0.3, cex=1.5)+
  # geom_smooth(aes(color = GR_type, fill = GR_type), method="lm", formula=y ~ log(x + 1e-6), se=TRUE, alpha = 0.3) +
  labs(x=expression(sqrt("Green roof area")~(m)), y="Mean CIg",
       group = "GR_type", linetype = "GR_type", pch = "GR_type")+
  scale_color_manual(name="Green roof type",labels = c("Extensive","Intensive"),values = c("#FC7307","#236AB9"))+
  scale_fill_manual(name="Green roof type",labels = c("Extensive","Intensive"),values = c("#FC7307","#236AB9"))+
  scale_linetype_manual(name="Green roof type", labels = c("Extensive","Intensive"), values = c(1,1))+
  scale_shape_manual(name="Green roof type", labels = c("Extensive","Intensive"), values = c(19,17))+
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

cig_area_2017.plot

# Linear regression 
summary(lm(MEAN ~ sqrt_area, data = GR_cig_2017))

# Linear regression by green roof type
summary(lm(MEAN ~ sqrt_area, data = subset(GR_cig_2017, GR_type =="int")))
summary(lm(MEAN ~ sqrt_area, data = subset(GR_cig_2017, GR_type =="ext")))

# Supplementary Figures S10 and S11: Mean SAVI, Mean GNDVI, Mean CIg vs. Building Height (grouped by green roof type)  --------
# SAVI --------------------------------------------------------------------
### 2018 (pre-growing season) ###
# Change the level order
GR_savi_2018$GR_type <- factor(GR_savi_2018$GR_type, levels = c("ext","int"))

# plot
savi_height_2018.plot <- ggplot(GR_savi_2018, aes(x=MAX_HEIGHT,y=MEAN, color = GR_type, linetype = GR_type))+
  geom_point(aes(shape = GR_type), size=2.5, alpha = 0.3) +
  geom_smooth(aes(color = GR_type, fill=GR_type, linetype = GR_type),
              method='lm', formula= y~x, alpha = 0.3, cex=1.5)+
  labs(x="Building height (m)", y="Mean SAVI",
       group = "GR_type", linetype = "GR_type", pch = "GR_type")+
  scale_color_manual(name="Green roof type",labels = c("Extensive","Intensive"),values = c("#FC7307","#236AB9"))+
  scale_fill_manual(name="Green roof type",labels = c("Extensive","Intensive"),values = c("#FC7307","#236AB9"))+
  scale_linetype_manual(name="Green roof type", labels = c("Extensive","Intensive"), values = c(1,1))+
  scale_shape_manual(name="Green roof type", labels = c("Extensive","Intensive"), values = c(19,17))+
  scale_x_continuous(limits = c(0, 180))+
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

savi_height_2018.plot

# Linear regression 
summary(lm(MEAN ~ MAX_HEIGHT, data = GR_savi_2018))

# Linear regression by green roof type
summary(lm(MEAN ~ MAX_HEIGHT, data = subset(GR_savi_2018, GR_type =="int")))
summary(lm(MEAN ~ MAX_HEIGHT, data = subset(GR_savi_2018, GR_type =="ext")))


### 2017 (growing season) ###
# Change the level order
GR_savi_2017$GR_type <- factor(GR_savi_2017$GR_type, levels = c("ext","int"))

# plot
savi_height_2017.plot <- ggplot(GR_savi_2017, aes(x=MAX_HEIGHT,y=MEAN, color = GR_type, linetype = GR_type))+
  geom_point(aes(shape = GR_type), size=2.5, alpha = 0.3) +
  geom_smooth(aes(color = GR_type, fill=GR_type, linetype = GR_type),
              method='lm', formula= y~x, alpha = 0.3, cex=1.5)+
  labs(x="Building height (m)", y="Mean SAVI",
       group = "GR_type", linetype = "GR_type", pch = "GR_type")+
  scale_color_manual(name="Green roof type",labels = c("Extensive","Intensive"),values = c("#FC7307","#236AB9"))+
  scale_fill_manual(name="Green roof type",labels = c("Extensive","Intensive"),values = c("#FC7307","#236AB9"))+
  scale_linetype_manual(name="Green roof type", labels = c("Extensive","Intensive"), values = c(1,2))+
  scale_shape_manual(name="Green roof type", labels = c("Extensive","Intensive"), values = c(19,17))+
  scale_x_continuous(limits = c(0, 180))+
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

savi_height_2017.plot

# Linear regression 
summary(lm(MEAN ~ MAX_HEIGHT, data = GR_savi_2017))

# Linear regression by green roof type
summary(lm(MEAN ~ MAX_HEIGHT, data = subset(GR_savi_2017, GR_type =="int")))
summary(lm(MEAN ~ MAX_HEIGHT, data = subset(GR_savi_2017, GR_type =="ext")))


# GNDVI --------------------------------------------------------------------
### 2018 (pre-growing season) ###
# Change the level order
GR_gndvi_2018$GR_type <- factor(GR_gndvi_2018$GR_type, levels = c("ext","int"))

# plot
gndvi_height_2018.plot <- ggplot(GR_gndvi_2018, aes(x=MAX_HEIGHT,y=MEAN, color = GR_type, linetype = GR_type))+
  geom_point(aes(shape = GR_type), size=2.5, alpha = 0.3) +
  geom_smooth(aes(color = GR_type, fill=GR_type, linetype = GR_type),
              method='lm', formula= y~x, alpha = 0.3, cex=1.5)+
  labs(x="Building height (m)", y="Mean GNDVI",
       group = "GR_type", linetype = "GR_type", pch = "GR_type")+
  scale_color_manual(name="Green roof type",labels = c("Extensive","Intensive"),values = c("#FC7307","#236AB9"))+
  scale_fill_manual(name="Green roof type",labels = c("Extensive","Intensive"),values = c("#FC7307","#236AB9"))+
  scale_linetype_manual(name="Green roof type", labels = c("Extensive","Intensive"), values = c(1,1))+
  scale_shape_manual(name="Green roof type", labels = c("Extensive","Intensive"), values = c(19,17))+
  scale_x_continuous(limits = c(0, 180))+
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

gndvi_height_2018.plot

# Linear regression 
summary(lm(MEAN ~ MAX_HEIGHT, data = GR_gndvi_2018))

# Linear regression by green roof type
summary(lm(MEAN ~ MAX_HEIGHT, data = subset(GR_gndvi_2018, GR_type =="int")))
summary(lm(MEAN ~ MAX_HEIGHT, data = subset(GR_gndvi_2018, GR_type =="ext")))


### 2017 (growing season) ###
# Change the level order
GR_gndvi_2017$GR_type <- factor(GR_gndvi_2017$GR_type, levels = c("ext","int"))

# plot
gndvi_height_2017.plot <- ggplot(GR_gndvi_2017, aes(x=MAX_HEIGHT,y=MEAN, color = GR_type, linetype = GR_type))+
  geom_point(aes(shape = GR_type), size=2.5, alpha = 0.3) +
  geom_smooth(aes(color = GR_type, fill=GR_type, linetype = GR_type),
              method='lm', formula= y~x, alpha = 0.3, cex=1.5)+
  labs(x="Building height (m)", y="Mean GNDVI",
       group = "GR_type", linetype = "GR_type", pch = "GR_type")+
  scale_color_manual(name="Green roof type",labels = c("Extensive","Intensive"),values = c("#FC7307","#236AB9"))+
  scale_fill_manual(name="Green roof type",labels = c("Extensive","Intensive"),values = c("#FC7307","#236AB9"))+
  scale_linetype_manual(name="Green roof type", labels = c("Extensive","Intensive"), values = c(1,2))+
  scale_shape_manual(name="Green roof type", labels = c("Extensive","Intensive"), values = c(19,17))+
  scale_x_continuous(limits = c(0, 180))+
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

gndvi_height_2017.plot

# Linear regression 
summary(lm(MEAN ~ MAX_HEIGHT, data = GR_gndvi_2017))

# Linear regression by green roof type
summary(lm(MEAN ~ MAX_HEIGHT, data = subset(GR_gndvi_2017, GR_type =="int")))
summary(lm(MEAN ~ MAX_HEIGHT, data = subset(GR_gndvi_2017, GR_type =="ext")))


# CIg --------------------------------------------------------------------
### 2018 (pre-growing season) ###
# Change the level order
GR_cig_2018$GR_type <- factor(GR_cig_2018$GR_type, levels = c("ext","int"))

# plot 
cig_height_2018.plot <- ggplot(GR_cig_2018, aes(x=MAX_HEIGHT,y=MEAN, color = GR_type, linetype = GR_type))+
  geom_point(aes(shape = GR_type), size=2.5, alpha = 0.3) +
  geom_smooth(aes(color = GR_type, fill=GR_type, linetype = GR_type),
              method='lm', formula= y~x, alpha = 0.3, cex=1.5)+
  labs(x="Building height (m)", y="Mean CIg",
       group = "GR_type", linetype = "GR_type", pch = "GR_type")+
  scale_color_manual(name="Green roof type",labels = c("Extensive","Intensive"),values = c("#FC7307","#236AB9"))+
  scale_fill_manual(name="Green roof type",labels = c("Extensive","Intensive"),values = c("#FC7307","#236AB9"))+
  scale_linetype_manual(name="Green roof type", labels = c("Extensive","Intensive"), values = c(1,1))+
  scale_shape_manual(name="Green roof type", labels = c("Extensive","Intensive"), values = c(19,17))+
  scale_x_continuous(limits = c(0, 180))+
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

cig_height_2018.plot

# Linear regression 
summary(lm(MEAN ~ MAX_HEIGHT, data = GR_cig_2018))

# Linear regression by green roof type
summary(lm(MEAN ~ MAX_HEIGHT, data = subset(GR_cig_2018, GR_type =="int")))
summary(lm(MEAN ~ MAX_HEIGHT, data = subset(GR_cig_2018, GR_type =="ext")))

### 2017 (growing season) ###
# Change the level order
GR_cig_2017$GR_type <- factor(GR_cig_2017$GR_type, levels = c("ext","int"))

# plot
cig_height_2017.plot <- ggplot(GR_cig_2017, aes(x=MAX_HEIGHT,y=MEAN, color = GR_type, linetype = GR_type))+
  geom_point(aes(shape = GR_type), size=2.5, alpha = 0.3) +
  geom_smooth(aes(color = GR_type, fill=GR_type, linetype = GR_type),
              method='lm', formula= y~x, alpha = 0.3, cex=1.5)+
  labs(x="Building height (m)", y="Mean CIg",
       group = "GR_type", linetype = "GR_type", pch = "GR_type")+
  scale_color_manual(name="Green roof type",labels = c("Extensive","Intensive"),values = c("#FC7307","#236AB9"))+
  scale_fill_manual(name="Green roof type",labels = c("Extensive","Intensive"),values = c("#FC7307","#236AB9"))+
  scale_linetype_manual(name="Green roof type", labels = c("Extensive","Intensive"), values = c(1,1))+
  scale_shape_manual(name="Green roof type", labels = c("Extensive","Intensive"), values = c(19,17))+
  scale_x_continuous(limits = c(0, 180))+
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

cig_height_2017.plot

# Linear regression 
summary(lm(MEAN ~ MAX_HEIGHT, data = GR_cig_2017))

# Linear regression by green roof type
summary(lm(MEAN ~ MAX_HEIGHT, data = subset(GR_cig_2017, GR_type =="int")))
summary(lm(MEAN ~ MAX_HEIGHT, data = subset(GR_cig_2017, GR_type =="ext")))

# Supplementary Figures S10 and S11: Mean SAVI, Mean GNDVI, Mean CIg vs. Green roof type  --------
# SAVI --------------------------------------------------------------------
### 2018 (pre-growing season) ###
summary_GR_savi_2018 <- GR_savi_2018 %>%
  group_by(GR_type) %>%
  summarise(mean_SAVI = mean(MEAN, na.rm = TRUE),
            se_SAVI = get_se(MEAN))

# Change the level order
summary_GR_savi_2018$GR_type <- factor(summary_GR_savi_2018$GR_type, levels = c("ext","int"))

# plot
savi_GRtype_barplot_2018 <- ggplot(summary_GR_savi_2018, aes(x = GR_type, y = mean_SAVI, fill = GR_type)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin = mean_SAVI - se_SAVI, ymax = mean_SAVI + se_SAVI),
                position = position_dodge(0.9), width = 0.3) +
  scale_x_discrete(name="Green roof type",labels = c("Extensive", "Intensive"))+
  scale_fill_manual(name="Green roof type",labels = c("Extensive", "Intensive"),values = c("#FC7307","#236AB9"))+
  labs(x = "Green roof type", y = "Mean SAVI",
       group = "GR_type")+
  coord_cartesian(ylim=c(0,0.15))+
  theme_bw(base_size = 28) + 
  theme(
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text=element_text(size=28, color = "black"),
    axis.title=element_text(size=32, color = "black"),
    legend.position = "none",
    legend.title = element_text(size = 32)
  )

print(savi_GRtype_barplot_2018)

# t-test
t.test(MEAN ~ GR_type, data = GR_savi_2018, var.equal = TRUE)

### 2017 (growing season) ###
summary_GR_savi_2017 <- GR_savi_2017 %>%
  group_by(GR_type) %>%
  summarise(mean_SAVI = mean(MEAN, na.rm = TRUE),
            se_SAVI = get_se(MEAN))

# Change the level order
summary_GR_savi_2017$GR_type <- factor(summary_GR_savi_2017$GR_type, levels = c("ext","int"))

# plot
savi_GRtype_barplot_2017 <- ggplot(summary_GR_savi_2017, aes(x = GR_type, y = mean_SAVI, fill = GR_type)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin = mean_SAVI - se_SAVI, ymax = mean_SAVI + se_SAVI),
                position = position_dodge(0.9), width = 0.3) +
  scale_x_discrete(name="Green roof type",labels = c("Extensive", "Intensive"))+
  scale_fill_manual(name="Green roof type",labels = c("Extensive", "Intensive"),values = c("#FC7307","#236AB9"))+
  labs(x = "Green roof type", y = "Mean SAVI",
       group = "GR_type")+
  coord_cartesian(ylim=c(0,0.2))+
  theme_bw(base_size = 28) + 
  theme(
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text=element_text(size=28, color = "black"),
    axis.title=element_text(size=32, color = "black"),
    legend.position = "none",
    legend.title = element_text(size = 32)
  )

print(savi_GRtype_barplot_2017)

# t-test
t.test(MEAN ~ GR_type, data = GR_savi_2017, var.equal = TRUE)

# GNDVI --------------------------------------------------------------------
### 2018 (pre-growing season) ###
summary_GR_gndvi_2018 <- GR_gndvi_2018 %>%
  group_by(GR_type) %>%
  summarise(mean_GNDVI = mean(MEAN, na.rm = TRUE),
            se_GNDVI = get_se(MEAN))

# Change the level order
summary_GR_gndvi_2018$GR_type <- factor(summary_GR_gndvi_2018$GR_type, levels = c("ext","int"))

# plot
gndvi_GRtype_barplot_2018 <- ggplot(summary_GR_gndvi_2018, aes(x = GR_type, y = mean_GNDVI, fill = GR_type)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin = mean_GNDVI - se_GNDVI, ymax = mean_GNDVI + se_GNDVI),
                position = position_dodge(0.9), width = 0.3) +
  scale_x_discrete(name="Green roof type",labels = c("Extensive", "Intensive"))+
  scale_fill_manual(name="Green roof type",labels = c("Extensive", "Intensive"),values = c("#FC7307","#236AB9"))+
  labs(x = "Green roof type", y = "Mean GNDVI",
       group = "GR_type")+
  coord_cartesian(ylim=c(0,0.25))+
  theme_bw(base_size = 28) + 
  theme(
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text=element_text(size=28, color = "black"),
    axis.title=element_text(size=32, color = "black"),
    legend.position = "none",
    legend.title = element_text(size = 32)
  )

print(gndvi_GRtype_barplot_2018)

# t-test
t.test(MEAN ~ GR_type, data = GR_gndvi_2018, var.equal = TRUE)

### 2017 (growing season) ###
summary_GR_gndvi_2017 <- GR_gndvi_2017 %>%
  group_by(GR_type) %>%
  summarise(mean_GNDVI = mean(MEAN, na.rm = TRUE),
            se_GNDVI = get_se(MEAN))

# Change the level order
summary_GR_gndvi_2017$GR_type <- factor(summary_GR_gndvi_2017$GR_type, levels = c("ext","int"))

# plot
gndvi_GRtype_barplot_2017 <- ggplot(summary_GR_gndvi_2017, aes(x = GR_type, y = mean_GNDVI, fill = GR_type)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin = mean_GNDVI - se_GNDVI, ymax = mean_GNDVI + se_GNDVI),
                position = position_dodge(0.9), width = 0.3) +
  scale_x_discrete(name="Green roof type",labels = c("Extensive", "Intensive"))+
  scale_fill_manual(name="Green roof type",labels = c("Extensive", "Intensive"),values = c("#FC7307","#236AB9"))+
  labs(x = "Green roof type", y = "Mean GNDVI",
       group = "GR_type")+
  coord_cartesian(ylim=c(0,0.3))+
  theme_bw(base_size = 28) + 
  theme(
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text=element_text(size=28, color = "black"),
    axis.title=element_text(size=32, color = "black"),
    legend.position = "none",
    legend.title = element_text(size = 32)
  )

print(gndvi_GRtype_barplot_2017)

# t-test
t.test(MEAN ~ GR_type, data = GR_gndvi_2017, var.equal = TRUE)

# CIg --------------------------------------------------------------------
### 2018 (pre-growing season) ###
summary_GR_cig_2018 <- GR_cig_2018 %>%
  group_by(GR_type) %>%
  summarise(mean_CIg = mean(MEAN, na.rm = TRUE),
            se_CIg = get_se(MEAN))

# Change the level order
summary_GR_cig_2018$GR_type <- factor(summary_GR_cig_2018$GR_type, levels = c("ext","int"))

# plot
cig_GRtype_barplot_2018 <- ggplot(summary_GR_cig_2018, aes(x = GR_type, y = mean_CIg, fill = GR_type)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin = mean_CIg - se_CIg, ymax = mean_CIg + se_CIg),
                position = position_dodge(0.9), width = 0.3) +
  scale_x_discrete(name="Green roof type",labels = c("Extensive", "Intensive"))+
  scale_fill_manual(name="Green roof type",labels = c("Extensive", "Intensive"),values = c("#FC7307","#236AB9"))+
  labs(x = "Green roof type", y = "Mean CIg",
       group = "GR_type")+
  coord_cartesian(ylim=c(0,0.65))+
  theme_bw(base_size = 28) + 
  theme(
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text=element_text(size=28, color = "black"),
    axis.title=element_text(size=32, color = "black"),
    legend.position = "none",
    legend.title = element_text(size = 32)
  )

print(cig_GRtype_barplot_2018)

# t-test
t.test(MEAN ~ GR_type, data = GR_cig_2018, var.equal = TRUE)

### 2017 (growing season) ###
summary_GR_cig_2017 <- GR_cig_2017 %>%
  group_by(GR_type) %>%
  summarise(mean_CIg = mean(MEAN, na.rm = TRUE),
            se_CIg = get_se(MEAN))

# Change the level order
summary_GR_cig_2017$GR_type <- factor(summary_GR_cig_2017$GR_type, levels = c("ext","int"))

# plot
cig_GRtype_barplot_2017 <- ggplot(summary_GR_cig_2017, aes(x = GR_type, y = mean_CIg, fill = GR_type)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin = mean_CIg - se_CIg, ymax = mean_CIg + se_CIg),
                position = position_dodge(0.9), width = 0.3) +
  scale_x_discrete(name="Green roof type",labels = c("Extensive", "Intensive"))+
  scale_fill_manual(name="Green roof type",labels = c("Extensive", "Intensive"),values = c("#FC7307","#236AB9"))+
  labs(x = "Green roof type", y = "Mean CIg",
       group = "GR_type")+
  coord_cartesian(ylim=c(0,0.9))+
  theme_bw(base_size = 28) + 
  theme(
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text=element_text(size=28, color = "black"),
    axis.title=element_text(size=32, color = "black"),
    legend.position = "none",
    legend.title = element_text(size = 32)
  )

print(cig_GRtype_barplot_2017)

# t-test
t.test(MEAN ~ GR_type, data = GR_cig_2017, var.equal = TRUE)

# Supplementary Figures S10 and S11: Mean SAVI, Mean GNDVI, Mean CIg vs. SQRT Aspect ratio (grouped by green roof type)  --------
# SAVI --------------------------------------------------------------------
### 2018 (pre-growing season) ###
# change order of display
GR_savi_2018$GR_type <- factor(GR_savi_2018$GR_type, levels = c("ext", "int"))

# plot
savi_sqrtAR_GRtype_2018.plot <- ggplot(GR_savi_2018, aes(x=sqrt_AR,y=MEAN, color = GR_type, linetype = GR_type))+
  geom_point(aes(shape = GR_type), size=2.5, alpha = 0.3) +
  geom_smooth(aes(color = GR_type, fill=GR_type, linetype = GR_type),
              method='lm', formula= y~x, alpha = 0.3, cex=1.5)+
  labs(x=expression(sqrt("Aspect ratio")), y="Mean SAVI",
       group = "GR_type", linetype = "GR_type", pch = "GR_type")+
  scale_color_manual(name="Green roof type",labels = c("Extensive", "Intensive"),values = c("#FC7307","#236AB9"))+
  scale_fill_manual(name="Green roof type",labels = c("Extensive", "Intensive"),values = c("#FC7307","#236AB9"))+
  scale_linetype_manual(name="Green roof type", labels = c("Extensive", "Intensive"), values = c(1,1))+
  scale_shape_manual(name="Green roof type", labels = c("Extensive", "Intensive"), values = c(19,17)) +
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

savi_sqrtAR_GRtype_2018.plot

# Linear regression 
summary(lm(MEAN ~ sqrt_AR, data = GR_savi_2018))

# Linear regression by green roof type
summary(lm(MEAN ~ sqrt_AR, data = subset(GR_savi_2018, GR_type =="int")))
summary(lm(MEAN ~ sqrt_AR, data = subset(GR_savi_2018, GR_type =="ext")))

### 2017 (growing season) ###
# change order of display
GR_savi_2017$GR_type <- factor(GR_savi_2017$GR_type, levels = c("ext", "int"))

# plot
savi_sqrtAR_GRtype_2017.plot <- ggplot(GR_savi_2017, aes(x=sqrt_AR,y=MEAN, color = GR_type, linetype = GR_type))+
  geom_point(aes(shape = GR_type), size=2.5, alpha = 0.3) +
  geom_smooth(aes(color = GR_type, fill=GR_type, linetype = GR_type),
              method='lm', formula= y~x, alpha = 0.3, cex=1.5)+
  labs(x=expression(sqrt("Aspect ratio")), y="Mean SAVI",
       group = "GR_type", linetype = "GR_type", pch = "GR_type")+
  scale_color_manual(name="Green roof type",labels = c("Extensive", "Intensive"),values = c("#FC7307","#236AB9"))+
  scale_fill_manual(name="Green roof type",labels = c("Extensive", "Intensive"),values = c("#FC7307","#236AB9"))+
  scale_linetype_manual(name="Green roof type", labels = c("Extensive", "Intensive"), values = c(2,1))+
  scale_shape_manual(name="Green roof type", labels = c("Extensive", "Intensive"), values = c(19,17)) +
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

savi_sqrtAR_GRtype_2017.plot

# Linear regression 
summary(lm(MEAN ~ sqrt_AR, data = GR_savi_2017))

# Linear regression by green roof type
summary(lm(MEAN ~ sqrt_AR, data = subset(GR_savi_2017, GR_type =="int")))
summary(lm(MEAN ~ sqrt_AR, data = subset(GR_savi_2017, GR_type =="ext")))

# GNDVI --------------------------------------------------------------------
### 2018 (pre-growing season) ###
# change order of display
GR_gndvi_2018$GR_type <- factor(GR_gndvi_2018$GR_type, levels = c("ext", "int"))

# plot
gndvi_sqrtAR_GRtype_2018.plot <- ggplot(GR_gndvi_2018, aes(x=sqrt_AR,y=MEAN, color = GR_type, linetype = GR_type))+
  geom_point(aes(shape = GR_type), size=2.5, alpha = 0.3) +
  geom_smooth(aes(color = GR_type, fill=GR_type, linetype = GR_type),
              method='lm', formula= y~x, alpha = 0.3, cex=1.5)+
  labs(x=expression(sqrt("Aspect ratio")), y="Mean GNDVI",
       group = "GR_type", linetype = "GR_type", pch = "GR_type")+
  scale_color_manual(name="Green roof type",labels = c("Extensive", "Intensive"),values = c("#FC7307","#236AB9"))+
  scale_fill_manual(name="Green roof type",labels = c("Extensive", "Intensive"),values = c("#FC7307","#236AB9"))+
  scale_linetype_manual(name="Green roof type", labels = c("Extensive", "Intensive"), values = c(1,1))+
  scale_shape_manual(name="Green roof type", labels = c("Extensive", "Intensive"), values = c(19,17)) +
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

gndvi_sqrtAR_GRtype_2018.plot

# Linear regression 
summary(lm(MEAN ~ sqrt_AR, data = GR_gndvi_2018))

# Linear regression by green roof type
summary(lm(MEAN ~ sqrt_AR, data = subset(GR_gndvi_2018, GR_type =="int")))
summary(lm(MEAN ~ sqrt_AR, data = subset(GR_gndvi_2018, GR_type =="ext")))

### 2017 (growing season) ###
# change order of display
GR_gndvi_2017$GR_type <- factor(GR_gndvi_2017$GR_type, levels = c("ext", "int"))

# plot
gndvi_sqrtAR_GRtype_2017.plot <- ggplot(GR_gndvi_2017, aes(x=sqrt_AR,y=MEAN, color = GR_type, linetype = GR_type))+
  geom_point(aes(shape = GR_type), size=2.5, alpha = 0.3) +
  geom_smooth(aes(color = GR_type, fill=GR_type, linetype = GR_type),
              method='lm', formula= y~x, alpha = 0.3, cex=1.5)+
  labs(x=expression(sqrt("Aspect ratio")), y="Mean GNDVI",
       group = "GR_type", linetype = "GR_type", pch = "GR_type")+
  scale_color_manual(name="Green roof type",labels = c("Extensive", "Intensive"),values = c("#FC7307","#236AB9"))+
  scale_fill_manual(name="Green roof type",labels = c("Extensive", "Intensive"),values = c("#FC7307","#236AB9"))+
  scale_linetype_manual(name="Green roof type", labels = c("Extensive", "Intensive"), values = c(2,1))+
  scale_shape_manual(name="Green roof type", labels = c("Extensive", "Intensive"), values = c(19,17)) +
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

gndvi_sqrtAR_GRtype_2017.plot

# Linear regression 
summary(lm(MEAN ~ sqrt_AR, data = GR_gndvi_2017))

# Linear regression by green roof type
summary(lm(MEAN ~ sqrt_AR, data = subset(GR_gndvi_2017, GR_type =="int")))
summary(lm(MEAN ~ sqrt_AR, data = subset(GR_gndvi_2017, GR_type =="ext")))


# CIg --------------------------------------------------------------------
### 2018 (pre-growing season) ###
# change order of display
GR_cig_2018$GR_type <- factor(GR_cig_2018$GR_type, levels = c("ext", "int"))

# plot
cig_sqrtAR_GRtype_2018.plot <- ggplot(GR_cig_2018, aes(x=sqrt_AR,y=MEAN, color = GR_type, linetype = GR_type))+
  geom_point(aes(shape = GR_type), size=2.5, alpha = 0.3) +
  geom_smooth(aes(color = GR_type, fill=GR_type, linetype = GR_type),
              method='lm', formula= y~x, alpha = 0.3, cex=1.5)+
  labs(x=expression(sqrt("Aspect ratio")), y="Mean CIg",
       group = "GR_type", linetype = "GR_type", pch = "GR_type")+
  scale_color_manual(name="Green roof type",labels = c("Extensive", "Intensive"),values = c("#FC7307","#236AB9"))+
  scale_fill_manual(name="Green roof type",labels = c("Extensive", "Intensive"),values = c("#FC7307","#236AB9"))+
  scale_linetype_manual(name="Green roof type", labels = c("Extensive", "Intensive"), values = c(1,1))+
  scale_shape_manual(name="Green roof type", labels = c("Extensive", "Intensive"), values = c(19,17)) +
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

cig_sqrtAR_GRtype_2018.plot

# Linear regression 
summary(lm(MEAN ~ sqrt_AR, data = GR_cig_2018))

# Linear regression by green roof type
summary(lm(MEAN ~ sqrt_AR, data = subset(GR_cig_2018, GR_type =="int")))
summary(lm(MEAN ~ sqrt_AR, data = subset(GR_cig_2018, GR_type =="ext")))

### 2017 (growing season) ###
# change order of display
GR_cig_2017$GR_type <- factor(GR_cig_2017$GR_type, levels = c("ext", "int"))

# plot
cig_sqrtAR_GRtype_2017.plot <- ggplot(GR_cig_2017, aes(x=sqrt_AR,y=MEAN, color = GR_type, linetype = GR_type))+
  geom_point(aes(shape = GR_type), size=2.5, alpha = 0.3) +
  geom_smooth(aes(color = GR_type, fill=GR_type, linetype = GR_type),
              method='lm', formula= y~x, alpha = 0.3, cex=1.5)+
  labs(x=expression(sqrt("Aspect ratio")), y="Mean CIg",
       group = "GR_type", linetype = "GR_type", pch = "GR_type")+
  scale_color_manual(name="Green roof type",labels = c("Extensive", "Intensive"),values = c("#FC7307","#236AB9"))+
  scale_fill_manual(name="Green roof type",labels = c("Extensive", "Intensive"),values = c("#FC7307","#236AB9"))+
  scale_linetype_manual(name="Green roof type", labels = c("Extensive", "Intensive"), values = c(2,1))+
  scale_shape_manual(name="Green roof type", labels = c("Extensive", "Intensive"), values = c(19,17)) +
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

cig_sqrtAR_GRtype_2017.plot

# Linear regression 
summary(lm(MEAN ~ sqrt_AR, data = GR_cig_2017))

# Linear regression by green roof type
summary(lm(MEAN ~ sqrt_AR, data = subset(GR_cig_2017, GR_type =="int")))
summary(lm(MEAN ~ sqrt_AR, data = subset(GR_cig_2017, GR_type =="ext")))

# Combine Supplementary Figure S10: Mean VIs grouped by green roof type (growing season) --------
merge_mean_VIs_design_2017 = 
  savi_area_2017.plot + savi_height_2017.plot + savi_GRtype_barplot_2017 + savi_sqrtAR_GRtype_2017.plot +
  gndvi_area_2017.plot + gndvi_height_2017.plot + gndvi_GRtype_barplot_2017 + gndvi_sqrtAR_GRtype_2017.plot +
  cig_area_2017.plot + cig_height_2017.plot + cig_GRtype_barplot_2017 + cig_sqrtAR_GRtype_2017.plot +
  plot_layout(guides = "collect", ncol = 4) & theme(legend.position = "top")

merge_mean_VIs_design_2017[[1]] = merge_mean_VIs_design_2017[[1]] + theme(axis.title.x = element_blank())
merge_mean_VIs_design_2017[[2]] = merge_mean_VIs_design_2017[[2]] + theme(legend.position = "none", axis.title.y = element_blank(), axis.title.x = element_blank())
merge_mean_VIs_design_2017[[3]] = merge_mean_VIs_design_2017[[3]] + theme(legend.position = "none", axis.title.y = element_blank(), axis.title.x = element_blank())
merge_mean_VIs_design_2017[[4]] = merge_mean_VIs_design_2017[[4]] + theme(legend.position = "none", axis.title.y = element_blank(), axis.title.x = element_blank())
merge_mean_VIs_design_2017[[5]] = merge_mean_VIs_design_2017[[5]] + theme(axis.title.x = element_blank())
merge_mean_VIs_design_2017[[6]] = merge_mean_VIs_design_2017[[6]] + theme(legend.position = "none", axis.title.y = element_blank(), axis.title.x = element_blank())
merge_mean_VIs_design_2017[[7]] = merge_mean_VIs_design_2017[[7]] + theme(legend.position = "none", axis.title.y = element_blank(), axis.title.x = element_blank())
merge_mean_VIs_design_2017[[8]] = merge_mean_VIs_design_2017[[8]] + theme(legend.position = "none", axis.title.y = element_blank(), axis.title.x = element_blank())
merge_mean_VIs_design_2017[[9]] = merge_mean_VIs_design_2017[[9]] + theme(legend.position = "none")
merge_mean_VIs_design_2017[[10]] = merge_mean_VIs_design_2017[[10]] + theme(legend.position = "none", axis.title.y = element_blank())
merge_mean_VIs_design_2017[[11]] = merge_mean_VIs_design_2017[[11]] + theme(legend.position = "none", axis.title.y = element_blank())
merge_mean_VIs_design_2017[[12]] = merge_mean_VIs_design_2017[[12]] + theme(legend.position = "none", axis.title.y = element_blank())

merge_mean_VIs_design_2017

# ggsave(plot=merge_mean_VIs_design_2017, filename = "path/VIs_DesignFactors_GRtype_2017.tif", width = 24, height = 16, device='tiff', dpi=600,unit="in")


# Combine Supplementary Figure S11: Mean VIs grouped by green roof type (pre-growing season) --------
merge_mean_VIs_design_2018 = 
  savi_area_2018.plot + savi_height_2018.plot + savi_GRtype_barplot_2018 + savi_sqrtAR_GRtype_2018.plot +
  gndvi_area_2018.plot + gndvi_height_2018.plot + gndvi_GRtype_barplot_2018 + gndvi_sqrtAR_GRtype_2018.plot +
  cig_area_2018.plot + cig_height_2018.plot + cig_GRtype_barplot_2018 + cig_sqrtAR_GRtype_2018.plot +
  plot_layout(guides = "collect", ncol = 4) & theme(legend.position = "top")

merge_mean_VIs_design_2018[[1]] = merge_mean_VIs_design_2018[[1]] + theme(axis.title.x = element_blank())
merge_mean_VIs_design_2018[[2]] = merge_mean_VIs_design_2018[[2]] + theme(legend.position = "none", axis.title.y = element_blank(), axis.title.x = element_blank())
merge_mean_VIs_design_2018[[3]] = merge_mean_VIs_design_2018[[3]] + theme(legend.position = "none", axis.title.y = element_blank(), axis.title.x = element_blank())
merge_mean_VIs_design_2018[[4]] = merge_mean_VIs_design_2018[[4]] + theme(legend.position = "none", axis.title.y = element_blank(), axis.title.x = element_blank())
merge_mean_VIs_design_2018[[5]] = merge_mean_VIs_design_2018[[5]] + theme(axis.title.x = element_blank())
merge_mean_VIs_design_2018[[6]] = merge_mean_VIs_design_2018[[6]] + theme(legend.position = "none", axis.title.y = element_blank(), axis.title.x = element_blank())
merge_mean_VIs_design_2018[[7]] = merge_mean_VIs_design_2018[[7]] + theme(legend.position = "none", axis.title.y = element_blank(), axis.title.x = element_blank())
merge_mean_VIs_design_2018[[8]] = merge_mean_VIs_design_2018[[8]] + theme(legend.position = "none", axis.title.y = element_blank(), axis.title.x = element_blank())
merge_mean_VIs_design_2018[[9]] = merge_mean_VIs_design_2018[[9]] + theme(legend.position = "none")
merge_mean_VIs_design_2018[[10]] = merge_mean_VIs_design_2018[[10]] + theme(legend.position = "none", axis.title.y = element_blank())
merge_mean_VIs_design_2018[[11]] = merge_mean_VIs_design_2018[[11]] + theme(legend.position = "none", axis.title.y = element_blank())
merge_mean_VIs_design_2018[[12]] = merge_mean_VIs_design_2018[[12]] + theme(legend.position = "none", axis.title.y = element_blank())

merge_mean_VIs_design_2018

# ggsave(plot=merge_mean_VIs_design_2018, filename = "path/VIs_DesignFactors_GRtype_2018.tif", width = 24, height = 16, device='tiff', dpi=600,unit="in")



# Supplementary Figures S12 and S13: CV of SAVI, GNDVI, CIg vs. SQRT Area (grouped by green roof type) --------
# SAVI --------------------------------------------------------------------
### 2018 (pre-growing season) ###
# Change the level order
GR_savi_2018$GR_type <- factor(GR_savi_2018$GR_type, levels = c("ext","int"))

# plot
CV_savi_area_2018.plot <- ggplot(GR_savi_2018, aes(x=sqrt_area,y=COV, color = GR_type, linetype = GR_type))+
  geom_point(aes(shape = GR_type), size=2.5, alpha = 0.3) +
  geom_smooth(aes(color = GR_type, fill=GR_type, linetype = GR_type),
              method='lm', formula= y~x, alpha = 0.3, cex=1.5)+
  # geom_smooth(aes(color = GR_type, fill = GR_type), method="lm", formula=y ~ log(x + 1e-6), se=TRUE, alpha = 0.3) +
  labs(x=expression(sqrt("Green roof area")~(m)), y="CV of SAVI",
       group = "GR_type", linetype = "GR_type", pch = "GR_type")+
  scale_color_manual(name="Green roof type",labels = c("Extensive","Intensive"),values = c("#FC7307","#236AB9"))+
  scale_fill_manual(name="Green roof type",labels = c("Extensive","Intensive"),values = c("#FC7307","#236AB9"))+
  scale_linetype_manual(name="Green roof type", labels = c("Extensive","Intensive"), values = c(1,1))+
  scale_shape_manual(name="Green roof type", labels = c("Extensive","Intensive"), values = c(19,17))+
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

CV_savi_area_2018.plot

# Linear regression 
summary(lm(COV ~ sqrt_area, data = GR_savi_2018))

# Linear regression by green roof type
summary(lm(COV ~ sqrt_area, data = subset(GR_savi_2018, GR_type =="int")))
summary(lm(COV ~ sqrt_area, data = subset(GR_savi_2018, GR_type =="ext")))

### 2017 (growing season) ###
# Change the level order
GR_savi_2017$GR_type <- factor(GR_savi_2017$GR_type, levels = c("ext","int"))

# plot
CV_savi_area_2017.plot <- ggplot(GR_savi_2017, aes(x=sqrt_area,y=COV, color = GR_type, linetype = GR_type))+
  geom_point(aes(shape = GR_type), size=2.5, alpha = 0.3) +
  geom_smooth(aes(color = GR_type, fill=GR_type, linetype = GR_type),
              method='lm', formula= y~x, alpha = 0.3, cex=1.5)+
  # geom_smooth(aes(color = GR_type, fill = GR_type), method="lm", formula=y ~ log(x + 1e-6), se=TRUE, alpha = 0.3) +
  labs(x=expression(sqrt("Green roof area")~(m)), y="CV of SAVI",
       group = "GR_type", linetype = "GR_type", pch = "GR_type")+
  scale_color_manual(name="Green roof type",labels = c("Extensive","Intensive"),values = c("#FC7307","#236AB9"))+
  scale_fill_manual(name="Green roof type",labels = c("Extensive","Intensive"),values = c("#FC7307","#236AB9"))+
  scale_linetype_manual(name="Green roof type", labels = c("Extensive","Intensive"), values = c(1,1))+
  scale_shape_manual(name="Green roof type", labels = c("Extensive","Intensive"), values = c(19,17))+
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

CV_savi_area_2017.plot

# Linear regression 
summary(lm(COV ~ sqrt_area, data = GR_savi_2017))

# Linear regression by green roof type
summary(lm(COV ~ sqrt_area, data = subset(GR_savi_2017, GR_type =="int")))
summary(lm(COV ~ sqrt_area, data = subset(GR_savi_2017, GR_type =="ext")))

# GNDVI --------------------------------------------------------------------
### 2018 (pre-growing season) ###
# Change the level order
GR_gndvi_2018$GR_type <- factor(GR_gndvi_2018$GR_type, levels = c("ext","int"))

# plot
CV_gndvi_area_2018.plot <- ggplot(GR_gndvi_2018, aes(x=sqrt_area,y=COV, color = GR_type, linetype = GR_type))+
  geom_point(aes(shape = GR_type), size=2.5, alpha = 0.3) +
  geom_smooth(aes(color = GR_type, fill=GR_type, linetype = GR_type),
              method='lm', formula= y~x, alpha = 0.3, cex=1.5)+
  labs(x=expression(sqrt("Green roof area")~(m)), y="CV of GNDVI",
       group = "GR_type", linetype = "GR_type", pch = "GR_type")+
  scale_color_manual(name="Green roof type",labels = c("Extensive","Intensive"),values = c("#FC7307","#236AB9"))+
  scale_fill_manual(name="Green roof type",labels = c("Extensive","Intensive"),values = c("#FC7307","#236AB9"))+
  scale_linetype_manual(name="Green roof type", labels = c("Extensive","Intensive"), values = c(1,1))+
  scale_shape_manual(name="Green roof type", labels = c("Extensive","Intensive"), values = c(19,17))+
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

CV_gndvi_area_2018.plot

# Linear regression 
summary(lm(COV ~ sqrt_area, data = GR_gndvi_2018))

# Linear regression by green roof type
summary(lm(COV ~ sqrt_area, data = subset(GR_gndvi_2018, GR_type =="int")))
summary(lm(COV ~ sqrt_area, data = subset(GR_gndvi_2018, GR_type =="ext")))

### 2017 (growing season) ###
# Change the level order
GR_gndvi_2017$GR_type <- factor(GR_gndvi_2017$GR_type, levels = c("ext","int"))

# plot
CV_gndvi_area_2017.plot <- ggplot(GR_gndvi_2017, aes(x=sqrt_area,y=COV, color = GR_type, linetype = GR_type))+
  geom_point(aes(shape = GR_type), size=2.5, alpha = 0.3) +
  geom_smooth(aes(color = GR_type, fill=GR_type, linetype = GR_type),
              method='lm', formula= y~x, alpha = 0.3, cex=1.5)+
  labs(x=expression(sqrt("Green roof area")~(m)), y="CV of GNDVI",
       group = "GR_type", linetype = "GR_type", pch = "GR_type")+
  scale_color_manual(name="Green roof type",labels = c("Extensive","Intensive"),values = c("#FC7307","#236AB9"))+
  scale_fill_manual(name="Green roof type",labels = c("Extensive","Intensive"),values = c("#FC7307","#236AB9"))+
  scale_linetype_manual(name="Green roof type", labels = c("Extensive","Intensive"), values = c(1,1))+
  scale_shape_manual(name="Green roof type", labels = c("Extensive","Intensive"), values = c(19,17))+
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

CV_gndvi_area_2017.plot

# Linear regression 
summary(lm(COV ~ sqrt_area, data = GR_gndvi_2017))

# Linear regression by green roof type
summary(lm(COV ~ sqrt_area, data = subset(GR_gndvi_2017, GR_type =="int")))
summary(lm(COV ~ sqrt_area, data = subset(GR_gndvi_2017, GR_type =="ext")))


# CIg --------------------------------------------------------------------
### 2018 (pre-growing season) ###
# Change the level order
GR_cig_2018$GR_type <- factor(GR_cig_2018$GR_type, levels = c("ext","int"))

# plot
CV_cig_area_2018.plot <- ggplot(GR_cig_2018, aes(x=sqrt_area,y=COV, color = GR_type, linetype = GR_type))+
  geom_point(aes(shape = GR_type), size=2.5, alpha = 0.3) +
  geom_smooth(aes(color = GR_type, fill=GR_type, linetype = GR_type),
              method='lm', formula= y~x, alpha = 0.3, cex=1.5)+
  labs(x=expression(sqrt("Green roof area")~(m)), y="CV of CIg",
       group = "GR_type", linetype = "GR_type", pch = "GR_type")+
  scale_color_manual(name="Green roof type",labels = c("Extensive","Intensive"),values = c("#FC7307","#236AB9"))+
  scale_fill_manual(name="Green roof type",labels = c("Extensive","Intensive"),values = c("#FC7307","#236AB9"))+
  scale_linetype_manual(name="Green roof type", labels = c("Extensive","Intensive"), values = c(1,1))+
  scale_shape_manual(name="Green roof type", labels = c("Extensive","Intensive"), values = c(19,17))+
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

CV_cig_area_2018.plot

# Linear regression 
summary(lm(COV ~ sqrt_area, data = GR_cig_2018))

# Linear regression by green roof type
summary(lm(COV ~ sqrt_area, data = subset(GR_cig_2018, GR_type =="int")))
summary(lm(COV ~ sqrt_area, data = subset(GR_cig_2018, GR_type =="ext")))

### 2017 (growing season) ###
# Change the level order
GR_cig_2017$GR_type <- factor(GR_cig_2017$GR_type, levels = c("ext","int"))

# plot
CV_cig_area_2017.plot <- ggplot(GR_cig_2017, aes(x=sqrt_area,y=COV, color = GR_type, linetype = GR_type))+
  geom_point(aes(shape = GR_type), size=2.5, alpha = 0.3) +
  geom_smooth(aes(color = GR_type, fill=GR_type, linetype = GR_type),
              method='lm', formula= y~x, alpha = 0.3, cex=1.5)+
  labs(x=expression(sqrt("Green roof area")~(m)), y="CV of CIg",
       group = "GR_type", linetype = "GR_type", pch = "GR_type")+
  scale_color_manual(name="Green roof type",labels = c("Extensive","Intensive"),values = c("#FC7307","#236AB9"))+
  scale_fill_manual(name="Green roof type",labels = c("Extensive","Intensive"),values = c("#FC7307","#236AB9"))+
  scale_linetype_manual(name="Green roof type", labels = c("Extensive","Intensive"), values = c(1,1))+
  scale_shape_manual(name="Green roof type", labels = c("Extensive","Intensive"), values = c(19,17))+
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

CV_cig_area_2017.plot

# Linear regression 
summary(lm(COV ~ sqrt_area, data = GR_cig_2017))

# Linear regression by green roof type
summary(lm(COV ~ sqrt_area, data = subset(GR_cig_2017, GR_type =="int")))
summary(lm(COV ~ sqrt_area, data = subset(GR_cig_2017, GR_type =="ext")))


# Supplementary Figures S12 and S13: CV of SAVI, GNDVI, CIg vs. Building Height (grouped by green roof type) --------
# SAVI --------------------------------------------------------------------
### 2018 (pre-growing season) ###
# Change the level order
GR_savi_2018$GR_type <- factor(GR_savi_2018$GR_type, levels = c("ext","int"))

# plot
CV_savi_height_2018.plot <- ggplot(GR_savi_2018, aes(x=MAX_HEIGHT,y=COV, color = GR_type, linetype = GR_type))+
  geom_point(aes(shape = GR_type), size=2.5, alpha = 0.3) +
  geom_smooth(aes(color = GR_type, fill=GR_type, linetype = GR_type),
              method='lm', formula= y~x, alpha = 0.3, cex=1.5)+
  labs(x="Building height (m)", y="CV of SAVI",
       group = "GR_type", linetype = "GR_type", pch = "GR_type")+
  scale_color_manual(name="Green roof type",labels = c("Extensive","Intensive"),values = c("#FC7307","#236AB9"))+
  scale_fill_manual(name="Green roof type",labels = c("Extensive","Intensive"),values = c("#FC7307","#236AB9"))+
  scale_linetype_manual(name="Green roof type", labels = c("Extensive","Intensive"), values = c(1,2))+
  scale_shape_manual(name="Green roof type", labels = c("Extensive","Intensive"), values = c(19,17))+
  scale_x_continuous(limits = c(0, 180))+
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

CV_savi_height_2018.plot

# Linear regression 
summary(lm(COV ~ MAX_HEIGHT, data = GR_savi_2018))

# Linear regression by green roof type
summary(lm(COV ~ MAX_HEIGHT, data = subset(GR_savi_2018, GR_type =="int")))
summary(lm(COV ~ MAX_HEIGHT, data = subset(GR_savi_2018, GR_type =="ext")))

### 2017 (growing season) ###
# Change the level order
GR_savi_2017$GR_type <- factor(GR_savi_2017$GR_type, levels = c("ext","int"))

# plot
CV_savi_height_2017.plot <- ggplot(GR_savi_2017, aes(x=MAX_HEIGHT,y=COV, color = GR_type, linetype = GR_type))+
  geom_point(aes(shape = GR_type), size=2.5, alpha = 0.3) +
  geom_smooth(aes(color = GR_type, fill=GR_type, linetype = GR_type),
              method='lm', formula= y~x, alpha = 0.3, cex=1.5)+
  labs(x="Building height (m)", y="CV of SAVI",
       group = "GR_type", linetype = "GR_type", pch = "GR_type")+
  scale_color_manual(name="Green roof type",labels = c("Extensive","Intensive"),values = c("#FC7307","#236AB9"))+
  scale_fill_manual(name="Green roof type",labels = c("Extensive","Intensive"),values = c("#FC7307","#236AB9"))+
  scale_linetype_manual(name="Green roof type", labels = c("Extensive","Intensive"), values = c(2,2))+
  scale_shape_manual(name="Green roof type", labels = c("Extensive","Intensive"), values = c(19,17))+
  scale_x_continuous(limits = c(0, 180))+
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

CV_savi_height_2017.plot

# Linear regression 
summary(lm(COV ~ MAX_HEIGHT, data = GR_savi_2017))

# Linear regression by green roof type
summary(lm(COV ~ MAX_HEIGHT, data = subset(GR_savi_2017, GR_type =="int")))
summary(lm(COV ~ MAX_HEIGHT, data = subset(GR_savi_2017, GR_type =="ext")))

# GNDVI --------------------------------------------------------------------
### 2018 (pre-growing season) ###
# Change the level order
GR_gndvi_2018$GR_type <- factor(GR_gndvi_2018$GR_type, levels = c("ext","int"))

# plot
CV_gndvi_height_2018.plot <- ggplot(GR_gndvi_2018, aes(x=MAX_HEIGHT,y=COV, color = GR_type, linetype = GR_type))+
  geom_point(aes(shape = GR_type), size=2.5, alpha = 0.3) +
  geom_smooth(aes(color = GR_type, fill=GR_type, linetype = GR_type),
              method='lm', formula= y~x, alpha = 0.3, cex=1.5)+
  labs(x="Building height (m)", y="CV of GNDVI",
       group = "GR_type", linetype = "GR_type", pch = "GR_type")+
  scale_color_manual(name="Green roof type",labels = c("Extensive","Intensive"),values = c("#FC7307","#236AB9"))+
  scale_fill_manual(name="Green roof type",labels = c("Extensive","Intensive"),values = c("#FC7307","#236AB9"))+
  scale_linetype_manual(name="Green roof type", labels = c("Extensive","Intensive"), values = c(2,2))+
  scale_shape_manual(name="Green roof type", labels = c("Extensive","Intensive"), values = c(19,17))+
  scale_x_continuous(limits = c(0, 180))+
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

CV_gndvi_height_2018.plot

# Linear regression 
summary(lm(COV ~ MAX_HEIGHT, data = GR_gndvi_2018))

# Linear regression by green roof type
summary(lm(COV ~ MAX_HEIGHT, data = subset(GR_gndvi_2018, GR_type =="int")))
summary(lm(COV ~ MAX_HEIGHT, data = subset(GR_gndvi_2018, GR_type =="ext")))

### 2017 (growing season) ###
# Change the level order
GR_gndvi_2017$GR_type <- factor(GR_gndvi_2017$GR_type, levels = c("ext","int"))

# plot
CV_gndvi_height_2017.plot <- ggplot(GR_gndvi_2017, aes(x=MAX_HEIGHT,y=COV, color = GR_type, linetype = GR_type))+
  geom_point(aes(shape = GR_type), size=2.5, alpha = 0.3) +
  geom_smooth(aes(color = GR_type, fill=GR_type, linetype = GR_type),
              method='lm', formula= y~x, alpha = 0.3, cex=1.5)+
  labs(x="Building height (m)", y="CV of GNDVI",
       group = "GR_type", linetype = "GR_type", pch = "GR_type")+
  scale_color_manual(name="Green roof type",labels = c("Extensive","Intensive"),values = c("#FC7307","#236AB9"))+
  scale_fill_manual(name="Green roof type",labels = c("Extensive","Intensive"),values = c("#FC7307","#236AB9"))+
  scale_linetype_manual(name="Green roof type", labels = c("Extensive","Intensive"), values = c(2,2))+
  scale_shape_manual(name="Green roof type", labels = c("Extensive","Intensive"), values = c(19,17))+
  scale_x_continuous(limits = c(0, 180))+
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

CV_gndvi_height_2017.plot

# Linear regression 
summary(lm(COV ~ MAX_HEIGHT, data = GR_gndvi_2017))

# Linear regression by green roof type
summary(lm(COV ~ MAX_HEIGHT, data = subset(GR_gndvi_2017, GR_type =="int")))
summary(lm(COV ~ MAX_HEIGHT, data = subset(GR_gndvi_2017, GR_type =="ext")))


# CIg --------------------------------------------------------------------
### 2018 (pre-growing season) ###
# Change the level order
GR_cig_2018$GR_type <- factor(GR_cig_2018$GR_type, levels = c("ext","int"))

# plot
CV_cig_height_2018.plot <- ggplot(GR_cig_2018, aes(x=MAX_HEIGHT,y=COV, color = GR_type, linetype = GR_type))+
  geom_point(aes(shape = GR_type), size=2.5, alpha = 0.3) +
  geom_smooth(aes(color = GR_type, fill=GR_type, linetype = GR_type),
              method='lm', formula= y~x, alpha = 0.3, cex=1.5)+
  labs(x="Building height (m)", y="CV of CIg",
       group = "GR_type", linetype = "GR_type", pch = "GR_type")+
  scale_color_manual(name="Green roof type",labels = c("Extensive","Intensive"),values = c("#FC7307","#236AB9"))+
  scale_fill_manual(name="Green roof type",labels = c("Extensive","Intensive"),values = c("#FC7307","#236AB9"))+
  scale_linetype_manual(name="Green roof type", labels = c("Extensive","Intensive"), values = c(2,2))+
  scale_shape_manual(name="Green roof type", labels = c("Extensive","Intensive"), values = c(19,17))+
  scale_x_continuous(limits = c(0, 180))+
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

CV_cig_height_2018.plot

# Linear regression 
summary(lm(COV ~ MAX_HEIGHT, data = GR_cig_2018))

# Linear regression by green roof type
summary(lm(COV ~ MAX_HEIGHT, data = subset(GR_cig_2018, GR_type =="int")))
summary(lm(COV ~ MAX_HEIGHT, data = subset(GR_cig_2018, GR_type =="ext")))

### 2017 (growing season) ###
# Change the level order
GR_cig_2017$GR_type <- factor(GR_cig_2017$GR_type, levels = c("ext","int"))

# plot
CV_cig_height_2017.plot <- ggplot(GR_cig_2017, aes(x=MAX_HEIGHT,y=COV, color = GR_type, linetype = GR_type))+
  geom_point(aes(shape = GR_type), size=2.5, alpha = 0.3) +
  geom_smooth(aes(color = GR_type, fill=GR_type, linetype = GR_type),
              method='lm', formula= y~x, alpha = 0.3, cex=1.5)+
  labs(x="Building height (m)", y="CV of CIg",
       group = "GR_type", linetype = "GR_type", pch = "GR_type")+
  scale_color_manual(name="Green roof type",labels = c("Extensive","Intensive"),values = c("#FC7307","#236AB9"))+
  scale_fill_manual(name="Green roof type",labels = c("Extensive","Intensive"),values = c("#FC7307","#236AB9"))+
  scale_linetype_manual(name="Green roof type", labels = c("Extensive","Intensive"), values = c(2,2))+
  scale_shape_manual(name="Green roof type", labels = c("Extensive","Intensive"), values = c(19,17))+
  scale_x_continuous(limits = c(0, 180))+
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

CV_cig_height_2017.plot

# Linear regression 
summary(lm(COV ~ MAX_HEIGHT, data = GR_cig_2017))

# Linear regression by green roof type
summary(lm(COV ~ MAX_HEIGHT, data = subset(GR_cig_2017, GR_type =="int")))
summary(lm(COV ~ MAX_HEIGHT, data = subset(GR_cig_2017, GR_type =="ext")))


# Supplementary Figures S12 and S13: CV of SAVI, GNDVI, CIg vs. Green roof type --------
# SAVI --------------------------------------------------------------------
### 2018 (pre-growing season) ###
CV_summary_GR_savi_2018 <- GR_savi_2018 %>%
  group_by(GR_type) %>%
  summarise(CV_mean_SAVI = mean(COV, na.rm = TRUE),
            CV_se_SAVI = get_se(COV))

# Change the level order
CV_summary_GR_savi_2018$GR_type <- factor(CV_summary_GR_savi_2018$GR_type, levels = c("ext","int"))

# plot
CV_savi_GRtype_barplot_2018 <- ggplot(CV_summary_GR_savi_2018, aes(x = GR_type, y = CV_mean_SAVI, fill = GR_type)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin = CV_mean_SAVI - CV_se_SAVI, ymax = CV_mean_SAVI + CV_se_SAVI),
                position = position_dodge(0.9), width = 0.3) +
  scale_x_discrete(name="Green roof type",labels = c("Extensive", "Intensive"))+
  scale_fill_manual(name="Green roof type",labels = c("Extensive", "Intensive"),values = c("#FC7307","#236AB9"))+
  labs(x = "Green roof type", y = "CV of SAVI",
       group = "GR_type")+
  coord_cartesian(ylim=c(0,0.045))+
  theme_bw(base_size = 28) + 
  theme(
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text=element_text(size=28, color = "black"),
    axis.title=element_text(size=32, color = "black"),
    legend.position = "none",
    legend.title = element_text(size = 32)
  )

CV_savi_GRtype_barplot_2018

# t-test
t.test(COV ~ GR_type, data = GR_savi_2018, var.equal = TRUE)

### 2017 (growing season) ###
CV_summary_GR_savi_2017 <- GR_savi_2017 %>%
  group_by(GR_type) %>%
  summarise(CV_mean_SAVI = mean(COV, na.rm = TRUE),
            CV_se_SAVI = get_se(COV))

# Change the level order
CV_summary_GR_savi_2017$GR_type <- factor(CV_summary_GR_savi_2017$GR_type, levels = c("ext","int"))

# plot
CV_savi_GRtype_barplot_2017 <- ggplot(CV_summary_GR_savi_2017, aes(x = GR_type, y = CV_mean_SAVI, fill = GR_type)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin = CV_mean_SAVI - CV_se_SAVI, ymax = CV_mean_SAVI + CV_se_SAVI),
                position = position_dodge(0.9), width = 0.3) +
  scale_x_discrete(name="Green roof type",labels = c("Extensive", "Intensive"))+
  scale_fill_manual(name="Green roof type",labels = c("Extensive", "Intensive"),values = c("#FC7307","#236AB9"))+
  labs(x = "Green roof type", y = "CV of SAVI",
       group = "GR_type")+
  coord_cartesian(ylim=c(0,0.05))+
  theme_bw(base_size = 28) + 
  theme(
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text=element_text(size=28, color = "black"),
    axis.title=element_text(size=32, color = "black"),
    legend.position = "none",
    legend.title = element_text(size = 32)
  )

CV_savi_GRtype_barplot_2017

# t-test
t.test(COV ~ GR_type, data = GR_savi_2017, var.equal = TRUE)


# GNDVI --------------------------------------------------------------------
### 2018 (pre-growing season) ###
CV_summary_GR_gndvi_2018 <- GR_gndvi_2018 %>%
  group_by(GR_type) %>%
  summarise(CV_mean_GNDVI = mean(COV, na.rm = TRUE),
            CV_se_GNDVI = get_se(COV))

# Change the level order
CV_summary_GR_gndvi_2018$GR_type <- factor(CV_summary_GR_gndvi_2018$GR_type, levels = c("ext","int"))

# plot
CV_gndvi_GRtype_barplot_2018 <- ggplot(CV_summary_GR_gndvi_2018, aes(x = GR_type, y = CV_mean_GNDVI, fill = GR_type)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin = CV_mean_GNDVI - CV_se_GNDVI, ymax = CV_mean_GNDVI + CV_se_GNDVI),
                position = position_dodge(0.9), width = 0.3) +
  scale_x_discrete(name="Green roof type",labels = c("Extensive", "Intensive"))+
  scale_fill_manual(name="Green roof type",labels = c("Extensive", "Intensive"),values = c("#FC7307","#236AB9"))+
  labs(x = "Green roof type", y = "CV of GNDVI",
       group = "GR_type")+
  coord_cartesian(ylim=c(0,0.055))+
  theme_bw(base_size = 28) + 
  theme(
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text=element_text(size=28, color = "black"),
    axis.title=element_text(size=32, color = "black"),
    legend.position = "none",
    legend.title = element_text(size = 32)
  )

CV_gndvi_GRtype_barplot_2018

# t-test
t.test(COV ~ GR_type, data = GR_gndvi_2018, var.equal = TRUE)

### 2017 (growing season) ###
CV_summary_GR_gndvi_2017 <- GR_gndvi_2017 %>%
  group_by(GR_type) %>%
  summarise(CV_mean_GNDVI = mean(COV, na.rm = TRUE),
            CV_se_GNDVI = get_se(COV))

# Change the level order
CV_summary_GR_gndvi_2017$GR_type <- factor(CV_summary_GR_gndvi_2017$GR_type, levels = c("ext","int"))

# plot
CV_gndvi_GRtype_barplot_2017 <- ggplot(CV_summary_GR_gndvi_2017, aes(x = GR_type, y = CV_mean_GNDVI, fill = GR_type)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin = CV_mean_GNDVI - CV_se_GNDVI, ymax = CV_mean_GNDVI + CV_se_GNDVI),
                position = position_dodge(0.9), width = 0.3) +
  scale_x_discrete(name="Green roof type",labels = c("Extensive", "Intensive"))+
  scale_fill_manual(name="Green roof type",labels = c("Extensive", "Intensive"),values = c("#FC7307","#236AB9"))+
  labs(x = "Green roof type", y = "CV of GNDVI",
       group = "GR_type")+
  coord_cartesian(ylim=c(0,0.06))+
  theme_bw(base_size = 28) + 
  theme(
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text=element_text(size=28, color = "black"),
    axis.title=element_text(size=32, color = "black"),
    legend.position = "none",
    legend.title = element_text(size = 32)
  )

CV_gndvi_GRtype_barplot_2017

# t-test
t.test(COV ~ GR_type, data = GR_gndvi_2017, var.equal = TRUE)

# CIg --------------------------------------------------------------------
### 2018 (pre-growing season) ###
CV_summary_GR_cig_2018 <- GR_cig_2018 %>%
  group_by(GR_type) %>%
  summarise(CV_mean_CIg = mean(COV, na.rm = TRUE),
            CV_se_CIg = get_se(COV))

# Change the level order
CV_summary_GR_cig_2018$GR_type <- factor(CV_summary_GR_cig_2018$GR_type, levels = c("ext","int"))

# plot
CV_cig_GRtype_barplot_2018 <- ggplot(CV_summary_GR_cig_2018, aes(x = GR_type, y = CV_mean_CIg, fill = GR_type)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin = CV_mean_CIg - CV_se_CIg, ymax = CV_mean_CIg + CV_se_CIg),
                position = position_dodge(0.9), width = 0.3) +
  scale_x_discrete(name="Green roof type",labels = c("Extensive", "Intensive"))+
  scale_fill_manual(name="Green roof type",labels = c("Extensive", "Intensive"),values = c("#FC7307","#236AB9"))+
  labs(x = "Green roof type", y = "CV of CIg",
       group = "GR_type")+
  coord_cartesian(ylim=c(0,0.15))+
  theme_bw(base_size = 28) + 
  theme(
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text=element_text(size=28, color = "black"),
    axis.title=element_text(size=32, color = "black"),
    legend.position = "none",
    legend.title = element_text(size = 32)
  )

CV_cig_GRtype_barplot_2018

# t-test
t.test(COV ~ GR_type, data = GR_cig_2018, var.equal = TRUE)

### 2017 (growing season) ###
CV_summary_GR_cig_2017 <- GR_cig_2017 %>%
  group_by(GR_type) %>%
  summarise(CV_mean_CIg = mean(COV, na.rm = TRUE),
            CV_se_CIg = get_se(COV))

# Change the level order
CV_summary_GR_cig_2017$GR_type <- factor(CV_summary_GR_cig_2017$GR_type, levels = c("ext","int"))

# plot
CV_cig_GRtype_barplot_2017 <- ggplot(CV_summary_GR_cig_2017, aes(x = GR_type, y = CV_mean_CIg, fill = GR_type)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin = CV_mean_CIg - CV_se_CIg, ymax = CV_mean_CIg + CV_se_CIg),
                position = position_dodge(0.9), width = 0.3) +
  scale_x_discrete(name="Green roof type",labels = c("Extensive", "Intensive"))+
  scale_fill_manual(name="Green roof type",labels = c("Extensive", "Intensive"),values = c("#FC7307","#236AB9"))+
  labs(x = "Green roof type", y = "CV of CIg",
       group = "GR_type")+
  coord_cartesian(ylim=c(0,0.15))+
  theme_bw(base_size = 28) + 
  theme(
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text=element_text(size=28, color = "black"),
    axis.title=element_text(size=32, color = "black"),
    legend.position = "none",
    legend.title = element_text(size = 32)
  )

CV_cig_GRtype_barplot_2017

# t-test
t.test(COV ~ GR_type, data = GR_cig_2017, var.equal = TRUE)

# Supplementary Figures S12 and S13: CV of SAVI, GNDVI, CIg vs. SQRT Aspect ratio (grouped by green roof type) --------
# SAVI --------------------------------------------------------------------
### 2018 (pre-growing season) ###
# change order of display
GR_savi_2018$GR_type <- factor(GR_savi_2018$GR_type, levels = c("ext", "int"))

# plot
CV_savi_sqrtAR_GRtype_2018.plot <- ggplot(GR_savi_2018, aes(x=sqrt_AR,y=COV, color = GR_type, linetype = GR_type))+
  geom_point(aes(shape = GR_type), size=2.5, alpha = 0.3) +
  geom_smooth(aes(color = GR_type, fill=GR_type, linetype = GR_type),
              method='lm', formula= y~x, alpha = 0.3, cex=1.5)+
  labs(x=expression(sqrt("Aspect ratio")), y="CV of SAVI",
       group = "GR_type", linetype = "GR_type", pch = "GR_type")+
  scale_color_manual(name="Green roof type",labels = c("Extensive", "Intensive"),values = c("#FC7307","#236AB9"))+
  scale_fill_manual(name="Green roof type",labels = c("Extensive", "Intensive"),values = c("#FC7307","#236AB9"))+
  scale_linetype_manual(name="Green roof type", labels = c("Extensive", "Intensive"), values = c(1,1))+
  scale_shape_manual(name="Green roof type", labels = c("Extensive", "Intensive"), values = c(19,17)) +
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

CV_savi_sqrtAR_GRtype_2018.plot

# Linear regression 
summary(lm(COV ~ sqrt_AR, data = GR_savi_2018))

# Linear regression by green roof type
summary(lm(COV ~ sqrt_AR, data = subset(GR_savi_2018, GR_type =="int")))
summary(lm(COV ~ sqrt_AR, data = subset(GR_savi_2018, GR_type =="ext")))

### 2017 (growing season) ###
# change order of display
GR_savi_2017$GR_type <- factor(GR_savi_2017$GR_type, levels = c("ext", "int"))

# Square root aspect ratio
CV_savi_sqrtAR_GRtype_2017.plot <- ggplot(GR_savi_2017, aes(x=sqrt_AR,y=COV, color = GR_type, linetype = GR_type))+
  geom_point(aes(shape = GR_type), size=2.5, alpha = 0.3) +
  geom_smooth(aes(color = GR_type, fill=GR_type, linetype = GR_type),
              method='lm', formula= y~x, alpha = 0.3, cex=1.5)+
  labs(x=expression(sqrt("Aspect ratio")), y="CV of SAVI",
       group = "GR_type", linetype = "GR_type", pch = "GR_type")+
  scale_color_manual(name="Green roof type",labels = c("Extensive", "Intensive"),values = c("#FC7307","#236AB9"))+
  scale_fill_manual(name="Green roof type",labels = c("Extensive", "Intensive"),values = c("#FC7307","#236AB9"))+
  scale_linetype_manual(name="Green roof type", labels = c("Extensive", "Intensive"), values = c(1,1))+
  scale_shape_manual(name="Green roof type", labels = c("Extensive", "Intensive"), values = c(19,17)) +
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

CV_savi_sqrtAR_GRtype_2017.plot

# Linear regression 
summary(lm(COV ~ sqrt_AR, data = GR_savi_2017))

# Linear regression by green roof type
summary(lm(COV ~ sqrt_AR, data = subset(GR_savi_2017, GR_type =="int")))
summary(lm(COV ~ sqrt_AR, data = subset(GR_savi_2017, GR_type =="ext")))

# GNDVI --------------------------------------------------------------------
### 2018 (pre-growing season) ###
# change order of display
GR_gndvi_2018$GR_type <- factor(GR_gndvi_2018$GR_type, levels = c("ext", "int"))

# plot
CV_gndvi_sqrtAR_GRtype_2018.plot <- ggplot(GR_gndvi_2018, aes(x=sqrt_AR,y=COV, color = GR_type, linetype = GR_type))+
  geom_point(aes(shape = GR_type), size=2.5, alpha = 0.3) +
  geom_smooth(aes(color = GR_type, fill=GR_type, linetype = GR_type),
              method='lm', formula= y~x, alpha = 0.3, cex=1.5)+
  labs(x=expression(sqrt("Aspect ratio")), y="CV of GNDVI",
       group = "GR_type", linetype = "GR_type", pch = "GR_type")+
  scale_color_manual(name="Green roof type",labels = c("Extensive", "Intensive"),values = c("#FC7307","#236AB9"))+
  scale_fill_manual(name="Green roof type",labels = c("Extensive", "Intensive"),values = c("#FC7307","#236AB9"))+
  scale_linetype_manual(name="Green roof type", labels = c("Extensive", "Intensive"), values = c(1,1))+
  scale_shape_manual(name="Green roof type", labels = c("Extensive", "Intensive"), values = c(19,17)) +
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

CV_gndvi_sqrtAR_GRtype_2018.plot

# Linear regression 
summary(lm(COV ~ sqrt_AR, data = GR_gndvi_2018))

# Linear regression by green roof type
summary(lm(COV ~ sqrt_AR, data = subset(GR_gndvi_2018, GR_type =="int")))
summary(lm(COV ~ sqrt_AR, data = subset(GR_gndvi_2018, GR_type =="ext")))


### 2017 (growing season) ###
# change order of display
GR_gndvi_2017$GR_type <- factor(GR_gndvi_2017$GR_type, levels = c("ext", "int"))

# plot
CV_gndvi_sqrtAR_GRtype_2017.plot <- ggplot(GR_gndvi_2017, aes(x=sqrt_AR,y=COV, color = GR_type, linetype = GR_type))+
  geom_point(aes(shape = GR_type), size=2.5, alpha = 0.3) +
  geom_smooth(aes(color = GR_type, fill=GR_type, linetype = GR_type),
              method='lm', formula= y~x, alpha = 0.3, cex=1.5)+
  labs(x=expression(sqrt("Aspect ratio")), y="CV of GNDVI",
       group = "GR_type", linetype = "GR_type", pch = "GR_type")+
  scale_color_manual(name="Green roof type",labels = c("Extensive", "Intensive"),values = c("#FC7307","#236AB9"))+
  scale_fill_manual(name="Green roof type",labels = c("Extensive", "Intensive"),values = c("#FC7307","#236AB9"))+
  scale_linetype_manual(name="Green roof type", labels = c("Extensive", "Intensive"), values = c(1,1))+
  scale_shape_manual(name="Green roof type", labels = c("Extensive", "Intensive"), values = c(19,17)) +
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

CV_gndvi_sqrtAR_GRtype_2017.plot

# Linear regression 
summary(lm(COV ~ sqrt_AR, data = GR_gndvi_2017))

# Linear regression by green roof type
summary(lm(COV ~ sqrt_AR, data = subset(GR_gndvi_2017, GR_type =="int")))
summary(lm(COV ~ sqrt_AR, data = subset(GR_gndvi_2017, GR_type =="ext")))


# CIg --------------------------------------------------------------------
### 2018 (pre-growing season) ###
# change order of display
GR_cig_2018$GR_type <- factor(GR_cig_2018$GR_type, levels = c("ext", "int"))

# plot
CV_cig_sqrtAR_GRtype_2018.plot <- ggplot(GR_cig_2018, aes(x=sqrt_AR,y=COV, color = GR_type, linetype = GR_type))+
  geom_point(aes(shape = GR_type), size=2.5, alpha = 0.3) +
  geom_smooth(aes(color = GR_type, fill=GR_type, linetype = GR_type),
              method='lm', formula= y~x, alpha = 0.3, cex=1.5)+
  labs(x=expression(sqrt("Aspect ratio")), y="CV of CIg",
       group = "GR_type", linetype = "GR_type", pch = "GR_type")+
  scale_color_manual(name="Green roof type",labels = c("Extensive", "Intensive"),values = c("#FC7307","#236AB9"))+
  scale_fill_manual(name="Green roof type",labels = c("Extensive", "Intensive"),values = c("#FC7307","#236AB9"))+
  scale_linetype_manual(name="Green roof type", labels = c("Extensive", "Intensive"), values = c(1,1))+
  scale_shape_manual(name="Green roof type", labels = c("Extensive", "Intensive"), values = c(19,17)) +
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

CV_cig_sqrtAR_GRtype_2018.plot

# Linear regression 
summary(lm(COV ~ sqrt_AR, data = GR_cig_2018))

# Linear regression by green roof type
summary(lm(COV ~ sqrt_AR, data = subset(GR_cig_2018, GR_type =="int")))
summary(lm(COV ~ sqrt_AR, data = subset(GR_cig_2018, GR_type =="ext")))

### 2017 (growing season) ###
# change order of display
GR_cig_2017$GR_type <- factor(GR_cig_2017$GR_type, levels = c("ext", "int"))

# plot
CV_cig_sqrtAR_GRtype_2017.plot <- ggplot(GR_cig_2017, aes(x=sqrt_AR,y=COV, color = GR_type, linetype = GR_type))+
  geom_point(aes(shape = GR_type), size=2.5, alpha = 0.3) +
  geom_smooth(aes(color = GR_type, fill=GR_type, linetype = GR_type),
              method='lm', formula= y~x, alpha = 0.3, cex=1.5)+
  labs(x=expression(sqrt("Aspect ratio")), y="CV of CIg",
       group = "GR_type", linetype = "GR_type", pch = "GR_type")+
  scale_color_manual(name="Green roof type",labels = c("Extensive", "Intensive"),values = c("#FC7307","#236AB9"))+
  scale_fill_manual(name="Green roof type",labels = c("Extensive", "Intensive"),values = c("#FC7307","#236AB9"))+
  scale_linetype_manual(name="Green roof type", labels = c("Extensive", "Intensive"), values = c(1,1))+
  scale_shape_manual(name="Green roof type", labels = c("Extensive", "Intensive"), values = c(19,17)) +
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

CV_cig_sqrtAR_GRtype_2017.plot

# Linear regression 
summary(lm(COV ~ sqrt_AR, data = GR_cig_2017))

# Linear regression by green roof type
summary(lm(COV ~ sqrt_AR, data = subset(GR_cig_2017, GR_type =="int")))
summary(lm(COV ~ sqrt_AR, data = subset(GR_cig_2017, GR_type =="ext")))


# Combine Supplementary Figure S12: CV of VIs grouped by green roof type (growing season) --------
merge_COV_VIs_design_2017 = 
  CV_savi_area_2017.plot + CV_savi_height_2017.plot + CV_savi_GRtype_barplot_2017 + CV_savi_sqrtAR_GRtype_2017.plot +
  CV_gndvi_area_2017.plot + CV_gndvi_height_2017.plot + CV_gndvi_GRtype_barplot_2017 + CV_gndvi_sqrtAR_GRtype_2017.plot +
  CV_cig_area_2017.plot + CV_cig_height_2017.plot + CV_cig_GRtype_barplot_2017 + CV_cig_sqrtAR_GRtype_2017.plot +
  plot_layout(guides = "collect", ncol = 4) & theme(legend.position = "top")

merge_COV_VIs_design_2017[[1]] = merge_COV_VIs_design_2017[[1]] + theme(axis.title.x = element_blank())
merge_COV_VIs_design_2017[[2]] = merge_COV_VIs_design_2017[[2]] + theme(legend.position = "none", axis.title.y = element_blank(), axis.title.x = element_blank())
merge_COV_VIs_design_2017[[3]] = merge_COV_VIs_design_2017[[3]] + theme(legend.position = "none", axis.title.y = element_blank(), axis.title.x = element_blank())
merge_COV_VIs_design_2017[[4]] = merge_COV_VIs_design_2017[[4]] + theme(legend.position = "none", axis.title.y = element_blank(), axis.title.x = element_blank())
merge_COV_VIs_design_2017[[5]] = merge_COV_VIs_design_2017[[5]] + theme(axis.title.x = element_blank())
merge_COV_VIs_design_2017[[6]] = merge_COV_VIs_design_2017[[6]] + theme(legend.position = "none", axis.title.y = element_blank(), axis.title.x = element_blank())
merge_COV_VIs_design_2017[[7]] = merge_COV_VIs_design_2017[[7]] + theme(legend.position = "none", axis.title.y = element_blank(), axis.title.x = element_blank())
merge_COV_VIs_design_2017[[8]] = merge_COV_VIs_design_2017[[8]] + theme(legend.position = "none", axis.title.y = element_blank(), axis.title.x = element_blank())
merge_COV_VIs_design_2017[[9]] = merge_COV_VIs_design_2017[[9]] + theme(legend.position = "none")
merge_COV_VIs_design_2017[[10]] = merge_COV_VIs_design_2017[[10]] + theme(legend.position = "none", axis.title.y = element_blank())
merge_COV_VIs_design_2017[[11]] = merge_COV_VIs_design_2017[[11]] + theme(legend.position = "none", axis.title.y = element_blank())
merge_COV_VIs_design_2017[[12]] = merge_COV_VIs_design_2017[[12]] + theme(legend.position = "none", axis.title.y = element_blank())

merge_COV_VIs_design_2017

# ggsave(plot=merge_COV_VIs_design_2017, filename = "path/VIs_DesignFactors_COV_GRtype_2017.tif", width = 24, height = 16, device='tiff', dpi=600,unit="in")

# Combine Supplementary Figure S13: CV of VIs grouped by green roof type (pre-growing season) --------
merge_COV_VIs_design_2018 = 
  CV_savi_area_2018.plot + CV_savi_height_2018.plot + CV_savi_GRtype_barplot_2018 + CV_savi_sqrtAR_GRtype_2018.plot +
  CV_gndvi_area_2018.plot + CV_gndvi_height_2018.plot + CV_gndvi_GRtype_barplot_2018 + CV_gndvi_sqrtAR_GRtype_2018.plot +
  CV_cig_area_2018.plot + CV_cig_height_2018.plot + CV_cig_GRtype_barplot_2018 + CV_cig_sqrtAR_GRtype_2018.plot +
  plot_layout(guides = "collect", ncol = 4) & theme(legend.position = "top")

merge_COV_VIs_design_2018[[1]] = merge_COV_VIs_design_2018[[1]] + theme(axis.title.x = element_blank())
merge_COV_VIs_design_2018[[2]] = merge_COV_VIs_design_2018[[2]] + theme(legend.position = "none", axis.title.y = element_blank(), axis.title.x = element_blank())
merge_COV_VIs_design_2018[[3]] = merge_COV_VIs_design_2018[[3]] + theme(legend.position = "none", axis.title.y = element_blank(), axis.title.x = element_blank())
merge_COV_VIs_design_2018[[4]] = merge_COV_VIs_design_2018[[4]] + theme(legend.position = "none", axis.title.y = element_blank(), axis.title.x = element_blank())
merge_COV_VIs_design_2018[[5]] = merge_COV_VIs_design_2018[[5]] + theme(axis.title.x = element_blank())
merge_COV_VIs_design_2018[[6]] = merge_COV_VIs_design_2018[[6]] + theme(legend.position = "none", axis.title.y = element_blank(), axis.title.x = element_blank())
merge_COV_VIs_design_2018[[7]] = merge_COV_VIs_design_2018[[7]] + theme(legend.position = "none", axis.title.y = element_blank(), axis.title.x = element_blank())
merge_COV_VIs_design_2018[[8]] = merge_COV_VIs_design_2018[[8]] + theme(legend.position = "none", axis.title.y = element_blank(), axis.title.x = element_blank())
merge_COV_VIs_design_2018[[9]] = merge_COV_VIs_design_2018[[9]] + theme(legend.position = "none")
merge_COV_VIs_design_2018[[10]] = merge_COV_VIs_design_2018[[10]] + theme(legend.position = "none", axis.title.y = element_blank())
merge_COV_VIs_design_2018[[11]] = merge_COV_VIs_design_2018[[11]] + theme(legend.position = "none", axis.title.y = element_blank())
merge_COV_VIs_design_2018[[12]] = merge_COV_VIs_design_2018[[12]] + theme(legend.position = "none", axis.title.y = element_blank())

merge_COV_VIs_design_2018

# ggsave(plot=merge_COV_VIs_design_2018, filename = "path/VIs_DesignFactors_COV_GRtype_2018.tif", width = 24, height = 16, device='tiff', dpi=600,unit="in")



# Supplementary Figures S14 and S15: Mean SAVI, Mean GNDVI, Mean CIg vs. SQRT area (grouped by vegetation type)  --------
# SAVI --------------------------------------------------------------------
### 2018 (pre-growing season) ###
# Change the level order
GR_savi_2018$new_GRtype <- factor(GR_savi_2018$new_GRtype, levels = c("grass", "sedum_mat", "woody_plants","mix_grass_tree"))

# plot
savi_area_2018_vegetype.plot <- ggplot(GR_savi_2018, aes(x=sqrt_area,y=MEAN, color = new_GRtype, linetype = new_GRtype))+
  geom_point(aes(shape = new_GRtype), size=2.5, alpha = 0.2) +
  geom_smooth(aes(color = new_GRtype, fill=new_GRtype, linetype = new_GRtype),
              method='lm', formula= y~x, alpha = 0.2, cex=1.5)+
  labs(x=expression(sqrt("Green roof area")~(m)), y="Mean SAVI",
       group = "new_GRtype", linetype = "new_GRtype", pch = "new_GRtype")+
  scale_color_manual(name="Vegetation type",labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"),values = c("#00BFFF", "#FF762E", "#7CAE00","#cd6ee8"))+
  scale_fill_manual(name="Vegetation type",labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"),values = c("#00BFFF", "#FF762E", "#7CAE00","#cd6ee8"))+
  scale_shape_manual(name="Vegetation type", labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"), values = c(19,17, 15, 18))+
  scale_linetype_manual(name="Vegetation type", labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"), values = c(1,1,1,2))+
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
  )+
  guides(
    color = guide_legend(nrow = 2, byrow = TRUE),
    linetype = guide_legend(nrow = 2, byrow = TRUE)
  )

savi_area_2018_vegetype.plot

# Linear regression by vegetation type
summary(lm(MEAN ~ sqrt_area, data = subset(GR_savi_2018, new_GRtype =="grass")))
summary(lm(MEAN ~ sqrt_area, data = subset(GR_savi_2018, new_GRtype =="sedum_mat")))
summary(lm(MEAN ~ sqrt_area, data = subset(GR_savi_2018, new_GRtype =="woody_plants")))
summary(lm(MEAN ~ sqrt_area, data = subset(GR_savi_2018, new_GRtype =="mix_grass_tree")))

### 2017 (growing season) ###
# Change the level order
GR_savi_2017$new_GRtype <- factor(GR_savi_2017$new_GRtype, levels = c("grass", "sedum_mat", "woody_plants","mix_grass_tree"))

# plot
savi_area_2017_vegetype.plot <- ggplot(GR_savi_2017, aes(x=sqrt_area,y=MEAN, color = new_GRtype, linetype = new_GRtype))+
  geom_point(aes(shape = new_GRtype), size=2.5, alpha = 0.2) +
  geom_smooth(aes(color = new_GRtype, fill=new_GRtype, linetype = new_GRtype),
              method='lm', formula= y~x, alpha = 0.2, cex=1.5)+
  labs(x=expression(sqrt("Green roof area")~(m)), y="Mean SAVI",
       group = "new_GRtype", linetype = "new_GRtype", pch = "new_GRtype")+
  scale_color_manual(name="Vegetation type",labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"),values = c("#00BFFF", "#FF762E", "#7CAE00","#cd6ee8"))+
  scale_fill_manual(name="Vegetation type",labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"),values = c("#00BFFF", "#FF762E", "#7CAE00","#cd6ee8"))+
  scale_shape_manual(name="Vegetation type", labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"), values = c(19,17, 15, 18))+
  scale_linetype_manual(name="Vegetation type", labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"), values = c(1,1,1,2))+
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
  )+
  guides(
    color = guide_legend(nrow = 2, byrow = TRUE),
    linetype = guide_legend(nrow = 2, byrow = TRUE)
  )

savi_area_2017_vegetype.plot

# Linear regression by vegetation type
summary(lm(MEAN ~ sqrt_area, data = subset(GR_savi_2017, new_GRtype =="grass")))
summary(lm(MEAN ~ sqrt_area, data = subset(GR_savi_2017, new_GRtype =="sedum_mat")))
summary(lm(MEAN ~ sqrt_area, data = subset(GR_savi_2017, new_GRtype =="woody_plants")))
summary(lm(MEAN ~ sqrt_area, data = subset(GR_savi_2017, new_GRtype =="mix_grass_tree")))


# GNDVI --------------------------------------------------------------------
### 2018 (pre-growing season) ###
# Change the level order
GR_gndvi_2018$new_GRtype <- factor(GR_gndvi_2018$new_GRtype, levels = c("grass", "sedum_mat", "woody_plants","mix_grass_tree"))

# plot
gndvi_area_2018_vegetype.plot <- ggplot(GR_gndvi_2018, aes(x=sqrt_area,y=MEAN, color = new_GRtype, linetype = new_GRtype))+
  geom_point(aes(shape = new_GRtype), size=2.5, alpha = 0.2) +
  geom_smooth(aes(color = new_GRtype, fill=new_GRtype, linetype = new_GRtype),
              method='lm', formula= y~x, alpha = 0.2, cex=1.5)+
  labs(x=expression(sqrt("Green roof area")~(m)), y="Mean GNDVI",
       group = "new_GRtype", linetype = "new_GRtype", pch = "new_GRtype")+
  scale_color_manual(name="Vegetation type",labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"),values = c("#00BFFF", "#FF762E", "#7CAE00","#cd6ee8"))+
  scale_fill_manual(name="Vegetation type",labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"),values = c("#00BFFF", "#FF762E", "#7CAE00","#cd6ee8"))+
  scale_shape_manual(name="Vegetation type", labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"), values = c(19,17, 15, 18))+
  scale_linetype_manual(name="Vegetation type", labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"), values = c(1,1,1,2))+
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
  )+
  guides(
    color = guide_legend(nrow = 2, byrow = TRUE),
    linetype = guide_legend(nrow = 2, byrow = TRUE)
  )

gndvi_area_2018_vegetype.plot

# Linear regression by vegetation type
summary(lm(MEAN ~ sqrt_area, data = subset(GR_gndvi_2018, new_GRtype =="grass")))
summary(lm(MEAN ~ sqrt_area, data = subset(GR_gndvi_2018, new_GRtype =="sedum_mat")))
summary(lm(MEAN ~ sqrt_area, data = subset(GR_gndvi_2018, new_GRtype =="woody_plants")))
summary(lm(MEAN ~ sqrt_area, data = subset(GR_gndvi_2018, new_GRtype =="mix_grass_tree")))

### 2017 (growing season) ###
# Change the level order
GR_gndvi_2017$new_GRtype <- factor(GR_gndvi_2017$new_GRtype, levels = c("grass", "sedum_mat", "woody_plants","mix_grass_tree"))

# plot
gndvi_area_2017_vegetype.plot <- ggplot(GR_gndvi_2017, aes(x=sqrt_area,y=MEAN, color = new_GRtype, linetype = new_GRtype))+
  geom_point(aes(shape = new_GRtype), size=2.5, alpha = 0.2) +
  geom_smooth(aes(color = new_GRtype, fill=new_GRtype, linetype = new_GRtype),
              method='lm', formula= y~x, alpha = 0.2, cex=1.5)+
  labs(x=expression(sqrt("Green roof area")~(m)), y="Mean GNDVI",
       group = "new_GRtype", linetype = "new_GRtype", pch = "new_GRtype")+
  scale_color_manual(name="Vegetation type",labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"),values = c("#00BFFF", "#FF762E", "#7CAE00","#cd6ee8"))+
  scale_fill_manual(name="Vegetation type",labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"),values = c("#00BFFF", "#FF762E", "#7CAE00","#cd6ee8"))+
  scale_shape_manual(name="Vegetation type", labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"), values = c(19,17, 15, 18))+
  scale_linetype_manual(name="Vegetation type", labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"), values = c(1,1,1,2))+
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
  )+
  guides(
    color = guide_legend(nrow = 2, byrow = TRUE),
    linetype = guide_legend(nrow = 2, byrow = TRUE)
  )

gndvi_area_2017_vegetype.plot

# Linear regression by vegetation type
summary(lm(MEAN ~ sqrt_area, data = subset(GR_gndvi_2017, new_GRtype =="grass")))
summary(lm(MEAN ~ sqrt_area, data = subset(GR_gndvi_2017, new_GRtype =="sedum_mat")))
summary(lm(MEAN ~ sqrt_area, data = subset(GR_gndvi_2017, new_GRtype =="woody_plants")))
summary(lm(MEAN ~ sqrt_area, data = subset(GR_gndvi_2017, new_GRtype =="mix_grass_tree")))

# CIg --------------------------------------------------------------------
### 2018 (pre-growing season) ###
# Change the level order
GR_cig_2018$new_GRtype <- factor(GR_cig_2018$new_GRtype, levels = c("grass", "sedum_mat", "woody_plants","mix_grass_tree"))

# plot
cig_area_2018_vegetype.plot <- ggplot(GR_cig_2018, aes(x=sqrt_area,y=MEAN, color = new_GRtype, linetype = new_GRtype))+
  geom_point(aes(shape = new_GRtype), size=2.5, alpha = 0.2) +
  geom_smooth(aes(color = new_GRtype, fill=new_GRtype, linetype = new_GRtype),
              method='lm', formula= y~x, alpha = 0.2, cex=1.5)+
  labs(x=expression(sqrt("Green roof area")~(m)), y="Mean CIg",
       group = "new_GRtype", linetype = "new_GRtype", pch = "new_GRtype")+
  scale_color_manual(name="Vegetation type",labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"),values = c("#00BFFF", "#FF762E", "#7CAE00","#cd6ee8"))+
  scale_fill_manual(name="Vegetation type",labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"),values = c("#00BFFF", "#FF762E", "#7CAE00","#cd6ee8"))+
  scale_shape_manual(name="Vegetation type", labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"), values = c(19,17, 15, 18))+
  scale_linetype_manual(name="Vegetation type", labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"), values = c(1,1,1,2))+
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
  )+
  guides(
    color = guide_legend(nrow = 2, byrow = TRUE),
    linetype = guide_legend(nrow = 2, byrow = TRUE)
  )

cig_area_2018_vegetype.plot

# Linear regression by vegetation type
summary(lm(MEAN ~ sqrt_area, data = subset(GR_cig_2018, new_GRtype =="grass")))
summary(lm(MEAN ~ sqrt_area, data = subset(GR_cig_2018, new_GRtype =="sedum_mat")))
summary(lm(MEAN ~ sqrt_area, data = subset(GR_cig_2018, new_GRtype =="woody_plants")))
summary(lm(MEAN ~ sqrt_area, data = subset(GR_cig_2018, new_GRtype =="mix_grass_tree")))

### 2017 (growing season) ###
# Change the level order
GR_cig_2017$new_GRtype <- factor(GR_cig_2017$new_GRtype, levels = c("grass", "sedum_mat", "woody_plants","mix_grass_tree"))

# plot
cig_area_2017_vegetype.plot <- ggplot(GR_cig_2017, aes(x=sqrt_area,y=MEAN, color = new_GRtype, linetype = new_GRtype))+
  geom_point(aes(shape = new_GRtype), size=2.5, alpha = 0.2) +
  geom_smooth(aes(color = new_GRtype, fill=new_GRtype, linetype = new_GRtype),
              method='lm', formula= y~x, alpha = 0.2, cex=1.5)+
  labs(x=expression(sqrt("Green roof area")~(m)), y="Mean CIg",
       group = "new_GRtype", linetype = "new_GRtype", pch = "new_GRtype")+
  scale_color_manual(name="Vegetation type",labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"),values = c("#00BFFF", "#FF762E", "#7CAE00","#cd6ee8"))+
  scale_fill_manual(name="Vegetation type",labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"),values = c("#00BFFF", "#FF762E", "#7CAE00","#cd6ee8"))+
  scale_shape_manual(name="Vegetation type", labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"), values = c(19,17, 15, 18))+
  scale_linetype_manual(name="Vegetation type", labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"), values = c(1,1,1,2))+
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
  )+
  guides(
    color = guide_legend(nrow = 2, byrow = TRUE),
    linetype = guide_legend(nrow = 2, byrow = TRUE)
  )

cig_area_2017_vegetype.plot

# Linear regression by vegetation type
summary(lm(MEAN ~ sqrt_area, data = subset(GR_cig_2017, new_GRtype =="grass")))
summary(lm(MEAN ~ sqrt_area, data = subset(GR_cig_2017, new_GRtype =="sedum_mat")))
summary(lm(MEAN ~ sqrt_area, data = subset(GR_cig_2017, new_GRtype =="woody_plants")))
summary(lm(MEAN ~ sqrt_area, data = subset(GR_cig_2017, new_GRtype =="mix_grass_tree")))

# Supplementary Figures S14 and S15: Mean SAVI, Mean GNDVI, Mean CIg vs. Building height (grouped by vegetation type)  --------
# SAVI --------------------------------------------------------------------
### 2018 (pre-growing season) ###
# Change the level order
GR_savi_2018$GR_type <- factor(GR_savi_2018$GR_type, levels = c("ext","int"))

savi_height_2018_vegetype.plot <- ggplot(GR_savi_2018, aes(x=MAX_HEIGHT,y=MEAN, color = new_GRtype, linetype = new_GRtype))+
  geom_point(aes(shape = new_GRtype), size=2.5, alpha = 0.2) +
  geom_smooth(aes(color = new_GRtype, fill=new_GRtype, linetype = new_GRtype),
              method='lm', formula= y~x, alpha = 0.2, cex=1.5)+
  labs(x="Building height (m)", y="Mean SAVI",
       group = "new_GRtype", linetype = "new_GRtype", pch = "new_GRtype")+
  scale_color_manual(name="Vegetation type",labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"),values = c("#00BFFF", "#FF762E", "#7CAE00","#cd6ee8"))+
  scale_fill_manual(name="Vegetation type",labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"),values = c("#00BFFF", "#FF762E", "#7CAE00","#cd6ee8"))+
  scale_shape_manual(name="Vegetation type", labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"), values = c(19,17, 15, 18))+
  scale_linetype_manual(name="Vegetation type", labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"), values = c(2,1,1,2))+
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
  )+
  guides(
    color = guide_legend(nrow = 2, byrow = TRUE),
    linetype = guide_legend(nrow = 2, byrow = TRUE)
  )

savi_height_2018_vegetype.plot

# Linear regression by vegetation type
summary(lm(MEAN ~ MAX_HEIGHT, data = subset(GR_savi_2018, new_GRtype =="grass")))
summary(lm(MEAN ~ MAX_HEIGHT, data = subset(GR_savi_2018, new_GRtype =="sedum_mat")))
summary(lm(MEAN ~ MAX_HEIGHT, data = subset(GR_savi_2018, new_GRtype =="woody_plants")))
summary(lm(MEAN ~ MAX_HEIGHT, data = subset(GR_savi_2018, new_GRtype =="mix_grass_tree")))

### 2017 (growing season) ###
# Change the level order
GR_savi_2017$GR_type <- factor(GR_savi_2017$GR_type, levels = c("ext","int"))

# plot
savi_height_2017_vegetype.plot <- ggplot(GR_savi_2017, aes(x=MAX_HEIGHT,y=MEAN, color = new_GRtype, linetype = new_GRtype))+
  geom_point(aes(shape = new_GRtype), size=2.5, alpha = 0.2) +
  geom_smooth(aes(color = new_GRtype, fill=new_GRtype, linetype = new_GRtype),
              method='lm', formula= y~x, alpha = 0.2, cex=1.5)+
  labs(x="Building height (m)", y="Mean SAVI",
       group = "new_GRtype", linetype = "new_GRtype", pch = "new_GRtype")+
  scale_color_manual(name="Vegetation type",labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"),values = c("#00BFFF", "#FF762E", "#7CAE00","#cd6ee8"))+
  scale_fill_manual(name="Vegetation type",labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"),values = c("#00BFFF", "#FF762E", "#7CAE00","#cd6ee8"))+
  scale_shape_manual(name="Vegetation type", labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"), values = c(19,17, 15, 18))+
  scale_linetype_manual(name="Vegetation type", labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"), values = c(2,1,2,2))+
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
  )+
  guides(
    color = guide_legend(nrow = 2, byrow = TRUE),
    linetype = guide_legend(nrow = 2, byrow = TRUE)
  )

savi_height_2017_vegetype.plot

# Linear regression by vegetation type
summary(lm(MEAN ~ MAX_HEIGHT, data = subset(GR_savi_2017, new_GRtype =="grass")))
summary(lm(MEAN ~ MAX_HEIGHT, data = subset(GR_savi_2017, new_GRtype =="sedum_mat")))
summary(lm(MEAN ~ MAX_HEIGHT, data = subset(GR_savi_2017, new_GRtype =="woody_plants")))
summary(lm(MEAN ~ MAX_HEIGHT, data = subset(GR_savi_2017, new_GRtype =="mix_grass_tree")))

# GNDVI --------------------------------------------------------------------
### 2018 (pre-growing season) ###
# Change the level order
GR_gndvi_2018$GR_type <- factor(GR_gndvi_2018$GR_type, levels = c("ext","int"))

# plot
gndvi_height_2018_vegetype.plot <- ggplot(GR_gndvi_2018, aes(x=MAX_HEIGHT,y=MEAN, color = new_GRtype, linetype = new_GRtype))+
  geom_point(aes(shape = new_GRtype), size=2.5, alpha = 0.2) +
  geom_smooth(aes(color = new_GRtype, fill=new_GRtype, linetype = new_GRtype),
              method='lm', formula= y~x, alpha = 0.2, cex=1.5)+
  labs(x="Building height (m)", y="Mean GNDVI",
       group = "new_GRtype", linetype = "new_GRtype", pch = "new_GRtype")+
  scale_color_manual(name="Vegetation type",labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"),values = c("#00BFFF", "#FF762E", "#7CAE00","#cd6ee8"))+
  scale_fill_manual(name="Vegetation type",labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"),values = c("#00BFFF", "#FF762E", "#7CAE00","#cd6ee8"))+
  scale_shape_manual(name="Vegetation type", labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"), values = c(19,17, 15, 18))+
  scale_linetype_manual(name="Vegetation type", labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"), values = c(2,1,1,2))+
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
  )+
  guides(
    color = guide_legend(nrow = 2, byrow = TRUE),
    linetype = guide_legend(nrow = 2, byrow = TRUE)
  )

gndvi_height_2018_vegetype.plot

# Linear regression by vegetation type
summary(lm(MEAN ~ MAX_HEIGHT, data = subset(GR_gndvi_2018, new_GRtype =="grass")))
summary(lm(MEAN ~ MAX_HEIGHT, data = subset(GR_gndvi_2018, new_GRtype =="sedum_mat")))
summary(lm(MEAN ~ MAX_HEIGHT, data = subset(GR_gndvi_2018, new_GRtype =="woody_plants")))
summary(lm(MEAN ~ MAX_HEIGHT, data = subset(GR_gndvi_2018, new_GRtype =="mix_grass_tree")))

### 2017 (growing season) ###
# Change the level order
GR_gndvi_2017$GR_type <- factor(GR_gndvi_2017$GR_type, levels = c("ext","int"))

# plot
gndvi_height_2017_vegetype.plot <- ggplot(GR_gndvi_2017, aes(x=MAX_HEIGHT,y=MEAN, color = new_GRtype, linetype = new_GRtype))+
  geom_point(aes(shape = new_GRtype), size=2.5, alpha = 0.2) +
  geom_smooth(aes(color = new_GRtype, fill=new_GRtype, linetype = new_GRtype),
              method='lm', formula= y~x, alpha = 0.2, cex=1.5)+
  labs(x="Building height (m)", y="Mean GNDVI",
       group = "new_GRtype", linetype = "new_GRtype", pch = "new_GRtype")+
  scale_color_manual(name="Vegetation type",labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"),values = c("#00BFFF", "#FF762E", "#7CAE00","#cd6ee8"))+
  scale_fill_manual(name="Vegetation type",labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"),values = c("#00BFFF", "#FF762E", "#7CAE00","#cd6ee8"))+
  scale_shape_manual(name="Vegetation type", labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"), values = c(19,17, 15, 18))+
  scale_linetype_manual(name="Vegetation type", labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"), values = c(2,1,1,2))+
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
  )+
  guides(
    color = guide_legend(nrow = 2, byrow = TRUE),
    linetype = guide_legend(nrow = 2, byrow = TRUE)
  )

gndvi_height_2017_vegetype.plot

# Linear regression by vegetation type
summary(lm(MEAN ~ MAX_HEIGHT, data = subset(GR_gndvi_2017, new_GRtype =="grass")))
summary(lm(MEAN ~ MAX_HEIGHT, data = subset(GR_gndvi_2017, new_GRtype =="sedum_mat")))
summary(lm(MEAN ~ MAX_HEIGHT, data = subset(GR_gndvi_2017, new_GRtype =="woody_plants")))
summary(lm(MEAN ~ MAX_HEIGHT, data = subset(GR_gndvi_2017, new_GRtype =="mix_grass_tree")))

# CIg --------------------------------------------------------------------
### 2018 (pre-growing season) ###
# Change the level order
GR_cig_2018$GR_type <- factor(GR_cig_2018$GR_type, levels = c("ext","int"))

# plot
cig_height_2018_vegetype.plot <- ggplot(GR_cig_2018, aes(x=MAX_HEIGHT,y=MEAN, color = new_GRtype, linetype = new_GRtype))+
  geom_point(aes(shape = new_GRtype), size=2.5, alpha = 0.2) +
  geom_smooth(aes(color = new_GRtype, fill=new_GRtype, linetype = new_GRtype),
              method='lm', formula= y~x, alpha = 0.2, cex=1.5)+
  labs(x="Building height (m)", y="Mean CIg",
       group = "new_GRtype", linetype = "new_GRtype", pch = "new_GRtype")+
  scale_color_manual(name="Vegetation type",labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"),values = c("#00BFFF", "#FF762E", "#7CAE00","#cd6ee8"))+
  scale_fill_manual(name="Vegetation type",labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"),values = c("#00BFFF", "#FF762E", "#7CAE00","#cd6ee8"))+
  scale_shape_manual(name="Vegetation type", labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"), values = c(19,17, 15, 18))+
  scale_linetype_manual(name="Vegetation type", labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"), values = c(2,1,1,2))+
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
  )+
  guides(
    color = guide_legend(nrow = 2, byrow = TRUE),
    linetype = guide_legend(nrow = 2, byrow = TRUE)
  )

cig_height_2018_vegetype.plot

# Linear regression by vegetation type
summary(lm(MEAN ~ MAX_HEIGHT, data = subset(GR_cig_2018, new_GRtype =="grass")))
summary(lm(MEAN ~ MAX_HEIGHT, data = subset(GR_cig_2018, new_GRtype =="sedum_mat")))
summary(lm(MEAN ~ MAX_HEIGHT, data = subset(GR_cig_2018, new_GRtype =="woody_plants")))
summary(lm(MEAN ~ MAX_HEIGHT, data = subset(GR_cig_2018, new_GRtype =="mix_grass_tree")))

### 2017 (growing season) ###
# Change the level order
GR_cig_2017$GR_type <- factor(GR_cig_2017$GR_type, levels = c("ext","int"))

# plot
cig_height_2017_vegetype.plot <- ggplot(GR_cig_2017, aes(x=MAX_HEIGHT,y=MEAN, color = new_GRtype, linetype = new_GRtype))+
  geom_point(aes(shape = new_GRtype), size=2.5, alpha = 0.2) +
  geom_smooth(aes(color = new_GRtype, fill=new_GRtype, linetype = new_GRtype),
              method='lm', formula= y~x, alpha = 0.2, cex=1.5)+
  labs(x="Building height (m)", y="Mean CIg",
       group = "new_GRtype", linetype = "new_GRtype", pch = "new_GRtype")+
  scale_color_manual(name="Vegetation type",labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"),values = c("#00BFFF", "#FF762E", "#7CAE00","#cd6ee8"))+
  scale_fill_manual(name="Vegetation type",labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"),values = c("#00BFFF", "#FF762E", "#7CAE00","#cd6ee8"))+
  scale_shape_manual(name="Vegetation type", labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"), values = c(19,17, 15, 18))+
  scale_linetype_manual(name="Vegetation type", labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"), values = c(2,1,1,2))+
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
  )+
  guides(
    color = guide_legend(nrow = 2, byrow = TRUE),
    linetype = guide_legend(nrow = 2, byrow = TRUE)
  )

cig_height_2017_vegetype.plot

# Linear regression by vegetation type
summary(lm(MEAN ~ MAX_HEIGHT, data = subset(GR_cig_2017, new_GRtype =="grass")))
summary(lm(MEAN ~ MAX_HEIGHT, data = subset(GR_cig_2017, new_GRtype =="sedum_mat")))
summary(lm(MEAN ~ MAX_HEIGHT, data = subset(GR_cig_2017, new_GRtype =="woody_plants")))
summary(lm(MEAN ~ MAX_HEIGHT, data = subset(GR_cig_2017, new_GRtype =="mix_grass_tree")))


# Supplementary Figures S14 and S15: Mean SAVI, Mean GNDVI, Mean CIg vs. vegetation type  --------
# SAVI --------------------------------------------------------------------
### 2018 (pre-growing season) ###
# Summarize the data
summary_vegetype_savi_2018 <-
  do.call(data.frame,
          aggregate(MEAN ~ new_GRtype,
                    data = GR_savi_2018,
                    FUN = function(x) c(mean_value = mean(x),se_value = get_se(x))))

# Change the level order
summary_vegetype_savi_2018$new_GRtype <- factor(summary_vegetype_savi_2018$new_GRtype, levels = c("grass","sedum_mat","woody_plants","mix_grass_tree"))

# plot
savi_vegetype_barplot_2018 <- ggplot(summary_vegetype_savi_2018, aes(x = new_GRtype, y = MEAN.mean_value, fill = new_GRtype)) +
  geom_bar(stat="identity", colour = "black", size = 0.3) +
  scale_fill_manual(name="Vegetation type",labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"),values = c("#00BFFF", "#FF762E", "#7CAE00","#cd6ee8"))+
  scale_x_discrete(name="Vegetation type",labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"))+
  geom_errorbar(aes(ymin = MEAN.mean_value - MEAN.se_value,
                    ymax = MEAN.mean_value + MEAN.se_value),
                width = 0.3, linetype = 1)+
  labs(x = "Green roof type", y = "Mean SAVI",
       group = "GR_type")+
  coord_cartesian(ylim=c(0,0.16))+
  theme_bw(base_size = 28) + 
  theme(
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text=element_text(size=22, color = "black"),
    axis.title=element_text(size=32, color = "black"),
    axis.text.x = element_text(angle = 15,hjust=1),
    legend.title = element_blank(),
    legend.position = "none"
  )

print(savi_vegetype_barplot_2018)

# ANOVA
summary(aov(MEAN~new_GRtype, data = GR_savi_2018))
print(HSD.test(aov(MEAN~new_GRtype, data = GR_savi_2018), "new_GRtype"))

### 2017 (growing season) ###
# Summarize the data
summary_vegetype_savi_2017 <-
  do.call(data.frame,
          aggregate(MEAN ~ new_GRtype,
                    data = GR_savi_2017,
                    FUN = function(x) c(mean_value = mean(x),se_value = get_se(x))))

# Change the level order
summary_vegetype_savi_2017$new_GRtype <- factor(summary_vegetype_savi_2017$new_GRtype, levels = c("grass","sedum_mat","woody_plants","mix_grass_tree"))

# plot
savi_vegetype_barplot_2017 <- ggplot(summary_vegetype_savi_2017, aes(x = new_GRtype, y = MEAN.mean_value, fill = new_GRtype)) +
  geom_bar(stat="identity", colour = "black", size = 0.3) +
  scale_fill_manual(name="Vegetation type",labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"),values = c("#00BFFF", "#FF762E", "#7CAE00","#cd6ee8"))+
  scale_x_discrete(name="Vegetation type",labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"))+
  geom_errorbar(aes(ymin = MEAN.mean_value - MEAN.se_value,
                    ymax = MEAN.mean_value + MEAN.se_value),
                width = 0.3, linetype = 1)+
  labs(x = "Green roof type", y = "Mean SAVI",
       group = "GR_type")+
  coord_cartesian(ylim=c(0,0.23))+
  theme_bw(base_size = 28) + 
  theme(
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text=element_text(size=22, color = "black"),
    axis.title=element_text(size=32, color = "black"),
    axis.text.x = element_text(angle = 15,hjust=1),
    legend.title = element_blank(),
    legend.position = "none"
  )

print(savi_vegetype_barplot_2017)

# ANOVA
summary(aov(MEAN~new_GRtype, data = GR_savi_2017))
print(HSD.test(aov(MEAN~new_GRtype, data = GR_savi_2017), "new_GRtype"))

# GNDVI --------------------------------------------------------------------
### 2018 (pre-growing season) ###
# Summarize the data
summary_vegetype_gndvi_2018 <-
  do.call(data.frame,
          aggregate(MEAN ~ new_GRtype,
                    data = GR_gndvi_2018,
                    FUN = function(x) c(mean_value = mean(x),se_value = get_se(x))))

# Change the level order
summary_vegetype_gndvi_2018$new_GRtype <- factor(summary_vegetype_gndvi_2018$new_GRtype, levels = c("grass","sedum_mat","woody_plants","mix_grass_tree"))

# plot
gndvi_vegetype_barplot_2018 <- ggplot(summary_vegetype_gndvi_2018, aes(x = new_GRtype, y = MEAN.mean_value, fill = new_GRtype)) +
  geom_bar(stat="identity", colour = "black", size = 0.3) +
  scale_fill_manual(name="Vegetation type",labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"),values = c("#00BFFF", "#FF762E", "#7CAE00","#cd6ee8"))+
  scale_x_discrete(name="Vegetation type",labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"))+
  geom_errorbar(aes(ymin = MEAN.mean_value - MEAN.se_value,
                    ymax = MEAN.mean_value + MEAN.se_value),
                width = 0.3, linetype = 1)+
  labs(x = "Green roof type", y = "Mean GNDVI",
       group = "GR_type")+
  coord_cartesian(ylim=c(0,0.28))+
  theme_bw(base_size = 28) + 
  theme(
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text=element_text(size=22, color = "black"),
    axis.title=element_text(size=32, color = "black"),
    axis.text.x = element_text(angle = 15,hjust=1),
    legend.title = element_blank(),
    legend.position = "none"
  )

print(gndvi_vegetype_barplot_2018)

# ANOVA
summary(aov(MEAN~new_GRtype, data = GR_gndvi_2018))
print(HSD.test(aov(MEAN~new_GRtype, data = GR_gndvi_2018), "new_GRtype"))


### 2017 (growing season) ###
# Summarize the data
summary_vegetype_gndvi_2017 <-
  do.call(data.frame,
          aggregate(MEAN ~ new_GRtype,
                    data = GR_gndvi_2017,
                    FUN = function(x) c(mean_value = mean(x),se_value = get_se(x))))

# Change the level order
summary_vegetype_gndvi_2017$new_GRtype <- factor(summary_vegetype_gndvi_2017$new_GRtype, levels = c("grass","sedum_mat","woody_plants","mix_grass_tree"))

# plot
gndvi_vegetype_barplot_2017 <- ggplot(summary_vegetype_gndvi_2017, aes(x = new_GRtype, y = MEAN.mean_value, fill = new_GRtype)) +
  geom_bar(stat="identity", colour = "black", size = 0.3) +
  scale_fill_manual(name="Vegetation type",labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"),values = c("#00BFFF", "#FF762E", "#7CAE00","#cd6ee8"))+
  scale_x_discrete(name="Vegetation type",labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"))+
  geom_errorbar(aes(ymin = MEAN.mean_value - MEAN.se_value,
                    ymax = MEAN.mean_value + MEAN.se_value),
                width = 0.3, linetype = 1)+
  labs(x = "Green roof type", y = "Mean GNDVI",
       group = "GR_type")+
  coord_cartesian(ylim=c(0,0.35))+
  theme_bw(base_size = 28) + 
  theme(
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text=element_text(size=22, color = "black"),
    axis.title=element_text(size=32, color = "black"),
    axis.text.x = element_text(angle = 15,hjust=1),
    legend.title = element_blank(),
    legend.position = "none"
  )

print(gndvi_vegetype_barplot_2017)

# ANOVA
summary(aov(MEAN~new_GRtype, data = GR_gndvi_2017))
print(HSD.test(aov(MEAN~new_GRtype, data = GR_gndvi_2017), "new_GRtype"))


# CIg --------------------------------------------------------------------
### 2018 (pre-growing season) ###
# Summarize the data
summary_vegetype_cig_2018 <-
  do.call(data.frame,
          aggregate(MEAN ~ new_GRtype,
                    data = GR_cig_2018,
                    FUN = function(x) c(mean_value = mean(x),se_value = get_se(x))))

# Change the level order
summary_vegetype_cig_2018$new_GRtype <- factor(summary_vegetype_cig_2018$new_GRtype, levels = c("grass","sedum_mat","woody_plants","mix_grass_tree"))

# plot
cig_vegetype_barplot_2018 <- ggplot(summary_vegetype_cig_2018, aes(x = new_GRtype, y = MEAN.mean_value, fill = new_GRtype)) +
  geom_bar(stat="identity", colour = "black", size = 0.3) +
  scale_fill_manual(name="Vegetation type",labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"),values = c("#00BFFF", "#FF762E", "#7CAE00","#cd6ee8"))+
  scale_x_discrete(name="Vegetation type",labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"))+
  geom_errorbar(aes(ymin = MEAN.mean_value - MEAN.se_value,
                    ymax = MEAN.mean_value + MEAN.se_value),
                width = 0.3, linetype = 1)+
  labs(x = "Green roof type", y = "Mean CIg",
       group = "GR_type")+
  coord_cartesian(ylim=c(0,0.73))+
  theme_bw(base_size = 28) + 
  theme(
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text=element_text(size=22, color = "black"),
    axis.title=element_text(size=32, color = "black"),
    axis.text.x = element_text(angle = 15,hjust=1),
    legend.title = element_blank(),
    legend.position = "none"
  )

print(cig_vegetype_barplot_2018)

# ANOVA
summary(aov(MEAN~new_GRtype, data = GR_cig_2018))
print(HSD.test(aov(MEAN~new_GRtype, data = GR_cig_2018), "new_GRtype"))

### 2017 (growing season) ###
# Summarize the data
summary_vegetype_cig_2017 <-
  do.call(data.frame,
          aggregate(MEAN ~ new_GRtype,
                    data = GR_cig_2017,
                    FUN = function(x) c(mean_value = mean(x),se_value = get_se(x))))

# Change the level order
summary_vegetype_cig_2017$new_GRtype <- factor(summary_vegetype_cig_2017$new_GRtype, levels = c("grass","sedum_mat","woody_plants","mix_grass_tree"))

# plot
cig_vegetype_barplot_2017 <- ggplot(summary_vegetype_cig_2017, aes(x = new_GRtype, y = MEAN.mean_value, fill = new_GRtype)) +
  geom_bar(stat="identity", colour = "black", size = 0.3) +
  scale_fill_manual(name="Vegetation type",labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"),values = c("#00BFFF", "#FF762E", "#7CAE00","#cd6ee8"))+
  scale_x_discrete(name="Vegetation type",labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"))+
  geom_errorbar(aes(ymin = MEAN.mean_value - MEAN.se_value,
                    ymax = MEAN.mean_value + MEAN.se_value),
                width = 0.3, linetype = 1)+
  labs(x = "Green roof type", y = "Mean CIg",
       group = "GR_type")+
  coord_cartesian(ylim=c(0,1))+
  theme_bw(base_size = 28) + 
  theme(
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text=element_text(size=22, color = "black"),
    axis.title=element_text(size=32, color = "black"),
    axis.text.x = element_text(angle = 15,hjust=1),
    legend.title = element_blank(),
    legend.position = "none"
  )

print(cig_vegetype_barplot_2017)

# ANOVA
summary(aov(MEAN~new_GRtype, data = GR_cig_2017))
print(HSD.test(aov(MEAN~new_GRtype, data = GR_cig_2017), "new_GRtype"))

# Supplementary Figures S14 and S15: Mean SAVI, Mean GNDVI, Mean CIg vs. SQRT Aspect ratio (grouped by vegetation type) --------
# SAVI --------------------------------------------------------------------
### 2018 (pre-growing season) ###
# change order of display
GR_savi_2018$new_GRtype <- factor(GR_savi_2018$new_GRtype, levels = c("grass", "sedum_mat", "woody_plants","mix_grass_tree"))

# plot
savi_sqrt_AR_vegetype_2018.plot <- ggplot(GR_savi_2018, aes(x=sqrt_AR,y=MEAN, color = new_GRtype, linetype = new_GRtype))+
  geom_point(aes(shape = new_GRtype), size=2.5, alpha = 0.2) +
  geom_smooth(aes(color = new_GRtype, fill=new_GRtype, linetype = new_GRtype),
              method='lm', formula= y~x, alpha = 0.2, cex=1.5)+
  labs(x=expression(sqrt("Aspect ratio")), y="Mean SAVI",
       group = "new_GRtype", linetype = "new_GRtype", pch = "new_GRtype")+
  scale_color_manual(name="Vegetation type",labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"),values = c("#00BFFF", "#FF762E", "#7CAE00","#cd6ee8"))+
  scale_fill_manual(name="Vegetation type",labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"),values = c("#00BFFF", "#FF762E", "#7CAE00","#cd6ee8"))+
  scale_linetype_manual(name="Vegetation type", labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"), values = c(2,2, 1, 2))+
  scale_shape_manual(name="Vegetation type", labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"), values = c(19,17, 15, 18)) +
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
  )+
  guides(
    color = guide_legend(nrow = 2, byrow = TRUE),
    linetype = guide_legend(nrow = 2, byrow = TRUE)
  )

savi_sqrt_AR_vegetype_2018.plot

# Linear regression by vegetation type
summary(lm(MEAN ~ sqrt_AR, data = subset(GR_savi_2018, new_GRtype =="grass")))
summary(lm(MEAN ~ sqrt_AR, data = subset(GR_savi_2018, new_GRtype =="sedum_mat")))
summary(lm(MEAN ~ sqrt_AR, data = subset(GR_savi_2018, new_GRtype =="woody_plants")))
summary(lm(MEAN ~ sqrt_AR, data = subset(GR_savi_2018, new_GRtype =="mix_grass_tree")))

### 2017 (growing season) ###
# change order of display
GR_savi_2017$new_GRtype <- factor(GR_savi_2017$new_GRtype, levels = c("grass", "sedum_mat", "woody_plants","mix_grass_tree"))

# plot
savi_sqrt_AR_vegetype_2017.plot <- ggplot(GR_savi_2017, aes(x=sqrt_AR,y=MEAN, color = new_GRtype, linetype = new_GRtype))+
  geom_point(aes(shape = new_GRtype), size=2.5, alpha = 0.2) +
  geom_smooth(aes(color = new_GRtype, fill=new_GRtype, linetype = new_GRtype),
              method='lm', formula= y~x, alpha = 0.2, cex=1.5)+
  labs(x=expression(sqrt("Aspect ratio")), y="Mean SAVI",
       group = "new_GRtype", linetype = "new_GRtype", pch = "new_GRtype")+
  scale_color_manual(name="Vegetation type",labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"),values = c("#00BFFF", "#FF762E", "#7CAE00","#cd6ee8"))+
  scale_fill_manual(name="Vegetation type",labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"),values = c("#00BFFF", "#FF762E", "#7CAE00","#cd6ee8"))+
  scale_linetype_manual(name="Vegetation type", labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"), values = c(2,2, 1, 2))+
  scale_shape_manual(name="Vegetation type", labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"), values = c(19,17, 15, 18)) +
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
  )+
  guides(
    color = guide_legend(nrow = 2, byrow = TRUE),
    linetype = guide_legend(nrow = 2, byrow = TRUE)
  )

savi_sqrt_AR_vegetype_2017.plot

# Linear regression by vegetation type
summary(lm(MEAN ~ sqrt_AR, data = subset(GR_savi_2017, new_GRtype =="grass")))
summary(lm(MEAN ~ sqrt_AR, data = subset(GR_savi_2017, new_GRtype =="sedum_mat")))
summary(lm(MEAN ~ sqrt_AR, data = subset(GR_savi_2017, new_GRtype =="woody_plants")))
summary(lm(MEAN ~ sqrt_AR, data = subset(GR_savi_2017, new_GRtype =="mix_grass_tree")))

# GNDVI --------------------------------------------------------------------
### 2018 (pre-growing season) ###
# change order of display
GR_gndvi_2018$new_GRtype <- factor(GR_gndvi_2018$new_GRtype, levels = c("grass", "sedum_mat", "woody_plants","mix_grass_tree"))

# plot
gndvi_sqrt_AR_vegetype_2018.plot <- ggplot(GR_gndvi_2018, aes(x=sqrt_AR,y=MEAN, color = new_GRtype, linetype = new_GRtype))+
  geom_point(aes(shape = new_GRtype), size=2.5, alpha = 0.2) +
  geom_smooth(aes(color = new_GRtype, fill=new_GRtype, linetype = new_GRtype),
              method='lm', formula= y~x, alpha = 0.2, cex=1.5)+
  labs(x=expression(sqrt("Aspect ratio")), y="Mean GNDVI",
       group = "new_GRtype", linetype = "new_GRtype", pch = "new_GRtype")+
  scale_color_manual(name="Vegetation type",labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"),values = c("#00BFFF", "#FF762E", "#7CAE00","#cd6ee8"))+
  scale_fill_manual(name="Vegetation type",labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"),values = c("#00BFFF", "#FF762E", "#7CAE00","#cd6ee8"))+
  scale_linetype_manual(name="Vegetation type", labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"), values = c(2,2, 1, 2))+
  scale_shape_manual(name="Vegetation type", labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"), values = c(19,17, 15, 18)) +
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
  )+
  guides(
    color = guide_legend(nrow = 2, byrow = TRUE),
    linetype = guide_legend(nrow = 2, byrow = TRUE)
  )

gndvi_sqrt_AR_vegetype_2018.plot

# Linear regression by vegetation type
summary(lm(MEAN ~ sqrt_AR, data = subset(GR_gndvi_2018, new_GRtype =="grass")))
summary(lm(MEAN ~ sqrt_AR, data = subset(GR_gndvi_2018, new_GRtype =="sedum_mat")))
summary(lm(MEAN ~ sqrt_AR, data = subset(GR_gndvi_2018, new_GRtype =="woody_plants")))
summary(lm(MEAN ~ sqrt_AR, data = subset(GR_gndvi_2018, new_GRtype =="mix_grass_tree")))

### 2017 (growing season) ###
# change order of display
GR_gndvi_2017$new_GRtype <- factor(GR_gndvi_2017$new_GRtype, levels = c("grass", "sedum_mat", "woody_plants","mix_grass_tree"))

# plot
gndvi_sqrt_AR_vegetype_2017.plot <- ggplot(GR_gndvi_2017, aes(x=sqrt_AR,y=MEAN, color = new_GRtype, linetype = new_GRtype))+
  geom_point(aes(shape = new_GRtype), size=2.5, alpha = 0.2) +
  geom_smooth(aes(color = new_GRtype, fill=new_GRtype, linetype = new_GRtype),
              method='lm', formula= y~x, alpha = 0.2, cex=1.5)+
  labs(x=expression(sqrt("Aspect ratio")), y="Mean GNDVI",
       group = "new_GRtype", linetype = "new_GRtype", pch = "new_GRtype")+
  scale_color_manual(name="Vegetation type",labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"),values = c("#00BFFF", "#FF762E", "#7CAE00","#cd6ee8"))+
  scale_fill_manual(name="Vegetation type",labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"),values = c("#00BFFF", "#FF762E", "#7CAE00","#cd6ee8"))+
  scale_linetype_manual(name="Vegetation type", labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"), values = c(2,1, 1, 2))+
  scale_shape_manual(name="Vegetation type", labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"), values = c(19,17, 15, 18)) +
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
  )+
  guides(
    color = guide_legend(nrow = 2, byrow = TRUE),
    linetype = guide_legend(nrow = 2, byrow = TRUE)
  )

gndvi_sqrt_AR_vegetype_2017.plot

# Linear regression by vegetation type
summary(lm(MEAN ~ sqrt_AR, data = subset(GR_gndvi_2017, new_GRtype =="grass")))
summary(lm(MEAN ~ sqrt_AR, data = subset(GR_gndvi_2017, new_GRtype =="sedum_mat")))
summary(lm(MEAN ~ sqrt_AR, data = subset(GR_gndvi_2017, new_GRtype =="woody_plants")))
summary(lm(MEAN ~ sqrt_AR, data = subset(GR_gndvi_2017, new_GRtype =="mix_grass_tree")))

# CIg --------------------------------------------------------------------
### 2018 (pre-growing season) ###
# change order of display
GR_cig_2018$new_GRtype <- factor(GR_cig_2018$new_GRtype, levels = c("grass", "sedum_mat", "woody_plants","mix_grass_tree"))

# plot
cig_sqrt_AR_vegetype_2018.plot <- ggplot(GR_cig_2018, aes(x=sqrt_AR,y=MEAN, color = new_GRtype, linetype = new_GRtype))+
  geom_point(aes(shape = new_GRtype), size=2.5, alpha = 0.2) +
  geom_smooth(aes(color = new_GRtype, fill=new_GRtype, linetype = new_GRtype),
              method='lm', formula= y~x, alpha = 0.2, cex=1.5)+
  labs(x=expression(sqrt("Aspect ratio")), y="Mean CIg",
       group = "new_GRtype", linetype = "new_GRtype", pch = "new_GRtype")+
  scale_color_manual(name="Vegetation type",labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"),values = c("#00BFFF", "#FF762E", "#7CAE00","#cd6ee8"))+
  scale_fill_manual(name="Vegetation type",labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"),values = c("#00BFFF", "#FF762E", "#7CAE00","#cd6ee8"))+
  scale_linetype_manual(name="Vegetation type", labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"), values = c(2,2, 1, 2))+
  scale_shape_manual(name="Vegetation type", labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"), values = c(19,17, 15, 18)) +
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
  )+
  guides(
    color = guide_legend(nrow = 2, byrow = TRUE),
    linetype = guide_legend(nrow = 2, byrow = TRUE)
  )

cig_sqrt_AR_vegetype_2018.plot

# Linear regression by vegetation type
summary(lm(MEAN ~ sqrt_AR, data = subset(GR_cig_2018, new_GRtype =="grass")))
summary(lm(MEAN ~ sqrt_AR, data = subset(GR_cig_2018, new_GRtype =="sedum_mat")))
summary(lm(MEAN ~ sqrt_AR, data = subset(GR_cig_2018, new_GRtype =="woody_plants")))
summary(lm(MEAN ~ sqrt_AR, data = subset(GR_cig_2018, new_GRtype =="mix_grass_tree")))

### 2017 (growing season) ###
# change order of display
GR_cig_2017$new_GRtype <- factor(GR_cig_2017$new_GRtype, levels = c("grass", "sedum_mat", "woody_plants","mix_grass_tree"))

# plot
cig_sqrt_AR_vegetype_2017.plot <- ggplot(GR_cig_2017, aes(x=sqrt_AR,y=MEAN, color = new_GRtype, linetype = new_GRtype))+
  geom_point(aes(shape = new_GRtype), size=2.5, alpha = 0.2) +
  geom_smooth(aes(color = new_GRtype, fill=new_GRtype, linetype = new_GRtype),
              method='lm', formula= y~x, alpha = 0.2, cex=1.5)+
  labs(x=expression(sqrt("Aspect ratio")), y="Mean CIg",
       group = "new_GRtype", linetype = "new_GRtype", pch = "new_GRtype")+
  scale_color_manual(name="Vegetation type",labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"),values = c("#00BFFF", "#FF762E", "#7CAE00","#cd6ee8"))+
  scale_fill_manual(name="Vegetation type",labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"),values = c("#00BFFF", "#FF762E", "#7CAE00","#cd6ee8"))+
  scale_linetype_manual(name="Vegetation type", labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"), values = c(2,1, 1, 2))+
  scale_shape_manual(name="Vegetation type", labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"), values = c(19,17, 15, 18)) +
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
  )+
  guides(
    color = guide_legend(nrow = 2, byrow = TRUE),
    linetype = guide_legend(nrow = 2, byrow = TRUE)
  )

cig_sqrt_AR_vegetype_2017.plot

# Linear regression by vegetation type
summary(lm(MEAN ~ sqrt_AR, data = subset(GR_cig_2017, new_GRtype =="grass")))
summary(lm(MEAN ~ sqrt_AR, data = subset(GR_cig_2017, new_GRtype =="sedum_mat")))
summary(lm(MEAN ~ sqrt_AR, data = subset(GR_cig_2017, new_GRtype =="woody_plants")))
summary(lm(MEAN ~ sqrt_AR, data = subset(GR_cig_2017, new_GRtype =="mix_grass_tree")))


# Combine Supplementary Figure S14: Mean VIs grouped by vegetation type (growing season) --------
merge_mean_VIs_design_vegetype_2017 = 
  savi_area_2017_vegetype.plot + savi_height_2017_vegetype.plot + savi_vegetype_barplot_2017 + savi_sqrt_AR_vegetype_2017.plot +
  gndvi_area_2017_vegetype.plot + gndvi_height_2017_vegetype.plot + gndvi_vegetype_barplot_2017 + gndvi_sqrt_AR_vegetype_2017.plot +
  cig_area_2017_vegetype.plot + cig_height_2017_vegetype.plot + cig_vegetype_barplot_2017 + cig_sqrt_AR_vegetype_2017.plot +
  plot_layout(guides = "collect", ncol = 4) & theme(legend.position = "top")

merge_mean_VIs_design_vegetype_2017[[1]] = merge_mean_VIs_design_vegetype_2017[[1]] + theme(axis.title.x = element_blank())
merge_mean_VIs_design_vegetype_2017[[2]] = merge_mean_VIs_design_vegetype_2017[[2]] + theme(legend.position = "none", axis.title.y = element_blank(), axis.title.x = element_blank())
merge_mean_VIs_design_vegetype_2017[[3]] = merge_mean_VIs_design_vegetype_2017[[3]] + theme(legend.position = "none", axis.title.y = element_blank(), axis.title.x = element_blank())
merge_mean_VIs_design_vegetype_2017[[4]] = merge_mean_VIs_design_vegetype_2017[[4]] + theme(legend.position = "none", axis.title.y = element_blank(), axis.title.x = element_blank())
merge_mean_VIs_design_vegetype_2017[[5]] = merge_mean_VIs_design_vegetype_2017[[5]] + theme(axis.title.x = element_blank())
merge_mean_VIs_design_vegetype_2017[[6]] = merge_mean_VIs_design_vegetype_2017[[6]] + theme(legend.position = "none", axis.title.y = element_blank(), axis.title.x = element_blank())
merge_mean_VIs_design_vegetype_2017[[7]] = merge_mean_VIs_design_vegetype_2017[[7]] + theme(legend.position = "none", axis.title.y = element_blank(), axis.title.x = element_blank())
merge_mean_VIs_design_vegetype_2017[[8]] = merge_mean_VIs_design_vegetype_2017[[8]] + theme(legend.position = "none", axis.title.y = element_blank(), axis.title.x = element_blank())
merge_mean_VIs_design_vegetype_2017[[9]] = merge_mean_VIs_design_vegetype_2017[[9]] + theme(legend.position = "none")
merge_mean_VIs_design_vegetype_2017[[10]] = merge_mean_VIs_design_vegetype_2017[[10]] + theme(legend.position = "none", axis.title.y = element_blank())
merge_mean_VIs_design_vegetype_2017[[11]] = merge_mean_VIs_design_vegetype_2017[[11]] + theme(legend.position = "none", axis.title.y = element_blank())
merge_mean_VIs_design_vegetype_2017[[12]] = merge_mean_VIs_design_vegetype_2017[[12]] + theme(legend.position = "none", axis.title.y = element_blank())

merge_mean_VIs_design_vegetype_2017

# ggsave(plot=merge_mean_VIs_design_vegetype_2017, filename = "path/VIs_DesignFactors_vegetype_2017.tif", width = 24, height = 16, device='tiff', dpi=600,unit="in")


# Combine Supplementary Figure S15: Mean VIs grouped by vegetation type (pre-growing season) --------
merge_mean_VIs_design_vegetype_2018 = 
  savi_area_2018_vegetype.plot + savi_height_2018_vegetype.plot + savi_vegetype_barplot_2018 + savi_sqrt_AR_vegetype_2018.plot +
  gndvi_area_2018_vegetype.plot + gndvi_height_2018_vegetype.plot + gndvi_vegetype_barplot_2018 + gndvi_sqrt_AR_vegetype_2018.plot +
  cig_area_2018_vegetype.plot + cig_height_2018_vegetype.plot + cig_vegetype_barplot_2018 + cig_sqrt_AR_vegetype_2018.plot +
  plot_layout(guides = "collect", ncol = 4) & theme(legend.position = "top")

merge_mean_VIs_design_vegetype_2018[[1]] = merge_mean_VIs_design_vegetype_2018[[1]] + theme(axis.title.x = element_blank())
merge_mean_VIs_design_vegetype_2018[[2]] = merge_mean_VIs_design_vegetype_2018[[2]] + theme(legend.position = "none", axis.title.y = element_blank(), axis.title.x = element_blank())
merge_mean_VIs_design_vegetype_2018[[3]] = merge_mean_VIs_design_vegetype_2018[[3]] + theme(legend.position = "none", axis.title.y = element_blank(), axis.title.x = element_blank())
merge_mean_VIs_design_vegetype_2018[[4]] = merge_mean_VIs_design_vegetype_2018[[4]] + theme(legend.position = "none", axis.title.y = element_blank(), axis.title.x = element_blank())
merge_mean_VIs_design_vegetype_2018[[5]] = merge_mean_VIs_design_vegetype_2018[[5]] + theme(axis.title.x = element_blank())
merge_mean_VIs_design_vegetype_2018[[6]] = merge_mean_VIs_design_vegetype_2018[[6]] + theme(legend.position = "none", axis.title.y = element_blank(), axis.title.x = element_blank())
merge_mean_VIs_design_vegetype_2018[[7]] = merge_mean_VIs_design_vegetype_2018[[7]] + theme(legend.position = "none", axis.title.y = element_blank(), axis.title.x = element_blank())
merge_mean_VIs_design_vegetype_2018[[8]] = merge_mean_VIs_design_vegetype_2018[[8]] + theme(legend.position = "none", axis.title.y = element_blank(), axis.title.x = element_blank())
merge_mean_VIs_design_vegetype_2018[[9]] = merge_mean_VIs_design_vegetype_2018[[9]] + theme(legend.position = "none")
merge_mean_VIs_design_vegetype_2018[[10]] = merge_mean_VIs_design_vegetype_2018[[10]] + theme(legend.position = "none", axis.title.y = element_blank())
merge_mean_VIs_design_vegetype_2018[[11]] = merge_mean_VIs_design_vegetype_2018[[11]] + theme(legend.position = "none", axis.title.y = element_blank())
merge_mean_VIs_design_vegetype_2018[[12]] = merge_mean_VIs_design_vegetype_2018[[12]] + theme(legend.position = "none", axis.title.y = element_blank())

merge_mean_VIs_design_vegetype_2018

# ggsave(plot=merge_mean_VIs_design_vegetype_2018, filename = "path/VIs_DesignFactors_vegetype_2018.tif", width = 24, height = 16, device='tiff', dpi=600,unit="in")



# Supplementary Figures S16 and S17: CV of SAVI, GNDVI, CIg vs. SQRT area (grouped by vegetation type)  --------
# SAVI --------------------------------------------------------------------
### 2018 (pre-growing season) ###
# Change the level order
GR_savi_2018$new_GRtype <- factor(GR_savi_2018$new_GRtype, levels = c("grass", "sedum_mat", "woody_plants","mix_grass_tree"))

# plot
CV_savi_area_2018_vegetype.plot <- ggplot(GR_savi_2018, aes(x=sqrt_area,y=COV, color = new_GRtype, linetype = new_GRtype))+
  geom_point(aes(shape = new_GRtype), size=2.5, alpha = 0.2) +
  geom_smooth(aes(color = new_GRtype, fill=new_GRtype, linetype = new_GRtype),
              method='lm', formula= y~x, alpha = 0.2, cex=1.5)+
  labs(x=expression(sqrt("Green roof area")~(m)), y="CV of SAVI",
       group = "new_GRtype", linetype = "new_GRtype", pch = "new_GRtype")+
  scale_color_manual(name="Vegetation type",labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"),values = c("#00BFFF", "#FF762E", "#7CAE00","#cd6ee8"))+
  scale_fill_manual(name="Vegetation type",labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"),values = c("#00BFFF", "#FF762E", "#7CAE00","#cd6ee8"))+
  scale_shape_manual(name="Vegetation type", labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"), values = c(19,17, 15, 18))+
  scale_linetype_manual(name="Vegetation type", labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"), values = c(1,1,1,2))+
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
  )+
  guides(
    color = guide_legend(nrow = 2, byrow = TRUE),
    linetype = guide_legend(nrow = 2, byrow = TRUE)
  )

CV_savi_area_2018_vegetype.plot

# Linear regression by vegetation type
summary(lm(COV ~ sqrt_area, data = subset(GR_savi_2018, new_GRtype =="grass")))
summary(lm(COV ~ sqrt_area, data = subset(GR_savi_2018, new_GRtype =="sedum_mat")))
summary(lm(COV ~ sqrt_area, data = subset(GR_savi_2018, new_GRtype =="woody_plants")))
summary(lm(COV ~ sqrt_area, data = subset(GR_savi_2018, new_GRtype =="mix_grass_tree")))

### 2017 (growing season) ###
# Change the level order
GR_savi_2017$new_GRtype <- factor(GR_savi_2017$new_GRtype, levels = c("grass", "sedum_mat", "woody_plants","mix_grass_tree"))

# plot
CV_savi_area_2017_vegetype.plot <- ggplot(GR_savi_2017, aes(x=sqrt_area,y=COV, color = new_GRtype, linetype = new_GRtype))+
  geom_point(aes(shape = new_GRtype), size=2.5, alpha = 0.2) +
  geom_smooth(aes(color = new_GRtype, fill=new_GRtype, linetype = new_GRtype),
              method='lm', formula= y~x, alpha = 0.2, cex=1.5)+
  labs(x=expression(sqrt("Green roof area")~(m)), y="CV of SAVI",
       group = "new_GRtype", linetype = "new_GRtype", pch = "new_GRtype")+
  scale_color_manual(name="Vegetation type",labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"),values = c("#00BFFF", "#FF762E", "#7CAE00","#cd6ee8"))+
  scale_fill_manual(name="Vegetation type",labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"),values = c("#00BFFF", "#FF762E", "#7CAE00","#cd6ee8"))+
  scale_shape_manual(name="Vegetation type", labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"), values = c(19,17, 15, 18))+
  scale_linetype_manual(name="Vegetation type", labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"), values = c(1,2,1,2))+
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
  )+
  guides(
    color = guide_legend(nrow = 2, byrow = TRUE),
    linetype = guide_legend(nrow = 2, byrow = TRUE)
  )

CV_savi_area_2017_vegetype.plot

# Linear regression by vegetation type
summary(lm(COV ~ sqrt_area, data = subset(GR_savi_2017, new_GRtype =="grass")))
summary(lm(COV ~ sqrt_area, data = subset(GR_savi_2017, new_GRtype =="sedum_mat")))
summary(lm(COV ~ sqrt_area, data = subset(GR_savi_2017, new_GRtype =="woody_plants")))
summary(lm(COV ~ sqrt_area, data = subset(GR_savi_2017, new_GRtype =="mix_grass_tree")))

# GNDVI --------------------------------------------------------------------
### 2018 (pre-growing season) ###
# Change the level order
GR_gndvi_2018$new_GRtype <- factor(GR_gndvi_2018$new_GRtype, levels = c("grass", "sedum_mat", "woody_plants","mix_grass_tree"))

# plot
CV_gndvi_area_2018_vegetype.plot <- ggplot(GR_gndvi_2018, aes(x=sqrt_area,y=COV, color = new_GRtype, linetype = new_GRtype))+
  geom_point(aes(shape = new_GRtype), size=2.5, alpha = 0.2) +
  geom_smooth(aes(color = new_GRtype, fill=new_GRtype, linetype = new_GRtype),
              method='lm', formula= y~x, alpha = 0.2, cex=1.5)+
  labs(x=expression(sqrt("Green roof area")~(m)), y="CV of GNDVI",
       group = "new_GRtype", linetype = "new_GRtype", pch = "new_GRtype")+
  scale_color_manual(name="Vegetation type",labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"),values = c("#00BFFF", "#FF762E", "#7CAE00","#cd6ee8"))+
  scale_fill_manual(name="Vegetation type",labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"),values = c("#00BFFF", "#FF762E", "#7CAE00","#cd6ee8"))+
  scale_shape_manual(name="Vegetation type", labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"), values = c(19,17, 15, 18))+
  scale_linetype_manual(name="Vegetation type", labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"), values = c(1,2,1,2))+
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
  )+
  guides(
    color = guide_legend(nrow = 2, byrow = TRUE),
    linetype = guide_legend(nrow = 2, byrow = TRUE)
  )

CV_gndvi_area_2018_vegetype.plot

# Linear regression by vegetation type
summary(lm(COV ~ sqrt_area, data = subset(GR_gndvi_2018, new_GRtype =="grass")))
summary(lm(COV ~ sqrt_area, data = subset(GR_gndvi_2018, new_GRtype =="sedum_mat")))
summary(lm(COV ~ sqrt_area, data = subset(GR_gndvi_2018, new_GRtype =="woody_plants")))
summary(lm(COV ~ sqrt_area, data = subset(GR_gndvi_2018, new_GRtype =="mix_grass_tree")))

### 2017 (growing season) ###
# Change the level order
GR_gndvi_2017$new_GRtype <- factor(GR_gndvi_2017$new_GRtype, levels = c("grass", "sedum_mat", "woody_plants","mix_grass_tree"))

# plot
CV_gndvi_area_2017_vegetype.plot <- ggplot(GR_gndvi_2017, aes(x=sqrt_area,y=COV, color = new_GRtype, linetype = new_GRtype))+
  geom_point(aes(shape = new_GRtype), size=2.5, alpha = 0.2) +
  geom_smooth(aes(color = new_GRtype, fill=new_GRtype, linetype = new_GRtype),
              method='lm', formula= y~x, alpha = 0.2, cex=1.5)+
  labs(x=expression(sqrt("Green roof area")~(m)), y="CV of GNDVI",
       group = "new_GRtype", linetype = "new_GRtype", pch = "new_GRtype")+
  scale_color_manual(name="Vegetation type",labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"),values = c("#00BFFF", "#FF762E", "#7CAE00","#cd6ee8"))+
  scale_fill_manual(name="Vegetation type",labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"),values = c("#00BFFF", "#FF762E", "#7CAE00","#cd6ee8"))+
  scale_shape_manual(name="Vegetation type", labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"), values = c(19,17, 15, 18))+
  scale_linetype_manual(name="Vegetation type", labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"), values = c(1,1,1,2))+
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
  )+
  guides(
    color = guide_legend(nrow = 2, byrow = TRUE),
    linetype = guide_legend(nrow = 2, byrow = TRUE)
  )

CV_gndvi_area_2017_vegetype.plot

# Linear regression by vegetation type
summary(lm(COV ~ sqrt_area, data = subset(GR_gndvi_2017, new_GRtype =="grass")))
summary(lm(COV ~ sqrt_area, data = subset(GR_gndvi_2017, new_GRtype =="sedum_mat")))
summary(lm(COV ~ sqrt_area, data = subset(GR_gndvi_2017, new_GRtype =="woody_plants")))
summary(lm(COV ~ sqrt_area, data = subset(GR_gndvi_2017, new_GRtype =="mix_grass_tree")))

# CIg --------------------------------------------------------------------
### 2018 (pre-growing season) ###
# Change the level order
GR_cig_2018$new_GRtype <- factor(GR_cig_2018$new_GRtype, levels = c("grass", "sedum_mat", "woody_plants","mix_grass_tree"))

# plot
CV_cig_area_2018_vegetype.plot <- ggplot(GR_cig_2018, aes(x=sqrt_area,y=COV, color = new_GRtype, linetype = new_GRtype))+
  geom_point(aes(shape = new_GRtype), size=2.5, alpha = 0.2) +
  geom_smooth(aes(color = new_GRtype, fill=new_GRtype, linetype = new_GRtype),
              method='lm', formula= y~x, alpha = 0.2, cex=1.5)+
  labs(x=expression(sqrt("Green roof area")~(m)), y="CV of CIg",
       group = "new_GRtype", linetype = "new_GRtype", pch = "new_GRtype")+
  scale_color_manual(name="Vegetation type",labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"),values = c("#00BFFF", "#FF762E", "#7CAE00","#cd6ee8"))+
  scale_fill_manual(name="Vegetation type",labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"),values = c("#00BFFF", "#FF762E", "#7CAE00","#cd6ee8"))+
  scale_shape_manual(name="Vegetation type", labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"), values = c(19,17, 15, 18))+
  scale_linetype_manual(name="Vegetation type", labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"), values = c(1,1,1,2))+
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
  )+
  guides(
    color = guide_legend(nrow = 2, byrow = TRUE),
    linetype = guide_legend(nrow = 2, byrow = TRUE)
  )

CV_cig_area_2018_vegetype.plot

# Linear regression by vegetation type
summary(lm(COV ~ sqrt_area, data = subset(GR_cig_2018, new_GRtype =="grass")))
summary(lm(COV ~ sqrt_area, data = subset(GR_cig_2018, new_GRtype =="sedum_mat")))
summary(lm(COV ~ sqrt_area, data = subset(GR_cig_2018, new_GRtype =="woody_plants")))
summary(lm(COV ~ sqrt_area, data = subset(GR_cig_2018, new_GRtype =="mix_grass_tree")))

### 2017 (growing season) ###
# Change the level order
GR_cig_2017$new_GRtype <- factor(GR_cig_2017$new_GRtype, levels = c("grass", "sedum_mat", "woody_plants","mix_grass_tree"))

# plot
CV_cig_area_2017_vegetype.plot <- ggplot(GR_cig_2017, aes(x=sqrt_area,y=COV, color = new_GRtype, linetype = new_GRtype))+
  geom_point(aes(shape = new_GRtype), size=2.5, alpha = 0.2) +
  geom_smooth(aes(color = new_GRtype, fill=new_GRtype, linetype = new_GRtype),
              method='lm', formula= y~x, alpha = 0.2, cex=1.5)+
  labs(x=expression(sqrt("Green roof area")~(m)), y="CV of CIg",
       group = "new_GRtype", linetype = "new_GRtype", pch = "new_GRtype")+
  scale_color_manual(name="Vegetation type",labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"),values = c("#00BFFF", "#FF762E", "#7CAE00","#cd6ee8"))+
  scale_fill_manual(name="Vegetation type",labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"),values = c("#00BFFF", "#FF762E", "#7CAE00","#cd6ee8"))+
  scale_shape_manual(name="Vegetation type", labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"), values = c(19,17, 15, 18))+
  scale_linetype_manual(name="Vegetation type", labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"), values = c(1,2,1,2))+
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
  )+
  guides(
    color = guide_legend(nrow = 2, byrow = TRUE),
    linetype = guide_legend(nrow = 2, byrow = TRUE)
  )

CV_cig_area_2017_vegetype.plot

# Linear regression by vegetation type
summary(lm(COV ~ sqrt_area, data = subset(GR_cig_2017, new_GRtype =="grass")))
summary(lm(COV ~ sqrt_area, data = subset(GR_cig_2017, new_GRtype =="sedum_mat")))
summary(lm(COV ~ sqrt_area, data = subset(GR_cig_2017, new_GRtype =="woody_plants")))
summary(lm(COV ~ sqrt_area, data = subset(GR_cig_2017, new_GRtype =="mix_grass_tree")))

# Supplementary Figures S16 and S17: CV of SAVI, GNDVI, CIg vs. Building height (grouped by vegetation type)  --------
# SAVI --------------------------------------------------------------------
### 2018 (pre-growing season) ###
# Change the level order
GR_savi_2018$GR_type <- factor(GR_savi_2018$GR_type, levels = c("ext","int"))

# plot
CV_savi_height_2018_vegetype.plot <- ggplot(GR_savi_2018, aes(x=MAX_HEIGHT,y=COV, color = new_GRtype, linetype = new_GRtype))+
  geom_point(aes(shape = new_GRtype), size=2.5, alpha = 0.2) +
  geom_smooth(aes(color = new_GRtype, fill=new_GRtype, linetype = new_GRtype),
              method='lm', formula= y~x, alpha = 0.2, cex=1.5)+
  labs(x="Building height (m)", y="CV of SAVI",
       group = "new_GRtype", linetype = "new_GRtype", pch = "new_GRtype")+
  scale_color_manual(name="Vegetation type",labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"),values = c("#00BFFF", "#FF762E", "#7CAE00","#cd6ee8"))+
  scale_fill_manual(name="Vegetation type",labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"),values = c("#00BFFF", "#FF762E", "#7CAE00","#cd6ee8"))+
  scale_shape_manual(name="Vegetation type", labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"), values = c(19,17, 15, 18))+
  scale_linetype_manual(name="Vegetation type", labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"), values = c(1,1,2,2))+
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
  )+
  guides(
    color = guide_legend(nrow = 2, byrow = TRUE),
    linetype = guide_legend(nrow = 2, byrow = TRUE)
  )

CV_savi_height_2018_vegetype.plot

# Linear regression by vegetation type
summary(lm(COV ~ MAX_HEIGHT, data = subset(GR_savi_2018, new_GRtype =="grass")))
summary(lm(COV ~ MAX_HEIGHT, data = subset(GR_savi_2018, new_GRtype =="sedum_mat")))
summary(lm(COV ~ MAX_HEIGHT, data = subset(GR_savi_2018, new_GRtype =="woody_plants")))
summary(lm(COV ~ MAX_HEIGHT, data = subset(GR_savi_2018, new_GRtype =="mix_grass_tree")))

### 2017 (growing season) ###
# Change the level order
GR_savi_2017$GR_type <- factor(GR_savi_2017$GR_type, levels = c("ext","int"))

# plot
CV_savi_height_2017_vegetype.plot <- ggplot(GR_savi_2017, aes(x=MAX_HEIGHT,y=COV, color = new_GRtype, linetype = new_GRtype))+
  geom_point(aes(shape = new_GRtype), size=2.5, alpha = 0.2) +
  geom_smooth(aes(color = new_GRtype, fill=new_GRtype, linetype = new_GRtype),
              method='lm', formula= y~x, alpha = 0.2, cex=1.5)+
  labs(x="Building height (m)", y="CV of SAVI",
       group = "new_GRtype", linetype = "new_GRtype", pch = "new_GRtype")+
  scale_color_manual(name="Vegetation type",labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"),values = c("#00BFFF", "#FF762E", "#7CAE00","#cd6ee8"))+
  scale_fill_manual(name="Vegetation type",labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"),values = c("#00BFFF", "#FF762E", "#7CAE00","#cd6ee8"))+
  scale_shape_manual(name="Vegetation type", labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"), values = c(19,17, 15, 18))+
  scale_linetype_manual(name="Vegetation type", labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"), values = c(2,2,2,2))+
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
  )+
  guides(
    color = guide_legend(nrow = 2, byrow = TRUE),
    linetype = guide_legend(nrow = 2, byrow = TRUE)
  )

CV_savi_height_2017_vegetype.plot

# Linear regression by vegetation type
summary(lm(COV ~ MAX_HEIGHT, data = subset(GR_savi_2017, new_GRtype =="grass")))
summary(lm(COV ~ MAX_HEIGHT, data = subset(GR_savi_2017, new_GRtype =="sedum_mat")))
summary(lm(COV ~ MAX_HEIGHT, data = subset(GR_savi_2017, new_GRtype =="woody_plants")))
summary(lm(COV ~ MAX_HEIGHT, data = subset(GR_savi_2017, new_GRtype =="mix_grass_tree")))


# GNDVI --------------------------------------------------------------------
### 2018 (pre-growing season) ###
# Change the level order
GR_gndvi_2018$GR_type <- factor(GR_gndvi_2018$GR_type, levels = c("ext","int"))

# plot
CV_gndvi_height_2018_vegetype.plot <- ggplot(GR_gndvi_2018, aes(x=MAX_HEIGHT,y=COV, color = new_GRtype, linetype = new_GRtype))+
  geom_point(aes(shape = new_GRtype), size=2.5, alpha = 0.2) +
  geom_smooth(aes(color = new_GRtype, fill=new_GRtype, linetype = new_GRtype),
              method='lm', formula= y~x, alpha = 0.2, cex=1.5)+
  labs(x="Building height (m)", y="CV_ GNDVI",
       group = "new_GRtype", linetype = "new_GRtype", pch = "new_GRtype")+
  scale_color_manual(name="Vegetation type",labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"),values = c("#00BFFF", "#FF762E", "#7CAE00","#cd6ee8"))+
  scale_fill_manual(name="Vegetation type",labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"),values = c("#00BFFF", "#FF762E", "#7CAE00","#cd6ee8"))+
  scale_shape_manual(name="Vegetation type", labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"), values = c(19,17, 15, 18))+
  scale_linetype_manual(name="Vegetation type", labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"), values = c(2,1,2,2))+
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
  )+
  guides(
    color = guide_legend(nrow = 2, byrow = TRUE),
    linetype = guide_legend(nrow = 2, byrow = TRUE)
  )

CV_gndvi_height_2018_vegetype.plot

# Linear regression by vegetation type
summary(lm(COV ~ MAX_HEIGHT, data = subset(GR_gndvi_2018, new_GRtype =="grass")))
summary(lm(COV ~ MAX_HEIGHT, data = subset(GR_gndvi_2018, new_GRtype =="sedum_mat")))
summary(lm(COV ~ MAX_HEIGHT, data = subset(GR_gndvi_2018, new_GRtype =="woody_plants")))
summary(lm(COV ~ MAX_HEIGHT, data = subset(GR_gndvi_2018, new_GRtype =="mix_grass_tree")))

### 2017 (growing season) ###
# Change the level order
GR_gndvi_2017$GR_type <- factor(GR_gndvi_2017$GR_type, levels = c("ext","int"))

# plot
CV_gndvi_height_2017_vegetype.plot <- ggplot(GR_gndvi_2017, aes(x=MAX_HEIGHT,y=COV, color = new_GRtype, linetype = new_GRtype))+
  geom_point(aes(shape = new_GRtype), size=2.5, alpha = 0.2) +
  geom_smooth(aes(color = new_GRtype, fill=new_GRtype, linetype = new_GRtype),
              method='lm', formula= y~x, alpha = 0.2, cex=1.5)+
  labs(x="Building height (m)", y="CV of GNDVI",
       group = "new_GRtype", linetype = "new_GRtype", pch = "new_GRtype")+
  scale_color_manual(name="Vegetation type",labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"),values = c("#00BFFF", "#FF762E", "#7CAE00","#cd6ee8"))+
  scale_fill_manual(name="Vegetation type",labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"),values = c("#00BFFF", "#FF762E", "#7CAE00","#cd6ee8"))+
  scale_shape_manual(name="Vegetation type", labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"), values = c(19,17, 15, 18))+
  scale_linetype_manual(name="Vegetation type", labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"), values = c(2,2,2,2))+
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
  )+
  guides(
    color = guide_legend(nrow = 2, byrow = TRUE),
    linetype = guide_legend(nrow = 2, byrow = TRUE)
  )

CV_gndvi_height_2017_vegetype.plot

# Linear regression by vegetation type
summary(lm(COV ~ MAX_HEIGHT, data = subset(GR_gndvi_2017, new_GRtype =="grass")))
summary(lm(COV ~ MAX_HEIGHT, data = subset(GR_gndvi_2017, new_GRtype =="sedum_mat")))
summary(lm(COV ~ MAX_HEIGHT, data = subset(GR_gndvi_2017, new_GRtype =="woody_plants")))
summary(lm(COV ~ MAX_HEIGHT, data = subset(GR_gndvi_2017, new_GRtype =="mix_grass_tree")))

# CIg --------------------------------------------------------------------
### 2018 (pre-growing season) ###
# Change the level order
GR_cig_2018$GR_type <- factor(GR_cig_2018$GR_type, levels = c("ext","int"))

# plot
CV_cig_height_2018_vegetype.plot <- ggplot(GR_cig_2018, aes(x=MAX_HEIGHT,y=COV, color = new_GRtype, linetype = new_GRtype))+
  geom_point(aes(shape = new_GRtype), size=2.5, alpha = 0.2) +
  geom_smooth(aes(color = new_GRtype, fill=new_GRtype, linetype = new_GRtype),
              method='lm', formula= y~x, alpha = 0.2, cex=1.5)+
  labs(x="Building height (m)", y="CV of CIg",
       group = "new_GRtype", linetype = "new_GRtype", pch = "new_GRtype")+
  scale_color_manual(name="Vegetation type",labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"),values = c("#00BFFF", "#FF762E", "#7CAE00","#cd6ee8"))+
  scale_fill_manual(name="Vegetation type",labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"),values = c("#00BFFF", "#FF762E", "#7CAE00","#cd6ee8"))+
  scale_shape_manual(name="Vegetation type", labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"), values = c(19,17, 15, 18))+
  scale_linetype_manual(name="Vegetation type", labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"), values = c(2,2,2,2))+
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
  )+
  guides(
    color = guide_legend(nrow = 2, byrow = TRUE),
    linetype = guide_legend(nrow = 2, byrow = TRUE)
  )

CV_cig_height_2018_vegetype.plot

# Linear regression by vegetation type
summary(lm(COV ~ MAX_HEIGHT, data = subset(GR_cig_2018, new_GRtype =="grass")))
summary(lm(COV ~ MAX_HEIGHT, data = subset(GR_cig_2018, new_GRtype =="sedum_mat")))
summary(lm(COV ~ MAX_HEIGHT, data = subset(GR_cig_2018, new_GRtype =="woody_plants")))
summary(lm(COV ~ MAX_HEIGHT, data = subset(GR_cig_2018, new_GRtype =="mix_grass_tree")))

### 2017 (growing season) ###
# Change the level order
GR_cig_2017$GR_type <- factor(GR_cig_2017$GR_type, levels = c("ext","int"))

# plot
CV_cig_height_2017_vegetype.plot <- ggplot(GR_cig_2017, aes(x=MAX_HEIGHT,y=COV, color = new_GRtype, linetype = new_GRtype))+
  geom_point(aes(shape = new_GRtype), size=2.5, alpha = 0.2) +
  geom_smooth(aes(color = new_GRtype, fill=new_GRtype, linetype = new_GRtype),
              method='lm', formula= y~x, alpha = 0.2, cex=1.5)+
  labs(x="Building height (m)", y="CV of CIg",
       group = "new_GRtype", linetype = "new_GRtype", pch = "new_GRtype")+
  scale_color_manual(name="Vegetation type",labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"),values = c("#00BFFF", "#FF762E", "#7CAE00","#cd6ee8"))+
  scale_fill_manual(name="Vegetation type",labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"),values = c("#00BFFF", "#FF762E", "#7CAE00","#cd6ee8"))+
  scale_shape_manual(name="Vegetation type", labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"), values = c(19,17, 15, 18))+
  scale_linetype_manual(name="Vegetation type", labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"), values = c(2,2,2,2))+
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
  )+
  guides(
    color = guide_legend(nrow = 2, byrow = TRUE),
    linetype = guide_legend(nrow = 2, byrow = TRUE)
  )

CV_cig_height_2017_vegetype.plot

# Linear regression by vegetation type
summary(lm(COV ~ MAX_HEIGHT, data = subset(GR_cig_2017, new_GRtype =="grass")))
summary(lm(COV ~ MAX_HEIGHT, data = subset(GR_cig_2017, new_GRtype =="sedum_mat")))
summary(lm(COV ~ MAX_HEIGHT, data = subset(GR_cig_2017, new_GRtype =="woody_plants")))
summary(lm(COV ~ MAX_HEIGHT, data = subset(GR_cig_2017, new_GRtype =="mix_grass_tree")))

# Supplementary Figures S16 and S17: CV of SAVI, GNDVI, CIg vs. vegetation type  --------
# SAVI --------------------------------------------------------------------
### 2018 (pre-growing season) ###
# Summarize the data
CV_summary_vegetype_savi_2018 <-
  do.call(data.frame,
          aggregate(COV ~ new_GRtype,
                    data = GR_savi_2018,
                    FUN = function(x) c(mean_value = mean(x),se_value = get_se(x))))

# Change the level order
CV_summary_vegetype_savi_2018$new_GRtype <- factor(CV_summary_vegetype_savi_2018$new_GRtype, levels = c("grass","sedum_mat","woody_plants","mix_grass_tree"))

# plot
CV_savi_vegetype_barplot_2018 <- ggplot(CV_summary_vegetype_savi_2018, aes(x = new_GRtype, y = COV.mean_value, fill = new_GRtype)) +
  geom_bar(stat="identity", colour = "black", size = 0.3) +
  scale_fill_manual(name="Vegetation type",labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"),values = c("#00BFFF", "#FF762E", "#7CAE00","#cd6ee8"))+
  scale_x_discrete(name="Vegetation type",labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"))+
  geom_errorbar(aes(ymin = COV.mean_value - COV.se_value,
                    ymax = COV.mean_value + COV.se_value),
                width = 0.3, linetype = 1)+
  labs(x = "Green roof type", y = "CV of SAVI",
       group = "GR_type")+
  coord_cartesian(ylim=c(0,0.06))+
  theme_bw(base_size = 28) + 
  theme(
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text=element_text(size=22, color = "black"),
    axis.title=element_text(size=32, color = "black"),
    axis.text.x = element_text(angle = 15,hjust=1),
    legend.title = element_blank(),
    legend.position = "none"
  )

CV_savi_vegetype_barplot_2018

# ANOVA
summary(aov(COV~new_GRtype, data = GR_savi_2018))
print(HSD.test(aov(COV~new_GRtype, data = GR_savi_2018), "new_GRtype"))

### 2017 (growing season) ###
# Summarize the data
CV_summary_vegetype_savi_2017 <-
  do.call(data.frame,
          aggregate(COV ~ new_GRtype,
                    data = GR_savi_2017,
                    FUN = function(x) c(mean_value = mean(x),se_value = get_se(x))))

# Change the level order
CV_summary_vegetype_savi_2017$new_GRtype <- factor(CV_summary_vegetype_savi_2017$new_GRtype, levels = c("grass","sedum_mat","woody_plants","mix_grass_tree"))

# plot
CV_savi_vegetype_barplot_2017 <- ggplot(CV_summary_vegetype_savi_2017, aes(x = new_GRtype, y = COV.mean_value, fill = new_GRtype)) +
  geom_bar(stat="identity", colour = "black", size = 0.3) +
  scale_fill_manual(name="Vegetation type",labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"),values = c("#00BFFF", "#FF762E", "#7CAE00","#cd6ee8"))+
  scale_x_discrete(name="Vegetation type",labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"))+
  geom_errorbar(aes(ymin = COV.mean_value - COV.se_value,
                    ymax = COV.mean_value + COV.se_value),
                width = 0.3, linetype = 1)+
  labs(x = "Green roof type", y = "CV of SAVI",
       group = "GR_type")+
  coord_cartesian(ylim=c(0,0.07))+
  theme_bw(base_size = 28) + 
  theme(
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text=element_text(size=22, color = "black"),
    axis.title=element_text(size=32, color = "black"),
    axis.text.x = element_text(angle = 15,hjust=1),
    legend.title = element_blank(),
    legend.position = "none"
  )

CV_savi_vegetype_barplot_2017

# ANOVA
summary(aov(COV~new_GRtype, data = GR_savi_2017))
print(HSD.test(aov(COV~new_GRtype, data = GR_savi_2017), "new_GRtype"))


# GNDVI --------------------------------------------------------------------
### 2018 (pre-growing season) ###
# Summarize the data
CV_summary_vegetype_gndvi_2018 <-
  do.call(data.frame,
          aggregate(COV ~ new_GRtype,
                    data = GR_gndvi_2018,
                    FUN = function(x) c(mean_value = mean(x),se_value = get_se(x))))

# Change the level order
CV_summary_vegetype_gndvi_2018$new_GRtype <- factor(CV_summary_vegetype_gndvi_2018$new_GRtype, levels = c("grass","sedum_mat","woody_plants","mix_grass_tree"))

# plot
CV_gndvi_vegetype_barplot_2018 <- ggplot(CV_summary_vegetype_gndvi_2018, aes(x = new_GRtype, y = COV.mean_value, fill = new_GRtype)) +
  geom_bar(stat="identity", colour = "black", size = 0.3) +
  scale_fill_manual(name="Vegetation type",labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"),values = c("#00BFFF", "#FF762E", "#7CAE00","#cd6ee8"))+
  scale_x_discrete(name="Vegetation type",labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"))+
  geom_errorbar(aes(ymin = COV.mean_value - COV.se_value,
                    ymax = COV.mean_value + COV.se_value),
                width = 0.3, linetype = 1)+
  labs(x = "Green roof type", y = "CV of GNDVI",
       group = "GR_type")+
  coord_cartesian(ylim=c(0,0.07))+
  theme_bw(base_size = 28) + 
  theme(
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text=element_text(size=22, color = "black"),
    axis.title=element_text(size=32, color = "black"),
    axis.text.x = element_text(angle = 15,hjust=1),
    legend.title = element_blank(),
    legend.position = "none"
  )

CV_gndvi_vegetype_barplot_2018

# ANOVA
summary(aov(COV~new_GRtype, data = GR_gndvi_2018))
print(HSD.test(aov(COV~new_GRtype, data = GR_gndvi_2018), "new_GRtype"))

### 2017 (growing season) ###
# Summarize the data
CV_summary_vegetype_gndvi_2017 <-
  do.call(data.frame,
          aggregate(COV ~ new_GRtype,
                    data = GR_gndvi_2017,
                    FUN = function(x) c(mean_value = mean(x),se_value = get_se(x))))

# Change the level order
CV_summary_vegetype_gndvi_2017$new_GRtype <- factor(CV_summary_vegetype_gndvi_2017$new_GRtype, levels = c("grass","sedum_mat","woody_plants","mix_grass_tree"))

# plot
CV_gndvi_vegetype_barplot_2017 <- ggplot(CV_summary_vegetype_gndvi_2017, aes(x = new_GRtype, y = COV.mean_value, fill = new_GRtype)) +
  geom_bar(stat="identity", colour = "black", size = 0.3) +
  scale_fill_manual(name="Vegetation type",labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"),values = c("#00BFFF", "#FF762E", "#7CAE00","#cd6ee8"))+
  scale_x_discrete(name="Vegetation type",labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"))+
  geom_errorbar(aes(ymin = COV.mean_value - COV.se_value,
                    ymax = COV.mean_value + COV.se_value),
                width = 0.3, linetype = 1)+
  labs(x = "Green roof type", y = "CV of GNDVI",
       group = "GR_type")+
  coord_cartesian(ylim=c(0,0.07))+
  theme_bw(base_size = 28) + 
  theme(
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text=element_text(size=22, color = "black"),
    axis.title=element_text(size=32, color = "black"),
    axis.text.x = element_text(angle = 15,hjust=1),
    legend.title = element_blank(),
    legend.position = "none"
  )

CV_gndvi_vegetype_barplot_2017

# ANOVA
summary(aov(COV~new_GRtype, data = GR_gndvi_2017))
print(HSD.test(aov(COV~new_GRtype, data = GR_gndvi_2017), "new_GRtype"))


# CIg --------------------------------------------------------------------
### 2018 (pre-growing season) ###
# Summarize the data
CV_summary_vegetype_cig_2018 <-
  do.call(data.frame,
          aggregate(COV ~ new_GRtype,
                    data = GR_cig_2018,
                    FUN = function(x) c(mean_value = mean(x),se_value = get_se(x))))

# Change the level order
CV_summary_vegetype_cig_2018$new_GRtype <- factor(CV_summary_vegetype_cig_2018$new_GRtype, levels = c("grass","sedum_mat","woody_plants","mix_grass_tree"))

# plot
CV_cig_vegetype_barplot_2018 <- ggplot(CV_summary_vegetype_cig_2018, aes(x = new_GRtype, y = COV.mean_value, fill = new_GRtype)) +
  geom_bar(stat="identity", colour = "black", size = 0.3) +
  scale_fill_manual(name="Vegetation type",labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"),values = c("#00BFFF", "#FF762E", "#7CAE00","#cd6ee8"))+
  scale_x_discrete(name="Vegetation type",labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"))+
  geom_errorbar(aes(ymin = COV.mean_value - COV.se_value,
                    ymax = COV.mean_value + COV.se_value),
                width = 0.3, linetype = 1)+
  labs(x = "Green roof type", y = "CV of CIg",
       group = "GR_type")+
  coord_cartesian(ylim=c(0,0.18))+
  theme_bw(base_size = 28) + 
  theme(
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text=element_text(size=22, color = "black"),
    axis.title=element_text(size=32, color = "black"),
    axis.text.x = element_text(angle = 15,hjust=1),
    legend.title = element_blank(),
    legend.position = "none"
  )

CV_cig_vegetype_barplot_2018

# ANOVA
summary(aov(COV~new_GRtype, data = GR_cig_2018))
print(HSD.test(aov(COV~new_GRtype, data = GR_cig_2018), "new_GRtype"))

### 2017 (growing season) ###
# Summarize the data
CV_summary_vegetype_cig_2017 <-
  do.call(data.frame,
          aggregate(COV ~ new_GRtype,
                    data = GR_cig_2017,
                    FUN = function(x) c(mean_value = mean(x),se_value = get_se(x))))

# Change the level order
CV_summary_vegetype_cig_2017$new_GRtype <- factor(CV_summary_vegetype_cig_2017$new_GRtype, levels = c("grass","sedum_mat","woody_plants","mix_grass_tree"))

# plot
CV_cig_vegetype_barplot_2017 <- ggplot(CV_summary_vegetype_cig_2017, aes(x = new_GRtype, y = COV.mean_value, fill = new_GRtype)) +
  geom_bar(stat="identity", colour = "black", size = 0.3) +
  scale_fill_manual(name="Vegetation type",labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"),values = c("#00BFFF", "#FF762E", "#7CAE00","#cd6ee8"))+
  scale_x_discrete(name="Vegetation type",labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"))+
  geom_errorbar(aes(ymin = COV.mean_value - COV.se_value,
                    ymax = COV.mean_value + COV.se_value),
                width = 0.3, linetype = 1)+
  labs(x = "Green roof type", y = "CV of CIg",
       group = "GR_type")+
  coord_cartesian(ylim=c(0,0.2))+
  theme_bw(base_size = 28) + 
  theme(
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text=element_text(size=22, color = "black"),
    axis.title=element_text(size=32, color = "black"),
    axis.text.x = element_text(angle = 15,hjust=1),
    legend.title = element_blank(),
    legend.position = "none"
  )

CV_cig_vegetype_barplot_2017

# ANOVA
summary(aov(COV~new_GRtype, data = GR_cig_2017))
print(HSD.test(aov(COV~new_GRtype, data = GR_cig_2017), "new_GRtype"))


# Supplementary Figures S16 and S17: CV of SAVI, GNDVI, CIg vs. SQRT Aspect ratio (grouped by vegetation type)  --------
# SAVI --------------------------------------------------------------------
### 2018 (pre-growing season) ###
# change order of display
GR_savi_2018$new_GRtype <- factor(GR_savi_2018$new_GRtype, levels = c("grass", "sedum_mat", "woody_plants","mix_grass_tree"))

# plot
CV_savi_sqrt_AR_vegetype_2018.plot <- ggplot(GR_savi_2018, aes(x=sqrt_AR,y=COV, color = new_GRtype, linetype = new_GRtype))+
  geom_point(aes(shape = new_GRtype), size=2.5, alpha = 0.2) +
  geom_smooth(aes(color = new_GRtype, fill=new_GRtype, linetype = new_GRtype),
              method='lm', formula= y~x, alpha = 0.2, cex=1.5)+
  labs(x=expression(sqrt("Aspect ratio")), y="CV of SAVI",
       group = "new_GRtype", linetype = "new_GRtype", pch = "new_GRtype")+
  scale_color_manual(name="Vegetation type",labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"),values = c("#00BFFF", "#FF762E", "#7CAE00","#cd6ee8"))+
  scale_fill_manual(name="Vegetation type",labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"),values = c("#00BFFF", "#FF762E", "#7CAE00","#cd6ee8"))+
  scale_linetype_manual(name="Vegetation type", labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"), values = c(1,1, 1, 2))+
  scale_shape_manual(name="Vegetation type", labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"), values = c(19,17, 15, 18)) +
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
  )+
  guides(
    color = guide_legend(nrow = 2, byrow = TRUE),
    linetype = guide_legend(nrow = 2, byrow = TRUE)
  )

CV_savi_sqrt_AR_vegetype_2018.plot

# Linear regression by vegetation type
summary(lm(COV ~ sqrt_AR, data = subset(GR_savi_2018, new_GRtype =="grass")))
summary(lm(COV ~ sqrt_AR, data = subset(GR_savi_2018, new_GRtype =="sedum_mat")))
summary(lm(COV ~ sqrt_AR, data = subset(GR_savi_2018, new_GRtype =="woody_plants")))
summary(lm(COV ~ sqrt_AR, data = subset(GR_savi_2018, new_GRtype =="mix_grass_tree")))

### 2017 (growing season) ###
# change order of display
GR_savi_2017$new_GRtype <- factor(GR_savi_2017$new_GRtype, levels = c("grass", "sedum_mat", "woody_plants","mix_grass_tree"))

# plot
CV_savi_sqrt_AR_vegetype_2017.plot <- ggplot(GR_savi_2017, aes(x=sqrt_AR,y=COV, color = new_GRtype, linetype = new_GRtype))+
  geom_point(aes(shape = new_GRtype), size=2.5, alpha = 0.2) +
  geom_smooth(aes(color = new_GRtype, fill=new_GRtype, linetype = new_GRtype),
              method='lm', formula= y~x, alpha = 0.2, cex=1.5)+
  labs(x=expression(sqrt("Aspect ratio")), y="CV of SAVI",
       group = "new_GRtype", linetype = "new_GRtype", pch = "new_GRtype")+
  scale_color_manual(name="Vegetation type",labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"),values = c("#00BFFF", "#FF762E", "#7CAE00","#cd6ee8"))+
  scale_fill_manual(name="Vegetation type",labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"),values = c("#00BFFF", "#FF762E", "#7CAE00","#cd6ee8"))+
  scale_linetype_manual(name="Vegetation type", labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"), values = c(1,2, 1, 2))+
  scale_shape_manual(name="Vegetation type", labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"), values = c(19,17, 15, 18)) +
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
  )+
  guides(
    color = guide_legend(nrow = 2, byrow = TRUE),
    linetype = guide_legend(nrow = 2, byrow = TRUE)
  )

CV_savi_sqrt_AR_vegetype_2017.plot

# Linear regression by vegetation type
summary(lm(COV ~ sqrt_AR, data = subset(GR_savi_2017, new_GRtype =="grass")))
summary(lm(COV ~ sqrt_AR, data = subset(GR_savi_2017, new_GRtype =="sedum_mat")))
summary(lm(COV ~ sqrt_AR, data = subset(GR_savi_2017, new_GRtype =="woody_plants")))
summary(lm(COV ~ sqrt_AR, data = subset(GR_savi_2017, new_GRtype =="mix_grass_tree")))

# GNDVI --------------------------------------------------------------------
### 2018 (pre-growing season) ###
# change order of display
GR_gndvi_2018$new_GRtype <- factor(GR_gndvi_2018$new_GRtype, levels = c("grass", "sedum_mat", "woody_plants","mix_grass_tree"))

# plot
CV_gndvi_sqrt_AR_vegetype_2018.plot <- ggplot(GR_gndvi_2018, aes(x=sqrt_AR,y=COV, color = new_GRtype, linetype = new_GRtype))+
  geom_point(aes(shape = new_GRtype), size=2.5, alpha = 0.2) +
  geom_smooth(aes(color = new_GRtype, fill=new_GRtype, linetype = new_GRtype),
              method='lm', formula= y~x, alpha = 0.2, cex=1.5)+
  labs(x=expression(sqrt("Aspect ratio")), y="CV of GNDVI",
       group = "new_GRtype", linetype = "new_GRtype", pch = "new_GRtype")+
  scale_color_manual(name="Vegetation type",labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"),values = c("#00BFFF", "#FF762E", "#7CAE00","#cd6ee8"))+
  scale_fill_manual(name="Vegetation type",labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"),values = c("#00BFFF", "#FF762E", "#7CAE00","#cd6ee8"))+
  scale_linetype_manual(name="Vegetation type", labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"), values = c(1,1, 1, 2))+
  scale_shape_manual(name="Vegetation type", labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"), values = c(19,17, 15, 18)) +
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
  )+
  guides(
    color = guide_legend(nrow = 2, byrow = TRUE),
    linetype = guide_legend(nrow = 2, byrow = TRUE)
  )

CV_gndvi_sqrt_AR_vegetype_2018.plot

# Linear regression by vegetation type
summary(lm(COV ~ sqrt_AR, data = subset(GR_gndvi_2018, new_GRtype =="grass")))
summary(lm(COV ~ sqrt_AR, data = subset(GR_gndvi_2018, new_GRtype =="sedum_mat")))
summary(lm(COV ~ sqrt_AR, data = subset(GR_gndvi_2018, new_GRtype =="woody_plants")))
summary(lm(COV ~ sqrt_AR, data = subset(GR_gndvi_2018, new_GRtype =="mix_grass_tree")))

### 2017 (growing season) ###
# change order of display
GR_gndvi_2017$new_GRtype <- factor(GR_gndvi_2017$new_GRtype, levels = c("grass", "sedum_mat", "woody_plants","mix_grass_tree"))

# plot
CV_gndvi_sqrt_AR_vegetype_2017.plot <- ggplot(GR_gndvi_2017, aes(x=sqrt_AR,y=COV, color = new_GRtype, linetype = new_GRtype))+
  geom_point(aes(shape = new_GRtype), size=2.5, alpha = 0.2) +
  geom_smooth(aes(color = new_GRtype, fill=new_GRtype, linetype = new_GRtype),
              method='lm', formula= y~x, alpha = 0.2, cex=1.5)+
  labs(x=expression(sqrt("Aspect ratio")), y="CV of GNDVI",
       group = "new_GRtype", linetype = "new_GRtype", pch = "new_GRtype")+
  scale_color_manual(name="Vegetation type",labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"),values = c("#00BFFF", "#FF762E", "#7CAE00","#cd6ee8"))+
  scale_fill_manual(name="Vegetation type",labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"),values = c("#00BFFF", "#FF762E", "#7CAE00","#cd6ee8"))+
  scale_linetype_manual(name="Vegetation type", labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"), values = c(1,1, 1, 2))+
  scale_shape_manual(name="Vegetation type", labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"), values = c(19,17, 15, 18)) +
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
  )+
  guides(
    color = guide_legend(nrow = 2, byrow = TRUE),
    linetype = guide_legend(nrow = 2, byrow = TRUE)
  )

CV_gndvi_sqrt_AR_vegetype_2017.plot

# Linear regression by vegetation type
summary(lm(COV ~ sqrt_AR, data = subset(GR_gndvi_2017, new_GRtype =="grass")))
summary(lm(COV ~ sqrt_AR, data = subset(GR_gndvi_2017, new_GRtype =="sedum_mat")))
summary(lm(COV ~ sqrt_AR, data = subset(GR_gndvi_2017, new_GRtype =="woody_plants")))
summary(lm(COV ~ sqrt_AR, data = subset(GR_gndvi_2017, new_GRtype =="mix_grass_tree")))


# CIg --------------------------------------------------------------------
### 2018 (pre-growing season) ###
# change order of display
GR_cig_2018$new_GRtype <- factor(GR_cig_2018$new_GRtype, levels = c("grass", "sedum_mat", "woody_plants","mix_grass_tree"))

# plot
CV_cig_sqrt_AR_vegetype_2018.plot <- ggplot(GR_cig_2018, aes(x=sqrt_AR,y=COV, color = new_GRtype, linetype = new_GRtype))+
  geom_point(aes(shape = new_GRtype), size=2.5, alpha = 0.2) +
  geom_smooth(aes(color = new_GRtype, fill=new_GRtype, linetype = new_GRtype),
              method='lm', formula= y~x, alpha = 0.2, cex=1.5)+
  labs(x=expression(sqrt("Aspect ratio")), y="CV of CIg",
       group = "new_GRtype", linetype = "new_GRtype", pch = "new_GRtype")+
  scale_color_manual(name="Vegetation type",labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"),values = c("#00BFFF", "#FF762E", "#7CAE00","#cd6ee8"))+
  scale_fill_manual(name="Vegetation type",labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"),values = c("#00BFFF", "#FF762E", "#7CAE00","#cd6ee8"))+
  scale_linetype_manual(name="Vegetation type", labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"), values = c(1,1, 1, 2))+
  scale_shape_manual(name="Vegetation type", labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"), values = c(19,17, 15, 18)) +
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
  )+
  guides(
    color = guide_legend(nrow = 2, byrow = TRUE),
    linetype = guide_legend(nrow = 2, byrow = TRUE)
  )

CV_cig_sqrt_AR_vegetype_2018.plot

# Linear regression by vegetation type
summary(lm(COV ~ sqrt_AR, data = subset(GR_cig_2018, new_GRtype =="grass")))
summary(lm(COV ~ sqrt_AR, data = subset(GR_cig_2018, new_GRtype =="sedum_mat")))
summary(lm(COV ~ sqrt_AR, data = subset(GR_cig_2018, new_GRtype =="woody_plants")))
summary(lm(COV ~ sqrt_AR, data = subset(GR_cig_2018, new_GRtype =="mix_grass_tree")))

### 2017 (growing season) ###
# change order of display
GR_cig_2017$new_GRtype <- factor(GR_cig_2017$new_GRtype, levels = c("grass", "sedum_mat", "woody_plants","mix_grass_tree"))

# plot
CV_cig_sqrt_AR_vegetype_2017.plot <- ggplot(GR_cig_2017, aes(x=sqrt_AR,y=COV, color = new_GRtype, linetype = new_GRtype))+
  geom_point(aes(shape = new_GRtype), size=2.5, alpha = 0.2) +
  geom_smooth(aes(color = new_GRtype, fill=new_GRtype, linetype = new_GRtype),
              method='lm', formula= y~x, alpha = 0.2, cex=1.5)+
  labs(x=expression(sqrt("Aspect ratio")), y="CV of CIg",
       group = "new_GRtype", linetype = "new_GRtype", pch = "new_GRtype")+
  scale_color_manual(name="Vegetation type",labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"),values = c("#00BFFF", "#FF762E", "#7CAE00","#cd6ee8"))+
  scale_fill_manual(name="Vegetation type",labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"),values = c("#00BFFF", "#FF762E", "#7CAE00","#cd6ee8"))+
  scale_linetype_manual(name="Vegetation type", labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"), values = c(1,1, 1, 2))+
  scale_shape_manual(name="Vegetation type", labels = c("Grass", "Sedum", "Woody plant","Mixed grass and tree"), values = c(19,17, 15, 18)) +
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
  )+
  guides(
    color = guide_legend(nrow = 2, byrow = TRUE),
    linetype = guide_legend(nrow = 2, byrow = TRUE)
  )

CV_cig_sqrt_AR_vegetype_2017.plot

# Linear regression by vegetation type
summary(lm(COV ~ sqrt_AR, data = subset(GR_cig_2017, new_GRtype =="grass")))
summary(lm(COV ~ sqrt_AR, data = subset(GR_cig_2017, new_GRtype =="sedum_mat")))
summary(lm(COV ~ sqrt_AR, data = subset(GR_cig_2017, new_GRtype =="woody_plants")))
summary(lm(COV ~ sqrt_AR, data = subset(GR_cig_2017, new_GRtype =="mix_grass_tree")))

# Combine Supplementary Figure S16: CV of VIs grouped by vegetation type (growing season) --------
merge_COV_VIs_design_vegetype_2017 = 
  CV_savi_area_2017_vegetype.plot + CV_savi_height_2017_vegetype.plot + CV_savi_vegetype_barplot_2017 + CV_savi_sqrt_AR_vegetype_2017.plot +
  CV_gndvi_area_2017_vegetype.plot + CV_gndvi_height_2017_vegetype.plot + CV_gndvi_vegetype_barplot_2017 + CV_gndvi_sqrt_AR_vegetype_2017.plot +
  CV_cig_area_2017_vegetype.plot + CV_cig_height_2017_vegetype.plot + CV_cig_vegetype_barplot_2017 + CV_cig_sqrt_AR_vegetype_2017.plot +
  plot_layout(guides = "collect", ncol = 4) & theme(legend.position = "top")

merge_COV_VIs_design_vegetype_2017[[1]] = merge_COV_VIs_design_vegetype_2017[[1]] + theme(axis.title.x = element_blank())
merge_COV_VIs_design_vegetype_2017[[2]] = merge_COV_VIs_design_vegetype_2017[[2]] + theme(legend.position = "none", axis.title.y = element_blank(), axis.title.x = element_blank())
merge_COV_VIs_design_vegetype_2017[[3]] = merge_COV_VIs_design_vegetype_2017[[3]] + theme(legend.position = "none", axis.title.y = element_blank(), axis.title.x = element_blank())
merge_COV_VIs_design_vegetype_2017[[4]] = merge_COV_VIs_design_vegetype_2017[[4]] + theme(legend.position = "none", axis.title.y = element_blank(), axis.title.x = element_blank())
merge_COV_VIs_design_vegetype_2017[[5]] = merge_COV_VIs_design_vegetype_2017[[5]] + theme(legend.position = "none",axis.title.x = element_blank())
merge_COV_VIs_design_vegetype_2017[[6]] = merge_COV_VIs_design_vegetype_2017[[6]] + theme(legend.position = "none", axis.title.y = element_blank(), axis.title.x = element_blank())
merge_COV_VIs_design_vegetype_2017[[7]] = merge_COV_VIs_design_vegetype_2017[[7]] + theme(legend.position = "none", axis.title.y = element_blank(), axis.title.x = element_blank())
merge_COV_VIs_design_vegetype_2017[[8]] = merge_COV_VIs_design_vegetype_2017[[8]] + theme(legend.position = "none", axis.title.y = element_blank(), axis.title.x = element_blank())
merge_COV_VIs_design_vegetype_2017[[9]] = merge_COV_VIs_design_vegetype_2017[[9]] + theme(legend.position = "none")
merge_COV_VIs_design_vegetype_2017[[10]] = merge_COV_VIs_design_vegetype_2017[[10]] + theme(legend.position = "none", axis.title.y = element_blank())
merge_COV_VIs_design_vegetype_2017[[11]] = merge_COV_VIs_design_vegetype_2017[[11]] + theme(legend.position = "none", axis.title.y = element_blank())
merge_COV_VIs_design_vegetype_2017[[12]] = merge_COV_VIs_design_vegetype_2017[[12]] + theme(legend.position = "none", axis.title.y = element_blank())

merge_COV_VIs_design_vegetype_2017

# ggsave(plot=merge_COV_VIs_design_vegetype_2017, filename = "path/VIs_DesignFactors_CV_vegetype_2017.tif", width = 24, height = 16, device='tiff', dpi=600,unit="in")


# Combine Supplementary Figure S17: CV of VIs grouped by vegetation type (pre-growing season) --------
merge_COV_VIs_design_vegetype_2018 = 
  CV_savi_area_2018_vegetype.plot + CV_savi_height_2018_vegetype.plot + CV_savi_vegetype_barplot_2018 + CV_savi_sqrt_AR_vegetype_2018.plot +
  CV_gndvi_area_2018_vegetype.plot + CV_gndvi_height_2018_vegetype.plot + CV_gndvi_vegetype_barplot_2018 + CV_gndvi_sqrt_AR_vegetype_2018.plot +
  CV_cig_area_2018_vegetype.plot + CV_cig_height_2018_vegetype.plot + CV_cig_vegetype_barplot_2018 + CV_cig_sqrt_AR_vegetype_2018.plot +
  plot_layout(guides = "collect", ncol = 4) & theme(legend.position = "top")

merge_COV_VIs_design_vegetype_2018[[1]] = merge_COV_VIs_design_vegetype_2018[[1]] + theme(axis.title.x = element_blank())
merge_COV_VIs_design_vegetype_2018[[2]] = merge_COV_VIs_design_vegetype_2018[[2]] + theme(legend.position = "none", axis.title.y = element_blank(), axis.title.x = element_blank())
merge_COV_VIs_design_vegetype_2018[[3]] = merge_COV_VIs_design_vegetype_2018[[3]] + theme(legend.position = "none", axis.title.y = element_blank(), axis.title.x = element_blank())
merge_COV_VIs_design_vegetype_2018[[4]] = merge_COV_VIs_design_vegetype_2018[[4]] + theme(legend.position = "none", axis.title.y = element_blank(), axis.title.x = element_blank())
merge_COV_VIs_design_vegetype_2018[[5]] = merge_COV_VIs_design_vegetype_2018[[5]] + theme(legend.position = "none",axis.title.x = element_blank())
merge_COV_VIs_design_vegetype_2018[[6]] = merge_COV_VIs_design_vegetype_2018[[6]] + theme(legend.position = "none", axis.title.y = element_blank(), axis.title.x = element_blank())
merge_COV_VIs_design_vegetype_2018[[7]] = merge_COV_VIs_design_vegetype_2018[[7]] + theme(legend.position = "none", axis.title.y = element_blank(), axis.title.x = element_blank())
merge_COV_VIs_design_vegetype_2018[[8]] = merge_COV_VIs_design_vegetype_2018[[8]] + theme(legend.position = "none", axis.title.y = element_blank(), axis.title.x = element_blank())
merge_COV_VIs_design_vegetype_2018[[9]] = merge_COV_VIs_design_vegetype_2018[[9]] + theme(legend.position = "none")
merge_COV_VIs_design_vegetype_2018[[10]] = merge_COV_VIs_design_vegetype_2018[[10]] + theme(legend.position = "none", axis.title.y = element_blank())
merge_COV_VIs_design_vegetype_2018[[11]] = merge_COV_VIs_design_vegetype_2018[[11]] + theme(legend.position = "none", axis.title.y = element_blank())
merge_COV_VIs_design_vegetype_2018[[12]] = merge_COV_VIs_design_vegetype_2018[[12]] + theme(legend.position = "none", axis.title.y = element_blank())

merge_COV_VIs_design_vegetype_2018

# ggsave(plot=merge_COV_VIs_design_vegetype_2018, filename = "path/VIs_DesignFactors_CV_vegetype_2018.tif", width = 24, height = 16, device='tiff', dpi=600,unit="in")

