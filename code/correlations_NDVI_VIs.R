# Remove variables in the global environment
rm(list = ls())

# install the package if have not
# install.packages("ggplot2")
# install.packages("dplyr")
# install.packages("openxlsx")
# install.packages("readxl")
# install.packages("patchwork")

# packages --------------
library(ggplot2)
library(dplyr)
library(readxl)
library(openxlsx)
library(patchwork)

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

# Function to remove duplicates
merge_remove_duplicates <- function(x, y) {
  common_cols <- setdiff(intersect(names(x), names(y)), "uniqueID_year")   # Identify common columns, excluding the uniqueID column used for merging
  y <- y[, !(names(y) %in% common_cols)]  # Decide which columns to keep. Here, we are keeping the columns from the first data frame (x)
  # Merge the data frames
  merge(x, y, by = "uniqueID_year", all = TRUE)
}


# Read Data Files ------------------------------------------------
ndvi_data <- read_excel('~/data/ndvi_data.xlsx',sheet = "Sheet 1")
savi_data <- read_excel('~/data/savi_data.xlsx',sheet = "Sheet 1")
gndvi_data <- read_excel('~/data/gndvi_data.xlsx',sheet = "Sheet 1")
cig_data <- read_excel('~/data/cig_data.xlsx',sheet = "Sheet 1")

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


# Figure S20: Correlation between mean NDVI and mean VIs ------------------
# clean the data: pool all VI data based on unique ID
ndvi_data = ndvi_data[, c(1,2,5:8, 15, 20:22, 26, 27)]
savi_data = savi_data[, c(1,2,5:8, 15, 20:22, 26, 27)]
gndvi_data = gndvi_data[, c(1,2,5:8, 15, 20:22, 26, 27)]
cig_data = cig_data[, c(1,2,5:8, 15, 20:22, 26, 27)]

ndvi_data$uniqueID_year = paste0(ndvi_data$uniqueID,"_", ndvi_data$year)
savi_data$uniqueID_year = paste0(savi_data$uniqueID,"_", savi_data$year)
gndvi_data$uniqueID_year = paste0(gndvi_data$uniqueID,"_", gndvi_data$year)
cig_data$uniqueID_year = paste0(cig_data$uniqueID,"_", cig_data$year)

# Change the names for VIs
colnames(ndvi_data)[7] <- "mean_NDVI"
colnames(savi_data)[7] <- "mean_SAVI"
colnames(gndvi_data)[7] <- "mean_GNDVI"
colnames(cig_data)[7] <- "mean_CIg"

# Merge the datasets to one data
pool_data <- Reduce(merge_remove_duplicates, list(ndvi_data, savi_data, gndvi_data, cig_data))

##### Correlation: NDVI vs. SAVI #####
general_ndvi_savi_cor.plot <- ggplot(pool_data, aes(x=mean_NDVI,y=mean_SAVI))+
  geom_point(color = "#8bb4c9", size=2.5, alpha = 0.3) +
  geom_smooth(color = "#2892c7", fill="#2892c7",
              method='lm', formula= y~x, alpha = 0.3, cex=1.3)+
  labs(x="Mean NDVI", y="Mean SAVI",
       group = "GR_type", linetype = "GR_type", pch = "GR_type")+
  annotate("text", x = -0.12, y = 0.42, label = expression("Adjusted"~R^2~" = 0.882"), size = 8, color = "black", hjust = 0) +
  annotate("text", x = -0.12, y = 0.36, label = expression(italic("P")~" < 0.001"), size = 8, color = "black", hjust = 0) +
  xlim(c(-0.15, 0.46)) +
  ylim(c(-0.15, 0.46)) +
  coord_fixed(ratio = 1) +
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

general_ndvi_savi_cor.plot

# Linear regression: mean NDVI vs. mean SAVI
summary(lm(mean_NDVI ~ mean_SAVI, data = pool_data))


##### Correlation: NDVI vs. GNDVI #####
general_ndvi_gndvi_cor.plot <- ggplot(pool_data, aes(x=mean_NDVI,y=mean_GNDVI))+
  geom_point(color = "#8bb4c9", size=2.5, alpha = 0.3) +
  geom_smooth(color = "#2892c7", fill="#2892c7",
              method='lm', formula= y~x, alpha = 0.3, cex=1.3)+
  labs(x="Mean NDVI", y="Mean GNDVI",
       group = "GR_type", linetype = "GR_type", pch = "GR_type")+
  annotate("text", x = -0.12, y = 0.52, label = expression("Adjusted"~R^2~" = 0.711"), size = 8, color = "black", hjust = 0) +
  annotate("text", x = -0.12, y = 0.45, label = expression(italic("P")~" < 0.001"), size = 8, color = "black", hjust = 0) +
  xlim(c(-0.15, 0.56)) +
  ylim(c(-0.15, 0.56)) +
  coord_fixed(ratio = 1) +
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

general_ndvi_gndvi_cor.plot

# Linear regression: mean NDVI vs. mean GNDVI
summary(lm(mean_NDVI ~ mean_GNDVI, data = pool_data)) 


##### Correlation: NDVI vs. CIg #####
general_ndvi_cig_cor.plot <- ggplot(pool_data, aes(x=mean_NDVI,y=mean_CIg))+
  geom_point(color = "#8bb4c9", size=2.5, alpha = 0.3) +
  geom_smooth(color = "#2892c7", fill="#2892c7",
              method='lm', formula= y~x, alpha = 0.3, cex=1.3)+
  labs(x="Mean NDVI", y="Mean CIg",
       group = "GR_type", linetype = "GR_type", pch = "GR_type")+
  annotate("text", x = 0.2, y = -0.32, label = expression("Adjusted"~R^2~" = 0.698"), size = 8, color = "black", hjust = 0) +
  annotate("text", x = 0.2, y = -0.5, label = expression(italic("P")~" < 0.001"), size = 8, color = "black", hjust = 0) +
  xlim(c(-0.55, 1.7)) +
  ylim(c(-0.55, 1.7)) +
  coord_fixed(ratio = 1) +
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

general_ndvi_cig_cor.plot

# Linear regression: mean NDVI vs. mean CIg
summary(lm(mean_NDVI ~ mean_CIg, data = pool_data))

