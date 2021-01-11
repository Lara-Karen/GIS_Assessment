# PACKAGES ######
library(sf)
library(tmap)
library(raster)
library(tmaptools)
library(tidyverse)
library(here)
library(ggplot2)
library(janitor)
library(dplyr)
library(rgeos)
library(ggmap)
library(leaflet)
library(spatstat)
library(RColorBrewer)
library(tibble)
library(ggspatial)
library(geojsonio)
library(plotly)
library(tidypredict)
library(rgdal)
library(broom)
library(mapview)
library(sp)
library(spdep)
library(car)
library(fs)
library(hexbin)

#setwd("c:/Users/larakaren/Documents/GitHub/GIS_Assessment")

#___________________________________________________
# SAN FRANCISCO SHAPE FILE IMPORT & PROCESSING #####
#___________________________________________________

cal_census <- st_read(here::here("Data",
                                            "shapefile",
                                            "sanfran.shp")) %>%
  st_transform(., 4326) %>%
  clean_names()

# Projecting from NAD83 to WSG84
library(sp)
library(rgdal)

cal_censusSP <- cal_census %>%
  as(., "Spatial")

# Check the CRS of the data 
proj4string(cal_censusSP) 
#Convert the CRS to WGS84
cal_censusSP=spTransform(cal_censusSP,CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))

cal_census <- cal_censusSP %>%
  st_as_sf()

# Filtering based on county code of 075
SF_census <- cal_census %>%
  filter(., countyfp == "075")%>%
  rename(census_block=geoid)%>%
  drop_na()

#delete the following SF blocks: 060759804011, 060750179021, 060750179021 since they are outside the boundary
SF_census <- SF_census %>%
  filter(., census_block != "060759804011")%>%
  filter(., census_block != "060750179021")

#make sure it is a spatial feature
class(SF_census)

#______________________________________



# CRIME DATA IMPORT & PROCESSING #####
#______________________________________

#importing and merging crime to census_block
crime <- st_read(here::here("Data",
                            "Police_Department_Crime.csv"))%>%
  clean_names()

crime_geom<- sf::st_as_sf(crime, coords = c("longitude", "latitude"), agr = "constant",  crs=4326)

crime_and_census <- st_join(SF_census, crime_geom)

#dividing crime to night and day 
crime_am_pm <- crime %>%
  mutate(am_pm = case_when(
    crime$incident_time < 8 ~ "night",
    crime$incident_time  == 8 ~ "day",
    crime$incident_time  == 9 ~ "day",
    crime$incident_time  == 10 ~ "day",
    crime$incident_time  == 11 ~ "day",
    crime$incident_time  == 12 ~ "day",
    crime$incident_time  == 13 ~ "day",
    crime$incident_time  == 14 ~ "day",
    crime$incident_time == 15 ~ "day",
    crime$incident_time == 16 ~ "day",
    crime$incident_time == 17 ~ "day",
    crime$incident_time == 18 ~ "day",
    crime$incident_time == 19 ~ "day",
    crime$incident_time == 20 ~ "day",
    crime$incident_time == 21 ~ "day",
    crime$incident_time >= 22 ~ "night"))

crime_am <- crime_am_pm %>%
  filter(., am_pm=="day")

crime_pm <- crime_am_pm %>%
  filter(., am_pm=="night")

#crime count per census block
crime_per_census <- crime_and_census [,c("census_block","incident_category", "point")]%>%
  clean_names()%>%
  group_by(census_block)%>%
  summarise(count=n())%>%
  rename(crime_count=count)

# finding the area of each census block
crime_per_census <- mutate(crime_per_census, census_block_area= st_area(crime_per_census$geometry))

#dividing crime by area
crime_per_census <- dplyr::mutate(crime_per_census, crime_per_km2= (crime_per_census$crime_count/crime_per_census$census_block_area)/0.000001)

crime_per_census <- type.convert(crime_per_census,
                                 na.strings = "NA",
                                 as.numeric(crime_per_census$crime_per_km2),
                                 numerals = c("warn.loss"))

#____________________


# PLOTTING CRIME #####
#____________________

# hexbin 
sfc_as_cols <- function(x, names = c("x","y")) {
  stopifnot(inherits(x,"sf") && inherits(sf::st_geometry(x),"sfc_POINT"))
  ret <- sf::st_coordinates(x)
  ret <- tibble::as_tibble(ret)
  stopifnot(length(names) == ncol(ret))
  x <- x[ , !names(x) %in% names]
  ret <- setNames(ret,names)
  dplyr::bind_cols(x,ret)
}

## Total Crime 
map_crime <- sf::st_as_sf(crime, coords = c("longitude", "latitude"),  crs=4326)

ma_spatial <- sfc_as_cols(map_crime, c("Longitude", "Latitude"))

ggplot(ma_spatial, aes(Longitude, Latitude)) +  #define data and variables for x and y axes 
  annotation_map_tile(zoom = 13, alpha = 0.4, type = "cartolight", cachedir = system.file("rosm.cache", package = "ggspatial"))+
  stat_binhex(bins=30, alpha =0.9) + #add binhex layer (hexbin)
  scale_fill_gradientn(colours = c("white","indianred"), name = "Frequency")    #add shading based on number of ASB incidents

## Day Crime 

map_crime_day <- sf::st_as_sf(crime_am, coords = c("longitude", "latitude"),  crs=4326)

ma_day_spatial <- sfc_as_cols(map_crime_day, c("Longitude", "Latitude"))

ggplot(ma_day_spatial, aes(Longitude, Latitude)) +  #define data and variables for x and y axes 
  annotation_map_tile(zoom = 13, alpha = 0.4, type = "cartolight", cachedir = system.file("rosm.cache", package = "ggspatial"))+
  stat_binhex(bins=30, alpha =1) + #add binhex layer (hexbin)
  scale_fill_gradientn(colours = c("white","light blue"), name = "Frequency")    #add shading based on number of ASB incidents

## Night Crime 

map_crime_night <- sf::st_as_sf(crime_pm, coords = c("longitude", "latitude"),  crs=4326)

ma_night_spatial <- sfc_as_cols(map_crime_night, c("Longitude", "Latitude"))

ggplot(ma_night_spatial, aes(Longitude, Latitude)) +  #define data and variables for x and y axes 
  annotation_map_tile(zoom = 13, alpha = 0.4, type = "cartolight", cachedir = system.file("rosm.cache", package = "ggspatial"))+
  stat_binhex(bins=30, alpha =1) + #add binhex layer (hexbin)
  scale_fill_gradientn(colours = c("white","midnightblue"), name = "Frequency")    #add shading based on number of ASB incidents


### crime per census blocks per km2
tmap_mode("view")
tmap_options(max.categories = 578) 

crime_cuts = c(0,200,400,600,800,1000,1200,1400,1600,1800,2000,2200,2400,2600,2800,3000,3200,3600,4000, 4800, 5200,6000)

tm_shape(crime_per_census) + 
  tm_polygons("crime_per_km2", breaks = crime_cuts, palette="Reds", contrast=1.5, id="nhood", title="Crime per Km2", alpha = 0.7)+
  tm_legend()+
  tm_layout(legend.outside = TRUE, title = "Neighborhoods in San Francisco", inner.margins = c(0.3, 0.0, 0.0, 0.3), frame = FALSE)+
  tm_compass(type = "4star", size = 2.5, text.size = 0.5, color.dark = "gray60", text.color = "gray60", position = c("right", "bottom")) +
  tm_scale_bar(color.dark = "gray60", position = c("right", "bottom"))

### crime per point 

map_crime <- sf::st_as_sf(crime, coords = c("longitude", "latitude"),  crs=4326)

#Total
tmap_mode("view")
tm_shape(SF_census) +
  tm_polygons(col = NA, alpha = 0.5, title= "Crime Locations") +
  tm_shape(map_crime) +
  tm_dots(col = "red", alpha = 0.2, size = 0.01)

#Daytime
tm_shape(SF_census) +
  tm_polygons(col = NA, alpha = 0.5, title= "Daytime Crime Locations") +
  tm_shape(crime_day) +
  tm_dots(col = "red", alpha = 0.2, size = 0.02)

#Nighttime
tm_shape(SF_census) +
  tm_polygons(col = NA, alpha = 0.5, title= "Crime Locations") +
  tm_shape(map_crime) +
  tm_dots(col = "red", alpha = 0.2, size = 0.02)


#______________________________________
# CRIME DESCRIPTIVE STATISTICS #####
#______________________________________


## Bar Graph for frequency during nighttime and daytime
require(ggplot2)

# first get rid of na's
crime_am_pm <- crime_am_pm %>%
  drop_na()

barplot <- ggplot(crime_am_pm, aes(x = am_pm, fill=incident_category),
                  legend.title = "Type of Crimes") +
  geom_bar() +
  scale_fill_brewer(palette="Dark2") +
  coord_flip()
print (barplot +
         ggtitle("Night-time & Day-time Frequency of Crime") + 
         labs(y="Count of Incidents ", x="Time of Day", fill="Types of Incident"))

## histogram for crime per km2
crime_per_census %>%
  ggplot(aes(x=crime_per_km2)) +
  geom_histogram(position="identity", 
                 alpha=0.5, 
                 bins=15, 
                 fill="indianred2", col="indianred2")+
  geom_vline(aes(xintercept=mean(crime_per_km2)),
             color="indianred4",
             linetype="dashed")+
  labs(title="Crime Incidents' Variable Distribution",
       x="Crime Incidents per km^2",
       y="Frequency")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))

#### after we log it 
crime_per_census %>%
  ggplot(aes(x=log(crime_per_km2))) +
  geom_histogram(position="identity", 
                 alpha=0.5, 
                 bins=15, 
                 fill="indianred2", col="indianred2")+
  geom_vline(aes(xintercept=mean(log(crime_per_km2))),
             color="indianred4",
             linetype="dashed")+
  labs(title="Log Transformed Crime Incidents' Variable Distribution",
       x="Log (Crime Incidents per km^2)",
       y="Frequency")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))

#______________________________________
# LANDUSE DATA IMPORT & PROCESSING #####
#______________________________________

### Landuse Data ###
landuse <- st_read(here::here("Data",
                              "Land Use",
                              "landuse.shp")) %>%
  st_transform(., 4326) %>%
  drop_na()%>%
  clean_names()

# spatial joining landuse using the census blocks
landuse_and_census <- st_join(SF_census, left = FALSE, landuse) # join all columns

#filtering landuse types into 4 categories: residential, commercial, mixed and vacant
landuse_proportion <- landuse_and_census %>%
  mutate(landuse_category = case_when(
    landuse_and_census$landuse == "RESUNITS" ~ "residential",
    landuse_and_census$landuse == "RESIDENT" ~ "residential",
    landuse_and_census$landuse == "VACANT" ~ "vacant",
    landuse_and_census$landuse == "RETAIL/ENT" ~ "commercial",
    landuse_and_census$landuse == "MIXED" ~ "mixed",
    landuse_and_census$landuse == "MIXRES" ~ "mixed",
    landuse_and_census$landuse == "OpenSpace" ~ "vacant"))%>%
      drop_na()

### Grouping landuse and adding total shape area in order to create proportion
landuse_proportion <- landuse_proportion [,c("census_block", "landuse_category", "shape_area")]%>%
  group_by(census_block,
           landuse_category) %>% 
  dplyr::summarize_all(funs(sum(shape_area)))

# finding the total area of landuse per census block
census_landuse_area <- landuse_proportion [,c("census_block", "shape_area")]%>%
  group_by(census_block) %>% 
  dplyr::summarize_all(funs(sum(shape_area))) %>%
  rename(census_area = shape_area) %>%
  as.tibble()

landuse_proportion_sf_trial <- left_join(landuse_proportion, census_landuse_area)

qtm(landuse_proportion_sf_trial)

landuse_proportion <- left_join(landuse_proportion, census_landuse_area) %>%
  as.tibble()

landuse_percentage <- landuse_proportion %>%
  dplyr::mutate(percentage = ((landuse_proportion$shape_area/landuse_proportion$census_area)*100))

landuse_percentage_per_category <- landuse_percentage %>%
  mutate(
    residential_percentage = case_when(
      landuse_category=="residential" ~ percentage)) %>%
  mutate(
    vacant_percentage = case_when(
      landuse_category=="vacant" ~ percentage)) %>%
  mutate(
    mixed_percentage = case_when(
      landuse_category=="mixed" ~ percentage)) %>%
  mutate(
    commercial_percentage = case_when(
      landuse_category=="commercial" ~ percentage))

landuse_percentage_per_category <- type.convert(landuse_percentage_per_category,
                              na.strings = "NA",
                              as.numeric(landuse_percentage_per_category$vacant_percentage),
                              as.numeric(landuse_percentage_per_category$mixed_percentage),
                              as.numeric(landuse_percentage_per_category$commercial_percentage),
                              as.numeric(landuse_percentage_per_category$residential_percentage),
                              numerals = c("warn.loss"))

#replace NA with 0 to avoid any calculation problems
landuse_percentage_per_category [is.na(landuse_percentage_per_category)] <- 0

#clean the unneeded columns
landuse_percentage <- landuse_percentage_per_category [,c("census_block","residential_percentage", "vacant_percentage", "commercial_percentage", "mixed_percentage")]

#merging and grouping per census_block
landuse_percentage <- landuse_percentage %>%
  group_by(census_block) %>% 
  dplyr::summarize_all(funs(sum))

#______________________________________
# LANDUSE DESCRIPTIVE STATISTICS #####
#______________________________________

# landuse distribution histogram 

# commercial variables
landuse_percentage %>%
  ggplot(aes(x=commercial_percentage)) +
  geom_histogram(position="identity", 
                 alpha=0.5, 
                 bins=15, 
                 fill="Orange", col="Orange")+
  geom_vline(aes(xintercept=mean(commercial_percentage)),
             color="turquoise4",
             linetype="dashed")+
  labs(title="Commercial Landuse Variable Distribution",
       x="Commercial Landuse per Census Block",
       y="Frequency")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))
#### after we log it 
landuse_percentage %>%
  ggplot(aes(x=log(commercial_percentage))) +
  geom_histogram(position="identity", 
                 alpha=0.5, 
                 bins=15, 
                 fill="Orange", col="Orange")+
  geom_vline(aes(xintercept=mean(log(commercial_percentage))),
             color="turquoise4",
             linetype="dashed")+
  labs(title="Log Transformed Commercial Landuse Variable Distribution",
       x="Log (Commercial Landuse per Census Block)",
       y="Frequency")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))


##residential variables
#its skewed 
landuse_percentage %>%
  ggplot(aes(x=residential_percentage)) +
  geom_histogram(position="identity", 
                 alpha=0.5, 
                 bins=15, 
                 fill="Blue", col="Blue")+
  geom_vline(aes(xintercept=mean(residential_percentage)),
             color="turquoise4",
             linetype="dashed")+
  labs(title="Distribution of Residential Landuse",
       x="Residential Landuse per Neighborhood",
       y="Frequency")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))

# Its still skewed after logging, so we can see the range of Tukey transformations 
# to see which is best (in this case 1.5)
symbox(~ residential_percentage, 
       landuse_percentage,
       na.rm=T,
       powers=seq(0.9,2,by=0.1))

#### after we log it #### REGECTED 
landuse_percentage %>%
  ggplot(aes(x=(residential_percentage)^1.5)) +
  geom_histogram(position="identity", 
                 alpha=0.5, 
                 bins=15, 
                 fill="turquoise", col="turquoise")+
  geom_vline(aes(xintercept=mean(log(residential_percentage))),
             color="turquoise4",
             linetype="dashed")+
  labs(title="Distribution of Residential Landuse after Log Transformation",
       x="Log (Residential Landuse per Neighborhood)",
       y="Frequency")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))

## mixed variable
landuse_percentage %>%
  ggplot(aes(x=mixed_percentage)) +
  geom_histogram(position="identity", 
                 alpha=0.5, 
                 bins=15, 
                 fill="turquoise", col="turquoise")+
  geom_vline(aes(xintercept=mean(mixed_percentage)),
             color="turquoise",
             linetype="dashed")+
  labs(title="Distribution of Mixed Landuse",
       x="Mixed Landuse per Census Block",
       y="Frequency")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))

# Its still skewed, so we can see the range of Tukey transformations to see which is best (in this case 0.4)
symbox(~ mixed_percentage, 
       landuse_percentage,
       na.rm=T,
       powers=seq(0,1,by=0.15))

#so raising our price variable to the power of 0.3 should make it more normally distributed 
# after we log it 
landuse_percentage %>%
  ggplot(aes(x=(mixed_percentage)^0.4)) +
  geom_histogram(position="identity", 
                 alpha=0.5, 
                 bins=15, 
                 fill="turquoise", col="turquoise")+
  geom_vline(aes(xintercept=mean((mixed_percentage)^0.3)),
             color="turquoise4",
             linetype="dashed")+
  labs(title="Distribution of Mixed Landuse after Log Transformation",
       x="Log (Mixed Landuse per Census Block)",
       y="Frequency")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))

## vacant variables
landuse_percentage %>%
  ggplot(aes(x=vacant_percentage)) +
  geom_histogram(position="identity", 
                 alpha=0.5, 
                 bins=15, 
                 fill="turquoise", col="turquoise")+
  geom_vline(aes(xintercept=mean(vacant_percentage)),
             color="turquoise4",
             linetype="dashed")+
  labs(title="Distribution of Vacant Landuse",
       x="Vacant Landuse per Census Block",
       y="Frequency")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))
#### after we log it, still skewed but a bit more normally distributed
landuse_percentage %>%
  ggplot(aes(x=log(vacant_percentage))) +
  geom_histogram(position="identity", 
                 alpha=0.5, 
                 bins=15, 
                 fill="turquoise", col="turquoise")+
  geom_vline(aes(xintercept=mean(log(vacant_percentage))),
             color="turquoise4",
             linetype="dashed")+
  labs(title="Distribution of Vacant Landuse after Log Transformation",
       x="Log (Vacant Landuse per Census Block)",
       y="Frequency")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))


#we add in the transformations, more normally distributed variables in order to have a more accurate regression.
#landuse_percentage <- landuse_percentage %>%
#  mutate(landuse_percentage, mixed_percentage_log = ((mixed_percentage)^0.4)) +
#  mutate(landuse_percentage, commercial_percentage_log = (log(commercial_percentage))) 


#______________________________________
# MERGING FINAL DATASET #####
#______________________________________

final_dataset<- dplyr::left_join(landuse_percentage,crime_per_census) 

final_dataset <- final_dataset %>% extract(geometry, c('lat', 'lon'), '\\((.*), (.*)\\)', convert = TRUE) 

reg_columns <- final_dataset [,c("crime_per_km2", "residential_percentage", "vacant_percentage", "commercial_percentage", "mixed_percentage")]

final <- cbind(SF_census, reg_columns)

#______________________________________
# LANDUSE PLOTS #####
#______________________________________
tmap_options(max.categories = 50)

## Census Block Divisions Map
tm_shape(SF_census) + 
  tm_polygons("census_block", palette="Paired", contrast=.7, alpha = 0.4)+
  tm_layout(title = "", inner.margins = c(0.1, 0.1, 0.1, 0.0), frame = FALSE)+
  tm_compass(type = "4star", size = 2.5, text.size = 0.5, color.dark = "gray60", text.color = "gray60", position = c("right", "bottom")) +
  tm_scale_bar(color.dark = "gray60", position = c("right", "bottom"))

tmap_mode("plot")


#COMMERCIAL
tmap_mode("plot")
cuts_commercial = c(0,5,10,15,20,25,30,35,40)

tm_shape(final)+
  tm_polygons("commercial_percentage",
              breaks = cuts_commercial,
              palette="Oranges",
              contrast=1,
              title = "Commercial (%)",
              alpha = 1,
              font.size=0.3)+
  tm_layout(legend.outside = FALSE, 
            title = NA, 
            inner.margins = c(0, 0.0, 0.0, 0.05), 
            frame = FALSE)
#if we want to plot crime points, run this in
  #tm_shape(crime_per_census) +
  #tm_dots(
    #palette = "indianred",
    #col = NA,
    #title = "Crime",
    #size = 0.02,
    #alpha=0.05,
    #popup.vars = TRUE)
  
#Residential
cuts_residential = c(0,20,40,60,80,100)

tm_shape(final)+
  tm_polygons("residential_percentage", 
              breaks=cuts_residential,
              palette="Blues",
              contrast=1,
              title="Residential (%)",
              alpha = 0.7,
              font.size=3)+
  tm_layout(legend.outside = FALSE, 
            title = NA, 
            inner.margins = c(0, 0.0, 0.0, 0.05), 
            frame = FALSE)
#mixed
cuts_landuse = c(0,20,40,60,80,100)

tm_shape(final)+
tm_polygons("mixed_percentage", 
            palette="BuPu",
            cuts= cuts_landuse,
            contrast=1,
            title="Mixed (%)",
            alpha = 1,
            font.size=0.1)+
  tm_layout(legend.outside = FALSE, 
            title = NA, 
            inner.margins = c(0, 0.0, 0.0, 0.05), 
            frame = FALSE)

#Vacant
tm_shape(final)+
  tm_polygons("vacant_percentage", 
              cuts= cuts_landuse,
              palette="Greens",
              contrast=1,
              title="Vacant(%)",
              alpha = 1,
              font.size=0.1)+
  tm_layout(legend.outside = FALSE, 
            title = NA, 
            inner.margins = c(0, 0.0, 0.0, 0.05), 
            frame = FALSE)
  

#______________________________________
# MULTIPLE REGRESSION #####
#______________________________________

Regressiondata <- final_dataset

model <- Regressiondata %>%
  lm(log(crime_per_km2) ~
       commercial_percentage + residential_percentage + mixed_percentage + vacant_percentage,
     data=.)

summary(model)
tidy(model)

model_data <- model %>%
  augment(., Regressiondata)

# Mixed Land Use Percentage
model1 <- Regressiondata %>%
  lm(log(crime_per_km2) ~
       mixed_percentage,
     data=.)

summary(model1)
tidy(model1)

model_data1 <- model1 %>%
  augment(., Regressiondata)

## Residential Land Use Percentage

model_data2 <- model2 %>%
  augment(., Regressiondata)

model2 <- Regressiondata %>%
  lm(log(crime_per_km2) ~
       residential_percentage,
     data=.)

summary(model2)
tidy(model2)

model_data2 <- model2 %>%
  augment(., Regressiondata)

# Vacant Land Use Percentage

model_data3 <- model3 %>%
  augment(., Regressiondata)

model3 <- Regressiondata %>%
  lm(log(crime_per_km2) ~
       vacant_percentage,
     data=.)

summary(model3)
tidy(model3)

model_data3 <- model3 %>%
  augment(., Regressiondata)

# Commercial Land Use Percentage

model_data4 <- model4 %>%
  augment(., Regressiondata)

model4 <- Regressiondata %>%
  lm(log(crime_per_km2) ~
       commercial_percentage,
     data=.)

summary(model4)
tidy(model4)

model_data4 <- model4 %>%
  augment(., Regressiondata)

#residuals look normally distributed! fulfilling assumption 2
model_data%>%
  dplyr::select(.resid)%>%
  pull()%>%
  qplot()+ 
  geom_histogram(position="identity", 
                 alpha=0.8, 
                 bins=30, 
                 fill="indianred1", col="indianred1")+
  labs(title="Residual Distribution",
       x="Residual Distribution",
       y="Frequency")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5)) 

# Taking the residuals
SF_census <- SF_census %>%
  mutate(model2resids = residuals(model))

#Examining of VIF less than 10 (close to 0) indicates that the mult. reg. 
#aligns with the multicollinearity assumption. #results are all ~1.2
vif(model)

#the residuals vs fits in the model diagnostics,  should show a straight line and a 
# randomly distributed cloud of points, which it fullfills, so we can proceed. 
par(mfrow=c(1,4))    #plot to 2 by 2 array
plot(model)

#to check for independence of errors, we do the DW test, it shows the stat of 1.37 
# which indicates a slight autocorrelation, but it is close to 2 so it is ok.
DW <- durbinWatsonTest(model)
tidy(DW)

#plotting the residuals: 
tmap_mode("view")
qtm(SF_census, fill = "census_block", palette = "Blues")

tm_shape(SF_census) +
  tm_polygons("model2resids",
              palette = "RdYlBu") +
  tm_shape(crime_geom) + tm_dots(size =0.000001,border.col = NULL, alpha = 0.6, col = "indianred3")


#___________________________________________________
# MORANS I #####
#___________________________________________________


#getting the centroids for the San Francisco Census Blocks
coordsSanFran <- SF_census%>%
  st_centroid()%>%
  st_geometry()

plot(coordsSanFran)

#we use the binary matrix of queen's case neighbors to get the weight matrix and nearest neighours 
SanFran_nb <- SF_census %>%
  poly2nb(., queen=T)

SanFran_blocks <-coordsSanFran %>%
  knearneigh(., k=4)

SanFranblock_knn <- SanFran_blocks %>%
  knn2nb()

plot(SanFran_nb, st_geometry(coordsSanFran), col="red") %>%
  plot(SF_census)

sf_weight <- SanFran_nb %>%
  nb2listw(., style="C")

sf.knn_4_weight <- SanFranblock_knn %>%
  nb2listw(., style="C")

Queen <- SF_census %>%
  st_drop_geometry()%>%
  dplyr::select(model2resids)%>%
  pull()%>%
  moran.test(., sf_weight)%>%
  tidy()

Nearest_neighbour <- SF_census %>%
  st_drop_geometry()%>%
  dplyr::select(model2resids)%>%
  pull()%>%
  moran.test(., sf.knn_4_weight)%>%
  tidy()

Queen

Nearest_neighbour



#___________________________________________________
# SPATIAL REGRESSION #####
#___________________________________________________


#use the Spatialreg Package to conduct a spatially lagged regression model.
library(spatialreg)

model_queen <- lagsarlm(crime_per_km2 ~ vacant_percentage + commercial_percentage + residential_percentage + mixed_percentage,
                        data = final_sf, 
                        nb2listw(SanFran_nb , style="C"), 
                        method = "eigen")
tidy(model_queen)

spreg_summary<-summary(model_queen)

SF_census <- SF_census %>%
  mutate(crime_per_km2 = residuals(model_knn4))

# spatially-lagged regression model
model_knn4 <- lagsarlm(crime_per_km2 ~ vacant_percentage + commercial_percentage + residential_percentage + mixed_percentage,
                       data = final_sf,
                       nb2listw(SanFranblock_knn, 
                                style="C"), 
                       method = "eigen")
tidy(model_knn4)

#taking the residuals
SF_census <- SF_census %>%
  mutate(SF_knn_resids = residuals(model_knn4))

KNN4Moran <- SF_census %>%
  st_drop_geometry()%>%
  dplyr::select(SF_knn_resids)%>%
  pull()%>%
  moran.test(., sf.knn_4_weight)%>%
  tidy()

KNN4Moran

write.csv(final_dataset,"File Name.csv", row.names = FALSE)

#___________________________________________________
# GWR #####
#___________________________________________________

#select some variables from the data file
myvars <- final_dataset %>%
  dplyr::select(crime_per_km2,
                mixed_percentage,
                residential_percentage,
                commercial_percentage)

#check their correlations first
Correlation_myvars <- myvars %>%
  correlate()

#run a final OLS model
model_final <- lm(log(crime_per_km2) ~ commercial_percentage + 
                    mixed_percentage + 
                    residential_percentage, 
                  data = myvars)

tidy(model_final)

SF_census <- SF_census %>%
  mutate(model_final_res = residuals(model_final))

par(mfrow=c(1,4))
plot(model_final)

qtm(SF_census, fill = "model_final_res")


final_model_Moran <- SF_census %>%
  st_drop_geometry()%>%
  dplyr::select(model_final_res)%>%
  pull()%>%
  moran.test(., sf.knn_4_weight)%>%
  tidy()

final_model_Moran

library(spgwr)

st_crs(SF_census) = 4326
SF_censusSP <- SF_census %>%
  as(., "Spatial")

st_crs(coordsSanFran) = 4326
coordsSanFranSP <- coordsSanFran %>%
  as(., "Spatial")

#running model after calculating the kernel bandwidth 
GWRbandwidth <- gwr.sel(crime_per_km2 ~ vacant_percentage + 
                          commercial_percentage + 
                          residential_percentage + 
                          mixed_percentage,
                        data = final_dataset, 
                        coords=coordsSanFran,
                        adapt=T)

gwr.model = gwr(crime_per_km2 ~ vacant_percentage + 
                  commercial_percentage + 
                  residential_percentage + 
                  mixed_percentage,
                data = final_dataset, 
                coords=coordsSanFran, 
                adapt=GWRbandwidth, 
                hatmatrix=TRUE, 
                se.fit=TRUE)

SF_census_1 <- SF_census %>%
  mutate(coefcrime = results$crime_per_km2,
         coefmixedlanduse = results$mixed_percentage,
         coefvacant = results$vacant_percentage,
         coefres = results$residential_percentage,
         coefcommercial = results$commercial_percentage)

tm_shape(SF_census_1) +
  tm_polygons(col = "coefcrime", 
              palette = "Reds", 
              alpha = 0.9)
