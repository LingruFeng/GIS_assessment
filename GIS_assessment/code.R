## Library packages
library(spatstat)
library(here)
library(sp)
library(rgeos)
library(maptools)
library(GISTools)
library(tmap)
library(sf)
library(geojson)
library(geojsonio)
library(tmaptools)
library(tidyverse)
library(stringr)
library(ggplot2)
library(spdep)

## Data reading
# First, get the London Borough Boundaries
LondonBoroughs <- st_read("https://github.com/LingruFeng/GIS_assessment/blob/main/London_Boroughs.gpkg?raw=true")
LondonBoroughs <- LondonBoroughs %>% st_transform(., 27700)

##Now get the location of all rapid charging points in the City
charging_points <- st_read("https://github.com/LingruFeng/GIS_assessment/blob/main/Rapid_charging_points.gpkg?raw=true")
charging_points <- charging_points %>% st_set_crs(., 27700) %>% st_transform(.,27700)
# get public use points and taxi charging points
taxi <- charging_points %>% filter(taxipublicuses == 'Taxi')
public <- charging_points %>% filter(taxipublicuses == 'Public use' | taxipublicuses == 'Public Use')


## Data cleaning and summarizing
# select the points inside London  
charging_points <- charging_points[LondonBoroughs,]
taxi <- taxi[LondonBoroughs,]
public <- public[LondonBoroughs,]
# Correct the borough name to make different data set match each other
LondonBoroughs[22,2]='Hammersmith & Fulham'
LondonBoroughs[19,2]='Richmond'
names(LondonBoroughs)[names(LondonBoroughs) == 'name'] <- 'borough'
public[77,1]='Hillingdon'
taxi[6,1]='City of London'


# Summarize the total number of rapid charging points for public use and taxi in each borough
# Count the number of rapid charging points for public use in each borough
public_point <- public %>% 
  sf::st_as_sf() %>%
  st_drop_geometry() %>% 
  full_join(LondonBoroughs, by = "borough") %>% 
  select('borough','numberrcpoints')
public_point[is.na(public_point)] <- 0
public_point$numberrcpoints <- as.numeric(public_point$numberrcpoints)
public_point <- public_point %>% group_by(borough) 
public_point <- summarise(public_point,public_point_number=sum(numberrcpoints))

# Count the number of rapid charging points for taxi in each borough
taxi_point <- taxi %>% 
  sf::st_as_sf() %>%
  st_drop_geometry() %>% 
  full_join(LondonBoroughs, by = "borough") %>% 
  select('borough','numberrcpoints')
taxi_point[is.na(taxi_point)] <- 0
taxi_point$numberrcpoints <- as.numeric(taxi_point$numberrcpoints)
taxi_point <- taxi_point %>% group_by(borough) 
taxi_point <- summarise(taxi_point,taxi_point_number=sum(numberrcpoints))

# Join the summarized data into LondonBoroughs
LondonBoroughs <- LondonBoroughs %>%
  left_join(.,
            public_point, 
            by = "borough") %>%
  left_join(.,
            taxi_point, 
            by = "borough")
# Calculate the density of the charging points in each borough
LondonBoroughs <- LondonBoroughs %>%
  mutate(taxi_density = taxi_point_number/hectares*10000) %>%
  mutate(public_density = public_point_number/hectares*10000)


## Data Mapping
# Charging site data mapping
tmap_mode("plot")
tm1 <- tm_shape(LondonBoroughs)+ 
  tm_polygons(col="gray",alpha = 0.2)+
  tm_layout(frame=FALSE)+
  tm_shape(public)+
  tm_symbols(col = "red", scale = .6)+
  tm_credits("(a) Public Use", position=c(0.25,0.8), size=2)
tm2 <- tm_shape(LondonBoroughs)+ 
  tm_polygons(col="gray",alpha = 0.2)+
  tm_layout(frame=FALSE)+
  tm_shape(taxi)+
  tm_symbols(col = "blue", scale = .6)+
  tm_credits("(b) Taxi", position=c(0.35,0.8), size=2)+
  tm_scale_bar(text.size=0.85,position=c(0.57,0.14))+
  tm_compass(north=0,position=c(0.8,0.25),size=3)+
  tm_credits("(c) Greater London Authority",position=c(0.53,0.12),size = 1)
t=tmap_arrange(tm1,tm2)
t

# Charging point density mapping
# Public Use Charging Point Density in Boroughs of London
tm3 <- tm_shape(LondonBoroughs) +
  tm_polygons("public_density",
  style="jenks",
  palette=brewer.pal(5, "Reds"),
  midpoint=NA,
  title="Density \n(per 10000 ha)")+
  tm_layout(frame=FALSE,
            legend.position = c("right","bottom"), 
            legend.text.size=1, 
            legend.title.size = 1.5)+
  tm_scale_bar(text.size=0.85,position=c(0,0.03))+
  tm_compass(north=0,position=c(0.03,0.12),size=3.5,text.size = 1)+
  tm_credits("(c) Greater London Authority",position=c(0,0),size = 1)
tm3
# Taxi Charging Point Density in Boroughs of London
tm4 <- tm_shape(LondonBoroughs) +
  tm_polygons("taxi_density",
              style="jenks",
              palette=brewer.pal(5, "Blues"),
              midpoint=NA,
              title="Density \n(per 10000 ha)")+
  tm_layout(frame=FALSE,
            legend.position = c("right","bottom"), 
            legend.text.size=1, 
            legend.title.size = 1.5)+
  tm_scale_bar(text.size=0.85,position=c(0,0.03))+
  tm_compass(north=0,position=c(0.03,0.12),size=3.5,text.size = 1)+
  tm_credits("(c) Greater London Authority",position=c(0,0),size = 1)
tm4

# Plot the histogram of Charging points
# dev.new()
taxi_sub <- LondonBoroughs%>%
  st_drop_geometry()%>%
  select(taxi_density) %>%
  mutate(type="taxi")
names(taxi_sub)[names(taxi_sub) == 'taxi_density'] <- 'density'
public_sub <- LondonBoroughs%>%
  st_drop_geometry()%>%
  select(public_density) %>%
  mutate(type="public use")
names(public_sub)[names(public_sub) == 'public_density'] <- 'density'
sub <-rbind(taxi_sub, public_sub)

gghist <- ggplot(sub, aes(x=density, color=type, fill=type)) +
  geom_histogram(position="identity", alpha=0.4,binwidth =4)+
  labs(x="Number of Charging Points",
       y="Frequency")+
  theme_classic()+
  theme(plot.title = element_text(hjust =0.5),
        legend.title = element_text(size =20),
        legend.text = element_text(size = 15),
        axis.title.x =element_text(size=15), 
        axis.title.y=element_text(size=15),)
gghist

## Charging Site's Point Pattern Analysis
# Ripley’s K Tests
# Do Ripley’s K Tests for both data sets to see if there is any cluster in charging sites
# Set a window as the borough boundary
window <- as.owin(LondonBoroughs)
plot(window)

# Ripley’s K Test on Charging Sites for Public Use in London
public<- public %>%
  as(., 'Spatial')
public.ppp <- ppp(x=public@coords[,1],
                          y=public@coords[,2],
                          window=window)
public.ppp %>%
  plot(.,pch=16,cex=0.5, 
       main="Charging points for public use in London")
K <- public.ppp %>%
  Kest(., correction="border") %>%
  plot()
# Ripley’s K Test on Charging Sites for Taxi in London
taxi<- taxi %>%
  as(., 'Spatial')
taxi.ppp <- ppp(x=taxi@coords[,1],
                  y=taxi@coords[,2],
                  window=window)
taxi.ppp %>%
  plot(.,pch=16,cex=0.5, 
       main="Pubilc charging points in London")
K <- taxi.ppp %>%
  Kest(., correction="border") %>%
  plot()

## Global Spatial Autocorrelation analysis
# Generate a spatial weights matrix using nearest k-nearest neighbours case
knn_boros <-coordsW %>%
  knearneigh(., k=4) #create a neighbours list of nearest k-nearest neighbours (k=4)
boro_knn <- knn_boros %>%
  knn2nb() %>%
  nb2listw(., style="C")

# Analysing Spatial Autocorrelation for both charging point density data set
# Moran’s I test (tells us whether we have clustered values (close to 1) or dispersed values (close to -1))
# For taxi points' density
I_global_taxi <- LondonBoroughs %>%
  pull(taxi_density) %>%
  as.vector()%>%
  moran.test(., boro_knn)
I_global_taxi
# For public use points' density 
I_global_public <- LondonBoroughs %>%
  pull(public_density) %>%
  as.vector()%>%
  moran.test(., boro_knn)
I_global_public

# Geary’s C test (This tells us whether similar values or dissimilar values are cluserting)
# For taxi points' density
C_global_taxi <- 
  LondonBoroughs %>%
  pull(taxi_density) %>%
  as.vector()%>%
  geary.test(., boro_knn)
C_global_taxi

# For public use points' density
C_global_public <- 
  LondonBoroughs %>%
  pull(public_density) %>%
  as.vector()%>%
  geary.test(., boro_knn)
C_global_public

# Getis Ord G test(This tells us whether high or low values are clustering.)
# For taxi points' density
G_global_taxi <- 
  LondonBoroughs %>%
  pull(taxi_density) %>%
  as.vector()%>%
  globalG.test(., boro_knn)
G_global_taxi
# For public use points' density
G_global_public <- 
  LondonBoroughs %>%
  pull(public_density) %>%
  as.vector()%>%
  globalG.test(., boro_knn)
G_global_public


## Local Moran’s I test
# Calculate the Local Moran’s I satistics for taxi charging point density data
I_local_taxi <- LondonBoroughs %>%
  pull(taxi_density) %>%
  as.vector()%>%
  localmoran(., boro_knn)%>%
  as_tibble()
LondonBoroughs_test <- LondonBoroughs %>%
  mutate(density_I =as.numeric(I_local_taxi$Ii))%>%
  mutate(density_Iz =as.numeric(I_local_taxi$Z.Ii))
# Plot a map of the local Moran’s I 
# Set the breaks and color bar manually
breaks1<-c(-1000,-2.58,-1.96,-1.65,1.65,1.96,2.58,1000)
MoranColours<- rev(brewer.pal(8, "RdGy"))
# Local Moran’s I test z-score map
tmap_mode("plot")
zscore_taxi <- tm_shape(LondonBoroughs_test) +
  tm_polygons("density_Iz",
              style="fixed",
              breaks=breaks1,
              palette=MoranColours,
              midpoint=NA,
              title="Z-score")+
  tm_layout(frame=FALSE,
            legend.position = c("right","bottom"), 
            legend.text.size=1, 
            legend.title.size = 1.5)+
  tm_scale_bar(text.size=0.85,position=c(0,0.03))+
  tm_compass(north=0,position=c(0.03,0.12),size=3.5,text.size = 1)+
  tm_credits("(c) Greater London Authority",position=c(0,0),size = 1)
zscore_taxi





