library(sf)
library(tmap)
library(leaflet)

#Load Data
hospitals <- st_read("/Users/bobbybuyalos/Downloads/Assignment3/Hospitals_data/Hospitals.shp")
parks <- st_read("/Users/bobbybuyalos/Downloads/Assignment3/Parks - Chicago Park District Facilities (current)/geo_export_26a375f6-91bc-450f-b789-b246d22d2ed1.shp")
stores <- read.csv("/Users/bobbybuyalos/Downloads/Assignment3/Grocery_Stores_-_2013.csv")
CommAreas <- st_read("/Users/bobbybuyalos/Downloads/Assignment3/Neighborhoods_2012/Neighborhoods_2012b.shp")

#Clean Parks Data
parks <- parks %>% 
  distinct(park, .keep_all = TRUE)

#Standardize CRS
CRS.new <- st_crs(CommAreas)
hospitals <- st_transform(hospitals, CRS.new)
parks <- st_transform(parks, CRS.new)

#Spatially Enable Stores Data
stores_new <- st_as_sf(stores, coords = c("X.COORDINATE","Y.COORDINATE"), crs = st_crs(CommAreas))

#PIP Parks
parks_in_comm <- st_join(parks, CommAreas, join = st_within)
parks_comm_count <- as.data.frame(table(parks_in_comm$SEC_NEIGH))
names(parks_comm_count) <- c("SEC_NEIGH","ParkCt")
Parks_New <- merge(CommAreas, parks_comm_count, by="SEC_NEIGH")

#PIP Hospital
hosp_in_comm <- st_join(hospitals, CommAreas, join = st_within)
hosp_comm_count <- as.data.frame(table(hosp_in_comm$SEC_NEIGH))
names(hosp_comm_count) <- c("SEC_NEIGH","HospCt")
Hosp_New <- merge(CommAreas, hosp_comm_count, by="SEC_NEIGH")

#PIP Stores
store_in_comm <- st_join(stores_new, CommAreas, join = st_within)
store_comm_count <- as.data.frame(table(store_in_comm$SEC_NEIGH))
names(store_comm_count) <- c("SEC_NEIGH","StoreCt")
Store_New <- merge(CommAreas, store_comm_count, by="SEC_NEIGH")

#Create binary count for all three
CommArea_counts <- CommAreas %>%
  mutate(park_counts = ifelse(SEC_NEIGH %in% Parks_New$SEC_NEIGH, 1, 0)) %>%
  mutate(hospital_counts = ifelse(SEC_NEIGH %in% Hosp_New$SEC_NEIGH, 1, 0)) %>%
  mutate(store_counts = ifelse(SEC_NEIGH %in% Store_New$SEC_NEIGH, 1, 0))

#Sum them for the index value
CommArea_counts <- CommArea_counts %>%
  mutate(total_counts = park_counts + hospital_counts + store_counts)

                    ### Visualization ###


## Point Data (Not included - busy and no value-add) ##
#tm_shape(CommAreas) + tm_borders(alpha = 0.4) + 
  #tm_shape(parks) + tm_dots(size = 0.1, shape = 17, col= "darkgreen") +
  #tm_shape(stores_new) + tm_dots(size = 0.1, col="#C6A2E1") +
  #tm_shape(hospitals) + tm_dots(size = 0.2, shape = 15, col="red") +
  #tm_layout(
    #main.title = "Parks by Community Area",
    #title.size = 1.2) +
  #tm_scale_bar(position = c("right", "top"), color.dark = "royalblue",
               #color.light = "orange") +
  #tm_credits("Created by Bobby Buyalos (2023). Data accessed through the Chicago Data Portal.", size = .4, align = "right", position = "center")


#Parks Map
Parks_Map <- tm_shape(CommAreas) + tm_borders(alpha = 0.3) + tm_fill(col = "grey90") +
tm_shape(Parks_New) +
  tm_polygons("ParkCt", style = "equal", n=5, pal = "Greens") +
  tm_layout(
    main.title = "Parks by Community Area",
    title.size = 1.2,
    legend.title.size = .01,
    legend.text.size = 1.6,
    legend.position = c("left", "bottom")) +
  tm_scale_bar(position = c("right", "top"), size = .8, color.dark = "darkgreen",
               color.light = "palegreen") +
  tm_credits("Created by Bobby Buyalos (2023). Data accessed through the Chicago Data Portal.", size = .66, align = "right", position = "center") 
tmap_save(Parks_Map, filename = "Parks_Map.png", width = 2550, height = 3300, dpi = 300)


#Stores Map
Stores_Map <- tm_shape(CommAreas) + tm_borders(alpha = 0.3) + tm_fill(col = "grey90") +
tm_shape(Store_New) +
  tm_polygons("StoreCt", style = "equal", n=5, pal = "Purples") +
  tm_layout(
    main.title = "Grocery Stores by Community Area",
    title.size = 1.2,
    legend.title.size = .01,
    legend.text.size = 1.6,
    legend.position = c("left", "bottom")) +
  tm_scale_bar(position = c("right", "top"), size = .8, color.dark = "#800080",
               color.light = "lavender") +
  tm_credits("Created by Bobby Buyalos (2023). Data accessed through the Chicago Data Portal.", size = .66, align = "right", position = "center")
tmap_save(Stores_Map, filename = "Stores_Map.png", width = 2550, height = 3300, dpi = 300)


#Hospital Map
Hospital_Map <- tm_shape(CommAreas) + tm_borders(alpha = 0.3) + tm_fill(col = "grey90") +
tm_shape(Hosp_New) +
  tm_polygons("HospCt", style = "equal", n=5, pal = "Reds") +
  tm_layout(
    main.title = "Hospitals by Community Area",
    title.size = 1.2,
    legend.title.size = .01,
    legend.text.size = 1.6,
    legend.position = c("left", "bottom")) +
  tm_scale_bar(position = c("right", "top"), size = .8, color.dark = "#8B0000",
               color.light = "#FFDAB9") +
  tm_credits("Created by Bobby Buyalos (2023). Data accessed through the Chicago Data Portal.", size = .66, align = "right", position = "center")
tmap_save(Hospital_Map, filename = "Hospital_Map.png", width = 2550, height = 3300, dpi = 300)

#Accessibility Index Map
colnames(CommArea_counts)[ncol(CommArea_counts)] <- "Index Values"
Acc_indx_Map <- tm_shape(CommAreas) + tm_borders(alpha = 0.3) + tm_fill(col = "grey90") +
  tm_shape(CommArea_counts) +
  tm_polygons("Index Values", style = "cat", n = 4, pal = "Blues") +
  tm_layout(
    main.title = "Accessibility Index by Community Area",
    title.size = 1.2,
    legend.title.size = 1.6,
    legend.text.size = 1.6,
    legend.position = c("left", "bottom"))  +
  tm_scale_bar(position = c("right", "top"), size = .8, color.dark = "royalblue",
               color.light = "turquoise") +
  tm_credits("Created by Bobby Buyalos (2023). Data accessed through the Chicago Data Portal.", size = .66, align = "right", position = "center")
tmap_save(Acc_indx_Map, filename = "Acc_indx_Map.png", width = 2550, height = 3300, dpi = 300)



