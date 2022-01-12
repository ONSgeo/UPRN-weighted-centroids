library(tidyverse)
library(sf)
library(nngeo)

#load in UPRN files and headers
uprn1 <- read_csv("S:/Chris_Gale/SOA_UPRN_Centroids/AddressBasePlus_ISL_FULL_2021-10-13_001.csv", col_names = FALSE)
uprn2 <- read_csv("S:/Chris_Gale/SOA_UPRN_Centroids/AddressBasePlus_ISL_FULL_2021-10-13_002.csv", col_names = FALSE)

headers <- read.csv("S:/Chris_Gale/SOA_UPRN_Centroids/AddressBasePlus_Header.csv")
headers_vec <- colnames(headers)

#join together UPRN files and add headers
uprn <- rbind(uprn1, uprn2)
colnames(uprn) <- headers_vec

#remove unused files
rm(uprn1)
rm(uprn2)
rm(headers)
rm(headers_vec)

#extract residential classification UPRNs
residential <- c("RD", "RD01", "RD02", "RD03", "RD04", "RD06", "RD07", "RD08", "RD10", "RG02", "RH", "RH01", "RH02", "RH03", "RI", "RI01", "RI02", "RI02NC", "RI02RC", "RI03")

residential_uprn <- filter(uprn, CLASS %in% residential)

#make a spatial version of residential UPRNs.
#extracting the coords in tm65 (29902) for use later
res_uprn_sf <- residential_uprn %>% select(UPRN, LATITUDE, LONGITUDE, CLASS, X_COORDINATE, Y_COORDINATE) %>% 
                st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = 4258) %>% #coords in lat/lon column are in ETRS89
                st_transform(29902) %>% 
                mutate(NORTHING_TM65 = st_coordinates(.)[,2],
                       EASTING_TM65 = st_coordinates(.)[,1])


#PIP to find what SOA each UPRN is in
soa <- st_read("data/SOA2011.shp") #crs is 29902 TM65 Irish Grid

res_uprn_sf <- st_join(res_uprn_sf, soa, join = st_intersects)
#turns out there are lots of non-joins here because the IoM is included in this dataset too... so filter.

res_uprn_sf <- res_uprn_sf %>% filter(., is.na(res_uprn_sf$SOA_CODE) == FALSE)


res_uprn_centroids <- res_uprn_sf %>% st_drop_geometry() %>% 
                            group_by(SOA_CODE) %>% 
                            summarise(MEAN_X_TM65 = mean(EASTING_TM65),
                                      MEAN_Y_TM65 = mean(NORTHING_TM65))



#check centroid falls within expected SOA
res_uprn_centroids_sf <- st_as_sf(res_uprn_centroids, coords = c("MEAN_X_TM65", "MEAN_Y_TM65"), crs = 29902)

centroid_check <- st_join(res_uprn_centroids_sf, soa, join = st_intersects)

centroid_check <- mutate(centroid_check, CHECK = ifelse(SOA_CODE.y == SOA_CODE.x, 1, 0))



#snap centroids which fall outside the correct SOA to the nearest UPRN which falls within the correct SOA.
needs_snapping <- filter(centroid_check, CHECK == 0)

#calculate nearest neighbours for each centroid which needs to be snapped - get a few of them because the first ones might not be in the right SOA
results <- st_nn(needs_snapping, res_uprn_centroids_sf, k = 10)



#unpack the list

incorrect_centroid <- needs_snapping[1, ]
position <- results[[1]][1]
snap_uprn <- centroid_check[position, ]




