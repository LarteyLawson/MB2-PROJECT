
#plotting DEM
m <- leaflet() %>%
  addTiles() %>%
  addRasterImage(DEM) %>%
  
  
  ggplot() +
  annotation_map_tile(zoomin = -1) +
  layer_spatial(DEM)


# basemap_magick( map_service = "esri", map_type = "world_imagery")
plot(DEM)



GalleryT <- c(T1, T2, T3, T4)
MImagery <- mosaic(T1, T2, T3, T4, c(-0.05,0.05,5.3,5.7), fun=mean())


gallery <- c(1:4)
Dfiles <- list.files("C:/Users/lynes/Desktop/RFinal Proj")
for(i in 1: length(Dfiles)){
  assign(paste0("*.jpg", gallery[i]))
}