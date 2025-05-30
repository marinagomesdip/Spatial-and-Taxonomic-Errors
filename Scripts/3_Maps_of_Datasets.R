#Chapter two - thesis 
#Maps with simullated errors
#Marina Morim Gomes 2023
#gomes.mari.95@gmail.com

#loading packages -------------------------------------
library(tidyverse)
library(raster)
library(sf)
library(tmap)

#loading data -------------------------------------
occs <- read_csv("./Data/Processados/6_para_rodar_modelagem.csv")   #occurrence data

shape <- "./Shapefiles/América"                                     #America shapefile
america <- sf::st_read(dsn = file.path(shape, 
                                  layer = "América.shp")) 



#looping for maps with error datasets --------------------------

#Setting a function to use in loop
  generate_map_error <- function(species_name, occs, america, base_path) {
    #Path to model raster
    caminho_arquivo <- paste0(base_path, "/", species_name, "/present/final_models/", species_name, "_maxent_raw_mean.tif")
    
    #Import model raster 
    model <- raster(caminho_arquivo)
    
    #Cropping extention to reduce the maps
    nova_extensao <- extent(-115, -30, -35, 40)
    
    #Cropping in target region
    model <- crop(model, nova_extensao)
    
    #Filtering occurrences to the atual dataset of model
    species <- occs %>%
      dplyr::filter(nome_cientifico == species_name) %>%
      st_as_sf(coords = c("longitude", "latitude"), remove = FALSE)
    
    #Adding geometry to the occurrences 
    species <- st_set_crs(species, 4326)
    
    #Filtering only erroneous occurrences
    specieserror <- species %>%
      dplyr::filter(erro == "sim")
    
    #Creating a map with tmap package
    map <- tm_shape(model) +                        #selecting the object with raster
      tm_raster(palette = "PuBu",                   #palette to choose the color palette from RColorBrewer
                style = "cont",                     #this argument is necessary for continuous rasters
                alpha = 0.8,                        #transparency
                title = "Suitability") +            #legend title
      tm_layout(frame.lwd = 3,
                legend.position = c("left", "bottom")) + #legend position
      tm_scale_bar(position = c("right", "top"),    #scale bar position
                   width = 0.15,                    #scale bar size
                   color.dark = "gray44")           #scale bar color
    
    map2 <- map +                                   #calling the previous map
      tm_shape(specieserror) +                      #selecting the raster object of erroneous point
      tm_symbols(border.col = "red",                #adding a red circle around occurrence point
                 border.lwd = 1,                    #line thickness
                 alpha = 0)                         #transparency
    
    map3 <- map2 +                                  #calling the previous map
      tm_shape(species) +                           #selecting the raster object of erroneous point
      tm_dots(size = 0.1)                           #adding points with the original 
    
    map4 <-map3 +                                   #calling the previous map
      tm_shape(america) +                           #selecting the raster shape of america
      tm_borders(col = "black")                     #color of line borders 
 
     #saving the map
      tmap_save(                                      
      tm = map4,                                                        #object with map 
      filename = paste0(base_path, "/maps/", species_name, "_map.png"), #file name and path
      width = 3000,                                                     #largura
      height = 2800                                                     #altura
    )
  }
  
  #List of species in the study with erroneous point
  study_sp <- c("Oxysarcodexia amorosa_1", "Oxysarcodexia amorosa_2", "Oxysarcodexia amorosa_3", "Oxysarcodexia amorosa_4", "Oxysarcodexia amorosa_5",
                "Oxysarcodexia amorosa_6", "Oxysarcodexia amorosa_7", "Oxysarcodexia amorosa_8", "Oxysarcodexia amorosa_9",
                "Oxysarcodexia amorosa_10", "Oxysarcodexia amorosa_11", "Oxysarcodexia amorosa_12", "Oxysarcodexia amorosa_13",
                "Oxysarcodexia amorosa_14", "Oxysarcodexia amorosa_15", "Oxysarcodexia amorosa_16", "Oxysarcodexia amorosa_17",
                "Oxysarcodexia amorosa_18", "Oxysarcodexia amorosa_19", "Oxysarcodexia amorosa_20", "Oxysarcodexia xanthosoma_1",
                "Oxysarcodexia xanthosoma_2", "Oxysarcodexia xanthosoma_3", "Oxysarcodexia xanthosoma_4", 
                "Oxysarcodexia xanthosoma_5", "Oxysarcodexia xanthosoma_6", "Oxysarcodexia xanthosoma_7", 
                "Oxysarcodexia xanthosoma_8", "Oxysarcodexia xanthosoma_9", "Oxysarcodexia xanthosoma_10",
                "Oxysarcodexia xanthosoma_11", "Oxysarcodexia xanthosoma_12", "Oxysarcodexia xanthosoma_13",
                "Oxysarcodexia xanthosoma_14", "Oxysarcodexia xanthosoma_15", "Oxysarcodexia xanthosoma_16",
                "Oxysarcodexia xanthosoma_17", "Oxysarcodexia xanthosoma_18", "Oxysarcodexia xanthosoma_19",
                "Oxysarcodexia xanthosoma_20", "Lipoptilocnema crispula_1", "Lipoptilocnema crispula_2", "Lipoptilocnema crispula_3",
                "Lipoptilocnema crispula_4", "Lipoptilocnema crispula_5", "Lipoptilocnema crispula_6", "Lipoptilocnema crispula_7",
                "Lipoptilocnema crispula_8", "Lipoptilocnema crispula_9", "Lipoptilocnema crispula_10", "Lipoptilocnema crispula_11",
                "Lipoptilocnema crispula_12", "Lipoptilocnema crispula_13", "Lipoptilocnema crispula_14", "Lipoptilocnema crispula_15",
                "Lipoptilocnema crispula_16", "Lipoptilocnema crispula_17", "Lipoptilocnema crispula_18", "Lipoptilocnema crispula_19",
                "Lipoptilocnema crispula_20", "Lipoptilocnema crispina_1", "Lipoptilocnema crispina_2", "Lipoptilocnema crispina_3",
                "Lipoptilocnema crispina_4", "Lipoptilocnema crispina_5", "Lipoptilocnema crispina_6", "Lipoptilocnema crispina_7",
                "Lipoptilocnema crispina_8", "Lipoptilocnema crispina_9", "Lipoptilocnema crispina_10", "Lipoptilocnema crispina_11",
                "Lipoptilocnema crispina_12", "Lipoptilocnema crispina_13", "Lipoptilocnema crispina_14", "Lipoptilocnema crispina_15",
                "Lipoptilocnema crispina_16", "Lipoptilocnema crispina_17", "Lipoptilocnema crispina_18", "Lipoptilocnema crispina_19",
                "Lipoptilocnema crispina_20")
  
  #Base path for the data
  base_path <- "./cap2"
  
  #Loop to generate the maps
  for (species_name in study_sp) {
    generate_map_error(species_name, occs, america, base_path)
  } 
  
#looping for maps without error datasets --------------------------

  #Setting a function to use in loop 
generate_map <- function(species_name, occs, america, base_path) {
 
  #Path to model raster 
  caminho_arquivo <- paste0(base_path, "/", species_name, "/present/final_models/", species_name, "_maxent_raw_mean.tif")
  
  #Import model raster 
  model <- raster(caminho_arquivo)
  
  #Cropping extention to reduce the maps
  nova_extensao <- extent(-115, -30, -35, 40)
  
  #Cropping in target region
  model <- crop(model, nova_extensao)
  
  #Filtering occurrences to the atual dataset of model
  species <- occs %>%
    dplyr::filter(nome_cientifico == species_name) %>%
    st_as_sf(coords = c("longitude", "latitude"), remove = FALSE)
  
  species <- st_set_crs(species, 4326)
  
  #Creating a map with tmap package
  map <- tm_shape(model) +
    tm_raster(palette = "PuBu", 
              style = "cont",
              alpha = 0.8,
              title = "Suitability") +
    tm_layout(frame.lwd = 3, legend.position = c("left", "bottom")) +
    tm_scale_bar(position = c("right", "top"), width = 0.15, color.dark = "gray44")
  
  map3 <- map +
    tm_shape(species) +
    tm_dots(size = 0.1)
  
  map4 <- map3 +
    tm_shape(america) +
    tm_borders(col = "black")
  
  #saving the map
  tmap_save(
    tm = map4, 
    filename = paste0(base_path, "/maps/", species_name, "_map.png"), 
    width = 3000, 
    height = 2800
  )
}

#List of species in the study with original points only
study_sp <- c("Oxysarcodexia amorosa", "Oxysarcodexia xanthosoma",
              "Lipoptilocnema crispula", "Lipoptilocnema crispina")

#Base path for the data
base_path <- "./cap2"

#Loop to generate the maps
for (species_name in study_sp) {
  generate_map(species_name, occs, america, base_path)
}