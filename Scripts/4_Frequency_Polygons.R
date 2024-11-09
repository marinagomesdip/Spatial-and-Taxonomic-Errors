#Chapter two - thesis 
#Mean Maps and Graphs
#Marina Morim Gomes 2023
#gomes.mari.95@gmail.com

#loading packages ----------------------------------------------------------
library(tidyverse)
library(raster)
library(sf)
library(tmap)

#loading data ------------------------------------------------------------------
occs <-  read_csv("./Data/Processados/6_para_rodar_modelagem.csv")     #occurrence point
#Creating a species list to run the loops -----------------------------------
study_sp <- c("Oxysarcodexia amorosa", "Oxysarcodexia xanthosoma",
              "Lipoptilocnema crispula", "Lipoptilocnema crispina")


#MEAN AND SD MAPS OF ERRORS --------------------------------------------------

#creating a mean and sd map for error1 (spacial)
for(i in 1:length(study_sp)){

#Read the model raster
model1 <- raster(paste0("./cap2/", study_sp[i], "_1/present/final_models/", study_sp[i], "_1_maxent_raw_mean.tif"))
model2 <- raster(paste0("./cap2/", study_sp[i], "_2/present/final_models/", study_sp[i], "_2_maxent_raw_mean.tif"))
model3 <- raster(paste0("./cap2/", study_sp[i], "_3/present/final_models/", study_sp[i], "_3_maxent_raw_mean.tif"))
model4 <- raster(paste0("./cap2/", study_sp[i], "_4/present/final_models/", study_sp[i], "_4_maxent_raw_mean.tif"))
model5 <- raster(paste0("./cap2/", study_sp[i], "_5/present/final_models/", study_sp[i], "_5_maxent_raw_mean.tif"))
model6 <- raster(paste0("./cap2/", study_sp[i], "_6/present/final_models/", study_sp[i], "_6_maxent_raw_mean.tif"))
model7 <- raster(paste0("./cap2/", study_sp[i], "_7/present/final_models/", study_sp[i], "_7_maxent_raw_mean.tif"))
model8 <- raster(paste0("./cap2/", study_sp[i], "_8/present/final_models/", study_sp[i], "_8_maxent_raw_mean.tif"))
model9 <- raster(paste0("./cap2/", study_sp[i], "_9/present/final_models/", study_sp[i], "_9_maxent_raw_mean.tif"))
model10 <- raster(paste0("./cap2/", study_sp[i], "_10/present/final_models/", study_sp[i], "_10_maxent_raw_mean.tif"))

#calculating media of models
media <- mean(model1, model2, model3, model4, model5, model6, model7, model8, model9, model10)

#saving raster
writeRaster(media, filename = paste0("./cap2/media/", study_sp[i], "_error1media.tif"), format = "GTiff")

#calculating sd for models
#stacking raster
model_rasters <- list(model1, model2, model3, model4, model5, model6, model7, model8, model9, model10)
stacked_rasters <- stack(model_rasters)

#calculating sd for each grid cell
desvio_padrao <- calc(stacked_rasters, sd, na.rm = TRUE)

#saving raster
writeRaster(desvio_padrao, filename = paste0("./cap2/media/", study_sp[i], "_error1desvio.tif"), format = "GTiff")
}

#creating a mean and sd map for error 2 (taxonomic)
for(i in 1:length(study_sp)){
  
  #Read the model raster
  model1 <- raster(paste0("./cap2/", study_sp[i], "_11/present/final_models/", study_sp[i], "_11_maxent_raw_mean.tif"))
  model2 <- raster(paste0("./cap2/", study_sp[i], "_12/present/final_models/", study_sp[i], "_12_maxent_raw_mean.tif"))
  model3 <- raster(paste0("./cap2/", study_sp[i], "_13/present/final_models/", study_sp[i], "_13_maxent_raw_mean.tif"))
  model4 <- raster(paste0("./cap2/", study_sp[i], "_14/present/final_models/", study_sp[i], "_14_maxent_raw_mean.tif"))
  model5 <- raster(paste0("./cap2/", study_sp[i], "_15/present/final_models/", study_sp[i], "_15_maxent_raw_mean.tif"))
  model6 <- raster(paste0("./cap2/", study_sp[i], "_16/present/final_models/", study_sp[i], "_16_maxent_raw_mean.tif"))
  model7 <- raster(paste0("./cap2/", study_sp[i], "_17/present/final_models/", study_sp[i], "_17_maxent_raw_mean.tif"))
  model8 <- raster(paste0("./cap2/", study_sp[i], "_18/present/final_models/", study_sp[i], "_18_maxent_raw_mean.tif"))
  model9 <- raster(paste0("./cap2/", study_sp[i], "_19/present/final_models/", study_sp[i], "_19_maxent_raw_mean.tif"))
  model10 <- raster(paste0("./cap2/", study_sp[i], "_20/present/final_models/", study_sp[i], "_20_maxent_raw_mean.tif"))
  
  media <- mean(model1, model2, model3, model4, model5, model6, model7, model8, model9, model10)
  
  writeRaster(media, filename = paste0("./cap2/media/", study_sp[i], "_error2media.tif"), format = "GTiff")
  
  #calculating sd for models
  #stacking raster
  model_rasters <- list(model1, model2, model3, model4, model5, model6, model7, model8, model9, model10)
  stacked_rasters <- stack(model_rasters)
  
  #calculating sd for each grid cell
  desvio_padrao <- calc(stacked_rasters, sd, na.rm = TRUE)
  
  #saving raster
  writeRaster(desvio_padrao, filename = paste0("./cap2/media/", study_sp[i], "_error2desvio.tif"), format = "GTiff")
}

#MEAN AUC AND TSS ---------------------------------------------------------

#creating mean statistics for error1
for(i in 1:length(study_sp)){
  
  #Read the statistics for every partition
  stat1 <- read_csv(paste0("./cap2/", study_sp[i], "_1/present/final_models/", study_sp[i], "_1_mean_statistics.csv"))
  stat2 <- read_csv(paste0("./cap2/", study_sp[i], "_2/present/final_models/", study_sp[i], "_2_mean_statistics.csv"))
  stat3 <- read_csv(paste0("./cap2/", study_sp[i], "_3/present/final_models/", study_sp[i], "_3_mean_statistics.csv"))
  stat4 <- read_csv(paste0("./cap2/", study_sp[i], "_4/present/final_models/", study_sp[i], "_4_mean_statistics.csv"))
  stat5 <- read_csv(paste0("./cap2/", study_sp[i], "_5/present/final_models/", study_sp[i], "_5_mean_statistics.csv"))
  stat6 <- read_csv(paste0("./cap2/", study_sp[i], "_6/present/final_models/", study_sp[i], "_6_mean_statistics.csv"))
  stat7 <- read_csv(paste0("./cap2/", study_sp[i], "_7/present/final_models/", study_sp[i], "_7_mean_statistics.csv"))
  stat8 <- read_csv(paste0("./cap2/", study_sp[i], "_8/present/final_models/", study_sp[i], "_8_mean_statistics.csv"))
  stat9 <- read_csv(paste0("./cap2/", study_sp[i], "_9/present/final_models/", study_sp[i], "_9_mean_statistics.csv"))
  stat10 <- read_csv(paste0("./cap2/", study_sp[i], "_10/present/final_models/", study_sp[i], "_10_mean_statistics.csv"))

  #Add the list to the global object
  stat_list <- list(stat1, stat2, stat3, stat4, stat5, stat6, stat7, stat8, stat9, stat10)

  #Combine all dataframes from the list into a single dataframe
df_combined <- bind_rows(stat_list)

 #Calculate the mean by group and select the necessary columns
df_final <- df_combined %>%
  dplyr::select(-1) %>%
  separate(species_name, into = c("species_name", "number"), sep = "_") %>%
  dplyr::select(-number) %>%
  group_by(species_name, algorithm, dismo_threshold) %>%
  summarise(
    spec_sense = mean(spec_sens),
    AUC = mean(AUC),
    TSS = mean(TSSmax)
  ) %>%
  ungroup()
  
  #saving csv
  write_csv(df_final, file = paste0("./cap2/estatisticas/", study_sp[i], "_error1media.csv"))
}

#creating mena statistics for error2
for(i in 1:length(study_sp)){
  
  #Read the statistics for every partition
  stat1 <- read_csv(paste0("./cap2/", study_sp[i], "_11/present/final_models/", study_sp[i], "_11_mean_statistics.csv"))
  stat2 <- read_csv(paste0("./cap2/", study_sp[i], "_12/present/final_models/", study_sp[i], "_12_mean_statistics.csv"))
  stat3 <- read_csv(paste0("./cap2/", study_sp[i], "_13/present/final_models/", study_sp[i], "_13_mean_statistics.csv"))
  stat4 <- read_csv(paste0("./cap2/", study_sp[i], "_14/present/final_models/", study_sp[i], "_14_mean_statistics.csv"))
  stat5 <- read_csv(paste0("./cap2/", study_sp[i], "_15/present/final_models/", study_sp[i], "_15_mean_statistics.csv"))
  stat6 <- read_csv(paste0("./cap2/", study_sp[i], "_16/present/final_models/", study_sp[i], "_16_mean_statistics.csv"))
  stat7 <- read_csv(paste0("./cap2/", study_sp[i], "_17/present/final_models/", study_sp[i], "_17_mean_statistics.csv"))
  stat8 <- read_csv(paste0("./cap2/", study_sp[i], "_18/present/final_models/", study_sp[i], "_18_mean_statistics.csv"))
  stat9 <- read_csv(paste0("./cap2/", study_sp[i], "_19/present/final_models/", study_sp[i], "_19_mean_statistics.csv"))
  stat10 <- read_csv(paste0("./cap2/", study_sp[i], "_20/present/final_models/", study_sp[i], "_20_mean_statistics.csv"))
  
  #Add the list to the global object
  stat_list <- list(stat1, stat2, stat3, stat4, stat5, stat6, stat7, stat8, stat9, stat10)
  
  #Combine all dataframes from the list into a single dataframe
  df_combined <- bind_rows(stat_list)
  
  #Calculate the mean by group and select the necessary columns
  df_final <- df_combined %>%
    dplyr::select(-1) %>%
    separate(species_name, into = c("species_name", "number"), sep = "_") %>%
    dplyr::select(-number) %>%
    group_by(species_name, algorithm, dismo_threshold) %>%
    summarise(
      spec_sense = mean(spec_sens),
      AUC = mean(AUC),
      TSS = mean(TSSmax)
    ) %>%
    ungroup()
  
  #saving csv
  write_csv(df_final, file = paste0("./cap2/estatisticas/", study_sp[i], "_error2media.csv"))
}



#FREQUENCY POLYGONS -------------------------------------------------------------------

for(i in 1:length(study_sp)){
  
  #Reading the SDM maps with the original data, error1, and error2
  origi <- raster(paste0("./cap2/", study_sp[i], "/present/final_models/", study_sp[i], "_maxent_raw_mean.tif"))
  erro1 <- raster(paste0("./cap2/media/", study_sp[i], "_error1media.tif"))
  erro2 <- raster(paste0("./cap2/media/", study_sp[i], "_error2media.tif"))
 
  #Organizing original points df
  occso <- occs %>%
    filter(str_detect(nome_cientifico, paste0(study_sp[i]))) %>%
    filter(is.na(erro)) %>%
    unite(coord, latitude, longitude, sep = ",", remove = FALSE)
  
  #Organizing original erro1 df
  occse1 <- occs %>%
    filter(str_detect(nome_cientifico, paste0(study_sp[i]))) %>%
    filter(is.na(erro) | erro == "sim") %>%
    separate(nome_cientifico, into = c("species", "numero"), sep = "_") %>%
    mutate(numero2 = as.numeric(numero)) %>%
    filter(is.na(numero) | numero2 <= 10) %>%
    dplyr::select(-numero, -numero2) %>%
    unite(coord, latitude, longitude, sep = ",", remove = FALSE)
  
  #Organizing original erro2 df
  occse2 <- occs %>%
    filter(str_detect(nome_cientifico, paste0(study_sp[i]))) %>%
    filter(is.na(erro) | erro == "sim") %>%
    separate(nome_cientifico, into = c("species", "numero"), sep = "_") %>%
    mutate(numero2 = as.numeric(numero)) %>%
    filter(is.na(numero) | numero2 > 10) %>%
    dplyr::select(-numero, -numero2) %>%
    unite(coord, latitude, longitude, sep = ",", remove = FALSE)
 
  #Extracting values from original points
  occso_sf <- st_as_sf(occso, coords = c("longitude", "latitude"), crs = st_crs(origi))
  valores_raster <- extract(origi, occso_sf)
  occso$valores_raster <- valores_raster
  occso <- occso %>%
    mutate(erro = ifelse(is.na(erro), "nao", erro))
  
  #Extracting values from original points
  occse1_sf <- st_as_sf(occse1, coords = c("longitude", "latitude"), crs = st_crs(erro1))
  valores_raster <- extract(erro1, occse1_sf)
  occse1$valores_raster <- valores_raster
  occse1 <- occse1 %>%
    mutate(erro = ifelse(is.na(erro), "nao", erro))
  
  #Saving the results
  write_csv(occse1, file = paste0("./cap2/estatisticas/", study_sp[i], "_valorrastererro1.csv"))
  
  #Extracting values from original points
  occse2_sf <- st_as_sf(occse2, coords = c("longitude", "latitude"), crs = st_crs(erro2))
  valores_raster <- extract(erro2, occse2_sf)
  occse2$valores_raster <- valores_raster
  occse2 <- occse2 %>%
    mutate(erro = ifelse(is.na(erro), "nao", erro))
  
  #Saving the results
  write_csv(occse2, file = paste0("./cap2/estatisticas/", study_sp[i], "_valorrastererro2.csv"))
  
  #Frequency poligon from original points
  origig <- ggplot(occso, aes(valores_raster, colour = erro)) +
    geom_freqpoly(binwidth = 0.1, size = 1.7) +
    scale_color_manual(values = c("darkgray","darkorange")) +
    theme_classic() +
    labs(x = "suitability of occurrence points") +
    ylim(0, 15) +
    xlim(0, 1)
  
  #saving this plot
  ggsave(filename = paste0("./cap2/histogramas/", study_sp[i], "_origihist.png"),
         plot = origig, device = "png", width = 10, height = 8, dpi = 300)
  
  #Frequency poligon from erro1 points
  erro1g <- ggplot(occse1, aes(valores_raster, colour = erro)) +
      geom_freqpoly(binwidth = 0.1, size = 1.7) +
      scale_color_manual(values = c("darkgray","darkorange")) +
      theme_classic() +
      labs(x = "suitability of occurrence points") +
      ylim(0, 15) +
      xlim(0, 1)
  
  #saving this plot
  ggsave(filename = paste0("./cap2/histogramas/", study_sp[i], "_error1hist.png"),
         plot = erro1g, device = "png", width = 10, height = 8, dpi = 300)
  
  #Frequency poligon from erro2 points
  erro2g <- ggplot(occse2, aes(valores_raster, colour = erro)) +
    geom_freqpoly(binwidth = 0.1, size = 1.7) +
    scale_color_manual(values = c("darkgray","darkorange")) +
    theme_classic() +
    labs(x = "suitability of occurrence points") +
    ylim(0, 15) + 
    xlim(0, 1)
  
  #saving this plot
  ggsave(filename = paste0("./cap2/histogramas/", study_sp[i], "_error2hist.png"),
         plot = erro2g, device = "png", width = 10, height = 8, dpi = 300)
  
}