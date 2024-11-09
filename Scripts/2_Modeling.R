#Chapter two - thesis 
#Erros in data - Modelling
#Marina Morim Gomes 2023
#gomes.mari.95@gmail.com

#installing packages (optional) ---------------------------------
#install.packages("devtools")
#devtools::install_github("cjbwalsh/hier.part",force=TRUE)
#remotes::install_github("marlonecobos/kuenm",force=TRUE)
#remotes::install_github("mrmaxent/maxnet",force=TRUE)
#remotes::install_github("Model-R/modleR", build = TRUE,force=TRUE)
#install.packages("raster", dependencies = TRUE)
#install.packages("rJava")
#remotes::install_version("rgeos", version = "0.6-4")
#remotes::install_version("rgdal", version = "1.5-18")

#loading packages -------------------------------------
library(dplyr)
library(readr)
library(modleR)
library(raster)
library(rJava)
library(dismo)
library(sp)
library(sf)
library(rgdal)

#loading data ----------------------------------------
#occurrences
occ <- read.csv("./Data/Processados/6_para_rodar_modelagem.csv")
#environment layers
bio1 <- raster("./Enviromental_data/wc2.1_10m_bio_1.tif")
bio2 <- raster("./Enviromental_data/wc2.1_10m_bio_2.tif")
bio3 <- raster("./Enviromental_data/wc2.1_10m_bio_3.tif")
bio4 <- raster("./Enviromental_data/wc2.1_10m_bio_4.tif")
bio5 <- raster("./Enviromental_data/wc2.1_10m_bio_5.tif")
bio6 <- raster("./Enviromental_data/wc2.1_10m_bio_6.tif")
bio7 <- raster("./Enviromental_data/wc2.1_10m_bio_7.tif")
bio10 <- raster("./Enviromental_data/wc2.1_10m_bio_10.tif")
bio11 <- raster("./Enviromental_data/wc2.1_10m_bio_11.tif")
bio12 <- raster("./Enviromental_data/wc2.1_10m_bio_12.tif")
bio13 <- raster("./Enviromental_data/wc2.1_10m_bio_13.tif")
bio14 <- raster("./Enviromental_data/wc2.1_10m_bio_14.tif")
bio15 <- raster("./Enviromental_data/wc2.1_10m_bio_15.tif")
bio16 <- raster("./Enviromental_data/wc2.1_10m_bio_16.tif")
bio17 <- raster("./Enviromental_data/wc2.1_10m_bio_17.tif")

#preparing enviromental data to model --------------

#uniting maps in a rasterStack 
variables <- stack(bio1, bio2, bio3, bio4, bio5, bio6, bio7,bio10,
                   bio11, bio12, bio13, bio14, bio15, bio16, bio17)

#cropping in americas extent (all species occur only in America)
americas_extent <- extent(-115, -30, -60, 30)

variables <- crop(variables, americas_extent) %>%
  stack()

plot(variables$wc2.1_10m_bio_1)

#projections
# Define the target CRS
targetCRS <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")

#Project the raster stack
variables <- projectRaster(variables, crs = targetCRS)

#preparing occurs to loop-------------------------------
#Species list
study_sp <- c("Oxysarcodexia amorosa", "Oxysarcodexia xanthosoma",
              "Lipoptilocnema crispula", "Lipoptilocnema crispina", "Oxysarcodexia amorosa_1", "Oxysarcodexia amorosa_2", 
              "Oxysarcodexia amorosa_3", "Oxysarcodexia amorosa_4", "Oxysarcodexia amorosa_5",
              "Oxysarcodexia amorosa_6", "Oxysarcodexia amorosa_7", "Oxysarcodexia amorosa_8", "Oxysarcodexia amorosa_9",
              "Oxysarcodexia amorosa_10", "Oxysarcodexia amorosa_11", "Oxysarcodexia amorosa_12", "Oxysarcodexia amorosa_13",
              "Oxysarcodexia amorosa_14", "Oxysarcodexia amorosa_15", "Oxysarcodexia amorosa_16", "Oxysarcodexia amorosa_17",
              "Oxysarcodexia amorosa_18", "Oxysarcodexia amorosa_19", "Oxysarcodexia amorosa_20", 
              "Oxysarcodexia xanthosoma_1", "Oxysarcodexia xanthosoma_2", "Oxysarcodexia xanthosoma_3", "Oxysarcodexia xanthosoma_4", 
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
              "Lipoptilocnema crispula_20", "Lipoptilocnema crispina_1", "Lipoptilocnema crispina_2", "Lipoptilocnema crispina_3", "Lipoptilocnema crispina_4",
              "Lipoptilocnema crispina_5", "Lipoptilocnema crispina_6", "Lipoptilocnema crispina_7", "Lipoptilocnema crispina_8",
              "Lipoptilocnema crispina_9", "Lipoptilocnema crispina_10", "Lipoptilocnema crispina_11", "Lipoptilocnema crispina_12",
              "Lipoptilocnema crispina_13", "Lipoptilocnema crispina_14", "Lipoptilocnema crispina_15", "Lipoptilocnema crispina_16",
              "Lipoptilocnema crispina_17", "Lipoptilocnema crispina_18", "Lipoptilocnema crispina_19", "Lipoptilocnema crispina_20")


# name a folder to save outputs of this run
model_folder <- "./cap2"

# modleR 1/3: Setup data --------------------------------------------------

for(i in 1:length(study_sp)){
  # getting only occurrences for this species
  species_df <- occ[occ$nome_cientifico == study_sp[i], ]
  
  #running setup_sdmdata
  setup_sdmdata(species_name = study_sp[i],              #filtering the species name for this iteration
                occurrences = as.data.frame(species_df), #transforming the tible of occurrences in df
                predictors = variables,                  #name of raster stack with the variables
                lon = "longitude",                       #name of longitude column
                lat = "latitude",                        #name of latitude column
                models_dir = model_folder,               #folder to save partitions
                partition_type = "crossvalidation",      #crossvalidation is a way to partition data without replacement
                cv_partitions = 5,                       #number of parts that the occurrences will be divided into
                cv_n = 1,                                #how many times this partitions will be separed
                seed = 512,
                buffer_type = "mean",                    #Shows whether the buffer should be calculated, in this case, using the "mean" distance between occurrence points
                env_filter = TRUE,                       #filter used to exclude pseudoabsence from very close to the occurrences point
                env_distance = "centroid",               #type of enviromental distance, the distance of each raster pixel to the environmental centroid of the distribution
                min_env_dist = 0.4,                      #Sets a minimum value to exclude the areas closest (in the environmental space) to the occurrences or their centroid, expressed in quantiles, from 0 (the closest) to 1
                png_sdmdata = TRUE,                      #Save the data in a png file
                n_back = nrow(species_df) * 10,          #number of pseudoabsences (in this case, 10x number of occurrences)
                clean_dupl = TRUE,                       #removes points with the same longitude and latitude
                clean_uni = FALSE,                       #selects only one point per pixel
                clean_nas = TRUE,                        #removes points that are outside the bounds of the raster
                geo_filt = FALSE,                        #delete occurrences that are too close to each other
                select_variables = TRUE,                 #Whether a variable selection should be performed. It excludes highly correlated environmental variables
                sample_proportion = 0.5,                 #Proportion of the raster values to be sampled to calculate the correlation. The value should be set as a decimal, between 0 and 1
                cutoff = 0.7)                            #Cutoff value of correlation between variables to exclude environmental layers
}

# modleR 2/3: model calibration -------------------------------------------

for(i in 1:length(study_sp)){
  
  # run selected algorithms for each partition
  do_any(species_name = study_sp[i],
         algorithm = "maxent",              #chosen algorithm
         predictors = variables,            #name of raster stack with variables
         models_dir = model_folder,         #folder to save partitions
         png_partitions = TRUE,
         write_bin_cut = FALSE,
         equalize = TRUE,
         write_rda = TRUE)
  
}

# modleR 3/3: final models ----------------------------------------

for(i in 1:length(study_sp)){
  
  final_model(species_name = study_sp[i],
              algorithms = NULL,
              models_dir = model_folder,
              scale_models = TRUE,                           #convert model outputs to 0-1
              which_models = c("raw_mean", "raw_mean_th"),   #export both binary and continuous map  
              mean_th_par = "spec_sens",                     #maximizes the best configuration for both omission and commission errors
              png_final = TRUE,
              overwrite = TRUE)
  
}