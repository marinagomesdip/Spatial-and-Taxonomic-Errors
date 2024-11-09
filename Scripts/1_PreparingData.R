#Chapter two - thesis 
#Erros in data - Preparing data
#Marina Morim Gomes 2023
#gomes.mari.95@gmail.com

#loading packages -------------------------------------
library(raster)
library(dismo)
library(tidyverse)

#loading data -------------------------------------
dadoslimpos <- read_csv("./Data/Processados/4_dadoslimpos.csv")

#Filtering species that will be used from all data

#Removing occurence data before 1970 to mach with WorlClim enviromental
dadoslimpos <- dadoslimpos %>%
  filter(ano_inicio >= 1970)

#Filtering the species name to create separate objetcs to work on  
#Species group 1 - Lipoptlocnema crispula & crispina ------------------------------------ 
crispina <- dadoslimpos %>%
  filter(nome_cientifico == "Lipoptilocnema crispina") %>%   #filter only target species
  dplyr::select(nome_cientifico, latitude, longitude) %>%    #select the columns that are important to SDM
  distinct()                                                 #clean duplicates 

crispula <- dadoslimpos %>%
  filter(nome_cientifico == "Lipoptilocnema crispula") %>%
  dplyr::select(nome_cientifico, latitude, longitude) %>%
  distinct()

#Species group 2 - Oxysarcodexia xanthosoma & amorosa ----------------------------------------------
xanthosoma <- dadoslimpos %>%
  filter(nome_cientifico == "Oxysarcodexia xanthosoma") %>%
  dplyr::select(nome_cientifico, latitude, longitude) %>%
  distinct()

amorosa <- dadoslimpos %>%
  filter(nome_cientifico == "Oxysarcodexia amorosa") %>%
  dplyr::select(nome_cientifico, latitude, longitude) %>%
  distinct()

#Uniting all species in only one dataset to loop SDM ------------------------------
completo <- bind_rows(amorosa, crispina, crispula, xanthosoma) 

#ERROR 1 - POSITIONAL ERROR (RANDOM COORDS) --------------------------------------------
#Creating a function to generate datasets with radom coords --------------------------
adding_random_coords <- function(dados, especie, rast_mask, prop = 0.1, num_iteracoes = 10) {
  dados_gerados <- list()  # List to store the generated dataframes
  
  for (i in 1:num_iteracoes) {
    # Filter the dataframe for the specific species
    especie_dados <- dados %>% filter(nome_cientifico == especie)
    
    # Calculating % of the total coordinates = value of prop (we used 10%)
    n <- round(nrow(especie_dados) * prop)
    
    # create raster mask for point sampling
    rmask <- rast_mask %>% 
      crop(extent(c(xmin=-115, 
                    xmax=-30,
                    ymin=-60, 
                    ymax=30)))
    
    # Generate random points inside the delimited area  
    pontos_aleatorios <- dismo::randomPoints(rmask, n)
    
    # Create a column "error" with "no" for the original data
    especie_dados$erro <- "nao"
    
    # Select randomly the rows that will be replaced
    linhas_substituidas <- sample(1:nrow(especie_dados), n)
    
    # Randomly replace the existing rows with the new coordinate points
    especie_dados$latitude[linhas_substituidas] <- pontos_aleatorios[, 2]
    especie_dados$longitude[linhas_substituidas] <- pontos_aleatorios[, 1]
    especie_dados$erro[linhas_substituidas] <- "sim"
    
    # Adds the generated dataframe to the list with a species name + number
    nome_especie_modificado <- paste0(especie, "_", i)
    dados_gerados[[i]] <- especie_dados %>% mutate(nome_cientifico = nome_especie_modificado)
  }
  
    # Combines all the generated dataframes back into the original dataframe
  dados_atualizados <- bind_rows(dados_gerados)
  
  return(dados_atualizados)
}

# Creating a list with the species names to which the function will be applied
study_sp <- c( "Oxysarcodexia amorosa", "Lipoptilocnema crispina",
              "Lipoptilocnema crispula", "Oxysarcodexia xanthosoma")

# Creating an object as a copy of the original dataframe
completo_atualizado <- completo  

# Loading a raster to use as a mask for the oceans
rast_mask <- raster("./Enviromental_data/wc2.1_10m_bio_1.tif")

#Aplying the function to all species of dataframe
for (i in 1:length(study_sp)) {
  especie_atualizada <- adding_random_coords(completo_atualizado, study_sp[i], rast_mask)
  completo_atualizado <- bind_rows(completo_atualizado, especie_atualizada)
}


#ERROR 2 - TAXONOMIC ERROR (SPECIES COORDS)-------------------------------------------- 
#Creating a function to generate datasets with taxonomic errors --------------------------
adding_taxonomic_error <- function(dados, especie1, prop = 0.1, num_iteracoes = 10) {
  dados_gerados <- list()  # # List to store the generated dataframes
  
  for (i in 1:num_iteracoes) {
    # Filtering the dataframe for species1
    especie1_dados <- dados %>% 
      filter(nome_cientifico == especie1)
    
    # Filter the dataframe for species2, ensuring it belongs to the same genus
    especie2 <- dados %>%
      dplyr::filter(genero == first(especie1_dados$genero)) %>%
      filter(nome_cientifico != first(especie1_dados$nome_cientifico))
    
    # Calculating % of the total coordinates = value of prop (we used 10%)
    n <- round(nrow(especie1_dados) * prop)
    
    # Randomly select the rows that will be replaced in species1
    linhas_substituir_sp1 <- sample(1:nrow(especie1_dados), n)
    
    # Randomly select the rows of species from the same genus that will be used for replacement
    linhas_substituir_sp2 <- sample(1:nrow(especie2), n)
    
    # Create a column "error" with "no" for the original data of species1
    especie1_dados$erro <- "nao"
    
    # Replace the coordinates in the rows of species1 with those of species from the same genus
    especie1_dados$latitude[linhas_substituir_sp1] <- especie2$latitude[linhas_substituir_sp2]
    especie1_dados$longitude[linhas_substituir_sp1] <- especie2$longitude[linhas_substituir_sp2]
    
    # Add the new rows to the original dataframe with the "error" column set to "yes"
    especie1_dados$erro[linhas_substituir_sp1] <- "sim"
    
    # Add the generated dataframe to the list with a modified species name
    nome_especie_modificado <- paste0(especie1, "_", i + 10)
    dados_gerados[[i]] <- especie1_dados %>% mutate(nome_cientifico = nome_especie_modificado)
  }
  
  # Combine all generated dataframes back into the original dataframe
  dados_atualizados <- bind_rows(dados_gerados)
  
  return(dados_atualizados)
}

#Creating a new object with genero and species separated to apply the function
completo_atualizado_taxonomic_error <- completo %>%
  separate(nome_cientifico, into = c("genero", "especie"), sep = " ", remove = FALSE)

#Aplying the function to all species of dataframe
for (i in 1:length(study_sp)) {
  especie_atualizada <- adding_taxonomic_error(completo_atualizado_taxonomic_error, study_sp[i])
  completo_atualizado_taxonomic_error <- bind_rows(completo_atualizado_taxonomic_error, especie_atualizada)
}

#Uniting the dataframes to export one csv
completo_atualizado_taxonomic_error <- completo_atualizado_taxonomic_error %>%
  select(-genero, -especie)
final <- union(completo_atualizado, completo_atualizado_taxonomic_error)

#exporting the final csv
write_csv(final,"./Data/Processados/6_para_rodar_modelagem.csv")
