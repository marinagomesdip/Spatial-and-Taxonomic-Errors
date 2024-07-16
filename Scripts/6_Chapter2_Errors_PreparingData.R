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
  
#Species group 1 - Lipoptlocnema crispula & crispina ------------------------------------ 
crispina <- dadoslimpos %>%
  filter(nome_cientifico == "Lipoptilocnema crispina") %>%
  dplyr::select(nome_cientifico, latitude, longitude) %>%
  distinct()

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

completo <- bind_rows(amorosa, crispina, crispula, xanthosoma) 

#ERROR 1 - POSITIONAL ERROR (RANDOM COORDS) --------------------------------------------
#Creating a function to generate datasets with radom coords --------------------------
adding_random_coords <- function(dados, especie, rast_mask, prop = 0.1, num_iteracoes = 10) {
  dados_gerados <- list()  # Lista para armazenar os dataframes gerados
  
  for (i in 1:num_iteracoes) {
    # Filter the dataframe for the specific species
    especie_dados <- dados %>% filter(nome_cientifico == especie)
    
    # Calculating % of the total coordinates = value of prop
    n <- round(nrow(especie_dados) * prop)
    
    # create raster mask for point sampling
    rmask <- rast_mask %>% 
      crop(extent(c(xmin=-115, 
                    xmax=-30,
                    ymin=-60, 
                    ymax=30)))
      
    pontos_aleatorios <- dismo::randomPoints(rmask, n)
    
    # Create a column "error" with "no" for the original data
    especie_dados$erro <- "nao"
    
    # Select randomly the rows that will be replaced
    linhas_substituidas <- sample(1:nrow(especie_dados), n)
    
    # Randomly replace the existing rows with the new coordinate points
    especie_dados$latitude[linhas_substituidas] <- pontos_aleatorios[, 2]
    especie_dados$longitude[linhas_substituidas] <- pontos_aleatorios[, 1]
    especie_dados$erro[linhas_substituidas] <- "sim"
    
    # Adiciona o dataframe gerado à lista com um nome de espécie modificado
    nome_especie_modificado <- paste0(especie, "_", i)
    dados_gerados[[i]] <- especie_dados %>% mutate(nome_cientifico = nome_especie_modificado)
  }
  
    # Combina todos os dataframes gerados de volta ao dataframe original
  dados_atualizados <- bind_rows(dados_gerados)
  
  return(dados_atualizados)
}

study_sp <- c( "Oxysarcodexia amorosa", "Lipoptilocnema crispina",
              "Lipoptilocnema crispula", "Oxysarcodexia xanthosoma")

completo_atualizado <- completo  

# Agora e necessario carregar um raster para usar como mascara dos oceanos
rast_mask <- raster("./Enviromental_data/wc2.1_10m_bio_1.tif")

#Aplying the function to all species of dataframe
for (i in 1:length(study_sp)) {
  especie_atualizada <- adding_random_coords(completo_atualizado, study_sp[i], rast_mask)
  completo_atualizado <- bind_rows(completo_atualizado, especie_atualizada)
}


#ERROR 2 - TAXONOMIC ERROR (SPECIES COORDS)-------------------------------------------- 
#Creating a function to generate datasets with taxonomic errors --------------------------
adding_taxonomic_error <- function(dados, especie1, prop = 0.1, num_iteracoes = 10) {
  dados_gerados <- list()  # Lista para armazenar os dataframes gerados
  
  for (i in 1:num_iteracoes) {
    # Filtrar o dataframe para a espécie1
    especie1_dados <- dados %>% 
      filter(nome_cientifico == especie1)
    
    # Filtrar o dataframe para a espécie2 que precisa ser do mesmo gênero
    especie2 <- dados %>%
      dplyr::filter(genero == first(especie1_dados$genero)) %>%
      filter(nome_cientifico != first(especie1_dados$nome_cientifico))
    
    # Calculating % of the total coordinates = value of prop
    n <- round(nrow(especie1_dados) * prop)
    
    # Selecionar aleatoriamente as linhas que serão substituídas na espécie1
    linhas_substituir_sp1 <- sample(1:nrow(especie1_dados), n)
    
    # Selecionar aleatoriamente as linhas das espécies do mesmo gênero que serão utilizadas para substituição
    linhas_substituir_sp2 <- sample(1:nrow(especie2), n)
    
    # Criar uma coluna "erro" com "não" para os dados originais da espécie1
    especie1_dados$erro <- "nao"
    
    # Substituir as coordenadas nas linhas da espécie1 com as das espécies do mesmo gênero
    especie1_dados$latitude[linhas_substituir_sp1] <- especie2$latitude[linhas_substituir_sp2]
    especie1_dados$longitude[linhas_substituir_sp1] <- especie2$longitude[linhas_substituir_sp2]
    
    # Adicionar as novas linhas ao dataframe original com a coluna "erro" como "sim"
    especie1_dados$erro[linhas_substituir_sp1] <- "sim"
    
    # Adiciona o dataframe gerado à lista com um nome de espécie modificado
    nome_especie_modificado <- paste0(especie1, "_", i + 10)
    dados_gerados[[i]] <- especie1_dados %>% mutate(nome_cientifico = nome_especie_modificado)
  }
  
  # Combina todos os dataframes gerados de volta ao dataframe original
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
