#Chapter two - thesis 
#Erros in data - Modelling
#Marina Morim Gomes 2023
#gomes.mari.95@gmail.com

#loading packages -------------------------------------------------
library(tidyverse)

#Uniting all Metadata of all species within a loop -----------------

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

#Create a list to store the results of each iteration
rows <- list()

for(i in 1:length(study_sp)){
  
  #path to file
  caminho_arquivo <- paste0("C:/Users/Dell/OneDrive/Área de Trabalho/Doutorado - R/sarcophagidaebrasil/cap2/", study_sp[i], "/present/data_setup/metadata.csv")

  #read the file
  metadados <- read_csv(caminho_arquivo)
  
  # Extract the second row from the dataframe
  segunda_linha <- metadados[1,]
  
  # Append the second row to the list
  rows[[i]] <- segunda_linha
}

# Convert the list into a dataframe
df_resultante <- do.call(rbind, rows)

#Saving metadata
write_csv(df_resultante,"./Appendix S3/metadata.csv")