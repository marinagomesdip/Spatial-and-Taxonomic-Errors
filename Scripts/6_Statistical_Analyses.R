#Chapter two - thesis 
#Statistical Analyses
#Marina Morim Gomes 2023
#gomes.mari.95@gmail.com

#loading packages -------------------------------------
library(tidyverse)

#Preparing datasets to test ------------------------------

#Creating a species list to run the loop
study_sp <- c("Oxysarcodexia amorosa", "Oxysarcodexia xanthosoma",
              "Lipoptilocnema crispula", "Lipoptilocnema crispina")


#Create a list to store the dataframes from each iteration.
lista_dataframes <- list()

for(i in 1:length(study_sp)){
  
  #Read the raster values by species to combine into a single dataset
  erro1 <- read_csv(paste0("./cap2/estatisticas/", study_sp[i], "_valorrastererro1.csv"))
  erro2 <- read_csv(paste0("./cap2/estatisticas/", study_sp[i], "_valorrastererro2.csv"))
  
  #Adding a column to identify the type of error
  erro1 <- erro1 %>%
    mutate(erro_type = "erro 1")
  erro2 <- erro2 %>%
    mutate(erro_type = "erro 2")
  
  #Combine the erro1 and erro2 dataframes into a single dataframe and store it in the list
  lista_dataframes[[i]] <- bind_rows(erro1, erro2)
  
}

#Combine all the dataframes in the list into a single dataframe
valores <- bind_rows(lista_dataframes)

#Removing the columns that are not of interest
valores <- valores %>%
  dplyr::select(species, erro_type, erro, valores_raster)

#Comparing original vs errors by species -------------------------------------------

#Data normality -

#Create a list to store the results of each iteration
lista_normalidade <- list()

for(i in 1:length(study_sp)){
  
  #Separating the data by species and error1 and error2
  erro1 <- valores %>%
    filter(str_detect(species, paste0(study_sp[i]))) %>%               #recognizing the species name
    filter(erro_type == "erro 1")                                      #Selecting only the error1
  
  erro2 <- valores %>%
    filter(str_detect(species, paste0(study_sp[i]))) %>%               #recognizing the species name
    filter(erro_type == "erro 2")                                      #Selecting only the error2
  
  #Performing the test for each error
  testee1 <- shapiro.test(erro1$valores_raster)
  testee2 <- shapiro.test(erro2$valores_raster)
  
  #Extracting the p-values from the tests
  test_statistice1 <- testee1$statistic
  p_valuee1 <- testee1$p.value
  test_statistice2 <- testee2$statistic
  p_valuee2 <- testee2$p.value
  
  #Transforming the values into dataframes
  dfe1 <- data.frame(Species = paste0(study_sp[i]), Error_type = "erro 1",
                     Test_Statistic = test_statistice1, P_Value = p_valuee1)
  dfe2 <- data.frame(Species = paste0(study_sp[i]), Error_type = "erro 2",
                     Test_Statistic = test_statistice2, P_Value = p_valuee2)
  
  #Combine the dataframes into a single dataframe and store it in the list
  lista_normalidade[[i]] <- bind_rows(dfe1, dfe2)
}

#Combine all the results from the list into a single dataframe
resul_normalidade <- bind_rows(lista_normalidade)

#Salving the results
write_csv(resul_normalidade, file = "./cap2/estatisticas/normalidade_comparando_orixerrors_specie.csv")

#Since it is a non-normal distribution, we need to choose a non-parametric test

#Hypothesis test for value difference - 
#Teste de Man-Whitney ou Wilcoxon rank sum test 

#Create a list to store the results of each iteration
lista_testes <- list()

for(i in 1:length(study_sp)){

  #Separating the data by species and error1 and error2
  erro1 <- valores %>%
    filter(str_detect(species, paste0(study_sp[i]))) %>%       #recognizing the species name
    filter(erro_type == "erro 1")                              #Selecting only the error1
  
  erro2 <- valores %>%
    filter(str_detect(species, paste0(study_sp[i]))) %>%       #recognizing the species name
    filter(erro_type == "erro 2")                              #Selecting only the error2
  
  #Performing the test for each error
  testee1 <- wilcox.test(valores_raster ~ erro, data = erro1)
  testee2 <- wilcox.test(valores_raster ~ erro, data = erro2)
  
  #Extracting the p-values from the tests
  test_statistice1 <- testee1$statistic                            #error1
  p_valuee1 <- testee1$p.value
  test_statistice2 <- testee2$statistic                            #error2
  p_valuee2 <- testee2$p.value
  
  #Transforming the values into dataframes
  dfe1 <- data.frame(Species = paste0(study_sp[i]), Error_type = "erro 1",
                     Test_Statistic = test_statistice1, P_Value = p_valuee1)
  dfe2 <- data.frame(Species = paste0(study_sp[i]), Error_type = "erro 2",
                     Test_Statistic = test_statistice2, P_Value = p_valuee2)
  
  #Combine the dataframes into a single dataframe and store it in the list
  lista_testes[[i]] <- bind_rows(dfe1, dfe2)
}

#Combine all the results from the list into a single dataframe
resul_testes <- bind_rows(lista_testes)

#saving results
write_csv(resul_testes, file = "./cap2/estatisticas/testes_comparando_orixerrors_specie.csv")

#Paper tables
tabela1 <- valores %>%
  group_by(species, erro_type, erro) %>%
  summarize(mean_valores_raster = mean(valores_raster, na.rm = TRUE)) %>%
  ungroup() %>%
  pivot_wider(names_from = erro, values_from = mean_valores_raster)

tabela2 <- valores %>%
  group_by(species, erro_type, erro) %>%
  summarize(sd_valores_raster = sd(valores_raster, na.rm = TRUE)) %>%
  ungroup() %>%
  pivot_wider(names_from = erro, values_from = sd_valores_raster)

#Saving tables
write_csv(tabela1, file = "./cap2/estatisticas/tabela_medias_errorperspecies.csv")
write_csv(tabela2, file = "./cap2/estatisticas/tabela_sd_errorperspecies.csv")

#Comparing error1 x error2 by species -----------------------------------------------

#Data normality -

#criar uma lista para armazenar os resultados de cada iteração
lista_normalidade <- list()

for(i in 1:length(study_sp)){
  
  #separando os dados por espécie e erro1 e 2
  erro1 <- valores %>%
    filter(str_detect(species, paste0(study_sp[i]))) %>%      #recognizing the species name
    filter(erro != "nao")                                     #filtering only erroneous points
 
  #Performing the test for each error
  testee1 <- shapiro.test(erro1$valores_raster)
  
  #Extracting the p-values from the tests
  test_statistice1 <- testee1$statistic
  p_valuee1 <- testee1$p.value
  
  #Transforming the values into dataframes
  dfe1 <- data.frame(Species = paste0(study_sp[i]), 
                     Test_Statistic = test_statistice1, P_Value = p_valuee1)
  
  #Combine the dataframes into a single dataframe and store it in the list
  lista_normalidade[[i]] <- bind_rows(dfe1)
}

#Combine all the results from the list into a single dataframe
resul_normalidade <- bind_rows(lista_normalidade)

#saving the results
write_csv(resul_normalidade, file = "./cap2/estatisticas/normalidade_comparando_errors_specie.csv")

#Since it is a non-normal distribution, we need to choose a non-parametric test

#Hypothesis test for value difference - 
#Teste de Man-Whitney ou Wilcoxon rank sum test 

#Create a list to store the results of each iteration
lista_testes <- list()

for(i in 1:length(study_sp)){
  
  #separating per species and erroneous point
  erro1 <- valores %>%
    filter(str_detect(species, paste0(study_sp[i]))) %>%      #recognizing the species name
    filter(erro != "nao")                                     #filtering only erronoeus points
  
  #Performing the test between errors
  testee1 <- wilcox.test(valores_raster ~ erro_type, data = erro1)
 
  #Extracting the p-values from the tests
  test_statistice1 <- testee1$statistic
  p_valuee1 <- testee1$p.value
 
  #Transforming the values into dataframes
  dfe1 <- data.frame(Species = paste0(study_sp[i]), 
                     Test_Statistic = test_statistice1, P_Value = p_valuee1)
 
  #Combine the dataframes into a single dataframe and store it in the list
  lista_testes[[i]] <- bind_rows(dfe1)
}

#Combine all the results from the list into a single dataframe
resul_testes <- bind_rows(lista_testes)

#saving results
write_csv(resul_testes, file = "./cap2/estatisticas/testes_comparando_errors_specie.csv")