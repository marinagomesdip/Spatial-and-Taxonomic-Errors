#Chapter two - thesis 
#Statistical Analyses
#Marina Morim Gomes 2023
#gomes.mari.95@gmail.com

#loading packages -------------------------------------
library(tidyverse)

#loading data and preparing datasets to test ------------------------------

#criando uma lista de espécies para rodar o for
study_sp <- c("Oxysarcodexia amorosa", "Oxysarcodexia xanthosoma",
              "Lipoptilocnema crispula", "Lipoptilocnema crispina")


#criar uma lista para armazenar os dataframes de cada iteração
lista_dataframes <- list()

for(i in 1:length(study_sp)){
  
  #Ler os valores raster por espécie para unir em um único dataset
  erro1 <- read_csv(paste0("./cap2/estatisticas/", study_sp[i], "_valorrastererro1.csv"))
  erro2 <- read_csv(paste0("./cap2/estatisticas/", study_sp[i], "_valorrastererro2.csv"))
  
  #acrescentando uma coluna para identificar o tipo de erro
  erro1 <- erro1 %>%
    mutate(erro_type = "erro 1")
  
  erro2 <- erro2 %>%
    mutate(erro_type = "erro 2")
  
  #combinar os dataframes erro1 e erro2 em um único dataframe e armazenar na lista
  lista_dataframes[[i]] <- bind_rows(erro1, erro2)
  
}

#combinar todos os dataframes da lista em um único dataframe
valores <- bind_rows(lista_dataframes)

#retirando as colunas que não são de interesse
valores <- valores %>%
  dplyr::select(species, erro_type, erro, valores_raster)

#COMPARANDO ORI X ERRORS POR ESPÉCIE -------------------------------------------

#NORMALIDADE DOS DADOS -

#criar uma lista para armazenar os resultados de cada iteração
lista_normalidade <- list()

for(i in 1:length(study_sp)){
  
  #separando os dados por espécie e erro1 e 2
  erro1 <- valores %>%
    filter(str_detect(species, paste0(study_sp[i]))) %>%      #reconhecendo o nome da espécie
    filter(erro_type == "erro 1")                                      #selecionando apenas o erro1
  
  erro2 <- valores %>%
    filter(str_detect(species, paste0(study_sp[i]))) %>%      #reconhecendo o nome da espécie
    filter(erro_type == "erro 2")                                      #selecionando apenas o erro2
  
  #realizando o teste para cada erro
  testee1 <- shapiro.test(erro1$valores_raster)
  testee2 <- shapiro.test(erro2$valores_raster)
  
  #extraindo os valores de p dos testes
  test_statistice1 <- testee1$statistic
  p_valuee1 <- testee1$p.value
  test_statistice2 <- testee2$statistic
  p_valuee2 <- testee2$p.value
  
  #transformando em dataframes os valores
  dfe1 <- data.frame(Species = paste0(study_sp[i]), Error_type = "erro 1",
                     Test_Statistic = test_statistice1, P_Value = p_valuee1)
  dfe2 <- data.frame(Species = paste0(study_sp[i]), Error_type = "erro 2",
                     Test_Statistic = test_statistice2, P_Value = p_valuee2)
  
  #combinar os dataframes em um único dataframe e armazenar na lista
  lista_normalidade[[i]] <- bind_rows(dfe1, dfe2)
}

#combinar todos os resultados da lista em um único dataframe
resul_normalidade <- bind_rows(lista_normalidade)

write_csv(resul_normalidade, file = "./cap2/estatisticas/normalidade_comparando_orixerrors_specie.csv")

#Por ser uma distribuição não normal, precisamos escolher um teste não parámetrico

#TESTE DE HIPÓTESES DE DIFERENÇA DE VALOR - 
#Teste de Man-Whitney ou Wilcoxon rank sum test 

#criar uma lista para armazenar os resultados de cada iteração
lista_testes <- list()

for(i in 1:length(study_sp)){

  #separando os dados por espécie e erro1 e 2
  erro1 <- valores %>%
    filter(str_detect(species, paste0(study_sp[i]))) %>%      #reconhecendo o nome da espécie
    filter(erro_type == "erro 1")                                      #selecionando apenas o erro1
  
  erro2 <- valores %>%
    filter(str_detect(species, paste0(study_sp[i]))) %>%      #reconhecendo o nome da espécie
    filter(erro_type == "erro 2")                                      #selecionando apenas o erro2
  
  #realizando o teste para cada erro
  testee1 <- wilcox.test(valores_raster ~ erro, data = erro1)
  testee2 <- wilcox.test(valores_raster ~ erro, data = erro2)
  
  #extraindo os valores de p dos testes
  test_statistice1 <- testee1$statistic
  p_valuee1 <- testee1$p.value
  test_statistice2 <- testee2$statistic
  p_valuee2 <- testee2$p.value
  
  #transformando em dataframes os valores
  dfe1 <- data.frame(Species = paste0(study_sp[i]), Error_type = "erro 1",
                     Test_Statistic = test_statistice1, P_Value = p_valuee1)
  dfe2 <- data.frame(Species = paste0(study_sp[i]), Error_type = "erro 2",
                     Test_Statistic = test_statistice2, P_Value = p_valuee2)
  
  #combinar os dataframes em um único dataframe e armazenar na lista
  lista_testes[[i]] <- bind_rows(dfe1, dfe2)
}

#combinar todos os resultados da lista em um único dataframe
resul_testes <- bind_rows(lista_testes)

#salvando os resultados
write_csv(resul_testes, file = "./cap2/estatisticas/testes_comparando_orixerrors_specie.csv")

#Tabelas para o artigo 
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

#salvando as tabelas
write_csv(tabela1, file = "./cap2/estatisticas/tabela_medias_errorperspecies.csv")
write_csv(tabela2, file = "./cap2/estatisticas/tabela_sd_errorperspecies.csv")

#COMPARANDO ERRO1 X ERRO2 POR ESPÉCIE -----------------------------------------------

#NORMALIDADE DOS DADOS -

#criar uma lista para armazenar os resultados de cada iteração
lista_normalidade <- list()

for(i in 1:length(study_sp)){
  
  #separando os dados por espécie e erro1 e 2
  erro1 <- valores %>%
    filter(str_detect(species, paste0(study_sp[i]))) %>%      #reconhecendo o nome da espécie
    filter(erro != "nao")                                      #selecionando apenas os valores de pontos errados
 
  #realizando o teste para cada erro
  testee1 <- shapiro.test(erro1$valores_raster)
  
  #extraindo os valores de p dos testes
  test_statistice1 <- testee1$statistic
  p_valuee1 <- testee1$p.value
  
  #transformando em dataframes os valores
  dfe1 <- data.frame(Species = paste0(study_sp[i]), 
                     Test_Statistic = test_statistice1, P_Value = p_valuee1)
  
  #combinar os dataframes em um único dataframe e armazenar na lista
  lista_normalidade[[i]] <- bind_rows(dfe1)
}

#combinar todos os resultados da lista em um único dataframe
resul_normalidade <- bind_rows(lista_normalidade)

write_csv(resul_normalidade, file = "./cap2/estatisticas/normalidade_comparando_errors_specie.csv")

#Por ser uma distribuição não normal, precisamos escolher um teste não parámetrico

#TESTE DE HIPÓTESES DE DIFERENÇA DE VALOR - 
#Teste de Man-Whitney ou Wilcoxon rank sum test 

#criar uma lista para armazenar os resultados de cada iteração
lista_testes <- list()

for(i in 1:length(study_sp)){
  
  #separando os dados por espécie e erro1 e 2
  erro1 <- valores %>%
    filter(str_detect(species, paste0(study_sp[i]))) %>%      #reconhecendo o nome da espécie
    filter(erro != "nao")                                      #selecionando apenas os valores de pontos errados
  
   #realizando o teste para cada erro
  testee1 <- wilcox.test(valores_raster ~ erro_type, data = erro1)
 
  #extraindo os valores de p dos testes
  test_statistice1 <- testee1$statistic
  p_valuee1 <- testee1$p.value
 
  #transformando em dataframes os valores
  dfe1 <- data.frame(Species = paste0(study_sp[i]), 
                     Test_Statistic = test_statistice1, P_Value = p_valuee1)
 
  #combinar os dataframes em um único dataframe e armazenar na lista
  lista_testes[[i]] <- bind_rows(dfe1)
}

#combinar todos os resultados da lista em um único dataframe
resul_testes <- bind_rows(lista_testes)

#salvando os resultados
write_csv(resul_testes, file = "./cap2/estatisticas/testes_comparando_errors_specie.csv")

