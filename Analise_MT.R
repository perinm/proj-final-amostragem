if(!require(data.table)){install.packages('data.table')}

setwd("/home/hariseldon/Documents/IESB/Proj_final_amostragem_miest/Enem 2019")  
options(stringsAsFactors=FALSE)
#memory.limit(24576)

ENEM_2019 <- data.table::fread(input='MICRODADOS_ENEM_2019.csv',
                               integer64='character',
                               skip=0,  #Ler do inicio
                               nrow=-1, #Ler todos os registros
                               na.strings = "", 
                               showProgress = TRUE,
                               encoding = 'UTF-8')


ENEM_2019_MT = subset(ENEM_2019, SG_UF_RESIDENCIA=="MT")
ENEM_2019_SP = subset(ENEM_2019, SG_UF_RESIDENCIA=="SP")
#ENEM_2019_MT = ENEM_2019[ENEM_2019$SG_UF_RESIDENCIA == 'MT',]
dplyr::count(ENEM_2019, SG_UF_RESIDENCIA)
dplyr::count(ENEM_2019, SG_UF_PROVA)
rm(ENEM_2019)
gc()



# Amostragem Estratificada
# Estimar média


# calcula o tamanho de n, em uma amostragem estratificada por CO_MUNICIPIO_RESIDENCIA e TP_SEXO, para tirar a média de NU_NOTA_MT, para qualquer estado
calcular_n_amostragem_estratificada = function(dataset,erros){
  N = nrow(dataset)
  soma_denominador = 0
  soma_numerador = 0
  municipios = unique(dataset$CO_MUNICIPIO_RESIDENCIA)
  generos = unique(dataset$TP_SEXO)
  for(municipio in municipios){
    dataset_municipio = subset(dataset, CO_MUNICIPIO_RESIDENCIA==municipio)
    for(genero in generos){
      dataset_municipio_genero = subset(dataset_municipio, TP_SEXO==genero)
      N_i = nrow(dataset_municipio_genero)
      var_i = var(dataset_municipio_genero$NU_NOTA_MT, na.rm = TRUE)
      w_i = N_i/N
      soma_n = ((N_i**2)*var_i)/w_i
      soma_d = (N_i)*var_i
      soma_numerador = soma_numerador + (if(is.na(soma_n)) 0 else soma_n)
      soma_denominador = soma_denominador + (if(is.na(soma_d)) 0 else soma_d)
    }
  }
  tamanhos = list()
  for(erro in erros){
    B = erro
    D = (B**2)/4
    n_media = soma_numerador / ( (N**2)*D + soma_denominador)
    tamanhos = c(tamanhos, n_media)
  }
  return(tamanhos)
}

tamanhos_n_MT = calcular_n_amostragem_estratificada(ENEM_2019_MT, c(0.05, 0.1, 0.15,.2))
tamanhos_n_SP = calcular_n_amostragem_estratificada(ENEM_2019_SP, c(0.05, 0.1, 0.15,.2))

media_de_amostragem = function(dataset, n){
  compData = data.frame(municipio = numeric(0), genero = character(0), media = numeric(0))
  municipios = unique(dataset$CO_MUNICIPIO_RESIDENCIA)
  generos = unique(dataset$TP_SEXO)
  N = nrow(dataset)
  porcentagem = n/N
  for(municipio in municipios){
    dataset_municipio = subset(dataset, CO_MUNICIPIO_RESIDENCIA==municipio)
    for(genero in generos){
      dataset_municipio_genero = subset(dataset_municipio, TP_SEXO==genero)
      n_municipio_genero = ceiling(porcentagem * nrow(dataset_municipio_genero))
      amostra = dplyr::sample_n(dataset_municipio_genero, n_municipio_genero)
      media_mat = mean(amostra$NU_NOTA_MT, na.rm = TRUE)
      compData[nrow(compData)+1, ] = c(municipio, genero, media_mat)
    }
  }
  return(compData)
}

media_MT = media_de_amostragem(ENEM_2019_MT, nrow(ENEM_2019_MT))
media_MT_5 = media_de_amostragem(ENEM_2019_MT, tamanhos_n_MT[[1]])
media_MT_10 = media_de_amostragem(ENEM_2019_MT, tamanhos_n_MT[[2]])
media_MT_15 = media_de_amostragem(ENEM_2019_MT, tamanhos_n_MT[[3]])
media_MT_20 = media_de_amostragem(ENEM_2019_MT, tamanhos_n_MT[[4]])

media_SP = media_de_amostragem(ENEM_2019_MT, nrow(ENEM_2019_SP))
media_SP_5 = media_de_amostragem(ENEM_2019_SP, tamanhos_n_SP[[1]])
media_SP_10 = media_de_amostragem(ENEM_2019_SP, tamanhos_n_SP[[2]])
media_SP_15 = media_de_amostragem(ENEM_2019_SP, tamanhos_n_SP[[3]])
media_SP_20 = media_de_amostragem(ENEM_2019_SP, tamanhos_n_SP[[4]])
