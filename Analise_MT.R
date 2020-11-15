if(!require(data.table)){install.packages('data.table')}

setwd("/home/hariseldon/Documents/IESB/Proj_final_amostragem_miest/Enem 2019")  

#memory.limit(24576)

ENEM_2019 <- data.table::fread(input='MICRODADOS_ENEM_2019.csv',
                               integer64='character',
                               skip=0,  #Ler do inicio
                               nrow=-1, #Ler todos os registros
                               na.strings = "", 
                               showProgress = TRUE,
                               encoding = 'UTF-8')

sum(is.na(ENEM_2019$SG_UF_RESIDENCIA))
ENEM_2019_MT = subset(ENEM_2019, SG_UF_RESIDENCIA=="MT")
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
  for(municipio in municipios){
    dataset_i = subset(dataset, CO_MUNICIPIO_RESIDENCIA==municipio)
    N_i = nrow(dataset_i)
    #dp_i = sd(ENEM_2019_MT_i$NU_NOTA_MT)
    var_i = var(dataset_i$NU_NOTA_MT, na.rm = TRUE)
    w_i = N_i/N
    soma_numerador = soma_numerador + ((N_i**2)*var_i)/w_i
    soma_denominador = soma_denominador + (N_i)*var_i
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

tamanhos_n = calcular_n_amostragem_estratificada(ENEM_2019_MT, c(0.05, 0.1, 0.15,.2))
