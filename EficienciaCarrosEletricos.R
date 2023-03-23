# Projeto com Feedback 01 - Machine learning em logística
# Prevendo o Consumo de Energia de Carros Elétricos

setwd("/home/mariobrum/repos/DSA/BigDataRAzure/Projeto_Feedback_1/")

# Instalando e carregando pacotes
install.packages("readxl")
install.packages("tidyverse")
library(tidyverse)
library(readxl)
write.csv(dados, "dataset.csv", row.names = FALSE)

# Carregando Dataset
# dados <- read_excel("dataset.xlsx")
dados <- read.csv("dataset.csv")
View(dados)

# Dimensões
dim(dados)

# Variáveis e tipos de dados
str(dados)

# Sumários das variáveis numéricas
summary(dados)

# Renomeando colunas
# Gravando os nomes em um vetor
myColumns <- colnames(dados)
myColumns

# Renomeando as colunas
myColumns[1] <- "Carro"
myColumns[2] <- "Fabricante"
myColumns[3] <- "Modelo"
myColumns[4] <- "ValorCarro"
myColumns[5] <- "ForcaMotor_Km"
myColumns[6] <- "TorqueMaximo"
myColumns[7] <- "TipoFreio"
myColumns[8] <- "TipoDirecao"
myColumns[9] <- "CapacidadeBateriaKwH"
myColumns[10] <- "KmPorCargaCompleta"
myColumns[11] <- "EntreEixos"
myColumns[12] <- "ComprimentoCm"
myColumns[13] <- "LarguraCm"
myColumns[14] <- "AlturaCm"
myColumns[15] <- "PesoMinimoKg"
myColumns[16] <- "PesoBrutoKg"
myColumns[17] <- "CargaMaximaKg"
myColumns[18] <- "Lugares"
myColumns[19] <- "Portas"
myColumns[20] <- "TamanhoPneuIn"
myColumns[21] <- "VelocidadeMaxima"
myColumns[22] <- "PortaMalasL"
myColumns[23] <- "Aceleracao0_100"
myColumns[24] <- "PotenciaCarregamento"
myColumns[25] <- "MediaConsumo"

# Alterando os nomes das colunas
colnames(dados) <- myColumns
rm(myColumns)

##### Análise Exploratória dos Dados - Limpeza dos Dados ##### 

# Quantas linhas tem casos completos?
complete_cases <- sum(complete.cases(dados$MediaConsumo))

# Quantas linhas tem casos incompletos?
not_complete_cases <- sum(!complete.cases(dados$MediaConsumo))
not_complete_cases

# Qual o percentual de dados incompletos?
percentual <- (not_complete_cases / count(dados)) * 100

# Por ser a variável target (MediaConsumo) e tem alguns valores em branco, 
# descidi excluir a observação, pois caso prenchesse com algum valor, seria um valor 
# fictício, e estas linhas representam quase 17% do dataset.
dados <- dados[complete.cases(dados$MediaConsumo),]

# Verificando algumas variãveis que tem valores em branco
table(dados$TipoFreio)
table(dados$TipoDirecao)

# Pesquisei em outro site onde tinham informações sobre o modelo o qual tinha a coluna
# TipoFreio e Acelereacao0_100 em branco, e encontrei os valores utilizados nestes modelos
dados$TipoFreio[43] <- "disc (front + rear)"
dados$PortaMalasL[43] <- "1030"

dados$Aceleracao0_100[44] <- 14
# Para preencher o valor Aceleracao0_100 do carro Mercedes-Benz EQV (long) utilizei as médias dos 
# veículos parecidos (vans) para que os valores fiquem próximo ao ideal
dados$Aceleracao0_100[43] <- 13.5

# Por ser uma variãvel categórica, transformei as colunas TipoFreio e TipoDireção em Factor
dados <- dados %>%
  mutate(TipoFreio = as.factor(TipoFreio)) %>%
  mutate(TipoDirecao = as.factor(TipoDirecao)) %>%
  mutate(Fabricante = as.factor(Fabricante))
str(dados)

# Excluindo a coluna Carro pois ela é junção da coluna Modelo e Fabricante
dados <- subset(dados, select= -Carro)


####### Análise exploratória - Dados salvos até aqui ########


# Verificando a quantidade de carros tem a quantidade de portas e lugares no dataset
table(dados$Portas)
# 3  4  5 
# 2  4 38 

table(dados$Lugares)
# 2  4  5  6  8 
# 1 10 31  1  1 

### Extraindo as variáveis numéricas
var_numerica <- sapply(dados, is.numeric)
numerical_data <- dados[var_numerica]

# Matriz de Correlação
cor(numerical_data)

# Correlation Plot
pairs(numerical_data)
pairs(numerical_data[1:3],labels = colnames(numerical_data)[1:3])
pairs(numerical_data[4:6],labels = colnames(numerical_data)[4:6])
pairs(numerical_data[7:10],labels = colnames(numerical_data)[7:10])
pairs(numerical_data[11:14],labels = colnames(numerical_data)[11:14])
pairs(numerical_data[15:19],labels = colnames(numerical_data)[15:19])

# Media de valor por Fabricante
mediavalor_profabricante <- dados %>%
  group_by(Fabricante) %>%
  summarise(ValorCarro = mean(ValorCarro))
mediavalor_profabricante

str(dados)
