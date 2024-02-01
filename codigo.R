#Caso não tenha os pacotes, descomente as linhas abaixo
#install.packages("tidyverse")
#install.packages("dplyr")
library(tidyverse)
library(dplyr)

#Relembrando o formato do conjunto:
news <- read.csv("News3.csv",header=T,sep=',')

#Preparações: separando os títulos em palavras
titulos <- news$Title

words<-separate_longer_delim(tibble(titulos),titulos,delim=' ')

words <- unique(words)
words <- words[-15,1]

#Tratamento: função de tratamento
tratamento <- function(palavra){
  palavra <- tolower(palavra)
  palavra <- gsub('[,.?:]','',palavra)
  if(!(str_detect(palavra,".\'."))){palavra <- gsub('\'','',palavra)}
  if(str_detect(palavra,".-year?")){palavra <- 'II_idade_II'}
  if(str_detect(palavra,"\\$.")){palavra <- 'II_dinheiro_II'}
  if(str_detect(palavra,"^[0-9]*$")){palavra <- 'II_numero_II'}
  if(str_length(palavra)<=3){palavra <- 'II_curta_II'}
  return(palavra)
}

#Montando o dicionário:
for(i in 1:nrow(words)){
  words$titulos[i]<-tratamento(words$titulos[i])
}

dicionario <- distinct(words)

#Construindo a matriz de repetições:
zeros <- matrix(0,nrow=length(titulos),ncol=nrow(dicionario)+2)

coords <- data.frame(row.names = titulos,zeros)

colnames(coords)<-c("II_noticia_II","II_tema_II",dicionario$titulos)

rm(zeros)

coords$II_noticia_II <- titulos
coords$II_tema_II <- news$Theme

#Povoando a matriz de repetições:
for(i in 1:length(titulos)){
  titulo <- c(titulos[i])
  words <- str_split_1(titulo," ")
  words <- head(words,-1)
  for(ii in 1:length(words)){
    a<- which(dicionario$titulos==tratamento(words[ii]))
    coords[i,a+2] <- (coords[i,a+2] +1)
  }
}

#Preparação para previsão: treino e teste
ran <- sample(1:length(titulos), 0.8 * length(titulos))
treino <- coords[ran,] 
teste <- coords[-ran,]

#Matriz de temas e palavras:
temas <- unique(treino$II_tema_II)

zeros2 <- matrix(0,nrow=length(temas),ncol=nrow(dicionario)+1)

coords_t <- data.frame(row.names = temas,zeros2)

colnames(coords_t)<-c("II_tema_II",dicionario$titulos)

rm(zeros2)

coords_t$II_tema_II<-temas

for(i in 1:length(temas)){
  temp <- treino[treino[,2]==temas[i],]
  for(ii in 2:ncol(coords_t)){
    coords_t[i,ii] <- sum(temp[,ii+1])
  }
}
rm(temp)

#Preparação para previsão:
coords_t <- data.frame(t(coords_t))[-1,]

coords_t<-mutate_all(coords_t,function(x) as.numeric(as.character(x)))
