#install.packages("tidyverse")
#install.packages("dplyr")
library(tidyverse)
library(dplyr)

news <- read.csv("News3.csv",header=T,sep=',')

titulos <- news$Title
classifics <-news$Theme

ran <- sample(1:length(titulos), 0.8 * length(titulos))

words <- separate_longer_delim(tibble(titulos),titulos,delim=' ')
words <- words[-15,1]

#tratamento baseado em regex

tratamento <- function(palavra){
  palavra <- tolower(palavra)
  palavra <- gsub('[,.?:]','',palavra)
  if(!(str_detect(palavra,".\'.")))
    palavra <- gsub('\'','',palavra)
  if(str_detect(palavra,".-year?"))
    palavra <- 'II_idade_II'
  if(str_detect(palavra,"\\$."))
    palavra <- 'II_dinheiro_II'
  if(str_detect(palavra,"^[0-9]*$"))
    palavra <- 'II_numero_II'
  if(str_length(palavra)<=3)
    palavra <- 'II_curta_II'
  return(palavra)
}

for(i in 1:nrow(words)){
  words$titulos[i]<-tratamento(words$titulos[i])
}

dicionario <- distinct(words)

#geração da matrix de relação entre noticia e palava

zeros <- matrix(0,nrow=length(titulos),ncol=nrow(dicionario)+2)

coords <- data.frame(row.names = titulos,zeros)
colnames(coords)<-c("II_noticia_II","II_tema_II",dicionario$titulos)
rm(zeros)

coords$II_noticia_II <- titulos
coords$II_tema_II <- classifics

#povoamento da matrix/dataset

for(i in 1:length(titulos)){
  titulo <- c(titulos[i])
  words <- separate_longer_delim(tibble(titulo),titulo,delim=' ')
  words <- words[-nrow(words),1]
  for(ii in 1:length(words$titulo)){
    a<- which(dicionario$titulos==tratamento(words$titulo[ii]))
    coords[i,a+2] <- (coords[i,a+2] +1)
  }
}

#separação em treino e teste

treino <- coords[ran,] 
teste <- coords[-ran,]

#nova matriz em função dos temas

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

coords_t <- data.frame(t(coords_t))[-1,]
coords_t <- mutate_all(coords_t, function(x) as.numeric(as.character(x)))

#naive bayes

probabilidade <- function(noticia,dic,cords,total_notcs,notcs_tema){
  temp <- 0
  for(i in 1:length(noticia)){
    qtd <- cords[which(dicionario==noticia[i])]
    temp <- temp + log((qtd+1)/(notcs_tema+2))
  }
  return(log(notcs_tema/total_notcs)+temp)
}

scores <- function(noticia,cords,dicionario,geral){
  temas <- colnames(cords)
  probs <- c()
  for(i in 1:length(colnames(cords))){
    resul <- probabilidade(noticia,dicionario,cords[,i],
                           sum(geral[,2]==temas[i]),nrow(geral))
    probs <- c(probs,resul)
  }
  return(probs)
}

classificador <- function(score,temas){
  temas[which(score==max(score))]
}

#testes

previsao <- function(noticia,cords,dicionario,geral){
  words <- str_split_1(noticia$II_noticia_II," ")
  words <- head(words,-1)
  for(i in 1:length(words)){
    words[i]<-tratamento(words[i])
  }
  temp <- scores(words,cords,dicionario,geral)
  temp2 <- classificador(temp,colnames(cords))
  print(temp2)
  print(noticia$II_tema_II)
  return(temp2==noticia$II_tema_II)
}

acertos <- c()
for(i in 1:nrow(teste)){
  acertos <- c(acertos,previsao(teste[i,],coords_t,dicionario,treino))
}

sum(acertos)/length(acertos)
