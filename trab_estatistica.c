rm(list=ls())
  pesquisa = read.csv(file.choose(), header=T, sep=";")
  
  #idade: quantitativa discreta
  idade = pesquisa$Idade
  idade_media = mean(idade)
  idade_mediana = median(idade)
  idade_variancia = var(idade)
  idade_desvio = sd(idade)
  summary(idade)
  
  #altura: quantitativa contínua
  altura = pesquisa$Altura
  altura_media = mean(altura)
  altura_mediana = median(altura)
  altura_variancia = var(altura)
  altura_desvio = sd(altura)
  summary(altura)
  hist(altura, xlab="Altura (cm)", ylab="Frequencia Absoluta", main="Altura dos Alunos de Estatistica", col = "#DB7093")
  
  #sono: qualitativa ordinal
  sono = pesquisa$Sono
  sono_media = mean(sono)
  sono_mediana = median(sono)
  sono_variancia = var(sono)
  sono_desvio = sd(sono)
  summary(sono)
  
  #satisfacao: qualitativa ordinal
  satisf = pesquisa$Satisfação
  satisf_media = mean(satisf)
  satisf_mediana = median(satisf)
  satisf_variancia = var(satisf)
  satisf_desvio = sd(satisf)
  summary(satisf)
  table(satisf)
  prop.table(table(satisf))
  barplot((table(satisf)), col="gray", main = "Pesquisa de Satisfacao", xlab = "Grau de Satisfacao", ylab="Frequencia Absoluta")
  
  #tempo: qualitativa nominal
  tempo = pesquisa$Tempo
  prop.table(table(tempo))
  areas = prop.table(table(tempo))*100
  nomes = names(areas)
  porcent = round(areas,4)
  rotulos = paste(nomes, "(", porcent, "%)", sep="")
  pie(areas, main="Tempo", labels=rotulos, col=rainbow(2))
  
  #voce: qualitativa nominal
  voce = pesquisa$Você