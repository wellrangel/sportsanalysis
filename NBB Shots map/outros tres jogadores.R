library(lattice)
library(rjson)
library(ggplot2)
library(grid)
library(gridExtra)
library(png) 
library(jpeg)
library(RCurl)
library(hexbin)
library(plyr)
library(cowplot)


source("functions.R")

#read all data if necessary
if (!exists("p1") ){
  shotDataFull <- read.csv("nbbdata/jogadas.csv", sep = ";" ,stringsAsFactors=FALSE)
  p1 = shotDataFull[shotDataFull$id_jogador==11657, ] #rafael
  p2 = shotDataFull[shotDataFull$id_jogador==482, ] # olivinha
  p3 = shotDataFull[shotDataFull$id_jogador==192, ]  #Larry

}

p1 = ajustaValores(p1);
p2 = ajustaValores(p2);
p3 = ajustaValores(p3);



g1 <- showGraphPercentage(p1, 'Rafael', 'Rafael.png')
g2 <- showGraphPercentage(p2, 'Olivinha', 'Olivinha.png')
g3 <- showGraphPercentage(p3, 'Larry', 'Larry.png')


g1;

#showAllGraphPercentage3Players(g1, g2, g3)

#print(gAll);

