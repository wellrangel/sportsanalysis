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
if (!exists("sdGiovanonni") ){
  shotDataFull <- read.csv("nbbdata/jogadas.csv", sep = ";" ,stringsAsFactors=FALSE)
  sdGiovanonni = shotDataFull[shotDataFull$id_jogador==78, ]
  sdAlex = shotDataFull[shotDataFull$id_jogador==481, ]
  sdMarquinhos = shotDataFull[shotDataFull$id_jogador==367, ]
  sdShamell = shotDataFull[shotDataFull$id_jogador==137, ]    
}

sdGiovanonni = ajustaValores(sdGiovanonni);
sdAlex = ajustaValores(sdAlex);
sdMarquinhos = ajustaValores(sdMarquinhos);
sdShamell = ajustaValores(sdShamell);

g1 <- showGraphPercentage(sdGiovanonni, 'Guilherme Giovannoni', 'Giovannoni.png')
g2 <- showGraphPercentage(sdAlex, 'Alex Garcia', 'AlexGarcia.png')
g3 <- showGraphPercentage(sdMarquinhos, 'Marquinhos', 'Marquinhos.png')
g4 <- showGraphPercentage(sdShamell, 'Shamell', 'Shamell.png')

#g4

showAllGraphPercentage4Players(g1, g2, g3, g4)

#print(gAll);

