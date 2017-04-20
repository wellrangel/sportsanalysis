## Created by Eduardo Maia
## thedatagame.com.au
## Twitter: @thedatagame
#application to NBB by Wellington Rangel

# load all packages to be used  
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

#read all data from Giovannoni
shotData <- read.csv("PlayerData.csv", sep = ";" ,stringsAsFactors=FALSE)

#Filtering by Giovannoni
shotData <- shotData[shotData$jogador==78, ]


shotData["posXA"] <- NA 
shotData["posYA"] <- NA 

shotData["resultado"]<- NA 
shotData["resultadoint"]<- NA 


#put zero where cell is not a number
shotData[is.na(shotData)] <- 0
#get only coords greater than zero
shotData <- shotData[shotData$posX > 0, ]

for (i in 1:nrow(shotData))
{ 
  # fazendo o mirror dos pontos do ado direito da quadra para o esquerdo
  # considerando quadra de 250 por 466
  if(shotData[i,c("posX")]> 233){
    shotData[i,c("posX")] <- 466 - shotData[i,c("posX")]
    shotData[i,c("posY")] <- 250 - shotData[i,c("posY")]
  }
  
  
  # agrupando erros e acertos
  if(shotData[i,c("tipo")] == "ENT" || shotData[i,c("tipo")] == "A2C" || shotData[i,c("tipo")] == "A3C" ){
    shotData[i,c("resultado")] <- "Made Shot"
    shotData[i,c("resultadoint")] <- 1
  }
  
  if(shotData[i,c("tipo")] == "A2E" || shotData[i,c("tipo")] == "A3E" ){
    shotData[i,c("resultado")]<- "Missed Shot"
    shotData[i,c("resultadoint")] <- 0
  }
  if(shotData[i,c("tipo")] == "ENT"  ){
    shotData[i,c("tipo")] <-  "A2C"
  }
  
  if (shotData[i,c("tipo")] != "A3C" && shotData[i,c("tipo")] != "A3E" && shotData[i,c("posX")] > 150){
    shotData[i,c("posX")] = 0
    
  }
  
  if (shotData[i,c("tipo")] != "A3C" && shotData[i,c("tipo")] != "A3E" && shotData[i,c("posY")] > 220){
    shotData[i,c("posX")] = 0
    
  }
  
  
  
}
shotData <- shotData[shotData$posX > 0, ]

# simple plot using EVENT_TYPE to colour the dots
 gNBB1 <- ggplot(shotData, aes(x=posX, y=posY, z=10)) +
  geom_point(aes(colour = tipo, shape = resultado))+
    geom_tile(aes(fill=resultado)) +
    geom_density2d(col='black', size=.3) 
 
 #stat_density2d(aes(fill = ..level..), geom="polygon", alpha = 0.5, col='white')
 
 
gNBB1 + scale_x_continuous(limits = c(0, 250))


#get court image from the folder
courtImg <- "fiba.jpg"

court <- rasterGrob(readJPEG(courtImg),
                    width=unit(1,"npc"), height=unit(1,"npc"))



# plot using NBA court background and colour by shot zone
ggplot(shotData, aes(x=posX, y=posY)) +
  annotation_custom(court, 14, 250, 12, 237) +
  geom_point(aes(colour = tipo, shape = resultado))+
  geom_rug(alpha = 0.2) +
  coord_fixed() +
  scale_x_continuous(limits = c(0, 250))+
  ggtitle(paste("Shot Chart\n", unique("Giovannoni"), sep = "")) +
  theme(line = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        legend.title = element_blank(),
        plot.title = element_text(size = 17, lineheight = 1.2, face = "bold"))

#get Giovannoni image

playerImgName <- "Giovannoni.png"
playerImg <- rasterGrob(readPNG(playerImgName),
                        width=unit(0.1, "npc"), height=unit(0.2, "npc"))


pushViewport(viewport(x = unit(0.9, "npc"), y = unit(0.8, "npc")))
print(grid.draw(playerImg), newpage=FALSE)
grid.text(label = "nbb", just = "center", vjust = 50)



#outras visualizacoes

ggplot(shotData, aes(x=posX, y=posY)) +
  annotation_custom(court, 14, 240, 12, 237) +
  geom_point(aes(colour = tipo, shape = resultado))+
  geom_rug(alpha = 0.2) +
  geom_tile(aes(fill=resultado)) +
  geom_density2d(col='black', size=.3) 
  ggtitle(paste("Shot Chart\n", unique("Giovannoni"), sep = "")) +
  theme(line = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        legend.title = element_blank(),
        plot.title = element_text(size = 17, lineheight = 1.2, face = "bold"))

#get Giovannoni image

playerImgName <- "Giovannoni.png"
playerImg <- rasterGrob(readPNG(playerImgName),
                        width=unit(0.1, "npc"), height=unit(0.2, "npc"))


pushViewport(viewport(x = unit(0.9, "npc"), y = unit(0.8, "npc")))
print(grid.draw(playerImg), newpage=FALSE)
grid.text(label = "nbb", just = "center", vjust = 50)


############################
ggplot(shotData, aes(x=posX, y=posY)) +
  annotation_custom(court, 14, 240, 12, 237) +
  geom_point(aes(colour = tipo, shape = resultado))+
  geom_rug(alpha = 0.2) +
  stat_density2d(aes(fill = ..level..), geom="polygon", alpha = 0.5, col='white')
  ggtitle(paste("Shot Chart\n", unique("Giovannoni"), sep = "")) +
  theme(line = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        legend.title = element_blank(),
        plot.title = element_text(size = 17, lineheight = 1.2, face = "bold"))

#get Giovannoni image

playerImgName <- "Giovannoni.png"
playerImg <- rasterGrob(readPNG(playerImgName),
                        width=unit(0.1, "npc"), height=unit(0.2, "npc"))


pushViewport(viewport(x = unit(0.9, "npc"), y = unit(0.8, "npc")))
print(grid.draw(playerImg), newpage=FALSE)
grid.text(label = "nbb", just = "center", vjust = 55)


