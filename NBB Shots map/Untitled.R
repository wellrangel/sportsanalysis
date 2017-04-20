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

# 
# 
# df <- read.table(text="xCoord yCoord   pts
#                  11.4     14.9     1
#                  2.6       1.1      0
#                  4.8       4.1      1
#                  -14.4    8.2      1
#                  4.2       0.3      0
#                  0.4       0.0     1
#                  -23.2   -1.1      1
#                  -23.2   -1.0      1
#                  -23.2   -1.0      0
#                  -23.2   -1.0      0", header=TRUE)
h <- hexbin (x=shotData$posX, y = shotData$posY, IDs = TRUE, xbins=35)

pts.binned <- hexTapply (h, shotData$resultadoint, FUN=sum)

shotData.binned <- data.frame (xCoord  = h@xcm, 
                         yCoord  = h@ycm, FGA = h@count, ptsbinned = pts.binned, pts = pts.binned/h@count)

# chart.player <- ggplot (shotData.binned, aes (x =xCoord , 
#                                         y =yCoord ,  col = pts, size = h@count)) + coord_fixed() + 
# geom_point()  + scale_colour_gradient("Points/Attempt", low = "yellow", high="red")


ggplot (shotData.binned, aes (x =xCoord, y =yCoord ,  col = pts, size = h@count)) +
  geom_point()+
  scale_colour_gradient("Points/Attempt", low = "yellow", high="red") + 
  scale_fill_gradientn(colours = c("yellow","orange","red")) +
  
  #annotation_custom(court, 14, 250, 12, 237) +
  #geom_rug(alpha = 0.2) +
  #coord_fixed() +
  #scale_x_continuous(limits = c(0, 250))+
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


# pushViewport(viewport(x = unit(0.9, "npc"), y = unit(0.8, "npc")))
# print(grid.draw(playerImg), newpage=FALSE)
# grid.text(label = "nbb", just = "center", vjust = 50)

#chart.player