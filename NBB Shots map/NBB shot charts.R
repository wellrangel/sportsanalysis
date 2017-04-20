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


#put zero where cell is not a number
shotData[is.na(shotData)] <- 0
#get only coords greater than zero
shotData <- shotData[shotData$posX > 0, ]

for (i in 1:nrow(shotData))
{ 
  # fazendo o mirror dos pontos do ado direito da quadra para o esquerdo
  # considerando quadra de 250 por 450
  if(shotData[i,c("posX")]> 225){
    shotData[i,c("posX")] <- 450 - shotData[i,c("posX")]
    shotData[i,c("posY")] <- 250 - shotData[i,c("posY")]
  }
  

  # agrupando erros e acertos
  if(shotData[i,c("tipo")] == "ENT" || shotData[i,c("tipo")] == "A2C" || shotData[i,c("tipo")] == "A3C" ){
    shotData[i,c("resultado")] <- "Made Shot"
  }
  
  if(shotData[i,c("tipo")] == "A2E" || shotData[i,c("tipo")] == "A3E" ){
    shotData[i,c("resultado")]<- "Missed Shot"
  }
  
  if(shotData[i,c("tipo")] == "ENT"  ){
    shotData[i,c("tipo")] <-  "A2C"
  }
  
  
}


# simple plot using EVENT_TYPE to colour the dots
gNBB1 <- ggplot(shotData, aes(x=posX, y=posY)) +
 geom_point(aes(colour = tipo))
gNBB1 + scale_x_continuous(limits = c(0, 250))


#get court image from the folder
courtImg <- "fiba.jpg"

court <- rasterGrob(readJPEG(courtImg),
                    width=unit(1,"npc"), height=unit(1,"npc"))



# plot using NBA court background and colour by shot zone
ggplot(shotData, aes(x=posX, y=posY)) +
  annotation_custom(court, 2, 250, 10, 235) +
  geom_point(aes(colour = tipo, shape = resultado))+
  geom_rug(alpha = 0.2) +
  coord_fixed() +
  scale_x_continuous(limits = c(0, 250))+
  ggtitle(paste("Shot Chart\n", unique("Giovanoni"), sep = "")) +
    theme(line = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          legend.title = element_blank(),
          plot.title = element_text(size = 17, lineheight = 1.2, face = "bold"))

#get Giovannoni image from the internet
playerImg.URL <- paste("http://lnb.com.br/wp-content/uploads/2016/11/Giovannoni.png", sep="")
playerImg <- rasterGrob(readPNG(getURLContent(playerImg.URL)), 
                        width=unit(0.1, "npc"), height=unit(0.2, "npc"))


pushViewport(viewport(x = unit(0.9, "npc"), y = unit(0.8, "npc")))
print(grid.draw(playerImg), newpage=FALSE)
grid.text(label = "nbb", just = "center", vjust = 50)


# plot shots using ggplot, hex bins, NBA court backgroung image.
ggplot(shotData, aes(x=posX, y=posY)) +
  annotation_custom(court, 2, 250, 10, 235) +
  stat_binhex(bins = 35, colour = "gray", alpha = 0.7) +
  scale_fill_gradientn(colours = c("yellow","orange","red")) +
  guides(alpha = TRUE, size = TRUE) +
  xlim(0, 250) +
  ylim(0, 250) +
  geom_rug(alpha = 0.5) +
  coord_fixed() +
  ggtitle(paste("Shot Chart\n", unique("Giovanoni"), sep = "")) 
  # +theme(line = element_blank(),
  #       axis.title.x = element_blank(),
  #       axis.title.y = element_blank(),
  #       axis.text.x = element_blank(),
  #       axis.text.y = element_blank(),
  #       legend.title = element_blank(),
  #       legend.text=element_text(size = 12),
  #       plot.title = element_text(size = 17, lineheight = 1.2, face = "bold"))

pushViewport(viewport(x = unit(0.9, "npc"), y = unit(0.8, "npc")))
print(grid.draw(playerImg), newpage=FALSE)
grid.text(label = "nbb", just = "center", vjust = 50)


#considerando made and miss
# 
ggplot(shotData, aes(x=posX, y=posY)) +
  annotation_custom(court, 2, 250, 10, 235) +
  stat_binhex(bins = 20, colour = "gray", alpha = 0.7) +
  geom_point(size=1, alpha=0.4, aes(shape=resultado), position="jitter") +
  coord_equal() +
  scale_fill_gradientn(colours = c("yellow","orange","red")) +
  guides(alpha = TRUE, size = TRUE) +
  xlim(0, 250) +
  ylim(0, 250) +
  geom_rug(alpha = 0.5) +
  coord_fixed() +
  ggtitle(paste("Shot Chart\n", unique("Giovanoni"), sep = ""))
  # +theme(line = element_blank(),
  #      axis.title.x = element_blank(),
  #      axis.title.y = element_blank(),
  #      axis.text.x = element_blank(),
  #      axis.text.y = element_blank(),
  #      legend.title = element_blank(),
  #      legend.text=element_text(size = 12),
  #      plot.title = element_text(size = 17, lineheight = 1.2, face = "bold"))

pushViewport(viewport(x = unit(0.9, "npc"), y = unit(0.8, "npc")))
print(grid.draw(playerImg), newpage=FALSE)
grid.text(label = "nbb", just = "center", vjust = 50)


shotDataMade <- shotData[shotData$resultado=="Made Shot", ]
shotDataMiss <- shotData[shotData$resultado=="Missed Shot", ]

ggplot(shotDataMade, aes(x=posX, y=posY)) +
  annotation_custom(court, 2, 250, 10, 235) +
  stat_density_2d(
    alpha = 0.7,
    data = shotDataMade,
    aes(x = posX, y = posY,
        fill = ..density..),
    geom = "raster", contour = FALSE, interpolate = TRUE, n = 200
  ) +
   
  scale_fill_gradientn(colors = c('#000004', '#010107', '#02020C', '#030312', '#050417', '#07051D', '#0A0723', '#0D0829', '#100A2F', '#140B35', '#170C3B', '#1B0C41', '#1F0C48', '#230C4E', '#280B53', '#2C0B58', '#310A5D', '#350960', '#3A0963', '#3E0966', '#430A68', '#470B6A', '#4B0C6B', '#4F0D6C', '#540F6D', '#58106E', '#5C126E', '#60136E', '#64156E', '#68166E', '#6C186E', '#70196E', '#741B6E', '#781C6D', '#7D1E6D', '#811F6C', '#85216B', '#89226A', '#8D2369', '#912568', '#952667', '#992865', '#9D2964', '#A12B62', '#A52D60', '#A92E5E', '#AD305C', '#B1325A', '#B53458', '#B93656', '#BD3853', '#C03A51', '#C43C4E', '#C83F4C', '#CB4149', '#CF4446', '#D24644', '#D54941', '#D84C3E', '#DB4F3B', '#DE5338', '#E15635', '#E45A32', '#E65D2F', '#E9612B', '#EB6528', '#ED6925', '#EF6D22', '#F1711E', '#F3751B', '#F47A18', '#F67E14', '#F78311', '#F8870E', '#F98C0A', '#FA9008', '#FB9506', '#FB9A06', '#FC9F07', '#FCA409', '#FCA80D', '#FCAD12', '#FCB217', '#FBB71C', '#FBBC22', '#FAC128', '#F9C72E', '#F8CC35', '#F7D13C', '#F6D643', '#F5DB4B', '#F4E054', '#F3E45D', '#F2E967', '#F1EE71', '#F2F27C', '#F3F587', '#F5F991', '#F8FC9B', '#FCFFA4'), guide = FALSE ) +
  scale_colour_gradientn("Shot frequency    ",
                         limits = c(0, 1),
                         breaks = c(0, 1),
                         labels = c("lower", "higher"),
                         colours =c('#000004', '#010107', '#02020C', '#030312', '#050417', '#07051D', '#0A0723', '#0D0829', '#100A2F', '#140B35', '#170C3B', '#1B0C41', '#1F0C48', '#230C4E', '#280B53', '#2C0B58', '#310A5D', '#350960', '#3A0963', '#3E0966', '#430A68', '#470B6A', '#4B0C6B', '#4F0D6C', '#540F6D', '#58106E', '#5C126E', '#60136E', '#64156E', '#68166E', '#6C186E', '#70196E', '#741B6E', '#781C6D', '#7D1E6D', '#811F6C', '#85216B', '#89226A', '#8D2369', '#912568', '#952667', '#992865', '#9D2964', '#A12B62', '#A52D60', '#A92E5E', '#AD305C', '#B1325A', '#B53458', '#B93656', '#BD3853', '#C03A51', '#C43C4E', '#C83F4C', '#CB4149', '#CF4446', '#D24644', '#D54941', '#D84C3E', '#DB4F3B', '#DE5338', '#E15635', '#E45A32', '#E65D2F', '#E9612B', '#EB6528', '#ED6925', '#EF6D22', '#F1711E', '#F3751B', '#F47A18', '#F67E14', '#F78311', '#F8870E', '#F98C0A', '#FA9008', '#FB9506', '#FB9A06', '#FC9F07', '#FCA409', '#FCA80D', '#FCAD12', '#FCB217', '#FBB71C', '#FBBC22', '#FAC128', '#F9C72E', '#F8CC35', '#F7D13C', '#F6D643', '#F5DB4B', '#F4E054', '#F3E45D', '#F2E967', '#F1EE71', '#F2F27C', '#F3F587', '#F5F991', '#F8FC9B', '#FCFFA4'),
                         guide = guide_colorbar(barwidth = 15)) +
  theme(legend.text = element_text(size = rel(0.6)))+
  coord_equal() +
  guides(alpha = TRUE, size = TRUE) +
  xlim(0, 250) +
  ylim(0, 250) +
  geom_rug(alpha = 0.1) +
  coord_fixed() +
  
  ggtitle(paste("Made Shot Chart\n", unique("Giovanoni"), sep = ""))
# +theme(line = element_blank(),
#      axis.title.x = element_blank(),
#      axis.title.y = element_blank(),
#      axis.text.x = element_blank(),
#      axis.text.y = element_blank(),
#      legend.title = element_blank(),
#      legend.text=element_text(size = 12),
#      plot.title = element_text(size = 17, lineheight = 1.2, face = "bold"))

pushViewport(viewport(x = unit(0.9, "npc"), y = unit(0.8, "npc")))
print(grid.draw(playerImg), newpage=FALSE)
grid.text(label = "nbb", just = "center", vjust = 50)



ggplot(shotDataMiss, aes(x=posX, y=posY)) +
  annotation_custom(court, 2, 250, 10, 235) +
  stat_density_2d(
    alpha = 0.7,
    data = shotDataMiss,
    aes(x = posX, y = posY,
        fill = ..density..),
    geom = "raster", contour = FALSE, interpolate = TRUE, n = 200
  ) +
  
  scale_fill_gradientn(colors = c('#000004', '#010107', '#02020C', '#030312', '#050417', '#07051D', '#0A0723', '#0D0829', '#100A2F', '#140B35', '#170C3B', '#1B0C41', '#1F0C48', '#230C4E', '#280B53', '#2C0B58', '#310A5D', '#350960', '#3A0963', '#3E0966', '#430A68', '#470B6A', '#4B0C6B', '#4F0D6C', '#540F6D', '#58106E', '#5C126E', '#60136E', '#64156E', '#68166E', '#6C186E', '#70196E', '#741B6E', '#781C6D', '#7D1E6D', '#811F6C', '#85216B', '#89226A', '#8D2369', '#912568', '#952667', '#992865', '#9D2964', '#A12B62', '#A52D60', '#A92E5E', '#AD305C', '#B1325A', '#B53458', '#B93656', '#BD3853', '#C03A51', '#C43C4E', '#C83F4C', '#CB4149', '#CF4446', '#D24644', '#D54941', '#D84C3E', '#DB4F3B', '#DE5338', '#E15635', '#E45A32', '#E65D2F', '#E9612B', '#EB6528', '#ED6925', '#EF6D22', '#F1711E', '#F3751B', '#F47A18', '#F67E14', '#F78311', '#F8870E', '#F98C0A', '#FA9008', '#FB9506', '#FB9A06', '#FC9F07', '#FCA409', '#FCA80D', '#FCAD12', '#FCB217', '#FBB71C', '#FBBC22', '#FAC128', '#F9C72E', '#F8CC35', '#F7D13C', '#F6D643', '#F5DB4B', '#F4E054', '#F3E45D', '#F2E967', '#F1EE71', '#F2F27C', '#F3F587', '#F5F991', '#F8FC9B', '#FCFFA4'), guide = FALSE ) +
  scale_colour_gradientn("Shot frequency    ",
                         limits = c(0, 1),
                         breaks = c(0, 1),
                         labels = c("lower", "higher"),
                         colours =c('#000004', '#010107', '#02020C', '#030312', '#050417', '#07051D', '#0A0723', '#0D0829', '#100A2F', '#140B35', '#170C3B', '#1B0C41', '#1F0C48', '#230C4E', '#280B53', '#2C0B58', '#310A5D', '#350960', '#3A0963', '#3E0966', '#430A68', '#470B6A', '#4B0C6B', '#4F0D6C', '#540F6D', '#58106E', '#5C126E', '#60136E', '#64156E', '#68166E', '#6C186E', '#70196E', '#741B6E', '#781C6D', '#7D1E6D', '#811F6C', '#85216B', '#89226A', '#8D2369', '#912568', '#952667', '#992865', '#9D2964', '#A12B62', '#A52D60', '#A92E5E', '#AD305C', '#B1325A', '#B53458', '#B93656', '#BD3853', '#C03A51', '#C43C4E', '#C83F4C', '#CB4149', '#CF4446', '#D24644', '#D54941', '#D84C3E', '#DB4F3B', '#DE5338', '#E15635', '#E45A32', '#E65D2F', '#E9612B', '#EB6528', '#ED6925', '#EF6D22', '#F1711E', '#F3751B', '#F47A18', '#F67E14', '#F78311', '#F8870E', '#F98C0A', '#FA9008', '#FB9506', '#FB9A06', '#FC9F07', '#FCA409', '#FCA80D', '#FCAD12', '#FCB217', '#FBB71C', '#FBBC22', '#FAC128', '#F9C72E', '#F8CC35', '#F7D13C', '#F6D643', '#F5DB4B', '#F4E054', '#F3E45D', '#F2E967', '#F1EE71', '#F2F27C', '#F3F587', '#F5F991', '#F8FC9B', '#FCFFA4'),
                         guide = guide_colorbar(barwidth = 15)) +
  theme(legend.text = element_text(size = rel(0.6)))+
  coord_equal() +
  guides(alpha = TRUE, size = TRUE) +
  xlim(0, 250) +
  ylim(0, 250) +
  geom_rug(alpha = 0.1) +
  coord_fixed() +
  
  ggtitle(paste("Missed Shot Chart \n", unique("Giovanoni"), sep = ""))
# +theme(line = element_blank(),
#      axis.title.x = element_blank(),
#      axis.title.y = element_blank(),
#      axis.text.x = element_blank(),
#      axis.text.y = element_blank(),
#      legend.title = element_blank(),
#      legend.text=element_text(size = 12),
#      plot.title = element_text(size = 17, lineheight = 1.2, face = "bold"))

pushViewport(viewport(x = unit(0.9, "npc"), y = unit(0.8, "npc")))
print(grid.draw(playerImg), newpage=FALSE)
grid.text(label = "nbb", just = "center", vjust = 50)

