
ajustaValores<-function(dataFrame)
{
  
  dataFrame["resultado"]<- NA 
  dataFrame["resultadoint"]<- NA 
  
  
  #put zero where cell is not a number
  dataFrame[is.na(dataFrame)] <- 0
  #get only coords greater than zero
  dataFrame <- dataFrame[dataFrame$posX > 0, ]
  
  for (i in 1:nrow(dataFrame))
  { 
    # fazendo o mirror dos pontos do ado direito da quadra para o esquerdo
    # considerando quadra de 250 por 466
    if(dataFrame[i,c("posX")]> 233){
      dataFrame[i,c("posX")] <- 466 - dataFrame[i,c("posX")]
      dataFrame[i,c("posY")] <- 250 - dataFrame[i,c("posY")]
    }
    
    
    # agrupando erros e acertos
    if(dataFrame[i,c("tipo")] == "ENT" || dataFrame[i,c("tipo")] == "A2C" || dataFrame[i,c("tipo")] == "A3C" ){
      dataFrame[i,c("resultado")] <- "Made Shot"
      dataFrame[i,c("resultadoint")] <- 1
    }
    
    if(dataFrame[i,c("tipo")] == "A2E" || dataFrame[i,c("tipo")] == "A3E" ){
      dataFrame[i,c("resultado")]<- "Missed Shot"
      dataFrame[i,c("resultadoint")] <- 0
    }
    if(dataFrame[i,c("tipo")] == "ENT"  ){
      dataFrame[i,c("tipo")] <-  "A2C"
    }
    
    if (dataFrame[i,c("tipo")] != "A3C" && dataFrame[i,c("tipo")] != "A3E" && dataFrame[i,c("posX")] > 150){
      dataFrame[i,c("posX")] = 0
      
    }
    
    if (dataFrame[i,c("tipo")] != "A3C" && dataFrame[i,c("tipo")] != "A3E" && dataFrame[i,c("posY")] > 220){
      dataFrame[i,c("posX")] = 0
      
    }
    if ( dataFrame[i,c("posY")] > 250){
      dataFrame[i,c("posX")] = 0
      
    }
    
    
    
  }
  dataFrame <- dataFrame[dataFrame$posX > 0, ]
  
  return(dataFrame)
  
}

showGraphPercentage<-function(dataFrame, playName, playerImgName)
{
  
  #get court image from the folder
  courtImg <- "fiba.jpg"
  
  court <- rasterGrob(readJPEG(courtImg),
                      width=unit(1,"npc"), height=unit(1,"npc"))
  
  
  h <- hexbin (x=dataFrame$posX, y = dataFrame$posY, IDs = TRUE, xbins=35)
  
  pts.binned <- hexTapply (h, dataFrame$resultadoint, FUN=sum)
  
  dataFrame.binned <- data.frame (xCoord  = h@xcm, 
                                 yCoord  = h@ycm, FGA = h@count, ptsbinned = pts.binned, pts = pts.binned/h@count)
  
  
  #get  image
  playerImg <- rasterGrob(readPNG(playerImgName),
                          width=unit(0.05, "npc"), height=unit(0.08, "npc"))
  
  
  
  chart.player <- ggplot (dataFrame.binned, aes (x =xCoord, y =yCoord ,  col = pts, size = h@count)) +
    annotation_custom(court, 14, 250, 12, 237) +
    annotation_custom(playerImg, 0, 470, 0, 430) +
    geom_point()+
    scale_colour_gradient("Points/Attempt", low = "yellow", high="red") + 
    scale_fill_gradientn(colours = c("yellow","orange","red")) +
    geom_rug(alpha = 0.2) +
    coord_fixed() +
    scale_x_continuous(limits = c(0, 250))+
    ggtitle(paste("Shot Chart\n", unique(playName), sep = "")) +
    theme(line = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          legend.title = element_blank(),
          plot.title = element_text(size = 17, lineheight = 1.2, face = "bold"))
  
  
  
  # pushViewport(viewport(x = unit(0.9, "npc"), y = unit(0.8, "npc")))
  # print(grid.draw(playerImg), newpage=FALSE)
  # grid.text(label = "nbb", just = "center", vjust = 50)
  
   return(chart.player)

}


showAllGraphPercentage4Players<-function(g1, g2, g3, g4)
{
  grid.arrange(g1 , g2 ,
               g3 , g4,
               ncol=2, nrow=2)
  
}


showAllGraphPercentage3Players<-function(g1, g2, g3)
{
  grid.arrange(g1 , g2 ,
               g3,
               ncol=3, nrow=1)
  
}