# Library
library(fmsb)
library(plyr)
library(party)
library(reshape2)
library(gplots)
library(gtools)
require(plyr)

options(stringsAsFactors = FALSE)


inputTeam <- function()
{
  team <- ask("Select an option? 1 - Chile x Equador Jogo 1, 2 - Chile x México Jogo 2, 
              3 - Chile x Bolivia Jogo 3, 4 - Chile x Uruguai Jogo 4
              5 - Chile x Peru Jogo 5, 6 - Chile  x Argentina Jogo 6
              , 7 - All")
  team <- as.numeric(team)
  return (team)
} 

lista_sdo <-  c("CB", "CD", "CM", "F", "PB", "PD", "PM") 

#team = 2
team <- inputTeam()

cat("You chose", team)

titulo = ""



jogo1 = read.csv(paste(getwd(),"/inputcsv/","Chile x Equador Jogo 1.csv", sep=""), sep = ";")
jogo2 = read.csv(paste(getwd(),"/inputcsv/","Chile x México Jogo 2.csv", sep=""), sep = ";")
jogo3 = read.csv(paste(getwd(),"/inputcsv/","Chile x Bolivia Jogo 3.csv", sep=""), sep = ";")
jogo4 = read.csv(paste(getwd(),"/inputcsv/","Chile x Uruguai Jogo 4.csv", sep=""), sep = ";")
jogo5 = read.csv(paste(getwd(),"/inputcsv/","Chile x Peru Jogo 5.csv", sep=""), sep = ";")
jogo6 = read.csv(paste(getwd(),"/inputcsv/","Chile x Argentina Jogo 6.csv", sep=""), sep = ";")

jogo1 <- jogo1[c("SDO")]
jogo2 <- jogo2[c("SDO")]
jogo3 <- jogo3[c("SDO")]
jogo4 <- jogo4[c("SDO")]
jogo5 <- jogo5[c("SDO")]
jogo6 <- jogo6[c("SDO")]

jogo1[jogo1==""] <- NA
jogo2[jogo2==""] <- NA
jogo3[jogo3==""] <- NA
jogo4[jogo4==""] <- NA
jogo5[jogo5==""] <- NA
jogo6[jogo6==""] <- NA
names(jogo1)

jogo1_freq_temp = count(jogo1, c('SDO'))
jogo2_freq_temp <- count(jogo2, c('SDO' ))
jogo3_freq_temp <- count(jogo3, c('SDO' ))
jogo4_freq_temp <- count(jogo4, c('SDO' ))
jogo5_freq_temp <- count(jogo5, c('SDO' ))
jogo6_freq_temp <- count(jogo6, c('SDO' ))

jogo1_freq <- na.omit(jogo1_freq_temp)
jogo2_freq <- na.omit(jogo2_freq_temp)
jogo3_freq <- na.omit(jogo3_freq_temp)
jogo4_freq <- na.omit(jogo4_freq_temp)
jogo5_freq <- na.omit(jogo5_freq_temp)
jogo6_freq <- na.omit(jogo6_freq_temp)


matrix_radar <- matrix(0, 6, 7)
#matrix_radar[1,] = lista_sdo
colnames(matrix_radar) <- lista_sdo




for(i in 1:length(lista_sdo)) {
  
  if (team == 1 || team == 7){
    temp = jogo1_freq[jogo1_freq$SDO == lista_sdo[i], ]
    if (nrow(temp) > 0){
      result <- temp$freq
      matrix_radar[1, lista_sdo[i]] = result
    }
    colors_border=c( "red" )
    colors_in=c( "darkred" )
    titulo = "Chile x Equador Jogo 1"
    
  }
  if (team == 2 || team == 7){
    temp = jogo2_freq[jogo2_freq$SDO == lista_sdo[i], ]
    if (nrow(temp) > 0){
      result <- temp$freq
      matrix_radar[2, lista_sdo[i]] = result
    }
    
    colors_border=c("blue")
    colors_in=c( "darkblue")
    titulo = "Chile x México Jogo 2"
  }
  if (team == 3 || team == 7){
    temp = jogo3_freq[jogo3_freq$SDO == lista_sdo[i], ]
    if (nrow(temp) > 0){
      result <- temp$freq
      matrix_radar[3, lista_sdo[i]] = result
    }
    
    colors_border=c( "yellow")
    colors_in=c( "orange")
    titulo = "Chile x Bolivia Jogo 3"
  }
  
  if (team == 4 || team == 7){
    temp = jogo4_freq[jogo4_freq$SDO == lista_sdo[i], ]
    if (nrow(temp) > 0){
      result <- temp$freq
      matrix_radar[4, lista_sdo[i]] = result
    }
    
    colors_border=c( "green" )
    colors_in=c("darkgreen" )
    titulo = "Chile x Uruguai Jogo 4"
    
    
  }
  
  if (team == 5 || team == 7){
    temp = jogo5_freq[jogo5_freq$SDO == lista_sdo[i], ]
    if (nrow(temp) > 0){
      result <- temp$freq
      matrix_radar[5, lista_sdo[i]] = result
    }
    
    colors_border=c( "blue" )
    colors_in=c("cyan" )
    titulo = "Chile x Peru Jogo 5"
  }
  
  if (team == 6 || team == 7){
    temp = jogo6_freq[jogo6_freq$SDO == lista_sdo[i], ]
    if (nrow(temp) > 0){
      result <- temp$freq
      matrix_radar[6, lista_sdo[i]] = result
    }
    
    colors_border=c( "red" )
    colors_in=c("pink" )
    titulo = "Chile x Argentina Jogo 6"
  }
  
  
  
}

if (team == 7){
  colors_border=c( "red", "blue" ,    "yellow",    "green", "blue", "red" )
  colors_in=c( "darkred", "darkblue" , "orange" , "darkgreen", "cyan" , "pink")
  
}



if (team < 7){
  
  
  data=as.data.frame(matrix_radar)
  colnames(data)=lista_sdo
  rownames(data)[1] = titulo
  
  # To use the fmsb package, I have to add 2 lines to the dataframe: the max and min of each topic to show on the plot!
  data=rbind(rep(103,2) , rep(0,2) , data)
  
  #browser()
  
  png(filename=paste(getwd(),"/outputradar/",titulo,"radar.png", sep=""), 
      units="in", 
      width=17, 
      height=10, 
      res=300)
  
  radarchart( data  , axistype=2, 
              #custom polygon
              pcol=colors_border , pfcol=colors_in,  plwd=4 , plty=1,
              #custom the grid
              cglcol="navy", cglty=1, axislabcol="grey", maxmin = TRUE,  cglwd=0.8, seg = 10,
              #custom labels
              vlcex=0.8 
  )
  legend(x=0.7, y=1.2, legend = c(titulo), 
         bty = "n", pch=20 , col=colors_border , text.col = "black", cex=2, pt.cex=3)
  
  dev.off()
}else if (team == 7){
  titulo = "All"
  
  data=as.data.frame(matrix_radar)
  colnames(data)=lista_sdo
  rownames(data)[1] = "Argentina x Paraguai Jogo 1"
  rownames(data)[2] = "Argentina x Uruguai Jogo 2"
  rownames(data)[3] = "Argentina x Jamaica Jogo 3"
  rownames(data)[4] = "Argentina x Colombia Jogo 4"
  rownames(data)[5] = "Argentina x Paraguai Jogo 5"
  rownames(data)[6] = "Argentina x Chile Jogo 6"
  
  
  # To use the fmsb package, I have to add 2 lines to the dataframe: the max and min of each topic to show on the plot!
  data=rbind(rep(92,2) , rep(0,2) , data)
  
  
  png(filename=paste(getwd(),"/outputradar/",titulo,"radar.png", sep=""), 
      units="in", 
      width=12, 
      height=10, 
      res=300)
  
  
  
  
  radarchart( data  , axistype=2, pin=c(100,50),
              
              #custom polygon
              pcol=colors_border , pfcol=colors_in,  plwd=4 , plty=1,
              #custom the grid
              cglcol="navy", cglty=2, axislabcol="grey", maxmin = TRUE,  cglwd=0.8, seg = 10,
              #custom labels
              vlcex=0.8 
  )
  legend(x=0.5, y=1.3, legend = c("Argentina x Paraguai Jogo 1", 
                                  "Argentina x Uruguai Jogo 2",
                                  "Argentina x Jamaica Jogo 3", 
                                  "Argentina x Colombia Jogo 4",
                                  "Argentina x Paraguai Jogo 5",
                                  "Argentina x Chile Jogo 6"), 
         bty = "n", pch=20 , col=colors_in , text.col = "black", cex=2, pt.cex=3)
  
  
  dev.off()
}

