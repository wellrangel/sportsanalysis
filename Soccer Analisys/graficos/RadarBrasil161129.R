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
  team <- ask("Select an option? 1 - Bra x Per, 2 - Bra x Col, 3 - Bra x Ven, 4 - Bra x Par, 5 - All")
  team <- as.numeric(team)
  return (team)
} 

lista_sdo <-  c("CB", "CD", "CM", "F", "PB", "PD", "PM") 

#team = 2
team <- inputTeam()

cat("You chose", team)

titulo = ""



bra_per = read.csv(paste(getwd(),"/inputcsv/","Brasil x Peru Jogo 1.csv", sep=""), sep = ";")
bra_col = read.csv(paste(getwd(),"/inputcsv/","Brasil x Colombia Jogo 2.csv", sep=""), sep = ";")
bra_ven = read.csv(paste(getwd(),"/inputcsv/","Brasil x Venezuela Jogo 3.csv", sep=""), sep = ";")
bra_par = read.csv(paste(getwd(),"/inputcsv/","Brasil x Paraguai Jogo 4.csv", sep=""), sep = ";")

bra_per <- bra_per[c("SDO")]
bra_col <- bra_col[c("SDO")]
bra_ven <- bra_ven[c("SDO")]
bra_par <- bra_par[c("SDO")]

bra_per[bra_per==""] <- NA
bra_col[bra_col==""] <- NA
bra_ven[bra_ven==""] <- NA
bra_par[bra_par==""] <- NA
names(bra_per)

bra_per_freq_temp = count(bra_per, c('SDO'))
bra_col_freq_temp <- count(bra_col, c('SDO' ))
bra_ven_freq_temp <- count(bra_ven, c('SDO' ))
bra_par_freq_temp <- count(bra_par, c('SDO' ))

bra_per_freq <- na.omit(bra_per_freq_temp)
bra_col_freq <- na.omit(bra_col_freq_temp)
bra_ven_freq <- na.omit(bra_ven_freq_temp)
bra_par_freq <- na.omit(bra_par_freq_temp)


matrix_radar <- matrix(0, 4, 7)
#matrix_radar[1,] = lista_sdo
colnames(matrix_radar) <- lista_sdo




for(i in 1:length(lista_sdo)) {
  
  if (team == 1 || team == 5){
    temp = bra_per_freq[bra_per_freq$SDO == lista_sdo[i], ]
    if (nrow(temp) > 0){
      result <- temp$freq
      matrix_radar[1, lista_sdo[i]] = result
    }
    colors_border=c( "red" )
    colors_in=c( "darkred" )
    titulo = "Brasil x Peru"
    
  }
  if (team == 2 || team == 5){
    temp = bra_col_freq[bra_col_freq$SDO == lista_sdo[i], ]
    if (nrow(temp) > 0){
      result <- temp$freq
      matrix_radar[2, lista_sdo[i]] = result
    }
    
    colors_border=c("blue")
    colors_in=c( "darkblue")
    titulo = "Brasil x Colômbia"
  }
  if (team == 3 || team == 5){
    temp = bra_ven_freq[bra_ven_freq$SDO == lista_sdo[i], ]
    if (nrow(temp) > 0){
      result <- temp$freq
      matrix_radar[3, lista_sdo[i]] = result
    }
    
    colors_border=c( "yellow")
    colors_in=c( "orange")
    titulo = "Brasil x Venezuela"
  }
  
  if (team == 4 || team == 5){
    temp = bra_par_freq[bra_par_freq$SDO == lista_sdo[i], ]
    if (nrow(temp) > 0){
      result <- temp$freq
      matrix_radar[4, lista_sdo[i]] = result
    }
    
    colors_border=c( "green" )
    colors_in=c("darkgreen" )
    titulo = "Brasil x Paraguai"
  }
  
  
  
  
}

if (team == 5){
  colors_border=c( "red", "blue" , "yellow", "green" )
  colors_in=c( "darkred", "darkblue" , "orange" , "darkgreen" )
  
}



if (team < 5){
  
  
  data=as.data.frame(matrix_radar)
  colnames(data)=lista_sdo
  rownames(data)[1] = titulo
  
  # To use the fmsb package, I have to add 2 lines to the dataframe: the max and min of each topic to show on the plot!
  data=rbind(rep(77,2) , rep(0,2) , data)
  
  #browser()
  
  
  #==================
  # Plot 1: Default radar chart proposed by the library:
  radarchart(data)
  
  # Plot 2: Same plot with custom features
  
  radarchart( data  , axistype=1, 
              #custom polygon
              pcol=colors_border , pfcol=colors_in,  plwd=4 , plty=1,
              #custom the grid
              cglcol="navy", cglty=1, axislabcol="grey", maxmin = TRUE,  cglwd=0.8, seg = 10,
              #custom labels
              vlcex=0.8 
  )
  legend(x=1, y=1.2, legend = c(titulo), 
         bty = "n", pch=20 , col=colors_border , text.col = "black", cex=2, pt.cex=3)
  
  
}else if (team == 5){
  titulo = "All"
  
  
  
  data=as.data.frame(matrix_radar)
  colnames(data)=lista_sdo
  rownames(data)[1] = "Brasil x Peru"
  rownames(data)[2] = "Brasil x Colombia"
  rownames(data)[3] = "Brasil x Venezuela"
  rownames(data)[4] = "Brasil x Paraguai"
  
  # To use the fmsb package, I have to add 2 lines to the dataframe: the max and min of each topic to show on the plot!
  data=rbind(rep(77,2) , rep(0,2) , data)
  
  
  
  
  #==================
  # Plot 1: Default radar chart proposed by the library:
  radarchart(data)
  
  # Plot 2: Same plot with custom features
  
  radarchart( data  , axistype=1, 
              #custom polygon
              pcol=colors_border , pfcol=colors_in,  plwd=4 , plty=1,
              #custom the grid
              cglcol="navy", cglty=2, axislabcol="grey", maxmin = TRUE,  cglwd=0.8, seg = 10,
              #custom labels
              vlcex=0.8 
  )
  legend(x=1, y=1.2, legend = c("Brasil x Peru", "Brasil x Colombia", "Brasil x Venezuela", "Brasil x Paraguai"), 
         bty = "n", pch=20 , col=colors_border , text.col = "black", cex=2, pt.cex=3)
  
}

