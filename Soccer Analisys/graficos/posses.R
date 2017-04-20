
# plotly library
library(plotly)

bra_per = read.csv("Brasil Jogo 2 x Colombia.csv", sep = ";")
bra_per <- bra_per[c("SDO")]
bra_per[is.na(bra_per)] <- 0
bra_per$symbol = 0

if (nrow(bra_per[bra_per$SDO == "CB", ]) > 0){
  bra_per[bra_per$SDO == "CB", ]$symbol = 1
}
if (nrow(bra_per[bra_per$SDO == "CD", ]) > 0){
  bra_per[bra_per$SDO == "CD", ]$symbol = 2
}
if (nrow(bra_per[bra_per$SDO == "CM", ]) > 0){
  bra_per[bra_per$SDO == "CM", ]$symbol = 3
}
if (nrow(bra_per[bra_per$SDO == "F", ]) > 0){
  bra_per[bra_per$SDO == "F", ]$symbol = 4
}
if (nrow(bra_per[bra_per$SDO == "PB", ]) > 0){
  bra_per[bra_per$SDO == "PB", ]$symbol = 5
}
if (nrow(bra_per[bra_per$SDO == "PD", ]) > 0){
  bra_per[bra_per$SDO == "PD", ]$symbol = 6
}
if (nrow(bra_per[bra_per$SDO == "PM", ]) > 0){
  bra_per[bra_per$SDO == "PM", ]$symbol = 7
}

matrix <- data.frame(
  Var1=integer(),
  Var2=integer(),
  sdo=character(),
  symbol=integer(),
  stringsAsFactors=FALSE)


indiceX = 1
indiceY = 0

iniciou = FALSE
for(i in 1:nrow(bra_per)) {
  myRow <- matrix[1,]
  rowAtual <- bra_per[i,]
  
  if (rowAtual$symbol > 0){
      iniciou = TRUE
      indiceY = indiceY + 1
    
    myRow$Var1 = indiceX
    myRow$Var2 = indiceY
    myRow$sdo = rowAtual$SDO
    myRow$symbol = rowAtual$symbol
  
    matrix <- rbind(matrix, myRow)   
  }else{
    if (iniciou){
      indiceX = indiceX + 1
      indiceY = 0
    }
  }
  
}

l <- list(
  font = list(
    family = "sans-serif",
    size = 12,
    color = "#000"),
  bgcolor = "#E2E2E2",
  bordercolor = "#FFFFFF",
  borderwidth = 2)

# Make the graph
my_graph=plot_ly(matrix , x=~Var2 , y=~Var1 , 
                 type="scatter", mode="markers+text" , hoverinfo="text" , 
                 text=~sdo , textposition = "bottom center",
                 marker=list(symbol=~symbol , 
                size=15 , color="red" , opacity=0.5) )%>% 

  layout(
    title= "Brasil x Colombia",
    hovermode="closest",
    yaxis = list(title="Poss number", range = matrix$Var1, dtick = 1),
    #yaxis=list(autorange="reversed" , title="Poss number" , tickfont=list(color="white"), range = c(2, 5) ) ,
    xaxis=list( title="SDO Sequence" , tickfont=list(color="white")),
    legend = l
  )

# Show graph
my_graph