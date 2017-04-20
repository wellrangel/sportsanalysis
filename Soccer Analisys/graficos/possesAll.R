  # plotly library
  library(plotly)
  library(tidyr)
  library(plyr)
  
  
  
  askstr = ""
  tempFiles = list.files(path=paste(getwd(),"/inputcsv/",sep=""), pattern="*.csv")
  for (ifile in 1:length(tempFiles)){
    askstr = paste(askstr, ifile, " - ", tempFiles[ifile], "\n",sep="")
    
  }
  
  
  inputTeam <- function()
  {
    team <- ask(paste("Select a time?\n\n", askstr))
    team <- as.numeric(team)
    return (team)
  } 
  
  team <- inputTeam()
  #team = 1
  cat("You chose", team)
  
  
  titulo =strsplit(tempFiles[team], '[.]')[[1]][1]
  titulo = sub('"', '', titulo)
  
  dados = read.csv(paste(getwd(),"/inputcsv/",tempFiles[team], sep=""), sep = ";")
  set.seed(100)
  options(stringsAsFactors = FALSE)
  
  
  dados <- dados[c("SDO")]
  dados[is.na(dados)] <- 0
  dados$symbol = 0
  dados$symbolcolor = "black"
  
  if (nrow(dados[dados$SDO == "CB", ]) > 0){
    dados[dados$SDO == "CB", ]$symbol = 1
    dados[dados$SDO == "CB", ]$symbolcolor = "red"
  }
  if (nrow(dados[dados$SDO == "CD", ]) > 0){
    dados[dados$SDO == "CD", ]$symbol = 2
    dados[dados$SDO == "CD", ]$symbolcolor = "green"
  }
  if (nrow(dados[dados$SDO == "CM", ]) > 0){
    dados[dados$SDO == "CM", ]$symbol = 3
    dados[dados$SDO == "CM", ]$symbolcolor = "yellow"
  }
  if (nrow(dados[dados$SDO == "F", ]) > 0){
    dados[dados$SDO == "F", ]$symbol = 4
    dados[dados$SDO == "F", ]$symbolcolor = "blue"
  }
  if (nrow(dados[dados$SDO == "PB", ]) > 0){
    dados[dados$SDO == "PB", ]$symbol = 5
    dados[dados$SDO == "PB", ]$symbolcolor = "pink"
  }
  if (nrow(dados[dados$SDO == "PD", ]) > 0){
    dados[dados$SDO == "PD", ]$symbol = 6
    dados[dados$SDO == "PD", ]$symbolcolor = "black"
  }
  if (nrow(dados[dados$SDO == "PM", ]) > 0){
    dados[dados$SDO == "PM", ]$symbol = 7
    dados[dados$SDO == "PM", ]$symbolcolor = "orange"
  }
  
  matrix <- data.frame(
    Var1=integer(),
    Var2=integer(),
    sdo=character(),
    symbol=integer(),
    symbolcolor=character(),
    stringsAsFactors=FALSE)
  
  
  indiceX = 1
  indiceY = 0
  
  iniciou = FALSE
  for(i in 1:nrow(dados)) {
    myRow <- matrix[1,]
    rowAtual <- dados[i,]
    
    if (rowAtual$symbol > 0){
      iniciou = TRUE
      indiceY = indiceY + 1
      
      myRow$Var1 = indiceX
      myRow$Var2 = indiceY
      myRow$sdo = rowAtual$SDO
      myRow$symbol = rowAtual$symbol
      myRow$symbolcolor = rowAtual$symbolcolor
      
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
  
  
  pal <- c("red", "blue", "green", "black", "yellow", "pink", "orange")
  # Make the graph
  plot_ly(matrix , x=~Var2 , y=~Var1 , 
          type="scatter", mode="markers" , hoverinfo="text" , 
          text=~sdo , textposition = "bottom center",
          color = ~sdo,
          colors = pal,
          marker=list(symbol=~sdo, 
                      size=8 , opacity=1) )%>% 
    
    layout(
      title= titulo,
      hovermode="closest",
      yaxis = list(title="Poss number", range = matrix$Var1, dtick = 1),
      #yaxis=list(autorange="reversed" , title="Poss number" , tickfont=list(color="white"), range = c(2, 5) ) ,
      xaxis=list( title="SDO Sequence" , tickfont=list(color="white")),
      legend = l
      
    )
  
 
  #} 
  
  