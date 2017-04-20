library(grid)
library(lattice)
library(fmsb)
library(plyr)
library(party)
library(reshape2)
library(gplots)
library(gtools)
require(plyr)


inputTeam <- function()
{
  team <- ask("Select an option? 1 - Bra x Per, 2 - Bra x Col, 3 - Bra x Ven, 4 - Bra x Par")
  team <- as.numeric(team)
  return (team)
} 

options(stringsAsFactors = FALSE)

team <- inputTeam()
#team = 1
cat("You chose", team)
titulo = ""
if (team == 1){
  titulo = "Brasil x Peru"
  #abre arquivo csv
  dados = read.csv("Brasil Jogo 1 x Peru.csv", sep = ";")
  
}else if (team == 2){
  titulo = "Brasil x Colômbia"
  #abre arquivo csv
  dados = read.csv("Brasil Jogo 2 x Colombia.csv", sep = ";")
}else if (team == 3){
  titulo = "Brasil x Venezuela"
  #abre arquivo csv
  dados = read.csv("Brasil Jogo 3 x Venezuela.csv", sep = ";")
  
}else if (team == 4){
  titulo = "Brasil x Paraguai"
  #abre arquivo csv
  dados = read.csv("Brasil Jogo 4 x Paraguai.csv", sep = ";")
  
}


#lista de todas os possiveis SDOs
lista_sdo <-  c("F","PM","CM", "PD","CD", "PB", "CB") 
#transforma em matrix para contar zero em todos os sdos
matrixAllSDO <- ldply (lista_sdo, data.frame)
#ajusta nome das colunas
colnames(matrixAllSDO) <- c("SDO")
matrixAllSDO = count(matrixAllSDO, c('SDO'))
matrixAllSDO[matrixAllSDO=="1"] <- 0

#seleciona somente as colunas necessarias
dados <- dados[c("SDO")]
#zera valores com NA(vazios)
#dados[is.na(dados)] <- 0

dados[dados==""] <- NA
dados[is.na(dados)] <- ""

#ajusta nome das colunas
colnames(dados) <- c("SDO")


dados_freq_temp = count(dados, c('SDO'))
dados_freq_temp[dados_freq_temp==""] <- NA
dados_freq <- na.omit(dados_freq_temp)
colnames(dados_freq) <- c("SDO", "freq")

dados_freq = merge(x = dados_freq, y = matrixAllSDO, by = c("SDO") , all.y = TRUE)

dados_freq[is.na(dados_freq)] <- 0


dados_freq <- dados_freq[c("SDO", "freq.x")]
colnames(dados_freq) <- c("SDO","freq")



pesos <-dados_freq[["freq"]]


sdo <-dados_freq[["SDO"]]

# gera matriz from to

matrixFT <- data.frame(
  from=integer(),
  to=integer())

for(i in 1:nrow(dados)) {
  myRow <- matrixFT[1,]
  rowAtual <- dados[i,]
  
  strFrom <- ""
  strTo <- ""
  
  if (i < nrow(dados)){
    if (!is.na(rowAtual)){
      strFrom <- rowAtual
      rowNext <- dados[1+i,]
      strTo <- rowNext
      
      myRow$from = strFrom
      myRow$to = strTo
      
      matrixFT <- rbind(matrixFT, myRow)   
      
    }
    
  }
  
}


matrixFT_temp = count(matrixFT, c('from', 'to'))
matrixFT_freq <- na.omit(matrixFT_temp)

matrixFT_freq[matrixFT_freq==""] <- NA
matrixFT_freq <- na.omit(matrixFT_freq)


#retirando totais para o mesmo no
#for(i in 1:NROW(pesos)) {
# if (nrow(matrixFT_freq[matrixFT_freq$from == sdo[i] & matrixFT_freq$to == sdo[i],])> 0){
#  pesos[i] = pesos[i] - matrixFT_freq[matrixFT_freq$from == sdo[i] & matrixFT_freq$to == sdo[i],]$freq
#}

#}

#matrixFT_freq <- matrixFT_freq[order(-matrixFT_freq$freq), ]

pesos <-dados_freq[["freq"]]
dados_freq <- dados_freq[order(dados_freq$SDO), ]

dados_freq['x'] = c(6,4,2,0,5,3,1)
dados_freq['y'] = c(3,4,3,4,2,5,3)
dados_freq['myColor'] = c("grey", "blue", "cyan", "red", 
                          "yellow", "orange", "pink")

xvalues = dados_freq[['x']]
yvalues = dados_freq[['y']]
myColor = dados_freq[['myColor']]

dados_freq <- dados_freq[order(dados_freq$freq), ]

dat <- data.frame(x=xvalues,
                  y=yvalues,
                  weight=pesos,
                  text=sdo
                  , stringsAsFactors=FALSE)

cols <- colorRampPalette(myColor)(nrow(dat))

xyplot(y~x,data=dat,groups=weight,
       xlim=extendrange(dat$x,f=.1),
       ylim=extendrange(dat$y,f=.15),
       scales=list(x=list(draw=FALSE), y=list(draw=FALSE)),
       main=titulo, 
       panel=function(x,y,groups,...){
         
         if (team == 1){
           correcaoRaio = .006
           
         }else if (team == 2){
           correcaoRaio = .016  
         }else if (team == 3){
           correcaoRaio = .016  
         }else if (team == 4){
           correcaoRaio = .016  
         }
         
         lapply(seq_along(x),function(i){
           
           grid.circle(x[i],y[i],
                       r=correcaoRaio*groups[i],
                       gp=gpar(fill=cols[i],lwd=2),
                       def='native')
           
           #grid.text(x[i],y[i],label=paste(dat$text[i],paste(x[i], y[i])),
           grid.text(x[i],y[i],label=dat$text[i],
                     gp=gpar(cex=1,col='black'),
                     def='native')
         })
         
         
         
         #browser()
         
         #for(i in 1:14) {
         for(i in 1:nrow(matrixFT_freq)) {
           
           rowFreqAtual <- matrixFT_freq[i,]
           indiceFrom = which(sdo == as.character(rowFreqAtual$from))
           indiceTo = which(sdo == as.character(rowFreqAtual$to))
           pesoSeta = rowFreqAtual$freq
           
           indiceMatrixFromX = dat[indiceFrom,]$x
           indiceMatrixToX = dat[indiceTo,]$x
           
           indiceMatrixFromY = dat[indiceFrom,]$y
           indiceMatrixToY = dat[indiceTo,]$y
           
           
           ajusteSetaTo = correcaoRaio*groups[indiceTo]
           ajusteSetaFrom = correcaoRaio*groups[indiceFrom]
           ajusteCurva = -.8;
           
           
           if (indiceFrom != indiceTo){
             
             if (indiceMatrixFromX > indiceMatrixToX && indiceMatrixFromY < indiceMatrixToY){
               #seta voltando cima
               
               xx <- c(x[indiceFrom], x[indiceFrom]-ajusteSetaFrom, 
                       x[indiceTo], x[indiceTo])
               
               yy <- c(y[indiceFrom]+ajusteSetaFrom, y[indiceFrom]+ajusteSetaFrom, 
                       y[indiceTo]-ajusteSetaTo, y[indiceTo]-ajusteSetaTo)
               
             }else if (indiceMatrixFromX > indiceMatrixToX && indiceMatrixFromY == indiceMatrixToY){
               #seta voltando mesma altura
               
               xx <- c(x[indiceFrom]-ajusteSetaFrom/2, x[indiceFrom], 
                       x[indiceTo]+ajusteSetaTo, x[indiceTo]+ajusteSetaTo/2)
               
               yy <- c(y[indiceFrom], y[indiceFrom], 
                       y[indiceTo], y[indiceTo])
               
             }else if (indiceMatrixFromX > indiceMatrixToX && indiceMatrixFromY > indiceMatrixToY){
               #seta voltando baixo
               
               xx <- c(x[indiceFrom], x[indiceFrom]+ajusteSetaFrom, 
                       x[indiceTo]+ajusteSetaTo, x[indiceTo]+ajusteSetaTo)
               yy <- c(y[indiceFrom]-ajusteSetaFrom, y[indiceFrom]-ajusteSetaFrom+ajusteCurva, 
                       y[indiceTo]-ajusteSetaTo/2, y[indiceTo]-ajusteSetaTo/2)
               
               
             }else if (indiceMatrixFromX == indiceMatrixToX && indiceMatrixFromY > indiceMatrixToY){
               #seta mesmo lado baixo
               
               xx <- c(x[indiceFrom], x[indiceFrom]-ajusteSetaFrom, 
                       x[indiceTo]-ajusteSetaTo, x[indiceTo]-ajusteSetaTo)
               
               yy <- c(y[indiceFrom]+ajusteSetaFrom, y[indiceFrom]+ajusteCurva, 
                       y[indiceTo], y[indiceTo])
               
             }else if (indiceMatrixFromX < indiceMatrixToX && indiceMatrixFromY > indiceMatrixToY){
               #seta indo baixo
               ajusteCurva = ajusteCurva*-1
               #browser()
               xx <- c(x[indiceFrom], x[indiceFrom], 
                       x[indiceTo], x[indiceTo])
               
               yy <- c(y[indiceFrom]-ajusteSetaFrom, y[indiceFrom]-ajusteSetaFrom+ajusteCurva, 
                       y[indiceTo]+ajusteSetaTo, y[indiceTo]+ajusteSetaTo)
               
             }else if (indiceMatrixFromX < indiceMatrixToX && indiceMatrixFromY == indiceMatrixToY){
               #seta indo mesma altura
               
               xx <- c(x[indiceFrom]+ajusteSetaFrom, x[indiceFrom], 
                       x[indiceTo]-ajusteSetaTo, x[indiceTo]-ajusteSetaTo)
               
               yy <- c(y[indiceFrom], y[indiceFrom]+ajusteCurva, 
                       y[indiceTo], y[indiceTo])
               
             }else if (indiceMatrixFromX < indiceMatrixToX && indiceMatrixFromY < indiceMatrixToY){
               #seta indo cima
               
               xx <- c(x[indiceFrom]+ajusteSetaFrom/2, x[indiceFrom], 
                       x[indiceTo]-ajusteSetaTo, x[indiceTo]-ajusteSetaTo) #altura
               
               yy <- c(y[indiceFrom], y[indiceFrom], 
                       y[indiceTo], y[indiceTo]) # altura
               
             }else if (indiceMatrixFromX == indiceMatrixToX && indiceMatrixFromY < indiceMatrixToY){
               #seta mesmo lado cima
               
               xx <- c(x[indiceFrom], x[indiceFrom]-ajusteSetaFrom, 
                       x[indiceTo]-ajusteSetaTo, x[indiceTo]-ajusteSetaTo)
               yy <- c(y[indiceFrom]+ajusteSetaFrom, y[indiceFrom]+ajusteCurva, 
                       y[indiceTo], y[indiceTo])
               
             }
             
             corSeta = cols[indiceFrom]
             grid.bezier(xx, yy,
                         gp=gpar(lwd=pesoSeta*.7, col=corSeta),
                         arrow=arrow( angle=20, type="open", length=unit(0.1, "inches")),
                         def='native')
             
             
           }else{
             #seta para o proprio no
             
             xx <- c(x[indiceFrom], x[indiceFrom]-ajusteSetaFrom, 
                     x[indiceTo]-ajusteSetaTo, x[indiceTo]-ajusteSetaTo)
             yy <- c(y[indiceFrom]+ajusteSetaFrom, y[indiceFrom]+1.9, 
                     y[indiceTo], y[indiceTo])
             
             corSeta = cols[indiceFrom]
             grid.bezier(xx, yy,
                         gp=gpar(lwd=pesoSeta*.1, col=corSeta),
                         arrow=arrow( angle=20, type="open", length=unit(0.2, "inches")),
                         def='native')
             
           }
           
           #ajustar angulo da seta para o proprio no
           #
           
           
         }
         
         
         
       })



#if (indiceMatrixFromY > indiceMatrixToY ){
#   xx <- c(x[indiceFrom], x[indiceFrom]-ajusteSetaFrom, 
#           x[indiceTo], x[indiceTo]-ajusteSetaTo)
#   yy <- c(y[indiceFrom]+ajusteSetaFrom, y[indiceFrom]+ajusteCurva, 
#           y[indiceTo]+ajustaLadoTo, y[indiceTo]+ajustaLadoTo)
# }

#xx <- c(x[indiceFrom], x[indiceFrom], 
#       x[indiceTo], x[indiceTo])
#yy <- c(y[indiceFrom], y[indiceFrom]+ajusteCurva, 
#        y[indiceTo], y[indiceTo])

#print (matrixFT_freq$from)
