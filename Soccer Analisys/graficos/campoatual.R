library(grid)
library(lattice)
library(fmsb)
library(plyr)
library(party)
library(reshape2)
library(gplots)
library(gtools)
require(plyr)


#lista de todas os possiveis SDOs
lista_sdo <-  c("F","PML","PMC","PMR", "CM", "PDL", "PDC", "PDR","CD", "PBL", "PBC", "PBR", "CB") 
#transforma em matrix para contar zero em todos os sdos
matrixAllSDO <- ldply (lista_sdo, data.frame)
#ajusta nome das colunas
colnames(matrixAllSDO) <- c("SDO2")
matrixAllSDO = count(matrixAllSDO, c('SDO2'))
matrixAllSDO[matrixAllSDO=="1"] <- 0

#abre arquivo csv
bra_per = read.csv("Brasil Jogo 3 x Venezuela.csv", sep = ";")
#seleciona somente as colunas necessarias
bra_per <- bra_per[c("ESC", "SDO")]
#zera valores com NA(vazios)
#bra_per[is.na(bra_per)] <- 0

bra_per[bra_per==""] <- NA
bra_per[is.na(bra_per)] <- ""

bra_per["SDO2"] <- NA
# soma as colunas SDO com ESC para obter a informacao de esquerda e direita
bra_per["SDO2"] = paste(bra_per$SDO, substring(bra_per$ESC, 2), sep="" )


#ajusta nome das colunas
colnames(bra_per) <- c("ESC","SDO", "SDO2")
#agora retira a informacao de lado para os seguintes SDO: F, CM, CD e CB

names(bra_per)

bra_per[bra_per$SDO2=="FR",] = "F"
bra_per[bra_per$SDO2=="FC",] = "F"
bra_per[bra_per$SDO2=="FL",] = "F"

bra_per[bra_per$SDO2=="CMC",] = "CM"
bra_per[bra_per$SDO2=="CMR",] = "CM"
bra_per[bra_per$SDO2=="CML",] = "CM"

bra_per[bra_per$SDO2=="CDC",] = "CD"
bra_per[bra_per$SDO2=="CDR",] = "CD"
bra_per[bra_per$SDO2=="CDL",] = "CD"

bra_per[bra_per$SDO2=="CBC",] = "CB"
bra_per[bra_per$SDO2=="CBR",] = "CB"
bra_per[bra_per$SDO2=="CBL",] = "CB"

bra_per_freq_temp = count(bra_per, c('SDO2'))
bra_per_freq_temp[bra_per_freq_temp==""] <- NA
bra_per_freq <- na.omit(bra_per_freq_temp)

bra_per_freq = merge(x = bra_per_freq, y = matrixAllSDO, by = c("SDO2") , all.y = TRUE)

bra_per_freq[is.na(bra_per_freq)] <- 0


bra_per_freq <- bra_per_freq[c("SDO2", "freq.x")]
colnames(bra_per_freq) <- c("SDO","freq")


bra_per_freq <- bra_per_freq[order(bra_per_freq$SDO), ]
pesos <-bra_per_freq[["freq"]]


sdo <-bra_per_freq[["SDO"]]

# gera matriz from to

matrixFT <- data.frame(
  from=integer(),
  to=integer(),
  stringsAsFactors=FALSE)

for(i in 1:nrow(bra_per)) {
  myRow <- matrixFT[1,]
  rowAtual <- bra_per[i,]
  
  strFrom <- ""
  strTo <- ""
  
  if (i < nrow(bra_per)){
    if (!is.na(rowAtual$SDO2)){
      strFrom <- rowAtual$SDO2
      rowNext <- bra_per[1+i,]
      strTo <- rowNext$SDO2
      
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
for(i in 1:NROW(pesos)) {
  if (nrow(matrixFT_freq[matrixFT_freq$from == sdo[i] & matrixFT_freq$to == sdo[i],])> 0){
    pesos[i] = pesos[i] - matrixFT_freq[matrixFT_freq$from == sdo[i] & matrixFT_freq$to == sdo[i],]$freq
  }
  
}

                  


dat <- data.frame(x=c(6,4,2,0,5,5,  5, 3, 3, 3, 1, 1, 1),
                  y=c(4,5,4,4,3,10,-3, 4, 9, -2, 4, 7, -1),
                  weight=pesos,
                  text=sdo)

cols <- colorRampPalette(c("grey", "darkgreen", "blue", "cyan", "red", "black"
                           , "yellow", "orange", "violetred", "darkred", "darkblue", 
                           "white", "pink"))(nrow(dat))

xyplot(y~x,data=dat,groups=weight,
       xlim=extendrange(dat$x,f=.2),
       ylim=extendrange(dat$y,f=.20),
       scales=list(x=list(draw=FALSE), y=list(draw=FALSE)),
       main="Brasil x Venezuela", 
       panel=function(x,y,groups,...){
         
         library(png)
         m <- readPNG( "campo.png")
         rimg <- as.raster(m)
         grid.raster(rimg, x=0.5, y=1, just="top", width=1)
         
         correcaoRaio = .13
         lapply(seq_along(x),function(i){
           
           grid.circle(x[i],y[i],
                       r=correcaoRaio*groups[i],
                       gp=gpar(fill=cols[i],lwd=2),
                       def='native')
           
           #grid.text(x[i],y[i],label=paste(dat$text[i],paste(x[i], y[i])),
           grid.text(x[i],y[i],label=dat$text[i],
                     gp=gpar(cex=.7,col='black'),
                     def='native')
         })
         
         
         
         ajustaLadoTo = 0;
         for(i in 1:14) {
         #   for(i in 1:nrow(matrixFT_freq)) {
           
           rowFreqAtual <- matrixFT_freq[i,]
           indiceFrom = which(sdo == rowFreqAtual$from)
           indiceTo = which(sdo == rowFreqAtual$to)
           pesoSeta = rowFreqAtual$freq
           
           indiceMatrixFromX = dat[indiceFrom,]$x
           indiceMatrixToX = dat[indiceTo,]$x
           
           indiceMatrixFromY = dat[indiceFrom,]$y
           indiceMatrixToY = dat[indiceTo,]$y
           
           #browser()
           ajusteSetaTo = correcaoRaio*groups[indiceTo]
           ajusteSetaFrom = correcaoRaio*groups[indiceFrom]
           ajusteCurva = 0;
           
           
           if (indiceFrom != indiceTo){
             
             if (indiceMatrixFromX > indiceMatrixToX && indiceMatrixFromY < indiceMatrixToY){
               #seta voltando cima
               
               xx <- c(x[indiceFrom], x[indiceFrom]-ajusteSetaFrom, 
                       x[indiceTo], x[indiceTo])
               
               yy <- c(y[indiceFrom]+ajusteSetaFrom, y[indiceFrom], 
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
                       x[indiceTo], x[indiceTo])
               yy <- c(y[indiceFrom]-ajusteSetaFrom, y[indiceFrom]+ajusteCurva, 
                       y[indiceTo]+ajusteSetaTo, y[indiceTo]+ajusteSetaTo)
               
               
             }else if (indiceMatrixFromX == indiceMatrixToX && indiceMatrixFromY > indiceMatrixToY){
               #seta mesmo lado baixo
               
               xx <- c(x[indiceFrom], x[indiceFrom]-ajusteSetaFrom, 
                       x[indiceTo]-ajusteSetaTo, x[indiceTo]-ajusteSetaTo)
               
               yy <- c(y[indiceFrom]+ajusteSetaFrom, y[indiceFrom]+ajusteCurva, 
                       y[indiceTo]+ajustaLadoTo, y[indiceTo]+ajustaLadoTo)
               
             }else if (indiceMatrixFromX < indiceMatrixToX && indiceMatrixFromY > indiceMatrixToY){
               #seta indo baixo
               #browser()
               xx <- c(x[indiceFrom], x[indiceFrom], 
                       x[indiceTo], x[indiceTo]) #altura
               
               yy <- c(y[indiceFrom]-ajusteSetaFrom, y[indiceFrom]+ajusteCurva, 
                       y[indiceTo]+ajusteSetaTo, y[indiceTo]+ajusteSetaTo)
               
             }else if (indiceMatrixFromX < indiceMatrixToX && indiceMatrixFromY == indiceMatrixToY){
               #seta indo mesma altura
               
               xx <- c(x[indiceFrom]+1, x[indiceFrom], 
                       x[indiceTo]+ajusteSetaTo, x[indiceTo]+ajusteSetaTo)
               
               yy <- c(y[indiceFrom], y[indiceFrom]+ajusteCurva, 
                       y[indiceTo]+ajustaLadoTo, y[indiceTo]+ajustaLadoTo)
               
             }else if (indiceMatrixFromX < indiceMatrixToX && indiceMatrixFromY < indiceMatrixToY){
               #seta indo cima
               
               xx <- c(x[indiceFrom]+ajusteSetaFrom/2, x[indiceFrom], 
                       x[indiceTo]-ajusteSetaTo/2, x[indiceTo]-ajusteSetaTo/2) #altura
               
               yy <- c(y[indiceFrom], y[indiceFrom], 
                       y[indiceTo], y[indiceTo]) # altura
               
             }else if (indiceMatrixFromX == indiceMatrixToX && indiceMatrixFromY < indiceMatrixToY){
               #seta mesmo lado cima
               
               xx <- c(x[indiceFrom], x[indiceFrom]-ajusteSetaFrom, 
                       x[indiceTo]-ajusteSetaTo, x[indiceTo]-ajusteSetaTo)
               yy <- c(y[indiceFrom]+ajusteSetaFrom, y[indiceFrom]+ajusteCurva, 
                       y[indiceTo]+ajustaLadoTo, y[indiceTo]+ajustaLadoTo)
               
             }
         
             corSeta = cols[indiceFrom]
             grid.bezier(xx, yy,
                         gp=gpar(lwd=pesoSeta, col=corSeta),
                         arrow=arrow( angle=20, type="open", length=unit(0.2, "inches")),
                         def='native')
           }
           
           
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
