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

bra_per = merge(x = bra_per, y = matrixAllSDO, by = c("SDO2") , all.y = TRUE)


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







pesos <- as.list(bra_per_freq['freq'])



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
      if (!is.na(rowAtual)){
        strFrom <- rowAtual
        rowNext <- bra_per[1+i,]
        strTo <- rowNext
        
        myRow$from = strFrom
        myRow$to = strTo
        
        matrixFT <- rbind(matrixFT, myRow)   
        
      }
      
    }
    
}


matrixFT_temp = count(matrixFT, c('from', 'to'))
matrixFT_freq <- na.omit(matrixFT_temp)


dat <- data.frame(x=c(0,2,2,2,4,4,4),
                  y=c(2,1,2,3,1,2,3),
                  weight=pesos,
                  text=lista_sdo)

cols <- colorRampPalette(c("grey", "green", "blue", "cyan", "red"))(nrow(dat))

xyplot(y~x,data=dat,groups=weight,
       xlim=extendrange(dat$x,f=0.2),
       ylim=extendrange(dat$y,f=0.5),
       main="Brasil x Venezuela", 
       panel=function(x,y,groups,...){

         library(png)
         m <- readPNG( "campo.png")
         rimg <- as.raster(m)
         grid.raster(rimg, x=0.5, y=1, just="top", width=.8)
         
         
         lapply(seq_along(x),function(i){
           
           grid.circle(x[i],y[i],
                          r=.01*groups[i],
                          gp=gpar(fill=cols[i],lwd=2,col='blue'),
                          def='native')
           
           #grid.text(x[i],y[i],label=paste(dat$text[i],paste(x[i], y[i])),
           grid.text(x[i],y[i],label=dat$text[i],
                     gp=gpar(cex=1,col='white'),
                     def='native')
         })
         
         
         
         ajustaLadoTo = 0;
         for(i in 1:13) {
         # for(i in 1:nrow(matrixFT_freq)) {
            
            rowFreqAtual <- matrixFT_freq[i,]
            indiceFrom = which(lista_sdo == rowFreqAtual$from)
            indiceTo = which(lista_sdo == rowFreqAtual$to)
            pesoSeta = rowFreqAtual$freq
            
            indiceMatrixFromX = dat[indiceFrom,]$x
            indiceMatrixToX = dat[indiceTo,]$x
            
            indiceMatrixFromY = dat[indiceFrom,]$y
            indiceMatrixToY = dat[indiceTo,]$y
            
            #browser()
            ajusteSetaTo = .01*groups[indiceTo];
            ajusteSetaFrom = .01*groups[indiceFrom];
            ajusteCurva = .5;
            
            #browser()
            if (indiceFrom != indiceTo){
              if (indiceMatrixFromX > indiceMatrixToX || indiceMatrixFromY > indiceMatrixToY){
                ajusteSetaTo = ajusteSetaTo*-1
                ajusteSetaFrom = ajusteSetaFrom*-1
                ajusteCurva = ajusteCurva*-1
                ajustaLadoTo = ajustaLadoTo - 0.025
              }else{
                ajustaLadoTo = ajustaLadoTo + 0.025
              }
              
              if (indiceMatrixFromX > indiceMatrixToX ){
                 xx <- c(x[indiceFrom]+ajusteSetaFrom, x[indiceFrom], 
                      x[indiceTo]-ajusteSetaTo, x[indiceTo]-ajusteSetaTo)
                  yy <- c(y[indiceFrom], y[indiceFrom]+ajusteCurva, 
                      y[indiceTo]+ajustaLadoTo, y[indiceTo]+ajustaLadoTo)
              }
              
              if (indiceMatrixFromY > indiceMatrixToY ){
                xx <- c(x[indiceFrom], x[indiceFrom]-ajusteSetaFrom, 
                        x[indiceTo], x[indiceTo]-ajusteSetaTo)
                yy <- c(y[indiceFrom]+ajusteSetaFrom, y[indiceFrom]+ajusteCurva, 
                        y[indiceTo]+ajustaLadoTo, y[indiceTo]+ajustaLadoTo)
              }
              
              #xx <- c(x[indiceFrom], x[indiceFrom], 
               #       x[indiceTo], x[indiceTo])
              #yy <- c(y[indiceFrom], y[indiceFrom]+ajusteCurva, 
              #        y[indiceTo], y[indiceTo])
          
              #print (matrixFT_freq$from)
           
              
              grid.bezier(xx, yy,
                         gp=gpar(lwd=pesoSeta*1.2, fill="black"),
                         arrow=arrow(  type="closed", length=unit(0.1, "inches")),
                         def='native')
            }
            
            
         }
         
          
         
       })

