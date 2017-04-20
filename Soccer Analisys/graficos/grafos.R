# Library
library(fmsb)
library(plyr)
library(party)
library(reshape2)
library(gplots)
library(gtools)
require(plyr)


 
bra_per = read.csv("Brasil Jogo 1 x Peru.csv", sep = ";")
bra_col = read.csv("Brasil Jogo 2 x Colombia.csv", sep = ";")
bra_ven = read.csv("Brasil Jogo 3 x Venezuela.csv", sep = ";")
bra_par = read.csv("Brasil Jogo 4 x Paraguai.csv", sep = ";")

bra_per <- bra_per[c("SDO")]
bra_col <- bra_col[c("SDO")]
bra_ven <- bra_ven[c("SDO")]
bra_par <- bra_par[c("SDO")]

bra_per[bra_per==""] <- NA
bra_col[bra_col==""] <- NA
bra_ven[bra_ven==""] <- NA
bra_par[bra_par==""] <- NA


AllGames = rbind(bra_per , bra_col)
AllGames = rbind(AllGames , bra_ven)
AllGames = rbind(AllGames , bra_par)


AllGames[AllGames==0] <- NA



AllGames <- AllGames[!duplicated(AllGames), ]

AllGames <- AllGames[!is.na(AllGames)]
#AllGames <- reshape(AllGames, direction = "wide", idvar="AllGames")


mAllGames = matrix(AllGames,nrow = 1,ncol = NROW(AllGames))
print(AllGames[2])
NROW(AllGames)

AllGamesDT <- as.data.frame(t(AllGames))


#localMatrix = merge(x = localMatrix, y = matrixAll, by = c("strFrom", "strTo") , all.y = TRUE)
#localMatrix <- localMatrix[order(localMatrix$strFrom, localMatrix$strTo), ]

#localMatrix[is.na(localMatrix)] <- 0
head(AllGames)

bra_per <- count(bra_per, c(1))
bra_col <- count(bra_col, c(1) )
bra_ven <- count(bra_ven, c(1) )
bra_par <- count(bra_par, c(1) )


#bra_per <- bra_per[!is.na(bra_per)]
#bra_col <- bra_col[!is.na(bra_col)]
#bra_ven <- bra_ven[!is.na(bra_ven)]
#bra_par <- bra_par[!is.na(bra_par)]



#bra_per <- bra_per[order(bra_per$V1 ), ]


AllGamesData = rbind(bra_per , bra_col)
AllGamesData = rbind(AllGamesData , bra_ven)
AllGamesData = rbind(AllGamesData , bra_par)



#bra_per <- count(bra_per,  )
#bra_per <- bra_per[order(bra_per$x), ]
#bra_per <- as.data.frame(t(bra_per))


set.seed(99)
data=as.data.frame(AllGamesData , ncol=NROW(mAllGames))
colnames(data)=mAllGames
#colnames(data)=c("math" , "english" , "biology" , "music" , "R-coding" )
#rownames(data)=paste("mister" , letters[1:3] , sep="-")

# To use the fmsb package, I have to add 2 lines to the dataframe: the max and min of each topic to show on the plot!
#data=rbind(rep(20,5) , rep(0,5) , data)




AllGamesDT = as.data.frame(table(AllGames))
#AllGamesNew = AllGames[!duplicated(AllGames),]





AllGamesNew <- as.data.frame(t(AllGamesNew))

AllGamesNew <- AllGamesNew[!is.na(AllGamesNew)]
warnings()
AllGamesNew
NROW(AllGamesNew)



#==================
# Plot 1: Default radar chart proposed by the library:
radarchart(bra_per)

