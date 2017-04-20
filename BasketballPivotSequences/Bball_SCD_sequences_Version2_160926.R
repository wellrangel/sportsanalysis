library(plyr)
library(party)
library(reshape2)
library(gplots)
library(gtools)
library(plyr)
require(stringi)


source("MyLibrary.R")


inputOutputType <- function()
{
  outputType <- ask("Select an option? 1 - Pivot Table, 2 - Sequences Table, 3 - Points per Possession")
  outputType <- as.numeric(outputType)
  return (outputType)
} 
outputType <- inputOutputType()

cat("You chose", outputType)

inputTeam <- function()
{
  team <- ask("Select an option? 1 - All, 2 - Miami x Indiana, 3 - Spurs x Oklahoma, 4 - Spurs x Miami ")
  team <- as.numeric(team)
  return (team)
} 
team <- inputTeam()

cat("You chose", team)


if (!file.exists("matrixAllSCDs.RData")) {

  # miami
  m1 = read.csv("inputCsv/Mia_versus_Ind_Full_160918.csv", sep = ";")
  m2 = read.csv("inputCsv/Spu_versus_Mia_Full_160918.csv", sep = ";")
  
  m1 = ReplaceWrongValues(m1)
  m2 = ReplaceWrongValues(m2)

  m1=m1[m1$Team == 'Miami',]
  m2=m2[m2$Team == 'Miami',]
  
  Mia = rbind(m1, m2)
  
  # cria matrix para miami jogos da temporada
  matrix_tableMia= GeraMatrixFromTo(ClassificaOutcome(Mia))
  matrix_tableMia <- count(matrix_tableMia, c("strFrom","strTo") )
  
  # indiana
  Ind = read.csv("inputCsv/Mia_versus_Ind_Full_160918.csv", sep = ";")
  Ind = ReplaceWrongValues(Ind)
  
  
  Ind=Ind[Ind$Team == 'Indiana',]
  
  
  # cria matrix para miami jogos da temporada
  matrix_tableInd= GeraMatrixFromTo(ClassificaOutcome(Ind))
  matrix_tableInd <- count(matrix_tableInd, c("strFrom","strTo") )
  
  # okc
  OKC = read.csv("inputCsv/Spu_versus_Okc_Full_160918.csv", sep = ";")
  OKC = ReplaceWrongValues(OKC)
  OKC=OKC[OKC$Team == 'OKC',]
  
  # cria matrix para miami jogos da temporada
  matrix_tableOKC= GeraMatrixFromTo(ClassificaOutcome(OKC))
  matrix_tableOKC <- count(matrix_tableOKC, c("strFrom","strTo") )
  
  #spurs
  s1 = read.csv("inputCsv/Spu_versus_Okc_Full_160918.csv", sep = ";")
  s2 = read.csv("inputCsv/Spu_versus_Mia_Full_160918.csv", sep = ";")
  
  s1 = ReplaceWrongValues(s1)
  s2 = ReplaceWrongValues(s2)
  
  s1=s1[s1$Team == 'Spurs',]
  s2=s2[s2$Team == 'Spurs',]
  
  Spu = rbind(s1, s2)
  
  # cria matrix para Spu jogos da temporada
  matrix_tableSpu= GeraMatrixFromTo(ClassificaOutcome(Spu))
  matrix_tableSpu <- count(matrix_tableSpu, c("strFrom","strTo") )
  
  
    matrixAll = rbind(matrix_tableMia , matrix_tableInd)
    matrixAll = rbind(matrixAll , matrix_tableOKC)
    matrixAll = rbind(matrixAll , matrix_tableSpu)
    
    matrixAll <- matrixAll[c("strFrom","strTo")]
  
    matrixAll <- matrixAll[!duplicated(matrixAll),]
    matrixAll <- matrixAll[order(matrixAll$strFrom, matrixAll$strTo), ]
  
 
    save(matrixAll, file = "matrixAllSCDs.RData")
  }else{
    load("matrixAllSCDs.RData")
  }
  
  
  
  
 #browser()
  if (team == 1){
    
    # miami
    m1 = read.csv("inputCsv/Mia_versus_Ind_Full_160918.csv", sep = ";")
    m2 = read.csv("inputCsv/Spu_versus_Mia_Full_160918.csv", sep = ";")
    
    m1 = ReplaceWrongValues(m1)
    m2 = ReplaceWrongValues(m2)
    
    m1=m1[m1$Team == 'Miami',]
    m2=m2[m2$Team == 'Miami',]
    
    Mia = rbind(m1, m2)
    
    
    if (outputType == 1){
      # cria matrix para miami jogos da temporada
      matrix_tableMia= GeraMatrixFromTo(ClassificaOutcome(Mia))
      matrix_tableMia <- count(matrix_tableMia, c("strFrom","strTo") )
    }else if (outputType == 2){
      #browser()
      seq_tableMia= CreateSequenceIndiscriminado(ClassificaOutcome(Mia))
      seq_tableMia <- count(seq_tableMia, c("seq") )
      
      seq_tableMiaDiscriminado= CreateSequenceDiscriminado(ClassificaOutcome(Mia))
      seq_tableMiaDiscriminado <- count(seq_tableMiaDiscriminado, c("seq") )
      
    }else if (outputType == 3){
      #browser()
      ppp_tableMia= CreateTableResult(Mia)
    }
    
    
    
    #browser()
    # indiana
    Ind = read.csv("inputCsv/Mia_versus_Ind_Full_160918.csv", sep = ";")
    Ind = ReplaceWrongValues(Ind)
    
    Ind=Ind[Ind$Team == 'Indiana',]
    
    
    if (outputType == 1){
      # cria matrix para indiana jogos da temporada
      matrix_tableInd= GeraMatrixFromTo(ClassificaOutcome(Ind))
      matrix_tableInd <- count(matrix_tableInd, c("strFrom","strTo") )
    }else if (outputType == 2){
      
      seq_tableInd= CreateSequenceIndiscriminado(ClassificaOutcome(Ind))
      seq_tableInd <- count(seq_tableInd, c("seq") )
      
      seq_tableIndDiscriminado= CreateSequenceDiscriminado(ClassificaOutcome(Ind))
      seq_tableIndDiscriminado <- count(seq_tableIndDiscriminado, c("seq") )
      
    }else if (outputType == 3){
      ppp_tableInd= CreateTableResult(Ind)
      
    }
    
    
    # okc
    OKC = read.csv("inputCsv/Spu_versus_Okc_Full_160918.csv", sep = ";")
    OKC = ReplaceWrongValues(OKC)
    
    OKC=OKC[OKC$Team == 'OKC',]
    
    
    
    if (outputType == 1){
      # cria matrix para okc jogos da temporada
      matrix_tableOKC= GeraMatrixFromTo(ClassificaOutcome(OKC))
      matrix_tableOKC <- count(matrix_tableOKC, c("strFrom","strTo") )
    }else if (outputType == 2){
      #browser()
      seq_tableOKC= CreateSequenceIndiscriminado(ClassificaOutcome(OKC))
      seq_tableOKC <- count(seq_tableOKC, c("seq") )
      
      seq_tableOKCDiscriminado= CreateSequenceDiscriminado(ClassificaOutcome(OKC))
      seq_tableOKCDiscriminado <- count(seq_tableOKCDiscriminado, c("seq") )
      
    }else if (outputType == 3){
      ppp_tableOKC= CreateTableResult(OKC)
    }
    
    #spurs
    s1 = read.csv("inputCsv/Spu_versus_Okc_Full_160918.csv", sep = ";")
    s2 = read.csv("inputCsv/Spu_versus_Mia_Full_160918.csv", sep = ";")
    
    s1 = ReplaceWrongValues(s1)
    s2 = ReplaceWrongValues(s2)
    
    s1=s1[s1$Team == 'Spurs',]
    s2=s2[s2$Team == 'Spurs',]
    
    Spu = rbind(s1, s2)
    
    if (outputType == 1){
      # cria matrix para Spu jogos da temporada
      matrix_tableSpu= GeraMatrixFromTo(ClassificaOutcome(Spu))
      matrix_tableSpu <- count(matrix_tableSpu, c("strFrom","strTo") )
    }else if (outputType == 2){
      seq_tableSpu= CreateSequenceIndiscriminado(ClassificaOutcome(Spu))
      seq_tableSpu <- count(seq_tableSpu, c("seq") )
      
      seq_tableSpuDiscriminado= CreateSequenceDiscriminado(ClassificaOutcome(Spu))
      seq_tableSpuDiscriminado <- count(seq_tableSpuDiscriminado, c("seq") )
      
    }else if (outputType == 3){
      ppp_tableSpu= CreateTableResult(Spu)
    }
    # if type is pivot table
    if (outputType == 1){
      matrix_tableMia = JoinColumns(matrix_tableMia, matrixAll)
      pivot_tableMia = CreatePivotTable(matrix_tableMia)
      
      #function to create tha pivot matrix
      matrix_tableInd = JoinColumns(matrix_tableInd, matrixAll)
      pivot_tableInd = CreatePivotTable(matrix_tableInd)
      
      #function to create tha pivot matrix
      matrix_tableSpu = JoinColumns(matrix_tableSpu, matrixAll)
      pivot_tableSpu = CreatePivotTable(matrix_tableSpu)
      
      #function to create tha pivot matrix
      matrix_tableOKC = JoinColumns(matrix_tableOKC, matrixAll)
      pivot_tableOKC = CreatePivotTable(matrix_tableOKC)
      
      write.csv(matrix_tableMia, file = "outputPivotTable/matrixMiaAll.csv")
      write.csv(pivot_tableMia, file = "outputPivotTable/pivotMiaAll.csv")

      write.csv(matrix_tableInd, file = "outputPivotTable/matrixIndAll.csv")
      write.csv(pivot_tableInd, file = "outputPivotTable/pivotIndAll.csv")
      
      write.csv(matrix_tableOKC, file = "outputPivotTable/matrixOKCAll.csv")
      write.csv(pivot_tableOKC, file = "outputPivotTable/pivotOKCAll.csv")
    
      write.csv(matrix_tableSpu, file = "outputPivotTable/matrixSpuAll.csv")
      write.csv(pivot_tableSpu, file = "outputPivotTable/pivotSpuAll.csv")
    }else if (outputType == 2){
      write.csv(seq_tableMia, file = "outputSequences/sequencesIndiscriminadoMiaAll.csv")
      write.csv(seq_tableInd, file = "outputSequences/sequencesIndiscriminadoIndAll.csv")
      write.csv(seq_tableOKC, file = "outputSequences/sequencesIndiscriminadoOKCAll.csv")
      write.csv(seq_tableSpu, file = "outputSequences/sequencesIndiscriminadoSpuAll.csv")
      
      write.csv(seq_tableMiaDiscriminado, file = "outputSequences/sequencesDiscriminadoMiaAll.csv")
      write.csv(seq_tableIndDiscriminado, file = "outputSequences/sequencesDiscriminadoIndAll.csv")
      write.csv(seq_tableOKCDiscriminado, file = "outputSequences/sequencesDiscriminadoOKCAll.csv")
      write.csv(seq_tableSpuDiscriminado, file = "outputSequences/sequencesDiscriminadoSpuAll.csv")
      
      
    }else if (outputType == 3){
      
      write.csv(ppp_tableMia, file = "outputPointsPerPossession/pointsPerPossessionMiaAll.csv")
      write.csv(ppp_tableInd, file = "outputPointsPerPossession/pointsPerPossessionIndAll.csv")
      write.csv(ppp_tableOKC, file = "outputPointsPerPossession/pointsPerPossessionOKCAll.csv")
      write.csv(ppp_tableSpu, file = "outputPointsPerPossession/pointsPerPossessionSpuAll.csv")

    }
  
}else{
  inputGame <- function()
  {
    game <- ask("Select an option? 'A' - All, 'S' - Season, 'P' - Playoff")
    return (game)
  }
  game <- inputGame()
  
  cat("You chose", game)
  
  #browser()
  matrixAll
  
  if (team == 2){
    CSVTable = read.csv("inputCsv/Mia_versus_Ind_Full_160918.csv", sep = ";")
    CSVTable = ReplaceWrongValues(CSVTable)
    
    if (game == 'A'){
      
      Mia=CSVTable[CSVTable$Team == 'Miami',]
      Ind=CSVTable[CSVTable$Team == 'Indiana',]
      
    }else if (game == 'S'){
      Mia=CSVTable[CSVTable$Team == 'Miami' & substr(CSVTable$Game, 0, 1) == 'S',]
      Ind=CSVTable[CSVTable$Team == 'Indiana' & substr(CSVTable$Game, 0, 1) == 'S',]
      
      
      
    }else if (game == 'P'){
      Mia=CSVTable[CSVTable$Team == 'Miami' & substr(CSVTable$Game, 0, 1) == 'P',]
      Ind=CSVTable[CSVTable$Team == 'Indiana' & substr(CSVTable$Game, 0, 1) == 'P',]
      
    }
    
    if (outputType == 1){
      # cria matrix para miami jogos da temporada
      matrix_tableMiaInd= GeraMatrixFromTo(ClassificaOutcome(Mia))
      matrix_tableMiaInd <- count(matrix_tableMiaInd, c("strFrom","strTo") )
      #function to create tha pivot matrix
      matrix_tableMiaInd = JoinColumns(matrix_tableMiaInd, matrixAll)
      pivot_tableMiaInd = CreatePivotTable(matrix_tableMiaInd)
      
      write.csv(matrix_tableMiaInd, file = paste("outputPivotTable/matrixMiaInd",game,".csv"))
      write.csv(pivot_tableMiaInd, file = paste("outputPivotTable/pivotMiaInd",game,".csv"))
      
      
      # cria matrix para miami jogos da temporada
      matrix_tableIndMia= GeraMatrixFromTo(ClassificaOutcome(Ind))
      matrix_tableIndMia <- count(matrix_tableIndMia, c("strFrom","strTo") )
      #function to create tha pivot matrix
      matrix_tableIndMia = JoinColumns(matrix_tableIndMia, matrixAll)
      pivot_tableIndMia=CreatePivotTable(matrix_tableIndMia)
      
      write.csv(matrix_tableIndMia, file = paste("outputPivotTable/matrixIndMia",game,".csv"))
      write.csv(pivot_tableIndMia, file = paste("outputPivotTable/pivotIndMia",game,".csv"))
    }else if (outputType == 2){
      seq_tableMiaInd= CreateSequenceIndiscriminado(ClassificaOutcome(Mia))
      seq_tableMiaInd <- count(seq_tableMiaInd, c("seq") )
      
      write.csv(seq_tableMiaInd, file = paste("outputSequences/sequencesIndiscriminadoMiaInd",game,".csv"))
      
      seq_tableIndMia= CreateSequenceIndiscriminado(ClassificaOutcome(Ind))
      seq_tableIndMia <- count(seq_tableIndMia, c("seq") )
      
      write.csv(seq_tableIndMia, file = paste("outputSequences/sequencesIndiscriminadoIndMia",game,".csv"))
      
      
      
      # discriminado
      seq_tableMiaInd= CreateSequenceDiscriminado(ClassificaOutcome(Mia))
      seq_tableMiaInd <- count(seq_tableMiaInd, c("seq") )
      
      write.csv(seq_tableMiaInd, file = paste("outputSequences/sequencesDiscriminadoMiaInd",game,".csv"))
      
      seq_tableIndMia= CreateSequenceDiscriminado(ClassificaOutcome(Ind))
      seq_tableIndMia <- count(seq_tableIndMia, c("seq") )
      
      write.csv(seq_tableIndMia, file = paste("outputSequences/sequencesDiscriminadoIndMia",game,".csv"))
      
    }else if (outputType == 3){
      
      ppp_tableMiaInd= CreateTableResult(Mia)
      write.csv(ppp_tableMiaInd, file = paste("outputPointsPerPossession/pointsPerPossessionMiaIndAll",game,".csv"))
      
      ppp_tableIndMia= CreateTableResult(Ind)
      write.csv(ppp_tableIndMia, file = paste("outputPointsPerPossession/pointsPerPossessionIndMiaAll",game,".csv"))
      
    }
    
    
  }else if (team == 3){
    CSVTable = read.csv("inputCsv/Spu_versus_Okc_Full_160918.csv", sep = ";")
    CSVTable = ReplaceWrongValues(CSVTable)
    
    if (game == 'A'){
      Spu=CSVTable[CSVTable$Team == 'Spurs',]
      OKC=CSVTable[CSVTable$Team == 'OKC',]
    }else if (game == 'S'){
      Spu=CSVTable[CSVTable$Team == 'Spurs' & substr(CSVTable$Game, 0, 1) == 'S',]
      OKC=CSVTable[CSVTable$Team == 'OKC' & substr(CSVTable$Game, 0, 1) == 'S',]
    }else if (game == 'P'){
      Spu=CSVTable[CSVTable$Team == 'Spurs' & substr(CSVTable$Game, 0, 1) == 'P',]
      OKC=CSVTable[CSVTable$Team == 'OKC' & substr(CSVTable$Game, 0, 1) == 'P',]
    }
    
    if (outputType == 1){
      # cria matrix para Spu jogos da temporada
      matrix_tableSpuOKC= GeraMatrixFromTo(ClassificaOutcome(Spu))
      matrix_tableSpuOKC <- count(matrix_tableSpuOKC, c("strFrom","strTo") )
      #function to create tha pivot matrix
      matrix_tableSpuOKC = JoinColumns(matrix_tableSpuOKC, matrixAll)
      pivot_tableSpuOKC = CreatePivotTable(matrix_tableSpuOKC)
      
      write.csv(matrix_tableSpuOKC, file = paste("outputPivotTable/matrixSpuOKC",game,".csv"))
      write.csv(pivot_tableSpuOKC, file = paste("outputPivotTable/pivotSpuOKC",game,".csv"))
      
      
      # cria matrix para OKC jogos da temporada
      matrix_tableOKCSpu= GeraMatrixFromTo(ClassificaOutcome(OKC))
      matrix_tableOKCSpu <- count(matrix_tableOKCSpu, c("strFrom","strTo") )
      #function to create tha pivot matrix
      matrix_tableOKCSpu = JoinColumns(matrix_tableOKCSpu, matrixAll)
      pivot_tableOKCSpu=CreatePivotTable(matrix_tableOKCSpu)
      
      write.csv(matrix_tableOKCSpu, file = paste("outputPivotTable/matrixOKCSpu",game,".csv"))
      write.csv(pivot_tableOKCSpu, file = paste("outputPivotTable/pivotOKCSpu",game,".csv"))
      
    }else if (outputType == 2){
      seq_tableSpuOKC= CreateSequenceIndiscriminado(ClassificaOutcome(Spu))
      seq_tableSpuOKC <- count(seq_tableSpuOKC, c("seq") )
      
      write.csv(seq_tableSpuOKC, file = paste("outputSequences/sequencesIndiscriminadoSpuOKC",game,".csv"))
      
      seq_tableOKCSpu= CreateSequenceIndiscriminado(ClassificaOutcome(OKC))
      seq_tableOKCSpu <- count(seq_tableOKCSpu, c("seq") )
      
      write.csv(seq_tableOKCSpu, file = paste("outputSequences/sequencesIndiscriminadoOKCSpu",game,".csv"))
      
      #discriminado
      seq_tableSpuOKC= CreateSequenceDiscriminado(ClassificaOutcome(Spu))
      seq_tableSpuOKC <- count(seq_tableSpuOKC, c("seq") )
      
      write.csv(seq_tableSpuOKC, file = paste("outputSequences/sequencesDiscriminadoSpuOKC",game,".csv"))
      
      seq_tableOKCSpu= CreateSequenceDiscriminado(ClassificaOutcome(OKC))
      seq_tableOKCSpu <- count(seq_tableOKCSpu, c("seq") )
      
      write.csv(seq_tableOKCSpu, file = paste("outputSequences/sequencesDiscriminadoOKCSpu",game,".csv"))
      
    }else if (outputType == 3){
      
      ppp_tableSpuOKC= CreateTableResult(Spu)
      write.csv(ppp_tableSpuOKC, file = paste("outputPointsPerPossession/pointsPerPossessionSpuOKCAll",game,".csv"))
      
      ppp_tableOKCSpu= CreateTableResult(OKC)
      write.csv(ppp_tableOKCSpu, file = paste("outputPointsPerPossession/pointsPerPossessionOKCSpuAll",game,".csv"))
      
    }
  }else if (team == 4){
    CSVTable = read.csv("inputCsv/Spu_versus_Mia_Full_160918.csv", sep = ";")  
    CSVTable = ReplaceWrongValues(CSVTable)
    
    if (game == 'A'){
      Spu=CSVTable[CSVTable$Team == 'Spurs',]
      Mia=CSVTable[CSVTable$Team == 'Miami',]
    }else if (game == 'S'){
      Spu=CSVTable[CSVTable$Team == 'Spurs' & substr(CSVTable$Game, 0, 1) == 'S',]
      Mia=CSVTable[CSVTable$Team == 'Miami' & substr(CSVTable$Game, 0, 1) == 'S',]
    }else if (game == 'P'){
      Spu=CSVTable[CSVTable$Team == 'Spurs' & substr(CSVTable$Game, 0, 1) == 'P',]
      Mia=CSVTable[CSVTable$Team == 'Miami' & substr(CSVTable$Game, 0, 1) == 'P',]
    }
    
      if (outputType == 1){
        # cria matrix para Spu jogos da temporada
        matrix_tableSpuMia= GeraMatrixFromTo(ClassificaOutcome(Spu))
        matrix_tableSpuMia <- count(matrix_tableSpuMia, c("strFrom","strTo") )
        #function to create tha pivot matrix
        matrix_tableSpuMia = JoinColumns(matrix_tableSpuMia, matrixAll)
        pivot_tableSpuMia = CreatePivotTable(matrix_tableSpuMia)
        
        write.csv(matrix_tableSpuMia, file = paste("outputPivotTable/matrixSpuMia",game,".csv"))
        write.csv(pivot_tableSpuMia, file = paste("outputPivotTable/pivotSpuMia",game,".csv"))
        
        # cria matrix para OKC jogos da temporada
        matrix_tableMiaSpu= GeraMatrixFromTo(ClassificaOutcome(Mia))
        matrix_tableMiaSpu <- count(matrix_tableMiaSpu, c("strFrom","strTo") )
        #function to create tha pivot matrix
        matrix_tableMiaSpu = JoinColumns(matrix_tableMiaSpu, matrixAll)
        pivot_tableMiaSpu=CreatePivotTable(matrix_tableMiaSpu)
        
        write.csv(matrix_tableMiaSpu, file = paste("outputPivotTable/matrixMiaSpu",game,".csv"))
        write.csv(pivot_tableMiaSpu, file = paste("outputPivotTable/pivotMiaSpu",game,".csv"))
      }else if (outputType == 2){
        seq_tableSpuMia= CreateSequenceIndiscriminado(ClassificaOutcome(Spu))
        seq_tableSpuMia <- count(seq_tableSpuMia, c("seq") )
        
        write.csv(seq_tableSpuMia, file = paste("outputSequences/sequencesIndiscriminadoSpuMia",game,".csv"))
        
        seq_tableMiaSpu= CreateSequenceIndiscriminado(ClassificaOutcome(Mia))
        seq_tableMiaSpu <- count(seq_tableMiaSpu, c("seq") )
        
        write.csv(seq_tableMiaSpu, file = paste("outputSequences/sequencesIndiscriminadoMiaSpu",game,".csv"))        
        
        #discriminado
        seq_tableSpuMia= CreateSequenceDiscriminado(ClassificaOutcome(Spu))
        seq_tableSpuMia <- count(seq_tableSpuMia, c("seq") )
        
        write.csv(seq_tableSpuMia, file = paste("outputSequences/sequencesDiscriminadoSpuMia",game,".csv"))
        
        seq_tableMiaSpu= CreateSequenceDiscriminado(ClassificaOutcome(Mia))
        seq_tableMiaSpu <- count(seq_tableMiaSpu, c("seq") )
        
        write.csv(seq_tableMiaSpu, file = paste("outputSequences/sequencesDiscriminadoMiaSpu",game,".csv"))        
        
      }else if (outputType == 3){
        
        ppp_tableSpuMia= CreateTableResult(Spu)
        write.csv(ppp_tableSpuOKC, file = paste("outputPointsPerPossession/pointsPerPossessionSpuMiaAll",game,".csv"))
        
        ppp_tableMiaSpu= CreateTableResult(Mia)
        write.csv(ppp_tableMiaSpu, file = paste("outputPointsPerPossession/pointsPerPossessionMiaSpuAll",game,".csv"))
        
      }
    
    
  }
  
  
}

print('fim')






