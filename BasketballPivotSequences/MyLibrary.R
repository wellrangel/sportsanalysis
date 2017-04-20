ReplaceWrongValues<-function(dtFrameOriginal)
{
  #browser();
  # replacing Hdoff_E_Re-Pk to Hdoff.E.Re-Pk
  dtFrameOriginal[] <- lapply(dtFrameOriginal, gsub, pattern = "Hdoff_E_Re-Pk", replacement = "Hdoff.E.Re-Pk", fixed = TRUE)
  
  # replacing Re-pick and Re-Pk to Re-Pick
  dtFrameOriginal[] <- lapply(dtFrameOriginal, gsub, pattern = "Re-pick", replacement = "Re-Pick", fixed = TRUE)
  dtFrameOriginal[] <- lapply(dtFrameOriginal, gsub, pattern = "Re-Pk", replacement = "Re-Pick", fixed = TRUE)
  
  
  # replacing Hdoff F Post to Hdof/Post
  dtFrameOriginal[] <- lapply(dtFrameOriginal, gsub, pattern = "Hdoff F Post", replacement = "Hdof/Post", fixed = TRUE)
  
  
  
  #browser();
  
  
  return(dtFrameOriginal)
} 

ClassificaOutcome<-function(dtFrameOriginal)
{
  
  
  #Team;String;SCD;Concatenation;Outcome;Jogo
  copiaCSV <- data.frame(Team=character(), 
                         String=character(), 
                         SCD=character(), 
                         Concatenation=character(), 
                         Outcome=character(), 
                         OutcomeManipulado=character(), 
                         Game=character(), 
                         stringsAsFactors=FALSE) 
  
  #Faz loop em cada linha para copiar todas as linhas para a nova planilha com o seguinte criterio
  #copia quando SCD for Screen e Concatenation for Stg.Scr e elimina um linha
  
  outcome = ""
  
  # L: aqui criei dois arquivos com a lista de SCDs e de SCDs concatenadas (nesse caso, muitas para listar uma a uma)
  
  Scd_Distinct = read.csv("inputCsv/zScd_Distinct_New_160926.csv", sep=";")
  Scd_Distinct  = as.vector(as.matrix(Scd_Distinct))
  
  Scd_concat =  read.csv("inputCsv/zScd_Concat.csv", sep=";")
  Scd_concat  = as.vector(as.matrix(Scd_concat))
  
  ## L: reduce outcomes to: i) shots; ii) fouls; iii) turnovers
  
  #preenche com a lista de valores que voce deseja testar
  
  lista_shot = c("2pts_made", "2pts_miss", "3ptos_made", "3pts_miss", "finalizacao correta", "Finalization correct", "Finalization wrong") 
  lista_foul = c("2points_foul", "3pts_foul", "Foul", "finalizacao falta", "Finalization foul") 
  lista_to = c("None", "TO", "Deflected")
  
  #Scd_Distinct = c("Screen", "Pick", "Spotup", "Post", "Dime in", "1on1", "Cut","Hdoff") 
  #Scd_concat = c("Stg.Scr", "Scr_Pker", "Scr_Scrner", "Seq.Scr.Mk", "Stg.Pk", "Pk_Scr.Rc", "Re-Pk", "Hdoff F Post", "Scr.E.Pk",  "Rc.Scr.E.Pk", "Pk_Hdof.Rc", "Rc.Hdof.E.Mk.One", "Rc.Pk.E.Mk.Hdof",  "Scr.E.Pk_SamePl", "Pk.E.Scr",  "Hdoff.E.Re-Pk", "Scr_Hdof.Mk", "Rc.Scr.E.MkOne",  "Pick_Screener", "Re-Screen", "Rc.Scr.E.Mk.Hdof", "Hdof_Scr.Rc", "Hdof_Scrner")
  
  #browser()
  
  copy_line = TRUE
  for(i in 1:nrow(dtFrameOriginal)) {
    row <- dtFrameOriginal[i,]  # linha atual
    
    outcome = ""
    #analisa outcome e substitui pelo par field_goal e foul
    if (any(row$Outcome == lista_shot) ) {
      outcome = "field_goal"
    }else if (any(row$Outcome == lista_foul)){
      outcome = "foul"
    }else if (any(row$Outcome == lista_to)){
      outcome = "turnover"
    }    
    
    #copia todas as linhas e atribui outcome correto a partir do if anterior
    
    row$OutcomeManipulado = outcome
    copiaCSV <- rbind(copiaCSV, row)
    
    rm(row)
  }
  #browser()
  
  return(copiaCSV)
  
}



GeraMatrixFromTo<-function(dtFrameLocal)
{
  
  options(stringsAsFactors = FALSE)
  
  #cria planilha vazia na memoria com as colunas strFrom e strTo
  matrix <- data.frame(
    strFrom=character(),
    strTo=character(),
    stringsAsFactors=FALSE)
  
  copiaCSV <- data.frame(Team=character(), 
                         String=character(), 
                         SCD=character(), 
                         Concatenation=character(), 
                         Outcome=character(), 
                         Game=character(), 
                         OutcomeManipulado=character(), 
                         SCDManipulado=character(), 
                         stringsAsFactors=FALSE) 
  
  
  primeira_linha = TRUE
  for(i in 1:nrow(dtFrameLocal)) {
    
    #browser()
    
    strFrom <- ""
    strTo <- ""
    rowAtual <- dtFrameLocal[i,]
    
    myRow <- dtFrameLocal[1,]
    
    
    #browser()
    if (primeira_linha == TRUE){
      rowAtual$SCDManipulado = rowAtual$SCD
      copiaCSV <- rbind(copiaCSV, rowAtual)   
      primeira_linha = FALSE
    }else if (( rowAtual$Concatenation == "Conc. Indep." || rowAtual$Concatenation == "Sem Conc.") ){
      
      rowAtual$SCDManipulado = rowAtual$SCD
      copiaCSV <- rbind(copiaCSV, rowAtual)  
    }else if ( ( rowAtual$Concatenation != "Conc. Indep." && rowAtual$Concatenation != "Sem Conc.") ){
      rowAtual$SCDManipulado = rowAtual$Concatenation
      copiaCSV <- rbind(copiaCSV, rowAtual)   
      #rowNext <- dtFrameLocal[i+1,] # proxima linha
      #rowAtual$SCDManipulado = rowNext$SCD
      
      #strFrom <- rowAtual$SCD
      #rowNext <- dtFrameLocal[1+i,]
      #strTo <- rowNext$SCD
      
      # copiaCSV <- rbind(copiaCSV, rowAtual)   
      
    }else{
      ##rowAtual$SCDManipulado = rowAtual$Concatenation
      #copiaCSV <- rbind(copiaCSV, rowAtual)   
    }
    
    if (rowAtual$OutcomeManipulado != ""){
      
      #browser()
      myRowOutCome <- dtFrameLocal[1,]
      myRowOutCome$String = ""
      myRowOutCome$SCD = ""
      myRowOutCome$Outcome = ""
      myRowOutCome$OutcomeManipulado = ""
      myRowOutCome$SCDManipulado = rowAtual$OutcomeManipulado
      copiaCSV <- rbind(copiaCSV, myRowOutCome)   
      
      primeira_linha = TRUE
      
    }
    
    
    
  }
  #browser()
  
  for(i in 1:nrow(copiaCSV)) {
    strFrom <- ""
    strTo <- ""
    rowAtual <- copiaCSV[i,]
    
    myRow <- matrix[1,]
    
    if (rowAtual$SCDManipulado != "field_goal" && rowAtual$SCDManipulado != "foul" && rowAtual$SCDManipulado != "turnover") {
      if (i < nrow(copiaCSV)){
        strFrom <- rowAtual$SCDManipulado
        rowNext <- copiaCSV[1+i,]
        strTo <- rowNext$SCDManipulado
        
      }
      
      
      myRow$strFrom = strFrom
      myRow$strTo = strTo
      
      matrix <- rbind(matrix, myRow)   
    }
    
  }
  
  matrix <- matrix[order(matrix$strFrom, matrix$strTo), ]
  
  #browser()
  return(matrix)
}




CreatePivotTable<-function(localMatrix)
{
  
  #conta todas as ocorrencias distintas de strFrom e strTo
  aggdata <- count(localMatrix, c("strFrom","strTo") )
  
  #cria a tabela pivot
  pivot_table = recast(aggdata, strFrom ~strTo)
  #ATRIBUI ZERO ONDE TEM CELULA VAZIA PARA DEIXAR TODOS OS VALORES NUMERICOS
  pivot_table[is.na(pivot_table)] <- 0
  
  #ATRIBUI A PRIMEIRA COLUNA DOS DADOS NOS LABES DA COLUNA 0, ISTO PARA DEIXAR A PLANILHA SOMENTE COM VALORES NUMERICOS 
  #PARA USO DAS FUNCOES NUMERICAS
  row.names(pivot_table) <- pivot_table$strFrom
  #RETIRA A PRIMEIRA COLUNA POIS ELA NAO EH NUMERICA
  pivot_table <- subset( pivot_table, select = -strFrom )
  
  #browser()
  
  return(pivot_table)
}


JoinColumns<-function(localMatrix, matrixAll)
{
  
  localMatrix = merge(x = localMatrix, y = matrixAll, by = c("strFrom", "strTo") , all.y = TRUE)
  localMatrix <- localMatrix[order(localMatrix$strFrom, localMatrix$strTo), ]
  
  localMatrix[is.na(localMatrix)] <- 0
  return(localMatrix)
}




CreateSequenceIndiscriminado<-function(dtFrameLocal)
{
  
  options(stringsAsFactors = FALSE)
  
  #cria planilha vazia na memoria com as colunas strFrom e strTo
  copiaCSV <- data.frame(Team=character(), 
                         String=character(), 
                         SCD=character(), 
                         Concatenation=character(), 
                         Outcome=character(), 
                         Game=character(), 
                         OutcomeManipulado=character(), 
                         SCDManipulado=character(),
                         Type=character(), 
                         stringsAsFactors=FALSE) 
  
  
  primeira_linha = TRUE
  for(i in 1:nrow(dtFrameLocal)) {
    
    #browser()
    
    strFrom <- ""
    strTo <- ""
    rowAtual <- dtFrameLocal[i,]
    
    myRow <- dtFrameLocal[1,]
    
    #browser()
    rowAtual$Type = 0
    if (primeira_linha == TRUE){
      rowAtual$SCDManipulado = rowAtual$SCD
      copiaCSV <- rbind(copiaCSV, rowAtual)   
      primeira_linha = FALSE
      
    }else if (( rowAtual$Concatenation == "Conc. Indep." || rowAtual$Concatenation == "Sem Conc.") ){
      rowAtual$SCDManipulado = rowAtual$SCD
      copiaCSV <- rbind(copiaCSV, rowAtual)  
    }else if ( ( rowAtual$Concatenation != "Conc. Indep." && rowAtual$Concatenation != "Sem Conc.") ){
      rowAtual$Type = 0
      rowAtual$SCDManipulado = rowAtual$Concatenation
      copiaCSV <- rbind(copiaCSV, rowAtual)   
      
      
    }else{
      
      ##rowAtual$SCDManipulado = rowAtual$Concatenation
      #copiaCSV <- rbind(copiaCSV, rowAtual)   
    }
   # browser()
    if (rowAtual$OutcomeManipulado != ""){
      
      #browser()
      rowAtual$Type = 2
      
      myRowOutCome <- dtFrameLocal[1,]
      myRowOutCome$String = ""
      myRowOutCome$SCD = ""
      myRowOutCome$Outcome = ""
      myRowOutCome$OutcomeManipulado = ""
      myRowOutCome$SCDManipulado = rowAtual$OutcomeManipulado
      myRowOutCome$Type = rowAtual$Type
      copiaCSV <- rbind(copiaCSV, myRowOutCome)   
      
      primeira_linha = TRUE
      
    }
    
  }
  #browser()
  
  dfSeq <- data.frame(seq=character(),
                      stringsAsFactors=FALSE) 
  
  #newrow = c("02010")
  #dfSeq <- rbind(dfSeq, newrow)
  
  strSequence <- ""
  for(i in 1:nrow(copiaCSV)) {
    
    
    rowAtual <- copiaCSV[i,]
    
    
    if (rowAtual$Type < 2){
      strSequence = paste( strSequence, rowAtual$Type, sep="")
      
    }else{
      newrow = data.frame(seq=strSequence)
      dfSeq <- rbind(dfSeq, newrow)
      strSequence <- ""
      
    }
    
    
  }
  
  #browser()
  return(dfSeq)
}

CreateSequenceDiscriminado<-function(dtFrameLocal)
{
  
  options(stringsAsFactors = FALSE)
  
  #cria planilha vazia na memoria com as colunas strFrom e strTo
  copiaCSV <- data.frame(Team=character(), 
                         String=character(), 
                         SCD=character(), 
                         Concatenation=character(), 
                         Outcome=character(), 
                         Game=character(), 
                         OutcomeManipulado=character(), 
                         SCDManipulado=character(),
                         Type=character(), 
                         stringsAsFactors=FALSE) 
  
  
  primeira_linha = TRUE
  for(i in 1:nrow(dtFrameLocal)) {
    
    #browser()
    
    strFrom <- ""
    strTo <- ""
    rowAtual <- dtFrameLocal[i,]
    
    myRow <- dtFrameLocal[1,]
    
    #browser()
    rowAtual$Type = 0
    if (primeira_linha == TRUE){
      rowAtual$SCDManipulado = rowAtual$SCD
      copiaCSV <- rbind(copiaCSV, rowAtual)   
      primeira_linha = FALSE
      
    }else if (( rowAtual$Concatenation == "Conc. Indep." || rowAtual$Concatenation == "Sem Conc.") ){
      rowAtual$SCDManipulado = rowAtual$SCD
      copiaCSV <- rbind(copiaCSV, rowAtual)  
    }else if ( ( rowAtual$Concatenation != "Conc. Indep." && rowAtual$Concatenation != "Sem Conc.") ){
      rowAtual$Type = 1
      rowAtual$SCDManipulado = rowAtual$Concatenation
      copiaCSV <- rbind(copiaCSV, rowAtual)   
      
      
    }else{
      
      ##rowAtual$SCDManipulado = rowAtual$Concatenation
      #copiaCSV <- rbind(copiaCSV, rowAtual)   
    }
    # browser()
    if (rowAtual$OutcomeManipulado != ""){
      
      #browser()
      rowAtual$Type = 2
      
      myRowOutCome <- dtFrameLocal[1,]
      myRowOutCome$String = ""
      myRowOutCome$SCD = ""
      myRowOutCome$Outcome = ""
      myRowOutCome$OutcomeManipulado = ""
      myRowOutCome$SCDManipulado = rowAtual$OutcomeManipulado
      myRowOutCome$Type = rowAtual$Type
      copiaCSV <- rbind(copiaCSV, myRowOutCome)   
      
      primeira_linha = TRUE
      
    }
    
  }
  #browser()
  
  dfSeq <- data.frame(seq=character(),
                      stringsAsFactors=FALSE) 
  
  #newrow = c("02010")
  #dfSeq <- rbind(dfSeq, newrow)
  
  strSequence <- ""
  hasConcatenada = FALSE
  for(i in 1:nrow(copiaCSV)) {
    
    
    rowAtual <- copiaCSV[i,]
    if (rowAtual$Type == 1){
      hasConcatenada = TRUE
    }
    
    if (rowAtual$Type < 2){
      
      strSequence = paste( strSequence, "0", sep="")
      
    }else{
      #ajustando para 1 se houver pelo menos uma acao concatenada
      if (hasConcatenada == TRUE){
        strSequence <- stri_replace_all(strSequence, "1", fixed="0")
      }
      
      newrow = data.frame(seq=strSequence)
      dfSeq <- rbind(dfSeq, newrow)
      strSequence <- ""
      hasConcatenada = FALSE
      
    }
    
    
  }
  
  
  #browser()
  return(dfSeq)
}




CreateTableResult<-function(dtFrameLocal)
{
  
  
  lista_shot = c("2pts_made", "2pts_miss", "3ptos_made", "3pts_miss", "finalizacao correta", "Finalization correct", "Finalization wrong") 
  lista_foul = c("2points_foul", "3pts_foul", "Foul", "finalizacao falta", "Finalization foul") 
  lista_to = c("None", "TO", "Deflected")
  
  
  options(stringsAsFactors = FALSE)
  
  #browser()
  
  copiaCSV <- data.frame(SCD=character(), 
                         Outcome=character(), 
                         points=integer(), 
                         stringsAsFactors=FALSE) 
  
  
  
  for(i in 1:nrow(dtFrameLocal)) {
    rowAtual <- dtFrameLocal[i,]
    #browser()
    strSCD <- ""
    strOutcome<- rowAtual$Outcome
    
    outcome = ""
    if (any(rowAtual$Outcome == lista_shot) ) {
      outcome = "true"
    }else if (any(rowAtual$Outcome == lista_foul)){
      outcome = "true"
    }else if (any(rowAtual$Outcome == lista_to)){
      outcome = "true"
    }
    
    
    points = 0
    if (outcome != ""){
      if ( ( rowAtual$Concatenation != "Conc. Indep." && rowAtual$Concatenation != "Sem Conc.") ){
        strSCD = rowAtual$Concatenation
      }else{
        strSCD = rowAtual$SCD
      }
      #analisa outcome e substitui pelo par field_goal e foul
      if (rowAtual$Outcome == "2pts_made") {
        points = 2
      }else if (rowAtual$Outcome == "3ptos_made") {
        points = 3
      }else{
        points = 0
      }  
      #browser()
      newrow = data.frame(SCD=strSCD, Outcome=strOutcome,points=points)
      copiaCSV <- rbind(copiaCSV, newrow)
    
    }
    
  }
  
  
  browser()
  #create summary table
  aggsum <-  aggregate(points ~ SCD, data = copiaCSV, sum)
  
  aggcount <- count(copiaCSV, c("SCD") )
  
  finalTable = merge(x = aggsum, y = aggcount, by = c("SCD", "SCD") , all.y = TRUE)
  
  #browser()
  finalTable$ppp <- finalTable[, 2]/finalTable[, 3]
  
  finalTable=finalTable[finalTable$points > 0,]
  
  
  return(finalTable)
}
