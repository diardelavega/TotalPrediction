# dataStruct handeling

csDtf$algoDataList[[1]]$instList[[1]]$
  
  
  
  for(i in 1:length(aa$algoDataList)){
    print( aa$algoDataList[[i]]$dtfCategory)
  }
  #---------

# convert to new Instances
{
  aa <- CleanTotHtDtf$new()
  aa$predAtt <- "totHt"
  convfunc <- function(dtf){
    
    bigList <- list()
    for(i in 1:length(dtf$algoDataList)){
      alist <- list()
      for(j in 1:length(dtf$algoDataList[[i]]$instList) ){
        inst<- Instance$new()
        inst$accVal <- dtf$algoDataList[[i]]$instList[[j]]$accVal
        inst$original_accVal <- dtf$algoDataList[[i]]$instList[[j]]$accVal
        inst$algo <-  dtf$algoDataList[[i]]$instList[[j]]$algo
        inst$attsDtsNr <-  dtf$algoDataList[[i]]$instList[[j]]$attsDtsNr
        inst$bet <-  dtf$algoDataList[[i]]$instList[[j]]$bet
        inst$dfCategory <-  dtf$algoDataList[[i]]$instList[[j]]$dfCategory
        inst$fullDiff <-  dtf$algoDataList[[i]]$instList[[j]]$fullDiff
        inst$ptype <-  dtf$algoDataList[[i]]$instList[[j]]$ptype
        alist[[j]] <- inst
      }
      ald <- AlgoData$new()
      ald$instList <- alist
      ald$dtfCategory <- dtf$algoDataList[[i]]$dtfCategory
      print( ald$dtfCategory)
      bigList[[i]] <-ald
    }
    aa$algoDataList<<- bigList
  }#end of function
  tftDtf<- aa
  
  
aa <- Clean1pDtf$new()
# aa$algoDataList <- csDtf$algoDataList
aa$predAtt <- "score"
bigList <- list()
for(i in 1:length(csDtf$algoDataList)){
  alist <- list()
  for(j in 1:length(csDtf$algoDataList[[i]]$instList) ){
    # browser()
     inst<- Instance$new()
     inst$accVal <- csDtf$algoDataList[[i]]$instList[[j]]$accVal
     inst$original_accVal <- csDtf$algoDataList[[i]]$instList[[j]]$accVal
     inst$algo <-  csDtf$algoDataList[[i]]$instList[[j]]$algo
     inst$attsDtsNr <-  csDtf$algoDataList[[i]]$instList[[j]]$attsDtsNr
     inst$bet <-  csDtf$algoDataList[[i]]$instList[[j]]$bet
     inst$dfCategory <-  csDtf$algoDataList[[i]]$instList[[j]]$dfCategory
     inst$fullDiff <-  csDtf$algoDataList[[i]]$instList[[j]]$fullDiff
     # inst$predvec <-  csDtf$algoDataList[[i]]$instList[[j]]$predvec
     inst$ptype <-  csDtf$algoDataList[[i]]$instList[[j]]$ptype
     # print(inst)
     alist[[j]] <- inst
     # ald$instList[[j]]<- inst
  }
  ald <- AlgoData$new()
  
  ald$instList <- alist
  ald$dtfCategory <- csDtf$algoDataList[[i]]$dtfCategory
  print( ald$dtfCategory)
  bigList[[i]] <-ald
}
aa$algoDataList<- bigList
csDtf<- aa

}





# aa$predCalcScore(tt)
# aa$getEnsamble()

pinhDtf <- hDtf

bb <- CleanHeadDtf$new()
bb$algoDataList <- hDtf$algoDataList
bb$predAtt <- "head"
bb$ensambleMat <- bb2$ensambleMat
bb$ensambleCount <-bb2$ensambleCount 
bb$predCalcScore()
hdtf <- bb


cc <- CleanTotFtDtf$new()
cc$algoDataList <- tftDtf$algoDataList
cc$predAtt <- "totFt"
cc$predCalcScore(tt)

gg <- CleanTotFtDtf$new()
gg$algoDataList <- thtDtf$algoDataList
gg$predAtt <- "totFt"
gg$predCalcScore(tt)


dd <- Clean2pDtf$new()
dd$algoDataList <- p2Dtf$algoDataList
dd$predAtt <- "p2"
dd$predCalcScore(tt)

ee <- Clean1pDtf$new()
ee$algoDataList <- p1Dtf$algoDataList
ee$predAtt <- "p1"
ee$predCalcScore(tt)



pf <- AlgoData$new(dtfCategory="c")
pf$dtfCategory<-"cc"

a <- Instance$new(algo = "C50",attsDtsNr=1,accVal=333, bet="yes", fullDiff="full")
pf$instList <- c(a)

for (i in 1:10){
  pf$instList[length(pf$instList)+1] <-  Instance$new(algo = "C50",attsDtsNr=1,accVal=i, bet="yes", fullDiff="full") }

pf$betInstance()

for(ins in pf$instList){
  print(ins$algo)
}



totFtCrfvInit()
thtDtf$predCalcScore(tt)
rm(totFtCrfvInit,totFtPredFunc,totFtTreBestChoser,totFtCrfv)
rm(fulltotFtBet,fullTotFtNoBet,differencedTotFtBet,differencedTotFtNoBet)
# tftDtf


totHtCrfvInit ()
thtDtf$predCalcScore(tt)
rm(totHtCrfvInit,totHtScorePredFunc,totHtTreBestChoser,totHtCrfv)
rm(fullTotHtBet,fullTotHtNoBet,differencedTotHtBet,differencedTotHtNoBet)
# thtDtf


p2CrfvInit()
p2Dtf$predCalcScore(tt)
rm(p2CrfvInit,p2PredFunc,p2TreBestChoser,p2Crfv)
rm(full2pBet,full2pNoBet,differenced2pBet,differenced2pNoBet)
# p2Dtf

p1CrfvInit()
p1Dtf$predCalcScore(tt)
rm(p1CrfvInit,p1PredFunc,p1TreBestChoser,p1Crfv)
rm(full1pBet,full1pNoBet,differenced1pBet,differenced1pNoBet)
# p1Dtf

headCrfvInit()
hDtf$predCalcScore(tt)
rm(headCrfvInit,headPredFunc,headTreBestChoser,headCrfv)
rm(fullHeadBet,fullHeadNoBet,differencedHeadBet,differencedHeadNoBet)

scoreCrfvInit()
csDtf$predCalcScore(tt)
rm(scoreCrfvInit,scorePredFunc,scoreTreBestChoser,scoreCrfv)
rm(fullScoreBet,fullScoreNoBet,differencedScoreBet,differencedScoreNoBet)



tftDtf$predCalcScore(tt)
thtDtf$predCalcScore(tt)
p2Dtf$predCalcScore(tt)
p1Dtf$predCalcScore(tt)
hDtf$predCalcScore(tt)