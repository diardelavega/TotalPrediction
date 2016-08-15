#page to be used for loading /7 initiating all the data for the creation of DTF obj

runAll<- function(trPaths){
  #trPaths is a vector with all the tr paths of a competition
  
  # -0 load the dtf-crfv-estimation files (head,score,1p,2p,ht,ft)
  DTFLoader();
  predAtt_Loader();
  libLoader();
  
 
  
  for(path in trPaths){
    # -1
    df <- read.csv(path);
    ndf <- diffFunc();
    
    # -2
    
    
    
    
    
  }
  
  
  
}

libLoader <- function(){
  library(plyr)
  library(e1071)  #svm
  library(C50)
  library(randomForest)
  library(ipred)
  library(RWeka)
  library(rpart)
  library(tree)
}

remover <- function(){
  # remove all unwanted vars, save only the dtf objs
  rm(df,ndf,DTFLoader,predAtt_Loader,diffFunc);
  
  rm(headCrfvInit,headPredFunc,headTreBestChoser,headCrfv)
  rm(fullHeadBet,fullHeadNoBet,differencedHeadBet,differencedHeadNoBet)
  
  rm(scoreCrfvInit,scorePredFunc,scoreTreBestChoser,scoreCrfv)
  rm(fullScoreBet,fullScoreNoBet,differencedScoreBet,differencedScoreNoBet)
  
  rm(p2CrfvInit,p2PredFunc,p2TreBestChoser,p2Crfv)
  rm(full2pBet,full2pNoBet,differenced2pBet,differenced2pNoBet)
  
  rm(p1CrfvInit,p1PredFunc,p1TreBestChoser,p1Crfv)
  rm(full1pBet,full1pNoBet,differenced1pBet,differenced1pNoBet)
  
  rm(totFtCrfvInit,totFtPredFunc,totFtTreBestChoser,totFtCrfv)
  rm(fulltotFtBet,fullTotFtNoBet,differencedTotFtBet,differencedTotFtNoBet)
  
  rm(totHtCrfvInit,totHtPredFunc,totHtTreBestChoser,totHtCrfv);
  rm(fullTotHtBet,fullTotHtNoBet,differencedTotHtBet,differencedTotHtNoBet);
}

DTFLoader <- function(){
  source("C:/TotalPrediction/HeadCrfvEstimation.R"); 
  source("C:/TotalPrediction/ScoreCrfvEstimation.R"); 
  source("C:/TotalPrediction/P1CrfvEstimation.R");
  source("C:/TotalPrediction/P2CrfvEstimation.R"); 
  source("C:/TotalPrediction/totHtScoreCrfvEstimation.R"); 
  source("C:/TotalPrediction/totFtScoreCrfvEstimation.R"); 
}

predAtt_Loader <- function(){
  source("C:/TotalPrediction/Head_AttPredDataset.R");
  source("C:/TotalPrediction/Score_AttPredDataset.R");
  source("C:/TotalPrediction/P1_AttPredDataset.R");
  source("C:/TotalPrediction/P2_AttPredDataset.R");
  source("C:/TotalPrediction/totFt_AttPredDataset.R");
  source("C:/TotalPrediction/totHt_AttPredDataset.R");
}

dataStructLoader <- function(){
  source("C:/TotalPrediction/dataStructure.R");
}

diffFunc <- function(){
  t1adoe <- df$t1AtackIn - abs(df$t2DefenseOut) # because defence is negative nr
  t2adoe <- abs(df$t1DefenseIn) - df$t2AtackOut
  t1e <- df$t1Atack - abs(df$t2Defense) # because defence is negative nr
  t2e <- df$t2Atack - abs(df$t1Defense) 
  #----------
  datk <- df$t1Atack-df$t2Atack
  datkin <- df$t1AtackIn-df$t2AtackIn
  datkout <- df$t1AtackOut-df$t2AtackOut
  ddef <- df$t1Defense-df$t2Defense
  ddefin <- df$t1DefenseIn-df$t2DefenseIn
  ddefout <- df$t1DefenseOut-df$t2DefenseOut
  
  
  doav_ht <- df$t1AvgHtScoreIn-df$t2AvgHtScoreOut
  doav_ft <- df$t1AvgFtScoreIn-df$t2AvgFtScoreOut
  #----------
  dav_htin <- df$t1AvgHtScoreIn-df$t2AvgHtScoreIn
  dav_htout <-  df$t1AvgHtScoreOut-df$t2AvgHtScoreOut
  dav_ftin <- df$t1AvgFtScoreIn-df$t2AvgFtScoreIn
  dav_ftout <-  df$t1AvgFtScoreOut-df$t2AvgFtScoreOut
  
  
  owd <- df$t1WinsIn-df$t2WinsOut
  odd <- df$t1DrawsIn- df$t2DrawsOut
  old <- df$t1LosesIn - df$t2LosesOut
  #----------
  dwin <- df$t1WinsIn-df$t2WinsIn
  dwout <- df$t1WinsOut-df$t2WinsOut
  ddin <- df$t1DrawsIn-df$t2DrawsIn
  ddout <- df$t1DrawsOut-df$t2DrawsOut
  dlin <- df$t1LosesIn-df$t2LosesIn
  dlout <- df$t1LosesOut-df$t2LosesOut
  
  pd <- df$t1Points-df$t2Points
  fd <- df$t1Form-df$t2Form
  
  mfd1<-c()
  mfd2<-c()
  for(i in 1:dim(df)[1]){mfd1[i] <- mean(df[i,13],df[i,14],df[i,15],df[i,16])}
  for(i in 1:dim(df)[1]){mfd2[i] <- mean(df[i,39],df[i,40],df[i,41],df[i,42])}
  #owd <- df$t1WinsIn-df$t2WinsOut
  #odd <- df$t1DrawsIn- df$t2DrawsOut
  #old <- df$t1LosesIn - df$t2LosesOut
  #----------DF data
  df$mfd1 <<-mfd1
  df$mfd2 <<-mfd2
  df$odd <<- odd
  df$old <<- old
  df$owd <<-owd
  
  #ttdf$mfd1 <-mfd1
  #ttdf$mfd2 <-mfd2
  #ttdf$odd <- odd
  #tdf$old<- old
  #tdf$owd <-owd
  #----------------
  
  
  f1d <- df[,13]-df[,39]
  f2d <- df[,14]-df[,40]
  f3d <- df[,15]-df[,41]                 
  f4d <- df[,16]-df[,42]
  #--------------
  ndf <<- data.frame( 
    mfd1,mfd2,pd,fd,
    #  f1d,f2d,f3d,f4d,
    t1adoe,t2adoe,t1e,t2e,
    owd,odd,old,
    dwin,dwout,ddin,ddout,dlin,dlout,
    datk,datkin,datkout,ddef,ddefin,ddefout,
    doav_ht,doav_ft,
    dav_htin,dav_htout,dav_ftin,dav_ftout
  )
  
  
  ndf$week <<- df$week
  ndf$headOutcome <<-df$headOutcome
  ndf$scoreOutcome<<-df$scoreOutcome
  ndf$ht1pOutcome <<-df$ht1pOutcome
  ndf$ht2pOutcome <<-df$ht2pOutcome
  ndf$ggOutcome <<-df$ggOutcome
  ndf$totHtScore  <<- df$totHtScore 
  ndf$totFtScore <<-df$totFtScore
  ndf$t1 <<-df$t1
  ndf$t2<<-df$t2
  ndf$bet_1<<-df$bet_1
  ndf$bet_X<<-df$bet_X
  ndf$bet_2<<-df$bet_2
  ndf$bet_O<<-df$bet_O
  ndf$bet_U<<-df$bet_U
  ndf$t1Classification<<-df$t1Classification
  ndf$t2Classification<<-df$t2Classification
  ndf$mfd <<- ndf$mfd1-ndf$mfd2
  
  ndf$t1Form <<- df$t1Form 
  ndf$t2Form <<- df$t2Form 
  ndf$f1d <<- f1d
  ndf$f2d <<- f2d
  ndf$f3d <<- f3d
  ndf$f4d <<- f4d
  
  rm(datk,datkin,datkout,ddef,ddefin,ddefout,doav_ht,doav_ft,dav_htin,dav_htout,
     dav_ftin,dav_ftout, owd,odd,old,dwin,dwout,ddin,ddout,dlin,dlout,pd,fd,mfd1,mfd2,f1d,f2d,f3d,f4d,
     t1adoe,t2adoe,t1e,t2e )
}


#--------Test & Stuff
trPaths <- c("hua","mua","beladona","maracaibo")

for(path in trPaths){
  print(path);
}