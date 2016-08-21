# Predict the results for the next set of matches

# load the objects & liraries -> predict & stroe the prediction in a csv file

#think for the reevaluation of the prediction (after we have the actual results of the match) 

# *paths are vectors of string with the paths to the files of every competition to be predicted
predictAll <- function(dtfPaths,trainPachs,testPaths){

  predAtt_Loader();
  libLoader();
  
  for(i in 1:length( dtfPaths)){
    print(dtfPaths[i]);
    # dtfObjLoader(dtfPaths[i]);  #fuppose that from here we have dtf objs
    load(dtfPaths[i])
    
    print(testPaths[i]);
    tt <<- read.csv(testPaths[i])  #test dataset/weekly matches
    dtf <<- tt        #to call the diffFunc with the hardcoded "dtf" as dataframe
    ntt <<- diffFunc();   #with ntt for the diff based  attributes & datasets
    tt$mfd1 <<- ntt$mfd1
    tt$mfd2 <<- ntt$mfd2
    tt$odd <<- ntt$odd 
    tt$old <<- ntt$old 
    tt$owd <<- ntt$owd 
    
    print(trainPachs[i]);
    dtf <<- read.csv(trainPachs[i]); # train datasets
    ndtf <<- diffFunc();
    
    
    print(hDtf$predAtt)
    
    #  cal dtf objs to make prediction fotr the matches in hand
    hDtf$predCalcScore();
    csDtf$predCalcScore();
    p1Dtf$predCalcScore();
    p2Dtf$predCalcScore();
    tftDtf$predCalcScore();
    thtDtf$predCalcScore();
    
    # dirName <- path to temp directory 
    #--------------------
    # some algorithms require the same levels between train & test dataset attributes
    # thats why we use levels
    
    
  }
}


dirMker <- function(path){
    # file_path <- patha
    fileName<- gsub("DTF","WeekPredPoints",file_path);
    fileName<- gsub(".dtf.RData",".csv",fileName);
    pathSegment <- strsplit(fileName,"/")[[1]];
    dirName <- paste0(pathSegment[1:length(pathSegment)-1],collapse = "/")
    if(!dir.exists(dirName)){
      dir.create(dirName,recursive = T,mode = 753)
    }
    if(dir.exists(dirName)){
      print(dirName);
    }
  }

dtfObjLoader <- function(path){
  # load the dtf ojects from the file
  load(path);
}
dtfobjcleaner <- function(){
  # after we finished pur work with the dtf objs remove them to let space for the next set of them
  rm(list = c(hDtf,csDtf,p1Dtf,p2Dtf,tftDtf,thtDtf));
}

libLoader <- function(){
  # loads libraries needed for the predictive algorithms to work
  library(plyr)
  library(e1071)  #svm
  library(C50)
  library(randomForest)
  library(ipred)
  library(RWeka)
  library(rpart)
  library(tree)
}
predAtt_Loader <- function(){
  # files containing pred_attribute dataset << pred_att ~ {att1,att2,...attn} >>
  source("C:/TotalPrediction/Head_AttPredDataset.R");
  source("C:/TotalPrediction/Score_AttPredDataset.R");
  source("C:/TotalPrediction/P1_AttPredDataset.R");
  source("C:/TotalPrediction/P2_AttPredDataset.R");
  source("C:/TotalPrediction/totFt_AttPredDataset.R");
  source("C:/TotalPrediction/totHt_AttPredDataset.R");
}

diffFunc <- function(){
  t1adoe <- dtf$t1AtackIn - abs(dtf$t2DefenseOut) # because defence is negative nr
  t2adoe <- abs(dtf$t1DefenseIn) - dtf$t2AtackOut
  t1e <- dtf$t1Atack - abs(dtf$t2Defense) # because defence is negative nr
  t2e <- dtf$t2Atack - abs(dtf$t1Defense) 
  #----------
  datk <- dtf$t1Atack-dtf$t2Atack
  datkin <- dtf$t1AtackIn-dtf$t2AtackIn
  datkout <- dtf$t1AtackOut-dtf$t2AtackOut
  ddef <- dtf$t1Defense-dtf$t2Defense
  ddefin <- dtf$t1DefenseIn-dtf$t2DefenseIn
  ddefout <- dtf$t1DefenseOut-dtf$t2DefenseOut
  
  
  doav_ht <- dtf$t1AvgHtScoreIn-dtf$t2AvgHtScoreOut
  doav_ft <- dtf$t1AvgFtScoreIn-dtf$t2AvgFtScoreOut
  #----------
  dav_htin <- dtf$t1AvgHtScoreIn-dtf$t2AvgHtScoreIn
  dav_htout <-  dtf$t1AvgHtScoreOut-dtf$t2AvgHtScoreOut
  dav_ftin <- dtf$t1AvgFtScoreIn-dtf$t2AvgFtScoreIn
  dav_ftout <-  dtf$t1AvgFtScoreOut-dtf$t2AvgFtScoreOut
  
  
  owd <- dtf$t1WinsIn-dtf$t2WinsOut
  odd <- dtf$t1DrawsIn- dtf$t2DrawsOut
  old <- dtf$t1LosesIn - dtf$t2LosesOut
  #----------
  dwin <- dtf$t1WinsIn-dtf$t2WinsIn
  dwout <- dtf$t1WinsOut-dtf$t2WinsOut
  ddin <- dtf$t1DrawsIn-dtf$t2DrawsIn
  ddout <- dtf$t1DrawsOut-dtf$t2DrawsOut
  dlin <- dtf$t1LosesIn-dtf$t2LosesIn
  dlout <- dtf$t1LosesOut-dtf$t2LosesOut
  
  pd <- dtf$t1Points-dtf$t2Points
  fd <- dtf$t1Form-dtf$t2Form
  
  mfd1<-c()
  mfd2<-c()
  for(i in 1:dim(dtf)[1]){mfd1[i] <- mean(dtf[i,13],dtf[i,14],dtf[i,15],dtf[i,16])}
  for(i in 1:dim(dtf)[1]){mfd2[i] <- mean(dtf[i,39],dtf[i,40],dtf[i,41],dtf[i,42])}
  #owd <- dtf$t1WinsIn-dtf$t2WinsOut
  #odd <- dtf$t1DrawsIn- dtf$t2DrawsOut
  #old <- dtf$t1LosesIn - dtf$t2LosesOut
  #----------dtf data
  dtf$mfd1 <<-mfd1
  dtf$mfd2 <<-mfd2
  dtf$odd <<- odd
  dtf$old <<- old
  dtf$owd <<-owd
  
  #ttdf$mfd1 <-mfd1
  #ttdf$mfd2 <-mfd2
  #ttdf$odd <- odd
  #tdf$old<- old
  #tdf$owd <-owd
  #----------------
  
  
  f1d <- dtf[,13]-dtf[,39]
  f2d <- dtf[,14]-dtf[,40]
  f3d <- dtf[,15]-dtf[,41]                 
  f4d <- dtf[,16]-dtf[,42]
  #--------------
  ndf <- data.frame( 
    mfd1,mfd2,pd,fd,
    #  f1d,f2d,f3d,f4d,
    t1adoe,t2adoe,t1e,t2e,
    owd,odd,old,
    dwin,dwout,ddin,ddout,dlin,dlout,
    datk,datkin,datkout,ddef,ddefin,ddefout,
    doav_ht,doav_ft,
    dav_htin,dav_htout,dav_ftin,dav_ftout
  )
  
  
  ndf$week <- dtf$week
  ndf$headOutcome <-dtf$headOutcome
  ndf$scoreOutcome<-dtf$scoreOutcome
  ndf$ht1pOutcome <-dtf$ht1pOutcome
  ndf$ht2pOutcome <-dtf$ht2pOutcome
  ndf$ggOutcome <-dtf$ggOutcome
  ndf$totHtScore  <- dtf$totHtScore 
  ndf$totFtScore <-dtf$totFtScore
  ndf$t1 <-dtf$t1
  ndf$t2<-dtf$t2
  ndf$bet_1<-dtf$bet_1
  ndf$bet_X<-dtf$bet_X
  ndf$bet_2<-dtf$bet_2
  ndf$bet_O<-dtf$bet_O
  ndf$bet_U<-dtf$bet_U
  ndf$t1Classification<-dtf$t1Classification
  ndf$t2Classification<-dtf$t2Classification
  ndf$mfd <- ndf$mfd1-ndf$mfd2
  
  ndf$t1Form <- dtf$t1Form 
  ndf$t2Form <- dtf$t2Form 
  ndf$f1d <- f1d
  ndf$f2d <- f2d
  ndf$f3d <- f3d
  ndf$f4d <- f4d
  
  # rm(datk,datkin,datkout,ddef,ddefin,ddefout,doav_ht,doav_ft,dav_htin,dav_htout,
  #    dav_ftin,dav_ftout, owd,odd,old,dwin,dwout,ddin,ddout,dlin,dlout,pd,fd,mfd1,mfd2,f1d,f2d,f3d,f4d,
  #    t1adoe,t2adoe,t1e,t2e )
  
  return(ndf);
}

#-------------Test & try
dtf <- read.csv("c:/BastData/Pred/Data/Norway/Eliteserien__112__Data")
dim(dtf)
ndf <- diffFunc()

trpath <-  "C:/BastData/Pred/Data/Norway/Eliteserien__112__Data"

dtfpath <- "C:/BastData/DTF/Norway/Eliteserien__112.dtf.RData"

tspath <-  "C:/BastData/Pred/Test/Norway/Eliteserien__112__Test__2016-07-29"


