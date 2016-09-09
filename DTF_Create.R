#page to be used for loading /7 initiating all the data for the creation of DTF obj

runAll<- function(trPaths,dtfKind){
  #trPaths is a vector with all the tr paths of the competitions
  # dtfKind is a vector with the kind of dtf that we want to create {h,s,p1,p2,ht,ft}
  
  
  # -0 load the dtf-crfv-estimation files (head,score,1p,2p,ht,ft)
  DTFLoader();
  predAtt_Loader();
  libLoader();
  dataStructLoader();
  
  for(path in trPaths){
    
    tryCatch({
      # -1  create datasets to work with
      dtf <<- read.csv(path);
      ndtf <<- diffFunc();
      
     dirNam<- dirmaker(path);# create the folder of the competiton where tho dtf object files will be stored
      
      # -2 start the object that will hold the pred data CREATION
     
     if("h" %in% dtfKind){
       if(!ishead(dirNam)){       # if file doesnt exzist
        headCrfvInit();          # ret :hDtf  calculate
         headmaker(dirNam)       # store
       }}
     
     if("s" %in% dtfKind){
       if(!isscore(dirNam)){
         scoreCrfvInit();         # ret :csDtf 
         scoremaker(dirNam)
       }}
     
     if("ft" %in% dtfKind){
       if(!isft(dirNam)){
         totFtCrfvInit();         # ret :tftDtf  
         ftmaker(dirNam);
       }}
     
     
     if(mean(dtf$t1AvgHtScoreIn)<=0){
       # average of ht scores is 0 or less (-1) ->  no real ht results
       # so skip the ht dtf objects
       next;
     }
     
     if("p1" %in% dtfKind){
       if(!isp1(dirNam)){
         p1CrfvInit();            # ret :p1Dtf 
         p1maker(dirNam);
       }}
     
     
     if("p2" %in% dtfKind){  
       if(!isp2(dirNam)){
          p2CrfvInit();            # ret :p2Dtf
         p2maker(dirNam);
       }}
     
       if("ht" %in% dtfKind){  
     if(!isht(dirNam)){
       totHtCrfvInit();         # ret :thtDtf 
       htmaker(dirNam);
     }}
     
      
      
      
    },
    error = function(err) {
      # error handler picks up where error was generated
      print(paste("MY_ERROR:  ",err))
      
    }, 
    finally = {
      # in case of error save whatever can be saved
      fileMaker(path);   # create folder/subfolders & save the dtfs
    }) # END tryCatch
    
    
  }
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

fileMaker <- function(file_path){
  # file_path <- patha
  fileName<- gsub("Pred/Data","DTF",file_path);
  fileName<- gsub("__Data",".dtf.RData",fileName);
  pathSegment <- strsplit(fileName,"/")[[1]];
  dirName <- paste0(pathSegment[1:length(pathSegment)-1],collapse = "/")
  if(!dir.exists(dirName)){
    dir.create(dirName,recursive = T,mode = 753)
  }
  if(dir.exists(dirName)){
    #create a new file and sotre the dtf objs created for the competition
    save(hDtf,csDtf,p1Dtf,p2Dtf,tftDtf,thtDtf,file=fileName);
  }
  print(fileName);
}

dirmaker<- function(trPath){
  dirName  <- gsub("Pred/Data","DTF",trPath);
  dirName  <- gsub("__Data","",dirName);
  if(!dir.exists(dirName)){
    dir.create(dirName);
    # print(dirName);
  }
  return(dirName);
}

ishead<-function(dirPath){
  headFile<- paste0(dirPath,"/head.dtf.RData");
  if(file.exists(headFile)){
    return (TRUE);
  }
  return(FALSE);
}
headmaker<- function(dirPath){
  headFile<- paste0(dirPath,"/head.dtf.RData");
    save(hDtf,file=headFile);
}

isscore<-function(dirPath){
  scoreFile<- paste0(dirPath,"/score.dtf.RData");
  if(file.exists(scoreFile)){
    return (TRUE);
  }
  return(FALSE);
}
scoremaker<- function(dirPath){
  scoreFile<- paste0(dirPath,"/score.dtf.RData");
  save(csDtf,file=scoreFile);
}

isp1<-function(dirPath){
  p1File<- paste0(dirPath,"/p1.dtf.RData");
  if(file.exists(p1File)){
    return (TRUE);
  }
  return(FALSE);
}
p1maker<- function(dirPath){
  p1File<- paste0(dirPath,"/p1.dtf.RData");
  save(p1Dtf,file=p1File);
}

isp2<-function(dirPath){
  p2File<- paste0(dirPath,"/p2.dtf.RData");
  if(file.exists(p2File)){
    return (TRUE);
  }
  return(FALSE);
}
p2maker<- function(dirPath){
  p2File<- paste0(dirPath,"/p2.dtf.RData");
  save(p2Dtf,file=p2File);
}

isht<-function(dirPath){
  htFile<- paste0(dirPath,"/ht.dtf.RData");
  if(file.exists(htFile)){
    return (TRUE);
  }
  return(FALSE);
}
htmaker<- function(dirPath){
  htFile<- paste0(dirPath,"/ht.dtf.RData");
  save(thtDtf,file=htFile);
}

isft<-function(dirPath){
  ftFile<- paste0(dirPath,"/ft.dtf.RData");
  if(file.exists(ftFile)){
    return (TRUE);
  }
  return(FALSE);
}
ftmaker<- function(dirPath){
  ftFile<- paste0(dirPath,"/ft.dtf.RData");
  save(tftDtf,file=ftFile);
}

remover <- function(){
  # aparently we dont need to use remove afterall, we are not saving the entire workspace just the DTF objs
  
  # remove all unwanted vars, save only the dtf objs
  rm(dtf,ndf,DTFLoader,predAtt_Loader,diffFunc);
  # 
  DTFRemover();
  dataStructRemover();
  predAtt_Remover();
  libRemover();
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
libRemover <- function(){
  detach(package:plyr, unload=TRUE)
  detach(package:e1071, unload=TRUE)
  detach(package:C50, unload=TRUE)
  detach(package:randomForest, unload=TRUE)
  detach(package:ipred, unload=TRUE)
  detach(package:RWeka, unload=TRUE)
  detach(package:rpart, unload=TRUE)
  detach(package:tree, unload=TRUE)
}

DTFLoader <- function(){
  source("C:/TotalPrediction/HeadCrfvEstimation.R"); 
  source("C:/TotalPrediction/ScoreCrfvEstimation.R"); 
  source("C:/TotalPrediction/P1CrfvEstimation.R");
  source("C:/TotalPrediction/P2CrfvEstimation.R"); 
  source("C:/TotalPrediction/totHtScoreCrfvEstimation.R"); 
  source("C:/TotalPrediction/totFtScoreCrfvEstimation.R"); 
}
DTFRemover <- function(){
  rm(headCrfvInit,headPredFunc,headTreBestChoser,headCrfv);
  rm(scoreCrfvInit,scorePredFunc,scoreTreBestChoser,scoreCrfv);
  rm(p2CrfvInit,p2PredFunc,p2TreBestChoser,p2Crfv);
  rm(p1CrfvInit,p1PredFunc,p1TreBestChoser,p1Crfv);
  rm(fulltotFtBet,fullTotFtNoBet,differencedTotFtBet,differencedTotFtNoBet)
  rm(totHtCrfvInit,totHtPredFunc,totHtTreBestChoser,totHtCrfv);
  
}

predAtt_Loader <- function(){
  source("C:/TotalPrediction/Head_AttPredDataset.R");
  source("C:/TotalPrediction/Score_AttPredDataset.R");
  source("C:/TotalPrediction/P1_AttPredDataset.R");
  source("C:/TotalPrediction/P2_AttPredDataset.R");
  source("C:/TotalPrediction/totFt_AttPredDataset.R");
  source("C:/TotalPrediction/totHt_AttPredDataset.R");
}
predAtt_Remover <- function(){
  rm(fullHeadBet,fullHeadNoBet,differencedHeadBet,differencedHeadNoBet)
  rm(fullScoreBet,fullScoreNoBet,differencedScoreBet,differencedScoreNoBet)
  rm(full2pBet,full2pNoBet,differenced2pBet,differenced2pNoBet)
  rm(full1pBet,full1pNoBet,differenced1pBet,differenced1pNoBet)
  rm(totFtCrfvInit,totFtPredFunc,totFtTreBestChoser,totFtCrfv)
  rm(fullTotHtBet,fullTotHtNoBet,differencedTotHtBet,differencedTotHtNoBet);
}

dataStructLoader <- function(){
  # the file with the description of the structure of the DTF obj
  source("C:/TotalPrediction/dataStructure.R");
}
dataStructRemover <- function(){
  rm(Instance,AlgoData,CleanScoreDtf,CleanHeadDtf,Clean2pDtf,Clean1pDtf,CleanTotFtDtf,CleanTotHtDtf)
  rm(modelFunc,attDtsFunc,scoreResultCount,headResultCount,p2ResultCount,p1ResultCount,totFtResultCount,totHtResultCount);
}

test <- function(v, vec){
	dir_nam <- 'C:/ff1/ff2/ff3/ff5';
	fil_nam <- paste(dir_nam,'marioFile.mar',sep="/");
	dir.create(dir_nam,recursive = T,mode = 753)

	for(i in 1:v){
		write(i,fil_nam,append=TRUE);
	}
	for(i in 1:length(vec)){
		write(vec[i],fil_nam,append=TRUE);
	}
	
	return(v+4);
}
