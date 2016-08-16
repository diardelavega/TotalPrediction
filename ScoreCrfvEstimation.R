
# supose we have dtf & ndtf datasets
scoreCrfvInit <- function(){
#initializes datasets and call for the crfv accuracy estimation
  folds <- 10;
  csDtf <<- CleanScoreDtf$new(predAtt="score")
  
  crfv_score_Struct <<- c() # the struct that will keep all the dataStores created
  bestOfSize <-3
  ret <- scorePredFunc("f" ,folds,bestOfSize)   # complet dataset crfv
  # crfv_score_Struct[length(crfv_score_Struct)+1:2] <<-ret
  csDtf$algoDataList[length(csDtf$algoDataList)+1:2]<<-ret

  if(max(ndtf$week)>20){  # second half dataset crfv
    bestOfSize <-3
    ret <- scorePredFunc("f2",folds,bestOfSize)
    # crfv_score_Struct[length(crfv_score_Struct)+1:2] <<-ret
    csDtf$algoDataList[length(csDtf$algoDataList)+1:2]<<-ret
  }
  
  if(max(ndtf$week)>10){  # last 6 weeks dataset crfv
    bestOfSize <-1
    ret <- scorePredFunc("f5",folds,bestOfSize)
    # crfv_score_Struct[length(crfv_score_Struct)+1:2] <<-ret
    csDtf$algoDataList[length(csDtf$algoDataList)+1:2]<<-ret
  }
  
  #------- someway to store the  crfv_dts_Struct
}

scorePredFunc <- function(dataframeCategory,crfoldNr,bestOfSize){
  # executes the crfv for all the algorithms we provide and stores the best results
  # return  AlgoData obj of the requested dataframe category {f,f2,f5}
  
  fds <- AlgoData$new(dtfCategory=dataframeCategory)  # to keep the instances of the  full datasets
  dds <- AlgoData$new(dtfCategory=dataframeCategory)  # to keep the instances of the  diff datasets
  
  switch (dataframeCategory,
    "f"  = {dataset_f <- dtf; dataset_d<- ndtf},
    "f2" = {dataset_f <- dtf[which(dtf$week>max(dtf$week)/2),]; 
            dataset_d<- ndtf[which(ndtf$week>max(ndtf$week)/2),]},
    "f5" = {dataset_f <- dtf[which(dtf$week>max(dtf$week)-6),]; 
            dataset_d<- ndtf[which(ndtf$week>max(ndtf$week)-6),]}
  )
  
  for(algorithm in c("C50"
                     ,"J48","svm","naiveBayes","randomForest","rpart","bagging", "PART","JRip","AdaBoostM1", "OneR"
                     )){
    print(algorithm)
    
    set.seed(1234)
    folds_f <- split(dataset_f, cut(sample(1:nrow(dataset_f)),crfoldNr)) # full folds
    set.seed(1234)
    folds_d <- split(dataset_d, cut(sample(1:nrow(dataset_d)),crfoldNr)) # differenced
    
   
    accuracy_df <- list()
    accuracy_ndf <- list()
    
    for(i in 1:fullScoreBet(-1)){
      acc <- scoreCrfv(fullScoreBet(i),algorithm,folds_f)
      ins<- Instance$new(algo = algorithm, attsDtsNr=i, accVal=acc, original_accVal=acc, bet="yes", fullDiff="full", dfCategory=dataframeCategory,ptype="categoric")
      accuracy_df[length(accuracy_df)+1] <- ins
    }
    for(i in 1:fullScoreNoBet(-1)){
      acc <- scoreCrfv(fullScoreNoBet(i),algorithm,folds_f)
      ins<- Instance$new(algo = algorithm, attsDtsNr=i, accVal=acc, original_accVal=acc, bet="no", fullDiff="full", dfCategory=dataframeCategory,ptype="categoric")
      accuracy_df[length(accuracy_df)+1] <- ins
    }
    #--- choose 3 instances with best results
    cur3best <- scoreTreBestChoser(accuracy_df,bestOfSize)
    fds$instList[length(fds$instList)+1 :length(cur3best)]  <- cur3best

    
    for(i in 1:differencedScoreBet(-1)){
      acc <- scoreCrfv(differencedScoreBet(i),algorithm,folds_d)
      ins<- Instance$new(algo = algorithm, attsDtsNr=i, accVal=acc, original_accVal=acc, bet="yes", fullDiff="diff", dfCategory=dataframeCategory,ptype="categoric")
      accuracy_ndf[length(accuracy_ndf)+1] <- ins
    }
    for(i in 1:differencedScoreNoBet(-1)){
      acc <- scoreCrfv(differencedScoreNoBet(i),algorithm, folds_d)
      ins<- Instance$new(algo = algorithm, attsDtsNr=i, accVal=acc, original_accVal=acc, bet="no", fullDiff="diff", dfCategory=dataframeCategory,ptype="categoric")
      accuracy_ndf[length(accuracy_ndf)+1] <- ins
    }
    #--- choose 3 instances with best results
    cur3best <- scoreTreBestChoser(accuracy_ndf,bestOfSize)
    dds$instList[length(dds$instList)+1 :length(cur3best)]  <- cur3best
  }# for algorithms
 return (c(fds,dds)) 
}

scoreTreBestChoser <- function(lst,bestOfSize){
  # i know that the first 3 instances i put in have different acc between them
  # after the third i check onli if the new accval is biger than the worst one 
  # in, otherwise (worst accval or equal i dont want it)
  # return a list of Instabce obj with the best accVal
  
  bv <-c()
  bv[1]<- lst[1]
  #browser()
  for (i in 2:length(lst)){
    if(bv[[length(bv)]]$accVal == lst[[i]]$accVal){ next }
    else if (length(bv)<bestOfSize) { 
        bv[length(bv)+1] <- lst[i] 
    }
    else{ # with 3 instances in, compare to find and eliminate the worst
      if(bestOfSize==1){
        if(bv[[1]]$accVal < lst[[i]]$accVal) { bv[[1]] <- lst[[i]]}
      }
      else if(bestOfSize==3){
        #@TODO fix for best of size ==1
        if(bv[[1]]$accVal < bv[[2]]$accVal) {
          tmpInstance <-bv[[1]];  bv[[1]] <- bv[[2]];  bv[[2]] <- tmpInstance; }
        if(bv[[2]]$accVal < bv[[3]]$accVal) {
          if(bv[[1]]$accVal < bv[[3]]$accVal) { 
            tmpInstance <-bv[[3]];  bv[[3]] <- bv[[2]];  bv[[2]] <- bv[[1]]; bv[[1]] <- tmpInstance }
          else {tmpInstance <-bv[[3]];  bv[[3]] <- bv[[2]]; bv[[2]] <- tmpInstance}
        }
        if(bv[[3]]$accVal < lst[[i]]$accVal){ 
          if(lst[[i]]$accVal != bv[[1]]$accVal & lst[[i]]$accVal != bv[[2]]$accVal)
          bv[3] <- lst[i] }
      }# el if size==3
    }
  }# for
  
  if(bestOfSize==3){
    if(length(bv)==3){
    if(bv[[2]]$accVal < bv[[3]]$accVal) {# last sorting
      if(bv[[1]]$accVal < bv[[3]]$accVal) { 
        tmpInstance <-bv[[3]];  bv[[3]] <- bv[[2]];  bv[[2]] <- bv[[1]]; bv[[1]] <- tmpInstance }
      else {tmpInstance <-bv[[3]];  bv[[3]] <- bv[[2]]; bv[[2]] <- tmpInstance}
    }
    }
  }
  for(k in 1:length(bv)){
    print(bv[[k]]$accVal)  
  }
  
  return (bv[1:length(bv)])
}

scoreCrfv <-function(ho,algorithm,folds){
  if(is.na(ho)){
    print("This ho is not found") 
    return(0)
  }
  
  
  accuracy <- c()
  #calc cros fold validation given the attributes involved
  for (i in 1:length(folds)) {
    test <- ldply(folds[i], data.frame)
    train <- ldply(folds[-i], data.frame)
    tmp.model<-c()
    # browser()
    #tmp.model <- algorithm(ho , train, method = "class")#,trails=trails)
    if(algorithm=="C50"){tmp.model <- C5.0(ho , train,trails=10)}
    else if(algorithm=="J48"){tmp.model <- J48(ho , train)}
    else if(algorithm=="svm"){tmp.model <- svm(ho , train)}
    else if(algorithm=="naiveBayes"){tmp.model <- naiveBayes(ho , train )}
    else if(algorithm=="randomForest"){tmp.model <- randomForest(ho , train )}
    
    else if(algorithm=="rpart"){tmp.model <- rpart(ho , train)}
    else if(algorithm=="bagging"){tmp.model <- bagging(ho , train )}
    #else if(algorithm=="bootest"){tmp.model <- bootest(ho , train, method = "class")}
    
    else if(algorithm=="PART"){tmp.model <- PART(ho , train )}
    else if(algorithm=="JRip"){tmp.model <- JRip(ho , train )}
    else if(algorithm=="OneR"){tmp.model <- OneR(ho , train )}
    else if(algorithm=="AdaBoostM1"){tmp.model <- AdaBoostM1(ho , train )}
    #else if(algorithm=="MultiBoostAB"){tmp.model <- MultiBoostAB(ho , train )}
    # browser()
    
    tmp.predict <- predict(tmp.model, newdata = test, type = "class")
    
    #--- totFtScore pred
      # derivedPred <-c()
      # tmpAcc=0
      # for (j in 1:length(tmp.predict)){
      #   if(tmp.predict[j]>=3 && test$scoreOutcome[j]=="O"){tmpAcc=tmpAcc+1}
      #   else if(tmp.predict[j]<=2 && test$scoreOutcome[j]=="U"){tmpAcc=tmpAcc+1}
      # }
      # accuracy[i] <- tmpAcc/length(tmp.predict)
    # ---------------------
      
    conf.mat <- table(test$scoreOutcome, tmp.predict)
    #errs[i] <- 1-sum(diag(conf.mat))/sum(conf.mat)
    accuracy[i] <- sum(diag(conf.mat)) / sum(conf.mat)
  }
  #gen_accuracy[length(gen_accuracy)+1]<<-100*mean(accuracy)
  #print(sprintf("average error using k-fold cross-validation: %.3f percent", 100*mean(errs)))
  print(sprintf("average accuracy using k-fold cross-validation: %.3f percent ", 100*mean(accuracy)))
  return(100*mean(accuracy))
}

#-----------
