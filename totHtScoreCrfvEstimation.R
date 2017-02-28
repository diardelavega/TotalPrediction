


# supose we have dtf & ndtf datasets
totHtCrfvInit <- function(fld=10, betNoBet='nobet',fff='f5'){
 #fld =folds, 
#betNoBet={bet,nobet,betnobet}, 
#fff{f2,f5,f25}(f is assumed, specify additional partial calc.)

  
  
  #initializes datasets and call for the crfv(cross fold validation) accuracy estimation
  folds <- fld;
  thtDtf <<- CleanTotHtDtf$new(predAtt="totHt")
  
  # crfv_TotHtscore_Struct <<- c() # the struct that will keep all the dataStores created
  bestOfSize <- 3
  ret <- totHtScorePredFunc("f",folds,bestOfSize,betNoBet)   # complet dataset crfv
  # crfv_TotHtscore_Struct[length(crfv_TotHtscore_Struct)+1:2] <<-ret
  thtDtf$algoDataList[length(thtDtf$algoDataList)+1:2]<<-ret
  
  if(fff=='f2' || fff=='f25'){
	  if(max(ndtf$week)>20){  # second half dataset crfv
		bestOfSize <- 3
		ret <- totHtScorePredFunc("f2",folds,bestOfSize,betNoBet)
		# crfv_TotHtscore_Struct[length(crfv_TotHtscore_Struct)+1:2] <<-ret
		thtDtf$algoDataList[length(thtDtf$algoDataList)+1:2]<<-ret
	  }
  }
  
  if(fff=='f5' || fff=='f25'){
	  if(max(ndtf$week)>10){  # last 6 weeks dataset crfv
		bestOfSize <- 1
		ret <- totHtScorePredFunc("f5",folds,bestOfSize,betNoBet)
		# crfv_TotHtscore_Struct[length(crfv_TotHtscore_Struct)+1:2] <<-ret
		thtDtf$algoDataList[length(thtDtf$algoDataList)+1:2]<<-ret
	  }
  }
  
  #------- someway to store the  crfv_dts_Struct
}

totHtScorePredFunc <- function(dataframeCategory,crfoldNr,bestOfSize,betNoBet){
  # executes the crfv for all the algorithms we provide and stores the best results
  #  since we are handling goals the accuracy value is actually the squared mean error rate of the prediction algorithms
  # we leave the var acc & accuracy for conventon
  
  fds <- AlgoData$new(dtfCategory=dataframeCategory)  # to keep the instances of the  full datasets
  dds <- AlgoData$new(dtfCategory=dataframeCategory)  # to keep the instances of the  diff datasets
  
  switch (dataframeCategory,
          "f"  = {dataset_f <- dtf; dataset_d<- ndtf},
          "f2" = {dataset_f <- dtf[which(dtf$week>max(dtf$week)/2),]; 
          dataset_d <- ndtf[which(ndtf$week>max(ndtf$week)/2),]},
          "f5" = {dataset_f <- dtf[which(dtf$week>max(dtf$week)-6),]; 
          dataset_d <- ndtf[which(ndtf$week>max(ndtf$week)-6),]}
  )
  
  for(algorithm in c( "glm","svm","lm","bagging","Bagging"
  )){
    print(algorithm)
    
    set.seed(1234)
    folds_f <- split(dataset_f, cut(sample(1:nrow(dataset_f)),crfoldNr)) # full folds
    set.seed(1234)
    folds_d <- split(dataset_d, cut(sample(1:nrow(dataset_d)),crfoldNr)) # differenced
    
    
    accuracy_df <- list()
    accuracy_ndf <- list()
    
	if(betNoBet=='betnobet' || betNoBet=='bet' ){
		for(i in 1:fullTotHtBet(-1)){
		  acc <- totHtScoreCrfv(fullTotHtBet(i),algorithm,folds_f)
		  ins<- Instance$new(algo = algorithm, attsDtsNr=i, accVal=acc, original_accVal=acc, bet="yes", fullDiff="full",dfCategory=dataframeCategory,ptype="numeric")
		  accuracy_df[length(accuracy_df)+1] <- ins
		}
	}
	if(betNoBet=='betnobet' || betNoBet=='nobet' ){
		for(i in 1:fullTotHtNoBet(-1)){
		  acc <- totHtScoreCrfv(fullTotHtNoBet(i),algorithm,folds_f)
		  ins<- Instance$new(algo = algorithm, attsDtsNr=i, accVal=acc, original_accVal=acc, bet="no", fullDiff="full",dfCategory=dataframeCategory,ptype="numeric")
		  accuracy_df[length(accuracy_df)+1] <- ins
		}
	}
    #--- choose 3 instances with best results
    cur3best <- totHtTreBestChoser(accuracy_df,bestOfSize)
    fds$instList[length(fds$instList)+1 :length(cur3best)]  <- cur3best
    
    if(betNoBet=='betnobet' || betNoBet=='bet' ){
		for(i in 1:differencedTotHtBet(-1)){
		  acc <- totHtScoreCrfv(differencedTotHtBet(i),algorithm,folds_d)
		  ins<- Instance$new(algo = algorithm, attsDtsNr=i, accVal=acc, original_accVal=acc, bet="yes", fullDiff="diff",dfCategory=dataframeCategory,ptype="numeric")
		  accuracy_ndf[length(accuracy_ndf)+1] <- ins
		}
	}
	if(betNoBet=='betnobet' || betNoBet=='nobet' ){
		for(i in 1:differencedTotHtNoBet(-1)){
		  acc <- totHtScoreCrfv(differencedTotHtNoBet(i),algorithm, folds_d)
		  ins<- Instance$new(algo = algorithm, attsDtsNr=i, accVal=acc, original_accVal=acc, bet="no", fullDiff="diff",dfCategory=dataframeCategory,ptype="numeric")
		  accuracy_ndf[length(accuracy_ndf)+1] <- ins
		}
	}
    #--- choose 3 instances with best results
    cur3best <- totHtTreBestChoser(accuracy_ndf,bestOfSize)
    dds$instList[length(dds$instList)+1 :length(cur3best)]  <- cur3best
  }# for algorithms
  return (c(fds,dds)) 
}

totHtTreBestChoser <- function(lst,bestOfSize){
  # i know that the first 3 instances i put in have different err between them
  # after the third i check only if the new accval is biger than the worst one 
  # in, otherwise (worst accval or equal i dont want it)
  #browser()
  bv <-c()
  bv[1]<- lst[1]
  #browser()
  for (i in 2:length(lst)){
    if(bv[[length(bv)]]$accVal == lst[[i]]$accVal){ next }
    else if (length(bv)<bestOfSize) { 
      bv[length(bv)+1] <- lst[i] 
    }
    
    # the best instances are the ones with lowest error rate
    else{ # with 3 instances in, compare to find and eliminate the worst
      if(bestOfSize==1){
        if(bv[[1]]$accVal > lst[[i]]$accVal) { bv[[1]] <- lst[[i]]}
      }
      else if(bestOfSize==3){
        #@TODO fix for best of size ==1
        if(bv[[1]]$accVal > bv[[2]]$accVal) {
          tmpInstance <-bv[[1]];  bv[[1]] <- bv[[2]];  bv[[2]] <- tmpInstance; }
        if(bv[[2]]$accVal > bv[[3]]$accVal) {
          if(bv[[1]]$accVal > bv[[3]]$accVal) { 
            tmpInstance <-bv[[3]];  bv[[3]] <- bv[[2]];  bv[[2]] <- bv[[1]]; bv[[1]] <- tmpInstance }
          else {tmpInstance <-bv[[3]];  bv[[3]] <- bv[[2]]; bv[[2]] <- tmpInstance}
        }
        if(bv[[3]]$accVal > lst[[i]]$accVal){ 
          if(lst[[i]]$accVal != bv[[1]]$accVal & lst[[i]]$accVal != bv[[2]]$accVal)
            bv[3] <- lst[i] }
      }# el if size==3
    }
  }# for
  
  if(bestOfSize==3){
    if(length(bv)==3){
    if(bv[[2]]$accVal > bv[[3]]$accVal) {# last sorting
      if(bv[[1]]$accVal > bv[[3]]$accVal) { 
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

totHtScoreCrfv <-function(ho,algorithm,folds){
  if(is.na(ho)){
    print("This ho is not found") 
    return(0)
  }
  
  erre <- c()
  #calc cros fold validation given the attributes involved
  for (i in 1:length(folds)) {
    test <- ldply(folds[i], data.frame)
    train <- ldply(folds[-i], data.frame)
    tmp.model<-c()
    # browser()
    #tmp.model <- algorithm(ho , train, method = "class")#,trails=trails)
         if(algorithm=="svm"){tmp.model <- svm(ho , train)}
    else if(algorithm=="naiveBayes"){tmp.model <- naiveBayes(ho , train )}
    else if(algorithm=="bagging"){tmp.model <- bagging(ho , train )}
    else if(algorithm=="lm"){tmp.model <- lm(ho , train )}
    else if(algorithm=="glm"){tmp.model <- glm(ho , train, family=poisson(link = "log") )}
    else if(algorithm=="Bagging"){tmp.model <- Bagging(ho , train )}
    
    tmp.predict <- predict(tmp.model, newdata = test)
    
    erre[i]<-sqrt(mean( (tmp.predict-test$totHtScore)^2 ))
    # ---------------------
  }
  print(sprintf("mean squared error rate with k-fold cross-validation: %.3f percent ", mean(erre)))
  return(mean(erre))
}

#---------------------TotHtscore
#--------------------------------------------------------------------------
