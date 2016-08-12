#libraries required
{
  library(plyr)
  library(e1071)  #svm
  library(C50)
  library(randomForest)
  library(ipred)
  library(RWeka)
  library(rpart)
}


rm(headCrfvInit,headPredFunc,headTreBestChoser,headCrfv)
rm(fullHeadBet,fullHeadNoBet,differencedHeadBet,differencedHeadNoBet)


# supose we have df & ndf datasets
headCrfvInit <- function(){
  # creates a Object (from datastruct prediction_Attribute_object) for a competition that has accumulated weeks of matches
  # in the df & ndf dataframes
  
  #initializes datasets and call for the crfv(cross fold validation) accuracy estimation
  folds <- 10;
  hDtf <<- CleanHeadDtf$new(predAtt="head")
  
  # calc for all the data available (all the weeks of competition)
  bestOfSize <-3                                           #nr of top from best results to chose from
  ret <- headPredFunc("f" ,folds,bestOfSize)               # complet dataset crfv
  hDtf$algoDataList[length(hDtf$algoDataList)+1:2]<<-ret   # the ret val contains two algodata vals from full (df) & diff(ndf)
  
  if(max(ndf$week)>20){  # calc the second half dataset crfv
    bestOfSize <-3
    ret <- headPredFunc("f2",folds,bestOfSize)
    hDtf$algoDataList[length(hDtf$algoDataList)+1:2]<<-ret
  }
  
  if(max(ndf$week)>10){  # calc last 6 weeks dataset crfv
    bestOfSize <-1
    ret <- headPredFunc("f5",folds,bestOfSize)
    hDtf$algoDataList[length(hDtf$algoDataList)+1:2]<<-ret
  }
  
}

headPredFunc <- function(dataframeCategory,crfoldNr,bestOfSize){
  # executes the crfv for all the algorithms we provide and stores the best results
  # return  AlgoData obj of the requested dataframe category {f,f2,f5}
  
  fds <- AlgoData$new(dtfCategory=dataframeCategory)  # to keep the instances of the  full datasets
  dds <- AlgoData$new(dtfCategory=dataframeCategory)  # to keep the instances of the  diff datasets
  
  switch (dataframeCategory,
          "f" = {dataset_f <- df; dataset_d<- ndf},
          "f2" = {dataset_f <- df[which(df$week>max(df$week)/2),]; 
          dataset_d<- ndf[which(ndf$week>max(ndf$week)/2),]},
          "f5" = {dataset_f <- df[which(df$week>max(df$week)-6),]; 
          dataset_d<- ndf[which(ndf$week>max(ndf$week)-6),]}
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
    
    for(i in 1:fullHeadBet(-1)){
      acc <- headCrfv(fullHeadBet(i),algorithm,folds_f)
      ins<- Instance$new(algo = algorithm, attsDtsNr=i, accVal=acc, original_accVal=acc, bet="yes", fullDiff="full", dfCategory=dataframeCategory,ptype="categoric")
      accuracy_df[length(accuracy_df)+1] <- ins
    }
    for(i in 1:fullHeadNoBet(-1)){
      acc <- headCrfv(fullHeadNoBet(i),algorithm,folds_f)
      ins<- Instance$new(algo = algorithm, attsDtsNr=i, accVal=acc, original_accVal=acc, bet="no", fullDiff="full", dfCategory=dataframeCategory,ptype="categoric")
      accuracy_df[length(accuracy_df)+1] <- ins
    }
    #--- choose nr of instances with best results
    cur3best <- headTreBestChoser(accuracy_df,bestOfSize)
    fds$instList[length(fds$instList)+1 :length(cur3best)]  <- cur3best
    
    
    for(i in 1:differencedHeadBet(-1)){
      acc <- headCrfv(differencedHeadBet(i),algorithm,folds_d)
      ins<- Instance$new(algo = algorithm, attsDtsNr=i, accVal=acc, original_accVal=acc, bet="yes", fullDiff="diff", dfCategory=dataframeCategory,ptype="categoric")
      accuracy_ndf[length(accuracy_ndf)+1] <- ins
    }
    for(i in 1:differencedHeadNoBet(-1)){
      acc <- headCrfv(differencedHeadNoBet(i),algorithm, folds_d)
      ins<- Instance$new(algo = algorithm, attsDtsNr=i, accVal=acc, original_accVal=acc, bet="no", fullDiff="diff", dfCategory=dataframeCategory,ptype="categoric")
      accuracy_ndf[length(accuracy_ndf)+1] <- ins
    }
    #--- choose 3 instances with best results
    cur3best <- headTreBestChoser(accuracy_ndf,bestOfSize)
    dds$instList[length(dds$instList)+1 :length(cur3best)]  <- cur3best
  }# for algorithms
  return (c(fds,dds)) 
}

headTreBestChoser <- function(lst,bestOfSize){
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

headCrfv <-function(ho,algorithm,folds){
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
    
    conf.mat <- table(test$headOutcome, tmp.predict)
    #errs[i] <- 1-sum(diag(conf.mat))/sum(conf.mat)
    accuracy[i] <- sum(diag(conf.mat)) / sum(conf.mat)
  }
  #gen_accuracy[length(gen_accuracy)+1]<<-100*mean(accuracy)
  #print(sprintf("average error using k-fold cross-validation: %.3f percent", 100*mean(errs)))
  print(sprintf("average accuracy using k-fold cross-validation: %.3f percent ", 100*mean(accuracy)))
  return(100*mean(accuracy))
}

#-----------
fullHeadBet <- function(i){
  if (i==-1){# return size of atts datasets
    return(5)
  }
  
  if (i==1){#all over 40
    return(  headOutcome~ #mfd1+ mfd2+  odd+  old+  owd+
               # t1+               t1Points+         t1Classification+ 
               t1Form+          
               t1Atack+          t1Defense+        
               # t1AtackIn+        t1AtackOut+       t1DefenseIn+      t1DefenseOut+
               # t1AvgHtScoreIn+   t1AvgHtScoreOut+  t1AvgFtScoreIn+   t1AvgFtScoreOut+  
               # t1AvgHtGgResult+  t1AvgFtGgResult+  
               # t1WinsIn+         t1WinsOut+        t1DrawsIn+        t1DrawsOut+       t1LosesIn+        t1LosesOut+
               # t2+               t2Points+         t2Classification+ 
               t2Form+           
               t2Atack+          t2Defense+        
               # t2DefenseIn+      t2DefenseOut+    t2AtackIn+        t2AtackOut+       
               # t2AvgHtScoreIn+   t2AvgHtScoreOut+  t2AvgFtScoreIn+   t2AvgFtScoreOut+  
               # t2AvgHtGgResult+  t2AvgFtGgResult+ 
               # t2WinsIn+         t2WinsOut+        t2DrawsIn+        t2DrawsOut+       t2LosesIn+        t2LosesOut+      
               bet_1+            bet_X+            bet_2)         
  }
  
  else if(i==2){return( headOutcome~ #mfd1+ mfd2+  odd+  old+  owd+
                          # t1+               
                          t1Form+          t1Points+         t1Classification+
                          # t1Atack+          t1Defense+
                          t1AtackIn+        t1AtackOut+       t1DefenseIn+      t1DefenseOut+
                          # t1AvgHtScoreIn+   t1AvgHtScoreOut+  t1AvgFtScoreIn+   t1AvgFtScoreOut+  
                          # t1AvgHtGgResult+  t1AvgFtGgResult+  
                          # t1WinsIn+         t1WinsOut+        t1DrawsIn+        t1DrawsOut+       t1LosesIn+        t1LosesOut+
                          # t2+               
                          t2Form+         t2Points+         t2Classification+
                          # t2Atack+          t2Defense+
                          t2DefenseIn+      t2DefenseOut+    t2AtackIn+        t2AtackOut+
                          # t2AvgHtScoreIn+   t2AvgHtScoreOut+  t2AvgFtScoreIn+   t2AvgFtScoreOut+  
                          # t2AvgHtGgResult+  t2AvgFtGgResult+ 
                          # t2WinsIn+         t2WinsOut+        t2DrawsIn+        t2DrawsOut+       t2LosesIn+        t2LosesOut+      
                          bet_1+            bet_X+            bet_2)}
  
  else if(i==3){return(headOutcome~ #mfd1+ mfd2+  
                         # odd+  old+  owd+
                         t1+
                         t1Form+          t1Points+         t1Classification+
                         t1Form1Diff+      t1Form2Diff+      t1Form3Diff+      t1Form4Diff+
                         t1Atack+          t1Defense+
                         t1AtackIn+        t1AtackOut+       t1DefenseIn+      t1DefenseOut+
                         t1AvgHtScoreIn+   t1AvgHtScoreOut+  t1AvgFtScoreIn+   t1AvgFtScoreOut+
                         # t1AvgHtGgResult+  t1AvgFtGgResult+
                         # t1WinsIn+         t1WinsOut+        t1DrawsIn+        t1DrawsOut+       t1LosesIn+        t1LosesOut+
                         t2+
                         t2Form+           t2Points+         t2Classification+
                         t2Form1Diff+      t2Form2Diff+      t2Form3Diff+      t2Form4Diff+
                         t2Atack+          t2Defense+
                         t2DefenseIn+      t2DefenseOut+    t2AtackIn+        t2AtackOut+
                         t2AvgHtScoreIn+   t2AvgHtScoreOut+  t2AvgFtScoreIn+   t2AvgFtScoreOut+
                         # t2AvgHtGgResult+  t2AvgFtGgResult+
                         # t2WinsIn+         t2WinsOut+        t2DrawsIn+        t2DrawsOut+       t2LosesIn+        t2LosesOut+
                         bet_1+            bet_X+            bet_2+
                         bet_O+            bet_U
  )}
  
  else if(i==4){return(headOutcome~ #mfd1+ mfd2+  
                         # odd+  old+  owd+
                         # t1+
                         t1Points+         t1Classification+ t1Form+
                         # t1Form1Diff+      t1Form2Diff+      #t1Form3Diff+      t1Form4Diff+      
                         t1AtackIn+        #t1AtackOut+        
                         t1DefenseIn+      #t1DefenseOut+     
                         # t1AvgHtScoreIn+   #t1AvgHtScoreOut+ 
                         # t1AvgFtScoreIn+   #t1AvgFtScoreOut+  
                         #t1AvgHtGgResult+  t1AvgFtGgResult+  
                         t1WinsIn+         #t1WinsOut+       
                         t1DrawsIn+        #t1DrawsOut+       
                         t1LosesIn+        #t1LosesOut+       
                         # t2+
                         t2Points+         t2Classification+ t2Form+
                         # t2Form1Diff+      t2Form2Diff+      #t2Form3Diff+      t2Form4Diff+     
                         t2AtackOut+       #t2AtackIn+               
                         t2DefenseOut+     #t2DefenseIn+      
                         # t2AvgHtScoreOut+  #t2AvgHtScoreIn+   
                         # t2AvgFtScoreOut+  #t2AvgFtScoreIn+   
                         #t2AvgHtGgResult+  t2AvgFtGgResult+ 
                         t2WinsOut+        #t2WinsIn+         
                         t2DrawsOut+       #t2DrawsIn+        
                         t2LosesOut+       #t2LosesIn+         t2LosesOut+
                         bet_1+            bet_X+            bet_2)}
  
  else if(i==5){return(headOutcome~ #mfd1+ mfd2+  
                         odd+  old+  owd+
                         t1+
                         t1Form+          #t1Points+          t1Classification+
                         t1Form1Diff+      t1Form2Diff+      #t1Form3Diff+      t1Form4Diff+
                         # t1Atack+          t1Defense+
                         t1AtackIn+        t1AtackOut+       t1DefenseIn+      t1DefenseOut+
                         # t1AvgHtScoreIn+   t1AvgHtScoreOut+  t1AvgFtScoreIn+   t1AvgFtScoreOut+
                         # t1AvgHtGgResult+  t1AvgFtGgResult+
                         t1WinsIn+         t1WinsOut+        t1DrawsIn+        t1DrawsOut+       t1LosesIn+        t1LosesOut+
                         t2+
                         t2Form+           #t2Points+          t2Classification+
                         t2Form1Diff+      t2Form2Diff+      #t2Form3Diff+      t2Form4Diff+
                         # t2Atack+          t2Defense+
                         t2DefenseIn+      t2DefenseOut+    t2AtackIn+        t2AtackOut+
                         # t2AvgHtScoreIn+   t2AvgHtScoreOut+  t2AvgFtScoreIn+   t2AvgFtScoreOut+
                         # t2AvgHtGgResult+  t2AvgFtGgResult+
                         t2WinsIn+         t2WinsOut+        t2DrawsIn+        t2DrawsOut+       t2LosesIn+        t2LosesOut+
                         bet_1+            bet_X+            bet_2)}
}

# 6 - 9 /4
fullHeadNoBet <- function(i){
  if (i==-1){# return size of atts datasets
    return(4)
  }
  
  if(i==1){return(
    f1  <- headOutcome~  #t1+              
      #t1Points+
      t1Classification+ t1Form+          
      # t1Form1Diff+      t1Form2Diff+      t1Form3Diff+      t1Form4Diff+     
      t1Atack+          t1Defense+        
      # t1AtackIn+        t1AtackOut+       t1DefenseIn+      t1DefenseOut+     
      # t1AvgHtScoreIn+   t1AvgHtScoreOut+ 
      t1AvgFtScoreIn+   t1AvgFtScoreOut+  
      #t1AvgHtGgResult+  t1AvgFtGgResult+  
      t1WinsIn+         t1WinsOut+        t1DrawsIn+        t1DrawsOut+       t1LosesIn+        t1LosesOut+
      # t2+
      #t2Points+       
      t2Classification+ t2Form+ 
      # t2Form1Diff+      t2Form2Diff+      t2Form3Diff+      t2Form4Diff+     
      t2Atack+          t2Defense+        
      # t2AtackIn+        t2AtackOut+       t2DefenseIn+      t2DefenseOut+    
      # t2AvgHtScoreIn+   t2AvgHtScoreOut+ 
      t2AvgFtScoreIn+   t2AvgFtScoreOut+
      #t2AvgHtGgResult+  t2AvgFtGgResult+ 
      t2WinsIn+         t2WinsOut+        t2DrawsIn+        t2DrawsOut+       t2LosesIn+        t2LosesOut+
      #bet_1+            bet_X+            bet_2+       
      owd+  old+ odd+  mfd1+ mfd2
  )}
  
  else if(i==2){return(headOutcome~  t1+              
                         #t1Points+
                         t1Classification+ t1Form+          
                         t1Form1Diff+      t1Form2Diff+      t1Form3Diff+      t1Form4Diff+     
                         t1Atack+          t1Defense+        
                         t1AtackIn+        t1AtackOut+       t1DefenseIn+      t1DefenseOut+     
                         t1AvgHtScoreIn+   t1AvgHtScoreOut+ 
                         t1AvgFtScoreIn+   t1AvgFtScoreOut+  
                         #t1AvgHtGgResult+  t1AvgFtGgResult+  
                         #t1WinsIn+         t1WinsOut+        t1DrawsIn+        t1DrawsOut+       t1LosesIn+        t1LosesOut+     
                         t2+
                         #t2Points+       
                         t2Classification+ t2Form+ 
                         t2Form1Diff+      t2Form2Diff+      t2Form3Diff+      t2Form4Diff+     
                         t2Atack+          t2Defense+        
                         t2AtackIn+        t2AtackOut+       t2DefenseIn+      t2DefenseOut+    
                         t2AvgHtScoreIn+   t2AvgHtScoreOut+  t2AvgFtScoreIn+   t2AvgFtScoreOut  
  )}
  
  else if(i==3){return(headOutcome~ #mfd1+ mfd2+  
                         # odd+  old+  owd+
                         t1+
                         t1Points+         t1Classification+ t1Form+
                         # t1Form1Diff+      t1Form2Diff+      #t1Form3Diff+      t1Form4Diff+      
                         t1AtackIn+        #t1AtackOut+        
                         t1DefenseIn+      #t1DefenseOut+     
                         t1AvgHtScoreIn+   #t1AvgHtScoreOut+
                         t1AvgFtScoreIn+   #t1AvgFtScoreOut+
                         #t1AvgHtGgResult+  t1AvgFtGgResult+  
                         t1WinsIn+         #t1WinsOut+       
                         t1DrawsIn+        #t1DrawsOut+       
                         t1LosesIn+        #t1LosesOut+       
                         t2+
                         t2Points+         t2Classification+ t2Form+
                         # t2Form1Diff+      t2Form2Diff+      #t2Form3Diff+      t2Form4Diff+     
                         t2AtackOut+       #t2AtackIn+               
                         t2DefenseOut+     #t2DefenseIn+      
                         t2AvgHtScoreOut+  #t2AvgHtScoreIn+
                         t2AvgFtScoreOut+  #t2AvgFtScoreIn+
                         #t2AvgHtGgResult+  t2AvgFtGgResult+ 
                         t2WinsOut+        #t2WinsIn+         
                         t2DrawsOut+       #t2DrawsIn+        
                         t2LosesOut 
  )}
  
  else if(i==4){return(
    f1  <- headOutcome~  t1+              
      #t1Points+
      t1Classification+ t1Form+          
      # t1Form1Diff+      t1Form2Diff+      t1Form3Diff+      t1Form4Diff+     
      t1Atack+          t1Defense+        
      # t1AtackIn+        t1AtackOut+       t1DefenseIn+      t1DefenseOut+     
      # t1AvgHtScoreIn+   t1AvgHtScoreOut+ 
      t1AvgFtScoreIn+   t1AvgFtScoreOut+  
      #t1AvgHtGgResult+  t1AvgFtGgResult+  
      t1WinsIn+         t1WinsOut+        t1DrawsIn+        t1DrawsOut+       t1LosesIn+        t1LosesOut+
      t2+
      #t2Points+       
      t2Classification+ t2Form+ 
      # t2Form1Diff+      t2Form2Diff+      t2Form3Diff+      t2Form4Diff+     
      t2Atack+          t2Defense+        
      # t2AtackIn+        t2AtackOut+       t2DefenseIn+      t2DefenseOut+    
      # t2AvgHtScoreIn+   t2AvgHtScoreOut+ 
      t2AvgFtScoreIn+   t2AvgFtScoreOut+
      #t2AvgHtGgResult+  t2AvgFtGgResult+ 
      t2WinsIn+         t2WinsOut+        t2DrawsIn+        t2DrawsOut+       t2LosesIn+        t2LosesOut+
      #bet_1+            bet_X+            bet_2+       
      owd+  old+ odd+  mfd1+ mfd2
  )}
}

# 1 - 3 nrs /3
differencedHeadBet <- function(i){
  if (i==-1){# return size of atts datasets
    return(3)
  }
  
  if(i==1){return(headOutcome~  t1+t2+ mfd1+      mfd2+    t1Classification+ t2Classification+  pd+  fd+  
                    #f1d+ f2d+ f3d+ f4d+
                    t1adoe+          t2adoe+           
                    t1e+             t2e+
                    owd+         odd+          old+    doav_ht+ doav_ft+
                    dwin+        dwout+       ddin+        ddout+       dlin+        dlout+
                    # datkin+      datkout+     ddefin+      ddefout+   dav_ftin+    dav_ftout+
                    bet_1+       bet_X+       bet_2+ bet_O+       bet_U)}
  
  else if(i==2){return(headOutcome~  #t1+ t2+ 
                         # mfd1+      mfd2+    t1Classification+ t2Classification+  pd+  fd+ f1d+ f2d+
                         #f3d+ f4d+
                         # t1adoe+          t2adoe+           
                         # t1e+             t2e+
                         owd+         odd+          old+    
                         # doav_ht+ doav_ft+
                         # dwin+        dwout+       ddin+        ddout+       dlin+        dlout+    
                         datkin+      datkout+     
                         # ddefin+      ddefout+   
                         # dav_ftin+    dav_ftout+
                         bet_1+       bet_X+       bet_2)}
  
  else if(i==3){return(headOutcome~  #t1+ t2+ 
                         # mfd1+      mfd2+    t1Classification+ t2Classification+  
                         pd+  fd+
                         # f1d+ f2d+ #f3d+ f4d+
                         # t1adoe+          t2adoe+
                         # t1e+             t2e+
                         owd+         odd+          old+
                         # doav_ht+ doav_ft+
                         # dwin+        dwout+       ddin+        ddout+       dlin+        dlout+
                         datkin+      datkout+     
                         # ddefin+      ddefout+   
                         # dav_ftin+    dav_ftout+
                         bet_1+       bet_X+       bet_2)}
  
  else if(i==4){return( headOutcome~  t1+ t2+ 
                          mfd1+      mfd2+    t1Classification+ t2Classification+
                          # pd+  fd+
                          # f1d+ f2d+ #f3d+ f4d+
                          # t1adoe+          t2adoe+
                          t1e+             t2e+
                          owd+         odd+          old+
                          # doav_ht+ doav_ft+
                          # dwin+        dwout+       ddin+        ddout+       dlin+        dlout+
                          datkin+      datkout+     
                          # ddefin+      ddefout+   
                          # dav_ftin+    dav_ftout+
                          bet_1+       bet_X+       bet_2#+ bet_O+       bet_U
  )}
  
  else if(i==5){return( headOutcome~  #t1+ t2+ 
                          #mfd1+      mfd2+    t1Classification+ t2Classification+
                          # pd+  fd+
                          f1d+ f2d+ #f3d+ f4d+
                          t1adoe+          t2adoe+
                          # t1e+             t2e+
                          # owd+         odd+          old+
                          # doav_ht+ doav_ft+
                          # dwin+        dwout+       ddin+        ddout+       dlin+        dlout+
                          # datkin+      datkout+     
                          # ddefin+      ddefout+   
                          # dav_ftin+    dav_ftout+
                          bet_1+       bet_X+       bet_2#+ bet_O+       bet_U
  )}
}

# 4 - 7 nrs /4
differencedHeadNoBet <- function(i){
  if (i==-1){# return size of atts datasets
    return(4)
  } 
  #56.21212
  if(i==1){return( headOutcome~#t1+t2+  
                     mfd1+      mfd2+    #
                     #t1Classification+ t2Classification+ 
                     pd+  fd+
                     #f1d+ f2d+ f3d+ f4d+
                     t1adoe+          t2adoe+
                     # t1e+             t2e+
                     owd+         odd+          old+    
                     # doav_ht+ doav_ft+
                     # dwin+        dwout+       ddin+        ddout+       dlin+        dlout+
                     
                     datkin+      datkout+     
                     ddefin+      ddefout
                   
  )}
  
  # not so good results
  else if(i==2){return(headOutcome~#t1+t2+  
                         mfd1+      mfd2+    #
                         #t1Classification+ t2Classification+ 
                         # pd+  fd+
                         #f1d+ f2d+ f3d+ f4d+
                         t1adoe+          t2adoe+
                         # t1e+             t2e+
                         # owd+         odd+          old+    
                         # doav_ht+ doav_ft+
                         # dwin+        dwout+       ddin+        ddout+       dlin+        dlout+
                         
                         datkin+      datkout+     
                         ddefin+      ddefout+
                         dav_ftin+    dav_ftout
  )}
  
  #midle algo -> higher accuracy
  else if(i==3){return(headOutcome~#t1+t2+  
                         mfd1+      mfd2+    #
                         # t1Classification+ t2Classification+
                         pd+  fd+
                         # f1d+ f2d+ #f3d+ f4d+
                         t1adoe+          t2adoe+
                         t1e+             t2e+
                         # owd+         odd+          old+
                         # doav_ht+ doav_ft+
                         dwin+        dwout+       ddin+        ddout+       dlin+        dlout
                       
  )}
  
  else if(i==4){return(headOutcome~t1+t2+  
                         mfd1+      mfd2+    #
                         # t1Classification+ t2Classification+
                         pd+  fd+
                         # f1d+ f2d+ #f3d+ f4d+
                         t1adoe+          t2adoe+
                         t1e+             t2e+
                         owd+         odd+          old+
                         # doav_ht+ doav_ft+
                         dwin+        dwout+       ddin+        ddout+       dlin+        dlout
  )}
}

