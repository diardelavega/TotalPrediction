
#libraries required
{
  library(plyr)
  library(e1071)  #svm
  library(C50)
  library(randomForest)
  library(ipred)
  library(RWeka)
  library(rpart)
  
  #bestOfSize <- 3  
}

# supose we have df & ndf datasets
totFtCrfvInit <- function(){
  folds <- 10;
  tftDtf <<- CleanTotFtDtf$new(predAtt="totFt")
  
  # crfv_TotFtscore_Struct <<- c() # the struct that will keep all the dataStores created
  bestOfSize <- 3
  ret <- totFtScorePredFunc("f",folds,bestOfSize)   # complet dataset crfv
  # crfv_TotFtscore_Struct[length(crfv_TotFtscore_Struct)+1:2] <<-ret
  tftDtf$algoDataList[length(tftDtf$algoDataList)+1:2]<<-ret

  if(max(ndf$week)>20){  # second half dataset crfv
    bestOfSize <- 3
    ret <- totFtScorePredFunc("f2",folds,bestOfSize)
    # crfv_TotFtscore_Struct[length(crfv_TotFtscore_Struct)+1:2] <<-ret
    tftDtf$algoDataList[length(tftDtf$algoDataList)+1:2]<<-ret
  }
  
  if(max(ndf$week)>10){  # last 6 weeks dataset crfv
    bestOfSize <- 1
    ret <- totFtScorePredFunc("f5",folds,bestOfSize)
    # crfv_TotFtscore_Struct[length(crfv_TotFtscore_Struct)+1:2] <<-ret
    tftDtf$algoDataList[length(tftDtf$algoDataList)+1:2]<<-ret
  }
  
  #------- someway to store the  crfv_dts_Struct
}

totFtScorePredFunc <- function(dataframeCategory,crfoldNr,bestOfSize){
  # executes the crfv for all the algorithms we provide and stores the best results
  #  since we are handling goals the accuracy value is actually the squared mean error rate of the prediction algorithms
  # we leave the var acc & accuracy for conventon
    
  fds <- AlgoData$new(dtfCategory=dataframeCategory)  # to keep the instances of the  full datasets
  dds <- AlgoData$new(dtfCategory=dataframeCategory)  # to keep the instances of the  diff datasets
  
  switch (dataframeCategory,
          "f"  = {dataset_f <- df; dataset_d<- ndf},
          "f2" = {dataset_f <- df[which(df$week>max(df$week)/2),]; 
                  dataset_d <- ndf[which(ndf$week>max(ndf$week)/2),]},
          "f5" = {dataset_f <- df[which(df$week>max(df$week)-6),]; 
                  dataset_d <- ndf[which(ndf$week>max(ndf$week)-6),]}
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
    
    for(i in 1:fulltotFtScoreBet(-1)){
      acc <- totFtScoreCrfv(fulltotFtScoreBet(i),algorithm,folds_f)
      ins<- Instance$new(algo = algorithm, attsDtsNr=i, accVal=acc, bet="yes", fullDiff="full",dfCategory=dataframeCategory,ptype="numeric")
      accuracy_df[length(accuracy_df)+1] <- ins
    }
    for(i in 1:fulltotFtScoreNoBet(-1)){
      acc <- totFtScoreCrfv(fulltotFtScoreNoBet(i),algorithm,folds_f)
      ins<- Instance$new(algo = algorithm, attsDtsNr=i, accVal=acc, bet="no", fullDiff="full",dfCategory=dataframeCategory,ptype="numeric")
      accuracy_df[length(accuracy_df)+1] <- ins
    }
    #--- choose 3 instances with best results
    cur3best <- treBestChoser(accuracy_df,bestOfSize)
    fds$instList[length(fds$instList)+1 :length(cur3best)]  <- cur3best
    
    
    for(i in 1:differencedtotFtScoreBet(-1)){
      acc <- totFtScoreCrfv(differencedtotFtScoreBet(i),algorithm,folds_d)
      ins<- Instance$new(algo = algorithm, attsDtsNr=i, accVal=acc, bet="yes", fullDiff="diff",dfCategory=dataframeCategory,ptype="numeric")
      accuracy_ndf[length(accuracy_ndf)+1] <- ins
    }
    for(i in 1:differencedtotFtScoreNoBet(-1)){
      acc <- totFtScoreCrfv(differencedtotFtScoreNoBet(i),algorithm, folds_d)
      ins<- Instance$new(algo = algorithm, attsDtsNr=i, accVal=acc, bet="no", fullDiff="diff",dfCategory=dataframeCategory,ptype="numeric")
      accuracy_ndf[length(accuracy_ndf)+1] <- ins
    }
    #--- choose 3 instances with best results
    cur3best <- treBestChoser(accuracy_ndf,bestOfSize)
    dds$instList[length(dds$instList)+1 :length(cur3best)]  <- cur3best
  }# for algorithms
  return (c(fds,dds)) 
}

totFtTreBestChoser <- function(lst,bestOfSize){
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
    if(bv[[2]]$accVal > bv[[3]]$accVal) {# last sorting
      if(bv[[1]]$accVal > bv[[3]]$accVal) { 
        tmpInstance <-bv[[3]];  bv[[3]] <- bv[[2]];  bv[[2]] <- bv[[1]]; bv[[1]] <- tmpInstance }
      else {tmpInstance <-bv[[3]];  bv[[3]] <- bv[[2]]; bv[[2]] <- tmpInstance}
    }
  }
  for(k in 1:bestOfSize){
    print(bv[[k]]$accVal)  
  }
  
  return (bv[1:bestOfSize])
}

totFtScoreCrfv <-function(ho,algorithm,folds){
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
    if(algorithm=="svm"){tmp.model <- svm(ho , train)}
    else if(algorithm=="naiveBayes"){tmp.model <- naiveBayes(ho , train)}
    else if(algorithm=="bagging"){tmp.model <- bagging(ho , train )}
    else if(algorithm=="Bagging"){tmp.model <- Bagging(ho , train )}
    else if(algorithm=="lm"){tmp.model <- lm(ho , train )}
    else if(algorithm=="glm"){tmp.model <- glm(ho , train,family="gaussian" )}
     
    tmp.predict <- predict(tmp.model, newdata = test)
    
    #--- totFtScore pred  sqrt(1/n * sum[ (pred[i]-val[i])^2 ] )
    # n=length(tmp.predict)
    # pred=mp.predict
    # val =test$totFtScore
    
    erre[i]<-sqrt(mean( (tmp.predict-test$totFtScore)^2 ))
    
  }
  print(sprintf("mean squared error rate with k-fold cross-validation: %.3f percent ", mean(erre)))
  return(mean(erre))
}

#---------------------TotFtscore



fullTotFtBet <- function(i){
  if (i==-1){return (3)}
  
  else if (i==1){
    return(
      totFtScore~  #owd+  old+ odd+  mfd1+ mfd2+
        # t1+
        #t1Points+        t1Form+
        #t1Classification+ 
        # t1Form1Diff+      t1Form2Diff+      t1Form3Diff+      t1Form4Diff+     
        t1Atack+          t1Defense+
        # t1AtackIn+        t1AtackOut+       t1DefenseIn+      t1DefenseOut+     
        t1AvgHtScoreIn+   t1AvgHtScoreOut+
        t1AvgFtScoreIn+   t1AvgFtScoreOut+  
        # t1AvgHtGgResult+  t1AvgFtGgResult+
        t1WinsIn+         t1WinsOut+        t1DrawsIn+        t1DrawsOut+       t1LosesIn+        t1LosesOut+
        # t2+
        #t2Points+       t2Form+
        #t2Classification+ t2Form+
        # t2Form1Diff+      t2Form2Diff+      t2Form3Diff+      t2Form4Diff+
        t2Atack+          t2Defense+
        # t2AtackIn+        t2AtackOut+       t2DefenseIn+      t2DefenseOut+    
        t2AvgHtScoreIn+   t2AvgHtScoreOut+
        t2AvgFtScoreIn+   t2AvgFtScoreOut+
        # t2AvgHtGgResult+  t2AvgFtGgResult+
        # t2WinsIn+         t2WinsOut+        t2DrawsIn+        t2DrawsOut+       t2LosesIn+        t2LosesOut+
        # bet_1+            bet_X+            bet_2
        bet_O+            bet_U
    )
  }
  
  else if (i==2){
    return(
      totFtScore~  #owd+  old+ odd+  mfd1+ mfd2+
        # t1+
        #t1Points+        t1Form+
        #t1Classification+ 
        t1Form1Diff+      t1Form2Diff+      t1Form3Diff+      t1Form4Diff+     
        # t1Atack+          t1Defense+
        # t1AtackIn+        t1AtackOut+       t1DefenseIn+      t1DefenseOut+     
        t1AvgHtScoreIn+   t1AvgHtScoreOut+
        t1AvgFtScoreIn+   t1AvgFtScoreOut+  
        # t1AvgHtGgResult+  t1AvgFtGgResult+
        # t1WinsIn+         t1WinsOut+        t1DrawsIn+        t1DrawsOut+       t1LosesIn+        t1LosesOut+
        # t2+
        #t2Points+       t2Form+
        # t2Classification+ t2Form+
        t2Form1Diff+      t2Form2Diff+      t2Form3Diff+      t2Form4Diff+
        # t2Atack+          t2Defense+
        # t2AtackIn+        t2AtackOut+       t2DefenseIn+      t2DefenseOut+    
        t2AvgHtScoreIn+   t2AvgHtScoreOut+
        t2AvgFtScoreIn+   t2AvgFtScoreOut+
        # t2AvgHtGgResult+  t2AvgFtGgResult+
        #t2WinsIn+         t2WinsOut+        t2DrawsIn+        t2DrawsOut+       t2LosesIn+        t2LosesOut
        #bet_1+            bet_X+            bet_2+
        bet_O+            bet_U
    )
  }
  
  else if (i==2){
    return(totFtScore~  #owd+  old+ odd+  mfd1+ mfd2+
             # t1+
             #t1Points+        t1Form+
             #t1Classification+ 
             t1Form1Diff+      t1Form2Diff+      t1Form3Diff+      t1Form4Diff+     
             # t1Atack+          t1Defense+
             # t1AtackIn+        t1AtackOut+       t1DefenseIn+      t1DefenseOut+     
             t1AvgHtScoreIn+   t1AvgHtScoreOut+
             t1AvgFtScoreIn+   t1AvgFtScoreOut+  
             # t1AvgHtGgResult+  t1AvgFtGgResult+
             # t1WinsIn+         t1WinsOut+        t1DrawsIn+        t1DrawsOut+       t1LosesIn+        t1LosesOut+
             # t2+
             #t2Points+       t2Form+
             # t2Classification+ t2Form+
             t2Form1Diff+      t2Form2Diff+      t2Form3Diff+      t2Form4Diff+
             # t2Atack+          t2Defense+
             # t2AtackIn+        t2AtackOut+       t2DefenseIn+      t2DefenseOut+    
             t2AvgHtScoreIn+   t2AvgHtScoreOut+
             t2AvgFtScoreIn+   t2AvgFtScoreOut+
             # t2AvgHtGgResult+  t2AvgFtGgResult+
             #t2WinsIn+         t2WinsOut+        t2DrawsIn+        t2DrawsOut+       t2LosesIn+        t2LosesOut
             bet_1+            bet_X+            bet_2
           # bet_O+            bet_U
    )
  }
}


fullTotFtNoBet <- function(i){
  if (i==-1){return (4)}
  
  else if (i==1){
    return(
      totFtScore~  #owd+  old+ odd+  mfd1+ mfd2+
        # t1+
        #t1Points+
        t1Classification+ t1Form+
        t1Form1Diff+      t1Form2Diff+      t1Form3Diff+      t1Form4Diff+     
        t1Atack+          t1Defense+
        t1AtackIn+        t1AtackOut+       t1DefenseIn+      t1DefenseOut+     
        t1AvgHtScoreIn+   t1AvgHtScoreOut+
        t1AvgFtScoreIn+   t1AvgFtScoreOut+  
        # t1AvgHtGgResult+  t1AvgFtGgResult+
        t1WinsIn+         t1WinsOut+        t1DrawsIn+        t1DrawsOut+       t1LosesIn+        t1LosesOut+
        # t2+
        # t2Points+
        t2Classification+ t2Form+
        t2Form1Diff+      t2Form2Diff+      t2Form3Diff+      t2Form4Diff+
        t2Atack+          t2Defense+
        t2AtackIn+        t2AtackOut+       t2DefenseIn+      t2DefenseOut+    
        t2AvgHtScoreIn+   t2AvgHtScoreOut+
        t2AvgFtScoreIn+   t2AvgFtScoreOut+
        # t2AvgHtGgResult+  t2AvgFtGgResult+
        t2WinsIn+         t2WinsOut+        t2DrawsIn+        t2DrawsOut+       t2LosesIn+        t2LosesOut
      #bet_1+            bet_X+            bet_2+
      #bet_O+            bet_U
    )
  }
  
  else if (i==2){
    return(totFtScore~  #owd+  old+ odd+  mfd1+ mfd2+
             # t1+
             #t1Points+
             t1Classification+ t1Form+
             # t1Form1Diff+      t1Form2Diff+      t1Form3Diff+      t1Form4Diff+     
             t1Atack+          t1Defense+
             t1AtackIn+        t1AtackOut+       t1DefenseIn+      t1DefenseOut+     
             t1AvgHtScoreIn+   t1AvgHtScoreOut+
             t1AvgFtScoreIn+   t1AvgFtScoreOut+  
             t1AvgHtGgResult+  t1AvgFtGgResult+
             # t1WinsIn+         t1WinsOut+        t1DrawsIn+        t1DrawsOut+       t1LosesIn+        t1LosesOut+
             # t2+
             # t2Points+
             t2Classification+ t2Form+
             # t2Form1Diff+      t2Form2Diff+      t2Form3Diff+      t2Form4Diff+
             t2Atack+          t2Defense+
             t2AtackIn+        t2AtackOut+       t2DefenseIn+      t2DefenseOut+    
             t2AvgHtScoreIn+   t2AvgHtScoreOut+
             t2AvgFtScoreIn+   t2AvgFtScoreOut
           # t2AvgHtGgResult+  t2AvgFtGgResult+
           # t2WinsIn+         t2WinsOut+        t2DrawsIn+        t2DrawsOut+       t2LosesIn+        t2LosesOut
           #bet_1+            bet_X+            bet_2+
           #bet_O+            bet_U
    )}
  
  else if (i==3){
    return(totFtScore~  #owd+  old+ odd+  mfd1+ mfd2+
             # t1+
             #t1Points+        
             t1Form+
             #t1Classification+ 
             t1Form1Diff+      t1Form2Diff+      t1Form3Diff+      t1Form4Diff+     
             # t1Atack+          t1Defense+
             # t1AtackIn+        t1AtackOut+       t1DefenseIn+      t1DefenseOut+     
             t1AvgHtScoreIn+   t1AvgHtScoreOut+
             t1AvgFtScoreIn+   t1AvgFtScoreOut+  
             # t1AvgHtGgResult+  t1AvgFtGgResult+
             # t1WinsIn+         t1WinsOut+        t1DrawsIn+        t1DrawsOut+       t1LosesIn+        t1LosesOut+
             # t2+
             #t2Points+       
             t2Form+
             # t2Classification+ t2Form+
             t2Form1Diff+      t2Form2Diff+      t2Form3Diff+      t2Form4Diff+
             # t2Atack+          t2Defense+
             # t2AtackIn+        t2AtackOut+       t2DefenseIn+      t2DefenseOut+    
             t2AvgHtScoreIn+   t2AvgHtScoreOut+
             t2AvgFtScoreIn+   t2AvgFtScoreOut
           # t2AvgHtGgResult+  t2AvgFtGgResult+
           #t2WinsIn+         t2WinsOut+        t2DrawsIn+        t2DrawsOut+       t2LosesIn+        t2LosesOut
           #bet_1+            bet_X+            bet_2+
           #bet_O+            bet_U
    )
  }
  
  else if (i==4){
    return(totFtScore~ #week+ #t1+
             #t1Form+
             t1Form1Diff+      t1Form2Diff+      #t1Form3Diff+      t1Form4Diff+     
             #t1Atack+          t1Defense+        
             t1AtackIn+        #t1AtackOut+       
             t1DefenseIn+      #t1DefenseOut+     
             t1AvgHtScoreIn+   #t1AvgHtScoreOut+ 
             t1AvgFtScoreIn+   #t1AvgFtScoreOut+
             # t1AvgHtGgResult+  #t1AvgFtGgResult+  
             #t1WinsIn+         t1WinsOut+        t1DrawsIn+        t1DrawsOut+       t1LosesIn+        t1LosesOut+     
             #t2+
             #t2Points+       
             #t2Classification+ 
             #t2Form+ 
             t2Form1Diff+      t2Form2Diff+      #t2Form3Diff+      t2Form4Diff+     
             #t2Atack+          t2Defense+        
             #t2AtackIn+        t2DefenseIn+      
             t2AtackOut+       t2DefenseOut+    
             #t2AvgHtScoreIn+   
             t2AvgHtScoreOut+  
             #t2AvgFtScoreIn+   
             t2AvgFtScoreOut
           # t2AvgHtGgResult+  #t2AvgFtGgResult+ 
           #t2WinsIn+         t2WinsOut+        t2DrawsIn+        t2DrawsOut+       t2LosesIn+        t2LosesOut+      
           # bet_1+            bet_X+            bet_2+       
           # owd+  old+ odd#+  mfd1+ mfd2
           #bet_O+            bet_U
    )
  }
}


differencedTotFtBet- function(i){
  if(i==-1){return(3)}
  
  else if (i==1){return(
    totFtScore~ mfd1+      mfd2+     pd+  fd+  
      # t1+ t2+   t1Form+ t2Form+   t1Classification+ t2Classification+
      f1d+ f2d+    f3d+ f4d+
      t1adoe+      t2adoe+      t1e+         t2e+
      owd+         odd+         old+         
      doav_ht+     doav_ft+
      dav_htin+    dav_htout+
      dav_ftin+    dav_ftout+
      doav_ht+   doav_ft+
      dwin+        dwout+       ddin+      ddout+       dlin+        dlout+ 
      datk+        datkin+      datkout+
      ddef+        ddefin+      ddefout+
      # bet_1+       bet_X+       bet_2+
      bet_O+       bet_U
  )}
  
  else if (i==2){return(
    totFtScore~ 
      # pd+  fd+  mfd1+      mfd2+     
      # t1+ t2+   t1Form+ t2Form+   t1Classification+ t2Classification+
      f1d+ f2d+    f3d+ f4d+
      # t1adoe+      t2adoe+      t1e+         t2e+
      owd+         odd+         old+         
      doav_ht+     doav_ft+
      dav_htin+    dav_htout+
      dav_ftin+    dav_ftout+
      doav_ht+   doav_ft+
      dwin+        dwout+       ddin+      ddout+       dlin+        dlout+ 
      datk+        datkin+      datkout+
      ddef+        ddefin+      ddefout+
      # bet_1+       bet_X+       bet_2+
      bet_O+       bet_U
  )}
  
  else if (i==3){return(
    totFtScore~ 
      # pd+  fd+  mfd1+      mfd2+     
      # t1+ t2+   t1Form+ t2Form+   t1Classification+ t2Classification+
      f1d+ f2d+   # f3d+ f4d+
      # t1adoe+      t2adoe+      t1e+         t2e+
      # owd+         odd+         old+         
      doav_ht+     doav_ft+
      dav_htin+    dav_htout+
      dav_ftin+    dav_ftout+
      doav_ht+   doav_ft+
      dwin+        dwout+       ddin+      ddout+       dlin+        dlout+ 
      datk+        datkin+      datkout+
      ddef+        ddefin+      ddefout+
      # bet_1+       bet_X+       bet_2+
      bet_O+       bet_U
  )}
}


differencedTotFtNoBet- function(i){
  if(i==-1){return(2)}
  
  else if (i==1){return(
    totFtScore~ 
      # pd+  fd+  mfd1+      mfd2+     
      # t1+ t2+   t1Form+ t2Form+   t1Classification+ t2Classification+
      f1d+ f2d+   # f3d+ f4d+
      # t1adoe+      t2adoe+      t1e+         t2e+
      # owd+         odd+         old+         
      doav_ht+     doav_ft+
      dav_htin+    dav_htout+
      dav_ftin+    dav_ftout+
      doav_ht+   doav_ft+
      # dwin+        dwout+       ddin+      ddout+       dlin+        dlout+ 
      # datk+        ddef+        
      datkin+      datkout+
      ddefin+      ddefout
    # bet_1+       bet_X+       bet_2+
    # bet_O+       bet_U
  )}
  
  else if (i==2){return(
    totFtScore~ 
      # pd+  fd+  mfd1+      mfd2+     
      # t1+ t2+   t1Form+ t2Form+   t1Classification+ t2Classification+
      f1d+ f2d+    f3d+ f4d+
      # t1adoe+      t2adoe+      t1e+         t2e+
      # owd+         odd+         old+         
      doav_ht+     doav_ft+
      dav_htin+    dav_htout+
      dav_ftin+    dav_ftout+
      doav_ht+   doav_ft
  )}
  
}
