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
rm(p1CrfvInit,p1PredFunc,p1TreBestChoser,p1Crfv)
rm(full1pBet,full1pNoBet,differenced1pBet,differenced1pNoBet)


# supose we have df & ndf datasets
p1CrfvInit <- function(){
  #initializes datasets and call for the crfv accuracy estimation
  folds <- 10;
  p1Dtf <<- Clean1pDtf$new(predAtt="p1")
  
  crfv_p1_Struct <<- c() # the struct that will keep all the dataStores created
  bestOfSize <-3
  ret <- p1PredFunc("f" ,folds,bestOfSize)   # complet dataset crfv
  # crfv_p1_Struct[length(crfv_p1_Struct)+1:2] <<-ret
  p1Dtf$algoDataList[length(p1Dtf$algoDataList)+1:2]<<-ret
  
  if(max(ndf$week)>20){  # second half dataset crfv
    bestOfSize <-3
    ret <- p1PredFunc("f2",folds,bestOfSize)
    # crfv_p1_Struct[length(crfv_p1_Struct)+1:2] <<-ret
    p1Dtf$algoDataList[length(p1Dtf$algoDataList)+1:2]<<-ret
  }
  
  if(max(ndf$week)>10){  # last 6 weeks dataset crfv
    bestOfSize <-1
    ret <- p1PredFunc("f5",folds,bestOfSize)
    # crfv_p1_Struct[length(crfv_p1_Struct)+1:2] <<-ret
    p1Dtf$algoDataList[length(p1Dtf$algoDataList)+1:2]<<-ret
  }
  
  #------- someway to store the  crfv_dts_Struct
}

p1PredFunc <- function(dataframeCategory,crfoldNr,bestOfSize){
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
    
    for(i in 1:full1pBet(-1)){
      acc <- p1Crfv(full1pBet(i),algorithm,folds_f)
      ins<- Instance$new(algo = algorithm, attsDtsNr=i, accVal=acc, bet="yes", fullDiff="full", dfCategory=dataframeCategory,ptype="categoric")
      accuracy_df[length(accuracy_df)+1] <- ins
    }
    for(i in 1:full1pNoBet(-1)){
      acc <- p1Crfv(full1pNoBet(i),algorithm,folds_f)
      ins<- Instance$new(algo = algorithm, attsDtsNr=i, accVal=acc, bet="no", fullDiff="full", dfCategory=dataframeCategory,ptype="categoric")
      accuracy_df[length(accuracy_df)+1] <- ins
    }
    #--- choose 3 instances with best results
    cur3best <- treBestChoser(accuracy_df,bestOfSize)
    fds$instList[length(fds$instList)+1 :length(cur3best)]  <- cur3best
    
    
    for(i in 1:differenced1pBet(-1)){
      acc <- p1Crfv(differenced1pBet(i),algorithm,folds_d)
      ins<- Instance$new(algo = algorithm, attsDtsNr=i, accVal=acc, bet="yes", fullDiff="diff", dfCategory=dataframeCategory,ptype="categoric")
      accuracy_ndf[length(accuracy_ndf)+1] <- ins
    }
    for(i in 1:differenced1pNoBet(-1)){
      acc <- p1Crfv(differenced1pNoBet(i),algorithm, folds_d)
      ins<- Instance$new(algo = algorithm, attsDtsNr=i, accVal=acc, bet="no", fullDiff="diff", dfCategory=dataframeCategory,ptype="categoric")
      accuracy_ndf[length(accuracy_ndf)+1] <- ins
    }
    #--- choose 3 instances with best results
    cur3best <- treBestChoser(accuracy_ndf,bestOfSize)
    dds$instList[length(dds$instList)+1 :length(cur3best)]  <- cur3best
  }# for algorithms
  return (c(fds,dds)) 
}

p1TreBestChoser <- function(lst,bestOfSize){
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
    if(bv[[2]]$accVal < bv[[3]]$accVal) {# last sorting
      if(bv[[1]]$accVal < bv[[3]]$accVal) { 
        tmpInstance <-bv[[3]];  bv[[3]] <- bv[[2]];  bv[[2]] <- bv[[1]]; bv[[1]] <- tmpInstance }
      else {tmpInstance <-bv[[3]];  bv[[3]] <- bv[[2]]; bv[[2]] <- tmpInstance}
    }
  }
  for(k in 1:bestOfSize){
    print(bv[[k]]$accVal)  
  }
  
  return (bv[1:bestOfSize])
}

p1Crfv <-function(ho,algorithm,folds){
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
    
    conf.mat <- table(test$ht1pOutcome, tmp.predict)
    #errs[i] <- 1-sum(diag(conf.mat))/sum(conf.mat)
    accuracy[i] <- sum(diag(conf.mat)) / sum(conf.mat)
  }
  #gen_accuracy[length(gen_accuracy)+1]<<-100*mean(accuracy)
  #print(sprintf("average error using k-fold cross-validation: %.3f percent", 100*mean(errs)))
  print(sprintf("average accuracy using k-fold cross-validation: %.3f percent ", 100*mean(accuracy)))
  return(100*mean(accuracy))
}

#-----------
full1pBet <- function(i){
  if(i==-1){return(6)}
  
  if(i==1){
    return(ht1pOutcome~ 
             t1+               t1Points+         t1Classification+ t1Form+          
             t1Atack+          t1AtackIn+        t1AtackOut+       t1Defense+        t1DefenseIn+      t1DefenseOut+
             t1AvgHtScoreIn+   t1AvgHtScoreOut+  t1AvgFtScoreIn+   t1AvgFtScoreOut+  t1AvgHtGgResult+  t1AvgFtGgResult+  
             t1WinsIn+         t1WinsOut+        t1DrawsIn+        t1DrawsOut+       t1LosesIn+        t1LosesOut+
             t2+               t2Points+         t2Classification+ t2Form+           
             t2Atack+          t2AtackIn+        t2AtackOut+       t2Defense+        t2DefenseIn+      t2DefenseOut+    
             t2AvgHtScoreIn+   t2AvgHtScoreOut+  t2AvgFtScoreIn+   t2AvgFtScoreOut+  t2AvgHtGgResult+  t2AvgFtGgResult+ 
             t2WinsIn+         t2WinsOut+        t2DrawsIn+        t2DrawsOut+       t2LosesIn+        t2LosesOut+      
             bet_1+            bet_X+            bet_2+            bet_O+            bet_U)
  }
  
  if(i==2){
    return(ht1pOutcome~ 
             t1+               t1Points+         t1Classification+ t1Form+          
             t1Atack+          t1AtackIn+        t1AtackOut+       t1Defense+        t1DefenseIn+      t1DefenseOut+
             t1AvgHtScoreIn+   t1AvgHtScoreOut+  t1AvgFtScoreIn+   t1AvgFtScoreOut+  t1AvgHtGgResult+  t1AvgFtGgResult+  
             t1WinsIn+         t1WinsOut+        t1DrawsIn+        t1DrawsOut+       t1LosesIn+        t1LosesOut+
             t2+               t2Points+         t2Classification+ t2Form+           
             t2Atack+          t2AtackIn+        t2AtackOut+       t2Defense+        t2DefenseIn+      t2DefenseOut+    
             t2AvgHtScoreIn+   t2AvgHtScoreOut+  t2AvgFtScoreIn+   t2AvgFtScoreOut+  t2AvgHtGgResult+  t2AvgFtGgResult+ 
             t2WinsIn+         t2WinsOut+        t2DrawsIn+        t2DrawsOut+       t2LosesIn+        t2LosesOut+
             bet_1+            bet_X+            bet_U)
  }
  
  if(i==3){return(ht1pOutcome~ 
                    t1+               #t1Points+         t1Classification+ t1Form+          
                    t1Atack+          #t1AtackIn+        t1AtackOut+       
                    t1Defense+        #t1DefenseIn+      t1DefenseOut+
                    t1AvgHtScoreIn+   t1AvgHtScoreOut+  
                    t1AvgFtScoreIn+   t1AvgFtScoreOut+  
                    t1AvgHtGgResult+  t1AvgFtGgResult+  
                    t1WinsIn+         t1WinsOut+        t1DrawsIn+        t1DrawsOut+       t1LosesIn+        t1LosesOut+
                    t2+               #t2Points+         t2Classification+ t2Form+           
                    t2Atack+          #t2AtackIn+        t2AtackOut+       
                    t2Defense+        #t2DefenseIn+      t2DefenseOut+    
                    t2AvgHtScoreIn+   t2AvgHtScoreOut+  
                    t2AvgFtScoreIn+   t2AvgFtScoreOut+  
                    t2AvgHtGgResult+  t2AvgFtGgResult+ 
                    t2WinsIn+         t2WinsOut+        t2DrawsIn+        t2DrawsOut+       t2LosesIn+        t2LosesOut+
                    bet_1+            bet_X+            bet_2)}
  
  if(i==4){return(ht1pOutcome~ t1Form1Diff+      t1Form2Diff+      #t1Form3Diff+      t1Form4Diff+     
                    #t1Atack+          t1Defense+        
                    t1AtackIn+        t1AtackOut+       t1DefenseIn+      t1DefenseOut+     
                    #t1AvgHtScoreIn+   t1AvgHtScoreOut+ 
                    t1AvgFtScoreIn+   t1AvgFtScoreOut+  
                    #t1AvgHtGgResult+  t1AvgFtGgResult+  
                    #t1WinsIn+         t1WinsOut+        t1DrawsIn+        t1DrawsOut+       t1LosesIn+        t1LosesOut+     
                    #t2+
                    #t2Points+       
                    #t2Classification+ t2Form+ 
                    t2Form1Diff+      t2Form2Diff+      #t2Form3Diff+      t2Form4Diff+     
                    #t2Atack+          t2Defense+        
                    t2AtackIn+        t2AtackOut+       t2DefenseIn+      t2DefenseOut+    
                    #t2AvgHtScoreIn+   t2AvgHtScoreOut+  
                    t2AvgFtScoreIn+   t2AvgFtScoreOut+
                    #t2AvgHtGgResult+  t2AvgFtGgResult+ 
                    #t2WinsIn+         t2WinsOut+        t2DrawsIn+        t2DrawsOut+       t2LosesIn+        t2LosesOut+      
                    #bet_1+            bet_X+            bet_2+       
                    owd+  old+ odd+  #mfd1+ mfd2
                    bet_O+            bet_U)}
  
  if(i==5){# good distribution of good results
    return(ht1pOutcome~ #week+ #t1+
             #t1Form+
             t1Form1Diff+      t1Form2Diff+      #t1Form3Diff+      t1Form4Diff+     
             #t1Atack+          t1Defense+        
             t1AtackIn+        #t1AtackOut+       
             t1DefenseIn+      #t1DefenseOut+     
             t1AvgHtScoreIn+   #t1AvgHtScoreOut+ 
             #t1AvgFtScoreIn+   t1AvgFtScoreOut+  
             t1AvgHtGgResult+  #t1AvgFtGgResult+  
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
             #t2AvgFtScoreIn+   t2AvgFtScoreOut+
             t2AvgHtGgResult+  #t2AvgFtGgResult+ 
             #t2WinsIn+         t2WinsOut+        t2DrawsIn+        t2DrawsOut+       t2LosesIn+        t2LosesOut+      
             bet_1+            bet_X+            bet_2+       
             owd+  old+ odd#+  mfd1+ mfd2
           #bet_O+            bet_U
    )}
  
  if(i==6){# partially veri high results
    return(ht1pOutcome~ #week+ #t1+
             #t1Form+
             t1Form1Diff+      t1Form2Diff+      #t1Form3Diff+      t1Form4Diff+     
             #t1Atack+          t1Defense+        
             t1AtackIn+        #t1AtackOut+       
             t1DefenseIn+      #t1DefenseOut+     
             t1AvgHtScoreIn+   #t1AvgHtScoreOut+ 
             #t1AvgFtScoreIn+   t1AvgFtScoreOut+  
             t1AvgHtGgResult+  #t1AvgFtGgResult+  
             t1WinsIn+         #t1WinsOut+        
             t1DrawsIn+        #t1DrawsOut+       
             t1LosesIn+        #t1LosesOut+     
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
             #t2AvgFtScoreIn+   t2AvgFtScoreOut+
             t2AvgHtGgResult+  #t2AvgFtGgResult+ 
             #t2WinsOut+        t2DrawsOut+       t2LosesOut+      
             t2WinsIn+         t2DrawsIn+         t2LosesIn+        
             #bet_1+            bet_X+            bet_2+       
             owd+  old+ odd+  mfd1+ mfd2+
             bet_O+            bet_U)}
}

full1pNoBet <- function(i){
  if (i==-1){return(5)}
  
  if(i==1){return(ht1pOutcome~ 
                    t1+               t1Points+         t1Classification+ t1Form+          
                    t1Atack+          t1AtackIn+        t1AtackOut+       t1Defense+        t1DefenseIn+      t1DefenseOut+
                    t1AvgHtScoreIn+   t1AvgHtScoreOut+  t1AvgFtScoreIn+   t1AvgFtScoreOut+  t1AvgHtGgResult+  t1AvgFtGgResult+  
                    t1WinsIn+         t1WinsOut+        t1DrawsIn+        t1DrawsOut+       t1LosesIn+        t1LosesOut+
                    t2+               t2Points+         t2Classification+ t2Form+           
                    t2Atack+          t2AtackIn+        t2AtackOut+       t2Defense+        t2DefenseIn+      t2DefenseOut+    
                    t2AvgHtScoreIn+   t2AvgHtScoreOut+  t2AvgFtScoreIn+   t2AvgFtScoreOut+  t2AvgHtGgResult+  t2AvgFtGgResult+ 
                    t2WinsIn+         t2WinsOut+        t2DrawsIn+        t2DrawsOut+       t2LosesIn+        t2LosesOut
                  # bet_1+            bet_X+            bet_2+            bet_O+            bet_U
  )}
  
  if(i==2){return(ht1pOutcome~ 
                    t1+               #t1Points+         t1Classification+ t1Form+          
                    t1Atack+          #t1AtackIn+        t1AtackOut+       
                    t1Defense+        #t1DefenseIn+      t1DefenseOut+
                    t1AvgHtScoreIn+   t1AvgHtScoreOut+  
                    t1AvgFtScoreIn+   t1AvgFtScoreOut+  
                    t1AvgHtGgResult+  t1AvgFtGgResult+  
                    t1WinsIn+         t1WinsOut+        t1DrawsIn+        t1DrawsOut+       t1LosesIn+        t1LosesOut+
                    t2+               #t2Points+         t2Classification+ t2Form+           
                    t2Atack+          #t2AtackIn+        t2AtackOut+       
                    t2Defense+        #t2DefenseIn+      t2DefenseOut+    
                    t2AvgHtScoreIn+   t2AvgHtScoreOut+  
                    t2AvgFtScoreIn+   t2AvgFtScoreOut+  
                    t2AvgHtGgResult+  t2AvgFtGgResult+ 
                    t2WinsIn+         t2WinsOut+        t2DrawsIn+        t2DrawsOut+       t2LosesIn+        t2LosesOut)}
  
  if(i==3){# good distributed results (to the previously not good att_dts)
    return(ht1pOutcome~ 
             #t1+               #t1Points+         t1Classification+ 
             t1Form+          
             t1Atack+          #t1AtackIn+        t1AtackOut+       
             t1Defense+        #t1DefenseIn+      t1DefenseOut+
             t1AvgHtScoreIn+   t1AvgHtScoreOut+  
             #t1AvgFtScoreIn+   t1AvgFtScoreOut+  
             t1AvgHtGgResult+  #t1AvgFtGgResult+  
             t1WinsIn+         t1WinsOut+        t1DrawsIn+        t1DrawsOut+       t1LosesIn+        t1LosesOut+
             #t2+               #t2Points+         t2Classification+ 
             t2Form+           
             t2Atack+          #t2AtackIn+        t2AtackOut+       
             t2Defense+        #t2DefenseIn+      t2DefenseOut+    
             t2AvgHtScoreIn+   t2AvgHtScoreOut+  
             #t2AvgFtScoreIn+   t2AvgFtScoreOut  +
             t2AvgHtGgResult+  #t2AvgFtGgResult 
             t2WinsIn+         t2WinsOut+        t2DrawsIn+        t2DrawsOut+       t2LosesIn+        t2LosesOut
           #bet_1+            bet_X+            bet_2
    )}
  
  if(i==4){# diversification of good results
    return(ht1pOutcome~ 
             #t1+               t1Form+           #t1Points+         #t1Classification+
             t1Atack+          t1Defense+        
             t1AvgHtScoreIn+   t1AvgFtScoreIn+   t1AvgHtGgResult+  t1AvgFtGgResult+  
             t1WinsIn+         t1DrawsIn+        t1LosesIn+
             #t2+               t2Form+            #t2Points+         #t2Classification+
             t2Atack+          t2Defense+        
             t2AvgHtScoreOut+  t2AvgFtScoreOut+  t2AvgHtGgResult+  t2AvgFtGgResult+ 
             t2WinsOut+        t2DrawsOut+       t2LosesOut
           #bet_O+            bet_U
    )}
  
  if(i==5){return(
    ht1pOutcome~ 
      t1Form1Diff+      t1Form2Diff+      #t1Form3Diff+      t1Form4Diff+     
      #t1Atack+          t1Defense+        
      t1AtackIn+        t1AtackOut+       t1DefenseIn+      t1DefenseOut+     
      t1AvgHtScoreIn+   t1AvgHtScoreOut+ 
      t1AvgFtScoreIn+   t1AvgFtScoreOut+  
      #t1AvgHtGgResult+  t1AvgFtGgResult+  
      #t1WinsIn+         t1WinsOut+        t1DrawsIn+        t1DrawsOut+       t1LosesIn+        t1LosesOut+     
      #t2+
      #t2Points+       
      #t2Classification+ t2Form+ 
      t2Form1Diff+      t2Form2Diff+      #t2Form3Diff+      t2Form4Diff+     
      #t2Atack+          t2Defense+        
      t2AtackIn+        t2AtackOut+       t2DefenseIn+      t2DefenseOut+    
      t2AvgHtScoreIn+   t2AvgHtScoreOut+  
      t2AvgFtScoreIn+   t2AvgFtScoreOut+
      #t2AvgHtGgResult+  t2AvgFtGgResult+ 
      #t2WinsIn+         t2WinsOut+        t2DrawsIn+        t2DrawsOut+       t2LosesIn+        t2LosesOut+      
      #bet_1+            bet_X+            bet_2+       
      owd+  old+ odd+  mfd1+ mfd2
    #bet_O+            bet_U
    
  )}
}

differenciated1pBet <- function(i){
  if (i==-1){return(6)}
  
  if(i==1){return(ht1pOutcome~   mfd1+      mfd2+     pd+  fd+  
                    #t1+ t2+   t1Form+ t2Form+   t1Classification+ t2Classification+   
                    #f1d+ f2d+    f3d+ f4d+ 
                    #t1adoe+      t2adoe+      t1e+         t2e+
                    owd+         odd+         old+         
                    doav_ht+     #doav_ft+
                    dav_htin+    dav_htout+
                    #dav_ftin+    dav_ftout+   
                    doav_ht+   #doav_ft
                    #dwin+        dwout+       ddin+      ddout+       dlin+        dlout+ datk+    
                    #datk+        #datkin+      datkout+     
                    #ddef+        #ddefin+      ddefout  
                    #bet_1+       bet_X+       bet_2
                    bet_O+       bet_U)}
  if(i==2){return(ht1pOutcome~  mfd1+      mfd2+     pd+  fd+  
                    t1+ t2+   #t1Form+ t2Form+   t1Classification+ t2Classification+   
                    #f1d+ f2d+    f3d+ f4d+ 
                    # t1adoe+      t2adoe+       t1e+         t2e+
                    owd+         odd+         old+         
                    doav_ht+     #doav_ft+
                    dav_htin+    dav_htout+
                    dav_ftin+    dav_ftout+
                    doav_ht+   #doav_ft
                    dwin+        dwout+       ddin+      ddout+       dlin+        dlout+
                    datk+        #
                    #datkin+      datkout+     
                    ddef+        #
                    #ddefin+      ddefout+
                    bet_1+       bet_X+       bet_2+
                    bet_O+       bet_U)}
  if(i==3){return(ht1pOutcome~   mfd1+      mfd2+     pd+  fd+  
                    t1+ t2+   t1Form+ t2Form+   t1Classification+ t2Classification+   
                    #f1d+ f2d+    f3d+ f4d+ 
                    #t1adoe+      t2adoe+      t1e+         t2e+
                    owd+         odd+         old+         
                    doav_ht+     doav_ft+
                    dav_htin+    dav_htout+
                    dav_ftin+    dav_ftout+   doav_ht+   doav_ft+
                    #dwin+        dwout+       ddin+      ddout+       dlin+        dlout+ datk+    
                    #datk+        #datkin+      datkout+     
                    #ddef+        #ddefin+      ddefout  
                    bet_1+       bet_X+       bet_2
                  #bet_O+       bet_U
                  
  )}
  if(i==4){return(ht1pOutcome~   mfd1+      mfd2+     pd+  fd+  
                    #t1+ t2+   t1Form+ t2Form+   t1Classification+ t2Classification+   
                    #f1d+ f2d+    f3d+ f4d+ 
                    #t1adoe+      t2adoe+      t1e+         t2e+
                    #owd+         odd+         old+         
                    doav_ht+     doav_ft+
                    dav_htin+    dav_htout+
                    dav_ftin+    dav_ftout+   doav_ht+   doav_ft+
                    #dwin+        dwout+       ddin+      ddout+       dlin+        dlout+ datk+    
                    #datk+        #datkin+      datkout+     
                    #ddef+        #ddefin+      ddefout  
                    #bet_1+       bet_X+       bet_2#+ 
                    bet_O+       bet_U)}
  if(i==5){return(ht1pOutcome~   mfd1+      mfd2+     pd+  fd+  
                    #t1+ t2+   t1Form+ t2Form+   t1Classification+ t2Classification+   
                    #f1d+ f2d+    f3d+ f4d+ 
                    t1adoe+      t2adoe+      t1e+         t2e+
                    #owd+         odd+         old+         
                    doav_ht+     doav_ft+
                    dav_htin+    dav_htout+
                    dav_ftin+    dav_ftout+   doav_ht+   doav_ft+
                    #dwin+        dwout+       ddin+      ddout+       dlin+        dlout+ datk+    
                    datk+        #datkin+      datkout+     
                    ddef+        #ddefin+      ddefout  
                    bet_1+       bet_X+       bet_2#+ bet_O+       bet_U
  )}
  if(i==6){return(ht1pOutcome~  t1+ t2+ mfd1+      mfd2+    t1Classification+ t2Classification+  pd+  fd+  
                    f1d+ f2d+ f3d+ f4d+ t1Form+ t2Form+
                    t1adoe+          t2adoe+           t1e+             t2e+
                    owd+         odd+          old+    doav_ht+ doav_ft+
                    dwin+        dwout+       ddin+        ddout+       dlin+        dlout+ datk+    
                    datk+        datkin+      datkout+     
                    ddef+        ddefin+      ddefout+  
                    dav_htin+    dav_htout+
                    dav_ftin+    dav_ftout+   doav_ht+   doav_ft+
                    bet_1+       bet_X+       bet_2+ bet_O+       bet_U)}
  
}

differenciated1pNoBet <- function(i){
  if (i==-1){return(6)}
  
  if(i==1){# partially high acc for the lower ones
    return(ht1pOutcome~  #mfd1+      mfd2+     pd+  fd+  
             #t1+ t2+   #t1Form+ t2Form+   t1Classification+ t2Classification+   
             f1d+ f2d+    #f3d+ f4d+ 
             #t1adoe+      t2adoe+       
             #t1e+         t2e+
             owd+         odd+         old+         
             doav_ht+     #doav_ft+
             dav_htin+    dav_htout+
             #dav_ftin+    dav_ftout+
             doav_ht+   #doav_ft
             dwin+        dwout+       ddin+      ddout+       dlin+        dlout+
             #datk+        #
             datkin+      datkout+     
             #ddef        #
             ddefin+      ddefout
    )}
  
  if(i==2){
    return(ht1pOutcome~  #mfd1+      mfd2+     pd+  fd+  
             #t1+ t2+   #t1Form+ t2Form+   t1Classification+ t2Classification+   
             #f1d+ f2d+    f3d+ f4d+ 
             t1adoe+      t2adoe+       
             #t1e+         t2e+
             owd+         odd+         old+         
             doav_ht+     #doav_ft+
             dav_htin+    dav_htout+
             #dav_ftin+    dav_ftout+
             doav_ht+   #doav_ft
             dwin+        dwout+       ddin+      ddout+       dlin+        dlout
           #datk+        #
           #datkin+      datkout+     
           #ddef        #
           #ddefin+      ddefout+
           #bet_1+       bet_X+       bet_2
           #bet_O+       bet_U
    )}
  
  if(i==3){#generaly very high results
    return(h10 <- ht1pOutcome~  mfd1+      mfd2+     pd+  fd+  
             t1+ t2+   #t1Form+ t2Form+   t1Classification+ t2Classification+   
             #f1d+ f2d+    f3d+ f4d+ 
             # t1adoe+      t2adoe+       t1e+         t2e+
             owd+         odd+         old+         
             doav_ht+     #doav_ft+
             dav_htin+    dav_htout+
             dav_ftin+    dav_ftout+
             doav_ht+   #doav_ft
             dwin+        dwout+       ddin+      ddout+       dlin+        dlout+
             datk+        #datkin+      datkout+     
             ddef        #ddefin+      ddefout  
           #bet_1+       bet_X+       bet_2+
           #bet_O+       bet_U
    )}
  
  if(i==4){#partialy high results
    return(ht1pOutcome~  mfd1+      mfd2+     pd+  fd+  
             t1+ t2+   #t1Form+ t2Form+   t1Classification+ t2Classification+   
             #f1d+ f2d+    f3d+ f4d+ 
             #t1adoe+      t2adoe+      
             #t1e+         t2e+
             owd+         odd+         old+         
             doav_ht+     #doav_ft+
             dav_htin+    dav_htout+
             #dav_ftin+    dav_ftout+   
             doav_ht+   #doav_ft
             dwin+        dwout+       ddin+      ddout+       dlin+        dlout
           #datk+        #datkin+      datkout+     
           #ddef+        #ddefin+      ddefout  
           #bet_1+       bet_X+       bet_2
           #bet_O+       bet_U
    )}
  
  if(i==5){# algorithms with bad results generaly have good results
    return(
      ht1pOutcome~   mfd1+      mfd2+     pd+  fd+  
        #t1+ t2+   t1Form+ t2Form+   t1Classification+ t2Classification+   
        #f1d+ f2d+    f3d+ f4d+ 
        #t1adoe+      t2adoe+      t1e+         t2e+
        owd+         odd+         old+         
        doav_ht+     doav_ft+
        dav_htin+    dav_htout+
        dav_ftin+    dav_ftout+   doav_ht+   doav_ft
      #dwin+        dwout+       ddin+      ddout+       dlin+        dlout+ datk+    
      #datk+        #datkin+      datkout+     
      #ddef+        #ddefin+      ddefout  
      #bet_1+       bet_X+       bet_2+
      #bet_O+       bet_U
    )}
  
  if(i==6){
    return(
      ht1pOutcome~   mfd1+      mfd2+     pd+  fd+  
        #t1+ t2+   t1Form+ t2Form+   t1Classification+ t2Classification+   
        #f1d+ f2d+    f3d+ f4d+ 
        t1adoe+      t2adoe+      t1e+         t2e+
        owd+         odd+         old+         doav_ht+     doav_ft+
        dwin+        dwout+       ddin+        ddout+       dlin+        dlout+ datk+    
        datk+        datkin+      datkout+     
        ddef+        ddefin+      ddefout+  
        dav_htin+    dav_htout+
        dav_ftin+    dav_ftout+   doav_ht+   doav_ft
    )}
}

