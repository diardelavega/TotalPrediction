
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
crfvInit <- function(){
  #initializes datasets and call for the crfv accuracy estimation
  
  crfv_TotFtscore_Struct <<- c() # the struct that will keep all the dataStores created
  bestOfSize <-3
  ret <- totFtScorePredFunc(df, ndf, 10,bestOfSize)   # complet dataset crfv
  crfv_TotFtscore_Struct[length(crfv_TotFtscore_Struct)+1:2] <<-ret

  if(max(ndf$week)>20){  # second half dataset crfv
    bestOfSize <-3
    ret <- totFtScorePredFunc(df[which(df$week>max(df$week)/2),], ndf[which(ndf$week>max(ndf$week)/2),], 10,bestOfSize)
    crfv_TotFtscore_Struct[length(crfv_TotFtscore_Struct)+1:2] <<-ret
  }
  
  if(max(ndf$week)>10){  # last 6 weeks dataset crfv
    bestOfSize <-1
    ret <- totFtScorePredFunc(df[which(df$week>max(df$week)-6),], ndf[which(ndf$week>max(ndf$week)-6),], 10,bestOfSize)
    crfv_TotFtscore_Struct[length(crfv_TotFtscore_Struct)+1:2] <<-ret
  }
  
  #------- someway to store the  crfv_dts_Struct
}

totFtScorePredFunc <- function(dataset_f,dataset_d,crfoldNr,bestOfSize){
  # executes the crfv for all the algorithms we provide and stores the best results
  #  since we are handling goals the accuracy value is actually the squared mean error rate of the prediction algorithms
  # we leave the var acc & accuracy for conventon
    
  fds <- AlgoDtf$new()  # to keep the instances of the  full datasets
  dds <- AlgoDtf$new()  # to keep the instances of the  diff datasets
  
  for(algorithm in c(#"C50" ,"J48", 
    "JRip", "OneR","svm","lm","lgm","naiveBayes"
                     #,"randomForest","rpart","bagging", "PART","AdaBoostM1"
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
      ins<- Instance$new(algo = algorithm, attsDtsNr=i, accVal=acc, bet="yes", fullDiff="full")
      accuracy_df[length(accuracy_df)+1] <- ins
    }
    for(i in 1:fulltotFtScoreNoBet(-1)){
      acc <- totFtScoreCrfv(fulltotFtScoreNoBet(i),algorithm,folds_f)
      ins<- Instance$new(algo = algorithm, attsDtsNr=i, accVal=acc, bet="no", fullDiff="full")
      accuracy_df[length(accuracy_df)+1] <- ins
    }
    #--- choose 3 instances with best results
    cur3best <- treBestChoser(accuracy_df,bestOfSize)
    fds$instList[length(fds$instList)+1 :length(cur3best)]  <- cur3best
    
    
    for(i in 1:differencedtotFtScoreBet(-1)){
      acc <- totFtScoreCrfv(differencedtotFtScoreBet(i),algorithm,folds_d)
      ins<- Instance$new(algo = algorithm, attsDtsNr=i, accVal=acc, bet="yes", fullDiff="diff")
      accuracy_ndf[length(accuracy_ndf)+1] <- ins
    }
    for(i in 1:differencedtotFtScoreNoBet(-1)){
      acc <- totFtScoreCrfv(differencedtotFtScoreNoBet(i),algorithm, folds_d)
      ins<- Instance$new(algo = algorithm, attsDtsNr=i, accVal=acc, bet="no", fullDiff="diff")
      accuracy_ndf[length(accuracy_ndf)+1] <- ins
    }
    #--- choose 3 instances with best results
    cur3best <- treBestChoser(accuracy_ndf,bestOfSize)
    dds$instList[length(dds$instList)+1 :length(cur3best)]  <- cur3best
  }# for algorithms
  return (c(fds,dds)) 
}

treBestChoser <- function(lst,bestOfSize){
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
    else if(algorithm=="lm"){tmp.model <- lm(ho , train )}
    else if(algorithm=="lgm"){tmp.model <- lgm(ho , train )}
     
    tmp.predict <- predict(tmp.model, newdata = test)
    
    #--- totFtScore pred  sqrt(1/n * sum[ (pred[i]-val[i])^2 ] )
    # n=length(tmp.predict)
    # pred=mp.predict
    # val =test$totFtScore
    {
        #   s=0;
        # for (j in 1:length(tmp.predict)){
        #   d2 <-( mp.predict[j] - test$totFtScore[j])^2
        #   s=s+d2
        # }
    }
    # smse<- sqrt(s/j)
    
    erre[i]<-sqrt(1/length(tmp.predict) * sum( (tmp.predict-test$totFtScore)^2 ))
    
    
    # tmpAcc=0
    # for (j in 1:length(tmp.predict)){
    #   if(tmp.predict[j]>=2.6 && test$scoreOutcome[j]=="O"){tmpAcc=tmpAcc+1}
    #   else if(tmp.predict[j]<=2.5 && test$scoreOutcome[j]=="U"){tmpAcc=tmpAcc+1}
    # }
    # accuracy[i] <- tmpAcc/length(tmp.predict)
    # ---------------------
  }
  print(sprintf("mean squared error rate with k-fold cross-validation: %.3f percent ", mean(erre)))
  return(mean(erre))
}

#---------------------TotFtscore
# att_datasets are not optimized but taken default from score crfv estimation
fulltotFtScoreBet <- function(i){
  if (i==-1){# return size of atts datasets
    return(5)
  }
  
  if (i==1){
    return( totFtScore~  #totHtScore+ 
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
  
  else if(i==2){return( totFtScore~  #totHtScore+ 
                          t1+               t1Points+         t1Classification+ t1Form+          
                          t1Atack+          t1AtackIn+        t1AtackOut+       t1Defense+        t1DefenseIn+      t1DefenseOut+
                          t1AvgHtScoreIn+   t1AvgHtScoreOut+  t1AvgFtScoreIn+   t1AvgFtScoreOut+  t1AvgHtGgResult+  t1AvgFtGgResult+
                          t1WinsIn+         t1WinsOut+       t1DrawsIn+        t1DrawsOut+       t1LosesIn+        t1LosesOut+
                          t2+               t2Points+         t2Classification+ t2Form+           
                          t2Atack+          t2AtackIn+        t2AtackOut+       t2Defense+        t2DefenseIn+      t2DefenseOut+    
                          t2AvgHtScoreIn+   t2AvgHtScoreOut+  t2AvgFtScoreIn+   t2AvgFtScoreOut+  t2AvgHtGgResult+  t2AvgFtGgResult+ 
                          t2WinsIn+         t2WinsOut+        t2DrawsIn+        t2DrawsOut+       t2LosesIn+        t2LosesOut+      
                          bet_O+            bet_U)}
  
  else if(i==3){return(totFtScore~  #totHtScore+ 
                         t1+               t1Form+           #t1Points+         #t1Classification+
                         t1Atack+          t1Defense+        
                         t1AvgHtScoreIn+   t1AvgFtScoreIn+   t1AvgHtGgResult+  t1AvgFtGgResult+  
                         t1WinsIn+         t1DrawsIn+        t1LosesIn+
                         t2+               t2Form+            #t2Points+         #t2Classification+
                         t2Atack+          t2Defense+        
                         t2AvgHtScoreOut+  t2AvgFtScoreOut+  t2AvgHtGgResult+  t2AvgFtGgResult+ 
                         t2WinsOut+        t2DrawsOut+       t2LosesOut+
                         bet_O+            bet_U)}
  
  else if(i==4){return(totFtScore~  #totHtScore+  #week+  
                         t1+               
                         #t1Points+         t1Classification+ t1Form+          
                         t1Form1Diff+      t1Form2Diff+      t1Form3Diff+      t1Form4Diff+      
                         t1AtackIn+        #t1AtackOut+        
                         t1DefenseIn+      #t1DefenseOut+     
                         t1AvgHtScoreIn+   #t1AvgHtScoreOut+ 
                         t1AvgFtScoreIn+   #t1AvgFtScoreOut+  
                         #t1AvgHtGgResult+  t1AvgFtGgResult+  
                         t1WinsIn+         #t1WinsOut+       
                         t1DrawsIn+        #t1DrawsOut+       
                         t1LosesIn+        #t1LosesOut+       
                         t2+               
                         #t2Points+         t2Classification+ t2Form+           
                         t2Form1Diff+      t2Form2Diff+      t2Form3Diff+      t2Form4Diff+     
                         t2AtackOut+       #t2AtackIn+               
                         t2DefenseOut+     #t2DefenseIn+      
                         t2AvgHtScoreOut+  #t2AvgHtScoreIn+   
                         t2AvgFtScoreOut+  #t2AvgFtScoreIn+   
                         #t2AvgHtGgResult+  t2AvgFtGgResult+ 
                         t2WinsOut+        #t2WinsIn+         
                         t2DrawsOut+       #t2DrawsIn+        
                         t2LosesOut+       #t2LosesIn+        
                         bet_O+            bet_U)}
  
  else if(i==5){return(totFtScore~  #totHtScore+ 
                         #t1+              
                         #t1Points+
                         #t1Classification+ t1Form+          
                         t1Form1Diff+      t1Form2Diff+      #t1Form3Diff+      t1Form4Diff+     
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
}

# 6 - 9 /4
fulltotFtScoreNoBet <- function(i){
  if (i==-1){# return size of atts datasets
    return(4)
  }
  
  if(i==1){return(totFtScore~  #totHtScore+ 
                    t1+               t1Points+         t1Classification+ t1Form+          
                    t1Atack+          t1AtackIn+        t1AtackOut+       t1Defense+        t1DefenseIn+      t1DefenseOut+
                    t1AvgHtScoreIn+   t1AvgHtScoreOut+  t1AvgFtScoreIn+   t1AvgFtScoreOut+  t1AvgHtGgResult+  t1AvgFtGgResult+
                    t1WinsIn+         t1WinsOut+        t1DrawsIn+        t1DrawsOut+       t1LosesIn+        t1LosesOut+
                    t2+               t2Points+         t2Classification+ t2Form+           
                    t2Atack+          t2AtackIn+        t2AtackOut+       t2Defense+        t2DefenseIn+      t2DefenseOut+    
                    t2AvgHtScoreIn+   t2AvgHtScoreOut+  t2AvgFtScoreIn+   t2AvgFtScoreOut+  t2AvgHtGgResult+  t2AvgFtGgResult+ 
                    t2WinsIn+         t2WinsOut+        t2DrawsIn+        t2DrawsOut+       t2LosesIn+        t2LosesOut)}
  
  else if(i==2){return(totFtScore~  #totHtScore+ 
                         t1+               t1Form+          
                         t1Atack+          t1Defense+        
                         t1AvgHtScoreIn+   t1AvgFtScoreIn+   t1AvgFtGgResult+  #t1AvgHtGgResult+  
                         t1WinsIn+         t1DrawsIn+        t1LosesIn+
                         t2+               t2Form+           
                         t2Atack+          t2Defense+        
                         t2AvgHtScoreOut+  t2AvgFtScoreOut+  t2AvgFtGgResult+ #t2AvgHtGgResult+  
                         t2WinsOut+        t2DrawsOut+       t2LosesOut
                       #bet_O+            bet_U
  )}
  
  else if(i==3){return(totFtScore~  #totHtScore+  
                         week+  
                         #t1+               
                         #t1Points+         t1Classification+ 
                         t1Form+          
                         #t1Form1Diff+      t1Form2Diff+      t1Form3Diff+      t1Form4Diff+      
                         t1AtackIn+        #t1AtackOut+        
                         t1DefenseIn+      #t1DefenseOut+     
                         t1AvgHtScoreIn+   #t1AvgHtScoreOut+ 
                         t1AvgFtScoreIn+   #t1AvgFtScoreOut+  
                         #t1AvgHtGgResult+  t1AvgFtGgResult+  
                         t1WinsIn+         #t1WinsOut+       
                         t1DrawsIn+        #t1DrawsOut+       
                         t1LosesIn+        #t1LosesOut+       
                         #t2+               
                         #t2Points+         t2Classification+ 
                         t2Form+           
                         #t2Form1Diff+      t2Form2Diff+      t2Form3Diff+      t2Form4Diff+     
                         t2AtackOut+       #t2AtackIn+               
                         t2DefenseOut+     #t2DefenseIn+      
                         t2AvgHtScoreOut+  #t2AvgHtScoreIn+   
                         t2AvgFtScoreOut+  #t2AvgFtScoreIn+   
                         #t2AvgHtGgResult+  t2AvgFtGgResult+ 
                         t2WinsOut+        #t2WinsIn+         
                         t2DrawsOut+       #t2DrawsIn+        
                         t2LosesOut#+       #t2LosesIn+        
                       #et_O+            bet_U
  )}
  
  else if(i==4){return(totFtScore~  #totHtScore+  
                         t1+              
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
                         t2AvgHtScoreIn+   t2AvgHtScoreOut+  t2AvgFtScoreIn+   t2AvgFtScoreOut+  
                         #t2AvgHtGgResult+  t2AvgFtGgResult+ 
                         #t2WinsIn+         t2WinsOut+        t2DrawsIn+        t2DrawsOut+       t2LosesIn+        t2LosesOut+      
                         #bet_1+            bet_X+            bet_2+       
                         owd+  old+ odd+  mfd1+ mfd2
                       #bet_O+            bet_U
  )}
}

# 1 - 3 nrs /3
differencedtotFtScoreBet <- function(i){
  if (i==-1){# return size of atts datasets
    return(3)
  }
  
  if(i==1){return(totFtScore~  #totHtScore+ 
                    mfd1+      mfd2+    t1Classification+ t2Classification+  pd+  fd+  
                    #f1d+ f2d+ f3d+ f4d+
                    t1adoe+          t2adoe+           t1e+             t2e+
                    owd+         odd+          old+    doav_ht+ doav_ft+
                    dwin+        dwout+       ddin+        ddout+       dlin+        dlout+    
                    datkin+      datkout+     ddefin+      ddefout+   dav_ftin+    dav_ftout+
                    bet_1+       bet_X+       bet_2+ bet_O+       bet_U )}
  
  else if(i==2){return(totFtScore~  #totHtScore+  
                         t1+t2+   t1Form+ t2Form+
                         #mfd1+      mfd2+  mfd+  #
                         #t1Classification+ t2Classification+ 
                         #pd+  fd+  
                         #f1d+ f2d+ f3d+ f4d+
                         t1adoe+          t2adoe+           t1e+             t2e+
                         owd+         odd+          old+    
                         doav_ht+ doav_ft+
                         dwin+        dwout+       ddin+        ddout+       dlin+        dlout+ 
                         #datkin+      datkout+     
                         #ddefin+      ddefout+   
                         dav_ftin+    dav_ftout+
                         bet_O+       bet_U)}
  
  else if(i==3){return(totFtScore~  #totHtScore+ 
                         mfd1+      mfd2+   # t1Classification+ t2Classification+  
                         pd+  fd+  
                         t1adoe+          t2adoe+           t1e+             t2e+
                         owd+         odd+          old+    doav_ht+ doav_ft+
                         dwin+        dwout+       ddin+        ddout+       dlin+        dlout+ datk+    
                         datkin+      datkout+     ddef+        ddefin+      ddefout+   dav_ftin+    dav_ftout+
                         bet_O+       bet_U)}
}

# 4 - 7 nrs /4
differencedtotFtScoreNoBet <- function(i){
  if (i==-1){# return size of atts datasets
    return(4)
  } 
  
  if(i==1){return( totFtScore~  #totHtScore+  
                     t1+t2+  
                     #mfd1+      mfd2+    #
                     #t1Classification+ t2Classification+ 
                     pd+  fd+  
                     #f1d+ f2d+ f3d+ f4d+
                     t1adoe+          t2adoe+           
                     t1e+             t2e+
                     owd+         odd+          old+ 
                     
                     doav_ht+     doav_ft+
                     dwin+        dwout+       ddin+        ddout+       dlin+        dlout+ 
                     datk+        ddef+        
                     datkin+      datkout+
                     ddefin+       ddefout+   
                     dav_ftin+    dav_ftout
                   #bet_O+       bet_U
                   
  )}
  
  # not so good results
  else if(i==2){return(totFtScore~  #totHtScore+  #t1+t2+  
                         mfd1+      mfd2+    #
                         #t1Classification+ t2Classification+ 
                         #pd+  fd+  
                         #f1d+ f2d+ f3d+ f4d+
                         t1adoe+          t2adoe+           t1e+             t2e+
                         owd+         odd+          old+    
                         doav_ht+ doav_ft+
                         dwin+        dwout+       ddin+        ddout+       dlin+        dlout+ 
                         
                         datkin+      datkout+     
                         ddefin+      ddefout+   
                         dav_ftin+    dav_ftout
                       #bet_O+       bet_U
  )}
  
  #midle algo -> higher accuracy
  else if(i==3){return(totFtScore~  #totHtScore+ 
                         t1+t2+  
                         #mfd1+      mfd2+  
                         mfd+  #
                         #t1Classification+ t2Classification+ 
                         #pd+  fd+  
                         #f1d+ f2d+ f3d+ f4d+
                         t1adoe+          t2adoe+           
                         #t1e+             t2e+
                         owd+         odd+          old+    
                         doav_ht+ doav_ft+
                         dwin+        dwout+       ddin+        ddout+       dlin+        dlout+ 
                         datkin+      datkout+     
                         ddefin+      ddefout+   
                         dav_ftin+    dav_ftout
                       #bet_O+       bet_U
  )}
  
  else if(i==4){return(totFtScore~  #totHtScore+  
                         #t1+t2+  
                         mfd1+      mfd2+  #mfd+  #
                         #t1Classification+ t2Classification+ 
                         #pd+  fd+  
                         #f1d+ f2d+ f3d+ f4d+
                         t1adoe+          t2adoe+           t1e+             t2e+
                         owd+         odd+          old+    
                         doav_ht+ doav_ft+
                         dwin+        dwout+       ddin+        ddout+       dlin+        dlout+ 
                         
                         #datkin+      datkout+     
                         #ddefin+      ddefout+   
                         dav_ftin+    dav_ftout
                       #bet_O+       bet_U
  )}
}
