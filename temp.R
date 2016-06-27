#libraries required
{
  library(plyr)
  library(e1071)  #svm
  library(C50)
  library(randomForest)
  library(ipred)
  library(RWeka)
  library(rpart)
  library(tree)
  #bestOfSize <- 3  
}

# "glm","svm","lm","bagging","Bagging" 
# "C50","J48","svm","naiveBayes","randomForest","rpart","bagging", "PART","JRip","AdaBoostM1", "OneR"

folds <- split(ndfn, cut(sample(1:nrow(ndf),replace = FALSE),10))
folds <- split(dfn, cut(sample(1:nrow(dfn),replace = FALSE),10))
folds <- split(df2, cut(sample(1:nrow(df2)),10))
folds <- split(df5, cut(sample(1:nrow(df5)),10))
accuracy<- rep(NA, length(folds))



gen_accuracy <- c();
for(algorithm in c("OneR","AdaBoostM1", "C50","J48","bagging"
  ,"naiveBayes","rpart", "PART","randomForest","svm", "JRip"
                   )){
  print(algorithm); set.seed(1234);
  gen_accuracy[length(gen_accuracy)+1]<- headCrfv(differencedheadBet(1),algorithm,folds)  }
mean(gen_accuracy)

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
    
    #--- totFtScore pred
    # derivedPred <-c()
    # tmpAcc=0
    # for (j in 1:length(tmp.predict)){
    #   if(tmp.predict[j]>=3 && test$scoreOutcome[j]=="O"){tmpAcc=tmpAcc+1}
    #   else if(tmp.predict[j]<=2 && test$scoreOutcome[j]=="U"){tmpAcc=tmpAcc+1}
    # }
    # accuracy[i] <- tmpAcc/length(tmp.predict)
    # ---------------------
    
    conf.mat <- table(test$headOutcome, tmp.predict)
    #errs[i] <- 1-sum(diag(conf.mat))/sum(conf.mat)
    accuracy[i] <- sum(diag(conf.mat)) / sum(conf.mat)
  }
  #gen_accuracy[length(gen_accuracy)+1]<<-100*mean(accuracy)
  #print(sprintf("average error using k-fold cross-validation: %.3f percent", 100*mean(errs)))
  print(sprintf("average accuracy using k-fold cross-validation: %.3f percent ", 100*mean(accuracy)))
  return(100*mean(accuracy))
}

f1 <- headOutcome~ mfd1+ mfd2+  odd+  old+  owd+
  t1+               t1Points+         t1Classification+ t1Form+          
  t1Atack+          t1AtackIn+        t1AtackOut+       t1Defense+        t1DefenseIn+      t1DefenseOut+
  t1AvgHtScoreIn+   t1AvgHtScoreOut+  t1AvgFtScoreIn+   t1AvgFtScoreOut+  t1AvgHtGgResult+  t1AvgFtGgResult+  
  t1WinsIn+         t1WinsOut+        t1DrawsIn+        t1DrawsOut+       t1LosesIn+        t1LosesOut+
  t2+               t2Points+         t2Classification+ t2Form+           
  t2Atack+          t2AtackIn+        t2AtackOut+       t2Defense+        t2DefenseIn+      t2DefenseOut+    
  t2AvgHtScoreIn+   t2AvgHtScoreOut+  t2AvgFtScoreIn+   t2AvgFtScoreOut+  t2AvgHtGgResult+  t2AvgFtGgResult+ 
  t2WinsIn+         t2WinsOut+        t2DrawsIn+        t2DrawsOut+       t2LosesIn+        t2LosesOut+      
  bet_1+            bet_X+            bet_2+            bet_O+            bet_U



  
#--------------------------------------------------------------------------
fullheadBet <- function(i){
  if (i==-1){# return size of atts datasets
    return(5)
  }
  
  if (i==1){
    return( headOutcome~ 
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
  
  else if(i==2){return( headOutcome~ 
                          t1+               t1Points+         t1Classification+ t1Form+          
                          t1Atack+          t1AtackIn+        t1AtackOut+       t1Defense+        t1DefenseIn+      t1DefenseOut+
                          t1AvgHtScoreIn+   t1AvgHtScoreOut+  t1AvgFtScoreIn+   t1AvgFtScoreOut+  t1AvgHtGgResult+  t1AvgFtGgResult+
                          t1WinsIn+         t1WinsOut+       t1DrawsIn+        t1DrawsOut+       t1LosesIn+        t1LosesOut+
                          t2+               t2Points+         t2Classification+ t2Form+           
                          t2Atack+          t2AtackIn+        t2AtackOut+       t2Defense+        t2DefenseIn+      t2DefenseOut+    
                          t2AvgHtScoreIn+   t2AvgHtScoreOut+  t2AvgFtScoreIn+   t2AvgFtScoreOut+  t2AvgHtGgResult+  t2AvgFtGgResult+ 
                          t2WinsIn+         t2WinsOut+        t2DrawsIn+        t2DrawsOut+       t2LosesIn+        t2LosesOut+      
                          bet_O+            bet_U)}
  
  else if(i==3){return(headOutcome~ 
                         t1+               t1Form+           #t1Points+         #t1Classification+
                         t1Atack+          t1Defense+        
                         t1AvgHtScoreIn+   t1AvgFtScoreIn+   t1AvgHtGgResult+  t1AvgFtGgResult+  
                         t1WinsIn+         t1DrawsIn+        t1LosesIn+
                         t2+               t2Form+            #t2Points+         #t2Classification+
                         t2Atack+          t2Defense+        
                         t2AvgHtScoreOut+  t2AvgFtScoreOut+  t2AvgHtGgResult+  t2AvgFtGgResult+ 
                         t2WinsOut+        t2DrawsOut+       t2LosesOut+
                         bet_O+            bet_U)}
  
  else if(i==4){return(headOutcome~#week+  
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
  
  else if(i==5){return(headOutcome~  
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
fullheadNoBet <- function(i){
  if (i==-1){# return size of atts datasets
    return(4)
  }
  
  if(i==1){return(headOutcome~ 
                    t1+               t1Points+         t1Classification+ t1Form+          
                    t1Atack+          t1AtackIn+        t1AtackOut+       t1Defense+        t1DefenseIn+      t1DefenseOut+
                    t1AvgHtScoreIn+   t1AvgHtScoreOut+  t1AvgFtScoreIn+   t1AvgFtScoreOut+  t1AvgHtGgResult+  t1AvgFtGgResult+
                    t1WinsIn+         t1WinsOut+        t1DrawsIn+        t1DrawsOut+       t1LosesIn+        t1LosesOut+
                    t2+               t2Points+         t2Classification+ t2Form+           
                    t2Atack+          t2AtackIn+        t2AtackOut+       t2Defense+        t2DefenseIn+      t2DefenseOut+    
                    t2AvgHtScoreIn+   t2AvgHtScoreOut+  t2AvgFtScoreIn+   t2AvgFtScoreOut+  t2AvgHtGgResult+  t2AvgFtGgResult+ 
                    t2WinsIn+         t2WinsOut+        t2DrawsIn+        t2DrawsOut+       t2LosesIn+        t2LosesOut)}
  
  else if(i==2){return(headOutcome~ 
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
  
  else if(i==3){return(headOutcome~week+  
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
  
  else if(i==4){return(headOutcome~  t1+              
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
differencedheadBet <- function(i){
  if (i==-1){# return size of atts datasets
    return(3)
  }
  
  if(i==1){return(headOutcome~  mfd1+      mfd2+    t1Classification+ t2Classification+  pd+  fd+  
                    #f1d+ f2d+ f3d+ f4d+
                    t1adoe+          t2adoe+           t1e+             t2e+
                    owd+         odd+          old+    doav_ht+ doav_ft+
                    dwin+        dwout+       ddin+        ddout+       dlin+        dlout+    
                    datkin+      datkout+     ddefin+      ddefout+   dav_ftin+    dav_ftout+
                    bet_1+       bet_X+       bet_2+ bet_O+       bet_U )}
  
  else if(i==2){return(headOutcome~t1+t2+   t1Form+ t2Form+
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
  
  else if(i==3){return(headOutcome~  mfd1+      mfd2+   # t1Classification+ t2Classification+  
                         pd+  fd+  
                         t1adoe+          t2adoe+           t1e+             t2e+
                         owd+         odd+          old+    doav_ht+ doav_ft+
                         dwin+        dwout+       ddin+        ddout+       dlin+        dlout+ datk+    
                         datkin+      datkout+     ddef+        ddefin+      ddefout+   dav_ftin+    dav_ftout+
                         bet_O+       bet_U)}
}

# 4 - 7 nrs /4
differencedheadNoBet <- function(i){
  if (i==-1){# return size of atts datasets
    return(4)
  } 
  
  if(i==1){return( headOutcome~t1+t2+  
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
  else if(i==2){return(headOutcome~#t1+t2+  
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
  else if(i==3){return(headOutcome~t1+t2+  
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
  
  else if(i==4){return(headOutcome~
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

