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



folds <- split(ndf, cut(sample(1:nrow(ndf)),10))
folds <- split(df2, cut(sample(1:nrow(df2)),10))
folds <- split(df5, cut(sample(1:nrow(df5)),10))
accuracy<- rep(NA, length(folds))

gen_accuracy <- c();
for(algorithm in c("glm","svm","lm","bagging","Bagging"
                   #"lm"
                   )){
  print(algorithm); set.seed(1234);
  gen_accuracy[length(gen_accuracy)+1]<- totFtScoreCrfv(f1,algorithm,folds)  }
mean(gen_accuracy)

totFtScoreCrfv <-function(ho,algorithm,folds){
  # if(is.na(ho)){ print("This ho is not found");  return(0) }
  erre <- c()
  for (i in 1:length(folds)) {
    test <- ldply(folds[i], data.frame)
    train <- ldply(folds[-i], data.frame)
    tmp.model<-c()
    # browser()
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
  # print(erre)
  print(sprintf("mean squared error rate with k-fold cross-validation: %.3f percent ", mean(erre)))
  return(mean(erre))
}















































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


