
folds <- split(df, cut(sample(1:nrow(df)),10))
folds <- split(df2, cut(sample(1:nrow(df2)),10))
folds <- split(df5, cut(sample(1:nrow(df5)),10))
accuracy<- rep(NA, length(folds))

gen_accuracy <- c();
for(algorithm in c("C50","J48","svm","naiveBayes","randomForest","rpart","bagging", "PART","JRip","AdaBoostM1")){
  print(algorithm); set.seed(1234);
  crv(f1,algorithm)  }
mean(gen_accuracy)



crv <-function(ho,algorithm){
  if(is.na(ho)){
    print("This ho is not found") 
    return(0)
  }
  
  accuracy <-c()
  
  #calc cros fold validation given the attributes involved
  for (i in 1:length(folds)) {
    test <- ldply(folds[i], data.frame)
    train <- ldply(folds[-i], data.frame)
    tmp.model<-c()
    #browser()
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
    
    
    tmp.predict <- predict(tmp.model, newdata = test, type = "class")
    conf.mat <- table(test$ht2pOutcome, tmp.predict)
    #errs[i] <- 1-sum(diag(conf.mat))/sum(conf.mat)
    accuracy[i] <- sum(diag(conf.mat)) / sum(conf.mat) 
  }
  gen_accuracy[length(gen_accuracy)+1]<<-100*mean(accuracy)
  #print(sprintf("average error using k-fold cross-validation: %.3f percent", 100*mean(errs)))
  print(sprintf("average accuracy using k-fold cross-validation: %.3f percent ", 100*mean(accuracy)))
}

full2pBet <- function(i){
  if(i==-1){return(6)}
  
  if(i==1){
    return(ht2pOutcome~ 
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
    return(ht2pOutcome~ 
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

  if(i==3){return(ht2pOutcome~ 
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
  
  if(i==4){return(ht2pOutcome~ t1Form1Diff+      t1Form2Diff+      #t1Form3Diff+      t1Form4Diff+     
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
    return(ht2pOutcome~ #week+ #t1+
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
    return(ht2pOutcome~ #week+ #t1+
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

full2pNoBet <- function(i){
  if (i==-1){return(5)}
  
  if(i==1){return(ht2pOutcome~ 
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
  
  if(i==2){return(ht2pOutcome~ 
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
    return(ht2pOutcome~ 
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
    return(ht2pOutcome~ 
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
    ht2pOutcome~ 
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



