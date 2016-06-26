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

folds <- split(wndf, cut(sample(1:nrow(ndf)),10))
folds <- split(dfn, cut(sample(1:nrow(dfn),replace = FALSE),10))
folds <- split(df2, cut(sample(1:nrow(df2)),10))
folds <- split(df5, cut(sample(1:nrow(df5)),10))
accuracy<- rep(NA, length(folds))



gen_accuracy <- c();
for(algorithm in c("OneR","AdaBoostM1", "C50","J48","bagging"
  #,"naiveBayes","rpart", "PART","randomForest","svm", "JRip"
                   )){
  print(algorithm); set.seed(1234);
  gen_accuracy[length(gen_accuracy)+1]<- scoreCrfv(differencedScoreNoBet(1),algorithm,folds)  }
mean(gen_accuracy)

dowscoreCrfv <-function(ho,algorithm,folds){
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

{f1 <- scoreOutcome~
  t1+t2+ t1Form+ t2Form+
  #mfd1+      mfd2+  #mfd+  #
  t1Classification+ t2Classification+ 
  #pd+  fd+  
  f1d+ f2d+ f3d+ f4d+
  # t1adoe+          t2adoe+           t1e+             t2e+
  # owd+         odd+          old+    
  # doav_ht+ 
  doav_ft+
  # dwin+        dwout+       ddin+        ddout+       dlin+        dlout+ 
  
  # datkin+      datkout+
  ddefin+      ddefout+
  dav_ftin+    dav_ftout}


f1 <- 
  
#--------------------------------------------------------------------------
 