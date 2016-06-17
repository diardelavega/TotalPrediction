
Instance <- setRefClass("Instance",
                        fields = list(algo="character", attsDtsNr="numeric",dfCategory="character",
                        accVal="numeric",fullDiff="character",bet="character",predvec="vector"),
                        methods = list(
                          accuracyReavaluation= function(ttResultsVec){
                            if(length(predvec)!=length(ttResultsVec)){
                              print("Prediction and results vector do not match");  return()}
                            
                            ttacc<-0;  # calculate prediction acccuracy
                            for(i in 1:length(ttResultsVec)){
                              if(ttResultsVec[i]==predvec[i]){ttacc=ttacc+1}
                            }
                            ttacc=ttacc/length(ttResultsVec)
                            
                            switch (dfCategory,
                              "df" = {dflen <-dim(df)[1] },
                              "ndf" = { dflen <-dim(df)[1] },
                              "df2" = { dflen <-dim(df2)[1] },
                              "ndf2" = {dflen <-dim(df2)[1] },
                              "df5" = { dflen <-dim(df5)[1] }, 
                              "ndf5" = { dflen <-dim(df5)[1] }
                            )
                            #value of new accuracy with the new component
                            nac <- (accVal * dflen + ttacc * length(ttResultsVec))/(dflen +  length(ttResultsVec))
                            accVal<<-nac                          
                          }
                        )
)


AlgoData <- setRefClass("AlgoData",
              # a field with the new moderated value for the overall crf_validation accuracy value
              # dtfCategory (df,ndf,df2,ndf2,df5,ndf5), attPred -> (ScoreOutcome, totFtScore, headOutcome, 2p,1p)
              # to consider a list of support predAtts like : *score| totFt; score|2p, 2p|totHt)
              fields = list( instList="vector", avgAcc="numeric", dtfCategory="character"),
              methods= list(
                betInstance = function(){  #get the instances of the instList attr with bet
                  rvec <- c()
                  for(j in 1:length(instList)){
                    if(instList[[j]]$bet =="yes"){
                      rvec[length(rvec)+1] <- j
                    }
                  }
                  return(rvec)
                },
                noBetInstance = function(){
                  rvec <- c()
                  for(j in 1:length(instList)){
                    if(instList[[j]]$bet =="no"){
                      rvec[length(rvec)+1] <- j
                    }
                  }
                  return(rvec)
                },
                calcAvgAccuracy = function(){ #get avg accuracy of instList instances
                  temp =0.0
                  for(j in 1:length(instList)){
                    temp = temp + instList[[j]]$accVal
                  }
                  avgAcc <<- temp/length(instList)
                },
                aboveAvgInstance = function(){# ret instList idx with accVal >= avgAcc
                  rvec <- c()
                  for(j in 1:length(instList)){
                    if(instList[[j]]$accVal>=avgAcc){
                      rvec[length(rvec)+1] <- j
                    }
                  }
                  return(rvec)
                }
              )         
)

CleanScoreDtf <- setRefClass("CleanDtf",
            fields = list(algoDataList="vector",predAtt="character",
                    ensambleMat="matrix", ensambleCount="numeric",      
                    fmat="matrix",fcount="numeric",f2mat="matrix",f2count="numeric",
                    f5mat="matrix",f5count="numeric",
                    betmat="matrix",betcount="numeric",nobetmat="matrix",nobetcount="numeric",
                    fullmat="matrix",fullcount="numeric",diffmat="matrix",diffcount="numeric"),
        methods = list(
          predCalcScore= function(tt){
            #TODO : for every instance in the Dtf, predict the match results, sum thir probabilities 
            # and present them
            initMatrixes(length(tt));
            
            for (algdat in algoDataList) {
              for (ins in algdat$instList) {
                model <- modelFunc(ins$algo,ins$attsDtsNr,ins$bet,ins$fullDiff,algdat$dtfCategory,predAtt)
                predVec <- predict(model,tt)
                
                retMat <-scoreResultCount(predVec,ins$accVal)
                ensambleMat <<- ensambleMat+retMat; ensambleCount<<-ensambleCount+1
                if(algdat$dtfCategory %in% c("df","ndf")){fmat <<- fmat+retMat; fcount<<-fcount+1}
                else if(algdat$dtfCategory %in% c("df2","ndf2")){f2mat <<- f2mat+retMat; f2count<<-f2count+1}
                else if(algdat$dtfCategory %in% c("df5","ndf5")){f5mat <<- f5mat+retMat; f5count<<-f5count+1}
                
                if(ins$bet =="bet"){betmat <<- betmat+retMat; betcount<<-betcount+1}
                else if(ins$bet =="no"){nobetmat <<- nobetmat+retMat; nobetcount<<-nobetcount+1}
                
                if(ins$fullDiff =="full"){fullmat <<- fullmat+retMat; fullcount<<-fullcount+1}
                else if(ins$fullDiff =="diff"){diffmat <<- diffmat+retMat; diffcount<<-diffcount+1}
              }
            }
          },
          initMatrixes=function(ttlength){
            #set to 0 all matrixes and counters
            ensambleMat <<-matrix(nrow =2, ncol = ttlength, data = 0)
            fmat<<-matrix(nrow =2, ncol = ttlength, data = 0)
            f2mat<<-matrix(nrow =2, ncol = ttlength, data = 0)
            f5mat<<-matrix(nrow =2, ncol = ttlength, data = 0)
            betcount<<-matrix(nrow =2, ncol = ttlength, data = 0)
            nobetmat<<-matrix(nrow =2, ncol = ttlength, data = 0)
            fullmat<<-matrix(nrow =2, ncol = ttlength, data = 0)
            diffmat<<-matrix(nrow =2, ncol = ttlength, data = 0)
            
            ensambleCount <<-0; fcount<<-0; f2count<<-0; f5count<<-0; 
            betcount<<-0;nobetcount<<-0;fullcount<<-0;diffcount<<-0;
          }
          #@ TODo functions that return the weighted accuracy for every 
          getEnsamble = function(){return(ensambleMat/ensambleCount)}
          getF = function(){return(fmat/fcount)}
          getF2 = function(){return(f2mat/f2count)}
          getF5 = function(){return(f5mat/f5count)}
          getBet = function(){return(betmat/betcount)}
          getNoBet = function(){return(nobetmat/nobetcount)}
          getFull = function(){return(fullmat/fullcount)}
          getDiff = function(){return(diffmat/diffcount)}
        )
)


#@ TODO think better a plan tomake the crfv and end up with these structures completed with data

modelFunc <- function(algorithm,attDtsNr,bet,fulDiff,dfCategory,predAtt){
  #generates a model for prediction based on the parameters from the best crfv results
  
  # get train based on df_category
  if(dfCategory == "df"){train <- df}
  else if(dfCategory == "ndf"){train <- ndf}
  else if(dfCategory == "df2"){train <- df[which(df$week>max(df$week)/2),]}
  else if(dfCategory == "ndf2"){train <- ndf[which(ndf$week>max(ndf$week)/2),]}
  else if(dfCategory == "df5"){train <- df[which(df$week>max(df$week)-6),]}
  else if(dfCategory == "ndf5"){train <-ndf[which(ndf$week>max(ndf$week)-6),]}
  
  ho = attDtsFunc(attDtsNr,bet,fulDiff,predAtt)
  
  if(algorithm=="C50"){tmp.model <- C5.0(ho , train,trails=10)}
  else if(algorithm=="J48"){tmp.model <- J48(ho , train)}
  else if(algorithm=="svm"){tmp.model <- svm(ho , train)}
  else if(algorithm=="naiveBayes"){tmp.model <- naiveBayes(ho , train )}
  else if(algorithm=="randomForest"){tmp.model <- randomForest(ho , train )}
  else if(algorithm=="rpart"){tmp.model <- rpart(ho , train)}
  else if(algorithm=="bagging"){tmp.model <- bagging(ho , train )}
  else if(algorithm=="PART"){tmp.model <- PART(ho , train )}
  else if(algorithm=="JRip"){tmp.model <- JRip(ho , train )}
  else if(algorithm=="OneR"){tmp.model <- OneR(ho , train )}
  else if(algorithm=="AdaBoostM1"){tmp.model <- AdaBoostM1(ho , train )}
  else if(algorithm=="lm"){tmp.model <- lm(ho , train )}
  else if(algorithm=="lgm"){tmp.model <- lgm(ho , train )}
  
  return (tmp.model)
}


attDtsFunc <- function(attDtsNr,bet,fullDiff,predAtt){
  # unifies the distinct functions for the att_dts_Nr .
  switch (predAtt,
    "head" = {switch(fullDiff,
                     full={switch (bet,
                                   "yes" = return (fullScoreBet(attDtsNr)),
                                   "no" = return (fullScoreNoBet(attDtsNr))
                     )},
                     diff={switch (bet,
                                   "yes" = return (differencedScoreBet(attDtsNr)),
                                   "no" = return (differencedScoreNoBet(attDtsNr))
                     )})
    },
    "score" = {switch(fullDiff,
                      full={switch (bet,
                        "yes" = return (fullScoreBet(attDtsNr)),
                        "no" = return (fullScoreNoBet(attDtsNr))
                      )},
                      diff={switch (bet,
                         "yes" = return (differencedScoreBet(attDtsNr)),
                         "no" = return (differencedScoreNoBet(attDtsNr))
                      )})
              },
    "2p" = {switch(fullDiff,
                   full={switch (bet,
                                 "yes" = return (fullScoreBet(attDtsNr)),
                                 "no" = return (fullScoreNoBet(attDtsNr))
                   )},
                   diff={switch (bet,
                                 "yes" = return (differencedScoreBet(attDtsNr)),
                                 "no" = return (differencedScoreNoBet(attDtsNr))
                   )})
    },
    "1p" = {switch(fullDiff,
                   full={switch (bet,
                                 "yes" = return (fullScoreBet(attDtsNr)),
                                 "no" = return (fullScoreNoBet(attDtsNr))
                   )},
                   diff={switch (bet,
                                 "yes" = return (differencedScoreBet(attDtsNr)),
                                 "no" = return (differencedScoreNoBet(attDtsNr))
                   )})
    },
    "totFt" = {switch(fullDiff,
                      full={switch (bet,
                                    "yes" = return (fullScoreBet(attDtsNr)),
                                    "no" = return (fullScoreNoBet(attDtsNr))
                      )},
                      diff={switch (bet,
                                    "yes" = return (differencedScoreBet(attDtsNr)),
                                    "no" = return (differencedScoreNoBet(attDtsNr))
                      )})
    },
    "totHt" = {switch(fullDiff,
                      full={switch (bet,
                                    "yes" = return (fullScoreBet(attDtsNr)),
                                    "no" = return (fullScoreNoBet(attDtsNr))
                      )},
                      diff={switch (bet,
                                    "yes" = return (differencedScoreBet(attDtsNr)),
                                    "no" = return (differencedScoreNoBet(attDtsNr))
                      )})
    }
  )
}



#####---------- XXXXXXResultCount__ series of func to summ accuracy for each prediction

scoreResultCount <- function(pv,acc){
  temp_mat <- matrix(nrow = 2,ncol = length(pv),data = 0)
  for(i in pv){
    if(pv[i]=="O"){temp_mat[1,i]<-acc}
    else if(pv[i]=="U"){temp_mat[2,i]<-acc}
  }
  return (temp_mat)
}

HeadResultCount <- function(pv,acc){
  temp_mat <- matrix(nrow = 3,ncol = length(pv),data = 0)
  for(i in pv){
    if(pv[i]=="1"){temp_mat[1,i]<-acc}
    else if(pv[i]=="2"){temp_mat[2,i]<-acc}
    else if(pv[i]=="X"){temp_mat[3,i]<-acc}
  }
  return (temp_mat)
}

p2ResultCount <- function(pv,acc){
  temp_mat <- matrix(nrow = 2,ncol = length(pv),data = 0)
  for(i in pv){
    if(pv[i]=="Y"){temp_mat[1,i]<-acc}
    else if(pv[i]=="N"){temp_mat[2,i]<-acc}
  }
  return (temp_mat)
}

p1ResultCount <- function(pv,acc){
  temp_mat <- matrix(nrow = 2,ncol = length(pv),data = 0)
  for(i in pv){
    if(pv[i]=="Y"){temp_mat[1,i]<-acc}
    else if(pv[i]=="N"){temp_mat[2,i]<-acc}
  }
  return (temp_mat)
}




pf <- AlgoData$new()

a<-  Instance$new(algo = "C50",attsDtsNr=1,accVal=333, bet="yes", fullDiff="full")
pf$instList <- c(a)

for (i in 1:10){
pf$instList[length(pf$instList)+1] <-  Instance$new(algo = "C50",attsDtsNr=1,accVal=i, bet="yes", fullDiff="full") }

pf$betInstance()

for(ins in pf$instList){
  print(ins$algo)
}




a <- matrix(nrow = 2,ncol = 5)
b <- matrix(nrow = 2,ncol = 5)

a[,]<-0
b[,]<-0

for( i in 1:5){
#  a[1,i]<- 4
  r<-sample(1:2,1)
  a[r,i]<- sample(1,1,10)
}

aa="fo"
switch(aa,foo={print("foo")},bar={print("bar")})
k=7

switch (aa,
  "foo" = {if(k>5){print("k>5 oooo")}else if(k<5){print("k<5 aaaa")}},
  "bar" = {if(k>5){print("k>5 sssss")}else if(k<5){print("k<5 llll")}}
)
