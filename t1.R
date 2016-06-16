
Instance <- setRefClass("Instance",
                        fields = list(algo="character", attsDtsNr="numeric",
                                      accVal="numeric",fullDiff="character",bet="character")
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

CleanDtf <- setRefClass("CleanDtf",
            fields = list(algoDataList="vector",predAtt="character",
                    fmat="matrix",fcount="numeric",f2mat="matrix",f2count="numeric",
                    f5mat="matrix",f5count="numeric",
                    betmat="matrix",betcount="numeric",nobetmat="matrix",nobetcount="numeric",
                    fullmat="matrix",fullcount="numeric",diffmat="matrix",diffcount="numeric"),
        methods = list(
          predCalc= function(tt){
            #TODO : for every instance in the Dtf, predict the match results, sum thir probabilities 
            # and present them
            initMatrixes();
            
            for (algdat in algoDataList) {
              for (ins in algdat$instList) {
                model <- modelFunc(ins$algo,ins$attsDtsNr,ins$bet,ins$fullDiff,algdat$dtfCategory,predAtt)
                preVec <- predFunc()
                
              }
            }
            
          },
          initMatrixes=function(){
            #set to 0 all matrixes and counters
            fmat[,]<<-0; fcount<<-0; f2mat[,]<<-0; f2count<<-0; f5mat[,]<<-0; f5count<<-0;
            betmat[,]<<-0; betcount<<-0; nobetmat[,]<<-0; nobetcount<<-0;
            fullmat[,]<<-0; fullcount<<-0; diffmat[,]<<-0; diffcount<<-0;
          }
        )
)

modelFunc <- function(algorithm,attDtsNr,bet,fulDiff,dfCategory,predAtt){
  # get train based on df_category
  
  #TODO find ho based on  bet,fullDiff,attDtsNr, predAtt
  ho = attDtsFunc(attDtsNr,bet,fulDiff,predAtt)
  
  
  
  
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
  
}


attDtsFunc <- function(attDtsNr,bet,fullDiff,predAtt){
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

aa="bar"
switch(aa,foo={print("foo")},bar={print("bar")})
k=7

switch (aa,
  foo = {if(k>5){print("k>5 oooo")}else if(k<5){print("k<5 aaaa")}},
  "bar" = {if(k>5){print("k>5 sssss")}else if(k<5){print("k<5 llll")}}
)