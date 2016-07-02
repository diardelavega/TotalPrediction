
Instance <- setRefClass("Instance",
                        fields = list(algo="character", attsDtsNr="numeric",dfCategory="character",ptype="character",
                        accVal="numeric",fullDiff="character",bet="character",predvec="vector"),
                        methods = list(
                          reEvaluate =function(ttResultsVec){
                              if(length(predvec)!=length(ttResultsVec)){
                                print("Prediction and results vector do not match");  return()}
                              switch (dfCategory,
                                      "f" = {dflen <-dim(df)[1] },
                                      "f2" = { dflen <-dim(df2)[1] },
                                      "f5" = { dflen <-dim(df5)[1] }
                              )
                              
                              if(ptype=="categoric"){accuracyReEvaluation(ttResultsVec, dflen)}
                              else if(ptype=="numeric"){errorReEvaluation(ttResultsVec, dflen)}
                            },
                          errorReEvaluation = function(ttResultsVec, dflen){
                            curent_errr <- sqrt(1/length(predvec) * sum( (tpredvec-tttResultsVec)^2 ))
                            nac <- (accVal * dflen + curent_errr * length(ttResultsVec)) / (dflen +  length(ttResultsVec))
                            accVal<<-nac 
                          },
                          accuracyReEvaluation= function(ttResultsVec, dflen){
                            ttacc<-0;  # calculate prediction acccuracy
                            for(i in 1:length(ttResultsVec)){
                              if(ttResultsVec[i]==predvec[i]){ttacc=ttacc+1}
                            }
                            ttacc=ttacc/length(ttResultsVec)
                           
                            #value of new accuracy with the new component
                            nac <- (accVal * dflen + ttacc * length(ttResultsVec)) / (dflen +  length(ttResultsVec))
                            accVal<<-nac                          
                          }
                        )
)


AlgoData <- setRefClass("AlgoData",
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

{
CleanScoreDtf <- setRefClass("CleanScoreDtf",
            fields = list(algoDataList="vector",predAtt="character",
                    ensambleMat="matrix", fmat="matrix",f2mat="matrix", f5mat="matrix", 
                    betmat="matrix", nobetmat="matrix",fullmat="matrix",diffmat="matrix",
                    ensambleCount="numeric", fcount="numeric",f2count="numeric", f5count="numeric",
                    betcount="numeric",nobetcount="numeric", fullcount="numeric",diffcount="numeric"),
        methods = list(
          scoreMatOrientation= function(){cat("1 row - O  \n2 row - U \n") },
          initMatrixes=function(ttlength){
            print("init mats")
            #set to 0 all matrixes and counters
            rowlen <-2
            ensambleMat <<-matrix(nrow =rowlen, ncol = ttlength, data = 0)
            fmat<<-matrix(nrow =rowlen, ncol = ttlength, data = 0)
            f2mat<<-matrix(nrow =rowlen, ncol = ttlength, data = 0)
            f5mat<<-matrix(nrow =rowlen, ncol = ttlength, data = 0)
            betmat<<-matrix(nrow =rowlen, ncol = ttlength, data = 0)
            nobetmat<<-matrix(nrow =rowlen, ncol = ttlength, data = 0)
            fullmat<<-matrix(nrow =rowlen, ncol = ttlength, data = 0)
            diffmat<<-matrix(nrow =rowlen, ncol = ttlength, data = 0)
            
            ensambleCount <<- 0; fcount<<- 0; f2count<<- 0; f5count<<- 0; 
            betcount<<- 0; nobetcount<<- 0; fullcount<<- 0; diffcount<<- 0;
          },
          accuracyRecalc =function(predResultVec){
            # for every Instance obj in the Dtf, call accuracy re-evaluation
            for (algdat in algoDataList) {
              for (ins in algdat$instList) {
                ins$accuracyReavaluation(predResultVec)
              }}
          },
          predCalcScore= function(tt){
            initMatrixes(dim(tt)[1]);
            newTt()# regulate tt with ndf and t1,t2 classification factors
             algcount <-0
             
          # for (algdat in algoDataList) {
             # for (ins in algdat$instList) {
            for (al_i in 1:length(algoDataList)) {
              algdat <- algoDataList[[al_i]]
              for (ins_j in 1:length( algdat$instList)) {
                ins <- algdat$instList[[ins_j]]
                  
                algcount=algcount+1
                cat(algcount,ins$algo,ins$attsDtsNr,ins$bet,ins$fullDiff,algdat$dtfCategory,predAtt,"\n")
                
                model <- modelFunc(ins$algo,ins$attsDtsNr,ins$bet,ins$fullDiff,algdat$dtfCategory,predAtt)
                predVec <- as.vector(predict(model,tt, type = "class"))
                print(predVec)
                
                algoDataList[[al_i]]$instList[[ins_j]]$predvec  <<-predVec
                # ins$predvec  <<-predVec
                
                retMat <-scoreResultCount(as.vector(predVec),ins$accVal)
                # cat("retmat", dim(retMat), "     ensam",dim(ensambleMat),"\n" )
                
                ensambleMat <<- ensambleMat+retMat; ensambleCount<<-ensambleCount+1
                if(algdat$dtfCategory =="f"){fmat <<- fmat+retMat; fcount<<-fcount+1}
                else if(algdat$dtfCategory =="f2"){f2mat <<- f2mat+retMat; f2count<<-f2count+1}
                else if(algdat$dtfCategory =="f5"){f5mat <<- f5mat+retMat; f5count<<-f5count+1}
                
                if(ins$bet =="yes"){betmat <<- betmat+retMat; betcount<<-betcount+1}
                else if(ins$bet =="no"){nobetmat <<- nobetmat+retMat; nobetcount<<-nobetcount+1}
                
                if(ins$fullDiff =="full"){fullmat <<- fullmat+retMat; fullcount<<-fullcount+1}
                else if(ins$fullDiff =="diff"){diffmat <<- diffmat+retMat; diffcount<<-diffcount+1}
              }
            }
          },
          
          getEnsamble = function(){scoreMatOrientation(); return(ensambleMat/ensambleCount)},
          getF = function(){return(fmat/fcount)},
          getF2 = function(){return(f2mat/f2count)},
          getF5 = function(){return(f5mat/f5count)},
          getBet = function(){return(betmat/betcount)},
          getNoBet = function(){return(nobetmat/nobetcount)},
          getFull = function(){return(fullmat/fullcount)},
          getDiff = function(){return(diffmat/diffcount)}
        )
)
}
 #### test for git libe
#---------
aa <- CleanScoreDtf$new()
aa$algoDataList <- csDtf$algoDataList
aa$predAtt <- "score"
aa$predCalcScore(tt)
aa$getEnsamble()

bb <- CleanHeadDtf$new()
bb$algoDataList <- hDtf$algoDataList
bb$predAtt <- "head"
bb$predCalcScore(tt)
#---------
#@ TODO a function to show a  vector composed of the dominant accuracy || error results   


#head
{
  CleanHeadDtf <- setRefClass("CleanHeadDtf",
                               fields = list(algoDataList="vector",predAtt="character",
                                             ensambleMat="matrix", fmat="matrix",f2mat="matrix", f5mat="matrix", 
                                             betmat="matrix", nobetmat="matrix",fullmat="matrix",diffmat="matrix",
                                             ensambleCount="numeric", fcount="numeric",f2count="numeric", f5count="numeric",
                                             betcount="numeric",nobetcount="numeric", fullcount="numeric",diffcount="numeric"),
                               methods = list(
                                 headMatOrientation= function(){cat("1 row - 1  \n2 row - X \n3 row - 2 \n") },
                                 initMatrixes=function(ttlength){
                                   print("init mats")
                                   #set to 0 all matrixes and counters
                                   rowlen <-3
                                   ensambleMat <<-matrix(nrow =rowlen, ncol = ttlength, data = 0)
                                   fmat<<-matrix(nrow =rowlen, ncol = ttlength, data = 0)
                                   f2mat<<-matrix(nrow =rowlen, ncol = ttlength, data = 0)
                                   f5mat<<-matrix(nrow =rowlen, ncol = ttlength, data = 0)
                                   betmat<<-matrix(nrow =rowlen, ncol = ttlength, data = 0)
                                   nobetmat<<-matrix(nrow =rowlen, ncol = ttlength, data = 0)
                                   fullmat<<-matrix(nrow =rowlen, ncol = ttlength, data = 0)
                                   diffmat<<-matrix(nrow =rowlen, ncol = ttlength, data = 0)
                                   
                                   ensambleCount <<- 0; fcount<<- 0; f2count<<- 0; f5count<<- 0; 
                                   betcount<<- 0; nobetcount<<- 0; fullcount<<- 0; diffcount<<- 0;
                                 },
                                 accuracyRecalc =function(predResultVec){
                                   # for every Instance obj in the Dtf, call accuracy re-evaluation
                                   for (algdat in algoDataList) {
                                     for (ins in algdat$instList) {
                                       ins$accuracyReavaluation(predResultVec)
                                     }}
                                 },
                                 predCalcScore= function(tt){
                                   initMatrixes(dim(tt)[1]);
                                   newTt()# regulate tt with ndf and t1,t2 classification factors
                                   algcount <-0
                                   for (al_i in 1:length(algoDataList)) {
                                     algdat <- algoDataList[[al_i]]
                                     for (ins_j in 1:length( algdat$instList)) {
                                       ins <- algdat$instList[[ins_j]]
                                       algcount=algcount+1
                                       cat(algcount,ins$algo,ins$attsDtsNr,ins$bet,ins$fullDiff,algdat$dtfCategory,predAtt,"\n")
                                       
                                       model <- modelFunc(ins$algo,ins$attsDtsNr,ins$bet,ins$fullDiff,algdat$dtfCategory,predAtt)
                                       predVec <- as.vector(predict(model,tt, type = "class"))
                                       print(predVec)
                                       
                                       algoDataList[[al_i]]$instList[[ins_j]]$predvec  <<-predVec
                                       
                                       retMat <-headResultCount(as.vector(predVec),ins$accVal)
                                       # cat("retmat", dim(retMat), "     ensam",dim(ensambleMat),"\n" )
                                       
                                       ensambleMat <<- ensambleMat+retMat; ensambleCount<<-ensambleCount+1
                                       if(algdat$dtfCategory =="f"){fmat <<- fmat+retMat; fcount<<-fcount+1}
                                       else if(algdat$dtfCategory =="f2"){f2mat <<- f2mat+retMat; f2count<<-f2count+1}
                                       else if(algdat$dtfCategory =="f5"){f5mat <<- f5mat+retMat; f5count<<-f5count+1}
                                       
                                       if(ins$bet =="yes"){betmat <<- betmat+retMat; betcount<<-betcount+1}
                                       else if(ins$bet =="no"){nobetmat <<- nobetmat+retMat; nobetcount<<-nobetcount+1}
                                       
                                       if(ins$fullDiff =="full"){fullmat <<- fullmat+retMat; fullcount<<-fullcount+1}
                                       else if(ins$fullDiff =="diff"){diffmat <<- diffmat+retMat; diffcount<<-diffcount+1}
                                     }
                                   }
                                 },
                                 
                                 getEnsamble = function(){headMatOrientation(); return(ensambleMat/ensambleCount)},
                                 getF = function(){return(fmat/fcount)},
                                 getF2 = function(){return(f2mat/f2count)},
                                 getF5 = function(){return(f5mat/f5count)},
                                 getBet = function(){return(betmat/betcount)},
                                 getNoBet = function(){return(nobetmat/nobetcount)},
                                 getFull = function(){return(fullmat/fullcount)},
                                 getDiff = function(){return(diffmat/diffcount)}
                               )
  )
}

#2p
{
  Clean2pDtf <- setRefClass("Clean2pDtf",
                              fields = list(algoDataList="vector",predAtt="character",
                                            ensambleMat="matrix", fmat="matrix",f2mat="matrix", f5mat="matrix", 
                                            betmat="matrix", nobetmat="matrix",fullmat="matrix",diffmat="matrix",
                                            ensambleCount="numeric", fcount="numeric",f2count="numeric", f5count="numeric",
                                            betcount="numeric",nobetcount="numeric", fullcount="numeric",diffcount="numeric"),
                              methods = list(
                                p2MatOrientation= function(){cat("1 row - Yes  \n2 row - No \n ") },
                                initMatrixes=function(ttlength){
                                  print("init mats")
                                  #set to 0 all matrixes and counters
                                  rowlen <-2
                                  ensambleMat <<-matrix(nrow =rowlen, ncol = ttlength, data = 0)
                                  fmat<<-matrix(nrow =rowlen, ncol = ttlength, data = 0)
                                  f2mat<<-matrix(nrow =rowlen, ncol = ttlength, data = 0)
                                  f5mat<<-matrix(nrow =rowlen, ncol = ttlength, data = 0)
                                  betmat<<-matrix(nrow =rowlen, ncol = ttlength, data = 0)
                                  nobetmat<<-matrix(nrow =rowlen, ncol = ttlength, data = 0)
                                  fullmat<<-matrix(nrow =rowlen, ncol = ttlength, data = 0)
                                  diffmat<<-matrix(nrow =rowlen, ncol = ttlength, data = 0)
                                  
                                  ensambleCount <<- 0; fcount<<- 0; f2count<<- 0; f5count<<- 0; 
                                  betcount<<- 0; nobetcount<<- 0; fullcount<<- 0; diffcount<<- 0;
                                },
                                accuracyRecalc =function(predResultVec){
                                  # for every Instance obj in the Dtf, call accuracy re-evaluation
                                  for (algdat in algoDataList) {
                                    for (ins in algdat$instList) {
                                      ins$accuracyReavaluation(predResultVec)
                                    }}
                                },
                                predCalcScore= function(tt){
                                  initMatrixes(dim(tt)[1]);
                                  newTt()# regulate tt with ndf and t1,t2 classification factors
                                  algcount <-0
                                  for (al_i in 1:length(algoDataList)) {
                                    algdat <- algoDataList[[al_i]]
                                    for (ins_j in 1:length( algdat$instList)) {
                                      ins <- algdat$instList[[ins_j]]
                                      algcount=algcount+1
                                      cat(algcount,ins$algo,ins$attsDtsNr,ins$bet,ins$fullDiff,algdat$dtfCategory,predAtt,"\n")
                                      
                                      model <- modelFunc(ins$algo,ins$attsDtsNr,ins$bet,ins$fullDiff,algdat$dtfCategory,predAtt)
                                      predVec <- as.vector(predict(model,tt, type = "class"))
                                      print(predVec)
                                      
                                      algoDataList[[al_i]]$instList[[ins_j]]$predvec  <<-predVec
                                      
                                      retMat <-p2ResultCount(as.vector(predVec),ins$accVal)
                                      # cat("retmat", dim(retMat), "     ensam",dim(ensambleMat),"\n" )
                                      
                                      ensambleMat <<- ensambleMat+retMat; ensambleCount<<-ensambleCount+1
                                      if(algdat$dtfCategory =="f"){fmat <<- fmat+retMat; fcount<<-fcount+1}
                                      else if(algdat$dtfCategory =="f2"){f2mat <<- f2mat+retMat; f2count<<-f2count+1}
                                      else if(algdat$dtfCategory =="f5"){f5mat <<- f5mat+retMat; f5count<<-f5count+1}
                                      
                                      if(ins$bet =="yes"){betmat <<- betmat+retMat; betcount<<-betcount+1}
                                      else if(ins$bet =="no"){nobetmat <<- nobetmat+retMat; nobetcount<<-nobetcount+1}
                                      
                                      if(ins$fullDiff =="full"){fullmat <<- fullmat+retMat; fullcount<<-fullcount+1}
                                      else if(ins$fullDiff =="diff"){diffmat <<- diffmat+retMat; diffcount<<-diffcount+1}
                                    }
                                  }
                                },
                                
                                getEnsamble = function(){p2MatOrientation(); return(ensambleMat/ensambleCount)},
                                getF = function(){return(fmat/fcount)},
                                getF2 = function(){return(f2mat/f2count)},
                                getF5 = function(){return(f5mat/f5count)},
                                getBet = function(){return(betmat/betcount)},
                                getNoBet = function(){return(nobetmat/nobetcount)},
                                getFull = function(){return(fullmat/fullcount)},
                                getDiff = function(){return(diffmat/diffcount)}
                              )
  )
}

#1p
{
  Clean1pDtf <- setRefClass("Clean1pDtf",
                              fields = list(algoDataList="vector",predAtt="character",
                                            ensambleMat="matrix", fmat="matrix",f2mat="matrix", f5mat="matrix", 
                                            betmat="matrix", nobetmat="matrix",fullmat="matrix",diffmat="matrix",
                                            ensambleCount="numeric", fcount="numeric",f2count="numeric", f5count="numeric",
                                            betcount="numeric",nobetcount="numeric", fullcount="numeric",diffcount="numeric"),
                              methods = list(
                                p1MatOrientation= function(){cat("1 row - Yes  \n2 row - No \n ") },
                                initMatrixes=function(ttlength){
                                  print("init mats")
                                  #set to 0 all matrixes and counters
                                  rowlen <-2
                                  ensambleMat <<-matrix(nrow =rowlen, ncol = ttlength, data = 0)
                                  fmat<<-matrix(nrow =rowlen, ncol = ttlength, data = 0)
                                  f2mat<<-matrix(nrow =rowlen, ncol = ttlength, data = 0)
                                  f5mat<<-matrix(nrow =rowlen, ncol = ttlength, data = 0)
                                  betmat<<-matrix(nrow =rowlen, ncol = ttlength, data = 0)
                                  nobetmat<<-matrix(nrow =rowlen, ncol = ttlength, data = 0)
                                  fullmat<<-matrix(nrow =rowlen, ncol = ttlength, data = 0)
                                  diffmat<<-matrix(nrow =rowlen, ncol = ttlength, data = 0)
                                  
                                  ensambleCount <<- 0; fcount<<- 0; f2count<<- 0; f5count<<- 0; 
                                  betcount<<- 0; nobetcount<<- 0; fullcount<<- 0; diffcount<<- 0;
                                },
                                accuracyRecalc =function(predResultVec){
                                  # for every Instance obj in the Dtf, call accuracy re-evaluation
                                  for (algdat in algoDataList) {
                                    for (ins in algdat$instList) {
                                      ins$accuracyReavaluation(predResultVec)
                                    }}
                                },
                                predCalcScore= function(tt){
                                  initMatrixes(dim(tt)[1]);
                                  newTt()# regulate tt with ndf and t1,t2 classification factors
                                  algcount <-0
                                  for (al_i in 1:length(algoDataList)) {
                                    algdat <- algoDataList[[al_i]]
                                    for (ins_j in 1:length( algdat$instList)) {
                                      ins <- algdat$instList[[ins_j]]
                                      algcount=algcount+1
                                      cat(algcount,ins$algo,ins$attsDtsNr,ins$bet,ins$fullDiff,algdat$dtfCategory,predAtt,"\n")
                                      
                                      model <- modelFunc(ins$algo,ins$attsDtsNr,ins$bet,ins$fullDiff,algdat$dtfCategory,predAtt)
                                      predVec <- as.vector(predict(model,tt, type = "class"))
                                      print(predVec)
                                      
                                      algoDataList[[al_i]]$instList[[ins_j]]$predvec  <<-predVec
                                      
                                      retMat <-p1ResultCount(as.vector(predVec),ins$accVal)
                                      # cat("retmat", dim(retMat), "     ensam",dim(ensambleMat),"\n" )
                                      
                                      ensambleMat <<- ensambleMat+retMat; ensambleCount<<-ensambleCount+1
                                      if(algdat$dtfCategory =="f"){fmat <<- fmat+retMat; fcount<<-fcount+1}
                                      else if(algdat$dtfCategory =="f2"){f2mat <<- f2mat+retMat; f2count<<-f2count+1}
                                      else if(algdat$dtfCategory =="f5"){f5mat <<- f5mat+retMat; f5count<<-f5count+1}
                                      
                                      if(ins$bet =="yes"){betmat <<- betmat+retMat; betcount<<-betcount+1}
                                      else if(ins$bet =="no"){nobetmat <<- nobetmat+retMat; nobetcount<<-nobetcount+1}
                                      
                                      if(ins$fullDiff =="full"){fullmat <<- fullmat+retMat; fullcount<<-fullcount+1}
                                      else if(ins$fullDiff =="diff"){diffmat <<- diffmat+retMat; diffcount<<-diffcount+1}
                                    }
                                  }
                                },
                                
                                getEnsamble = function(){p1MatOrientation(); return(ensambleMat/ensambleCount)},
                                getF = function(){return(fmat/fcount)},
                                getF2 = function(){return(f2mat/f2count)},
                                getF5 = function(){return(f5mat/f5count)},
                                getBet = function(){return(betmat/betcount)},
                                getNoBet = function(){return(nobetmat/nobetcount)},
                                getFull = function(){return(fullmat/fullcount)},
                                getDiff = function(){return(diffmat/diffcount)}
                              )
  )
}

#totFt
{
  # the totFt & totHt DTF predict non categorical data, they pred numeric data. thus they are different in their result interpretation
  CleanTotFtDtf <- setRefClass("CleanTotFtDtf",
                            fields = list(algoDataList="vector",predAtt="character",
                                          ensambleMat="matrix", fmat="matrix",f2mat="matrix", f5mat="matrix", 
                                          betmat="matrix", nobetmat="matrix",fullmat="matrix",diffmat="matrix",
                                          ensambleCount="numeric", fcount="numeric",f2count="numeric", f5count="numeric",
                                          betcount="numeric",nobetcount="numeric", fullcount="numeric",diffcount="numeric"),
                            methods = list(
                              totFtMatOrientation= function(){
                                print("the values represent error rate values; the smaller the better")
                                print(" row 1 --:  0.5 > goals        .... 0")
                                print(" row 2 --:  0.6 <= goals < 1.5 .... 1")
                                print(" row 3 --:  1.6 <= goals < 2.5 .... 2")
                                print(" row 4 --:  2.6 <= goals < 3.5 .... 3")
                                print(" row 5 --:  3.6 <= goals < 4.5 .... 4")
                                print(" row 6 --:  4.6 <= goals       .... 5")
                              },
                              initMatrixes=function(ttlength){
                                print("init mats")
                                #set to 0 all matrixes and counters
                                rowlen <-6
                                ensambleMat <<-matrix(nrow =rowlen, ncol = ttlength, data = 0)
                                fmat<<-matrix(nrow =rowlen, ncol = ttlength, data = 0)
                                f2mat<<-matrix(nrow =rowlen, ncol = ttlength, data = 0)
                                f5mat<<-matrix(nrow =rowlen, ncol = ttlength, data = 0)
                                betmat<<-matrix(nrow =rowlen, ncol = ttlength, data = 0)
                                nobetmat<<-matrix(nrow =rowlen, ncol = ttlength, data = 0)
                                fullmat<<-matrix(nrow =rowlen, ncol = ttlength, data = 0)
                                diffmat<<-matrix(nrow =rowlen, ncol = ttlength, data = 0)
                                
                                ensambleCount <<- 0; fcount<<- 0; f2count<<- 0; f5count<<- 0; 
                                betcount<<- 0; nobetcount<<- 0; fullcount<<- 0; diffcount<<- 0;
                              },
                              accuracyRecalc =function(predResultVec){
                                # for every Instance obj in the Dtf, call accuracy re-evaluation
                                for (algdat in algoDataList) {
                                  for (ins in algdat$instList) {
                                    ins$accuracyReavaluation(predResultVec)
                                  }}
                              },
                              predCalcScore= function(tt){
                                initMatrixes(dim(tt)[1]);
                                newTt()# regulate tt with ndf and t1,t2 classification factors
                                algcount <-0
                                for (al_i in 1:length(algoDataList)) {
                                  algdat <- algoDataList[[al_i]]
                                  for (ins_j in 1:length( algdat$instList)) {
                                    ins <- algdat$instList[[ins_j]]
                                    algcount=algcount+1
                                    cat(algcount,ins$algo,ins$attsDtsNr,ins$bet,ins$fullDiff,algdat$dtfCategory,predAtt,"\n")
                                    
                                    model <- modelFunc(ins$algo,ins$attsDtsNr,ins$bet,ins$fullDiff,algdat$dtfCategory,predAtt)
                                    predVec <- as.vector(predict(model,tt, type = "class"))
                                    print(predVec)
                                    
                                    algoDataList[[al_i]]$instList[[ins_j]]$predvec  <<-predVec
                                    
                                    retMat <-totFtResultCount(as.vector(predVec),ins$accVal)
                                    # cat("retmat", dim(retMat), "     ensam",dim(ensambleMat),"\n" )
                                    
                                    ensambleMat <<- ensambleMat+retMat; ensambleCount<<-ensambleCount+1
                                    if(algdat$dtfCategory =="f"){fmat <<- fmat+retMat; fcount<<-fcount+1}
                                    else if(algdat$dtfCategory =="f2"){f2mat <<- f2mat+retMat; f2count<<-f2count+1}
                                    else if(algdat$dtfCategory =="f5"){f5mat <<- f5mat+retMat; f5count<<-f5count+1}
                                    
                                    if(ins$bet =="yes"){betmat <<- betmat+retMat; betcount<<-betcount+1}
                                    else if(ins$bet =="no"){nobetmat <<- nobetmat+retMat; nobetcount<<-nobetcount+1}
                                    
                                    if(ins$fullDiff =="full"){fullmat <<- fullmat+retMat; fullcount<<-fullcount+1}
                                    else if(ins$fullDiff =="diff"){diffmat <<- diffmat+retMat; diffcount<<-diffcount+1}
                                  }
                                }
                              },
                              
                              getEnsamble = function(){totFtMatOrientation(); return(ensambleMat/ensambleCount)},
                              getF = function(){return(fmat/fcount)},
                              getF2 = function(){return(f2mat/f2count)},
                              getF5 = function(){return(f5mat/f5count)},
                              getBet = function(){return(betmat/betcount)},
                              getNoBet = function(){return(nobetmat/nobetcount)},
                              getFull = function(){return(fullmat/fullcount)},
                              getDiff = function(){return(diffmat/diffcount)}
                            )
  )
}

#totHt
{
  # the totFt & totHt DTF predict non categorical data, they pred numeric data. thus they are different in their result interpretation
  CleanTotHtDtf <- setRefClass("CleanTotHtDtf",
                               fields = list(algoDataList="vector",predAtt="character",
                                             ensambleMat="matrix", fmat="matrix",f2mat="matrix", f5mat="matrix", 
                                             betmat="matrix", nobetmat="matrix",fullmat="matrix",diffmat="matrix",
                                             ensambleCount="numeric", fcount="numeric",f2count="numeric", f5count="numeric",
                                             betcount="numeric",nobetcount="numeric", fullcount="numeric",diffcount="numeric"),
                               methods = list(
                                 totHtMatOrientation= function(){
                                   print("the values represent error rate values; the smaller the better")
                                   print(" row 1 --:  0.5 > goals        .... 0")
                                   print(" row 2 --:  0.6 <= goals < 1.5 .... 1")
                                   print(" row 3 --:  1.6 <= goals < 2.5 .... 2")
                                   print(" row 4 --:  2.6 <= goals < 3.5 .... 3")
                                   print(" row 5 --:  3.6 <= goals < 4.5 .... 4")
                                   print(" row 6 --:  4.6 <= goals       .... 5")
                                 },
                                 initMatrixes=function(ttlength){
                                   print("init mats")
                                   #set to 0 all matrixes and counters
                                   rowlen <-6
                                   ensambleMat <<-matrix(nrow =rowlen, ncol = ttlength, data = 0)
                                   fmat<<-matrix(nrow =rowlen, ncol = ttlength, data = 0)
                                   f2mat<<-matrix(nrow =rowlen, ncol = ttlength, data = 0)
                                   f5mat<<-matrix(nrow =rowlen, ncol = ttlength, data = 0)
                                   betmat<<-matrix(nrow =rowlen, ncol = ttlength, data = 0)
                                   nobetmat<<-matrix(nrow =rowlen, ncol = ttlength, data = 0)
                                   fullmat<<-matrix(nrow =rowlen, ncol = ttlength, data = 0)
                                   diffmat<<-matrix(nrow =rowlen, ncol = ttlength, data = 0)
                                   
                                   ensambleCount <<- 0; fcount<<- 0; f2count<<- 0; f5count<<- 0; 
                                   betcount<<- 0; nobetcount<<- 0; fullcount<<- 0; diffcount<<- 0;
                                 },
                                 accuracyRecalc =function(predResultVec){
                                   # for every Instance obj in the Dtf, call accuracy re-evaluation
                                   for (algdat in algoDataList) {
                                     for (ins in algdat$instList) {
                                       ins$accuracyReavaluation(predResultVec)
                                     }}
                                 },
                                 predCalcScore= function(tt){
                                   initMatrixes(dim(tt)[1]);
                                   newTt()# regulate tt with ndf and t1,t2 classification factors
                                   algcount <-0
                                   for (al_i in 1:length(algoDataList)) {
                                     algdat <- algoDataList[[al_i]]
                                     for (ins_j in 1:length( algdat$instList)) {
                                       ins <- algdat$instList[[ins_j]]
                                       algcount=algcount+1
                                       cat(algcount,ins$algo,ins$attsDtsNr,ins$bet,ins$fullDiff,algdat$dtfCategory,predAtt,"\n")
                                       
                                       model <- modelFunc(ins$algo,ins$attsDtsNr,ins$bet,ins$fullDiff,algdat$dtfCategory,predAtt)
                                       predVec <- as.vector(predict(model,tt, type = "class"))
                                       print(predVec)
                                       
                                       algoDataList[[al_i]]$instList[[ins_j]]$predvec  <<-predVec
                                       
                                       retMat <-totHtResultCount(as.vector(predVec),ins$accVal)
                                       # cat("retmat", dim(retMat), "     ensam",dim(ensambleMat),"\n" )
                                       
                                       ensambleMat <<- ensambleMat+retMat; ensambleCount<<-ensambleCount+1
                                       if(algdat$dtfCategory =="f"){fmat <<- fmat+retMat; fcount<<-fcount+1}
                                       else if(algdat$dtfCategory =="f2"){f2mat <<- f2mat+retMat; f2count<<-f2count+1}
                                       else if(algdat$dtfCategory =="f5"){f5mat <<- f5mat+retMat; f5count<<-f5count+1}
                                       
                                       if(ins$bet =="yes"){betmat <<- betmat+retMat; betcount<<-betcount+1}
                                       else if(ins$bet =="no"){nobetmat <<- nobetmat+retMat; nobetcount<<-nobetcount+1}
                                       
                                       if(ins$fullDiff =="full"){fullmat <<- fullmat+retMat; fullcount<<-fullcount+1}
                                       else if(ins$fullDiff =="diff"){diffmat <<- diffmat+retMat; diffcount<<-diffcount+1}
                                     }
                                   }
                                 },
                                 
                                 getEnsamble = function(){totHtMatOrientation(); return(ensambleMat/ensambleCount)},
                                 getF = function(){return(fmat/fcount)},
                                 getF2 = function(){return(f2mat/f2count)},
                                 getF5 = function(){return(f5mat/f5count)},
                                 getBet = function(){return(betmat/betcount)},
                                 getNoBet = function(){return(nobetmat/nobetcount)},
                                 getFull = function(){return(fullmat/fullcount)},
                                 getDiff = function(){return(diffmat/diffcount)}
                               )
  )
}


#@ TODO test score Dtf an if succesfull implement head, 2p, 1p cleanDtfs

modelFunc <- function(algorithm,attDtsNr,bet,fulDiff,dfCategory,predAtt){
  #generates a model for prediction based on the parameters from the best crfv results
  # print("modelFunc")
  # get train based on df_category
  switch (dfCategory,
    f = {switch (fulDiff,
                  "full" = {train <- df},
                  "diff" = {train <- ndf})},
    f2 = {switch (fulDiff,
                  "full" = {train <- df[which(df$week>max(df$week)/2),]},
                  "diff" = {train <- ndf[which(ndf$week>max(ndf$week)/2),]})},
    f5 = {switch (fulDiff,
                  "full" = {train <- df[which(df$week>max(df$week)-6),]},
                  "diff" = {train <- ndf[which(ndf$week>max(ndf$week)-6),]})}
  )
  # cat(algorithm,attDtsNr,bet,fulDiff,dfCategory,predAtt,"\n")
  
  ho = attDtsFunc(attDtsNr,bet,fulDiff,predAtt)
  # print(ho)
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

#@ TODO implement the attdataset for header, 2p, 1p, totFt, totHt,
attDtsFunc <- function(attDtsNr,bet,fullDiff,predAtt){
  # unifies the distinct functions for the att_dts_Nr .
  # print("attDtsFunc")
  
  switch (predAtt,
    "head" = {switch(fullDiff,
                     full={switch (bet,
                                   "yes" = return (fullHeadBet(attDtsNr)),
                                   "no" = return (fullHeadNoBet(attDtsNr))
                     )},
                     diff={switch (bet,
                                   "yes" = return (differencedHeadBet(attDtsNr)),
                                   "no" = return (differencedHeadNoBet(attDtsNr))
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
    "p2" = {switch(fullDiff,
                   full={switch (bet,
                                 "yes" = return (full2pBet(attDtsNr)),
                                 "no" = return (full2pNoBet(attDtsNr))
                   )},
                   diff={switch (bet,
                                 "yes" = return (differenced2pBet(attDtsNr)),
                                 "no" = return (differenced2pNoBet(attDtsNr))
                   )})
    },
    "p1" = {switch(fullDiff,
                   full={switch (bet,
                                 "yes" = return (full1pBet(attDtsNr)),
                                 "no" = return (full1pNoBet(attDtsNr))
                   )},
                   diff={switch (bet,
                                 "yes" = return (differenced1pBet(attDtsNr)),
                                 "no" = return (differenced1pNoBet(attDtsNr))
                   )})
    },
    "totFt" = {switch(fullDiff,
                      full={switch (bet,
                                    "yes" = return (fullTotFtBet(attDtsNr)),
                                    "no" = return (fullTotFtNoBet(attDtsNr))
                      )},
                      diff={switch (bet,
                                    "yes" = return (differencedTotFtBet(attDtsNr)),
                                    "no" = return (differencedTotFtNoBet(attDtsNr))
                      )})
    },
    "totHt" = {switch(fullDiff,
                      full={switch (bet,
                                    "yes" = return (fullTotHtBet(attDtsNr)),
                                    "no" = return (fullTotHtNoBet(attDtsNr))
                      )},
                      diff={switch (bet,
                                    "yes" = return (differencedTotHtBet(attDtsNr)),
                                    "no" = return (differencedTotHtNoBet(attDtsNr))
                      )})
    }
  )
}



#####---------- XXXXXXResultCount__ series of func to summ accuracy for each prediction

scoreResultCount <- function(pv,acc){
  # print("Score Result Counter")
  # cat(length(pv),"  --  ",pv,"\n")
  temp_mat <- matrix(nrow = 2,ncol = length(pv),data = 0)
  for(i in 1: length(pv)){
    if(pv[i]=="O"){temp_mat[1,i]<-acc}
    else if(pv[i]=="U"){temp_mat[2,i]<-acc}
  }
  return (temp_mat)
}

headResultCount <- function(pv,acc){
  temp_mat <- matrix(nrow = 3,ncol = length(pv),data = 0)
  for(i in 1:length(pv)){
    if(pv[i]=="1"){temp_mat[1,i]<-acc}
    else if(pv[i]=="X"){temp_mat[2,i]<-acc}
    else if(pv[i]=="2"){temp_mat[3,i]<-acc}
  }
  return (temp_mat)
}

p2ResultCount <- function(pv,acc){
  temp_mat <- matrix(nrow = 2,ncol = length(pv),data = 0)
  for(i in 1:length(pv)){
    if(pv[i]=="Y"){temp_mat[1,i]<-acc}
    else if(pv[i]=="N"){temp_mat[2,i]<-acc}
  }
  return (temp_mat)
}

p1ResultCount <- function(pv,acc){
  temp_mat <- matrix(nrow = 2,ncol = length(pv),data = 0)
  for(i in 1:length(pv)){
    if(pv[i]=="Y"){temp_mat[1,i]<-acc}
    else if(pv[i]=="N"){temp_mat[2,i]<-acc}
  }
  return (temp_mat)
}

totFtResultCount <- function(pv,acc){
  temp_mat <- matrix(nrow = 6,ncol = length(pv),data = 0)
  for(i in 1:length(pv)){
    if(pv[i] <0.5)                  {temp_mat[1,i]<-acc}
    else if(pv[i]>=0.6 && pv[i]<1.5 ){temp_mat[2,i]<-acc}
    else if(pv[i]>=1.6 && pv[i]<2.5 ){temp_mat[3,i]<-acc}
    else if(pv[i]>=2.6 && pv[i]<3.5 ){temp_mat[4,i]<-acc}
    else if(pv[i]>=3.6 && pv[i]<4.5 ){temp_mat[5,i]<-acc}
    else if(pv[i]>=4.6 )            {temp_mat[6,i]<-acc}
  }
  return (temp_mat)
}

totHtResultCount <- function(pv,acc){
  temp_mat <- matrix(nrow = 6,ncol = length(pv),data = 0)
  for(i in 1:length(pv)){
    if(pv[i] <0.5)                  {temp_mat[1,i]<-acc}
    else if(pv[i]>=0.6 && pv[i]<1.5 ){temp_mat[2,i]<-acc}
    else if(pv[i]>=1.6 && pv[i]<2.5 ){temp_mat[3,i]<-acc}
    else if(pv[i]>=2.6 && pv[i]<3.5 ){temp_mat[4,i]<-acc}
    else if(pv[i]>=3.6 && pv[i]<4.5 ){temp_mat[5,i]<-acc}
    else if(pv[i]>=4.6 )            {temp_mat[6,i]<-acc}
  }
  return (temp_mat)
}


#--===============================-USAGE
pf <- AlgoData$new(dtfCategory="c")
pf$dtfCategory<-"cc"

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
