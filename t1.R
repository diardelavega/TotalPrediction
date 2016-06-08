
Instance <- setRefClass("Instance",
                        fields = list(algo="character", attsDtsNr="numeric",
                                      accVal="numeric",fullDiff="character",bet="character")
)


DataStore <- setRefClass("DataStore",
              # a field with the new moderated value for the overall crf_validation accuracy value
              fields = list( instList="vector", avgAcc="numeric"),
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
                calcAvgAccuracy = function(){
                  temp =0.0
                  for(j in 1:length(instList)){
                    temp = temp + instList[[j]]$accVal
                  }
                  avgAcc <<- temp/length(instList)
                },
                aboveAvgInstance = function(){
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


pf <- DataStore$new()

a<-  Instance$new(algo = "C50",attsDtsNr=1,accVal=333, bet="yes", fullDiff="full")
pf$instList <- c(a)

for (i in 1:10){
pf$instList[length(pf$instList)+1] <-  Instance$new(algo = "C50",attsDtsNr=1,accVal=i, bet="yes", fullDiff="full") }

pf$betInstance()


