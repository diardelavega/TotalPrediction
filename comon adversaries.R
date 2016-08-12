# Same team adversary
  df <- read.csv("C:/Users/Administrator/Rdata/Championship_50_Data")
  posFinder("Bolton Wanderers","Charlton Athletic",150)
  fase1("Bolton Wanderers","Charlton Athletic",150)
  
  
fase1 <- function(team1,team2, back){
  list1 <- list() # t1 data
  list2 <- list() # t2 data
  back<-100
  # @tt
  if(!is.null(tt)){
    for(i in dim(tt)[1]:1){  # look in the tt dataframe, fills the first position
    if(df[i,"t1"]==team1 ){
      add1 <- adversaryData$new()
      add1$dfPos <- i
      add1$advName <- as.character(df[i,"t2"])
      add1$advAtack <- df[i,"t2Atack"]
      add1$tAtack <- df[i,"t1Atack"]
      add1$advDef<- df[i,"t2Defense"]
      add1$tDef <-df[i,"t1Defense"]
      add1$advForm <- df[i,"t2Form"]
      add1$tForm <- df[i,"t1Form"]
      list1[[1]] <- add1
    }
    else if(df[i,"t2"]==team1 ){
      add1 <- adversaryData$new()
      add1$advName <- as.character(df[i,"t1"])
      add1$dfPos <- i
      add1$advAtack <- df[i,"t1Atack"]
      add1$tAtack <- df[i,"t2Atack"]
      add1$advDef<- df[i,"t1Defense"]
      add1$tDef <-df[i,"t2Defense"]
      add1$advForm <- df[i,"t1Form"]
      add1$tForm <- df[i,"t2Form"]
      list1[[1]] <- add1
    }
   
    if(df[i,"t1"]==team2 ){
      add2 <- adversaryData$new()
      add2$advName <- as.character(df[i,"t2"])
      add2$dfPos <- i
      add2$advAtack <- df[i,"t2Atack"]
      add2$tAtack <- df[i,"t1Atack"]
      add2$advDef<- df[i,"t2Defense"]
      add2$tDef <-df[i,"t1Defense"]
      add2$advForm <- df[i,"t2Form"]
      add2$tForm <- df[i,"t1Form"]
      list2[[1]] <- add2
    }
    else if(df[i,"t2"]==team2 ){
      add2 <- adversaryData$new()
      add2$advName <- as.character(df[i,"t1"])
      add2$dfPos <- i
      add2$advAtack <- df[i,"t1Atack"]
      add2$tAtack <- df[i,"t1Atack"]
      add2$advDef<- df[i,"t2Defense"]
      add2$tDef <-df[i,"t2Defense"]
      add2$advForm <- df[i,"t1Form"]
      add2$tForm <- df[i,"t2Form"]
      list2[[1]] <- add2
    }
    
  }
  }
  
  # @df
  for(i in dim(df)[1]: (dim(df)[1]-back)){
    if(df[i,"t1"]==team1 ){
      add1 <- adversaryData$new()
      add1$dfPos <- i
      add1$advName <- as.character(df[i,"t2"])
      add1$advAtack <- df[i,"t2Atack"]
      add1$tAtack <- df[i,"t1Atack"]
      add1$advDef<- df[i,"t2Defense"]
      add1$tDef <-df[i,"t1Defense"]
      add1$advForm <- df[i,"t2Form"]
      add1$tForm <- df[i,"t1Form"]
      add1$tavgFtScore <- (df[i,"t1AvgFtScoreIn"]+df[i,"t1AvgFtScoreOut"]+df[i,"t2AvgFtScoreIn"]+df[i,"t2AvgFtScoreOut"] )/4
      add1$totFtScore <- df[i,"totFtScore"]
      list1[[length(list1) +1]] <- add1
    }
    else if(df[i,"t2"]==team1 ){
      add1 <- adversaryData$new()
      add1$advName <- as.character(df[i,"t1"])
      add1$dfPos <- i
      add1$advAtack <- df[i,"t1Atack"]
      add1$tAtack <- df[i,"t2Atack"]
      add1$advDef<- df[i,"t1Defense"]
      add1$tDef <-df[i,"t2Defense"]
      add1$advForm <- df[i,"t1Form"]
      add1$tForm <- df[i,"t2Form"]
      add1$tavgFtScore <- (df[i,"t1AvgFtScoreIn"]+df[i,"t1AvgFtScoreOut"]+df[i,"t2AvgFtScoreIn"]+df[i,"t2AvgFtScoreOut"] )/4
      add1$totFtScore <- df[i,"totFtScore"]
      list1[[length(list1) +1]] <- add1
    }
    
    if(df[i,"t1"]==team2 ){
      add2 <- adversaryData$new()
      add2$advName <- as.character(df[i,"t2"])
      add2$dfPos <- i
      add2$advAtack <- df[i,"t2Atack"]
      add2$tAtack <- df[i,"t1Atack"]
      add2$advDef<- df[i,"t2Defense"]
      add2$tDef <-df[i,"t1Defense"]
      add2$advForm <- df[i,"t2Form"]
      add2$tForm <- df[i,"t1Form"]
      add2$tavgFtScore <- (df[i,"t1AvgFtScoreIn"]+df[i,"t1AvgFtScoreOut"]+df[i,"t2AvgFtScoreIn"]+df[i,"t2AvgFtScoreOut"] )/4
      add2$totFtScore <- df[i,"totFtScore"]
      list2[[length(list2) +1]] <- add2
    }
    else if(df[i,"t2"]==team2 ){
      add2 <- adversaryData$new()
      add2$advName <- as.character(df[i,"t1"])
      add2$dfPos <- i
      add2$advAtack <- df[i,"t1Atack"]
      add2$tAtack <- df[i,"t2Atack"]
      add2$advDef<- df[i,"t1Defense"]
      add2$tDef <-df[i,"t2Defense"]
      add2$advForm <- df[i,"t1Form"]
      add2$tForm <- df[i,"t2Form"]
      add2$tavgFtScore <- (df[i,"t1AvgFtScoreIn"]+df[i,"t1AvgFtScoreOut"]+df[i,"t2AvgFtScoreIn"]+df[i,"t2AvgFtScoreOut"] )/4
      add2$totFtScore <- df[i,"totFtScore"]
      list2[[length(list2) +1]] <- add2
    }
  }#for
   
 { #-----------------fase 2
  # compare and find the comon adversaries. First 3 from list1 & 2 from list2.
  # fi found common adversaries set var to false else to true.
  # do the comparison from the list2 as base compare to list 1
  # after the comon adversary teams have been foubd, fill the new form, atack and defence data
  # $ the new data can be found in the dataframe in the next pos of the obj in list 1||2
 }
  # {
  # Find first 3 similar from list 1
  counter=0;
  for(i in 2:length(list1)){ #start from 2 becausethe 1 pos in bouth list is from tt
    if(counter>=3){break}       #first 3 from list1
    for(j in 2:length(list2)){
      if(length(list2[[j]]$common)>0){next} # skip the already paired instances

      if(list1[[i]]$advName==list2[[j]]$advName){
        # cat("for 1-----",i,j,list1[[i]]$advName,"\n")
        list1[[i]]$common <- j # asigne the list position of the coresponding adversary
        list2[[j]]$common <- i # in the other list
        counter <- counter+1
        #------------new vals for the changes
        #if i+1 > length of list -> look in the tt_dataframe  values that is pos 1 of the list
        list1[[i]]$newTForm  <- list1[[i-1]]$tForm
        list1[[i]]$newTAtack <- list1[[i-1]]$tAtack
        list1[[i]]$newTDef   <- list1[[i-1]]$tDef

        list2[[j]]$newTForm  <- list2[[j-1]]$tForm
        list2[[j]]$newTAtack <- list2[[j-1]]$tAtack
        list2[[j]]$newTDef   <- list2[[j-1]]$tDef
      }
    }
  }

  # Find first 2 similar from list 2
  counter=0;
  for(i in 2:length(list2)){
    if(counter>=2){break}      # the other 2 from list2 to complete the 5 common adversaries
    if(length(list2[[i]]$common)>0){next}
    for(j in 2:length(list1)){
      if(length(list1[[j]]$common)>0){next}
      
      if(list2[[i]]$advName==list1[[j]]$advName){
        # cat("for 2----",i,j,list2[[i]]$advName,"\n")
        list2[[i]]$common <- j # asigne the list position of the coresponding adversary
        list1[[j]]$common <- i # in the other list

        counter <- counter+1
        #------------new vals for the changes
        list2[[i]]$newTForm  <- list2[[i-1]]$tForm
        list2[[i]]$newTAtack <- list2[[i-1]]$tAtack
        list2[[i]]$newTDef   <- list2[[i-1]]$tDef

        list1[[j]]$newTForm  <- list1[[j-1]]$tForm
        list1[[j]]$newTAtack <- list1[[j-1]]$tAtack
        list1[[j]]$newTDef   <- list1[[j-1]]$tDef
      }
    }
  }
  l1<<-list1
  l2<<-list2
  #@ returns all values wonted and anwonted  returns 17 vals fix
  return(list(list1,list2))
  # }
}

fase2 <- function(list1,list2){
  #calculate form, atack & defence difference. Keep track of them to get an average in the end
  v1 <- list()
  v2 <- list()
  for(i in 2:length(list1)){
    # browser()
    if(length(list1[[i]]$common)>0){
      md <- matchData$new()
      md$t_aFormDiff<- list1[[i]]$tForm  - list1[[i]]$advForm
      md$t_aAtakDiff<- list1[[i]]$tAtack - abs(list1[[i]]$advDef)
      md$t_aDeffDiff<- abs(list1[[i]]$tDef) - list1[[i]]$advAtack
      
      md$tFormChange <- list1[[i]]$newTForm  - list1[[i]]$tForm
      md$tAtakChange <- list1[[i]]$newTAtack - list1[[i]]$tAtack
      md$tDeffChange <- list1[[i]]$newTDef   - list1[[i]]$tDef
      
      md$t_aAvgFtScore <- list1[[i]]$tavgFtScore
      md$totFtScore    <- list1[[i]]$totFtScore
      v1[length(v1)+1] <- md
      
      pos2 <- list1[[i]]$common # get the pos in list2 of the corresponding same adversary match
      md <- matchData$new()
      md$t_aFormDiff<- list2[[pos2]]$tForm  - list2[[pos2]]$advForm
      md$t_aAtakDiff<- list2[[pos2]]$tAtack - abs(list2[[pos2]]$advDef)
      md$t_aDeffDiff<- abs(list2[[pos2]]$tDef) - list2[[pos2]]$advAtack
      
      md$tFormChange <- list2[[pos2]]$newTForm  - list2[[pos2]]$tForm
      md$tAtakChange <- list2[[pos2]]$newTAtack - list2[[pos2]]$tAtack
      md$tDeffChange <- list2[[pos2]]$newTDef   - list2[[pos2]]$tDef
      
      md$t_aAvgFtScore <- list2[[pos2]]$tavgFtScore
      md$totFtScore    <- list2[[pos2]]$totFtScore
      v2[length(v2)+1] <- md
    }
  }

    return(list(v1,v2))
}

fase4 <- function(v1,v2){
  # fase 4 calculate the average difference and change for all the matches of both teams
  amdt1<<-avgMatchData$new()
  afd<-0; afc <-0
  aad<-0; aac <-0
  add<-0; adc <-0
  atft<-0; aft<-0
  for(i in 1:length(v1)){
    afd= afd + v1[[i]]$t_aFormDiff
    aad= aad + v1[[i]]$t_aAtakDiff
    add= add + v1[[i]]$t_aDeffDiff
    afc= afc + v1[[i]]$tFormChange
    aac= aac + v1[[i]]$tAtakChange
    adc= adc + v1[[i]]$tDeffChange
    atft=atft+ v1[[i]]$t_aAvgFtScore
    aft=aft  + v1[[i]]$totFtScore
  }
  amdt1$avgFormDiff <<-afd/length(v1);  amdt1$avgFormChange <<-afc/length(v1)
  amdt1$avgAtakDiff <<-aad/length(v1);  amdt1$avgAtakChang <<-aac/length(v1)
  amdt1$avgDeffDiff <<-add/length(v1);  amdt1$avgDeffChange <<-adc/length(v1)
  amdt1$avgTemsTotFt<<-atft/length(v1);  amdt1$avgTotFtScore <<-aft/length(v1)
  
  amdt2<<-avgMatchData$new()
  afd<-0; afc <-0
  aad<-0; aac <-0
  add<-0; adc <-0
  add<-0; adc <-0
  atft<-0; aft<-0
  for(i in 1:length(v2)){
    afd= afd + v2[[i]]$t_aFormDiff
    aad= aad + v2[[i]]$t_aAtakDiff
    add= add + v2[[i]]$t_aDeffDiff
    afc= afc + v2[[i]]$tFormChange
    aac= aac + v2[[i]]$tAtakChange
    adc= adc + v2[[i]]$tDeffChange
    atft=atft+ v2[[i]]$t_aAvgFtScore
    aft=aft  + v2[[i]]$totFtScore
  }
  amdt2$avgFormDiff<<-afd/length(v2);  amdt2$avgFormChange <<-afc/length(v2)
  amdt2$avgAtakDiff<<-aad/length(v2);  amdt2$avgAtakChang <<-aac/length(v2)
  amdt2$avgDeffDiff<<-add/length(v2);  amdt2$avgDeffChange <<-adc/length(v2)
  amdt2$avgTemsTotFt<<-atft/length(v2);  amdt2$avgTotFtScore <<-aft/length(v2)
  
  # return(c(amdt1,amdt2))
}

posFinder <- function(team1,team2, back){
  #find the position in th dataframe where t1 & t2 are, untill back nr of weeks from the last week
  #also get the names of the adversary teams for t1 & t2,
  # keep only the five last comon adversaries or as many as possible in back # of weeks
  team1Ft<- NULL; team2Ft<- NULL;
  for(i in dim(df)[1]: (dim(df)[1]-back)){
    if(df[i,"t1"]==team1 && df[i,"t2"]==team2 ){
      team1Ft<- df[i,]
    }
    if(df[i,"t1"]==team1 && df[i,"t2"]==team2 ){
      
    }
  }
  
  
 list1 <- list() # t1 data
 list2 <- list() # t2 data

 rv <-fase1(team1,team2, back)
 
 #--------------------fase 3 
 #calculate form, atack & defence difference. Keep track of them to get an average in the end
 rv2 <- fase2( rv[[1]], rv[[2]])
 # at the end we have v1 & v2 whihc in the same position have the corresponding same adversary match
 # for team1 and team2
 
 
 # fase 4 calculate the average difference and change for all the matches of both teams
 fase4(rv2[[1]], rv2[[2]])
 # @ end we have amdt1 & amdt2
 
 #' @ in the firs pos of the fase1 rets we have the data trom tt/ or the most curent data
 amdt1$newForm    (rv[[1]][[1]]$tForm,        rv[[2]][[1]]$tForm)
 amdt1$newAtack   (rv[[1]][[1]]$tAtack,       rv[[2]][[1]]$tAtack)
 amdt1$newDefence (rv[[1]][[1]]$tDef,         rv[[2]][[1]]$tDef)
 amdt1$newTotFt   (rv[[1]][[1]]$tavgFtScore,  rv[[2]][[1]]$tavgFtScore)
 
 amdt2$newForm   (rv[[2]][[1]]$tForm,       rv[[1]][[1]]$tForm)
 amdt2$newAtack  (rv[[2]][[1]]$tAtack,      rv[[1]][[1]]$tAtack)
 amdt2$newDefence(rv[[2]][[1]]$tDef,        rv[[1]][[1]]$tDef)
 amdt2$newTotFt  (rv[[2]][[1]]$tavgFtScore, rv[[1]][[1]]$tavgFtScore)
 
}# end of function
#@@ fix untested funcs
#@@ find a final combination to express superiority or equality for the teams
#@@ add the instance of the two teams to have been faced before-> as 1/2 of all other data
#@@ the final nr should be comprised of 1/2(the avg, x calc) &  1/2 (from direct prev match)


adversaryData <- setRefClass("adversaryData",
              fields = list(advName="character",advForm="numeric",advAtack="numeric",advDef="numeric",
                            dfPos="numeric", tForm="numeric",tAtack="numeric",tDef="numeric",
                            newTForm="numeric",newTAtack="numeric",newTDef="numeric",
                            common="numeric", tavgFtScore="numeric",totFtScore="numeric")
)

matchData <- setRefClass("matchData",fields = list(
              t_aFormDiff="numeric", tFormChange="numeric", t_aAtakDiff="numeric",tAtakChange="numeric",
              t_aDeffDiff="numeric", tDeffChange="numeric", t_aAvgFtScore="numeric",totFtScore="numeric"
))

avgMatchData <- setRefClass("avgMatchData",fields = list(
                avgFormDiff="numeric", avgFormChange="numeric",avgAtakDiff="numeric",avgAtakChang="numeric",
                avgDeffDiff="numeric",avgDeffChange="numeric",avgTemsTotFt="numeric", avgTotFtScore="numeric",
                newFormDiff="numeric",newAtackDiff="numeric",newDeffDiff="numeric", newTotFtScore="numeric"),
                methods = list(
                  newForm    = function(curForm,advForm){
                    fd <- curForm - advForm
                    newFormDiff <<- fd * avgFormChange / avgFormDiff
                    return(newFormDiff+ curForm)
                  },
                  newAtack   = function(curAtak,advDef){
                    ad <- curAtak - abs(advDef)
                    newAtackDiff <<- ad * avgDeffChange / avgDeffDiff
                    return (newAtackDiff+curAtak)
                  },
                  newDefence = function(curDef,advAtack){
                    dd <- abs(curDef) - advAtack
                    newDeffDiff <<- dd * avgDeffChange / avgDeffDiff
                    return (newDeffDiff+curDef)
                  },
                  newTotFt   = function(avgTotTeam, avgAdvTeam){
                     curAvg<- avgTotTeam + avgAdvTeam / 2
                     newTotFtScore <<- curAvg * avgTotFtScore/avgTemsTotFt
                     return(newTotFtScore)
                  }
                )  
)

