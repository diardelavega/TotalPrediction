# Re-evaluation. After we have the actual results we can confront them with the prediction and alter the accuracy value acoardingly 

reEvalAll <- function(dtfPaths, testPaths){
  library(methods);
  print("methods");
  log <- "C:/BastData/R_LOG";
  write("RE_EVALUATION...", file = log, ncolumns = 10, append = T, sep = ",")
  for(i in 1:length( dtfPaths)){
		
    tryCatch({
      write(c("\t", testPaths[i]), file = log, ncolumns = 10, append = T, sep = ",")	   
       # dtfPaths is a vector with the path of the folder containing the competitions dtf file objects
      
      tt <<- read.csv(testPaths[i])  #test dataset/weekly matches
      
      #  cal dtf objs and recalcualte their accuracy based on previous prediction
      tryCatch({
        fnam=paste0(dtfPaths[i],"/head.dtf.RData");
        if(file.exists(fnam)){
          print("------------------------------------: HEAD")
          hDtf$accuracyRecalc(tt$headOutcome)
          save(hDtf, file=fnam);
          rm(hDtf);
          write("\t HEAD", file = log, ncolumns = 10, append = T, sep = ",")
        }
      })
      
      tryCatch({
        fnam=paste0(dtfPaths[i],"/score.dtf.RData");
        if(file.exists(fnam)){
          print("------------------------------------: SCORE")
          csDtf$predCalcScore(tt$scoreOutcome);
          save(csDtf, file=fnam);
          rm(csDtf);
          write("\t SCORE", file = log, ncolumns = 10, append = T, sep = ",")
        }
      })
      
      tryCatch({
        fnam=paste0(dtfPaths[i],"/p1.dtf.RData");
        if(file.exists(fnam)){
          print("------------------------------------: P1")
          p1Dtf$predCalcScore(tt$ht1pOutcome);
          save(p1Dtf, file=fnam);
          rm(p1Dtf);
          write("\t P1", file = log, ncolumns = 10, append = T, sep = ",")
        }
      })
      
      tryCatch({
        fnam=paste0(dtfPaths[i],"/p2.dtf.RData");
        if(file.exists(fnam)){
          print("------------------------------------: P2")
          p2Dtf$predCalcScore(tt$ht2pOutcome);
          save(p2Dtf, file=fnam);
          rm(p2Dtf);
          write("\t P2", file = log, ncolumns = 10, append = T, sep = ",")
        }
      })
      
      tryCatch({
        fnam=paste0(dtfPaths[i],"/ht.dtf.RData");
        if(file.exists(fnam)){
          print("------------------------------------: HT")
          thtDtf$predCalcScore(tt$totHtScore);
          save(thtDtf, file=fnam);
          rm(thtDtf);
          write("\t HT", file = log, ncolumns = 10, append = T, sep = ",")
        }
      })
      
      tryCatch({
        fnam=paste0(dtfPaths[i],"/ft.dtf.RData");
        if(file.exists(fnam)){
          print("------------------------------------: FT")
          tftDtf$predCalcScorett$totFtScore();
          save(tftDtf, file=fnam);
          rm(tftDtf);
          write("\t  FT", file = log, ncolumns = 10, append = T, sep = ",")
        }
      })
      
      
      # print("------------------------------------: SCORE")
      # csDtf$predCalcScore(tt$scoreOutcome);
      # print("------------------------------------: P1")
      # p1Dtf$predCalcScore(tt$ht1pOutcome);
      # print("------------------------------------: P2")
      # p2Dtf$predCalcScore(tt$ht2pOutcome);
      # print("------------------------------------: HT")
      # thtDtf$predCalcScore(tt$totHtScore);
      # print("------------------------------------: FT")
      # tftDtf$predCalcScorett$totFtScore();
      
      # after the DTH objs have been updated with the new Accuracy in each instance save them again 
      # save(hDtf,csDtf,p1Dtf,p2Dtf,tftDtf,thtDtf,file=dtfPaths[i]);
      # dtfobjcleaner();
    },
      error = function(err) {
        # error handler picks up where error was generated
        print(paste("MY_ERROR:  ",err))
        write(paste(" \t MY_ERROR:  ",err), file = log, ncolumns = 10, append = T, sep = ",")
    }, 
    finally = {
	write(" \t ENDED.....:  ", file = log, ncolumns = 10, append = T, sep = ",")
      # in case of error save whatever can be saved
      # save(hDtf,csDtf,p1Dtf,p2Dtf,tftDtf,thtDtf,file=dtfPaths[i]);
      # dtfobjcleaner();
    }) # END tryCatch
    
    
  }
  
  return("ReEVAL_FUNC_END");
}