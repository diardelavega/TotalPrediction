# Re-evaluation. After we have the actual results we can confront them with the prediction and alter the accuracy value acoardingly 

reEvalAll <- function(dtfPaths, testPaths){
  
  for(i in 1:length( dtfPaths)){
    print(dtfPaths[i]);
    load(dtfPaths[i])
    
    
    tt <<- read.csv(testPaths[i])  #test dataset/weekly matches

        
    
    #  cal dtf objs and recalcualte their accuracy based on previous prediction
    print("------------------------------------: HEAD")
    hDtf$accuracyRecalc(tt$headOutcome)
    print("------------------------------------: SCORE")
    csDtf$predCalcScore(tt$scoreOutcome);
    print("------------------------------------: P1")
    p1Dtf$predCalcScore(tt$ht1pOutcome);
    print("------------------------------------: P2")
    p2Dtf$predCalcScore(tt$ht2pOutcome);
    print("------------------------------------: HT")
    thtDtf$predCalcScore(tt$totHtScore);
    print("------------------------------------: FT")
    tftDtf$predCalcScorett$totFtScore();
    
    
    # after the DTH objs have been updated with the new Accuracy in each instance save them again 
    save(hDtf,csDtf,p1Dtf,p2Dtf,tftDtf,thtDtf,file=dtfPaths[i]);
    dtfobjcleaner();
    
  }
  
}