fullHeadBet <- function(i){
  if (i==-1){# return size of atts datasets
    return(5)
  }
  
  if (i==1){#all over 40
    return(  headOutcome~ #mfd1+ mfd2+  odd+  old+  owd+
               # t1+               t1Points+         t1Classification+ 
               t1Form+          
               t1Atack+          t1Defense+        
               # t1AtackIn+        t1AtackOut+       t1DefenseIn+      t1DefenseOut+
               # t1AvgHtScoreIn+   t1AvgHtScoreOut+  t1AvgFtScoreIn+   t1AvgFtScoreOut+  
               # t1AvgHtGgResult+  t1AvgFtGgResult+  
               # t1WinsIn+         t1WinsOut+        t1DrawsIn+        t1DrawsOut+       t1LosesIn+        t1LosesOut+
               # t2+               t2Points+         t2Classification+ 
               t2Form+           
               t2Atack+          t2Defense+        
               # t2DefenseIn+      t2DefenseOut+    t2AtackIn+        t2AtackOut+       
               # t2AvgHtScoreIn+   t2AvgHtScoreOut+  t2AvgFtScoreIn+   t2AvgFtScoreOut+  
               # t2AvgHtGgResult+  t2AvgFtGgResult+ 
               # t2WinsIn+         t2WinsOut+        t2DrawsIn+        t2DrawsOut+       t2LosesIn+        t2LosesOut+      
               bet_1+            bet_X+            bet_2)         
  }
  
  else if(i==2){return( headOutcome~ #mfd1+ mfd2+  odd+  old+  owd+
                          # t1+               
                          t1Form+          t1Points+         t1Classification+
                          # t1Atack+          t1Defense+
                          t1AtackIn+        t1AtackOut+       t1DefenseIn+      t1DefenseOut+
                          # t1AvgHtScoreIn+   t1AvgHtScoreOut+  t1AvgFtScoreIn+   t1AvgFtScoreOut+  
                          # t1AvgHtGgResult+  t1AvgFtGgResult+  
                          # t1WinsIn+         t1WinsOut+        t1DrawsIn+        t1DrawsOut+       t1LosesIn+        t1LosesOut+
                          # t2+               
                          t2Form+         t2Points+         t2Classification+
                          # t2Atack+          t2Defense+
                          t2DefenseIn+      t2DefenseOut+    t2AtackIn+        t2AtackOut+
                          # t2AvgHtScoreIn+   t2AvgHtScoreOut+  t2AvgFtScoreIn+   t2AvgFtScoreOut+  
                          # t2AvgHtGgResult+  t2AvgFtGgResult+ 
                          # t2WinsIn+         t2WinsOut+        t2DrawsIn+        t2DrawsOut+       t2LosesIn+        t2LosesOut+      
                          bet_1+            bet_X+            bet_2)}
  
  else if(i==3){return(headOutcome~ #mfd1+ mfd2+  
                         # odd+  old+  owd+
                         t1+
                         t1Form+          t1Points+         t1Classification+
                         t1Form1Diff+      t1Form2Diff+      t1Form3Diff+      t1Form4Diff+
                         t1Atack+          t1Defense+
                         t1AtackIn+        t1AtackOut+       t1DefenseIn+      t1DefenseOut+
                         t1AvgHtScoreIn+   t1AvgHtScoreOut+  t1AvgFtScoreIn+   t1AvgFtScoreOut+
                         # t1AvgHtGgResult+  t1AvgFtGgResult+
                         # t1WinsIn+         t1WinsOut+        t1DrawsIn+        t1DrawsOut+       t1LosesIn+        t1LosesOut+
                         t2+
                         t2Form+           t2Points+         t2Classification+
                         t2Form1Diff+      t2Form2Diff+      t2Form3Diff+      t2Form4Diff+
                         t2Atack+          t2Defense+
                         t2DefenseIn+      t2DefenseOut+    t2AtackIn+        t2AtackOut+
                         t2AvgHtScoreIn+   t2AvgHtScoreOut+  t2AvgFtScoreIn+   t2AvgFtScoreOut+
                         # t2AvgHtGgResult+  t2AvgFtGgResult+
                         # t2WinsIn+         t2WinsOut+        t2DrawsIn+        t2DrawsOut+       t2LosesIn+        t2LosesOut+
                         bet_1+            bet_X+            bet_2+
                         bet_O+            bet_U
  )}
  
  else if(i==4){return(headOutcome~ #mfd1+ mfd2+  
                         # odd+  old+  owd+
                         # t1+
                         t1Points+         t1Classification+ t1Form+
                         # t1Form1Diff+      t1Form2Diff+      #t1Form3Diff+      t1Form4Diff+      
                         t1AtackIn+        #t1AtackOut+        
                         t1DefenseIn+      #t1DefenseOut+     
                         # t1AvgHtScoreIn+   #t1AvgHtScoreOut+ 
                         # t1AvgFtScoreIn+   #t1AvgFtScoreOut+  
                         #t1AvgHtGgResult+  t1AvgFtGgResult+  
                         t1WinsIn+         #t1WinsOut+       
                         t1DrawsIn+        #t1DrawsOut+       
                         t1LosesIn+        #t1LosesOut+       
                         # t2+
                         t2Points+         t2Classification+ t2Form+
                         # t2Form1Diff+      t2Form2Diff+      #t2Form3Diff+      t2Form4Diff+     
                         t2AtackOut+       #t2AtackIn+               
                         t2DefenseOut+     #t2DefenseIn+      
                         # t2AvgHtScoreOut+  #t2AvgHtScoreIn+   
                         # t2AvgFtScoreOut+  #t2AvgFtScoreIn+   
                         #t2AvgHtGgResult+  t2AvgFtGgResult+ 
                         t2WinsOut+        #t2WinsIn+         
                         t2DrawsOut+       #t2DrawsIn+        
                         t2LosesOut+       #t2LosesIn+         t2LosesOut+
                         bet_1+            bet_X+            bet_2)}
  
  else if(i==5){return(headOutcome~ #mfd1+ mfd2+  
                         odd+  old+  owd+
                         t1+
                         t1Form+          #t1Points+          t1Classification+
                         t1Form1Diff+      t1Form2Diff+      #t1Form3Diff+      t1Form4Diff+
                         # t1Atack+          t1Defense+
                         t1AtackIn+        t1AtackOut+       t1DefenseIn+      t1DefenseOut+
                         # t1AvgHtScoreIn+   t1AvgHtScoreOut+  t1AvgFtScoreIn+   t1AvgFtScoreOut+
                         # t1AvgHtGgResult+  t1AvgFtGgResult+
                         t1WinsIn+         t1WinsOut+        t1DrawsIn+        t1DrawsOut+       t1LosesIn+        t1LosesOut+
                         t2+
                         t2Form+           #t2Points+          t2Classification+
                         t2Form1Diff+      t2Form2Diff+      #t2Form3Diff+      t2Form4Diff+
                         # t2Atack+          t2Defense+
                         t2DefenseIn+      t2DefenseOut+    t2AtackIn+        t2AtackOut+
                         # t2AvgHtScoreIn+   t2AvgHtScoreOut+  t2AvgFtScoreIn+   t2AvgFtScoreOut+
                         # t2AvgHtGgResult+  t2AvgFtGgResult+
                         t2WinsIn+         t2WinsOut+        t2DrawsIn+        t2DrawsOut+       t2LosesIn+        t2LosesOut+
                         bet_1+            bet_X+            bet_2)}
}

# 6 - 9 /4
fullHeadNoBet <- function(i){
  if (i==-1){# return size of atts datasets
    return(4)
  }
  
  if(i==1){return(
    f1  <- headOutcome~  #t1+              
      #t1Points+
      t1Classification+ t1Form+          
      # t1Form1Diff+      t1Form2Diff+      t1Form3Diff+      t1Form4Diff+     
      t1Atack+          t1Defense+        
      # t1AtackIn+        t1AtackOut+       t1DefenseIn+      t1DefenseOut+     
      # t1AvgHtScoreIn+   t1AvgHtScoreOut+ 
      t1AvgFtScoreIn+   t1AvgFtScoreOut+  
      #t1AvgHtGgResult+  t1AvgFtGgResult+  
      t1WinsIn+         t1WinsOut+        t1DrawsIn+        t1DrawsOut+       t1LosesIn+        t1LosesOut+
      # t2+
      #t2Points+       
      t2Classification+ t2Form+ 
      # t2Form1Diff+      t2Form2Diff+      t2Form3Diff+      t2Form4Diff+     
      t2Atack+          t2Defense+        
      # t2AtackIn+        t2AtackOut+       t2DefenseIn+      t2DefenseOut+    
      # t2AvgHtScoreIn+   t2AvgHtScoreOut+ 
      t2AvgFtScoreIn+   t2AvgFtScoreOut+
      #t2AvgHtGgResult+  t2AvgFtGgResult+ 
      t2WinsIn+         t2WinsOut+        t2DrawsIn+        t2DrawsOut+       t2LosesIn+        t2LosesOut+
      #bet_1+            bet_X+            bet_2+       
      owd+  old+ odd+  mfd1+ mfd2
  )}
  
  else if(i==2){return(headOutcome~  t1+              
                         #t1Points+
                         t1Classification+ t1Form+          
                         t1Form1Diff+      t1Form2Diff+      t1Form3Diff+      t1Form4Diff+     
                         t1Atack+          t1Defense+        
                         t1AtackIn+        t1AtackOut+       t1DefenseIn+      t1DefenseOut+     
                         t1AvgHtScoreIn+   t1AvgHtScoreOut+ 
                         t1AvgFtScoreIn+   t1AvgFtScoreOut+  
                         #t1AvgHtGgResult+  t1AvgFtGgResult+  
                         #t1WinsIn+         t1WinsOut+        t1DrawsIn+        t1DrawsOut+       t1LosesIn+        t1LosesOut+     
                         t2+
                         #t2Points+       
                         t2Classification+ t2Form+ 
                         t2Form1Diff+      t2Form2Diff+      t2Form3Diff+      t2Form4Diff+     
                         t2Atack+          t2Defense+        
                         t2AtackIn+        t2AtackOut+       t2DefenseIn+      t2DefenseOut+    
                         t2AvgHtScoreIn+   t2AvgHtScoreOut+  t2AvgFtScoreIn+   t2AvgFtScoreOut  
  )}
  
  else if(i==3){return(headOutcome~ #mfd1+ mfd2+  
                         # odd+  old+  owd+
                         t1+
                         t1Points+         t1Classification+ t1Form+
                         # t1Form1Diff+      t1Form2Diff+      #t1Form3Diff+      t1Form4Diff+      
                         t1AtackIn+        #t1AtackOut+        
                         t1DefenseIn+      #t1DefenseOut+     
                         t1AvgHtScoreIn+   #t1AvgHtScoreOut+
                         t1AvgFtScoreIn+   #t1AvgFtScoreOut+
                         #t1AvgHtGgResult+  t1AvgFtGgResult+  
                         t1WinsIn+         #t1WinsOut+       
                         t1DrawsIn+        #t1DrawsOut+       
                         t1LosesIn+        #t1LosesOut+       
                         t2+
                         t2Points+         t2Classification+ t2Form+
                         # t2Form1Diff+      t2Form2Diff+      #t2Form3Diff+      t2Form4Diff+     
                         t2AtackOut+       #t2AtackIn+               
                         t2DefenseOut+     #t2DefenseIn+      
                         t2AvgHtScoreOut+  #t2AvgHtScoreIn+
                         t2AvgFtScoreOut+  #t2AvgFtScoreIn+
                         #t2AvgHtGgResult+  t2AvgFtGgResult+ 
                         t2WinsOut+        #t2WinsIn+         
                         t2DrawsOut+       #t2DrawsIn+        
                         t2LosesOut 
  )}
  
  else if(i==4){return(
    f1  <- headOutcome~  t1+              
      #t1Points+
      t1Classification+ t1Form+          
      # t1Form1Diff+      t1Form2Diff+      t1Form3Diff+      t1Form4Diff+     
      t1Atack+          t1Defense+        
      # t1AtackIn+        t1AtackOut+       t1DefenseIn+      t1DefenseOut+     
      # t1AvgHtScoreIn+   t1AvgHtScoreOut+ 
      t1AvgFtScoreIn+   t1AvgFtScoreOut+  
      #t1AvgHtGgResult+  t1AvgFtGgResult+  
      t1WinsIn+         t1WinsOut+        t1DrawsIn+        t1DrawsOut+       t1LosesIn+        t1LosesOut+
      t2+
      #t2Points+       
      t2Classification+ t2Form+ 
      # t2Form1Diff+      t2Form2Diff+      t2Form3Diff+      t2Form4Diff+     
      t2Atack+          t2Defense+        
      # t2AtackIn+        t2AtackOut+       t2DefenseIn+      t2DefenseOut+    
      # t2AvgHtScoreIn+   t2AvgHtScoreOut+ 
      t2AvgFtScoreIn+   t2AvgFtScoreOut+
      #t2AvgHtGgResult+  t2AvgFtGgResult+ 
      t2WinsIn+         t2WinsOut+        t2DrawsIn+        t2DrawsOut+       t2LosesIn+        t2LosesOut+
      #bet_1+            bet_X+            bet_2+       
      owd+  old+ odd+  mfd1+ mfd2
  )}
}

# 1 - 3 nrs /3
differencedHeadBet <- function(i){
  if (i==-1){# return size of atts datasets
    return(3)
  }
  
  if(i==1){return(headOutcome~  t1+t2+ mfd1+      mfd2+    t1Classification+ t2Classification+  pd+  fd+  
                    #f1d+ f2d+ f3d+ f4d+
                    t1adoe+          t2adoe+           
                    t1e+             t2e+
                    owd+         odd+          old+    doav_ht+ doav_ft+
                    dwin+        dwout+       ddin+        ddout+       dlin+        dlout+
                    # datkin+      datkout+     ddefin+      ddefout+   dav_ftin+    dav_ftout+
                    bet_1+       bet_X+       bet_2+ bet_O+       bet_U)}
  
  else if(i==2){return(headOutcome~  #t1+ t2+ 
                         # mfd1+      mfd2+    t1Classification+ t2Classification+  pd+  fd+ f1d+ f2d+
                         #f3d+ f4d+
                         # t1adoe+          t2adoe+           
                         # t1e+             t2e+
                         owd+         odd+          old+    
                         # doav_ht+ doav_ft+
                         # dwin+        dwout+       ddin+        ddout+       dlin+        dlout+    
                         datkin+      datkout+     
                         # ddefin+      ddefout+   
                         # dav_ftin+    dav_ftout+
                         bet_1+       bet_X+       bet_2)}
  
  else if(i==3){return(headOutcome~  #t1+ t2+ 
                         # mfd1+      mfd2+    t1Classification+ t2Classification+  
                         pd+  fd+
                         # f1d+ f2d+ #f3d+ f4d+
                         # t1adoe+          t2adoe+
                         # t1e+             t2e+
                         owd+         odd+          old+
                         # doav_ht+ doav_ft+
                         # dwin+        dwout+       ddin+        ddout+       dlin+        dlout+
                         datkin+      datkout+     
                         # ddefin+      ddefout+   
                         # dav_ftin+    dav_ftout+
                         bet_1+       bet_X+       bet_2)}
  
  else if(i==4){return( headOutcome~  t1+ t2+ 
                          mfd1+      mfd2+    t1Classification+ t2Classification+
                          # pd+  fd+
                          # f1d+ f2d+ #f3d+ f4d+
                          # t1adoe+          t2adoe+
                          t1e+             t2e+
                          owd+         odd+          old+
                          # doav_ht+ doav_ft+
                          # dwin+        dwout+       ddin+        ddout+       dlin+        dlout+
                          datkin+      datkout+     
                          # ddefin+      ddefout+   
                          # dav_ftin+    dav_ftout+
                          bet_1+       bet_X+       bet_2#+ bet_O+       bet_U
  )}
  
  else if(i==5){return( headOutcome~  #t1+ t2+ 
                          #mfd1+      mfd2+    t1Classification+ t2Classification+
                          # pd+  fd+
                          f1d+ f2d+ #f3d+ f4d+
                          t1adoe+          t2adoe+
                          # t1e+             t2e+
                          # owd+         odd+          old+
                          # doav_ht+ doav_ft+
                          # dwin+        dwout+       ddin+        ddout+       dlin+        dlout+
                          # datkin+      datkout+     
                          # ddefin+      ddefout+   
                          # dav_ftin+    dav_ftout+
                          bet_1+       bet_X+       bet_2#+ bet_O+       bet_U
  )}
}

# 4 - 7 nrs /4
differencedHeadNoBet <- function(i){
  if (i==-1){# return size of atts datasets
    return(4)
  } 
  #56.21212
  if(i==1){return( headOutcome~#t1+t2+  
                     mfd1+      mfd2+    #
                     #t1Classification+ t2Classification+ 
                     pd+  fd+
                     #f1d+ f2d+ f3d+ f4d+
                     t1adoe+          t2adoe+
                     # t1e+             t2e+
                     owd+         odd+          old+    
                     # doav_ht+ doav_ft+
                     # dwin+        dwout+       ddin+        ddout+       dlin+        dlout+
                     
                     datkin+      datkout+     
                     ddefin+      ddefout
                   
  )}
  
  # not so good results
  else if(i==2){return(headOutcome~#t1+t2+  
                         mfd1+      mfd2+    #
                         #t1Classification+ t2Classification+ 
                         # pd+  fd+
                         #f1d+ f2d+ f3d+ f4d+
                         t1adoe+          t2adoe+
                         # t1e+             t2e+
                         # owd+         odd+          old+    
                         # doav_ht+ doav_ft+
                         # dwin+        dwout+       ddin+        ddout+       dlin+        dlout+
                         
                         datkin+      datkout+     
                         ddefin+      ddefout+
                         dav_ftin+    dav_ftout
  )}
  
  #midle algo -> higher accuracy
  else if(i==3){return(headOutcome~#t1+t2+  
                         mfd1+      mfd2+    #
                         # t1Classification+ t2Classification+
                         pd+  fd+
                         # f1d+ f2d+ #f3d+ f4d+
                         t1adoe+          t2adoe+
                         t1e+             t2e+
                         # owd+         odd+          old+
                         # doav_ht+ doav_ft+
                         dwin+        dwout+       ddin+        ddout+       dlin+        dlout
                       
  )}
  
  else if(i==4){return(headOutcome~t1+t2+  
                         mfd1+      mfd2+    #
                         # t1Classification+ t2Classification+
                         pd+  fd+
                         # f1d+ f2d+ #f3d+ f4d+
                         t1adoe+          t2adoe+
                         t1e+             t2e+
                         owd+         odd+          old+
                         # doav_ht+ doav_ft+
                         dwin+        dwout+       ddin+        ddout+       dlin+        dlout
  )}
}
