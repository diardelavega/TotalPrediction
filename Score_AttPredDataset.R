fullScoreBet <- function(i){
  if (i==-1){# return size of atts datasets
    return(6)
  }
  
  if (i==1){
    return( scoreOutcome~ 
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
  
  else if(i==2){return(scoreOutcome~ 
                         t1+               t1Form+           #t1Points+         #t1Classification+
                         t1Atack+          t1Defense+        
                         t1AvgHtScoreIn+   t1AvgFtScoreIn+   t1AvgHtGgResult+  t1AvgFtGgResult+  
                         t1WinsIn+         t1DrawsIn+        t1LosesIn+
                         t2+               t2Form+            #t2Points+         #t2Classification+
                         t2Atack+          t2Defense+        
                         t2AvgHtScoreOut+  t2AvgFtScoreOut+  t2AvgHtGgResult+  t2AvgFtGgResult+ 
                         t2WinsOut+        t2DrawsOut+       t2LosesOut+
                         bet_O+            bet_U)}
  
  else if(i==3){return(scoreOutcome~#week+  
                         t1+               
                         #t1Points+         t1Classification+ t1Form+          
                         t1Form1Diff+      t1Form2Diff+      t1Form3Diff+      t1Form4Diff+      
                         t1AtackIn+        #t1AtackOut+        
                         t1DefenseIn+      #t1DefenseOut+     
                         t1AvgHtScoreIn+   #t1AvgHtScoreOut+ 
                         t1AvgFtScoreIn+   #t1AvgFtScoreOut+  
                         #t1AvgHtGgResult+  t1AvgFtGgResult+  
                         t1WinsIn+         #t1WinsOut+       
                         t1DrawsIn+        #t1DrawsOut+       
                         t1LosesIn+        #t1LosesOut+       
                         t2+               
                         #t2Points+         t2Classification+ t2Form+           
                         t2Form1Diff+      t2Form2Diff+      t2Form3Diff+      t2Form4Diff+     
                         t2AtackOut+       #t2AtackIn+               
                         t2DefenseOut+     #t2DefenseIn+      
                         t2AvgHtScoreOut+  #t2AvgHtScoreIn+   
                         t2AvgFtScoreOut+  #t2AvgFtScoreIn+   
                         #t2AvgHtGgResult+  t2AvgFtGgResult+ 
                         t2WinsOut+        #t2WinsIn+         
                         t2DrawsOut+       #t2DrawsIn+        
                         t2LosesOut+       #t2LosesIn+        
                         bet_O+            bet_U)}
  
  else if(i==4){return(scoreOutcome~  
                         #owd+  old+ odd+  
                         # mfd1+ mfd2+
                         t1+
                         t1Points+             #t1Classification+
                         # t1Form+
                         t1Form1Diff+      t1Form2Diff+      t1Form3Diff+      t1Form4Diff+
                         # t1Atack+          t1Defense+
                         # t1AtackIn+        t1AtackOut+       t1DefenseIn+      t1DefenseOut+
                         # t1AvgHtScoreIn+   t1AvgHtScoreOut+
                         # t1AvgFtScoreIn+   t1AvgFtScoreOut+  
                         # t1AvgHtGgResult+  t1AvgFtGgResult+
                         # t1WinsIn+         t1WinsOut+        t1DrawsIn+        t1DrawsOut+       t1LosesIn+        t1LosesOut+
                         t2+
                         # t2Classification+
                         t2Points+
                         t2Form+
                         t2Form1Diff+      t2Form2Diff+      t2Form3Diff+      t2Form4Diff+
                         # t2Atack+          t2Defense+
                         # t2AtackIn+        t2AtackOut+       t2DefenseIn+      t2DefenseOut+
                         # t2AvgHtScoreIn+
                         # t2AvgHtScoreOut+
                         # t2AvgFtScoreIn+
                         # t2AvgFtScoreOut
                         # t2AvgHtGgResult+  t2AvgFtGgResult+
                         # t2WinsIn+         t2WinsOut+        t2DrawsIn+        t2DrawsOut+       t2LosesIn+        t2LosesOut
                         # bet_1+            bet_X+            bet_2+
                         bet_O+            bet_U)}
  
  else if(i==5){return(scoreOutcome~  #owd+  old+ odd+  
                         mfd1+ mfd2+
                         t1+
                         # t1Points+            # t1Classification+
                         t1Form+
                         # t1Form1Diff+      t1Form2Diff+      #t1Form3Diff+      t1Form4Diff+
                         # t1Atack+          t1Defense+
                         # t1AtackIn+        t1AtackOut+       t1DefenseIn+      t1DefenseOut+
                         # t1AvgHtScoreIn+   t1AvgHtScoreOut+
                         t1AvgFtScoreIn+   t1AvgFtScoreOut+  
                         # t1AvgHtGgResult+  t1AvgFtGgResult+
                         # t1WinsIn+         t1WinsOut+        t1DrawsIn+        t1DrawsOut+       t1LosesIn+        t1LosesOut+
                         t2+     # t2Classification+
                         # t2Points+       
                         t2Form+
                         # t2Form1Diff+      t2Form2Diff+      #t2Form3Diff+      t2Form4Diff+
                         # t2Atack+          t2Defense+
                         # t2AtackIn+        t2AtackOut+       t2DefenseIn+      t2DefenseOut+
                         # t2AvgHtScoreIn+
                         # t2AvgHtScoreOut+
                         t2AvgFtScoreIn+
                         t2AvgFtScoreOut+
                         # t2AvgHtGgResult+  t2AvgFtGgResult+
                         # t2WinsIn+         t2WinsOut+        t2DrawsIn+        t2DrawsOut+       t2LosesIn+        t2LosesOut+
                         # bet_1+            bet_X+            bet_2+
                         bet_O+            bet_U)}
  
  else if(i==6){return(scoreOutcome~  #owd+  old+ odd+  
                         mfd1+ mfd2+
                         t1+
                         # t1Points+            # t1Classification+
                         # t1Form+
                         # t1Form1Diff+      t1Form2Diff+      #t1Form3Diff+      t1Form4Diff+
                         # t1Atack+          t1Defense+
                         # t1AtackIn+        t1AtackOut+       t1DefenseIn+      t1DefenseOut+
                         # t1AvgHtScoreIn+   t1AvgHtScoreOut+
                         t1AvgFtScoreIn+   t1AvgFtScoreOut+  
                         # t1AvgHtGgResult+  t1AvgFtGgResult+
                         # t1WinsIn+         t1WinsOut+        t1DrawsIn+        t1DrawsOut+       t1LosesIn+        t1LosesOut+
                         t2+     # t2Classification+
                         # t2Points+       
                         # t2Form+
                         # t2Form1Diff+      t2Form2Diff+      #t2Form3Diff+      t2Form4Diff+
                         # t2Atack+          t2Defense+
                         # t2AtackIn+        t2AtackOut+       t2DefenseIn+      t2DefenseOut+
                         # t2AvgHtScoreIn+
                         # t2AvgHtScoreOut+
                         t2AvgFtScoreIn+
                         t2AvgFtScoreOut+
                         # t2AvgHtGgResult+  t2AvgFtGgResult+
                         # t2WinsIn+         t2WinsOut+        t2DrawsIn+        t2DrawsOut+       t2LosesIn+        t2LosesOut+
                         # bet_1+            bet_X+            bet_2+
                         bet_O+            bet_U)}
}

fullScoreNoBet <- function(i){
  if (i==-1){# return size of atts datasets
    return(5)
  }
  
  if(i==1){return(scoreOutcome~ 
                    t1+               t1Points+         t1Classification+ t1Form+          
                    t1Atack+          t1AtackIn+        t1AtackOut+       t1Defense+        t1DefenseIn+      t1DefenseOut+
                    t1AvgHtScoreIn+   t1AvgHtScoreOut+  t1AvgFtScoreIn+   t1AvgFtScoreOut+  t1AvgHtGgResult+  t1AvgFtGgResult+
                    t1WinsIn+         t1WinsOut+        t1DrawsIn+        t1DrawsOut+       t1LosesIn+        t1LosesOut+
                    t2+               t2Points+         t2Classification+ t2Form+           
                    t2Atack+          t2AtackIn+        t2AtackOut+       t2Defense+        t2DefenseIn+      t2DefenseOut+    
                    t2AvgHtScoreIn+   t2AvgHtScoreOut+  t2AvgFtScoreIn+   t2AvgFtScoreOut+  t2AvgHtGgResult+  t2AvgFtGgResult+ 
                    t2WinsIn+         t2WinsOut+        t2DrawsIn+        t2DrawsOut+       t2LosesIn+        t2LosesOut)}
  
  else if(i==2){return(scoreOutcome~ 
                         t1+               t1Form+          
                         t1Atack+          t1Defense+        
                         t1AvgHtScoreIn+   t1AvgFtScoreIn+   t1AvgFtGgResult+  #t1AvgHtGgResult+  
                         t1WinsIn+         t1DrawsIn+        t1LosesIn+
                         t2+               t2Form+           
                         t2Atack+          t2Defense+        
                         t2AvgHtScoreOut+  t2AvgFtScoreOut+  t2AvgFtGgResult+ #t2AvgHtGgResult+  
                         t2WinsOut+        t2DrawsOut+       t2LosesOut
                       #bet_O+            bet_U
  )}
  
  else if(i==2){return(scoreOutcome~  
                         #owd+  old+ odd+  
                         mfd1+ mfd2+
                         t1+
                         # t1Points+             t1Classification+
                         t1Form+
                         # t1Form1Diff+      t1Form2Diff+      t1Form3Diff+      t1Form4Diff+
                         t1Atack+          t1Defense+
                         t1AtackIn+        t1AtackOut+       t1DefenseIn+      t1DefenseOut+
                         t1AvgHtScoreIn+   t1AvgHtScoreOut+
                         t1AvgFtScoreIn+   t1AvgFtScoreOut+  
                         t1AvgHtGgResult+  t1AvgFtGgResult+
                         t1WinsIn+         t1WinsOut+        t1DrawsIn+        t1DrawsOut+       t1LosesIn+        t1LosesOut+
                         # t2+     t2Classification+
                         t2Points+
                         t2Form+
                         # t2Form1Diff+      t2Form2Diff+      t2Form3Diff+      t2Form4Diff+
                         t2Atack+          t2Defense+
                         t2AtackIn+        t2AtackOut+       t2DefenseIn+      t2DefenseOut+
                         t2AvgHtScoreIn+
                         t2AvgHtScoreOut+
                         t2AvgFtScoreIn+
                         t2AvgFtScoreOut+
                         t2AvgHtGgResult+  t2AvgFtGgResult+
                         t2WinsIn+         t2WinsOut+        t2DrawsIn+        t2DrawsOut+       t2LosesIn+        t2LosesOut
  )}
  
  
  else if(i==3){return(scoreOutcome~week+  
                         #t1+               
                         #t1Points+         t1Classification+ 
                         t1Form+          
                         #t1Form1Diff+      t1Form2Diff+      t1Form3Diff+      t1Form4Diff+      
                         t1AtackIn+        #t1AtackOut+        
                         t1DefenseIn+      #t1DefenseOut+     
                         t1AvgHtScoreIn+   #t1AvgHtScoreOut+ 
                         t1AvgFtScoreIn+   #t1AvgFtScoreOut+  
                         #t1AvgHtGgResult+  t1AvgFtGgResult+  
                         t1WinsIn+         #t1WinsOut+       
                         t1DrawsIn+        #t1DrawsOut+       
                         t1LosesIn+        #t1LosesOut+       
                         #t2+               
                         #t2Points+         t2Classification+ 
                         t2Form+           
                         #t2Form1Diff+      t2Form2Diff+      t2Form3Diff+      t2Form4Diff+     
                         t2AtackOut+       #t2AtackIn+               
                         t2DefenseOut+     #t2DefenseIn+      
                         t2AvgHtScoreOut+  #t2AvgHtScoreIn+   
                         t2AvgFtScoreOut+  #t2AvgFtScoreIn+   
                         #t2AvgHtGgResult+  t2AvgFtGgResult+ 
                         t2WinsOut+        #t2WinsIn+         
                         t2DrawsOut+       #t2DrawsIn+        
                         t2LosesOut#+       #t2LosesIn+        
                       #et_O+            bet_U
  )}
  
  else if(i==4){return(scoreOutcome~  t1+              
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
                         t2AvgHtScoreIn+   t2AvgHtScoreOut+  t2AvgFtScoreIn+   t2AvgFtScoreOut+  
                         #t2AvgHtGgResult+  t2AvgFtGgResult+ 
                         #t2WinsIn+         t2WinsOut+        t2DrawsIn+        t2DrawsOut+       t2LosesIn+        t2LosesOut+      
                         #bet_1+            bet_X+            bet_2+       
                         owd+  old+ odd+  mfd1+ mfd2
                       #bet_O+            bet_U
  )}
  
  else if(i==5){return(scoreOutcome~  
                         #owd+  old+ odd+  
                         # mfd1+ mfd2+
                         t1+
                         t1Points+             #t1Classification+
                         # t1Form+
                         t1Form1Diff+      t1Form2Diff+      t1Form3Diff+      t1Form4Diff+
                         # t1Atack+          t1Defense+
                         # t1AtackIn+        t1AtackOut+       t1DefenseIn+      t1DefenseOut+
                         # t1AvgHtScoreIn+   t1AvgHtScoreOut+
                         # t1AvgFtScoreIn+   t1AvgFtScoreOut+  
                         # t1AvgHtGgResult+  t1AvgFtGgResult+
                         # t1WinsIn+         t1WinsOut+        t1DrawsIn+        t1DrawsOut+       t1LosesIn+        t1LosesOut+
                         t2+
                         # t2Classification+
                         t2Points+
                         t2Form+
                         t2Form1Diff+      t2Form2Diff+      t2Form3Diff+      t2Form4Diff
  )}
}

# 5
differencedScoreBet <- function(i){
  if (i==-1){# return size of atts datasets
    return(3)
  }
  
  if(i==1){return(scoreOutcome~  mfd1+      mfd2+    t1Classification+ t2Classification+  pd+  fd+  
                    #f1d+ f2d+ f3d+ f4d+
                    t1adoe+          t2adoe+           t1e+             t2e+
                    owd+         odd+          old+    doav_ht+ doav_ft+
                    dwin+        dwout+       ddin+        ddout+       dlin+        dlout+    
                    datkin+      datkout+     ddefin+      ddefout+   dav_ftin+    dav_ftout+
                    bet_1+       bet_X+       bet_2+ bet_O+       bet_U )}
  
  else if(i==2){return(scoreOutcome~t1+t2+   t1Form+ t2Form+
                         #mfd1+      mfd2+  mfd+  #
                         #t1Classification+ t2Classification+ 
                         #pd+  fd+  
                         #f1d+ f2d+ f3d+ f4d+
                         t1adoe+          t2adoe+           t1e+             t2e+
                         owd+         odd+          old+    
                         doav_ht+ doav_ft+
                         dwin+        dwout+       ddin+        ddout+       dlin+        dlout+ 
                         #datkin+      datkout+     
                         #ddefin+      ddefout+   
                         dav_ftin+    dav_ftout+
                         bet_O+       bet_U)}
  
  else if(i==3){return(scoreOutcome~  mfd1+      mfd2+   # t1Classification+ t2Classification+  
                         pd+  fd+  
                         t1adoe+          t2adoe+           t1e+             t2e+
                         owd+         odd+          old+    doav_ht+ doav_ft+
                         dwin+        dwout+       ddin+        ddout+       dlin+        dlout+ datk+    
                         datkin+      datkout+     ddef+        ddefin+      ddefout+   dav_ftin+    dav_ftout+
                         bet_O+       bet_U)}
  
  else if(i==4){return(scoreOutcome~  t1+  t2+ 
                         #mfd1+      mfd2+    
                         # t1Classification+ t2Classification+
                         # pd+  fd+  
                         f1d+ f2d+ #f3d+ f4d+
                         # t1adoe+          t2adoe+
                         # t1e+             t2e+
                         # owd+         odd+          old+    
                         doav_ht+ 
                         doav_ft+
                         # dwin+        dwout+       ddin+        ddout+       dlin+        dlout+    
                         datkin+      datkout+     ddefin+      ddefout+
                         dav_ftin+    dav_ftout+
                         bet_1+       bet_X+       bet_2+
                         bet_O+       bet_U)}
  
  else if(i==5){return(scoreOutcome~  t1+  t2+ 
                         mfd1+      mfd2+
                         # t1Classification+ t2Classification+
                         # pd+  fd+  
                         # f1d+ f2d+ #f3d+ f4d+
                         # t1adoe+          t2adoe+
                         # t1e+             t2e+
                         # owd+         odd+          old+    
                         doav_ht+ 
                         doav_ft+
                         dwin+        dwout+       ddin+        ddout+       dlin+        dlout+
                         # datkin+      datkout+     ddefin+      ddefout+
                         dav_ftin+    dav_ftout+
                         # bet_1+       bet_X+       bet_2+
                         bet_O+       bet_U)}
  
  
}

# 5
differencedScoreNoBet <- function(i){
  if (i==-1){# return size of atts datasets
    return(5)
  } 
  
  if(i==1){return( scoreOutcome~t1+t2+  
                     #mfd1+      mfd2+    #
                     #t1Classification+ t2Classification+ 
                     pd+  fd+  
                     #f1d+ f2d+ f3d+ f4d+
                     t1adoe+          t2adoe+           
                     t1e+             t2e+
                     owd+         odd+          old+ 
                     
                     doav_ht+     doav_ft+
                     dwin+        dwout+       ddin+        ddout+       dlin+        dlout+ 
                     datk+        ddef+        
                     datkin+      datkout+
                     ddefin+       ddefout+   
                     dav_ftin+    dav_ftout
                   #bet_O+       bet_U
                   
  )}
  
  # not so good results
  else if(i==2){return(scoreOutcome~#t1+t2+  
                         mfd1+      mfd2+    #
                         #t1Classification+ t2Classification+ 
                         #pd+  fd+  
                         #f1d+ f2d+ f3d+ f4d+
                         t1adoe+          t2adoe+           t1e+             t2e+
                         owd+         odd+          old+    
                         doav_ht+ doav_ft+
                         dwin+        dwout+       ddin+        ddout+       dlin+        dlout+ 
                         
                         datkin+      datkout+     
                         ddefin+      ddefout+   
                         dav_ftin+    dav_ftout
                       #bet_O+       bet_U
  )}
  
  #midle algo -> higher accuracy
  else if(i==3){return(scoreOutcome~t1+t2+  
                         #mfd1+      mfd2+  
                         mfd+  #
                         #t1Classification+ t2Classification+ 
                         #pd+  fd+  
                         #f1d+ f2d+ f3d+ f4d+
                         t1adoe+          t2adoe+           
                         #t1e+             t2e+
                         owd+         odd+          old+    
                         doav_ht+ doav_ft+
                         dwin+        dwout+       ddin+        ddout+       dlin+        dlout+ 
                         datkin+      datkout+     
                         ddefin+      ddefout+   
                         dav_ftin+    dav_ftout
                       #bet_O+       bet_U
  )}
  
  else if(i==4){return(scoreOutcome~
                         #t1+t2+  
                         mfd1+      mfd2+  #mfd+  #
                         #t1Classification+ t2Classification+ 
                         #pd+  fd+  
                         #f1d+ f2d+ f3d+ f4d+
                         t1adoe+          t2adoe+           t1e+             t2e+
                         owd+         odd+          old+    
                         doav_ht+ doav_ft+
                         dwin+        dwout+       ddin+        ddout+       dlin+        dlout+ 
                         
                         #datkin+      datkout+     
                         #ddefin+      ddefout+   
                         dav_ftin+    dav_ftout
                       #bet_O+       bet_U
  )}
  
  else if(i==5){return(scoreOutcome~
                         t1+t2+ t1Form+ t2Form+
                         #mfd1+      mfd2+  #mfd+  #
                         t1Classification+ t2Classification+ 
                         #pd+  fd+  
                         f1d+ f2d+ f3d+ f4d+
                         # t1adoe+          t2adoe+
                         t1e+             t2e+
                         # owd+         odd+          old+
                         # doav_ht+ 
                         doav_ft+
                         # dwin+        dwout+       ddin+        ddout+       dlin+        dlout+
                         
                         # datkin+      datkout+
                         ddefin+      ddefout+
                         dav_ftin+    dav_ftout
  )}
}
