fullTotHtBet <- function(i){
  if (i==-1){return (4)}
  
  else if (i==1){
    return(totHtScore~  owd+  old+ odd+  mfd1+ mfd2+
             # t1+
             # t1Points+        t1Form+
             t1Classification+
             t1Form1Diff+      t1Form2Diff+      t1Form3Diff+      t1Form4Diff+
             t1Atack+          t1Defense+
             t1AtackIn+        t1AtackOut+       t1DefenseIn+      t1DefenseOut+
             t1AvgHtScoreIn+   t1AvgHtScoreOut+
             t1AvgFtScoreIn+   t1AvgFtScoreOut+  
             t1AvgHtGgResult+  t1AvgFtGgResult+
             t1WinsIn+         t1WinsOut+        t1DrawsIn+        t1DrawsOut+       t1LosesIn+        t1LosesOut+
             #  t2+
             # t2Points+       t2Form+
             t2Classification+
             t2Form1Diff+      t2Form2Diff+      t2Form3Diff+      t2Form4Diff+
             t2Atack+          t2Defense+
             t2AtackIn+        t2AtackOut+       t2DefenseIn+      t2DefenseOut+
             t2AvgHtScoreIn+   t2AvgHtScoreOut+
             t2AvgFtScoreIn+   t2AvgFtScoreOut+
             t2AvgHtGgResult+  t2AvgFtGgResult+
             t2WinsIn+         t2WinsOut+        t2DrawsIn+        t2DrawsOut+       t2LosesIn+        t2LosesOut+
             # bet_1+            bet_X+            bet_2
             bet_O+            bet_U
           
    )
  }
  
  else if (i==2){
    return(
      totHtScore~  
        # owd+  old+ odd+
        # mfd1+ mfd2+
        # t1+
        t1Points+        t1Form+
        t1Classification+
        t1Form1Diff+      t1Form2Diff+      t1Form3Diff+      t1Form4Diff+
        t1Atack+          t1Defense+
        # t1AtackIn+        t1AtackOut+       t1DefenseIn+      t1DefenseOut+
        t1AvgHtScoreIn+   t1AvgHtScoreOut+
        t1AvgFtScoreIn+   t1AvgFtScoreOut+  
        # t1AvgHtGgResult+  t1AvgFtGgResult+
        # t1WinsIn+         t1WinsOut+        t1DrawsIn+        t1DrawsOut+       t1LosesIn+        t1LosesOut+
        #  t2+
        t2Points+       t2Form+
        t2Classification+
        t2Form1Diff+      t2Form2Diff+      t2Form3Diff+      t2Form4Diff+
        t2Atack+          t2Defense+
        # t2AtackIn+        t2AtackOut+       t2DefenseIn+      t2DefenseOut+
        t2AvgHtScoreIn+   t2AvgHtScoreOut+
        t2AvgFtScoreIn+   t2AvgFtScoreOut+
        # t2AvgHtGgResult+  t2AvgFtGgResult+
        # bet_1+            bet_X+            bet_2
        bet_O+            bet_U
    )
  }
  
  else if (i==3){
    return(totHtScore~  
             # owd+  old+ odd+
             # mfd1+ mfd2+
             # t1+
             t1Points+        t1Form+
             t1Classification+
             t1Form1Diff+      t1Form2Diff+      #t1Form3Diff+      t1Form4Diff+
             t1Atack+          t1Defense+
             # t1AtackIn+        t1AtackOut+       t1DefenseIn+      t1DefenseOut+
             # t1AvgHtScoreIn+   t1AvgHtScoreOut+
             t1AvgFtScoreIn+   t1AvgFtScoreOut+  
             # t1AvgHtGgResult+  t1AvgFtGgResult+
             # t1WinsIn+         t1WinsOut+        t1DrawsIn+        t1DrawsOut+       t1LosesIn+        t1LosesOut+
             #  t2+
             t2Points+       t2Form+
             t2Classification+
             t2Form1Diff+      t2Form2Diff+      #t2Form3Diff+      t2Form4Diff+
             t2Atack+          t2Defense+
             # t2AtackIn+        t2AtackOut+       t2DefenseIn+      t2DefenseOut+
             t2AvgHtScoreIn+   t2AvgHtScoreOut+
             # t2AvgFtScoreIn+   t2AvgFtScoreOut+
             # t2AvgHtGgResult+  t2AvgFtGgResult+
             # bet_1+            bet_X+            bet_2
             bet_O+            bet_U
    )
  }
  
  
  else if (i==4){
    return(
      totHtScore~   #t1Form+
        t1Form1Diff+      t1Form2Diff+      #t1Form3Diff+      t1Form4Diff+     
        #t1Atack+          t1Defense+        
        t1AtackIn+        #t1AtackOut+       
        t1DefenseIn+      #t1DefenseOut+     
        t1AvgHtScoreIn+   #t1AvgHtScoreOut+ 
        t1AvgFtScoreIn+   #t1AvgFtScoreOut+
        # t1AvgHtGgResult+  #t1AvgFtGgResult+  
        #t1WinsIn+         t1WinsOut+        t1DrawsIn+        t1DrawsOut+       t1LosesIn+        t1LosesOut+     
        #t2+
        #t2Points+       
        #t2Classification+ 
        #t2Form+ 
        t2Form1Diff+      t2Form2Diff+      #t2Form3Diff+      t2Form4Diff+     
        #t2Atack+          t2Defense+        
        #t2AtackIn+        t2DefenseIn+      
        t2AtackOut+       t2DefenseOut+    
        #t2AvgHtScoreIn+   
        t2AvgHtScoreOut+  
        #t2AvgFtScoreIn+   
        t2AvgFtScoreOut+
        bet_O+            bet_U
    )
  }
}

fullTotHtNoBet <- function(i){
  if(i==-1){return(3)}
  
  else if (i==1){
    return(
      totHtScore~  
        # owd+  old+ odd+
        # mfd1+ mfd2+
        # t1+
        t1Points+        t1Form+
        t1Classification+
        t1Form1Diff+      t1Form2Diff+      t1Form3Diff+      t1Form4Diff+
        t1Atack+          t1Defense+
        t1AtackIn+        t1AtackOut+       t1DefenseIn+      t1DefenseOut+
        t1AvgHtScoreIn+   t1AvgHtScoreOut+
        t1AvgFtScoreIn+   t1AvgFtScoreOut+  
        t1AvgHtGgResult+  t1AvgFtGgResult+
        t1WinsIn+         t1WinsOut+        t1DrawsIn+        t1DrawsOut+       t1LosesIn+        t1LosesOut+
        #  t2+
        t2Points+       t2Form+
        t2Classification+
        t2Form1Diff+      t2Form2Diff+      t2Form3Diff+      t2Form4Diff+
        t2Atack+          t2Defense+
        t2AtackIn+        t2AtackOut+       t2DefenseIn+      t2DefenseOut+
        t2AvgHtScoreIn+   t2AvgHtScoreOut+
        t2AvgFtScoreIn+   t2AvgFtScoreOut+
        t2AvgHtGgResult+  t2AvgFtGgResult+
        t2WinsIn+         t2WinsOut+        t2DrawsIn+        t2DrawsOut+       t2LosesIn+        t2LosesOut
      # bet_1+            bet_X+            bet_2
      # bet_O+            bet_U
    )}
  
  else if (i==2){
    return(
      totHtScore~  
        # owd+  old+ odd+
        # mfd1+ mfd2+
        # t1+
        t1Points+        t1Form+
        t1Classification+
        t1Form1Diff+      t1Form2Diff+      t1Form3Diff+      t1Form4Diff+
        t1Atack+          t1Defense+
        t1AtackIn+        t1AtackOut+       t1DefenseIn+      t1DefenseOut+
        t1AvgHtScoreIn+   t1AvgHtScoreOut+
        t1AvgFtScoreIn+   t1AvgFtScoreOut+  
        t1AvgHtGgResult+  t1AvgFtGgResult+
        # t1WinsIn+         t1WinsOut+        t1DrawsIn+        t1DrawsOut+       t1LosesIn+        t1LosesOut+
        #  t2+
        t2Points+       t2Form+
        t2Classification+
        t2Form1Diff+      t2Form2Diff+      t2Form3Diff+      t2Form4Diff+
        t2Atack+          t2Defense+
        t2AtackIn+        t2AtackOut+       t2DefenseIn+      t2DefenseOut+
        t2AvgHtScoreIn+   t2AvgHtScoreOut+
        t2AvgFtScoreIn+   t2AvgFtScoreOut+
        t2AvgHtGgResult+  t2AvgFtGgResult
      # t2WinsIn+         t2WinsOut+        t2DrawsIn+        t2DrawsOut+       t2LosesIn+        t2LosesOut
      # bet_1+            bet_X+            bet_2
      # bet_O+            bet_U
    )}
  
  else if (i==3){
    return(
      totHtScore~  
        # owd+  old+ odd+
        # mfd1+ mfd2+
        # t1+
        # t1Points+        t1Form+
        t1Classification+
        t1Form1Diff+      t1Form2Diff+      #t1Form3Diff+      t1Form4Diff+
        t1Atack+          t1Defense+
        # t1AtackIn+        t1AtackOut+       t1DefenseIn+      t1DefenseOut+
        # t1AvgHtScoreIn+   t1AvgHtScoreOut+
        t1AvgFtScoreIn+   t1AvgFtScoreOut+  
        # t1AvgHtGgResult+  t1AvgFtGgResult+
        # t1WinsIn+         t1WinsOut+        t1DrawsIn+        t1DrawsOut+       t1LosesIn+        t1LosesOut+
        #  t2+
        # t2Points+       t2Form+
        t2Classification+
        t2Form1Diff+      t2Form2Diff+      #t2Form3Diff+      t2Form4Diff+
        t2Atack+          t2Defense+
        # t2AtackIn+        t2AtackOut+       t2DefenseIn+      t2DefenseOut+
        t2AvgHtScoreIn+   t2AvgHtScoreOut
      # t2AvgFtScoreIn+   t2AvgFtScoreOut+
      # t2AvgHtGgResult+  t2AvgFtGgResult+
      # bet_1+            bet_X+            bet_2
      # bet_O+            bet_U
    )}
  
}

differencedTotHtBet <- function(i){
  if (i==-1){return (4)}
  
  else if(i==1){return(
    totHtScore~ mfd1+      mfd2+     pd+  fd+  
      # t1+ t2+   t1Form+ t2Form+   t1Classification+ t2Classification+
      f1d+ f2d+    f3d+ f4d+
      t1adoe+      t2adoe+      t1e+         t2e+
      owd+         odd+         old+         
      doav_ht+     doav_ft+
      dav_htin+    dav_htout+
      dav_ftin+    dav_ftout+
      doav_ht+   doav_ft+
      dwin+        dwout+       ddin+      ddout+       dlin+        dlout+ 
      datk+        datkin+      datkout+
      ddef+        ddefin+      ddefout+
      # bet_1+       bet_X+       bet_2+
      bet_O+       bet_U
  )}
  
  else if(i==2){return(
    totHtScore~ 
      # pd+  fd+  mfd1+      mfd2+     
      # t1+ t2+   t1Form+ t2Form+   t1Classification+ t2Classification+
      f1d+ f2d+    f3d+ f4d+
      # t1adoe+      t2adoe+      t1e+         t2e+
      owd+         odd+         old+         
      doav_ht+     doav_ft+
      dav_htin+    dav_htout+
      dav_ftin+    dav_ftout+
      doav_ht+   doav_ft+
      dwin+        dwout+       ddin+      ddout+       dlin+        dlout+ 
      datk+        #datkin+      datkout+
      ddef+        #ddefin+      ddefout+
      bet_1+       bet_X+       bet_2+
      bet_O+       bet_U
  )}
  
  else if(i==3){return(
    totHtScore~ 
      # pd+  fd+  mfd1+      mfd2+     
      # t1+ t2+   t1Form+ t2Form+   t1Classification+ t2Classification+
      f1d+ f2d+    f3d+ f4d+
      # t1adoe+      t2adoe+      t1e+         t2e+
      # owd+         odd+         old+         
      doav_ht+     doav_ft+
      dav_htin+    dav_htout+
      dav_ftin+    dav_ftout+
      doav_ht+   doav_ft+
      # dwin+        dwout+       ddin+      ddout+       dlin+        dlout+ 
      # datk+        ddef+        
      datkin+      datkout+
      ddefin+      ddefout+
      bet_1+       bet_X+       bet_2+
      bet_O+       bet_U
  )}
  
  else if(i==4){return(
    totHtScore~ 
      # pd+  fd+  mfd1+      mfd2+     
      # t1+ t2+   t1Form+ t2Form+   t1Classification+ t2Classification+
      f1d+ f2d+    f3d+ f4d+
      # t1adoe+      t2adoe+      t1e+         t2e+
      # owd+         odd+         old+         
      doav_ht+     doav_ft+
      dav_htin+    dav_htout+
      dav_ftin+    dav_ftout+
      doav_ht+   doav_ft+
      # dwin+        dwout+       ddin+      ddout+       dlin+        dlout+ 
      # datk+        ddef+        
      datkin+      datkout+
      ddefin+      ddefout+
      # bet_1+       bet_X+       bet_2+
      bet_O+       bet_U
  )}
}

differencedTotHtNoBet <- function(i){
  if (i==-1){return (3)}
  
  else if(i==1){return(
    totHtScore~ 
      # pd+  fd+  mfd1+      mfd2+     
      # t1+ t2+   t1Form+ t2Form+   t1Classification+ t2Classification+
      f1d+ f2d+    f3d+ f4d+
      # t1adoe+      t2adoe+      t1e+         t2e+
      # owd+         odd+         old+         
      doav_ht+     doav_ft+
      dav_htin+    dav_htout+
      dav_ftin+    dav_ftout+
      doav_ht+   doav_ft+
      # dwin+        dwout+       ddin+      ddout+       dlin+        dlout+ 
      # datk+        ddef+        
      datkin+      datkout+
      ddefin+      ddefout
    # bet_1+       bet_X+       bet_2+
    # bet_O+       bet_U
  )}
  
  else if(i==2){return(
    totHtScore~ 
      # pd+  fd+  mfd1+      mfd2+     
      # t1+ t2+   t1Form+ t2Form+   t1Classification+ t2Classification+
      f1d+ f2d+    f3d+ f4d+
      # t1adoe+      t2adoe+      t1e+         t2e+
      # owd+         odd+         old+         
      doav_ht+     doav_ft+
      dav_htin+    dav_htout+
      dav_ftin+    dav_ftout+
      doav_ht+   doav_ft
  )}
  
  else if(i==3){return(
    totHtScore~ 
      # pd+  fd+  mfd1+      mfd2+
      # t1+ t2+   t1Form+ t2Form+
      # t1Classification+ t2Classification+
      f1d+ f2d+   # f3d+ f4d+
      # t1adoe+      t2adoe+      t1e+         t2e+
      # owd+         odd+         old+         
      doav_ht+     doav_ft+
      dav_htin+    dav_htout+
      # dav_ftin+    dav_ftout+
      doav_ht+   doav_ft
    # dwin+        dwout+       ddin+      ddout+       dlin+        dlout+ 
    # datk+        ddef+        
    # datkin+      datkout+
    # ddefin+      ddefout
    # bet_1+       bet_X+       bet_2+
    # bet_O+       bet_U
  )}
  
}
