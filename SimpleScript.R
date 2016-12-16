# a script to read from dtf files of cometitions the other than Ensamle  groups of prediction data

args <- commandArgs(TRUE)

dtfPaths <- eval( parse(text=args[1]) )
print(args[1])

callFunc<- args[2]
print(callFunc)

# getEnsamlem  getF,   getF2,   getF5,  getBet,   getNoBet,   getFull ,   getDiff ,


call <- "";
switch(callFunc,
	"E"={call<-"getEnsamle()"},#ensamle
	"F"={call<-"getF()"},#full
	"F2"={call<-"getF2()"},#full
	"F5"={call<-"getF5()"},#full
	"B"={call<-"getBet()"},#bet
	"NB"={call<-"getNoBet()"},#noBet
	"FULL"={call<-"getFull()"},#full
	"DIFF"={call<-"getDiff()"},#differenced
	
	"e"={call<-"getEnsamle()"},#ensamle
	"f"={call<-"getF()"},#full
	"f2"={call<-"getF2()"},#full
	"f5"={call<-"getF5()"},#full
	"b"={call<-"getBet()"},#bet
	"nb"={call<-"getNoBet()"},#noBet
	"full"={call<-"getFull()"},#full
	"diff"={call<-"getDiff()"},#differenced
)

library(methods);
print("methods");
source("C:/TotalPrediction/dataStructure.R");
print("dataStructure loader");
for(i in 1:length( dtfPaths)){
			
	 tryCatch({
        fnam=paste0(dtfPaths[i],"/head.dtf.RData");
        print("------------------------------------: HEAD")
		load(fnam)
		print(fnam)  
		switch(callFunc,
			"e"={print(hDtf$getEnsamle())},#ensamle
			"f"={print(hDtf$getF())},#full
			"f2"={print(hDtf$getF2())},#full
			"f5"={print(hDtf$getF5())},#full
			"b"={print(hDtf$getBet())},#bet
			"nb"={print(hDtf$getNoBet())},#noBet
			"full"={print(hDtf$getFull())},#full
			"diff"={print(hDtf$getDiff())},#differenced
		  )
      })
      
      tryCatch({
        fnam=paste0(dtfPaths[i],"/score.dtf.RData");
          print("------------------------------------: SCORE")
		load(fnam)
		print(fnam)  
		switch(callFunc,
			"e"=	{print(csDtf$getEnsamle())},#ensamle
			"f"=	{print(csDtf$getF())},#full
			"f2"=	{print(csDtf$getF2())},#full
			"f5"=	{print(csDtf$getF5())},#full
			"b"=	{print(csDtf$getBet())},#bet
			"nb"=	{print(csDtf$getNoBet())},#noBet
			"full"=	{print(csDtf$getFull())},#full
			"diff"=	{print(csDtf$getDiff())},#differenced
		  )
      })
      
      tryCatch({
        fnam=paste0(dtfPaths[i],"/p1.dtf.RData");
          print("------------------------------------: P1")
		load(fnam)
		print(fnam)  
		switch(callFunc,
			"e"=	{print(p1Dtf$getEnsamle())},#ensamle
			"f"=	{print(p1Dtf$getF())},#full
			"f2"=	{print(p1Dtf$getF2())},#full
			"f5"=	{print(p1Dtf$getF5())},#full
			"b"=	{print(p1Dtf$getBet())},#bet
			"nb"=	{print(p1Dtf$getNoBet())},#noBet
			"full"=	{print(p1Dtf$getFull())},#full
			"diff"=	{print(p1Dtf$getDiff())},#differenced
		  )
      })
      
      tryCatch({
       fnam=paste0(dtfPaths[i],"/p2.dtf.RData");
        print("------------------------------------: P2")
		load(fnam)
		print(fnam)  
		switch(callFunc,
			"e"=	{print(p2Dtf$getEnsamle())},#ensamle
			"f"=	{print(p2Dtf$getF())},#full
			"f2"=	{print(p2Dtf$getF2())},#full
			"f5"=	{print(p2Dtf$getF5())},#full
			"b"=	{print(p2Dtf$getBet())},#bet
			"nb"=	{print(p2Dtf$getNoBet())},#noBet
			"full"=	{print(p2Dtf$getFull())},#full
			"diff"=	{print(p2Dtf$getDiff())},#differenced
		  )
      })
      
      tryCatch({
        fnam=paste0(dtfPaths[i],"/ht.dtf.RData");
        print("------------------------------------: HT")
		load(fnam)
		print(fnam)  
		switch(callFunc,
			"e"=	{print(thtDtf$getEnsamle())},#ensamle
			"f"=	{print(thtDtf$getF())},#full
			"f2"=	{print(thtDtf$getF2())},#full
			"f5"=	{print(thtDtf$getF5())},#full
			"b"=	{print(thtDtf$getBet())},#bet
			"nb"=	{print(thtDtf$getNoBet())},#noBet
			"full"=	{print(thtDtf$getFull())},#full
			"diff"=	{print(thtDtf$getDiff())},#differenced
		  )
      })
      
      tryCatch({
        fnam=paste0(dtfPaths[i],"/ft.dtf.RData");
        print("------------------------------------: FT")
		load(fnam)
		print(fnam)  
		switch(callFunc,
			"e"=	{print(tftDtf$getEnsamle())},#ensamle
			"f"=	{print(tftDtf$getF())},#full
			"f2"=	{print(tftDtf$getF2())},#full
			"f5"=	{print(tftDtf$getF5())},#full
			"b"=	{print(tftDtf$getBet())},#bet
			"nb"=	{print(tftDtf$getNoBet())},#noBet
			"full"=	{print(tftDtf$getFull())},#full
			"diff"=	{print(tftDtf$getDiff())},#differenced
		  )
      })
      
      
}		
		
#Rscript.exe --no-save --no-restore reEval_script.R "c('C:/BastData/DTF/France/Ligue_1__60')" "c('C:/BastData/Pred/Data/France/Ligue_1__60__Data')" "c('C:/BastData/Pred/Test/France/Ligue_1__60__Test__2016-12-15')"
#Rscript.exe --no-save --no-restore SimpleScript.R "c('C:/BastData/DTF/France/Ligue_1__60')" 






