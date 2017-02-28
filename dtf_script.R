args <- commandArgs(TRUE)

trPaths <- eval( parse(text=args[1]) )
dtfKind <- eval( parse(text=args[2]) )

folds <- as.integer(args[3])
betNoBet <- args[4]
fff<- args[5]
print(trPaths);
print(dtfKind);

print(folds);
print(betNoBet);
print(fff);

os<-Sys.info()["sysname"];  # find the operating system
base<-"/home/user/Git"; 	# the base for the files to load
if(grepl("win",tolower(os))){
	base <- "C:";
}
source(paste0(base,"/TotalPrediction/DTF_Create.R"));
runAll(trPaths,dtfKind,folds,betNoBet,fff);
