args <- commandArgs(TRUE)

trPaths <- eval( parse(text=args[1]) )
dtfKind <- eval( parse(text=args[2]) )
print(trPaths);
print(dtfKind);

os<-Sys.info()["sysname"];  # find the operating system
base<-"/home/user/Git"; 	# the base for the files to load
if(grepl("win",tolower(os))){
	base <- "C:";
}
source(paste0(base,"/TotalPrediction/DTF_Create.R"));
runAll(trPaths,dtfKind);
