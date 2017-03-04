#reEvalAll <- function(dtfPaths, testPaths){

args <- commandArgs(TRUE)

dtfPaths <- eval( parse(text=args[1]) )
trainPaths <- eval( parse(text=args[2]) )
testPaths <- eval( parse(text=args[3]) )
#print(dtfPaths)
#print(testPaths)

os<-Sys.info()["sysname"];  # find the operating system
base<-"/home/user/Git"; 	# the base for the files to load
if(grepl("win",tolower(os))){
	base <- "C:";
}
source(paste0(base,"/TotalPrediction/reevaluation.R"));
reEvalAll(dtfPaths,trainPaths,testPaths);
