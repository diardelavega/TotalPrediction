#test script  // to test the r call in different os types

#predictAll <- function(dtfPaths,trainPaths,testPaths,dtfKind){
args <- commandArgs(TRUE)
a <- as.double(args[1])
#a<-args[1]
print(a);
print(a);
print(a);
#dtfPaths <- eval( parse(text=args[1]) )
#trainPaths <- eval( parse(text=args[2]) )
#testPaths <- eval( parse(text=args[3]) )
#dtfKind <- eval( parse(text=args[4]) )
#print(dtfPaths);
#print(trainPaths);
#print(testPaths);
#print(dtfKind);
os<-Sys.info()["sysname"];  # find the operating system
base<-"/home/user/Git"; 	# the base for the files to load
if(grepl("win",tolower(os))){
	base <- "C:";
}
path <-paste0(base,"/TotalPrediction/test2.R");
source(path);
#predictAll(dtfPaths,trainPaths,testPaths,dtfKind);
foo(a);
