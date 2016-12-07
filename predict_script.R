#predictAll <- function(dtfPaths,trainPaths,testPaths,dtfKind){
args <- commandArgs(TRUE)
#a <- as.double(args[1])
#a<-args[1]

dtfPaths <- eval( parse(text=args[1]) )
trainPaths <- eval( parse(text=args[2]) )
testPaths <- eval( parse(text=args[3]) )
dtfKind <- eval( parse(text=args[4]) )


source("C:/TotalPrediction/Predict.R")
predictAll(dtfPaths,trainPaths,testPaths,dtfKind)
