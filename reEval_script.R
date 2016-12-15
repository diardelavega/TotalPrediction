#reEvalAll <- function(dtfPaths, testPaths){

args <- commandArgs(TRUE)

dtfPaths <- eval( parse(text=args[1]) )
trainPaths <- eval( parse(text=args[2]) )
testPaths <- eval( parse(text=args[3]) )
#print(dtfPaths)
#print(testPaths)

source("C:/TotalPrediction/reevaluation.R")
reEvalAll(dtfPaths,trainPaths,testPaths)
