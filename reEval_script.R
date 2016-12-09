#reEvalAll <- function(dtfPaths, testPaths){

args <- commandArgs(TRUE)

dtfPaths <- eval( parse(text=args[1]) )
testPaths <- eval( parse(text=args[2]) )
source("C:/TotalPrediction/reevaluation.R")
reEvalAll(dtfPaths,testPaths)
