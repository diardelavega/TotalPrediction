args <- commandArgs(TRUE)

trPaths <- eval( parse(text=args[1]) )
dtfKind <- eval( parse(text=args[2]) )
source("C:/TotalPrediction/DTF_Create.R")
runAll(trPaths,dtfKind)