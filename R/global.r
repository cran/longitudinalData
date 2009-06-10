#setGenericVerif <- function(x,y){if(!isGeneric(x)){setGeneric(x,y)}else{}}

setGeneric("longData",function(traj,id,time,varName="V",other=list()){standardGeneric("longData")})
setGeneric("as.longData",function(data,id="I1",timeCol=0,timeReal=0,varName="V",other=list()){standardGeneric("as.longData")})
setGeneric("selectSupTrajMinSize",function(object,minSize){standardGeneric("selectSupTrajMinSize")})
setGeneric("partition",function(clusters,nbClusters){standardGeneric("partition")})
#setGeneric("expandPartition",function(object,listId){standardGeneric("expandPartition")})
setGeneric("imputation",function(object,method,partition)standardGeneric("imputation"))
setGeneric("plotTraj",function(x,y,...){standardGeneric("plotTraj")})
setGeneric("plotSubGroups",function(x,y,...){standardGeneric("plotSubGroups")})
#setGeneric("plotAll",function(x,y,...){standardGeneric("plotAll")})
setGeneric("criterion",function(object,partition,method){standardGeneric("criterion")})

