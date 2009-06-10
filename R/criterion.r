.LongData.quality.criterion <- function(object,partition,method){
    isNA <- is.na(partition["clusters"])
    clusters <- partition["clusters"][!isNA]
    values <- object["traj"][!isNA,,drop=FALSE]
    if(any(is.na(values))){values <- imputation(values,clusters,method=method)}else{}
    values <- matrix(as.numeric(values),nrow=nrow(values))       # Il arrive que values soit une matrice d'entier, et ca coincerait...
    cls.attr <- cls.attrib(values,match(clusters,LETTERSletters))
    varBetween <- bcls.matrix(cls.attr$cluster.center,cls.attr$cluster.size,cls.attr$mean)
    varWithin <- wcls.matrix(values,match(clusters,LETTERSletters),cls.attr$cluster.center)
    traceBetween <- sum(diag(varBetween))
    traceWithin <- sum(diag(varWithin))
    calinski <- traceBetween/traceWithin*(length(clusters)-partition@nbClusters)/(partition@nbClusters-1)
    if(is.na(calinski)){calinski<-NaN}
    return(list(varBetween=varBetween,varWithin=varWithin,calinski=calinski))
}
cleanProg(.LongData.quality.criterion,,,1) # LETTERS
setMethod("criterion",
          signature=c(object="LongData",partition="Partition",method="ANY"),
          .LongData.quality.criterion
)


.matrix.quality.criterion <- function(object,partition,method){
    isNA <- is.na(partition["clusters"])
    clusters <- partition["clusters"][!isNA]
    values <- object["traj"][!isNA,,drop=FALSE]
    if(any(is.na(values))){values <- imputation(values,clusters,method=method)}else{}
    values <- matrix(as.numeric(values),nrow=nrow(values))       # Il arrive que values soit une matrice d'entier, et ca coincerait...
    cls.attr <- cls.attrib(values,match(clusters,LETTERSletters))
    varBetween <- bcls.matrix(cls.attr$cluster.center,cls.attr$cluster.size,cls.attr$mean)
    varWithin <- wcls.matrix(values,match(clusters,LETTERSletters),cls.attr$cluster.center)
    traceBetween <- sum(diag(varBetween))
    traceWithin <- sum(diag(varWithin))
    calinski <- traceBetween/traceWithin*(length(clusters)-partition@nbClusters)/(partition@nbClusters-1)
    if(is.na(calinski)){calinski<-NaN}
    return(list(varBetween=varBetween,varWithin=varWithin,calinski=calinski))
}
cleanProg(.matrix.quality.criterion,,,1) # LETTERS
setMethod("criterion",
          signature=c(object="matrix",partition="Partition",method="ANY"),
          .matrix.quality.criterion
)

