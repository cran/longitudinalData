.LongData.quality.criterion <- function(object,partition,method="linearInterpolation"){
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
    return(list(calinski=calinski))
}
setMethod("criterion",
          signature=c(object="LongData",partition="Partition"),
          .LongData.quality.criterion
)



.matrix.quality.criterion <- function(object,partition,method="linearInterpolation"){
    isNA <- is.na(partition["clusters"])
    clusters <- partition["clusters"][!isNA]
    values <- object[!isNA,,drop=FALSE]
    if(any(is.na(values))){values <- imputation(values,clusters,method=method)}else{}
    values <- matrix(as.numeric(values),nrow=nrow(values))       # Il arrive que values soit une matrice d'entier, et ca coincerait...
    cls.attr <- cls.attrib(values,match(clusters,LETTERSletters))
    varBetween <- bcls.matrix(cls.attr$cluster.center,cls.attr$cluster.size,cls.attr$mean)
    varWithin <- wcls.matrix(values,match(clusters,LETTERSletters),cls.attr$cluster.center)
    traceBetween <- sum(diag(varBetween))
    traceWithin <- sum(diag(varWithin))
    calinski <- traceBetween/traceWithin*(length(clusters)-partition@nbClusters)/(partition@nbClusters-1)
    if(is.na(calinski)){calinski<-NaN}
    return(list(calinski=calinski))
}

setMethod("criterion",
          signature=c(object="matrix",partition="Partition"),
          .matrix.quality.criterion
)


.LongData.quality.criterion3 <- function(object,partition,method="linearInterpolation"){
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

    if(nrow(cls.attr[[2]])==partition@nbClusters){
        rayInter <- +Inf
        for(i in 1:partition@nbClusters){
            distTrajI <- function(x){dist(rbind(x,cls.attr[[2]][i,]))}
            rayInter <- min(apply(cls.attr[[2]][-i,,drop=FALSE],1,distTrajI),rayInter)
        }
    }else{
        rayInter <- NaN
    }

    rayIntra <- 0
    for (i in 1:nrow(values)){
      rayIntra <- rayIntra+dist(rbind(values[i,],cls.attr[[2]][as.integer(clusters[i]),]))^2
    }
    rayIntra <- rayIntra/nrow(values)
    ray <- as.numeric(rayInter/rayIntra)

    clsScat <- cls.scatt.data(values,as.integer(clusters))
    davies <- clv.Davies.Bouldin(clsScat,"average","average")

    return(list(calinski=calinski,ray=ray,davies=davies))
}

setMethod("criterion3",
          signature=c(object="LongData",partition="Partition"),
          .LongData.quality.criterion3
)


.matrix.quality.criterion3 <- function(object,partition,method="linearInterpolation"){
    isNA <- is.na(partition["clusters"])
    clusters <- partition["clusters"][!isNA]
    values <- object[!isNA,,drop=FALSE]
    if(any(is.na(values))){values <- imputation(values,clusters,method=method)}else{}
    values <- matrix(as.numeric(values),nrow=nrow(values))       # Il arrive que values soit une matrice d'entier, et ca coincerait...
    cls.attr <- cls.attrib(values,match(clusters,LETTERSletters))
    varBetween <- bcls.matrix(cls.attr$cluster.center,cls.attr$cluster.size,cls.attr$mean)
    varWithin <- wcls.matrix(values,match(clusters,LETTERSletters),cls.attr$cluster.center)
    traceBetween <- sum(diag(varBetween))
    traceWithin <- sum(diag(varWithin))
    calinski <- traceBetween/traceWithin*(length(clusters)-partition@nbClusters)/(partition@nbClusters-1)
    if(is.na(calinski)){calinski<-NaN}

    if(nrow(cls.attr[[2]])==partition@nbClusters){
        rayInter <- +Inf
        for(i in 1:partition@nbClusters){
            distTrajI <- function(x){dist(rbind(x,cls.attr[[2]][i,]))}
            rayInter <- min(apply(cls.attr[[2]][-i,,drop=FALSE],1,distTrajI),rayInter)
        }
    }else{
        rayInter <- NaN
    }

    rayIntra <- 0
    for (i in 1:nrow(values)){
      rayIntra <- rayIntra+dist(rbind(values[i,],cls.attr[[2]][as.integer(clusters[i]),]))^2
    }
    rayIntra <- rayIntra/nrow(values)
    ray <- as.numeric(rayInter/rayIntra)

    clsScat <- cls.scatt.data(values,as.integer(clusters))
    davies <- clv.Davies.Bouldin(clsScat,"average","average")

    return(list(calinski=calinski,ray=ray,davies=davies))
}

setMethod("criterion3",
          signature=c(object="matrix",partition="Partition"),
          .matrix.quality.criterion3
)

