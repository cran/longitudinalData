cat("\n####################################################################
########################## Class LongData ##########################
############################# Creation #############################
####################################################################\n")

cat("### Definition ###\n")
.LongData.validity <- function(object){
#    cat("**** validity LongData ****\n")
    if(length(object@id)==0&length(object@time)==0&length(object@varName)==0&length(object@traj)==0){
    }else{
        if(any(c(length(object@id)==0,length(object@time)==0,length(object@varName)==0,length(object@traj)==0))){
            stop("[LongData:validity]: at least one slot is empty")}else{}
        if(any(apply(object@traj,1,function(x)all(is.na(x))))){
            stop("[LongData:validity]: some longData are only missing value")}else{}
        if(length(object@id)!=dim(object@traj)[1]){
            stop("[LongData:validity] : The number of id does not fit with the number of longData")}else{}
        if(length(object@time)!=dim(object@traj)[2]){
            stop("[LongData:validity] : The number of time does not fit with the length of longData")}else{}
        if(length(object@varName)!=1){
            stop("[LongData:validity] : There is not exactly one variable name.")}else{}
        if(any(is.na(object@time))){
            stop("[LongData:validity] : There is some unknow times")}else{}
        if(!identical(object@time,sort(object@time))){
            stop("[LongData:validity] : time is not in the right order")}else{}
        if(any(duplicated(object@time))){
            stop("[LongData:validity]: Some time are duplicate")}else{}
        if(any(is.na(object@id))){
            stop("[LongData:validity]: Some id are NA")}else{}
        if(any(duplicated(object@id))){
            stop("[LongData:validity]: Some id are duplicate")}else{}
        if(any(dimnames(object@traj)[[1]]!=object@id,dimnames(object@traj)[[2]]!=paste(object@varName,object@time,sep=""))){
            stop("[LongData:validity]: dimnames of traj is not correct")}else{}
    }
}
cleanProg(.LongData.validity,,,0)

setClass(
    Class="LongData",
    representation=representation(
        id="character",
        time="numeric",
        varName="character",
        traj="array",
        other="list"
    ),
    prototype=prototype(
        id=character(),
        time=numeric(),
        varName=character(),
        traj=array(dim=c(0,0)),
        other=list()
    ),
    validity=.LongData.validity
)
rm(.LongData.validity)


cat("\n###################################################################
########################## Class LongData #########################
########################### Constructeur ##########################
###################################################################\n")

setMethod("longData",signature=c("missing","missing","missing","missing","missing"),
    function(traj,id,time,varName,other){new("LongData")}
)
setMethod("longData",signature=c("ANY","ANY","ANY","ANY","ANY"),
    function(traj,id,time,varName,other){
        isOnlyNA <- apply(traj,1,function(x)all(is.na(x)))
        if(any(isOnlyNA)){
            warning("[longData:constructor] longData with only NA are exclude")
            traj <- traj[!isOnlyNA,,drop=FALSE]
            id <- id[!isOnlyNA]
        }else{}
        dimnames(traj) <- list(id,paste(varName,time,sep=""))
        new("LongData",id=as.character(id),time=as.numeric(time),varName=varName,traj=traj,other=other)
    }
)

cat("### Conversion d'un data.frame en longData ###\n")

setMethod("as.longData",signature=c("data.frame"),
    function(data,id=data[,1],
             timeCol=2:length(data),
             timeReal=0:(length(timeCol)-1),
             varName="V",
             other=list()
#             timeReal= gsub("(^.+[[:alpha:]])(\\d+(\\.\\d+)?$)","\\2",names(data)[timeCol],perl=T),
 #            varName=gsub("(^.+[[:alpha:]])(\\d+(\\.\\d+)?$)","\\1",names(data)[timeCol[1]],perl=T),
  #            ...
    ){
#        print(data);cat(timeCol);cat(timeReal)
        traj <- as.matrix(data[,timeCol])
        dimnames(traj) <- list(id,paste(varName,timeReal,sep=""))
        return(longData(id=id,time=timeReal,varName=varName,traj=traj))
    }
)
#as.longData(dn2)

setMethod("as.longData",signature=c("matrix"),
    function(data,id=1:nrow(data),
             timeCol=1:ncol(data),
             timeReal=0:(length(timeCol)-1),
             varName="V",
             other=list()
#             timeReal=gsub("(^.+[[:alpha:]])(\\d+(\\.\\d+)?$)","\\2",colnames(data)[timeCol],perl=T),
 #            varName=gsub("(^.+[[:alpha:]])(\\d+(\\.\\d+)?$)","\\1",colnames(data)[timeCol[1]],perl=T),
  #           ...
    ){
#         print(data);cat("\nX",timeReal,"X");cat("\nX",timeCol,"X");cat("\nX",timeReal,"X");cat("\nX",id,"\nX");cat("\nX",varName,"\n")
        traj <- as.matrix(data[,timeCol])
        dimnames(traj) <- list(id,paste(varName,timeReal,sep=""))
        print(traj)
        return(longData(id=id,time=timeReal,varName=varName,traj=traj))
    }
)


#as.longData.ArtificialLongData <- function(data,...){
#   return(as(data,"LongData"))
#}
#cleanProg(as.longData.ArtificialLongData,,,0)



cat("\n####################################################################
########################### Class LongData #########################
############################# Accesseurs ###########################
####################################################################\n")

cat("### Getteur ###\n")
setMethod("[","LongData",
    function(x,i,j,drop){
        switch(EXPR=i,
            "id"={return(x@id)},
            "varName"={return(x@varName)},
            "time"={return(x@time)},
            "traj"={return(x@traj)},
            "other"={return(x@other)},
            stop("[LongData:getteur]: there is not such a slot in LongData")
        )
    }
)

cat("### Setteur ###\n")
setMethod("[<-","LongData",
    function(x,i,j,value){
        switch(EXPR=i,
            "id"={x@id<-as.character(value)},
            "varName"={x@varName<-value},
            "time"={x@time<-value},
            "traj"={x@traj<-value},
            "other"={x@other<-value},
            stop("[LongData:getteur]: this is not a LongData slot")
        )
        dimnames(x@traj) <- list(x@id,paste(x@varName,x@time,sep=""))
        validObject(x)
        return(x)
    }
)


cat("\n###################################################################
########################## Class LongData #########################
############################# Affichage ###########################
###################################################################\n")

cat("### Method: 'show' pour LongData ###\n")
.LongData.show <- function(object){
#    cat("\n****** show(LongData) *******")
    cat("   ~~~ Class :",class(object),"~~~ ")
    cat("\n~ id      : [",length(object@id),"] ",sep="");catShort(object@id)
    cat("\n~ time    : [",length(object@time),"] ",sep="");catShort(object@time)
    cat("\n~ varName :",object@varName)
    cat("\n~ traj    : [",length(object@id),"x",length(object@time),"] (limited to 10x10) :\n",sep="")
    if(length(object@id)!=0){
        if(ncol(object@traj)>10){
            trajToShow <- as.data.frame(object@traj[,1:10,drop=FALSE])
            trajToShow$more <- "..."
        }else{
            trajToShow <- as.data.frame(object@traj)
        }
        if(nrow(object@traj)>10){
            print(trajToShow[1:10,])
            cat("... ...\n")
        }else{
            print(trajToShow)
        }
    }else{cat("   <no longData>\n")}
    cat("\n~ other   :\n")
    if(length(object@other)!=0){
      cat("  ",names(object@other),"\n")
    }else{}
#    cat("*** End of  show(longData) ***\n")
    return(invisible(object))
}
cleanProg(.LongData.show)
setMethod("show","LongData",.LongData.show)
rm(.LongData.show)


.LongData.selectSupTrajMinSize <- function(object,minSize){
    return(apply(object@traj,1,function(x)sum(!is.na(x))>=minSize))
}
cleanProg(.LongData.selectSupTrajMinSize)
setMethod("selectSupTrajMinSize",c("LongData","ANY"),.LongData.selectSupTrajMinSize)
rm(.LongData.selectSupTrajMinSize)




gald <- generateArtificialLongData <- function(
    nbEachClusters=50,time=0:10,decimal=2,percentOfMissing=0,
    functionClusters=list(function(t){0},function(t){t},function(t){10-t},function(t){-0.4*t^2+4*t}),
    functionNoise=function(t){rnorm(1,0,3)}
){
    nbClusters <- max(length(nbEachClusters),length(functionNoise),length(functionClusters))
    if(length(nbEachClusters)==1){nbEachClusters<-rep(nbEachClusters,nbClusters)}else{}
    nbEachClusters <- sort(nbEachClusters,decreasing=TRUE)
    if(length(functionClusters)==1){functionClusters<-unlist(list(rep(list(functionClusters),nbClusters)))}else{}
    if(length(functionNoise)==1){functionNoise<-unlist(list(rep(list(functionNoise),nbClusters)))}else{}
    if(length(percentOfMissing)==1){percentOfMissing<- rep(percentOfMissing,nbClusters)}else{}
    nbTime <- length(time)
    id <- paste("I-",1:(sum(nbEachClusters)),sep="")
    indivInCluster <- rep(1:nbClusters,times=nbEachClusters)

    traj <- matrix(0,nrow=sum(nbEachClusters),ncol=nbTime)
    for (iIndiv in 1:nrow(traj)){
        traj[iIndiv,] <- functionClusters[[indivInCluster[iIndiv]]](time)+apply(t(time),2,functionNoise[[indivInCluster[iIndiv]]])
    }
    traj <- round(traj,digit=decimal)


    for (iCluster in 1:nbClusters){
        nbVal <- nbTime*nbEachClusters[iCluster]
        while(sum(is.na(traj[indivInCluster==iCluster,]))/nbVal < percentOfMissing[iCluster]){
            randL <- floor(runif(1,cumsum(c(0,nbEachClusters))[iCluster]+1,cumsum(nbEachClusters)[iCluster]+1))
            randC <- floor(runif(1,1,nbTime+1))
            if(sum(!is.na(traj[randL,]))>1){traj[randL,randC]<-NA}else{}
        }
    }

    colnames(traj) <- paste("V",time,sep="")
    rownames(traj) <- id

    return(
        new("LongData",id=id,time=time,varName="V",traj=traj,
            other=list(
                functionClusters=functionClusters,functionNoise=functionNoise,percentOfMissing=percentOfMissing,
                trueClusters=partition(
                    nbClusters=nbClusters,
                    clusters=as.factor(
                        rep(LETTERSletters[1:nbClusters],time=nbEachClusters)
                    )
                )
            )
        )
    )
}
cleanProg(generateArtificialLongData,,,1)


