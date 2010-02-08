#####################################################
################### Distance Traj ###################
#####################################################

distTraj <- function(x,y,method="euclidean",p=2){
    power <- switch(method,
                    "euclidean"=2,
                    "manhattan"=1,
                    "maximum"=Inf,
                    "minkowski"=p,
                    "canberra"=-1,
                    "binary"=-2)
    return(.C("distance",x=as.numeric(x),y=as.numeric(y),taille=as.integer(length(x)),
            power=as.numeric(power),dist=as.numeric(0),NAOK=TRUE,PACKAGE="longitudinalData")$dist)
}



#####################################################
#################### Frechet max ####################
#####################################################

#####################
### Frechet max


distFrechetR <- function(P,Q,method="max",Fdist=dist){
    if(identical(Fdist,"2D")){Fdist <- dist}else{}
    if(identical(Fdist,"1D")){Fdist <- function(x){abs(x[1,2]-x[2,2])}}else{}
    maxP <- length(P)
    maxQ <- length(Q)
    Mdist <- Mfret <- matrix(0,maxP,maxQ,dimnames=c(list(paste("P",1:maxP,sep=""),paste("Q",1:maxQ,sep=""))))
    for(i in 1:maxP){
        for (j in 1:maxQ){
            Mdist[i,j] <- Fdist(rbind(c(i,P[i]),c(j,Q[j])))
            if(i == 1 && j == 1){Mfret[1,1] = Mdist[1,1]}
            if(i > 1 && j == 1){Mfret[i,1] = do.call(method , list( Mfret[i-1,1] , Mdist[i,1] ) )}
            if(i == 1 && j > 1){Mfret[1,j] = do.call(method , list( Mfret[1,j-1] , Mdist[1,j] ) )}
            if(i > 1  && j > 1){Mfret[i,j] = do.call(method , list( min(Mfret[i-1,j],Mfret[i-1,j-1],Mfret[i,j-1]) , Mdist[i,j] ) )}
        }
    }
#    print(Mdist)
 #   print(Mfret)
 #   cat("\n\n Result =",Mfret[maxP,maxQ],"\n");
    return(Mfret[maxP,maxQ])
}


####################
### Frechet max selon l'article

distFrechetRec <- function(P,Q,method="max",Fdist=dist){
    if(identical(Fdist,"2D")){Fdist <- dist}else{}
    if(identical(Fdist,"1D")){Fdist <- function(x){abs(x[1,2]-x[2,2])}}else{}
    maxP <- length(P)
    maxQ <- length(Q)
    Mdist <- matrix(0,maxP,maxQ,dimnames=c(list(paste("P",1:maxP,sep=""),paste("Q",1:maxQ,sep=""))))
    for(p in 1:maxP){
        for(q in 1:maxQ){
            Mdist[p,q] <- Fdist(rbind(c(p,P[p]),c(q,Q[q])))
        }
    }
    ca <-matrix(-1,maxP,maxQ)
    rec <- function(i,j){
        if(ca[i,j] > -1){return(ca[i,j])}
        if(i == 1 && j == 1){ca[i,j] = Mdist[1,1]}
        if(i > 1 && j == 1){ca[i,1] = do.call( method , list( rec(i-1,1) , Mdist[i,1] ) )}
        if(i == 1 && j > 1){ca[1,j] = do.call( method , list( rec(1,j-1) , Mdist[1,j] ))}
        if(i > 1  && j > 1){ca[i,j] = do.call( method , list( min(rec(i-1,j),rec(i-1,j-1),rec(i,j-1)) , Mdist[i,j]) )}
        return(ca[i,j]);
    }
    rec(maxP,maxQ)
}

########################
### Frechet en C
distFrechet <- function(P,Q,method="max",Fdist=dist){
    if(any(is.na(c(P,Q)))){stop("Frechet distance : polynomes can not contain missing values")}else{}
    result <- .C("distFrechet",x=as.numeric(P),y=as.numeric(Q),tailleX=as.integer(length(P)),tailleY=as.integer(length(Q)),
                 methodMax=as.integer(method=="max"),distanceTraj=as.integer(identical(Fdist,"1D")),dist=as.numeric(0),NAOK=TRUE,PACKAGE="longitudinalData")
    return(result$dist)
}


##################################################
##################################################
##################################################

pathFrechetR <- function(P,Q,method="max",Fdist=dist){
    if(identical(Fdist,"2D")){Fdist <- dist}else{}
    if(identical(Fdist,"1D")){Fdist <- function(x){abs(x[1,2]-x[2,2])}}else{}
    way <- c("PQ","P","Q")
    maxP <- length(P)
    maxQ <- length(Q)
    Mpath <- Mdist <- Mfret <- matrix(0,maxP,maxQ,dimnames=c(list(paste("P",1:maxP,sep=""),paste("Q",1:maxQ,sep=""))))
    for(i in 1:maxP){
        for (j in 1:maxQ){
            Mdist[i,j] <- Fdist(rbind(c(i,P[i]),c(j,Q[j])))
            if(i == 1 && j == 1){
                Mfret[1,1] = Mdist[1,1]
                Mpath[1,1]="start"
            }
            if(i > 1 && j == 1){
                Mfret[i,1] = do.call(method , list( Mfret[i-1,1] , Mdist[i,1] ) )
                Mpath[i,1] = "P"
            }
            if(i == 1 && j > 1){
                Mfret[1,j] = do.call(method , list( Mfret[1,j-1] , Mdist[1,j] ) )
                Mpath[1,j] = "Q"
            }
            if(i > 1  && j > 1){
                movePQ <- Mfret[i-1,j-1]
                moveP <- Mfret[i-1,j]
                moveQ <- Mfret[i,j-1]
                Mfret[i,j] = do.call(method , list( min(movePQ,moveP,moveQ) , Mdist[i,j] ) )
                Mpath[i,j] =  way[which.min(c(movePQ,moveP,moveQ))]
            }
        }
    }
#    print(Mdist)
 #   print(Mfret)
  #  print(Mpath)

    i <- maxP
    j <- maxQ
    bestPath <- c(maxP,maxQ)
    while(i>1||j>1){
        if(Mpath[i,j]=="Q"){
            j<-j-1;
        }else{
            if(Mpath[i,j]=="P"){
                i<-i-1;
            }else{
                i<-i-1;
                j<-j-1;
            }
        }
        bestPath <- rbind(c(i,j),bestPath)
    }
    colnames(bestPath)<-c("P","Q")
    rownames(bestPath)<-NULL
#    print(bestPath)
    return(list(dist=Mfret[maxP,maxQ],bestPath=bestPath))
}

pathFrechet <- function(P,Q,method="max",Fdist=dist){
    if(any(is.na(c(P,Q)))){stop("Frechet distance : polynomes can not contain missing values")}else{}
    result <- .C("pathFrechet",x=as.numeric(P),y=as.numeric(Q),tailleX=as.integer(length(P)),tailleY=as.integer(length(Q)),
                 methodMax=as.integer(method=="max"),distanceTraj=as.integer(identical(Fdist,"1D")),
                 dist=as.numeric(0),bestPath=integer((length(P)+length(Q))*2),pathLength=as.integer(0),NAOK=TRUE,PACKAGE="longitudinalData")
    bestPath <- matrix(na.omit(result$bestPath),result$pathLength,2,byrow=TRUE)
    return(list(dist=result$dist,bestPath=bestPath))
}


#newQ <- numeric(maxP)
#for (p in 1:maxP){
#    newQ[p] <- mean(Q[fPath[,2][fPath[,1]==p]])
#}
#lines(newQ,col="green")
