cat("###################################################################
########################### class matrix ##########################
################### Imputations des manquantes ####################
###################################################################\n")


#################
### locf
###  - pour monotones et intermitentes
imput.locf.traj <- function(traj){
    if(all(is.na(traj))){
        warning("[Imputation:locf] There is only NA on this trajectory, impossible to impute\n")
        return(traj)
    }else{
        if(all(!is.na(traj))){return(traj)}else{}
    }
    nonMissing <- !is.na(traj)
    valNonMissing <- which(nonMissing)
    return(traj[c(valNonMissing[1],valNonMissing)][cumsum(nonMissing)+1])
}

imput.locf <- function(longData){
    return(t(apply(longData,1,imput.locf.traj)))
}


## imput.locf.traj.end <- function(traj){
##     if(all(is.na(traj))){
##         warning("There is only NA on this trajectory, impossible to impute\n")
##         return(traj)
##     }else{
##         if(all(!is.na(traj))){return(traj)}else{}
##     }
##     lastNoNA <- max(which(!is.na(traj)))
##     traj[lastNoNA:length(traj)] <- traj[lastNoNA]
##     return(traj)
## }

## imput.locfend <- function(longData){
##     return(t(apply(longData,1,imput.locf.traj.end)))
## }


#################
### NOCB
###  - pour monotones et intermitentes


imput.nocb <- function(longData){
    return(imput.locf(longData[,ncol(longData):1,drop=FALSE])[,ncol(longData):1,drop=FALSE])
}


#################
### traj mean
###  - pour monotones et intermitentes

imput.trajMean.traj <- function(traj){
    if(all(is.na(traj))){
        warning("[Imputation:trajMean] There is only NA on this trajectory, impossible to impute\n")
        return(traj)
    }else{
        if(all(!is.na(traj))){return(traj)}else{}
    }
    traj[is.na(traj)] <- mean(traj,na.rm=TRUE)
    return(traj)
}

imput.trajMean <- function(longData){
    return(t(apply(longData,1,imput.trajMean.traj)))
}


#################
### traj median
###  - pour monotones et intermitentes

imput.trajMedian.traj <- function(traj){
    if(all(is.na(traj))){
        warning("[Imputation:trajMedian] There is only NA on this trajectory, impossible to impute\n")
        return(traj)
    }else{
        if(all(!is.na(traj))){return(traj)}else{}
    }
    traj[is.na(traj)] <- median(traj,na.rm=TRUE)
    return(traj)
}

imput.trajMedian <- function(longData){
    return(t(apply(longData,1,imput.trajMedian.traj)))
}



#################
### traj hot deck
###  - pour monotones et intermitentes

imput.trajHotDeck.traj <- function(traj){
    if(all(is.na(traj))){
        warning("[Imputation:trajHotDeck] There is only NA on this trajectory, impossible to impute\n")
        return(traj)
    }else{
        if(all(!is.na(traj))){return(traj)}else{}
    }
    missing <- is.na(traj)
    nbPos <- sum(!missing)
    alea <- floor(runif(length(traj)-nbPos,min=1,max=nbPos+1))
    traj[missing] <- sapply(alea,function(x)traj[cumsum(!missing)==x & !missing])
    return(traj)
}

imput.trajHotDeck <- function(longData){
    return(t(apply(longData,1,imput.trajHotDeck.traj)))
}



###############
### spline

imput.spline.traj <- function(traj){
    if(all(is.na(traj))){
        warning("[Imputation:spline] There is only NA on this trajectory, impossible to impute\n")
        return(traj)
    }else{
        if(all(!is.na(traj))){return(traj)}else{}
    }

    traj <- splinefun(traj)(1:length(traj))
    return(traj)
}

imput.spline <- function(longData){
    return(t(apply(longData,1,imput.spline.traj)))
}




