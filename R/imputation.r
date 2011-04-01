cat("\n###################################################################
########################## Class LongData #########################
############################ Imputation ###########################
###################################################################\n")

.imputationMatrix <- function(traj,method="copyMean",lowerBound="min",upperBound="max"){

    ## Imputation according to the method
    trajImp <- switch(EXPR=method,
                  "crossMean"={imput.crossMean(traj)},
                  "crossMedian"={imput.crossMedian(traj)},
                  "crossHotDeck"={imput.crossHotDeck(traj)},
                  "locf"={lowerBound <- upperBound <- NA ; imput.locf(traj)},
                  "nocb"={lowerBound <- upperBound <- NA ; imput.nocb(traj)},
                  "trajMean"={lowerBound <- upperBound <- NA ; imput.trajMean(traj)},
                  "trajMedian"={lowerBound <- upperBound <- NA ; imput.trajMedian(traj)},
                  "trajHotDeck"={lowerBound <- upperBound <- NA ; imput.trajHotDeck(traj)},
                  "spline"=imput.spline(traj),
                  "linearInterpol"=,"linearInterpol.locf"=imput.linearInterpol.locf(traj),
                  "linearInterpol.global"=imput.linearInterpol.global(traj),
                  "linearInterpol.local"=imput.linearInterpol.local(traj),
                  "linearInterpol.bisector"=imput.linearInterpol.bisector(traj),
                  "copyMean"=,"copyMean.locf"=imput.copyMean.locf(traj),
                  "copyMean.global"=imput.copyMean.global(traj),
                  "copyMean.local"=imput.copyMean.local(traj),
                  "copyMean.bisector"=imput.copyMean.bisector(traj),
#                  "regressionInt"=imput.regressionInt(traj),
#                  "regressionExt"=imput.regressionExt(traj,predictor),
                  stop("[imputation] Method ",method," does not exists")
    )

    ## Low bounding
    if(!identical(lowerBound,NA)){
        if(length(lowerBound)==1){
            lowerBound <- rep(lowerBound,ncol(traj))
        }else{}
        lowerBound[lowerBound=="min"] <- apply(traj,2,min,na.rm=TRUE)[lowerBound=="min"]
        lowerBound[lowerBound=="globalMin"] <- min(traj,na.rm=TRUE)
        lowerBound[lowerBound=="Inf"] <- min(traj,na.rm=TRUE)

        lowerBound <- as.numeric(lowerBound)
        for(i in 1:ncol(traj)){
            trajImp[trajImp[,i]<lowerBound[i],i] <- lowerBound[i]
        }
    }else{}

    ## Upper bounding
    if(!identical(upperBound,NA)){
        if(length(upperBound)==1){
            upperBound <- rep(upperBound,ncol(traj))
        }else{}
        upperBound[upperBound=="max"] <- apply(traj,2,max,na.rm=TRUE)[upperBound=="max"]
        upperBound[upperBound=="globalMax"] <- max(traj,na.rm=TRUE)
        upperBound[upperBound=="-Inf"] <- max(traj,na.rm=TRUE)
        upperBound <- as.numeric(upperBound)
        for(i in 1:ncol(traj)){
            trajImp[trajImp[,i]>upperBound[i],i] <- upperBound[i]
        }
    }else{}

    return(trajImp)
}

setMethod(f="imputation",
    signature=c("matrix","ANY","ANY","ANY"),
    definition=.imputationMatrix
)



.imputationArray <- function(traj,method="copyMean",lowerBound="min",upperBound="max"){
    for(i in 1:dim(traj)[3]){
        traj[,,i] <- imputation(traj[,,i],method=method,lowerBound=lowerBound,upperBound=upperBound)
    }
    return(traj)
}

setMethod(f="imputation",
    signature=c("array","ANY","ANY","ANY"),
    definition=.imputationArray
)


.imputationLongData <- function(traj,method="copyMean",lowerBound="min",upperBound="max"){
    traj@traj <- imputation(traj["traj"],method=method,lowerBound=lowerBound,upperBound=upperBound)
    return(traj)
}

setMethod(f="imputation",
    signature=c("LongData","ANY","ANY","ANY"),
    definition=.imputationLongData
)

.imputationLongData3d <- function(traj,method="copyMean",lowerBound="min",upperBound="max"){
    traj@traj <- imputation(traj["traj"],method=method,lowerBound=lowerBound,upperBound=upperBound)
    return(traj)
}

setMethod(f="imputation",
    signature=c("LongData3d","ANY","ANY","ANY"),
    definition=.imputationLongData3d
)

## ### Autre methode: interpolation quadratique
## x <- 1:10
## y1 <- 10+1:10*2
## y2 <- -25+1:10*3
## plot(x[1:2],y1[1:2],ylim=c(-10,30),xlim=c(0,10),type="b")
## lines(x[9:10],y2[9:10],type="b")
## y3 <- rep(NA,10)
## for (t in 2:9){
##   y3[t] <- (y2[t]*((t-2)/7)^2 +y1[t]*((9-t)/7)^2  )/ (((t-2)/7)^2+((9-t)/7)^2)
## }
## lines(x,y3,col=2,type="p")


cat("\n+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
++++++++++++++++++++++++++ Class LongData +++++++++++++++++++++++++
++++++++++++++++++++++++++ Fin Imputation +++++++++++++++++++++++++
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++\n")

