cat("###################################################################
#################### Class LongData & Partition ###################
################### Imputations des manquantes ####################
###################################################################\n")


#################
### LOCF

trajImput.LOCB.begin <- function(traj){
    if(all(is.na(traj))){
        warning("There is only NA on this line, impossible to impute\n")
        return(traj)
    }else{}
    firstNoNA <- min(which(!is.na(traj)))
    traj[1:firstNoNA]<-traj[firstNoNA]
    return(traj)
}

trajImput.LOCF.end <- function(traj){
    if(all(is.na(traj))){
        warning("There is only NA on this line, impossible to impute\n")
        return(traj)
    }else{}
    lastNoNA <- max(which(!is.na(traj)))
    traj[lastNoNA:length(traj)]<-traj[lastNoNA]
    return(traj)
}

trajImput.LOCF <- function(traj){
    return(trajImput.LOCF.middle(trajImput.LOCB.begin(traj)))
}


#################
### LOCB

trajImput.LOCB.middle <- function(traj){
    if(all(is.na(traj))){
        warning("There is only NA on this line, impossible to impute\n")
        return(traj)
    }else{
        if(is.na(traj[length(traj)])){stop("Last value is NA; run trajImput.LOCF.end before\n")}else{}
    }
    while(any(is.na(traj))){
        traj[max(which(is.na(traj)))]<-traj[max(which(is.na(traj)))+1]
    }
    return(traj)
}

trajImput.LOCF.middle <- function(traj){
    if(all(is.na(traj))){
        warning("There is only NA on this line, impossible to impute\n")
        return(traj)
    }else{
        if(is.na(traj[1])){stop("First value is NA; run trajImput.LOCB.begin before\n")}else{}
    }
    while(any(is.na(traj))){
        traj[min(which(is.na(traj)))]<-traj[min(which(is.na(traj)))-1]
    }
    return(traj)
}

trajImput.LOCB <- function(traj){
    return(trajImput.LOCB.middle(trajImput.LOCF.end(traj)))
}




###################################################################
###################### Interpolation Lineraire ####################
###################################################################

###############
### Linear interpolation middle

trajImput.interpoLin.middle <- function(traj){
    if(all(is.na(traj))){
        warning("There is only NA on this line, impossible to impute\n")
        return(traj)
    }else{}
    if(sum(!is.na(traj))==1){
        warning("There is only one non-missing value on this line, impossible to use localSlope\n")
        return(traj)
    }else{}
    if(is.na(traj[1])|is.na(traj[length(traj)])){
        stop("First or last value is NA; run InterpolBeginEnd first\n")
    }else{}

    while(any(is.na(traj))){
        NAinfM <- min(which(is.na(traj)))-1
        NAsupM <- min(which(!is.na( traj[-(1:NAinfM)] ))) + NAinfM
        traj[NAinfM:NAsupM] <- seq(from=traj[NAinfM],to=traj[NAsupM],length.out=NAsupM-NAinfM+1)
    }
    return(traj)
}



###############
### Linear interpolation 2 : global

trajImput.globalSlope.beginEnd <- function(traj){
    if(all(is.na(traj))){
        warning("There is only NA on this line, impossible to impute\n")
        return(traj)
    }else{}
    if(sum(!is.na(traj))==1){
        warning("There is only one non-missing value on this line, impossible to use globalSlope\n")
        return(traj)
    }else{}
    lengthTraj <- length(traj)
    firstNoNA <- min(which(!is.na(traj)))
    lastNoNA <- max(which(!is.na(traj)))

    a <- (traj[firstNoNA]-traj[lastNoNA])/(firstNoNA-lastNoNA)
    b <- traj[lastNoNA] - a*lastNoNA
    indNA <- c(1:firstNoNA,lastNoNA:lengthTraj)
    traj[indNA]<-a*indNA+b
    return(traj)
}


trajImput.interpoLin2 <- function(traj){
    return(trajImput.interpoLin.middle(trajImput.globalSlope.beginEnd(traj)))
}



###############
### Linear interpolation 3 : Local

trajImput.localSlope.beginEnd <- function(traj){
    if(all(is.na(traj))){
        warning("There is only NA on this line, impossible to impute\n")
        return(traj)
    }else{}
    if(sum(!is.na(traj))==1){
        warning("There is only one non-missing value on this line, impossible to use localSlope\n")
        return(traj)
    }else{}
    firstNoNA <- min(which(!is.na(traj)))
    secondNoNA <- min(which(!is.na(traj[-firstNoNA])))+1

    a <- (traj[firstNoNA]-traj[secondNoNA])/(firstNoNA-secondNoNA)
    b <- traj[secondNoNA] - a*secondNoNA
    indNA <- 1:firstNoNA
    traj[indNA]<-a*indNA+b

    lengthTraj <- length(traj)
    lastNoNA <- max(which(!is.na(traj)))
    penultimateNoNA <- max(which(!is.na(traj[-lastNoNA])))

    a <- (traj[penultimateNoNA]-traj[lastNoNA])/(penultimateNoNA-lastNoNA)
    b <- traj[lastNoNA] - a*lastNoNA
    indNA <- lastNoNA:lengthTraj
    traj[indNA]<-a*indNA+b
    return(traj)
}

trajImput.interpoLin3 <- function(traj){
    return(trajImput.interpoLin.middle(trajImput.localSlope.beginEnd(traj)))
}



###############
### Linear interpolation 4 : LOCF

trajImput.interpoLin4 <- function(traj){
    return(trajImput.interpoLin.middle(trajImput.LOCF.end(trajImput.LOCB.begin(traj))))
}


#trajImput.localSlope.end <- function(traj){
#    if(all(is.na(traj))){
#        warning("There is only NA on this line, impossible to impute\n")
#        return(traj)
#    }else{}
#    if(sum(!is.na(traj))==1){
#        warning("There is only one non-missing value on this line, impossible to use localSlope\n")
#        return(traj)
#    }else{}
#    lengthTraj <- length(traj)
#    lastNoNA <- max(which(!is.na(traj)))
#    penultimateNoNA <- max(which(!is.na(traj[-lastNoNA])))
#
#    a <- (traj[penultimateNoNA]-traj[lastNoNA])/(penultimateNoNA-lastNoNA)
#    b <- traj[lastNoNA] - a*lastNoNA
#    indNA <- lastNoNA:lengthTraj
#    traj[indNA]<-a*indNA+b
#    return(traj)
#}
#cleanProg(trajImput.localSlope.end,,,0)




###############
### Linear interpolation 1

## trajImput.localGlobalSlope.beginEnd <- function(traj){
##     if(all(is.na(traj))){
##         warning("There is only NA on this line, impossible to impute\n")
##         return(traj)
##     }else{}
##     if(sum(!is.na(traj))==1){
##         warning("There is only one non-missing value on this line, impossible to use localSlope\n")
##         return(traj)
##     }else{}
##     lengthTraj <- length(traj)
##     firstNoNA <- min(which(!is.na(traj)))
##     lastNoNA <- max(which(!is.na(traj)))
##     secondNoNA <- min(which(!is.na(traj[-firstNoNA])))+1
##     penultimateNoNA <- max(which(!is.na(traj[-lastNoNA])))

##     a <- ((traj[firstNoNA]-traj[secondNoNA])/(firstNoNA-secondNoNA) + (traj[firstNoNA]-traj[lastNoNA])/(firstNoNA-lastNoNA))/2
##     b <- traj[firstNoNA] - a*firstNoNA
##     indNA <- 1:firstNoNA
##     traj[indNA]<-a*indNA+b

##     a <- ((traj[penultimateNoNA]-traj[lastNoNA])/(penultimateNoNA-lastNoNA) + (traj[firstNoNA]-traj[lastNoNA])/(firstNoNA-lastNoNA))/2
##     b <- traj[lastNoNA] - a*lastNoNA
##     indNA <- lastNoNA:lengthTraj
##     traj[indNA]<-a*indNA+b
##     return(traj)
## }

## trajImput.interpoLin <- function(traj){
##     return(trajImput.interpoLin.middle(trajImput.localGlobalSlope.beginEnd(traj)))
## }

trajImput.localGlobalSlope.beginEnd <- function(traj){
    if(all(is.na(traj))){
        warning("There is only NA on this line, impossible to impute\n")
        return(traj)
    }else{}
    if(sum(!is.na(traj))==1){
        warning("There is only one non-missing value on this line, impossible to use localSlope\n")
        return(traj)
    }else{}

    lengthTraj <- length(traj)
    firstNoNA <- min(which(!is.na(traj)))
    lastNoNA <- max(which(!is.na(traj)))
    secondNoNA <- min(which(!is.na(traj[-firstNoNA])))+1
    penultimateNoNA <- max(which(!is.na(traj[-lastNoNA])))

    # formule on http://forums.futura-sciences.com/mathematiques-superieur/39936-equation-dune-bissectrice.html#post2823519
    xA <- firstNoNA ; yA <- traj[xA]
    xB <- lastNoNA ; yB <- traj[xB]
    xC <- secondNoNA ; yC <- traj[xC]
    xD <- penultimateNoNA ; yD <- traj[xD]
    if((xA-xB)*(yA-yC)-(yA-yB)*(xA-xC)<1e-15){
        a <- (yA-yC)/(xA-xC)
        b <- yA - a*xA
        indNA <- c(1:firstNoNA,lastNoNA:lengthTraj)
        traj[indNA]<-a*indNA+b
    }else{
        dAB <- as.numeric(dist(rbind(c(xA,yA),c(xB,yB))))
        dAC <- as.numeric(dist(rbind(c(xA,yA),c(xC,yC))))
        dBD <- as.numeric(dist(rbind(c(xB,yB),c(xD,yD))))

#    alpha <- (xC-xA)*dAB-(xB-xA)*dAC
#    beta <-  (yC-yA)*dAB-(yB-yA)*dAC
#    a <- -alpha/beta
#    b <- yA-a*xA
        a <- -((xC-xA)*dAB-(xB-xA)*dAC)/((yC-yA)*dAB-(yB-yA)*dAC)
        b <- yA-a*xA
        indNA <- 1:firstNoNA
        traj[indNA]<-a*indNA+b

        a <- -((xD-xB)*dAB-(xA-xB)*dBD)/((yD-yB)*dAB-(yA-yB)*dBD)
        b <- yB-a*xB
        indNA <- lastNoNA:lengthTraj
        traj[indNA]<-a*indNA+b
    }
    return(traj)
}

trajImput.interpoLin <- function(traj){
    return(trajImput.interpoLin.middle(trajImput.localGlobalSlope.beginEnd(traj)))
}




#######################################################
###################### copy mean ######################
# paste the shape of the model

trajImput.copy.begin <- function(traj,model){
    if(all(is.na(traj))){
        warning("there is only NA on this line, impossible to impute\n")
        return(traj)
    }else{}
    if(all(is.na(traj))){
        warning("There is only NA on this line, impossible to impute\n")
        return(traj)
    }else{}

    NAinf <- min(which(!is.na(traj)))
    traj[1:NAinf]<-model[1:NAinf] + traj[NAinf]-model[NAinf]
    return(traj)
}

trajImput.copy.end <- function(traj,model){
    if(all(is.na(traj))){
        warning("there is only NA on this line, impossible to impute\n")
        return(traj)
    }else{}
    if(all(is.na(traj))){
        warning("There is only NA on this line, impossible to impute\n")
        return(traj)
    }else{}

    NAsup <- max(which(!is.na(traj)))
    trajLength <- length(traj)
    traj[NAsup:trajLength] <- model[NAsup:trajLength] + traj[NAsup]-model[NAsup]
    return(traj)
}

trajImput.copy.middle <- function(traj,model){
    if(all(is.na(traj))){
        warning("there is only NA on this line, impossible to impute\n")
        return(traj)
    }else{}
    if(any(is.na(model))){stop("There is NA in model, impossible to impute\n")}else{}
    while(any(is.na(traj))){
        NAinfM <- min(which(is.na(traj)))-1
        NAsupM <- min(which(!is.na( traj[-(1:NAinfM)] ))) + NAinfM
        traj[NAinfM:NAsupM] <- (model[NAinfM:NAsupM]
            + seq(from=traj[NAinfM],to=traj[NAsupM],length.out=NAsupM-NAinfM+1)
            - seq(from=model[NAinfM],to=model[NAsupM],length.out=NAsupM-NAinfM+1))
    }
    return(traj)
}


trajImput.copyMean <- function(traj,model){
    return(trajImput.copy.middle(trajImput.copy.end(trajImput.copy.begin(traj,model),model),model))
}




### Autre méthode : scaleShape
### Pour le centre, on fixe un facteur p, puis on pose d=(y_B'+p)/(y_B+p).
### ensuite, y_C'=y_D'+Delta C * d
### avec Delta C = y_C-y_D

### Pour les extrèmes, on considère les droites T' passant par y_A' et y_B' et T passant par y_A et y_B
### on calcule le  Delta C relativement a T, on l'ajoute a T' avec éventuellement un scale

###Autre méthode, dans le cas a <- c(NA,0,3,NA),  (m <- 1:4) on pourrait imputer en (-3,0,3,6)

### Plus précisément : on calcule les variations de la moyenne par rapport à une ligne moyenne qui irait tout droit
###   On ajoute ces variations a la ligne reliant InfNA et SupNA
### Une autre méthode est possible [MARCHE PAS] : calculer les variations par rapport a meanLine[infNA]
###   puis les normaliser en divisant par (meanLine[SupNA]-meanLine[InfNA])
###   puis les adapter a line en multipliant par (line[SupNA]-line[InfNA])
###   et enfin ajouter a line[InfNA]

#plot(line2,ylim=c(0,15),type="b",col="red",lwd=3)
#lines(meanLine,type="b",col="blue",lwd=2)
#line2imp <- .LongData.imputeOne(line2,meanLine)
#lines(line2imp-0.1,type="b",col="red",lty="dotted")

#plot(line2,ylim=c(0,15),type="b",col="red",lwd=3)
#lines(meanLine-1:9*2+15,type="b",col="green")
#line2imp2<-.LongData.imputeOne(line2,meanLine-1:9*2+10)
#lines(line2imp2+0.1,type="b",col="red",lty="dotted")
#rm(firstNA,lastNA,line,line2,line3,meanLine,line2imp,line2imp2)
### Fonctions moyenne, ecart type et which.min résistante aux NA.


### Autre methode: interpolation quadratique
# x <- 1:10
# y1 <- 10+1:10*2
# y2 <- -25+1:10*3
# plot(x[1:2],y1[1:2],ylim=c(-10,30),xlim=c(0,10),type="b")
# lines(x[9:10],y2[9:10],type="b")
# y3 <- rep(NA,10)
# for (t in 2:9){
#   y3[t] <- (y2[t]*((t-2)/7)^2 +y1[t]*((9-t)/7)^2  )/ (((t-2)/7)^2+((9-t)/7)^2)
# }
# lines(x,y3,col=2,type="p")




#########################################################
##### All method imputing without using a partition #####


cat("### imputation(LongData), methode ###\n")
#object <- ld2n;partition <- p2b


matrixImput <- function(object,method,partition){
    ### Method without partition
    object <- switch(EXPR=method,
        "LOCF"={t(apply(object,1,trajImput.LOCF))},
        "FOCB"={t(apply(object,1,trajImput.LOCB))},
        "linearInterpolation"={t(apply(object,1,trajImput.interpoLin))},
        "LI-Bisector"={t(apply(object,1,trajImput.interpoLin))},
        "LI-Global"={t(apply(object,1,trajImput.interpoLin2))},
        "LI-Local"={t(apply(object,1,trajImput.interpoLin3))},
        "LI-OCBF"={t(apply(object,1,trajImput.interpoLin4))},
        "copyMean"={
            traj <- object[!is.na(partition),]
            clusters <- partition[!is.na(partition)]
            trajMeanObs <- aggregate(traj,by=list(clusters),FUN=meanNA)[,-1]
            trajMeanObs <- t(apply(trajMeanObs,1,trajImput.interpoLin))
            trajMeanObs <- trajMeanObs[as.integer(clusters),]
            valAndMeans <- array(c(traj,trajMeanObs),dim=c(dim(traj),2))
            copyValAndMeans <- function(xy){trajImput.copyMean(xy[,1],xy[,2])}
	    t(apply(valAndMeans,1,copyValAndMeans))
        },
        {warning("Unknow imputation method\n");object}
    )
    return(object)
}
setMethod(f="imputation",
          signature=c(object="matrix",method="ANY",partition="ANY"),
          definition=matrixImput
)



trajImput <- function(object,method,partition){
    ### Method without partition
    object@traj <- switch(EXPR=method,
        "LOCF"={t(apply(object@traj,1,trajImput.LOCF))},
        "FOCB"={t(apply(object@traj,1,trajImput.LOCB))},
        "linearInterpolation"={t(apply(object@traj,1,trajImput.interpoLin))},
        "LI-Bisector"={t(apply(object@traj,1,trajImput.interpoLin))},
        "LI-Global"={t(apply(object@traj,1,trajImput.interpoLin2))},
        "LI-Local"={t(apply(object@traj,1,trajImput.interpoLin3))},
        "LI-OCBF"={t(apply(object@traj,1,trajImput.interpoLin4))},
        "copyMean"={
            clusters <- partition["clusters"]
            traj <- object@traj[!is.na(clusters),]
            clusters <- clusters[!is.na(clusters)]
            trajMeanObs <- aggregate(traj,by=list(clusters),FUN=meanNA)[,-1]
            trajMeanObs <- t(apply(trajMeanObs,1,trajImput.interpoLin))
            trajMeanObs <- trajMeanObs[as.integer(clusters),]
            valAndMeans <- array(c(traj,trajMeanObs),dim=c(dim(traj),2))
            copyValAndMeans <- function(xy){trajImput.copyMean(xy[,1],xy[,2])}
	    t(apply(valAndMeans,1,copyValAndMeans))
        },
        {warning("Unknow imputation method\n");object@traj}
    )
    return(object)
}
setMethod(f="imputation",
    signature=c(object="LongData",method="ANY",partition="ANY"),
    definition=trajImput
)


#trajImput <- function(object,method,partition){
#    ### Method without partition
#    traj <- NA
#    if(method=="LOCF"){
#        return(t(apply(object@traj,1,trajImput.LOCF)))
#    }else{}
#    if(method=="LOCB"){
#        return(t(apply(object@traj,1,trajImput.LOCB)))
#    }else{}
#    if(method=="linearInterpolation"){
#        return(t(apply(object@traj,1,trajImput.interpoLin)))
#    }else{}
#    if(method=="linearInterpolation2"){
#        return(t(apply(object@traj,1,trajImput.interpoLin2)))
#    }else{}
#    if(method=="linearInterpolation3"){
#        return(t(apply(object@traj,1,trajImput.interpoLin3)))
#    }else{}
#
#    ### Method with partition, and mean longData
#    if(method=="copyMean"){
#        clusters <- y["clusters"]
#        traj <- object@traj[!is.na(y["clusters"]),]
#        clusters <- clusters[!is.na(clusters)]
#        trajMeanObs <- aggregate(traj,by=list(clusters),FUN=meanNA)[,-1]
#        trajMeanObs <- t(apply(trajMeanObs,1,trajImput.interpoLin))
#        trajMeanObs <- trajMeanObs[as.integer(clusters),]
#        valAndMeans <- array(c(traj,trajMeanObs),dim=c(dim(traj),2))
#        copyValAndMeans <- function(xy){trajImput.copyMean(xy[,1],xy[,2])}
#        return(t(apply(valAndMeans,1,copyValAndMeans)))
#    }else{}#
#
#    warning("Unknow method\n")
#}
#cleanProg(trajImput,,,6) # trajImput.interpoLin trajImput.interpoLinS trajImput.LOCF trajImput.LOCB meanNA
#setMethod(f="imputation",
#    signature=c(object="LongData",y="ANY",method="ANY"),
#    definition=trajImput
#)
#rm(trajImput)
##

