cat("###################################################################
########################### class matrix ##########################
################### Imputations des manquantes ####################
###################################################################\n")


bisector <- function(xA,yA,xB,yB,xC,yC){
   ## Les distances ne peuvent pas être nulles car xA!=XB et xA!=xC
   dAB <- sqrt((xA-xB)^2+(yA-yB)^2)
   dAC <- sqrt((xA-xC)^2+(yA-yC)^2)

   ## Calcul de B' et C' tel AB' = AB/|AB| et AC'=AC/|AC|
   xB1 <- xA+(xB-xA)/dAB ; yB1 <- yA+(yB-yA)/dAB
   xC1 <- xA+(xC-xA)/dAC ; yC1 <- yA+(yC-yA)/dAC

   ## Si B'=C', les droites sont confondues
   if(abs(xB1-xC1)<1e-10&abs(yB1-yC1)<1e-10){
       a <- (yA-yC)/(xA-xC)
       b <- yA - a*xA
   }else{
       a <- -((xC-xA)*dAB-(xB-xA)*dAC)/((yC-yA)*dAB-(yB-yA)*dAC)
       b <- yA-a*xA
   }
   return(c(a,b))
}




#################
### linearInterpol

imput.linearInterpol.middleTrajAux <- function(traj){
    while(any(is.na(traj))){
        NAinfM <- min(which(is.na(traj)))-1
        NAsupM <- min(which(!is.na( traj[-(1:NAinfM)] ))) + NAinfM
        traj[NAinfM:NAsupM] <- seq(from=traj[NAinfM],to=traj[NAsupM],length.out=NAsupM-NAinfM+1)
    }
    return(traj)
}

imput.linearInterpol.middleTraj <- function(traj){
    if(all(is.na(traj))){
        warning("[Imputation:linearInterpol.middleTraj] There is only NA on this trajectory, impossible to impute\n")
        return(traj)
    }else{
        if(all(!is.na(traj))){return(traj)}else{}
    }

    infNotNA <-  min(which(!is.na(traj)))
    supNotNA <-  max(which(!is.na(traj)))
    traj[infNotNA:supNotNA] <- imput.linearInterpol.middleTrajAux(traj[infNotNA:supNotNA])
    return(traj)
}

#imput.linearInterpol.middle <- function(longData){
#    return(t(apply(longData,1,imput.linearInterpol.middleTraj)))
#}



###############
### linearInterpolLOCF

imput.linearInterpol.locfTraj <- function(traj){
    if(all(is.na(traj))){
        warning("[Imputation:linearInterpol.locfTraj] There is only NA on this line, impossible to impute\n")
        return(traj)
    }else{}
    traj <- imput.linearInterpol.middleTraj(traj)

    return(imput.locf.traj(traj))
}


imput.linearInterpol.locf <- function(longData){
    return(t(apply(longData,1,imput.linearInterpol.locfTraj)))
}

###############
### linearInterpol.center

imput.linearInterpol.centerTraj <- function(traj){
    if(all(is.na(traj))){
        warning("[Imputation:linearInterpol.centerTraj] There is only NA on this line, impossible to impute\n")
        return(traj)
    }else{}
    return(imput.linearInterpol.middleTraj(traj))
}


imput.linearInterpol.center <- function(longData){
    return(t(apply(longData,1,imput.linearInterpol.centerTraj)))
}



###############
### linearInterpolGlobal

imput.linearInterpol.globalTraj <- function(traj){
    if(all(is.na(traj))){
        warning("[Imputation:linearInterpol.global] There is only NA on this line, impossible to impute\n")
        return(traj)
    }else{}
    if(sum(!is.na(traj))==1){
        warning("[Imputation:linearInterpol.global] There is only one non-NA on this line, linearInterpol.locf is used instead of linearInterpol.global\n")
        return(imput.linearInterpol.locfTraj(traj))
    }else{}

    traj <- imput.linearInterpol.middleTraj(traj)

    lengthTraj <- length(traj)
    firstNoNA <- min(which(!is.na(traj)))
    lastNoNA <- max(which(!is.na(traj)))

    a <- (traj[firstNoNA]-traj[lastNoNA])/(firstNoNA-lastNoNA)
    b <- traj[lastNoNA] - a*lastNoNA
    toImpute <- c(1:firstNoNA,lastNoNA:lengthTraj)
    traj[toImpute]<-a*toImpute+b
    return(traj)
}


imput.linearInterpol.global <- function(longData){
    return(t(apply(longData,1,imput.linearInterpol.globalTraj)))
}



###############
### linearInterpolLocal

imput.linearInterpol.localTraj <- function(traj){
    if(all(is.na(traj))){
        warning("[Imputation:linearInterpol.local] There is only NA on this line, impossible to impute\n")
        return(traj)
    }else{}
    if(sum(!is.na(traj))==1){
        warning("[Imputation:linearInterpol.local] There is only one non-NA on this line, linearInterpol.locf is used instead of linearInterpol.local\n")
        return(imput.linearInterpol.locfTraj(traj))
    }else{}

    traj <- imput.linearInterpol.middleTraj(traj)

    firstNoNA <- min(which(!is.na(traj)))
    secondNoNA <- min(which(!is.na(traj[-firstNoNA])))+1

    a <- (traj[firstNoNA]-traj[secondNoNA])/(firstNoNA-secondNoNA)
    b <- traj[secondNoNA] - a*secondNoNA
    toImpute <- 1:firstNoNA
    traj[toImpute]<-a*toImpute+b

    lengthTraj <- length(traj)
    lastNoNA <- max(which(!is.na(traj)))
    penultimateNoNA <- max(which(!is.na(traj[-lastNoNA])))

    a <- (traj[penultimateNoNA]-traj[lastNoNA])/(penultimateNoNA-lastNoNA)
    b <- traj[lastNoNA] - a*lastNoNA
    toImpute <- lastNoNA:lengthTraj
    traj[toImpute]<-a*toImpute+b
    return(traj)
}

imput.linearInterpol.local <- function(longData){
    return(t(apply(longData,1,imput.linearInterpol.localTraj)))
}



###############
### linearInterpolBisector

imput.linearInterpol.bisectorTraj <- function(traj){
    if(all(is.na(traj))){
        warning("[Imputation:linearInterpol.bisector] There is only NA on this line, impossible to impute\n")
        return(traj)
    }else{}
    if(sum(!is.na(traj))==1){
        warning("[Imputation:linearInterpol.bisector] There is only one non-NA on this line, linearInterpol.locf is used instead of linearInterpol.bisector\n")
        return(imput.linearInterpol.locfTraj(traj))
    }else{}

   lengthTraj <- length(traj)
   firstNoNA <- min(which(!is.na(traj)))
   lastNoNA <- max(which(!is.na(traj)))
   secondNoNA <- min(which(!is.na(traj[-firstNoNA])))+1
   penultimateNoNA <- max(which(!is.na(traj[-lastNoNA])))

   traj <- imput.linearInterpol.middleTraj(traj)

   # formule on http://forums.futura-sciences.com/mathematiques-superieur/39936-equation-dune-bissectrice.html#post2823519
   xA <- firstNoNA ;       yA <- traj[xA]
   xB <- lastNoNA ;        yB <- traj[xB]
   xC <- secondNoNA ;      yC <- traj[xC]
   xD <- penultimateNoNA ; yD <- traj[xD]

   lineLeft <- bisector(xA,yA,xB,yB,xC,yC)
   indNA <- 1:firstNoNA
   traj[indNA] <- lineLeft[1]*indNA + lineLeft[2]

   lineRight <- bisector(xB,yB,xA,yA,xD,yD)
   indNA <- lastNoNA:lengthTraj
   traj[indNA] <- lineRight[1]*indNA + lineRight[2]

   return(traj)
}


imput.linearInterpol.bisector <- function(longData){
    return(t(apply(longData,1,imput.linearInterpol.bisectorTraj)))
}






