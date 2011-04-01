
###############
### la fonction bissector est définit dans imputLinearInterpol.R



###############
### copyMean

imput.copyMean.centerTrajAux <- function(traj,model){
    while(any(is.na(traj))){
        NAinfM <- min(which(is.na(traj)))-1
        NAsupM <- min(which(!is.na( traj[-(1:NAinfM)] ))) + NAinfM
        traj[NAinfM:NAsupM] <- (model[NAinfM:NAsupM]
            + seq(from=traj[NAinfM],to=traj[NAsupM],length.out=NAsupM-NAinfM+1)
            - seq(from=model[NAinfM],to=model[NAsupM],length.out=NAsupM-NAinfM+1))
    }
    return(traj)
}

imput.copyMean.centerTraj <- function(traj,model){
    if(all(is.na(traj))){
        warning("[Imputation:copyMean.centerTraj] There is only NA on this line, impossible to impute\n")
        return(traj)
    }else{
        if(sum(!is.na(traj))==1){
            ### Pas de manquantes intermitantes, elles sont toutes monotones.
            return(traj)
        }else{
            if(all(!is.na(traj))){return(traj)}else{}
        }
    }

    infNotNA <-  min(which(!is.na(traj)))
    supNotNA <-  max(which(!is.na(traj)))
    traj[infNotNA:supNotNA] <- imput.copyMean.centerTrajAux(traj[infNotNA:supNotNA],model[infNotNA:supNotNA])
    return(traj)
}

### ATTENTION : copyMean.center N'EST PAS équivalent a copyMean.locf,
###   car il a besoin d'un model et il n'y a pas les mécanismes de controle.
###   Peut-être peut-on supprimer cette fonction ?
# imput.copyMean.center <- function(longData,model){
#     return(t(apply(longData,1,imput.copyMean.centerTraj,model)))
# }


## On note MT trajectoire moyenne et IT la trajectoire de l'individu.
## Soit miss les manquantes. Soit MTmiss la trajectoire moyenne à laquelle on
## ajoute les manquantes. Soit MTlocf, la trajectoire MTmiss imputé selon la
## méthode linearInterpol.LOCF. Alors les variations sont : varMean = MTlocf-MT.
## La trajectoire final est donc IT+linearInterpol + varMean



###############
### copyMeanLOCF

imput.copyMean.locfTraj <- function(traj,model){
    if(all(is.na(traj))){
        warning("[Imputation:copyMean.locfTraj] There is only NA on this line, impossible to impute\n")
        return(traj)
    }else{}
    traj <- imput.copyMean.centerTraj(traj,model)

    firstNoNA <- min(which(!is.na(traj)))
    traj[1:firstNoNA]<-model[1:firstNoNA] + traj[firstNoNA]-model[firstNoNA]

    lastNoNA <- max(which(!is.na(traj)))
    trajLength <- length(traj)
    traj[lastNoNA:trajLength] <- model[lastNoNA:trajLength] + traj[lastNoNA]-model[lastNoNA]

    return(traj)
}


imput.copyMean.locf <- function(longData){

    ## Préparation de la trajectoire moyenne.
    ## En particulier, imputation si manquantes
    model <- apply(longData,2,mean,na.rm=TRUE)

    if(all(is.na(model))){
        warning("[Imputation:copyMean.locf] There is only NA in the model, impossible to impute\n")
        return(longData)
    }else{
        if(any(is.na(model))){
            warning("[Imputation:copyMean.locf] There is NA in the model. linearInterpol.locf is used to complete the model\n")
            model <- imput.linearInterpol.locf(t(model))
        }else{}
    }

    ## Imputation
    return(t(apply(longData,1,imput.copyMean.locfTraj,model)))
}


###############
### copyMeanGlobal


imput.copyMean.globalTraj <- function(traj,model){
    if(all(is.na(traj))){
        warning("[Imputation:copyMean.globalTraj] There is only NA on this line, impossible to impute\n")
        return(traj)
    }else{
        if(sum(!is.na(traj))==1){
            warning("[Imputation:copyMean.globalTraj] There is only one non-NA on this line, copyMean.locf is used instead of copyMean.global\n")
            return(imput.copyMean.locfTraj(traj,model))
        }else{
            if(all(!is.na(traj))){return(traj)}else{}
        }
    }
    traj <- imput.copyMean.centerTraj(traj,model)

    firstNoNA <- min(which(!is.na(traj)))
    lastNoNA <- max(which(!is.na(traj)))
    trajLength <- length(traj)

    aTraj <- (traj[firstNoNA]-traj[lastNoNA])/(firstNoNA-lastNoNA)
    bTraj <- traj[lastNoNA] - aTraj*lastNoNA
    aModel <- (model[firstNoNA]-model[lastNoNA])/(firstNoNA-lastNoNA)
    bModel <- model[lastNoNA] - aModel*lastNoNA

    indNA <- c(1:firstNoNA,lastNoNA:trajLength)
    traj[indNA] <- aTraj*indNA+bTraj + model[indNA] - (aModel*indNA+bModel)
    return(traj)
}


imput.copyMean.global <- function(longData){

    ## Préparation de la trajectoire moyenne.
    ## En particulier, imputation si manquantes
    model <- apply(longData,2,mean,na.rm=TRUE)

    if(all(is.na(model))){
        warning("[Imputation:copyMean.global] There is only NA in the model, impossible to impute\n")
        return(longData)
    }else{
        if(any(is.na(model))){
            warning("[Imputation:copyMean.global] There is NA in the model. linearInterpol.global is used to complete the model\n")
            model <- imput.linearInterpol.global(t(model))
        }else{}
    }

    ## Imputation
    return(t(apply(longData,1,imput.copyMean.globalTraj,model)))
}



###############
### copyMeanLocal

imput.copyMean.localTraj <- function(traj,model){
    if(all(is.na(traj))){
        warning("[Imputation:copyMean.localTraj] There is only NA on this line, impossible to impute\n")
        return(traj)
    }else{
        if(sum(!is.na(traj))==1){
            warning("[Imputation:copyMean.localTraj] There is only one non-NA on this line, copyMean.locf is used instead of copyMean.global\n")
            return(imput.copyMean.locfTraj(traj,model))
        }else{
            if(all(!is.na(traj))){return(traj)}else{}
        }
    }

    ## We can either imput the center then Compute these value,
    ## or compute the values then impute the center this does not change the results.

    firstNoNA <- min(which(!is.na(traj)))
    secondNoNA <- min(which(!is.na(traj[-firstNoNA])))+1
    lastNoNA <- max(which(!is.na(traj)))
    penultimateNoNA <- max(which(!is.na(traj[-lastNoNA])))
    trajLength <- length(traj)
    traj <- imput.copyMean.centerTraj(traj,model)

    ## Begin
    aTraj <- (traj[firstNoNA]-traj[secondNoNA])/(firstNoNA-secondNoNA)
    bTraj <- traj[firstNoNA] - aTraj*firstNoNA
    aModel <- (model[firstNoNA]-model[secondNoNA])/(firstNoNA-secondNoNA)
    bModel <- model[firstNoNA] - aModel*firstNoNA

    indNA <- c(1:firstNoNA)
    traj[indNA] <- aTraj*indNA+bTraj + model[indNA] - (aModel*indNA+bModel)

    ## End
    aTraj <- (traj[lastNoNA]-traj[penultimateNoNA])/(lastNoNA-penultimateNoNA)
    bTraj <- traj[lastNoNA] - aTraj*lastNoNA
    aModel <- (model[lastNoNA]-model[penultimateNoNA])/(lastNoNA-penultimateNoNA)
    bModel <- model[lastNoNA] - aModel*lastNoNA

    indNA <- c(lastNoNA:trajLength)
    traj[indNA] <- aTraj*indNA+bTraj + model[indNA] - (aModel*indNA+bModel)

    return(traj)
}


imput.copyMean.local <- function(longData){

    ## Préparation de la trajectoire moyenne.
    ## En particulier, imputation si manquantes
    model <- apply(longData,2,mean,na.rm=TRUE)

    if(all(is.na(model))){
        warning("[Imputation:copyMean.local] There is only NA in the model, impossible to impute\n")
        return(longData)
    }else{
        if(any(is.na(model))){
            warning("[Imputation:copyMean.local] There is NA in the model. linearInterpol.local is used to complete the model\n")
            model <- imput.linearInterpol.local(t(model))
        }else{}
    }

    ## Imputation
    return(t(apply(longData,1,imput.copyMean.localTraj,model)))
}




###############
### copyMeanBisector

imput.copyMean.bisectorTraj <- function(traj,model){
    if(all(is.na(traj))){
        warning("[Imputation:copyMean.bisectorTraj] There is only NA on this line, impossible to impute\n")
        return(traj)
    }else{
        if(sum(!is.na(traj))==1){
            warning("[Imputation:copyMean.bisectorTraj] There is only one non-NA on this line, copyMean.locf is used instead of copyMean.bisector\n")
            return(imput.copyMean.locfTraj(traj,model))
        }else{
            if(all(!is.na(traj))){return(traj)}else{}
        }
    }

    ## Compute these BEFORE imput.copyMean.center
    firstNoNA <- min(which(!is.na(traj)))
    lastNoNA <- max(which(!is.na(traj)))
    secondNoNA <- min(which(!is.na(traj[-firstNoNA])))+1
    penultimateNoNA <- max(which(!is.na(traj[-lastNoNA])))
    trajLength <- length(traj)

    traj <- imput.copyMean.centerTraj(traj,model)

    xA <- firstNoNA       ; yA <- traj[xA] ; zA <- model[xA]
    xB <- lastNoNA        ; yB <- traj[xB] ; zB <- model[xB]
    xC <- secondNoNA      ; yC <- traj[xC] ; zC <- model[xC]
    xD <- penultimateNoNA ; yD <- traj[xD] ; zD <- model[xD]

    ## Begin
    lineLeft  <- bisector(xA,yA,xB,yB,xC,yC)
    modelLeft <- bisector(xA,zA,xB,zB,xC,zC)
    indNA <- 1:firstNoNA
    traj[indNA] <- lineLeft[1]*indNA+lineLeft[2] + model[indNA] - (modelLeft[1]*indNA+modelLeft[2])

    ## End
    lineRight  <- bisector(xB,yB,xA,yA,xD,yD)
    modelRight <- bisector(xB,zB,xA,zA,xD,zD)
    indNA <- lastNoNA:trajLength
    traj[indNA] <- lineRight[1]*indNA+lineRight[2] + model[indNA] - (modelRight[1]*indNA+modelRight[2])

    return(traj)
}


imput.copyMean.bisector <- function(longData){

    ## Préparation de la trajectoire moyenne.
    ## En particulier, imputation si manquantes
    model <- apply(longData,2,mean,na.rm=TRUE)

    if(all(is.na(model))){
        warning("[Imputation:copyMean.bisector] There is only NA in the model, impossible to impute the model\n")
        return(longData)
    }else{
        if(any(is.na(model))){
            warning("[Imputation:copyMean.bisector] There is NA in the model. linearInterpol.bisector is used to complete\n")
            model <- imput.linearInterpol.bisector(t(model))
        }else{}
    }

    ## Imputation
    return(t(apply(longData,1,imput.copyMean.bisectorTraj,model)))
}

