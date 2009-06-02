cat("###################################################################
#################### Class LongData & Partition ###################
############################### plot ##############################
###################################################################\n")

### Definition of the layout. Principal draw is 1, the other are 2 to nbClusters. If there is more than 5 cluster, we use two lines
### col : soit une couleur, soit un vecteur de couleur, soit "clusters" (se qui colorie chaque clusters avec sa couleur).
### col.mean : même chose
### pch.mean : "symbols"
###

.LongData.partition.plot <- function(x,y,subGroups=LETTERSletters[1:y@nbClusters],
                                         type="l",type.mean="b",col="clusters",col.mean="clusters",
                                         lty=1,lty.mean=1,pch=1,pch.mean="letters",pch.time=NA,
                                         xlab="Time",ylab="",ylim=NA,cex.mean=1,legends=TRUE,sizeMin=2,...){
    # Gestion des valeurs "particulières" des parametres
    if(missing(y)){y <- partition(clusters=rep("A",length(x["id"])),nbClusters=1)}else{}
    if(identical(col,"clusters")){col <- match(subGroups,LETTERSletters)+1}else{}
    if(length(col)==1){col <- rep(col,y@nbClusters)}else{}
    if(identical(col.mean,"clusters")){col.mean <- match(subGroups,LETTERSletters)+1}else{}
    if(identical(pch.mean,"symbols")){pch.mean <- match(subGroups,LETTERSletters)+1}else{}
    if(identical(pch.mean,"letters")){pch.mean <- LETTERSletters[match(subGroups,LETTERSletters)]}else{}

    noNA <- selectSupTrajMinSize(x,sizeMin) # Selection des trajectoires a tracer.
    trajNoNA <- x@traj[noNA,]
    partNoNA <- y@clusters[noNA]

    # Percent est calculé avant l'exclusion des clusters non selectionnés
    percent <- paste(": ",formatC(table(partNoNA)/length(partNoNA)*100,digit=3),"%",sep="")[LETTERSletters[1:25]%in%subGroups]

    trajNoNA <- trajNoNA[partNoNA %in% subGroups,,drop=FALSE]
    partNoNA <- partNoNA[partNoNA %in% subGroups,drop=FALSE]

    if(length(partNoNA)==0){
        if(identical(ylim,NA)){ylim <- c(0,1)}else{}
        par("mar"=c(5,4,1,ifelse(legends,6,1)))
        plot(x@time,x@time,ylim=ylim,type="n",xaxt="n",xlab=xlab,ylab=ylab,...)
        axis(1,at=x@time,xlab=xlab,...)
        return(invisible())
    }else{}

    # si besoin, calcul des trajectoires moyennes et des points a placer
    if(type.mean!="n"){
        trajMeanPoint <- trajMean <- aggregate(trajNoNA,list(partNoNA),meanNA)[,-1]

        if(!identical(pch.time,NA)){
            if(!is.matrix(pch.time)){pch.time <- matrix(pch.time,y["nbClusters"],length(pch.time),byrow=TRUE)}else{}
            pch.timeExclu <- matrix(TRUE,nrow(trajMean),ncol(trajMean))
            for(i in 1:nrow(trajMean)){pch.timeExclu[i,] <- !x["time"]%in%pch.time[i,]}
            trajMeanPoint[pch.timeExclu] <- NA
        }else{}
    }else{}

    # calcul des limites de l'axe des y, selon qu'on trace les traj ou non.
    if(identical(ylim,NA)){
        if(type!="n"){
            ylim <- range(trajNoNA,na.rm=TRUE)
        }else{
            if(type.mean!="n"){
                ylim <- range(trajMean,na.rm=TRUE)
            }else{
                ylim <- c(0,1)
            }
        }
    }else{}

    # initialisation du graphe
    par("mar"=c(5,4,1,ifelse(legends,6,1)))
    plot(x@time,x@time,ylim=ylim,type="n",xaxt="n",xlab=xlab,ylab=ylab,...)
    axis(1,at=x@time,xlab=xlab,...)

    if(legends){
        legend(grconvertX(1.02,'npc'), grconvertY(0.5,'npc'),legend = percent,pch=pch.mean,xjust=0, yjust=0.4,horiz = FALSE, xpd = NA)
    }else{}

    # Tracé des trajectoires
        colorId <- col[as.integer(factor(partNoNA))]
        shuffle <- order(runif(nrow(trajNoNA)))
        matlines(x@time,t(trajNoNA[shuffle,,drop=FALSE]),type=type,col=colorId[shuffle],pch=pch,lty=lty,...)

    # Tracé des moyennes avec ou sans symbols
    if(type.mean %in% c("l","b","c","o","h","s","S")){
        matlines(x@time,t(trajMean),col=1,lwd=8,lty=lty.mean,type="l",...)
        matlines(x@time,t(trajMean),col=col.mean,lwd=4,lty=lty.mean,type="l",...)
    }else{}

    # Tracé des points
    if(type.mean %in% c("b","c")){
        par(bg="white")
        matlines(x@time,t(trajMeanPoint),col=0,type="p",pch=19,cex=cex.mean*2.5,...)
    }else{}
    if(type.mean %in% c("p","b","o")){
        matlines(x@time,t(trajMeanPoint),col=1,type="p",pch=pch.mean,cex=cex.mean,...)
    }else{}
    return(invisible())
}
cleanProg(.LongData.partition.plot,,,2) # LETTERSletters meanNA
setMethod("plotTraj",signature=c(x="LongData"),def=.LongData.partition.plot)
setMethod("plot",signature=c(x="LongData",y="ANY"),def=.LongData.partition.plot)
setMethod("plot",signature=c(x="LongData",y="missing"),def=.LongData.partition.plot)

rm(.LongData.partition.plot)


.LongData.plotSubGroups <- function(x,y,subGroups=LETTERSletters[1:y@nbClusters],
                                        type="l",type.mean="b",col="clusters",col.mean="clusters",
                                        lty=1,lty.mean=1,pch=1,pch.mean="letters",pch.time=NA,
                                        xlab="percent",ylab="",ylim=NA,cex.mean=1,legends=FALSE,sizeMin=2,...){
    if(missing(y)){y <- partition(clusters=rep("A",length(x["id"])),nbClusters=1)}else{}
    nbSubGroups <- length(subGroups)

    noNA <- selectSupTrajMinSize(x,2)
    trajNoNA <- x@traj[noNA,]
    partNoNA <- y@clusters[noNA]
    xlabAux <- xlab

    percent <- paste(formatC(table(partNoNA)/length(partNoNA)*100,digit=3),"%",sep="")[LETTERSletters[1:25]%in%subGroups]

    if(missing(ylim)){
        if(type!="n"){
            ylim <- range(trajNoNA,na.rm=TRUE)
        }else{
            trajMean <- aggregate(trajNoNA,list(partNoNA),meanNA)[,-1]
            ylim <- range(trajMean,na.rm=TRUE)
        }
    }else{}

    nbLignes <- ceiling(sqrt(nbSubGroups))
    nbCol <- ceiling(nbSubGroups/nbLignes)
    scr <- split.screen(c(nbLignes,nbCol))

    for(iCluster in 1:nbSubGroups){
        screen(scr[iCluster])
        if(identical(xlab,"percent")){xlabAux <- percent[iCluster]}else{}
        plot(x=x,y=y,subGroups=subGroups[iCluster],
             type=type,type.mean=type.mean,col=col,col.mean=col.mean,
             lty=lty,lty.mean=lty.mean,pch=pch,pch.mean=pch.mean,pch.time=pch.time,
             xlab=xlabAux,ylab=ylab,ylim=ylim,cex.mean=cex.mean,legends=legends,sizeMin=sizeMin,...)
    }
    close.screen(scr)
    return(invisible())
}
cleanProg(.LongData.plotSubGroups,,,2) # LETTERSletters meanNA
setMethod("plotSubGroups",signature=c(x="LongData"),def=.LongData.plotSubGroups)
rm(.LongData.plotSubGroups)













