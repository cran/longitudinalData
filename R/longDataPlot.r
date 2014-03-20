
cat("+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
++++++++++++++++++++++++++ Class LongData +++++++++++++++++++++++++
+++++++++++++++++++++++++++++++ plot ++++++++++++++++++++++++++++++
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++\n")

### Possibilité de passer a parTraj une liste de parTRAJ ? Puis de passer une liste a un élément
### qui serait dupliquée ?

cat("### Method: 'plot' pour LongData et Partition, divers ###\n")



### Calcule les trajectoires moyennes de chaque clusters.
### A noter, traj est un array et part est un vecteur
### NE FONCTIONNE PAS POUR LES PARTITIONS A UN SEUL CLUSTER

calculTrajMean <- function(traj,clust,centerMethod=function(x){mean(x,na.rm=TRUE)}){
    trajMean <- apply(traj, 2, tapply, clust, centerMethod)
    return(trajMean)
}

calculTrajMean3d <- function(traj,clust,centerMethod=function(x){mean(x,na.rm=TRUE)}){
    trajMean <- apply(traj, c(2,3), tapply, clust, centerMethod)
    return(trajMean)
}

### Exclut de la matrice des trajectoires moyennes les valeurs des points qui ne doivent pas être imprimé.
### trajMeanPoint est la matrice des trajectoires moyenne.
### pchPeriod précise la période d'apparition d'un point sur une courbe.
###   - si pchPeriod=0, tous les points sont plotés
###   - si pchPeriod>1, les points sont plotés tous les 'nbClusters*pchPeriod', avec un décalage initial

## calculTrajMeanPoint <- function(trajMeanPoint,nbClusters,nbTime,pchPeriod){
##     period <- nbClusters*pchPeriod
##     if(period>=1){
##         if(period<nbTime){
##             for(i in 1:nbClusters){
##                 toKeep <- seq(from=pchPeriod*(i-1)%%nbTime+1,to=nbTime,by=period)
##                 trajMeanPoint[i,(1:nbTime)[c(-toKeep)],]<-NA
##             }
##         }else{
##             toKeep <- round(seq(from=1,to=nbTime,length.out=nbClusters))
##             for(i in 1:nbClusters){
##                 trajMeanPoint[i,-toKeep[i],] <- NA
##             }
##         }
##     }else{}
##     return(trajMeanPoint)
## }

calculTrajMeanPoint <- function(trajMeanPoint,pchPeriod){
    nbClusters <- nrow(trajMeanPoint)
    nbTime <- ncol(trajMeanPoint)
    period <- nbClusters*pchPeriod
    if(period>=1){
        if(period<nbTime){
            for(i in 1:nbClusters){
                toKeep <- seq(from=pchPeriod*(i-1)%%nbTime+1,to=nbTime,by=period)
                trajMeanPoint[i,(1:nbTime)[c(-toKeep)]]<-NA
            }
        }else{
            toKeep <- round(seq(from=1,to=nbTime,length.out=nbClusters))
            for(i in 1:nbClusters){
                trajMeanPoint[i,-toKeep[i]] <- NA
            }
        }
    }else{}
    return(trajMeanPoint)
}

calculTrajMeanPoint3d <- function(trajMeanPoint,pchPeriod){
    nbClusters <- nrow(trajMeanPoint)
    nbTime <- ncol(trajMeanPoint)
    period <- nbClusters*pchPeriod
    if(period>=1){
        if(period<nbTime){
            for(i in 1:nbClusters){
                toKeep <- seq(from=pchPeriod*(i-1)%%nbTime+1,to=nbTime,by=period)
                trajMeanPoint[i,(1:nbTime)[c(-toKeep)],]<-NA
            }
        }else{
            toKeep <- round(seq(from=1,to=nbTime,length.out=nbClusters))
            for(i in 1:nbClusters){
                trajMeanPoint[i,-toKeep[i],] <- NA
            }
        }
    }else{}
    return(trajMeanPoint)
}

legendCol <- function(nbVar){
    if(nbVar<6){return(nbVar)}else{
        if(nbVar %in% c(6)){return(3)}else{
            if(nbVar %in% c(7,8,11,12)){return(4)}else{
                if(nbVar %in% c(9,10,13:15)){return(5)}else{
                    if(nbVar %in% c(16:18,21:24)){return(6)}else{
                        return(7)}}}}}
}


cat("### Method: 'plot' pour LongData3d sans Partition ###\n")

.longData3d.plotTraj <- function(x,parTraj=parTRAJ(),parWin=windowsCut(x['nbVar'],addLegend=FALSE),nbSample=Inf,...){
    ## Duplication de la couleur (équivalent a expandParLongData)
    ## Comme il n'y a pas de partition, 'clusters' devient 'black'
    if(identical(parTraj['col'],'clusters')){parTraj['col']<-'black'}else{}
    if(length(parTraj['col'])==1){parTraj['col'] <- rep(parTraj['col'],length(x['idFewNA']))}else{}

    ## Si nbSample est grand, l'instruction suivante a tout de meme pour effet de mélanger l'ordre des trajectoires
    nbSample <- min(nbSample,length(x['idFewNA']))
    toKeep <- sample(1:length(x['idFewNA']),nbSample)

    ## Calcul du layout
    listScreen<-split.screen(parWin['screenMatrix'])

    for (i in 1:x['nbVar']){
        screen(listScreen[i])
        par(mar=c(3,4,2,1))
        matplot(x['time'],t(x['traj'][toKeep,,i]),type=parTraj['type'],col=parTraj['col'][toKeep],lty=1,
                pch=parTraj['pch'],cex=parTraj['cex'],xlab="",ylab=x['varNames'][i],...)
    }
    if(parWin['closeScreen']){
        close.screen(listScreen)
        return(invisible())
    }else{
        return(listScreen)
    }
}
setMethod("plotTraj",signature=c(x="LongData3d",y="missing"),.longData3d.plotTraj)


cat("### Method: 'plot' pour LongData3d et Partition ###\n")

.longData3d.Partition.plotTraj <- function(x,y,parTraj=parTRAJ(),parMean=parMEAN(),parWin=windowsCut(x['nbVar'],addLegend=TRUE),nbSample=1000,...){
    ## ############################# Preparation ############################# ##
    nbVar <- x['nbVar']
    nbTime <- x['nbTime']
    traj <- x['traj']

    ## Gestion de la partition
    ### TROP COMPLIQUE : Il faut modifier calculTrajMean, mais aussi t(trajMean[,,i])
    #if(missing(y)){y <- partition(clusters=rep("A",length(x["idFewNA"])),nbClusters=1)}else{}

    nbClusters <- y['nbClusters']

    ## Vérification que la partition est de la bonne taille.
#    y <- resizePartition(x,y)
    part <- factor(y['clusters'],levels=LETTERS[1:nbClusters])

    ## Prépare les ParLongData en fonction de la partition :
    parTraj <- expandParLongData(parTraj,y)
    parMean <- expandParLongData(parMean,nbClusters)

    ## si besoin, calcul des trajectoires moyennes et des points a placer
    if(parMean['type']!="n"){
        trajMean <- calculTrajMean3d(traj,part)#,nbClusters,nbTime,nbVar)
        if( parMean['type']%in%c("p","b","o")){# & !identical(parMean['pch'],NA) ){
#            trajMeanPoint <- calculTrajMeanPoint(trajMean,nbClusters,nbTime,parMean['pchPeriod'])
            trajMeanPoint <- calculTrajMeanPoint3d(trajMean,parMean['pchPeriod'])
        }else{}
    }else{}

    ## Si nbSample est grand, l'instruction suivante a tout de meme pour effet de mélanger l'ordre des trajectoires
    nbSample <- min(nbSample,x['nbIdFewNA'])
    toKeep <- sample(1:x['nbIdFewNA'],nbSample)

    ## ############################# Tracé ############################# ##

    ## Calcul du layout
    listScreen<-split.screen(parWin['screenMatrix'])

    for (i in 1:nbVar){
        screen(listScreen[i])
        par(mar=c(3,4,2,1))
        matplot(x['time'],t(traj[toKeep,,i]),type=parTraj['type'],col=parTraj['col'][toKeep],lty=1,
                pch=parTraj['pch'],cex=parTraj['cex'],xlab="",ylab=x['varNames'][i],...)

        ## Tracé des moyennes avec ou sans symbols
        if(parMean['type'] %in% c("l","b","c","o","h","s","S")){
            matlines(x['time'],t(trajMean[,,i]),col=1,lwd=8,lty=1,type="l")
            matlines(x['time'],t(trajMean[,,i]),col=parMean['col'],lwd=4,lty=1,type="l")
        }else{}

        ## Tracé des points
        if(parMean['type'] %in% c("b","c")){
            par(bg="white")
            matlines(x['time'],t(trajMeanPoint[,,i]),col=0,type="p",pch=19,cex=parMean['cex']*2.5)
        }else{}
        if(parMean['type'] %in% c("p","b","o")){
            matlines(x['time'],t(trajMeanPoint[,,i]),col=parMean['col'],type="p",pch=parMean['pch'],cex=parMean['cex'])
        }else{}
    }

    ## Tracé éventuelle de la legend
    if(parWin['addLegend']){
        percent <- paste(": ",formatC(as.numeric(table(part)/length(part)*100),digits=3),"%",sep="")
        screen(listScreen[length(listScreen)],FALSE)
#        legend(grconvertX(0.5,'npc'), grconvertY(ifelse(parWin["nbRow"]==1,1.3,1.05),'npc')^2,percent,lty=1,col=parMean['col'],pch=parMean['pch'],
 #              ncol=legendCol(nbClusters),xpd=NA,xjust=0.5,yjust=0.5,inset=-0.1)
        par(mar=c(0,0,0,0))
        plot(1,axes=FALSE,type="n",xlab="",ylab="",xlim=c(-1,1),ylim=c(-1,1))
        legend(x=0,y=1,legend=percent,lty=1,col=parMean['col'],pch=parMean['pch'],
               ncol=legendCol(nbClusters),xpd=NA,xjust=0.5,inset=-0.1)
    }
    close.screen(listScreen)
}
setMethod("plotTraj",signature=c(x="LongData3d",y="Partition"),.longData3d.Partition.plotTraj)


cat("### Method: 'plot' pour LongData, avec ou sans Partition ###\n")

.longData.Partition.plotTraj <- function(x,y,parTraj=parTRAJ(),parMean=parMEAN(),parWin=windowsCut(x['nbVar'],addLegend=TRUE),nbSample=1000,...){
    plotTraj(longDataTo3d(x),y=y,parTraj=parTraj,parMean=parMean,parWin=parWin,nbSample=nbSample,...)
}

setMethod("plotTraj",signature=c(x="LongData",y="Partition"),.longData.Partition.plotTraj)

.longData.plotTraj <- function(x,parTraj=parTRAJ(),parWin=windowsCut(x['nbVar'],addLegend=TRUE),nbSample=1000,...){
    plotTraj(longDataTo3d(x),parTraj=parTraj,parWin=parWin,nbSample=nbSample,...)
}

setMethod("plotTraj",signature=c(x="LongData",y="missing"),.longData.plotTraj)




cat("###################################################################
########################## Class LongData #########################
############################## plot3d #############################
###################################################################\n")




### Affichage les axes et redimentionne des graphes 3D
### varNames1 et varNames2 sont les noms des variables qui sont représentées
adjustGraph3d <- function(varName1,varName2){
    axes3d(c('x','y','z'))
    title3d(,,"Time",varName1,varName2)
    box3d()
    aspect3d(c(2,1,1))
    rgl.viewpoint(0,-90,zoom=1.2)
}

.LongData3d.plotTraj3d <- function(x,y,varY=1,varZ=2,parTraj=parTRAJ(),parMean=parMEAN(),nbSample=1000,...){
    ## Duplication de la couleur (équivalent a expandParLongData)
    if(identical(parTraj['col'],'clusters')){parTraj['col']<-'black'}else{}
    if(length(parTraj['col'])==1){parTraj['col'] <- rep(parTraj['col'],x['nbIdFewNA'])}else{color<-parTraj['col']}

    ## Si nbSample est grand, l'instruction suivante a tout de meme pour effet de mélanger l'ordre des trajectoires
    nbSample <- min(nbSample,x['nbIdFewNA'])
    toKeep <- sample(1:x['nbIdFewNA'],nbSample)

    varY <- varNumAndName(varY,x['varNames'])
    varZ <- varNumAndName(varZ,x['varNames'])

    traj3d <- array(c(rep(x['time'],each=nbSample),x['traj'][toKeep,,c(varY$num,varZ$num)]), dim = c(nbSample,x['nbTime'],3))

    open3d()
    if(parTraj['type']!="n"){
        for (i in 1:nbSample){lines3d(traj3d[i, , ], col = parTraj['col'][toKeep[i]],lwd=1)}
    }else{}

     adjustGraph3d(varY$name,varZ$name)
}
setMethod("plotTraj3d",signature=c("LongData3d","missing"),.LongData3d.plotTraj3d)




.LongData3d.Partition.plotTraj3d <- function(x,y,varY=1,varZ=2,parTraj=parTRAJ(type="n"),parMean=parMEAN(),nbSample=1000,...){
    ## ############################# Preparation ############################# ##
    nbVar <- x['nbVar']
    nbTime <- x['nbTime']
    nbClusters <- y['nbClusters']

    ## Gestion de la partition
    ## TROP compliqué, cf plot3d
    ## if(missing(y)){y <- partition(clusters=rep("A",length(x["idFewNA"])),nbClusters=1)}else{}

    ## Vérification que la partition est de la bonne taille.
#    y <- resizePartition(x,y)
    part <- factor(y['clusters'],levels=LETTERS[1:nbClusters])

    ## Prépare les ParLongData en fonction de la partition :
    parTraj <- expandParLongData(parTraj,y)
    parMean <- expandParLongData(parMean,nbClusters)

    ## si besoin, calcul des trajectoires moyennes et des points a placer
    if(parMean['type']!="n"){
        trajMean <- calculTrajMean3d(x['traj'],part)#,nbClusters,nbTime,nbVar)
        if( parMean['type']%in%c("p","b","o")){# & !identical(parMean['pch'],NA) ){
#            trajMeanPoint <- calculTrajMeanPoint(trajMean,nbClusters,nbTime,parMean['pchPeriod'])
            trajMeanPoint <- calculTrajMeanPoint3d(trajMean,parMean['pchPeriod'])
        }else{}
    }else{}

    ## Si nbSample est grand, l'instruction suivante a tout de meme pour effet de mélanger l'ordre des trajectoires
    nbSample <- min(nbSample,x['nbIdFewNA'])
    toKeep <- sample(1:x['nbIdFewNA'],nbSample)

    varY <- varNumAndName(varY,x['varNames'])
    varZ <- varNumAndName(varZ,x['varNames'])

    ## ############################# Tracé ############################# ##
    open3d()

    ## matrice des trajectoires
    traj3d <- array(c(rep(x['time'],each=nbSample),x['traj'][toKeep,,c(varY$num,varZ$num)]), dim = c(nbSample,nbTime,3))
    if(parTraj['type']!="n"){
        for (i in 1:nbSample){lines3d(traj3d[i, , ], col = parTraj['col'][toKeep[i]],lwd=1)}
    }else{}

    ## matrice des moyennes
    if(parMean['type']!="n"){
        mean3d <- array(c(rep(x['time'],each=nbClusters),trajMean[,,c(varY$num,varZ$num)]), dim = c(nbClusters,nbTime,3))
        for (i in 1:nbClusters){
            if(!all(is.na(mean3d[i,,-1]))){lines3d(mean3d[i, , ], col = parMean['col'][i],lwd=5)}else{}
        }
    }else{}

    adjustGraph3d(varY$name,varZ$name)
}
setMethod("plotTraj3d",signature=c("LongData3d","Partition"),.LongData3d.Partition.plotTraj3d)






cat("###################################################################
########################## Class LongData #########################
############################# plot3dPdf ###########################
###################################################################\n")


misc3dPoint <- function(A,r,color="black",alpha=1){
   t1 <- c(A[1]+r,A[2],A[3],A[1],A[2]+r,A[3],A[1],A[2],A[3]+r)
   t2 <- c(A[1]+r,A[2],A[3],A[1],A[2]+r,A[3],A[1],A[2],A[3]-r)
   t3 <- c(A[1]+r,A[2],A[3],A[1],A[2]-r,A[3],A[1],A[2],A[3]+r)
   t4 <- c(A[1]+r,A[2],A[3],A[1],A[2]-r,A[3],A[1],A[2],A[3]-r)
   t5 <- c(A[1]-r,A[2],A[3],A[1],A[2]+r,A[3],A[1],A[2],A[3]+r)
   t6 <- c(A[1]-r,A[2],A[3],A[1],A[2]+r,A[3],A[1],A[2],A[3]-r)
   t7 <- c(A[1]-r,A[2],A[3],A[1],A[2]-r,A[3],A[1],A[2],A[3]+r)
   t8 <- c(A[1]-r,A[2],A[3],A[1],A[2]-r,A[3],A[1],A[2],A[3]-r)
   return(data.frame(rbind(t1,t2,t3,t4,t5,t6,t7,t8),color=color,alpha=alpha))
}

misc3dPoints <- function(MA,r,color="black",alpha=1){
   dataV <- data.frame()
   if(length(r)==1){r <- rep(r,ncol(MA))}else{}
   if(length(color)==1){color <- rep(color,ncol(MA))}else{}
   if(length(alpha)==1){alpha <- rep(alpha,ncol(MA))}else{}
   for(i in 1:ncol(MA)){
       dataV <- rbind(dataV,misc3dPoint(MA[,i],r[i],color=color[i],alpha=alpha[i]))
   }
   return(dataV)
}

misc3dPlan <- function(A,Ax,Ay){
   v1 <- matrix(c(A,Ax),ncol=3,byrow=TRUE)
   v2 <- matrix(c(Ax,Ay),ncol=3,byrow=TRUE)
   v3 <- matrix(c(Ay,Ax+Ay-A),ncol=3,byrow=TRUE)
   return(data.frame(v1=v1,v2=v2,v3=v3))
}

misc3dPave <- function(A,Ax,Ay,Az,color="black",alpha=1){
    Axy <- Ax+Ay-A
    Axz <- Ax+Az-A
    Ayz <- Ay+Az-A
    dataV <- cbind(rbind(misc3dPlan(A,Ax,Ay),misc3dPlan(A,Ax,Az),misc3dPlan(A,Ay,Az),
         misc3dPlan(Az,Axz,Ayz),misc3dPlan(Ax,Axz,Axy),misc3dPlan(Ay,Axy,Ayz)),color=color,alpha=alpha)
    return(dataV)
}

misc3dLine <- function(A,B,color="black",alpha=0.8,lwd=0.05){
    misc3dPave(A-c(0,0,lwd),B-c(0,0,lwd),A-c(0,lwd,0),A+c(0,lwd,0),color=color,alpha=alpha)
}

misc3dLines <- function(x,y,z,color="black",alpha=0.8,lwd=0.05){
    dataV <- data.frame()#misc3dLine(A=c(x[1],y[1],z[1]),B=c(x[2],y[2],z[2]),color=color,alpha=alpha,lwd=lwd)
    for(i in 2:length(x)){
	dataV <- rbind(dataV,misc3dLine(A=c(x[i-1],y[i-1],z[i-1]),B=c(x[i],y[i],z[i]),color=color,alpha=alpha,lwd=lwd))
    }
    return(dataV)
}





.LongData3d.Partition.plot3dPdf <- function(x,y,varY=1,varZ=2){
    ## ############################# Preparation ############################# ##
    time <- x['time']
    nbClusters <- y['nbClusters']

    ## Vérification que la partition est de la bonne taille.
#    y <- resizePartition(x,y)
    part <- factor(y['clusters'],levels=LETTERS[1:nbClusters])

    ## Calcul des trajectoires moyennes et des points a placer
    trajMean <- calculTrajMean3d(x['traj'],part)#,nbClusters,nbTime,nbVar)

    ## Gestion du choix des variables
    varY <- varNumAndName(varY,x['varNames'])
    varZ <- varNumAndName(varZ,x['varNames'])

    ## Normalisation
    xx <- (time-min(time))/(max(time)-min(time))
    yy <- (trajMean[,,varY$num]-min(trajMean[,,varY$num]))/(max(trajMean[,,varY$num])-min(trajMean[,,varY$num]))*0.5
    zz <- (trajMean[,,varZ$num]-min(trajMean[,,varZ$num]))/(max(trajMean[,,varZ$num])-min(trajMean[,,varZ$num]))*0.5

    ## ############################# Tracé ############################# ##

    ## Cadre
    triangles <- rbind(
        misc3dPave(A=c(0,0,0),Ax=c(1,0,0),Ay=c(0,0.5,0),Az=c(0,0,0.5),alpha=0.02),
        misc3dLine(A=c(0,0,0),B=c(1,0,0),lwd=0.01),
        misc3dLine(A=c(0,0,0),B=c(0,0.5,0),lwd=0.01),
        misc3dLine(A=c(0,0,0),B=c(0,0,0.5),lwd=0.01),

        misc3dLine(A=c(0,0.5,-0.04),B=c(0,0.56,0.04),lwd=0.005),
        misc3dLine(A=c(0,0.5,0.04),B=c(0,0.53,0),lwd=0.005),

        misc3dLine(A=c(0,-0.03,0.60),B=c(0,0.03,0.60),lwd=0.005),
        misc3dLine(A=c(0,0.03,0.60),B=c(0,-0.03,0.53),lwd=0.005),
        misc3dLine(A=c(0,-0.03,0.53),B=c(0,0.03,0.53),lwd=0.005)
    )

    ## Tracé des moyennes
    colorMean <- rainbow(dim(trajMean)[1])
    for(i in 1:dim(trajMean)[1]){triangles <- rbind(triangles,misc3dLines(xx,yy[i,],zz[i,],color=colorMean[i],alpha=0.8,lwd=0.01))}
    return(makeTriangles(v1=as.matrix(triangles[,1:3]),v2=as.matrix(triangles[,4:6]),v3=as.matrix(triangles[,7:9]),
                         alpha=triangles$alpha,color=triangles$color))
}
setMethod("plot3dPdf",signature=c("LongData3d","Partition"),.LongData3d.Partition.plot3dPdf)



saveTrianglesAsASY <- function(scene, filename = "scene.asy") {
    scene <- misc3d:::colorScene(scene)
    triangles <- misc3d:::canonicalizeAndMergeScene(scene, "color",
                                                    "color2", "alpha",
                                                    "col.mesh", "fill",
                                                    "smooth")
    ve <- misc3d:::t2ve(triangles)
    f <- file(filename, open = "w")
    on.exit(close(f))

    ## write out header information and vertices
    cat("//generated by saveTrianglesAsASY\n\n",
        "import three;\n\n",
        "size(20cm);\n\n",
        "//currentprojection=perspective(250,-250,250);\n",
        "currentlight=Viewport;\n\n",
        "typedef path3[] trimesh;\n\n",
        "// Vertices\n",
        "triple[] V;\n",
        sep = "", file = f)

    nv <- ncol(ve$vb)
    x <- ve$vb[1,]
    y <- ve$vb[2,]
    z <- ve$vb[3,]
    for (i in 1 : nv)
        cat(sprintf("V[%d] = (%f, %f, %f);\n",
                    i - 1, x[i], y[i], z[i]), file = f)

    ## write out the faces
    cat("\n",
        "guide3 triface_(int i, int j, int k) {\n",
        "  guide3 gh; gh=V[i-1]--V[j-1]--V[k-1]--cycle;\n",
        "  return gh;\n",
        "};\n\n",
        "// Faces\n",
        "trimesh F;\n",
        sep = "", file = f)

    nf <- ncol(ve$ib)
    v1 <- ve$ib[1,]
    v2 <- ve$ib[2,]
    v3 <- ve$ib[3,]
    for (i in 1 : nf)
        cat(sprintf("F[%d] = triface_(%d, %d, %d);\n",
                    i - 1, v1[i], v2[i], v3[i]), file = f)

    ## write out color and transparency values
    cat("\n",
        "// Colors\n",
        "material M[];\n",
        sep = "", file = f)

    cols <- col2rgb(triangles$color)
    alpha <- triangles$alpha
    r <- cols[1,]
    g <- cols[2,]
    b <- cols[3,]
    if (any(alpha < 1))
        for (i in 1 : nf)
            cat(sprintf("M[%d] = rgb(%f, %f, %f) + opacity(%f);\n",
                        i - 1, r[i], g[i], b[i], alpha[i]),
                file = f)
    else
        for (i in 1 : nf)
            cat(sprintf("M[%d] = rgb(%f, %f, %f);\n",
                        i - 1, r[i], g[i], b[i]),
                file = f)

    cat("\ndraw(surface(F), M);\n", file = f)
    invisible(NULL)
}


makeLatexFile <- function(filename="main.tex",asyToInclude="scene+0.prc"){
    f <- file(filename, open = "w")
    on.exit(close(f))

    cat("\\documentclass{article}

\\usepackage[colorlinks=true]{hyperref}
\\usepackage[3D]{movie15}

\\begin{document}
\\includemovie[
   poster,toolbar,
   3Dcoo=0 -0.4 -113,
   3Dc2c=-60 10.4 -3,
   3Droo=61,
   3Daac=30,
   3Droll=-73,
   3Dlights=CAD
]{\\linewidth}{\\linewidth}{",asyToInclude,"}
\\end{document}",
        sep="",file=f)
    return(invisible(NULL))
}


cat("\n-------------------------------------------------------------------
------------------------ Class LongData plot ----------------------
------------------------------- Fin -------------------------------
-------------------------------------------------------------------\n")
