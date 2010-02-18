#setwd("C:/Documents and Settings/Christophe/Mes documents/Recherche/Trajectoires/kmeal/longitudinalData/src")
source("testCriterion.r")
dyn.load("../src/longitudinalData.dll")
source("../R/distanceFrechet.r")
#dyn.unload("distanceTraj.dll")


#dyn.load("distanceTraj.dll")

traj <- matrix(1:12,3,4,byrow=TRUE)
.C("printMatrix",mTraj=as.numeric(t(traj)),nbCol=as.integer(4),nbLigne=as.integer(3))

for(i in 1:1000){
    cat("XXXXXXXXXXX",i,"XXXXXXXXXXXX\n");
    x <- rnorm(25)
    x[floor(runif(12,1,26))] <- NA
    y <- rnorm(25)
    y[floor(runif(12,1,26))] <- NA

    a <- .C("distance",x=as.numeric(x),y=as.numeric(y),taille=as.integer(length(x)),
            power=as.numeric(1),dist=as.numeric(0),NAOK=TRUE)
    b <- dist(rbind(x,y),method="manhattan")
    c <- distTraj(x,y,method="manhattan")
    print(a)
    print(b)
    if(abs(a$dist-b)>1e-12 || abs(b-c)>1e-12){cat("\a");stop();}else{}

    a <- .C("distance",x=as.numeric(x),y=as.numeric(y),taille=as.integer(length(x)),
            power=as.numeric(2),dist=as.numeric(0),NAOK=TRUE)
    b <- dist(rbind(x,y),method="euclidean")
    c <- distTraj(x,y,method="euclidean")
    print(a)
    print(b)
    if(abs(a$dist-b)>1e-12 || abs(b-c)>1e-12){cat("\a");stop();}else{}

    a <- .C("distance",x=as.numeric(x),y=as.numeric(y),taille=as.integer(length(x)),
            power=as.numeric(Inf),dist=as.numeric(0),NAOK=TRUE)
    b <- dist(rbind(x,y),method="maximum")
    c <- distTraj(x,y,method="maximum")
    print(a)
    print(b)
    if(abs(a$dist-b)>1e-12 || abs(b-c)>1e-12){cat("\a");stop();}else{}

    a <- .C("distance",x=as.numeric(x),y=as.numeric(y),taille=as.integer(length(x)),
            power=as.numeric(1.8),dist=as.numeric(0),NAOK=TRUE)
    b <- dist(rbind(x,y),method="minkowski",p=1.8)
    c <- distTraj(x,y,method="minkowski",p=1.8)
    print(a)
    print(b)
    if(abs(a$dist-b)>1e-12 || abs(b-c)>1e-12){cat("\a");stop();}else{}

    a <- .C("distance",x=as.numeric(x),y=as.numeric(y),taille=as.integer(length(x)),
            power=as.numeric(-1),dist=as.numeric(0),NAOK=TRUE)
    b <- dist(rbind(x,y),method="canberra")
    c <- distTraj(x,y,method="canberra")
  #  print(a)
 #   print(b)
#    if(abs(a$dist-b)>1e-12 || abs(b-c)>1e-12){cat("\a");stop();}else{}
}

for(i in 1:1000){
    cat("XXXXXXXXXXX",i,"XXXXXXXXXXXX\n");
    x <- floor(runif(25,0,2))
    x[floor(runif(12,1,26))] <- NA
    y <- floor(runif(25,0,2))
    y[floor(runif(12,1,26))] <- NA

    a <- .C("distance",x=as.numeric(x),y=as.numeric(y),taille=as.integer(length(x)),
            power=as.numeric(-2),dist=as.numeric(0),NAOK=TRUE)
    b <- dist(rbind(x,y),method="binary")
    c <- distTraj(x,y,method="binary")
    print(a)
    print(b)
    if(abs(a$dist-b)>1e-12 || abs(b-c)>1e-12){cat("\a");stop();}else{}
}


for(i in 1:10){
    cat("XXXXXXXXXXX",i,"XXXXXXXXXXXX\n");
    P <- rnorm(7)
    Q <- rnorm(6)

    print(a <- distFrechetR(P,Q))
    print(b <- distFrechetRec(P,Q))
    print(c <- distFrechet(P,Q))
    if(abs(a-c)>1e-12 || abs(b-c)>1e-12){cat("\a");stop();}else{}

    print(a <- distFrechetR(P,Q,method="sum"))
    print(b <- distFrechetRec(P,Q,method="sum"))
    print(c <- distFrechet(P,Q,method="sum"))
    if(abs(a-c)>1e-12 || abs(b-c)>1e-12){cat("\a");stop();}else{}

    print(a <- distFrechetR(P,Q,Fdist="1D" ))
    print(b <- distFrechetRec(P,Q,Fdist="1D"))
    print(c <- distFrechet(P,Q ,Fdist="1D"))
    if(abs(a-c)>1e-12 || abs(b-c)>1e-12){cat("\a");stop();}else{}

    print(a <- distFrechetR(P,Q,Fdist="1D",method="sum"))
    print(b <- distFrechetRec(P,Q,Fdist="1D",method="sum"))
    print(c <- distFrechet(P,Q,Fdist="1D",method="sum"))
    if(abs(a-c)>1e-12 || abs(b-c)>1e-12){cat("\a");stop();}else{}
}


for(i in 1:1000){
    cat("XXXXXXXXXXX",i,"XXXXXXXXXXXX\n");
    P <- rnorm(7)
    Q <- rnorm(6)

    print(a <- pathFrechetR(P,Q))
    print(c <- pathFrechet(P,Q))
    if(abs(a$dist-c$dist)>1e-12){cat("\a");stop();}else{}

    print(a <- pathFrechetR(P,Q,method="sum"))
    print(c <- pathFrechet(P,Q,method="sum"))
    if(abs(a$dist-c$dist)>1e-12){cat("\a");stop();}else{}

    print(a <- pathFrechetR(P,Q,Fdist="1D" ))
    print(c <- pathFrechet(P,Q ,Fdist="1D"))
    if(abs(a$dist-c$dist)>1e-12){cat("\a");stop();}else{}

    print(a <- pathFrechetR(P,Q,Fdist="1D",method="sum"))
    print(c <- pathFrechet(P,Q,Fdist="1D",method="sum"))
    if(abs(a$dist-c$dist)>1e-12){cat("\a");stop();}else{}
}
