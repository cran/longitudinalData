ld5["traj"]->ld

ldA <- ld[c(1,3,5,7),]
ldB <- ld[c(1,3,5,7)+1,]
apply(ldA,2,mean)->mA
apply(ldB,2,mean)->mB
distA <- function(x){dist(rbind(x,mA))}
distB <- function(x){dist(rbind(x,mB))}
intra <- 1/8*(distA(ldA[1,])^2+distA(ldA[2,])^2+distA(ldA[3,])^2+distA(ldA[4,])^2+
distB(ldB[1,])^2+distB(ldB[2,])^2+distB(ldB[3,])^2+distB(ldB[4,])^2)
inter <- dist(rbind(mA,mB))
(inter/intra)

ld5["traj"]->ld
ldA <- ld[c(1,4,7),]
ldB <- ld[c(2,5,8),]
ldC <- ld[c(3,6),]
apply(ldA,2,mean)->mA
apply(ldB,2,mean)->mB
apply(ldC,2,mean)->mC
distA <- function(x){dist(rbind(x,mA))}
distB <- function(x){dist(rbind(x,mB))}
distC <- function(x){dist(rbind(x,mC))}
intra <- 1/8*(
   distA(ldA[1,])^2+distA(ldA[2,])^2+distA(ldA[3,])^2+
   distB(ldB[1,])^2+distB(ldB[2,])^2+distB(ldB[3,])^2+
   distC(ldC[1,])^2+distC(ldC[2,])^2)
inter <- min(dist(rbind(mA,mB)),dist(rbind(mC,mB)),dist(rbind(mA,mC)))
(inter/intra)


intraclust = c("average")
interclust = c("average")
     

index.list <- cls.scatt
intracls <- intraclust
intercls <- interclust

    if (class(index.list) != cls.class()) 
        stop("Bad input data: 'index.list' is not an object created by function 'cls.scatt.measures(..)'\n\t\t\t\tor 'cls.scatt.measures.diss.mx(..)' .")
    idx = index.list
    if (length(index.list) == 7) {
        intra.bool = check.intracls.diss.mx.method(intracls)
        inter.bool = check.intercls.diss.mx.method(intercls)
        intra.name = c("comp", "ave")
        inter.name = c("sin", "comp", "ave", "haus")
        intra.list = list(idx$intracls.complete, idx$intracls.average)[intra.bool]
        inter.list = list(idx$intercls.single, idx$intercls.complete, 
            idx$intercls.average, idx$intercls.hausdorff)[inter.bool]
    }else if (length(index.list) == 11) {
        intra.bool = check.intracls.method(intracls)
        inter.bool = check.intercls.method(intercls)
        intra.name = c("comp", "ave", "cent")
        inter.name = c("sin", "comp", "ave", "cent", "aveto", 
            "haus")
        intra.list = list(idx$intracls.complete, idx$intracls.average, 
            idx$intracls.centroid)[intra.bool]
        inter.list = list(idx$intercls.single, idx$intercls.complete, 
            idx$intercls.average, idx$intercls.centroid, idx$intercls.ave_to_cent, 
            idx$intercls.hausdorff)[inter.bool]
    }
    else stop("Bad input data: 'index.list' is not an object created by function 'cls.scatt.measures(..)'\n\t\t\t\t\tor 'cls.scatt.measures.diss.mx(..)' .")
    intra.num = length(intra.bool[intra.bool])
    inter.num = length(inter.bool[inter.bool])
    not.empty.cls = (idx$cluster.size > 0)
    clust_num = length(not.empty.cls[not.empty.cls])
    for (i in 1:intra.num) intra.list[[i]] = intra.list[[i]][not.empty.cls]
    for (i in 1:inter.num) inter.list[[i]] = inter.list[[i]][not.empty.cls, 
        not.empty.cls]
    result = matrix(0, inter.num, intra.num)
    for (i in 1:inter.num) for (j in 1:intra.num) result[i, j] = clv.DB.ind(intra.list[[j]],inter.list[[i]], clust_num)
    rownames(result) = inter.name[inter.bool]
    colnames(result) = intra.name[intra.bool]





