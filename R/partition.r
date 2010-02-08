### partition est un découpage non associé a une longData
### Donc pas de calcul d'indices ou autre.
###  - nbCluster est le nombre de cluster
###  - cluster est la liste des clusters sous forme d'un facteur
###
### nbCluster doit être égal ou supérieur au nombre effectif de cluster
### cluster est a valeur dans A, B, C...
### A priori, une lettre peut être absente (a voir : classe-t-on les lettre dans l'ordre ?)

#cleanProg <- function(...){return(invisible())}
cat("####################################################################
########################## Class Partition #########################
############################# Creation #############################
####################################################################\n")

cat("### Definition ###\n")

.Partition.validity <- function(object){
#    cat("**** validity Partition ****\n")
    if(!(length(object@nbClusters)==0&length(object@clusters)==0)){#not empty object
        if(any(c(length(object@nbClusters)==0,length(object@clusters)==0))){
            stop("[Partition:validity]: at least one slot is empty")}else{}
        if(object@nbClusters > 52){
            stop("[Partition:validity]: More than 52 clusters")}else{}
        if(!all(na.omit(object@clusters)%in%LETTERSletters[1:object@nbClusters])){
            stop("[Partition:validity]: Clusters name out of range")}else{}
    }else{}
    return(TRUE)
}



setClass(
   Class="Partition",
   representation=representation(
      nbClusters = "numeric",
      clusters = "factor"
   ),
   prototype=prototype(
      nbClusters=numeric(),
      clusters=factor()
   ),
   validity=.Partition.validity
)

cat("\n####################################################################
########################## Class Partition #########################
############################ Constructeur ##########################
####################################################################\n")

#cleanProg(.Partition.initialize,,,1)     # LETTERSletters est une globale
#setMethod(f="initialize",signature="Partition",definition=.Partition.initialize)
#rm(.Partition.initialize)




#cat("### Initialize ###\n")
#.Partition.initialize <- function(.Object,nbClusters,id,clusters){
#    cat("*** initialize Partition ***\n")
#    if(missing(nbClusters) & missing(id) & missing(clusters)){
#        return(.Object)
#    }else{
#        if(missing(id)){id <- seq_along(clusters)}else{}
#        if(length(names(clusters))==0){names(clusters) <- id}else{}
#        if(missing(nbClusters)){ # cluster with no (implicit) nbCluster
#            nbClusters <- max(1,match(clusters,LETTERSletters[1:10]),na.rm=TRUE)
#        }else{
#            if(max(match(clusters,LETTERSletters[1:10]),na.rm=TRUE)>nbClusters){stop("[Partition:initiale] : More cluster than indicated in nbClusters")}else{}
#        }
#        .Object@nbClusters <- nbClusters
#        .Object@clusters <- factor(clusters,levels=LETTERSletters[1:.Object@nbClusters])
#     }else{
#        if(missing(nbClusters)){ # No cluster, no nbCluster -> empty object, no validation
#        }else{ # nbCluster but no cluster
#            .Object@nbClusters <- nbClusters
#        }
#    }
#    validObject(.Object)
#   return(.Object)
#}
#cleanProg(.Partition.initialize,,,1)     # LETTERSletters est une globale
#setMethod(f="initialize",signature="Partition",definition=.Partition.initialize)
#rm(.Partition.initialize)


setMethod("partition",signature=c("missing","missing"),function(clusters,nbClusters){new("Partition")})
setMethod("partition",signature=c("ANY","ANY"),
    function(clusters,nbClusters){
        if(missing(nbClusters)){
            nbClusters <- max(1,match(clusters,LETTERSletters),na.rm=TRUE)
        }else{}
        if(is.numeric(clusters)){
            clusters <- factor(LETTERSletters[clusters],levels=LETTERSletters[1:nbClusters])
        }else{
            clusters <- factor(clusters,levels=LETTERSletters[1:nbClusters])
        }
        new("Partition",
            nbClusters=nbClusters,
            clusters=clusters
        )
    }
)


setMethod("ordered",signature="Partition",
    function(x,...){
        clust <- factor(x@clusters,levels=LETTERSletters[1:x@nbClusters])
        new("Partition",
            nbClusters=x@nbClusters,
            clusters=factor(clust,
                levels=LETTERSletters[order(table(clust),decreasing=TRUE)],
                labels=LETTERSletters[1:x@nbClusters]
            )
        )
    }
)


cat("\n####################################################################
########################## Class Partition #########################
############################# Accesseurs ###########################
####################################################################\n")


cat("### Getteur ###\n")
setMethod("[","Partition",
    function(x,i,j,drop){
        switch(EXPR=i,
               "nbClusters"={return(x@nbClusters)},
               "k"={return(x@nbClusters)},
               "clusters"={return(x@clusters)},
               "clustersAsInteger"={return(as.integer(x@clusters))},
               stop("[Partition:getteur]: there is not such a slot in Partition")
        )
    }
)

cat("### Setteur ###\n")
setReplaceMethod("[","Partition",
    function(x,i,j,value){
        switch(EXPR=i,
               "nbClusters"={x@nbClusters<-value},
               "k"={x@nbClusters<-value},
               "clusters"={
                   if(is.numeric(value)){
                       x@clusters <- factor(LETTERSletters[value],levels=LETTERSletters[1:x@nbClusters])
                   }else{
                       x@clusters <- factor(value,levels=LETTERSletters[1:x@nbClusters])
                   }
               },
            stop("[Partition:setteur]: this is not a Partition slot")
        )
        validObject(x)
        return(x)
    }
)



cat("\n####################################################################
########################## Class Partition #########################
############################# Affichage ############################
####################################################################\n")

cat("### Method : 'show' for partition ###\n") # Si on ajouter un titre a traj, on pourra afficher 'associate traj ='
.Partition.show <- function(object){
    cat("   ~~~ Class :",class(object),"~~~ ")
    cat("\n ~ nbClusters : ",object@nbClusters)
    cat("\n ~ clusters   : [",length(object@clusters),"]",sep="")
    if(length(object@nbClusters)!=0){
        for (iCluster in LETTERSletters[1:object@nbClusters]){
            toKeep <- iCluster==object@clusters
            cat("\n    ",iCluster," : [",sum(toKeep,na.rm=TRUE),"] ",sep="")
            catShort((1:length(object@clusters))[toKeep & !is.na(toKeep)])
        }
        cat("\n   <NA> : [",sum(is.na(object@clusters)),"] ",sep="")
        catShort((1:length(object@clusters))[is.na(object@clusters)])
        cat("\n")
    }else{
        cat("\n     <empty Partition>\n")
    }
    return(invisible(object))
}
setMethod(f="show",signature="Partition",definition=.Partition.show)



cat("\n####################################################################
########################## Class Partition #########################
############################### Autre ##############################
####################################################################\n")


