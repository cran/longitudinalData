library("clv")
source("../R/partition.r")
cleanProg(.qualityCriterion.matrix,,,4) # meanNA prod sdcNA sum

qualityCriterion(ld2["traj"],c(1,2,1,2,2))
qualityCriterion(ld2["traj"],as.integer(c(1,2,1,2,2)))
qualityCriterion(ld2["traj"],as.numeric(c(1,2,1,2,2)))
qualityCriterion(ld4["traj"],rep(1:4,50))
qualityCriterion(ld4["traj"],rep(1:4,each=50))
qualityCriterion(ld4n["traj"],rep(1:4,each=50)[1:ld4n["nbIdFewNA"]],"copyMean")
qualityCriterion(ld4n["traj"],rep(1:4,each=50)[1:ld4n["nbIdFewNA"]],"copyMean")
qualityCriterion(LD4["traj"],rep(1:4,50))

tryBug(qualityCriterion(LD4["traj"],rep(1:4,25)))


cat("####################################################################
########################## Test  Partition #########################
####################################################################\n")



cleanProg(.partition.validity,,,2)  # LETTERS MAX_CLUSTERS
new("Partition") # Doit marcher meme apres recompilation
new("Partition",clusters=as.factor(c("A","B","A")),nbClusters=2)
new("Partition",clusters=as.factor(c("A","B","A")),nbClusters=4)
new("Partition",clusters=as.factor(c("A","M","A")),nbClusters=20)
new("Partition",clusters=as.factor(c("A","B",NA,"A")),nbClusters=3)
new("Partition",clusters=as.factor(c(NA,NA)),nbClusters=3)
new("Partition",clusters=as.factor(c("A","B","A")),nbClusters=26)
new("Partition",clusters=as.factor(c("A","B","B")),nbClusters=26)
new("Partition",clusters=as.factor(c("A","A","A")),nbClusters=1)

#ordered(new("Partition",clusters=as.factor(c("A","B","B")),nbClusters=26))

tryBug(new("Partition",clusters=factor(c("A","C","A"),levels=LETTERS[1:3]),nbClusters=2))
new("Partition",clusters=as.factor(c("A","B","D")),nbClusters=4)
new("Partition",clusters=as.factor(c("A","C",NA,"A")),nbClusters=3)
new("Partition",clusters=as.factor(c("A","C",NA,"A")),nbClusters=20)
tryBug(new("Partition",clusters=as.factor(c("A","C",NA,"M")),nbClusters=3))
tryBug(new("Partition",clusters=factor(c("A","C",NA,"aa")),nbClusters=3))



cleanProg(.partition.constructor,,,7) # CRITERION_NAMES LETTERS MAX_CLUSTERS meanNA prod sdcNA sum
cleanProg(.partition.constructor3d,,,) # CRITERION_NAMES LETTERS MAX_CLUSTERS meanNA prod sdcNA sum
cleanProg(.partition.show,,,1) # LETTERS

partition()
partition(clusters=c("A","B"))
partition(clusters=c("A",NA))
tryBug(partition(nbClusters=3))
tryBug(partition(clusters=c("C","B","C"),nbClusters=4))
partition(clusters=c("C","B","C"))
tryBug(partition(clusters=c(NA,NA)))
partition(clusters=c(1,3,1))


cat("### Jeux de données ###\n")
##cleanProg(.partition.ordered,,,1) # LETTERS
p0a <- p0b <- partition()

p1a <- partition(clusters=c("A","B","B"))
p1b <- partition(clusters=c("A","B","A"),ld1["traj"])
p1b <- partition(clusters=c("A","B","A"),ld1n["traj"])
p1c <- partition(clusters=c("A","C","B"),LD1["traj"])
p1d <- partition(clusters=c("A","C","A"),ld2n["traj"])
p1d <- partition(clusters=c("A","C","A"),ld2n["traj"])

p2a <- partition(clusters=c("A","A","B","A","B"))
(p2b <- partition(clusters=c("A","B","A","A","B"),ld2["traj"],details=c(convergenceTime="3",algorithm="kml",aze=4,multiplicity="4")))
p2c <- partition(clusters=c("A","C","B","A","B"),LD2["traj"])

p2an <- partition(clusters=c("A",NA,"B",NA,"B"))
p2bn <- partition(clusters=c("A",NA,NA,"B",NA))
tryBug(p2cn <- partition(clusters=c(NA,NA,NA,NA,NA)))

p3a <- partition(clusters=rep(LETTERS[1:2],4))
p3b <- partition(clusters=rep(LETTERS[c(1:3,1:3,1:2)]),ld3n["traj"])
p3c <- partition(clusters=rep(LETTERS[1:4],each=2),LD3["traj"])
p3d <- partition(clusters=rep(LETTERS[1:4],2),LD3["traj"])
p3e <- partition(clusters=rep(c(4,4,1,3,2,3,3,2)),ld3["traj"])
p3f <- partition(clusters=rep(c(1,1,1,3,3,3,3,2)),ld3["traj"])
p3g <- partition(clusters=rep(c(6,5,4,3,2,3,1,2)),ld3["traj"])
p3h <- partition(clusters=rep(c(2,2,1,2,2,3,3,2)),ld3["traj"])
p3i <- partition(clusters=rep(c(1,2,3,3,3,3,3,2)),ld3["traj"])
p3j <- partition(clusters=rep(c(1,2,1,3,4,3,4,2)),ld3["traj"])

p4a <- partition(clusters=c(rep(1:2,each=100)))
p4b <- partition(clusters=c(rep(1:3,c(50,30,120))),ld4["traj"])
tryBug(p4b["clusters"][1:6]<-"B")
tryBug(p4b["clusters"][7:9]<-"C")
p4c <- partition(clusters=c(rep(c(1:4,2:3,2:3),25)),ld4["traj"])
p4d <- partition(clusters=c(rep(c(3,2:4,1,3:5),25)),LD4["traj"])
p4e <- partition(clusters=c(rep(c(1:6,2,2),25)),ld4["traj"])
p4f <- partition(clusters=c(rep(c(1:7,4),25)),LD4["traj"])

p5a <- partition(clusters=LETTERS[rep(c(1,2),1000)])
p5b <- partition(clusters=LETTERS[c(rep(c(1,2,3),666),1:2)],ld5["traj"]) ###PROBLEME DE VRAISEMBLANCE NaN
p5c <- partition(clusters=rep(1:4,500),LD5["traj"])
p7e <- partition(clusters=c(rep(1:6,333),1,2),ld5["traj"])
p7g <- partition(clusters=c(rep(1:8,250)),LD5["traj"])




cat("\n####################################################################
########################## Test  Partition #########################
############################# Accesseurs ###########################
####################################################################\n")

p0a["nbClusters"]
p1a["nbClusters"]
p1b["nbClusters"]
p1c["nbClusters"]
p2a["nbClusters"]
p2bn["nbClusters"]
p2c["nbClusters"]
p3a["nbClusters"]
p3b["nbClusters"]
p3c["nbClusters"]
p4a["nbClusters"]
p4c["nbClusters"]
p4e["nbClusters"]
p5a["nbClusters"]
p5b["nbClusters"]


p0a["clusters"]
p1a["clusters"]
p1b["clusters"]
p1c["clusters"]
p2an["clusters"]
p2bn["clusters"]
p2c["clusters"]
p3a["clusters"]
p3b["clusters"]
p3c["clusters"]
p4b["clusters"]
p4e["clusters"]
p5a["clusters"]
p5b["clusters"]
p5c["clusters"]

p2a["clustersAsInteger"]
p3b["clustersAsInteger"]
p4e["clustersAsInteger"]

p1a["percentEachCluster"]
p2b["percentEachCluster"]
p3c["percentEachCluster"]
p4a["percentEachCluster"]
p5b["percentEachCluster"]


p1b["criterionValues"]
p2c["criterionValues"]
p3a["criterionValues"]
p4b["criterionValues"]
p5c["criterionValues"]

p2b["convergenceTime"]
p2b["aze"]
tryBug(p2b["ert"])
p2b["Genolini.Calinski"]


p2b["multiplicity"] <- "6"
p2b["multiplicity"] <- 8


p5c["postProba"]


p5c["postProbaEachCluster"]
p5c["BIC"]
p5c["BIC2"]
p5c["AIC"]
p5c["AICc"]
p5c["postProbaGlobal"]


cat("\n####################################################################
########################## Test  Partition #########################
############################# Affichage ############################
####################################################################\n")

cleanProg(.partition.show,,,1) #LETTERS
p0a
p1a



cat("\n####################################################################
########################## Test  Partition #########################
############################### Autre ##############################
####################################################################\n")



qualityCriterion(ld1,p1c)
qualityCriterion(LD2,p2b)
qualityCriterion(ld3,p3c)
qualityCriterion(LD3,p3c)
qualityCriterion(ld4,p4a)
qualityCriterion(LD4,p4b)
qualityCriterion(ld5,p5c)
qualityCriterion(LD5,p5b)


cat("\n++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
++++++++++++++++++++++++ Fin Test Partition ++++++++++++++++++++++++
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++\n")
