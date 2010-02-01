source("../R/plotLongData.R")


cat("###################################################################
#################### Test  LongData & Partition ###################
############################### plot ##############################
###################################################################\n")
cleanProg(.LongData.partition.plot,,,2) # LETTERSletters meanNA
par(mfrow=c(3,3))
plot(ld3)
plot(ld3,p3a)
plot(ld3,p3b,col="clusters")
plot(ld3,p3c,col="red")
plot(ld3,p3a,col.mean="black",pch.mean="symbols")
plot(ld3,p3a,col.mean="black",pch.mean="letters")
plot(ld3,p3a,type="n",type.mean="n")
plot(ld3,p3a,type.mean="p",cex.mean=3)

par(mfrow=c(3,4))
plot(ld4)
plot(ld4,p4a)
plot(ld4,p4b,type="n")
plot(ld4,p4c,col=1)
plot(ld4,p4a,col="clusters")
plot(ld4,p4a,type.mean="n")
plot(ld4,p4a,col.mean=1,pch.time=c(1,2,4,20))
plot(ld4,p4a,col.mean="clusters",pch.time=matrix(c(0,1,3,4,16,20),2))
plot(ld4,p4a,col.mean=1,pch.mean="symbols")
plot(ld4,p4a,col="clusters",type.mean="n",pch.mean="letters")
plot(ld4,p4a,type="n",type.mean="n")
plot(ld4,p4a,col="clusters",type.mean="o",pch.mean="letters",size=3)

par(mfrow=c(3,3))
plot(ld3,p3a,subGroups="A")
plot(ld3,p3a,subGroups="C")
plot(ld3,p3a,subGroups=c("A","C"))
plot(ld3,p3a,subGroups=c("A","C"),col.mean="clusters")
plot(ld3,p3a,subGroups=c("A","C"),col.mean=1)
plot(ld3,p3a,type="n",subGroups=c("A","C"),type.mean="n")
plot(ld3,p3a,type="n",subGroups=c("A","C"),type.mean="o")
plot(ld3,p3a,type="n",subGroups=c("A","C"),type.mean="p")
plot(ld3,p3a,type="n",subGroups=c("A","C","B"),type.mean="p")


par(mfrow=c(3,4))
plot(ld1)
plot(ld1,col=1)
plot(ld2)
plot(ld2n)
plot(ld3)
plot(ld3n,col=1,col.mean=1)
plot(ld4,col=1)
plot(ld4n,col=1,pch="symbols")
plot(ld4n,col=1)
plot(ld5,col=1)
plot(ld5,pch.mean=2)
plot(ld5n)




cat("###################################################################
#################### Test  LongData & Partition ###################
############################ plotSubGroup #########################
###################################################################\n")

cleanProg(.LongData.plotSubGroups,,,2) # LETTERSletters meanNA
dev.off()
plotSubGroups(ld3,p3a)
plotSubGroups(ld3,p3b,col="clusters")
plotSubGroups(ld3,p3c,col="red")
plotSubGroups(ld3,p3a,col.mean="black",pch.mean="symbols")
plotSubGroups(ld3,p3a,col.mean="black",pch.mean="letters")
plotSubGroups(ld3,p3a,type="n",type.mean="n")
plotSubGroups(ld3,p3a,type.mean="p",cex.mean=3)

plotSubGroups(ld4,p4a)
plotSubGroups(ld4,p4b,type="n")
plotSubGroups(ld4,p4c,col=1)
plotSubGroups(ld4,p4a,col="clusters")
plotSubGroups(ld4,p4a,type.mean="n")
plotSubGroups(ld4,p4a,col.mean=1)
plotSubGroups(ld4,p4a,col.mean="clusters")
plotSubGroups(ld4,p4a,col.mean=1,pch.mean="symbols")
plotSubGroups(ld4,p4a,col="clusters",type.mean="n",pch.mean="letters")
plotSubGroups(ld4,p4a,type="n",type.mean="n")
plotSubGroups(ld4,p4a,col="clusters",type.mean="o",pch.mean="letters",size=3)

plotSubGroups(ld3)
plotSubGroups(ld3,p3a,subGroups="A")
plotSubGroups(ld3,p3a,subGroups="C")
plotSubGroups(ld3,p3a,subGroups=c("A","C"))
plotSubGroups(ld3,p3a,subGroups=c("A","C"),col.mean="clusters")
plotSubGroups(ld3,p3a,subGroups=c("A","C"),col.mean=1)
plotSubGroups(ld3,p3a,type="n",subGroups=c("A","C"),type.mean="n")
plotSubGroups(ld3,p3a,type="n",subGroups=c("A","C"),type.mean="o")
plotSubGroups(ld3,p3a,type="n",subGroups=c("A","C"),type.mean="p")
plotSubGroups(ld3,p3a,type="n",subGroups=c("A","C","B"),type.mean="p")


plotSubGroups(ld1)
plotSubGroups(ld1,col=1)
plotSubGroups(ld2)
plotSubGroups(ld2n)
plotSubGroups(ld3)
plotSubGroups(ld3n,col=1,col.mean=1)
plotSubGroups(ld4,col=1)
plotSubGroups(ld4n,col=1,pch="symbols")
plotSubGroups(ld4n,col=1)
plotSubGroups(ld5,col=1)
plotSubGroups(ld5,pch.mean=2)
plotSubGroups(ld5n)



#electTrajNoNA(ld1n)
#electTrajNoNA(ld2n)
#electTrajNoNA(ld3n)
#selectTrajNoNA(ld3n)
#selectTrajNoNA(ld3n)
#selectTrajNoNA(ld3n)
#try(selectTrajNoNA(ld4n))
#selectTrajNoNA(ld4n)
#selectTrajNoNA(ld5n)
#selectTrajNoNA(ld5n)

cat("\n++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
+++++++++++++++++++ Fin Test  LongData-Partition +++++++++++++++++++
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++\n")





#choicePlot(ld4,p4a)
