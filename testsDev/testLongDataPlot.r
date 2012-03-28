#source("testDistance.r")
source("../R/longDataPlot.R")


cat("###################################################################
#################### Test  LongData & Partition ###################
############################### plot ##############################
###################################################################\n")


cleanProg(calculTrajMean,,,1) # tapply
m1 <- calculTrajMean(ld3["traj"],p3a["clusters"])
calculTrajMean(ld3["traj"],p3b["clusters"])
calculTrajMean(ld4["traj"],p4c["clusters"])
m2 <- calculTrajMean(ld3["traj"],p3c["clusters"],centerMethod=function(x){median(x,na.rm=TRUE)})

m3 <-  calculTrajMean3d(LD4["traj"],p4a["clusters"])
m4 <- calculTrajMean3d(LD4["traj"],p4c["clusters"])
calculTrajMean3d(LD5["traj"],p5b["clusters"],centerMethod=function(x){median(x,na.rm=TRUE)})


cleanProg(calculTrajMeanPoint)
calculTrajMeanPoint(m1,3)
calculTrajMeanPoint(m2,5)
calculTrajMeanPoint3d(m3,0)
calculTrajMeanPoint3d(m4,100)

cleanProg(legendCol)
for(i in 1:26)cat(i,"->",legendCol(i)," ; ",sep="")


cleanProg(.longData.plot,,,0)
cleanProg(.longData.Partition.plot,,,1) # LETTERS

plot(ld3)
plot(ld3,parTraj=parTRAJ(col="red"),parWin=windowsCut(1,addLegend=TRUE))
plot(ld3,p3a,parWin=windowsCut(2,addLegend=TRUE))
plot(ld3,p3a,parWin=windowsCut(3,addLegend=TRUE))
plot(ld3,p3b,parTraj=parTRAJ(col="clusters"))
plot(ld3,p3c,parTraj=parTRAJ(col="red",type="b",pch=2))

plot(ld3,p3a,parTraj=parTRAJ(type="n"),parMean=parMEAN(col="black"))
plot(ld3,p3b,parMean=parMEAN(col="clusters",pch="symbols"))
plot(ld3,p3c,parMean=parMEAN(col="clusters",pch="symbols",pchPeriod=Inf))
plot(ld3,p3a,parMean=parMEAN(col="black"))
plot(ld3,p3a,parMean=parMEAN(col="black",pch="letters"))
plot(ld3,p3a,parMean=parMEAN(pchPeriod=3,cex=2))

plot(ld1)
plot(ld1n,parTraj=parTRAJ(col="red"))
plot(ld2,p2a)
plot(ld3n,p3b,parTraj=parTRAJ(col="clusters"))
plot(ld4n,parTraj=parTRAJ(col="red",type="b",pch=2))
plot(ld5,parTraj=parTRAJ(type="o",pch=1:3))
plot(ld4n,parTraj=parTRAJ(col=1:3,type="b",pch='letters'))

plot(LD1,p1b,parWin=windowsCut(c(2,2)))
plot(LD3n,p3a,parWin=windowsCut(c(1,3)))
plot(LD5,p5c,parTraj=parTRAJ(col=(rep(2:7,20)),type="b",pch="letters"))
plot(LD4,p4a)

plot(LD1,p1a,parTraj=parTRAJ(type="n"))
plot(LD1n,p1b,parMean=parMEAN(col="clusters",pch="symbols"))
plot(LD2,p2c,parMean=parMEAN(col="clusters",pch="symbols",pchPeriod=Inf))
plot(LD2n,p2b,parMean=parMEAN(col="black"))
plot(LD3n,p3a,parMean=parMEAN(col="black",pch="letters"))



## cat("###################################################################
## #################### Test  LongData & Partition ###################
## ############################ plotSubGroup #########################
## ###################################################################\n")

## cleanProg(.LongData.plotSubGroups,,,2) # LETTERSletters meanNA
## dev.off()
## plotSubGroups(ld3,p3a)
## plotSubGroups(ld3,p3b,col="clusters")
## plotSubGroups(ld3,p3c,col="red")
## plotSubGroups(ld3,p3a,col.mean="black",pch.mean="symbols")
## plotSubGroups(ld3,p3a,col.mean="black",pch.mean="letters")
## plotSubGroups(ld3,p3a,type="n",type.mean="n")
## plotSubGroups(ld3,p3a,type.mean="p",cex.mean=3)

## plotSubGroups(ld4,p4a)
## plotSubGroups(ld4,p4b,type="n")
## plotSubGroups(ld4,p4c,col=1)
## plotSubGroups(ld4,p4a,col="clusters")
## plotSubGroups(ld4,p4a,type.mean="n")
## plotSubGroups(ld4,p4a,col.mean=1)
## plotSubGroups(ld4,p4a,col.mean="clusters")
## plotSubGroups(ld4,p4a,col.mean=1,pch.mean="symbols")
## plotSubGroups(ld4,p4a,col="clusters",type.mean="n",pch.mean="letters")
## plotSubGroups(ld4,p4a,type="n",type.mean="n")
## plotSubGroups(ld4,p4a,col="clusters",type.mean="o",pch.mean="letters",size=3)

## plotSubGroups(ld3)
## plotSubGroups(ld3,p3a,subGroups="A")
## plotSubGroups(ld3,p3a,subGroups="C")
## plotSubGroups(ld3,p3a,subGroups=c("A","C"))
## plotSubGroups(ld3,p3a,subGroups=c("A","C"),col.mean="clusters")
## plotSubGroups(ld3,p3a,subGroups=c("A","C"),col.mean=1)
## plotSubGroups(ld3,p3a,type="n",subGroups=c("A","C"),type.mean="n")
## plotSubGroups(ld3,p3a,type="n",subGroups=c("A","C"),type.mean="o")
## plotSubGroups(ld3,p3a,type="n",subGroups=c("A","C"),type.mean="p")
## plotSubGroups(ld3,p3a,type="n",subGroups=c("A","C","B"),type.mean="p")


## plotSubGroups(ld1)
## plotSubGroups(ld1,col=1)
## plotSubGroups(ld2)
## plotSubGroups(ld2n)
## plotSubGroups(ld3)
## plotSubGroups(ld3n,col=1,col.mean=1)
## plotSubGroups(ld4,col=1)
## plotSubGroups(ld4n,col=1,pch="symbols")
## plotSubGroups(ld4n,col=1)
## plotSubGroups(ld5,col=1)
## plotSubGroups(ld5,pch.mean=2)
## plotSubGroups(ld5n)



cat("###################################################################
########################## Test  LongData #########################
############################## plot3d #############################
###################################################################\n")


cleanProg(adjustGraph3d)
adjustGraph3d("ER","ty")

cleanProg(.LongData3d.plot3d)
plot3d(LD1)
plot3d(LD3n)
plot3d(LD4,parTraj=parTRAJ(col=(rep(2:7,20))))
plot3d(LD5n,nbSample=200)
#plot3d(LD8n,nbSample=Inf)
plot3d(LD4)

plot3d(LD7,varY=5,varZ=7)
plot3d(LD7,varY="V3")
tryBug(plot3d(LD4,varY=30))
tryBug(plot3d(LD4,varY="VT4"))

cleanProg(.LongData3d.Partition.plot3d,,,1)
plot3d(LD1,p1a)
plot3d(LD3,p3d)
plot3d(LD3,p3b)
plot3d(LD7,p3b,parTraj=parTRAJ(col=(rep(2:7,20))))
plot3d(LD7,p3b,varY=5,varZ=7)
plot3d(LD7,p3c,varY="V3")
tryBug(plot3d(LD4,p4a,varY=30))
tryBug(plot3d(LD4,p4b,varY="VT4"))

plot3d(LD5,p5a,parTraj=parTRAJ(col=(rep(2:7,400)),type="b",pch="letters"),nbSample=100)
plot3d(LD5,p5b,parTraj=parTRAJ(col=(rep(2:7,400)),type="b",pch="symbols"),nbSample=100)

cat("###################################################################
########################## Test LongData ##########################
############################ plot3dPdf ############################
###################################################################\n")

cleanProg(misc3dPlan)
misc3dPlan(c(1,1,1),c(1,2,1),c(2,1,1))

cleanProg(misc3dPave)
misc3dPave(c(1,1,1),c(1,2,1),c(2,1,1),c(1,1,3))

cleanProg(misc3dLine)
misc3dLine(c(1,1,1),c(1,2,1))

cleanProg(misc3dLines)
misc3dLines(c(1,1,2,1),c(1,2,3,1),c(4,3,2,1))

cleanProg(plot3dPdf)
sce <- plot3dPdf(LD3n,p3a)

cleanProg(saveTrianglesAsASY)
saveTrianglesAsASY(sce)

cleanProg(makeLatexFile)
makeLatexFile()

cat("\n++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
+++++++++++++++++++ Fin Test  LongData-Partition +++++++++++++++++++
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++\n")





#choicePlot(LD4,p4a)