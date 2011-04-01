source("../R/parLongData.r")

cat("\n####################################################################
######################### Test ParLongData #########################
####################################################################\n")

cleanProg(.ParLongData.validity)
cleanProg(parLongData)
cleanProg(parTRAJ)
cleanProg(parMEAN)
cleanProg(.ParLongData.show)
cleanProg(.parLongData.Partition.expand,,,1) # LETTERS
cleanProg(.parLongData.nbClusters.expand,,,1) # LETTERS

new("ParLongData",col="red",type="b",pch="clusters")
#parLongData()
parTRAJ()
parMEAN()
par1 <- parTRAJ(col="red")
par2 <- parMEAN(type="n")

par1['col']
par1['col']<-"blue"

expandParLongData(par2,p3a)
par2['pch']<-"symbols"
expandParLongData(par2,p3a)
par2['pch']<-"A"
expandParLongData(par2,p2a)

par2['pch']<-"letters"
expandParLongData(par2,3)
par2['pch']<-"symbols"
expandParLongData(par2,5)
par2['pch']<-"A"
expandParLongData(par2,2)

cat("\n++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
+++++++++++++++++++++++ Fin Test ParLongData +++++++++++++++++++++++
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++\n")
