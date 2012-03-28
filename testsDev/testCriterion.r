library("clv")
source("./testImputation.r")
source("../R/criterion.r")


cat("\n++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
++++++++++++++++++++++++++ Test criterion ++++++++++++++++++++++++++
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++\n")
cleanProg(resizePartition)
resizePartition(ld2n,p2a)
resizePartition(ld2n,p2b)
resizePartition(ld2n,p2bn)


cleanProg(.qualityCriterion.matrix)
cleanProg(.qualityCriterion.longData)

qualityCriterion(ld2,p2a)
qualityCriterion(ld3,p3a)
qualityCriterion(ld3n,p3a,"copyMean")
qualityCriterion(ld3n,p3b,"copyMean")
tryBug(qualityCriterion(ld4n,p3a,"copyMean"))



cat("\n++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
++++++++++++++++++++++++ Fin Test criterion ++++++++++++++++++++++++
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++\n")


