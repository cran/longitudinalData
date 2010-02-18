library("clv")
source("./testImputation.r")
source("../R/criterion.r")


cat("\n++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
++++++++++++++++++++++++++ Test criterion ++++++++++++++++++++++++++
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++\n")
cleanProg(.LongData.quality.criterion,,,1) # LETTERS
cleanProg(.matrix.quality.criterion,,,1) # LETTERS

criterion(ld2,p2a)
criterion(ld3,p3a)
criterion(ld2n,p2a,"LOCF")
criterion(ld3n,p3a,"copyMean")
criterion(ld4n,p4cn,"copyMean")



cat("\n++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
++++++++++++++++++++++++ Fin Test criterion ++++++++++++++++++++++++
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++\n")


