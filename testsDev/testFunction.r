source("../R/function.r")

### .meanNA
cleanProg(meanNA(c(2,3,4)),3,TRUE,0)
cleanProg(meanNA(c(2,3,NA)),2.5,TRUE,0)
cleanProg(meanNA(c(NA,NA)),NaN,TRUE,0)
#cleanProg(meanNA(c()),NA,TRUE,0)


### .sdNA
cleanProg(sdNA(c(NA,2,3,4)),1,TRUE,0)

### .which.minNA
cleanProg(which.minNA(c(1:3,0.4,3),4,,0))
cleanProg(which.minNA(c(1:3,NA,0.4,3),4,,0))
cleanProg(which.minNA(c(1:3,NA,1,3),1,,0))
cleanProg(which.minNA(c(NA),1,,0))
cleanProg(which.minNA(c(NA),1,,0))

### .is.tna
cleanProg(is.tna(c(2,4,NA,NaN)),c(F,F,T,F),TRUE,0)


cat("\n++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
+++++++++++++++++++++++++ Fin Test Function ++++++++++++++++++++++++
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++\n")

