library(codetools)
cleanProg <- function(realResult,theoResult="",result=TRUE,tolerance=0){
  functionNames <- strsplit(deparse(substitute(realResult)),"\\(")[[1]][1]
  if(identical(theoResult,"")==FALSE){
    if( isTRUE(all.equal( realResult , theoResult ))!=result ){
      cat("WARNING(PreTest2) in    ",functionNames,":",deparse(substitute(realResult)), " == ",theoResult," is not ",result,"\a\n\a")
    }
  }else{}
  if(length(findGlobals(get(functionNames),FALSE)$variables)  > tolerance){
    cat("WARNIGS(detectGlobal) in ",functionNames,": These are the globals:",findGlobals(get(functionNames),FALSE)$variables,"\a\n")
  }else{}
}



source("../R/global.r")
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

