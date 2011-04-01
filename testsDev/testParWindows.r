source("../R/parWindows.r")

cat("\n####################################################################
########################## Test ParWindows #########################
####################################################################\n")

cleanProg(parWindows)
cleanProg(windowsCut)
cleanProg(.parWindows.show)

new("ParWindows")
parWindows(2,3,FALSE,TRUE)
parWindows(2,3,TRUE,FALSE)

windowsCut(5)
windowsCut(5,FALSE)
windowsCut(c(2,3))
windowsCut(c(1,3),FALSE)
windowsCut(c(3,1))
pwin <- windowsCut(c(3,1),FALSE)

pwin['nbCol']
pwin['nbRow']
pwin['addLegend']
pwin['screenMatrix']

cat("\n++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
+++++++++++++++++++++++ Fin Test ParWindows ++++++++++++++++++++++++
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++\n")
