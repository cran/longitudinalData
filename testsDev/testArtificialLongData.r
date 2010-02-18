par(mfrow=c(2,3))
part3 <- partition(rep(LETTERS[1:3],each=50))
part4 <- partition(rep(LETTERS[1:4],each=50))

aldB1 <- generateArtificialLongData(functionNoise=function(t){rnorm(1,0,1)})
plot(aldB1,part4,col="clusters",type.mean="n")

aldB3 <- generateArtificialLongData()
plot(aldB3,part4,type.mean="b")

aldB7 <- generateArtificialLongData(functionNoise=function(t){rnorm(1,0,7)})
plot(aldB7,part4)

aldA3 <- generateArtificialLongData(time=0:7,nbEachClusters=c(50,50,50),
    functionClusters=list(function(x){0},function(x){x},function(x){-x}),
    functionNoise=function(t){rnorm(1,0,3)}
)
plot(aldA3,part3)

aldC3 <- generateArtificialLongData(time=0:6,nbEachClusters=c(50,50,50),
    functionClusters=list(function(x){2},function(x){10},function(x){12-2*x}),
    functionNoise=function(t){rnorm(1,0,3)}
)
plot(aldC3,part3)

aldD3 <- generateArtificialLongData(time=5:45,nbEachClusters=c(50,50,50,50),
    functionClusters=list(function(x){50*dnorm(x,20,2)},function(x){50*dnorm(x,25,2)},function(x){50*dnorm(x,30,2)},function(x){30*dnorm(x,25,5)}),
    functionNoise=function(t){rnorm(1,0,3)}
)
plot(aldD3,part3)
