cat("\n####################################################################
############################## Function ############################
####################################################################")

cat("\n### Functions accepting NA ###")
meanNA <- function(x){mean(x,na.rm=TRUE)}
medianNA <- function(x){median(x,na.rm=TRUE)}

sdNA   <- function(x){sd(c(x),na.rm=TRUE)}
sdcNA <- function(x){leng<-length(x);sd(c(x),na.rm=TRUE)*sqrt((leng-1)/leng)}
varNA   <- function(x){var(c(x),na.rm=TRUE)}
rangeNA   <- function(x){range(x,na.rm=TRUE)}

which.minNA <- function(x){
  y <- which.min(x)
  if(length(y)==0){y<-NA}
  return(y)
}

### TRUE for Truly NA : false for NaN
is.tna <- function(x){
    if(length(x)==0){
        return(TRUE)
    }else{
        if(is.list(x)){x <- unlist(x)}else{}
        return(is.na(x)&!is.nan(x))
    }
}


### Printing long line shortening them
catShort <- function(x){
    if(length(x)<=10){
        cat(x)
    }else{
        cat(x[1:10],"...")
    }
}

#NAtrunc <- function(x) x[1:max(which(!is.na(x)))]



cat("\n++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
++++++++++++++++++++++++++++ Fin Function ++++++++++++++++++++++++++
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++\n")



