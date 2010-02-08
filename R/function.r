### Functions accepting NA
meanNA <- function(x){mean(x,na.rm=TRUE)}
medianNA <- function(x){median(x,na.rm=TRUE)}

sdNA   <- function(x){sd(x,na.rm=TRUE)}

which.minNA <- function(x){
  y <- which.min(x)
  if(length(y)==0){y<-NA}
  return(y)
}

### TRUE for Truly NA : false for NaN
is.tna <- function(x){return(is.na(x)&!is.nan(x))}


### Printing long line shortening them
catShort <- function(x){
    if(length(x)<=10){
        cat(x)
    }else{
        cat(x[1:10],"...")
    }
}

LETTERSletters <- c(LETTERS,letters)
METHODS <- c("manhattan", "euclidean", "minkowski", "maximum", "canberra", "binary")

NAtrunc <- function(x) x[1:max(which(!is.na(x)))]
