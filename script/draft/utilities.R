###############################################################################################
## 1. check columns with NAs ##################################################################
###############################################################################################
CheckColNAs <- function(dt){
    intNAs <- apply(dt, 2, function (x) sum(is.na(x)))
    intColNAs <- intNAs[intNAs != 0]
    return(intColNAs)
}