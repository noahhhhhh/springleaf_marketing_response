###############################################################################################
## 1. check columns with NAs ##################################################################
###############################################################################################
CheckColNAs <- function(dt){
    intNAs <- apply(dt, 2, function (x) sum(is.na(x)))
    intColNAs <- intNAs[intNAs != 0]
    return(intColNAs)
}

###############################################################################################
## 2. check the column levels #################################################################
###############################################################################################
CheckColLevels <- function(dt, colNames){
    intColLevels <- as.integer()
    names <- as.character()
    for (colName in colNames){
        intColLevel <- dim(table(dt[, colName, with = F]))
        intColLevels <- c(intColLevels, intColLevel)
        
        name <- colName
        names <- c(names, name)
    }
    names(intColLevels) <- names
    return(intColLevels)
}

###############################################################################################
## 3. check the column levels #################################################################
###############################################################################################