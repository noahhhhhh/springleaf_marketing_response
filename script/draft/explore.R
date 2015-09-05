require(data.table) # fread and data.table
require(bit64) # dealing with integer64 issue when fread
###############################################################################################
## 1. load the data ###########################################################################
###############################################################################################
load("RData/dtRaw.RData")

###############################################################################################
## 2. take a look #############################################################################
###############################################################################################
dim(dtRawTrain)
# [1] 145231   1934
dim(dtRawTest)
# [1] 145232   1933
summary(dtRawTrain)
str(dtRawTrain)

##################################
## 2.1 number of NAs per column ##
##################################
CheckColNAs <- function(dt){
    intNAs <- apply(dt, 2, function (x) sum(is.na(x)))
    intColNAs <- intNAs[intNAs != 0]
}
intColNAs.train <- CheckColNAs(dtRawTrain) # 476 columns in the train set with NAs
intColNAs.test <- CheckColNAs(dtRawTest) # 476 columns in the test set with NAs

# check if the NA columns in the train and the test are the same
sum(names(intColNAs.train) == names(intColNAs.test)) # 476, meaning they are the same

# calculate the percentage of NAs of each column with NAs
numPercColNAS.train <- intColNAs.train / ncol(dtRawTrain)
numPercColNAS.test <- intColNAs.test / ncol(dtRawTest)

sum(numPercColNAS.train > .25) # 10 columns in the train have more than half NAs
sum(numPercColNAS.test > .5) # 10 columns in the test have more than half NAs



















