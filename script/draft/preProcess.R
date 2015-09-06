require(data.table)
require(bit64) # dealing with integer64 issue when fread
require(caret)
source("script/draft/utilities.R")
###############################################################################################
## 1. remove columns with too many NAs ########################################################
###############################################################################################
## no. of NAs is over .6
nameColNAs.6
# [1] "VAR_0044" "VAR_0073" "VAR_0074" "VAR_0156" "VAR_0157" "VAR_0158"
# [7] "VAR_0159" "VAR_0166" "VAR_0167" "VAR_0168" "VAR_0169" "VAR_0176"
# [13] "VAR_0177" "VAR_0178" "VAR_0179" "VAR_0205" "VAR_0206" "VAR_0207"
# [19] "VAR_0208" "VAR_0209" "VAR_0210" "VAR_0211" "VAR_0213" "VAR_0214"
# [25] "VAR_0840"
dtProcTrain <- dtRawTrain[, nameColNAs.6 := NULL, with = F]
dtProcTest <- dtRawTest[, nameColNAs.6 := NULL, with = F]
###############################################################################################
## 2. remove columns with only one value ######################################################
###############################################################################################
## character columns
sum(intColLevelsChar.Train == 1)# 12
sum(intColLevelsChar.Test == 1) # 12
# are these columns the same in train and test?
sum(names(intColLevelsChar.Train[intColLevelsChar.Train == 1]) == names(intColLevelsChar.Test[intColLevelsChar.Test == 1])) # yes, they are
# remove
dtProcTrain <- dtProcTrain[, names(intColLevelsChar.Train[intColLevelsChar.Train == 1]) := NULL, with = F]
dtProcTest <- dtProcTest[, names(intColLevelsChar.Test[intColLevelsChar.Test == 1]) := NULL, with = F]

## integer columns
sum(intColLevelsInt.Train == 1) # 38
sum(intColLevelsInt.Test == 1) # 40
# which columns are the same
names(intColLevelsInt.Train[intColLevelsInt.Train == 1])
# [1] "VAR_0018" "VAR_0019" "VAR_0020" "VAR_0021" "VAR_0022" "VAR_0023"
# [7] "VAR_0024" "VAR_0025" "VAR_0026" "VAR_0027" "VAR_0028" "VAR_0029"
# [13] "VAR_0030" "VAR_0031" "VAR_0032" "VAR_0038" "VAR_0039" "VAR_0040"
# [19] "VAR_0041" "VAR_0042" "VAR_0188" "VAR_0189" "VAR_0190" "VAR_0197"
# [25] "VAR_0199" "VAR_0203" "VAR_0215" "VAR_0221" "VAR_0223" "VAR_0246"
# [31] "VAR_0394" "VAR_0438" "VAR_0446" "VAR_0527" "VAR_0528" "VAR_0530"
# [37] "VAR_0847" "VAR_1428"
names(intColLevelsInt.Train[intColLevelsInt.Test == 1])
# [1] "VAR_0018" "VAR_0019" "VAR_0020" "VAR_0021" "VAR_0022" "VAR_0023"
# [7] "VAR_0024" "VAR_0025" "VAR_0026" "VAR_0027" "VAR_0028" "VAR_0029"
# [13] "VAR_0030" "VAR_0031" "VAR_0032" "VAR_0038" "VAR_0039" "VAR_0040"
# [19] "VAR_0041" "VAR_0042" "VAR_0188" "VAR_0189" "VAR_0190" "VAR_0197"
# [25] "VAR_0199" "VAR_0203" "VAR_0215" "VAR_0221" "VAR_0223" "VAR_0246"
# [31] "VAR_0394" "VAR_0438" "VAR_0446" "VAR_0526" "VAR_0527" "VAR_0528"
# [37] "VAR_0529" "VAR_0530" "VAR_0847" "VAR_1428"
# VAR_0526 and VAR_0529 in Test has only 1 value, but not in train
# check VAR_0526 and VAR_0529
table(dtProcTrain$VAR_0526) # all values are 0 except one is 1, remove it
table(dtProcTrain$VAR_0529) # all values are 0 except one is 1, remove it

# remove them
dtProcTrain <- dtProcTrain[, names(intColLevelsInt.Train[intColLevelsInt.Test == 1]) := NULL, with = F]
dtProcTest <- dtProcTest[, names(intColLevelsInt.Train[intColLevelsInt.Test == 1]) := NULL, with = F]

## numeric columns
sum(intColLevelsNum.Train == 1) # 0
sum(intColLevelsNum.Test == 1) # 0

## integer 64 column
sum(intColLevelsInt64.Train == 1) # 0
sum(intColLevelsInt64.Test == 1) # 0

###############################################################################################
## 3. remove columns with tiny variance #######################################################
###############################################################################################
nzv <- nearZeroVar(dtProcTrain, saveMetrics = T)

## character columns
rownames(x)[x$nzv][rownames(x)[x$nzv] %in% names(charColClassChar)]
# [1] "VAR_0226" "VAR_0230" "VAR_0236" "VAR_0404" "VAR_0493"

## integer columns
rownames(x)[x$nzv][rownames(x)[x$nzv] %in% names(charColClassInt)]

table(dtProcTrain$VAR_0018)

intColLevelsInt.Train

names(intColLevelsChar.Train[intColLevelsChar.Train == 1])




