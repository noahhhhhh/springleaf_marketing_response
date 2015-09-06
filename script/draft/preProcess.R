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
## how many zero variance?
sum(nzv$zeroVar) # 0, as this has been removed previously

## check the near zero var #############
## character columns
charColNZVsChar <- rownames(nzv)[nzv$nzv][rownames(nzv)[nzv$nzv] %in% names(charColClassChar)]
# [1] "VAR_0226" "VAR_0230" "VAR_0236" "VAR_0404" "VAR_0493"

## integer columns
charColNZVsInt <- rownames(nzv)[nzv$nzv][rownames(nzv)[nzv$nzv] %in% names(charColClassInt)]
length(rownames(nzv)[nzv$nzv][rownames(nzv)[nzv$nzv] %in% names(charColClassInt)]) # 383

## numeric columns
charColNZVsNum <- rownames(nzv)[nzv$nzv][rownames(nzv)[nzv$nzv] %in% names(charColClassNum)]
# [1] "VAR_0299" "VAR_0300" "VAR_0301" "VAR_0319" "VAR_0320" "VAR_0321"
# [7] "VAR_0370"

## integer 64 column
rownames(nzv)[nzv$nzv][rownames(nzv)[nzv$nzv] %in% names(charColClassInt64)] # 0

###############################################################################################
## 4. factorize features ######################################################################
###############################################################################################
## character columns
charColNamesCharProc <- names(charColClassChar)[names(charColClassChar) %in% names(dtProcTrain)]
str(dtProcTrain[, charColNamesCharProc, with = F])
# $ VAR_0001: chr  "H" "H" "H" "H" ...
# $ VAR_0005: chr  "C" "B" "C" "C" ...
# $ VAR_0075: chr  "08NOV11:00:00:00" "10NOV11:00:00:00" "13DEC11:00:00:00" "23SEP10:00:00:00" ...
# $ VAR_0200: chr  "FT LAUDERDALE" "SANTEE" "REEDSVILLE" "LIBERTY" ...
# $ VAR_0204: chr  "29JAN14:21:16:00" "01FEB14:00:11:00" "30JAN14:15:11:00" "01FEB14:00:07:00" ...
# $ VAR_0217: chr  "08NOV11:02:00:00" "02OCT12:02:00:00" "13DEC11:02:00:00" "01NOV12:02:00:00" ...
# $ VAR_0226: chr  "false" "false" "false" "false" ...
# $ VAR_0230: chr  "false" "false" "false" "false" ...
# $ VAR_0232: chr  "true" "false" "true" "false" ...
# $ VAR_0236: chr  "true" "true" "true" "true" ...
# $ VAR_0237: chr  "FL" "CA" "WV" "TX" ... * different levels in train and test
# $ VAR_0274: chr  "FL" "MI" "WV" "TX" ...
# $ VAR_0283: chr  "S" "S" "S" "S" ...
# $ VAR_0305: chr  "S" "S" "P" "P" ...
# $ VAR_0325: chr  "-1" "H" "R" "H" ...
# $ VAR_0342: chr  "CF" "EC" "UU" "-1" ...
# $ VAR_0352: chr  "O" "O" "R" "R" ...
# $ VAR_0353: chr  "U" "R" "R" "R" ...
# $ VAR_0354: chr  "O" "R" "-1" "-1" ...
# $ VAR_0404: chr  "CHIEF EXECUTIVE OFFICER" "-1" "-1" "-1" ...
# $ VAR_0466: chr  "-1" "I" "-1" "-1" ...
# $ VAR_0467: chr  "-1" "Discharged" "-1" "-1" ...
# $ VAR_0493: chr  "COMMUNITY ASSOCIATION MANAGER" "-1" "-1" "-1" ...
# $ VAR_1934: chr  "IAPS" "IAPS" "IAPS" "RCC" ...

# easily identified factorable features
CheckColLevels(dtProcTrain, charColNamesCharProc) # train
# VAR_0001 VAR_0005 VAR_0075 VAR_0200 VAR_0204 VAR_0217 VAR_0226 VAR_0230 
# 3        4     2371    12386     1192      397        2        2 
# VAR_0232 VAR_0236 VAR_0237 VAR_0274 VAR_0283 VAR_0305 VAR_0325 VAR_0342 
# 2        2       45       57        7        8        9       50 
# VAR_0352 VAR_0353 VAR_0354 VAR_0404 VAR_0466 VAR_0467 VAR_0493 VAR_1934 
# 4        4        4     1823        2        4      608        5
CheckColLevels(dtProcTest, charColNamesCharProc) # test
# VAR_0001 VAR_0005 VAR_0075 VAR_0200 VAR_0204 VAR_0217 VAR_0226 VAR_0230 
# 3        4     2369    12457     1190      397        2        2 
# VAR_0232 VAR_0236 VAR_0237 VAR_0274 VAR_0283 VAR_0305 VAR_0325 VAR_0342 
# 2        2       45       57        8        8        9       50 
# VAR_0352 VAR_0353 VAR_0354 VAR_0404 VAR_0466 VAR_0467 VAR_0493 VAR_1934 
# 4        4        4     1873        2        4      619        5 
# same levels in both train and test
CheckColLevels(dtProcTrain, charColNamesCharProc)[CheckColLevels(dtProcTrain, charColNamesCharProc) == CheckColLevels(dtProcTest, charColNamesCharProc)]
# VAR_0001 VAR_0005 VAR_0217 VAR_0226 VAR_0230 VAR_0232 VAR_0236 VAR_0237 
# 3        4      397        2        2        2        2       45 
# VAR_0274 VAR_0305 VAR_0325 VAR_0342 VAR_0352 VAR_0353 VAR_0354 VAR_0466 
# 57        8        9       50        4        4        4        2 
# VAR_0467 VAR_1934 
# 4        5
intColNamesCharProcSame <- names(CheckColLevels(dtProcTrain, charColNamesCharProc)[CheckColLevels(dtProcTrain, charColNamesCharProc) == CheckColLevels(dtProcTest, charColNamesCharProc)])
CheckColLevelsSame(dtProcTrain, intColNamesCharProcSame) # VAR_0237 is not the same
# VAR_0001 VAR_0005 VAR_0217 VAR_0226 VAR_0230 VAR_0232 VAR_0236 VAR_0237 
# TRUE     TRUE     TRUE     TRUE     TRUE     TRUE     TRUE    FALSE 
# VAR_0274 VAR_0305 VAR_0325 VAR_0342 VAR_0352 VAR_0353 VAR_0354 VAR_0466 
# TRUE     TRUE     TRUE     TRUE     TRUE     TRUE     TRUE     TRUE 
# VAR_0467 VAR_1934 
# TRUE     TRUE 
# check VAR_0237
table(dtProcTrain$VAR_0237)
# AK    AL    AR    AZ    CA    CO    CT    DC    DE    FL    GA    HI 
# 1  4985     9  1099 15770  2808     1    28   161  7004  8730   264 
# IA    ID    IL    IN    KS    KY    LA    MD    MI    MN    MO    MS 
# 241   695  8479  5506   548  3404  3284   592  1027     9  3257  2386 
# MT    NC    NE    NJ    NM    NV    NY    OH    OK    OR    PA    SC 
# 105 11691   151   536  1322   146  1372  7307  2338  1523  6937  6083 
# SD    TN    TX    UT    VA    WA    WI    WV    WY 
# 66  5226 14186   293  8081  3722  2234  1459   109 
table(dtProcTest$VAR_0237)
# AK    AL    AR    AZ    CA    CO    DC    DE    FL    GA    HI    IA 
# 2  5008     3  1069 15781  2828    32   167  7148  8953   246   275 
# ID    IL    IN    KS    KY    LA    MD    MI    MN    MO    MS    MT 
# 700  8630  5410   608  3344  3220   538  1017     4  3248  2371   125 
# NC    ND    NE    NJ    NM    NV    NY    OH    OK    OR    PA    SC 
# 11830     1   157   550  1396   119  1258  7117  2342  1477  6870  5873 
# SD    TN    TX    UT    VA    WA    WI    WV    WY 
# 79  5224 14417   308  7950  3699  2211  1467   112 

# transform the features
# $ VAR_0001: chr  "H" "H" "H" "H" ...
# $ VAR_0005: chr  "C" "B" "C" "C" ...
# $ VAR_0075: chr  "08NOV11:00:00:00" "10NOV11:00:00:00" "13DEC11:00:00:00" "23SEP10:00:00:00" ...
# $ VAR_0200: chr  "FT LAUDERDALE" "SANTEE" "REEDSVILLE" "LIBERTY" ...
# $ VAR_0204: chr  "29JAN14:21:16:00" "01FEB14:00:11:00" "30JAN14:15:11:00" "01FEB14:00:07:00" ...
# $ VAR_0217: chr  "08NOV11:02:00:00" "02OCT12:02:00:00" "13DEC11:02:00:00" "01NOV12:02:00:00" ...
# $ VAR_0226: chr  "false" "false" "false" "false" ...
# $ VAR_0230: chr  "false" "false" "false" "false" ...
# $ VAR_0232: chr  "true" "false" "true" "false" ...
# $ VAR_0236: chr  "true" "true" "true" "true" ...
# $ VAR_0237: chr  "FL" "CA" "WV" "TX" ... * different levels in train and test
# $ VAR_0274: chr  "FL" "MI" "WV" "TX" ...
# $ VAR_0283: chr  "S" "S" "S" "S" ...
# $ VAR_0305: chr  "S" "S" "P" "P" ...
# $ VAR_0325: chr  "-1" "H" "R" "H" ...
# $ VAR_0342: chr  "CF" "EC" "UU" "-1" ...
# $ VAR_0352: chr  "O" "O" "R" "R" ...
# $ VAR_0353: chr  "U" "R" "R" "R" ...
# $ VAR_0354: chr  "O" "R" "-1" "-1" ...
# $ VAR_0404: chr  "CHIEF EXECUTIVE OFFICER" "-1" "-1" "-1" ...
# $ VAR_0466: chr  "-1" "I" "-1" "-1" ...
# $ VAR_0467: chr  "-1" "Discharged" "-1" "-1" ...
# $ VAR_0493: chr  "COMMUNITY ASSOCIATION MANAGER" "-1" "-1" "-1" ...
# $ VAR_1934: chr  "IAPS" "IAPS" "IAPS" "RCC" ...
# some are time and some are boolean
# time ones
# VAR_0075 e.g. 28SEP12:00:00:00
dtProcTrain <- dtProcTrain[, "VAR_0075" := as.numeric(as.Date(substr(dtProcTrain$VAR_0075, 1, 7), format = "%d%b%y")), with = F]
dtProcTest <- dtProcTest[, "VAR_0075" := as.numeric(as.Date(substr(dtProcTest$VAR_0075, 1, 7), format = "%d%b%y")), with = F]

# VAR_0217 e.g. 22OCT11:02:00:00 all 02:00:00, so just need the date
dtProcTrain$VAR_0217 <- as.numeric(as.Date(substr(dtProcTrain$VAR_0217, 1, 7), format = "%d%b%y"))
dtProcTest$VAR_0217 <- as.numeric(as.Date(substr(dtProcTest$VAR_0075, 1, 7), format = "%d%b%y"))

# VAR_0204 e.g. 29JAN14:19:52:00, split into date and time, while time is splited into 24 levels
dtProcTrain$VAR_0204.date <- as.numeric(as.Date(substr(dtProcTrain$VAR_0204, 1, 7), format = "%d%b%y"))
dtProcTest$VAR_0204.date <- as.numeric(as.Date(substr(dtProcTest$VAR_0204, 1, 7), format = "%d%b%y"))

dtProcTrain$VAR_0204.hour <- as.numeric(substr(dtProcTrain$VAR_0204, 9, 10))
dtProcTest$VAR_0204.hour <- as.numeric(substr(dtProcTest$VAR_0204, 9, 10))

dtProcTrain <- dtProcTrain[, "VAR_0204" := NULL, with = F]
dtProcTest <- dtProcTest[, "VAR_0204" := NULL, with = F]

# boolean ones
# $ VAR_0226: chr  "false" "false" "false" "false" ...
# $ VAR_0230: chr  "false" "false" "false" "false" ...
# $ VAR_0232: chr  "true" "false" "true" "false" ...
# $ VAR_0236: chr  "true" "true" "true" "true" ...
table(dtProcTrain$VAR_0236)



dtProcTrain$VAR_0204
substr("02NOV09:00:00:00", 1, 7)








