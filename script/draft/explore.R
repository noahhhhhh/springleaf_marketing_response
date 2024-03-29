require(data.table) # fread and data.table
require(bit64) # dealing with integer64 issue when fread
source("script/draft/utilities.R")
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
intColNAs.train <- CheckColNAs(dtRawTrain) # 524 columns in the train set with NAs
intColNAs.test <- CheckColNAs(dtRawTest) # 524 columns in the test set with NAs

## check if the NA columns in the train and the test are the same
sum(names(intColNAs.train) == names(intColNAs.test)) # 524, meaning they are the same

## calculate the percentage of NAs of each column with NAs
numPercColNAS.train <- intColNAs.train / nrow(dtRawTrain)
numPercColNAS.test <- intColNAs.test / nrow(dtRawTest)

sum(numPercColNAS.train > .1) # 25 columns in the train have more than half NAs
sum(numPercColNAS.test > .1) # 25 columns in the test have more than half NAs

table(cut(numPercColNAS.train, 10)) # 25 columns have more than 60% of NAs
# (-0.000614,0.1]       (0.1,0.2]       (0.2,0.3]       (0.3,0.4] 
# 499                   0               0               0 
# (0.4,0.5]             (0.5,0.6]       (0.6,0.7]       (0.7,0.8] 
# 0                     0               2               0 
# (0.8,0.9]             (0.9,1] 
# 5                     18 
table(cut(numPercColNAS.test, 10)) # 10 columns have more than 60% of NAs
# (-0.000669,0.1]       (0.1,0.2]       (0.2,0.3]       (0.3,0.4] 
# 499                   0               0               0 
# (0.4,0.5]             (0.5,0.6]       (0.6,0.7]       (0.7,0.8] 
# 0                     0               2               0 
# (0.8,0.9]             (0.9,1] 
# 5                     18 

## check if the 25 NA columns in the train and the test are the same
sum(names(numPercColNAS.train)[numPercColNAS.train > .6] == names(numPercColNAS.test)[numPercColNAS.test > .6]) # 25, meaning they are the same

## check the NA columns
# start from the above 10 columns
nameColNAs.6 <- names(numPercColNAS.train)[numPercColNAS.train > .6]
str(dtRawTrain[, nameColNAs.6, with = F])
# $ VAR_0044: chr  NA NA NA NA ...
# $ VAR_0073: chr  NA "04SEP12:00:00:00" NA NA ...
# $ VAR_0074: int  NA 5208 NA NA NA NA NA NA 2487 NA ...
# $ VAR_0156: chr  NA NA NA NA ...
# $ VAR_0157: chr  NA NA NA NA ...
# $ VAR_0158: chr  NA NA NA NA ...
# $ VAR_0159: chr  NA NA NA NA ...
# $ VAR_0166: chr  NA NA NA NA ...
# $ VAR_0167: chr  NA NA NA NA ...
# $ VAR_0168: chr  NA NA NA NA ...
# $ VAR_0169: chr  NA NA NA NA ...
# $ VAR_0176: chr  NA NA NA NA ...
# $ VAR_0177: chr  NA NA NA NA ...
# $ VAR_0178: chr  NA NA NA NA ...
# $ VAR_0179: chr  NA NA NA NA ...
# $ VAR_0205: int  NA NA NA NA NA NA NA NA NA NA ...
# $ VAR_0206: int  NA NA NA NA NA NA NA NA NA NA ...
# $ VAR_0207: logi  NA NA NA NA NA NA ...
# $ VAR_0208: int  NA 146 NA NA NA NA NA NA NA NA ...
# $ VAR_0209: int  NA NA NA NA NA NA NA NA NA NA ...
# $ VAR_0210: int  NA 146 NA NA NA NA NA NA NA NA ...
# $ VAR_0211: int  NA 146 NA NA NA NA NA NA NA NA ...
# $ VAR_0213: logi  NA NA NA NA NA NA ...
# $ VAR_0214: chr  NA NA NA NA ...
# $ VAR_0840: logi  NA NA NA NA NA NA ...
# check the rest
nameColNAs.0 <- names(numPercColNAS.train)[numPercColNAS.train <= .6]
str(dtRawTrain[, nameColNAs.0, with = F])
# $ VAR_0006: int  0 1 0 0 0 0 1 0 1 0 ...
# $ VAR_0007: int  0 0 0 0 0 0 1 0 1 0 ...
# $ VAR_0008: chr  "false" "false" "false" "false" ...
# $ VAR_0009: chr  "false" "false" "false" "false" ...
# $ VAR_0010: chr  "false" "false" "false" "false" ...
# $ VAR_0011: chr  "false" "false" "false" "false" ...
# $ VAR_0012: chr  "false" "false" "false" "false" ...
# $ VAR_0013: int  0 1 0 0 0 0 1 0 1 0 ...
# $ VAR_0014: int  0 0 0 0 0 0 1 0 1 0 ...
# $ VAR_0015: int  0 1 0 0 0 0 1 0 1 0 ...
# $ VAR_0016: int  1 2 1 2 1 1 2 1 1 1 ...
# $ VAR_0017: int  0 1 0 0 0 0 0 0 1 0 ...
# $ VAR_0018: int  0 0 0 0 0 0 0 0 0 0 ...
# $ VAR_0019: int  0 0 0 0 0 0 0 0 0 0 ...
# $ VAR_0020: int  0 0 0 0 0 0 0 0 0 0 ...
# $ VAR_0021: int  0 0 0 0 0 0 0 0 0 0 ...
# $ VAR_0022: int  0 0 0 0 0 0 0 0 0 0 ...
# $ VAR_0023: int  0 0 0 0 0 0 0 0 0 0 ...
# $ VAR_0024: int  0 0 0 0 0 0 0 0 0 0 ...
# $ VAR_0025: int  0 0 0 0 0 0 0 0 0 0 ...
# $ VAR_0026: int  0 0 0 0 0 0 0 0 0 0 ...
# $ VAR_0027: int  0 0 0 0 0 0 0 0 0 0 ...
# $ VAR_0028: int  0 0 0 0 0 0 0 0 0 0 ...
# $ VAR_0029: int  0 0 0 0 0 0 0 0 0 0 ...
# $ VAR_0030: int  0 0 0 0 0 0 0 0 0 0 ...
# $ VAR_0031: int  0 0 0 0 0 0 0 0 0 0 ...
# $ VAR_0032: int  0 0 0 0 0 0 0 0 0 0 ...
# $ VAR_0033: int  1 1 1 1 1 1 2 1 1 2 ...
# $ VAR_0034: int  0 0 0 0 0 0 2 0 1 1 ...
# $ VAR_0035: int  0 0 0 0 0 0 2 0 1 1 ...
# $ VAR_0036: int  0 0 0 0 0 0 1 0 1 1 ...
# $ VAR_0037: int  0 0 0 0 0 0 1 0 1 1 ...
# $ VAR_0038: int  0 0 0 0 0 0 0 0 0 0 ...
# $ VAR_0039: int  0 0 0 0 0 0 0 0 0 0 ...
# $ VAR_0040: int  0 0 0 0 0 0 0 0 0 0 ...
# $ VAR_0041: int  0 0 0 0 0 0 0 0 0 0 ...
# $ VAR_0042: int  0 0 0 0 0 0 0 0 0 0 ...
# $ VAR_0043: chr  "false" "false" "false" "false" ...
# $ VAR_0045: int  0 0 0 0 0 0 0 0 0 0 ...
# $ VAR_0046: int  0 0 0 0 0 0 0 0 0 0 ...
# $ VAR_0047: int  0 0 0 0 0 0 0 0 0 0 ...
# $ VAR_0048: int  0 0 0 0 0 0 0 0 0 0 ...
# $ VAR_0049: int  0 1 0 0 0 0 0 0 0 0 ...
# $ VAR_0050: int  0 1 0 0 0 0 0 0 0 0 ...
# $ VAR_0051: int  0 1 0 1 0 0 0 0 0 0 ...
# $ VAR_0052: int  1 1 1 1 1 1 1 1 1 1 ...
# $ VAR_0053: int  1 2 1 1 1 1 1 1 1 1 ...
# $ VAR_0054: int  1 2 1 1 1 1 1 1 1 1 ...
# $ VAR_0055: int  1 2 1 1 1 1 1 1 1 1 ...
# $ VAR_0056: int  1 3 1 1 1 1 1 1 1 1 ...
# $ VAR_0057: int  1 3 1 1 1 1 1 1 1 1 ...
# $ VAR_0058: int  1 3 1 1 1 1 1 1 1 1 ...
# $ VAR_0059: int  1 1 1 1 1 1 2 1 1 1 ...
# $ VAR_0060: int  1 2 1 1 1 1 2 1 1 1 ...
# $ VAR_0061: int  1 2 1 1 1 1 2 1 1 1 ...
# $ VAR_0062: int  1 2 1 1 1 1 2 1 2 1 ...
# $ VAR_0063: int  1 3 1 1 1 1 3 1 2 1 ...
# $ VAR_0064: int  1 3 1 1 1 1 3 1 2 1 ...
# $ VAR_0065: int  1 3 1 1 1 1 3 1 2 1 ...
# $ VAR_0066: int  0 0 0 0 0 0 1 0 0 0 ...
# $ VAR_0067: int  0 0 0 0 0 0 1 0 0 0 ...
# $ VAR_0068: int  0 0 0 0 0 0 1 0 0 0 ...
# $ VAR_0069: int  0 0 0 0 0 0 1 0 1 0 ...
# $ VAR_0070: int  0 0 0 0 0 0 2 0 1 0 ...
# $ VAR_0071: int  0 0 0 0 0 0 2 0 1 0 ...
# $ VAR_0072: int  0 0 0 0 0 0 2 0 1 0 ...
# $ VAR_0075: chr  "08NOV11:00:00:00" "10NOV11:00:00:00" "13DEC11:00:00:00" "23SEP10:00:00:00" ...
# $ VAR_0076: int  1 1 1 1 1 1 2 1 1 1 ...
# $ VAR_0077: int  1 2 1 1 1 1 2 1 1 1 ...
# $ VAR_0078: int  1 2 1 1 1 1 2 1 1 1 ...
# $ VAR_0079: int  1 2 1 1 1 1 2 1 2 1 ...
# $ VAR_0080: int  1 3 1 1 1 1 3 1 2 1 ...
# $ VAR_0081: int  1 3 1 1 1 1 3 1 2 1 ...
# $ VAR_0082: int  1 3 1 1 1 1 3 1 2 1 ...
# $ VAR_0083: int  1 1 1 1 1 1 2 1 1 1 ...
# $ VAR_0084: int  1 2 1 1 1 1 2 1 1 1 ...
# $ VAR_0085: int  1 2 1 1 1 1 2 1 1 1 ...
# $ VAR_0086: int  1 2 1 1 1 1 2 1 2 1 ...
# $ VAR_0087: int  1 4 1 1 1 1 3 1 2 1 ...
# $ VAR_0088: int  1 4 1 1 1 1 3 1 2 1 ...
# $ VAR_0089: int  1 4 1 2 1 1 3 1 2 1 ...
# $ VAR_0090: int  0 1 1 1 1 1 1 1 1 1 ...
# $ VAR_0091: int  0 1 1 1 1 1 1 1 1 1 ...
# $ VAR_0092: int  0 1 1 1 1 1 1 1 1 1 ...
# $ VAR_0093: int  0 1 1 1 1 1 1 1 1 1 ...
# $ VAR_0094: int  0 1 1 1 1 1 1 1 1 1 ...
# $ VAR_0095: int  0 1 1 1 1 1 1 1 1 1 ...
# $ VAR_0096: int  0 1 1 1 1 1 1 1 1 1 ...
# $ VAR_0097: int  0 1 1 1 1 1 1 1 1 1 ...
# $ VAR_0098: int  0 0 0 0 0 0 0 0 0 0 ...
# $ VAR_0099: int  0 0 0 0 0 0 0 0 0 0 ...
# $ VAR_0100: int  0 0 0 0 0 0 0 0 0 0 ...
# $ VAR_0101: int  0 0 0 0 0 0 1 0 0 0 ...
# $ VAR_0102: int  0 0 0 0 0 0 1 0 0 0 ...
# $ VAR_0103: int  0 0 0 0 0 0 1 0 0 0 ...
# $ VAR_0104: int  0 1 0 0 0 0 1 0 0 0 ...
# $ VAR_0105: int  0 1 0 0 0 0 1 0 1 0 ...
# $ VAR_0106: int  0 0 0 0 0 0 0 0 0 0 ...
# $ VAR_0107: int  0 0 0 0 0 0 0 0 0 0 ...
# [list output truncated]

##################################
## 2.2 check the class of columns 
##################################
charColClass <- unlist(lapply(dtRawTrain, class))
table(charColClass)
# character     integer         integer64   logical     numeric 
# 51            1869            1           3           10 

## check the character columns
charColClassChar <- charColClass[charColClass == "character"]
str(dtRawTrain[, names(charColClassChar), with = F])
# $ VAR_0001: chr  "H" "H" "H" "H" ...
# $ VAR_0005: chr  "C" "B" "C" "C" ...
# $ VAR_0008: chr  "false" "false" "false" "false" ...
# $ VAR_0009: chr  "false" "false" "false" "false" ...
# $ VAR_0010: chr  "false" "false" "false" "false" ...
# $ VAR_0011: chr  "false" "false" "false" "false" ...
# $ VAR_0012: chr  "false" "false" "false" "false" ...
# $ VAR_0043: chr  "false" "false" "false" "false" ...
# $ VAR_0044: chr  "[]" "[]" "[]" "[]" ...
# $ VAR_0073: chr  "" "04SEP12:00:00:00" "" "" ...
# $ VAR_0075: chr  "08NOV11:00:00:00" "10NOV11:00:00:00" "13DEC11:00:00:00" "23SEP10:00:00:00" ...
# $ VAR_0156: chr  "" "" "" "" ...
# $ VAR_0157: chr  "" "" "" "" ...
# $ VAR_0158: chr  "" "" "" "" ...
# $ VAR_0159: chr  "" "" "" "" ...
# $ VAR_0166: chr  "" "" "" "" ...
# $ VAR_0167: chr  "" "" "" "" ...
# $ VAR_0168: chr  "" "" "" "" ...
# $ VAR_0169: chr  "" "" "" "" ...
# $ VAR_0176: chr  "" "" "" "" ...
# $ VAR_0177: chr  "" "" "" "" ...
# $ VAR_0178: chr  "" "" "" "" ...
# $ VAR_0179: chr  "" "" "" "" ...
# $ VAR_0196: chr  "false" "false" "false" "false" ...
# $ VAR_0200: chr  "FT LAUDERDALE" "SANTEE" "REEDSVILLE" "LIBERTY" ...
# $ VAR_0202: chr  "BatchInquiry" "BatchInquiry" "BatchInquiry" "BatchInquiry" ...
# $ VAR_0204: chr  "29JAN14:21:16:00" "01FEB14:00:11:00" "30JAN14:15:11:00" "01FEB14:00:07:00" ...
# $ VAR_0214: chr  "" "" "" "" ...
# $ VAR_0216: chr  "DS" "DS" "DS" "DS" ...
# $ VAR_0217: chr  "08NOV11:02:00:00" "02OCT12:02:00:00" "13DEC11:02:00:00" "01NOV12:02:00:00" ...
# $ VAR_0222: chr  "C6" "C6" "C6" "C6" ...
# $ VAR_0226: chr  "false" "false" "false" "false" ...
# $ VAR_0229: chr  "false" "false" "false" "false" ...
# $ VAR_0230: chr  "false" "false" "false" "false" ...
# $ VAR_0232: chr  "true" "false" "true" "false" ...
# $ VAR_0236: chr  "true" "true" "true" "true" ...
# $ VAR_0237: chr  "FL" "CA" "WV" "TX" ...
# $ VAR_0239: chr  "false" "false" "false" "false" ...
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
CheckColNAs(dtRawTrain[, names(charColClassChar), with = F])
# VAR_0008 VAR_0009 VAR_0010 VAR_0011 VAR_0012 VAR_0043 VAR_0044 VAR_0073 
# 56       56       56       56       56       56   145231   101127 
# VAR_0075 VAR_0156 VAR_0157 VAR_0158 VAR_0159 VAR_0166 VAR_0167 VAR_0168 
# 56   139361   144311   143142   139361   131001   142664   134506 
# VAR_0169 VAR_0176 VAR_0177 VAR_0178 VAR_0179 VAR_0196 VAR_0200 VAR_0202 
# 131001   127699   141873   133158   127699       56       56       56 
# VAR_0204 VAR_0214 VAR_0216 VAR_0217 VAR_0222 VAR_0226 VAR_0229 VAR_0230 
# 56   145219       56       56       56       56       56       56 
# VAR_0232 VAR_0236 VAR_0237 VAR_0239 VAR_0274 VAR_0283 VAR_0305 VAR_0325 
# 56       56       56       56      918      918      918      918 
# VAR_0342 VAR_0352 VAR_0353 VAR_0354 VAR_0404 VAR_0466 VAR_0467 VAR_0493 
# 918      918      918      918      918      918      918      918 
CheckColNAs(dtRawTest[, names(charColClassChar), with = F]) # 0 NAs in the train set
# VAR_0008 VAR_0009 VAR_0010 VAR_0011 VAR_0012 VAR_0043 VAR_0044 VAR_0073 
# 48       48       48       48       48       48   145232   101492 
# VAR_0075 VAR_0156 VAR_0157 VAR_0158 VAR_0159 VAR_0166 VAR_0167 VAR_0168 
# 48   139625   144417   143187   139625   131189   142673   134598 
# VAR_0169 VAR_0176 VAR_0177 VAR_0178 VAR_0179 VAR_0196 VAR_0200 VAR_0202 
# 131189   128014   141968   133293   128014       48       48       48 
# VAR_0204 VAR_0214 VAR_0216 VAR_0217 VAR_0222 VAR_0226 VAR_0229 VAR_0230 
# 48   145214       48       48       48       48       48       48 
# VAR_0232 VAR_0236 VAR_0237 VAR_0239 VAR_0274 VAR_0283 VAR_0305 VAR_0325 
# 48       48       48       48      909      909      909      909 
# VAR_0342 VAR_0352 VAR_0353 VAR_0354 VAR_0404 VAR_0466 VAR_0467 VAR_0493 
# 909      909      909      909      909      909      909      909

## check the logical columns
charColClassLog <- charColClass[charColClass == "logical"]
str(dtRawTrain[, names(charColClassLog), with = F])
# $ VAR_0207: logi  NA NA NA NA NA NA ...
# $ VAR_0213: logi  NA NA NA NA NA NA ...
# $ VAR_0840: logi  NA NA NA NA NA NA ...
CheckColNAs(dtRawTrain[, names(charColClassLog), with = F]) # all NAs
# VAR_0207 VAR_0213 VAR_0840 
# 145231   145231   145231 
CheckColNAs(dtRawTest[, names(charColClassLog), with = F]) # all NAs
# VAR_0207 VAR_0213 VAR_0840 
# 145232   145232   145232 

## check the integer64 columns
charColClassInt64 <- charColClass[charColClass == "integer64"]
str(dtRawTrain[, names(charColClassInt64), with = F])
# $ VAR_0212:Class 'integer64'  num [1:145231] NA 4.55e-313 1.31e-313 3.83e-313 2.99e-313 ...
CheckColNAs(dtRawTrain[, names(charColClassInt64), with = F]) # 12550
# VAR_0212 
# 12550 
CheckColNAs(dtRawTest[, names(charColClassInt64), with = F]) # 12552
# VAR_0212 
# 12552 
# above is not correct, integer64 has some exceptions!
sum(is.na(dtRawTrain$VAR_0212)) # 0 NAS

## check the numeric columns
charColClassNum <- charColClass[charColClass == "numeric"]
str(dtRawTrain[, names(charColClassNum), with = F])
# $ VAR_0299: num  0.53 0.98 0 0 0 1.22 1.51 0 0.67 0 ...
# $ VAR_0300: num  0.74 1.3 0 0 0 0.91 1.72 0 0.83 0 ...
# $ VAR_0301: num  0.72 1.1 0 0 0 1.07 1.83 0 0.83 0 ...
# $ VAR_0319: num  0.53 0.98 0 0 0 1.22 1.51 0 0 0 ...
# $ VAR_0320: num  0.74 1.3 0 0 0 0.91 1.72 0 0 0 ...
# $ VAR_0321: num  0.72 1.1 0 0 0 1.07 1.83 0 0 0 ...
# $ VAR_0335: num  0 0 0 0 0 0 1.04 0.2 0.67 0 ...
# $ VAR_0336: num  0 0 0 0 0 0 1.4 0.23 0.83 0 ...
# $ VAR_0337: num  0 0 0 0 0 0 1.34 0.23 0.83 0 ...
# $ VAR_0370: num  0 1.36 -1 -1 -1 -1 0 -1 -1 -1 ...
CheckColNAs(dtRawTrain[, names(charColClassNum), with = F]) # 918, it is suspected that the same 918 rows are NAs
# VAR_0299 VAR_0300 VAR_0301 VAR_0319 VAR_0320 VAR_0321 VAR_0335 VAR_0336 
# 918      918      918      918      918      918      918      918 
# VAR_0337 VAR_0370 
# 918      918 
CheckColNAs(dtRawTest[, names(charColClassNum), with = F]) # 909, it is suspected that the same 909 rows are NAs
# VAR_0299 VAR_0300 VAR_0301 VAR_0319 VAR_0320 VAR_0321 VAR_0335 VAR_0336 
# 909      909      909      909      909      909      909      909 
# VAR_0337 VAR_0370 
# 909      909 

## check the integer columns
charColClassInt <- charColClass[charColClass == "integer"]
str(dtRawTrain[, names(charColClassInt), with = F])
# $ ID      : int  2 4 5 7 8 14 16 20 21 22 ...
# $ VAR_0002: int  224 7 116 240 72 4 60 13 17 24 ...
# $ VAR_0003: int  0 53 3 300 261 4 132 75 16 72 ...
# $ VAR_0004: int  4300 4448 3464 3200 2000 4422 40000 3600 2296 450 ...
# $ VAR_0006: int  0 1 0 0 0 0 1 0 1 0 ...
# $ VAR_0007: int  0 0 0 0 0 0 1 0 1 0 ...
# $ VAR_0013: int  0 1 0 0 0 0 1 0 1 0 ...
# $ VAR_0014: int  0 0 0 0 0 0 1 0 1 0 ...
# $ VAR_0015: int  0 1 0 0 0 0 1 0 1 0 ...
# $ VAR_0016: int  1 2 1 2 1 1 2 1 1 1 ...
# $ VAR_0017: int  0 1 0 0 0 0 0 0 1 0 ...
# $ VAR_0018: int  0 0 0 0 0 0 0 0 0 0 ...
# $ VAR_0019: int  0 0 0 0 0 0 0 0 0 0 ...
# $ VAR_0020: int  0 0 0 0 0 0 0 0 0 0 ...
# $ VAR_0021: int  0 0 0 0 0 0 0 0 0 0 ...
# $ VAR_0022: int  0 0 0 0 0 0 0 0 0 0 ...
# $ VAR_0023: int  0 0 0 0 0 0 0 0 0 0 ...
# $ VAR_0024: int  0 0 0 0 0 0 0 0 0 0 ...
# $ VAR_0025: int  0 0 0 0 0 0 0 0 0 0 ...
# $ VAR_0026: int  0 0 0 0 0 0 0 0 0 0 ...
# $ VAR_0027: int  0 0 0 0 0 0 0 0 0 0 ...
# $ VAR_0028: int  0 0 0 0 0 0 0 0 0 0 ...
# $ VAR_0029: int  0 0 0 0 0 0 0 0 0 0 ...
# $ VAR_0030: int  0 0 0 0 0 0 0 0 0 0 ...
# $ VAR_0031: int  0 0 0 0 0 0 0 0 0 0 ...
# $ VAR_0032: int  0 0 0 0 0 0 0 0 0 0 ...
# $ VAR_0033: int  1 1 1 1 1 1 2 1 1 2 ...
# $ VAR_0034: int  0 0 0 0 0 0 2 0 1 1 ...
# $ VAR_0035: int  0 0 0 0 0 0 2 0 1 1 ...
# $ VAR_0036: int  0 0 0 0 0 0 1 0 1 1 ...
# $ VAR_0037: int  0 0 0 0 0 0 1 0 1 1 ...
# $ VAR_0038: int  0 0 0 0 0 0 0 0 0 0 ...
# $ VAR_0039: int  0 0 0 0 0 0 0 0 0 0 ...
# $ VAR_0040: int  0 0 0 0 0 0 0 0 0 0 ...
# $ VAR_0041: int  0 0 0 0 0 0 0 0 0 0 ...
# $ VAR_0042: int  0 0 0 0 0 0 0 0 0 0 ...
# $ VAR_0045: int  0 0 0 0 0 0 0 0 0 0 ...
# $ VAR_0046: int  0 0 0 0 0 0 0 0 0 0 ...
# $ VAR_0047: int  0 0 0 0 0 0 0 0 0 0 ...
# $ VAR_0048: int  0 0 0 0 0 0 0 0 0 0 ...
# $ VAR_0049: int  0 1 0 0 0 0 0 0 0 0 ...
# $ VAR_0050: int  0 1 0 0 0 0 0 0 0 0 ...
# $ VAR_0051: int  0 1 0 1 0 0 0 0 0 0 ...
# $ VAR_0052: int  1 1 1 1 1 1 1 1 1 1 ...
# $ VAR_0053: int  1 2 1 1 1 1 1 1 1 1 ...
# $ VAR_0054: int  1 2 1 1 1 1 1 1 1 1 ...
# $ VAR_0055: int  1 2 1 1 1 1 1 1 1 1 ...
# $ VAR_0056: int  1 3 1 1 1 1 1 1 1 1 ...
# $ VAR_0057: int  1 3 1 1 1 1 1 1 1 1 ...
# $ VAR_0058: int  1 3 1 1 1 1 1 1 1 1 ...
# $ VAR_0059: int  1 1 1 1 1 1 2 1 1 1 ...
# $ VAR_0060: int  1 2 1 1 1 1 2 1 1 1 ...
# $ VAR_0061: int  1 2 1 1 1 1 2 1 1 1 ...
# $ VAR_0062: int  1 2 1 1 1 1 2 1 2 1 ...
# $ VAR_0063: int  1 3 1 1 1 1 3 1 2 1 ...
# $ VAR_0064: int  1 3 1 1 1 1 3 1 2 1 ...
# $ VAR_0065: int  1 3 1 1 1 1 3 1 2 1 ...
# $ VAR_0066: int  0 0 0 0 0 0 1 0 0 0 ...
# $ VAR_0067: int  0 0 0 0 0 0 1 0 0 0 ...
# $ VAR_0068: int  0 0 0 0 0 0 1 0 0 0 ...
# $ VAR_0069: int  0 0 0 0 0 0 1 0 1 0 ...
# $ VAR_0070: int  0 0 0 0 0 0 2 0 1 0 ...
# $ VAR_0071: int  0 0 0 0 0 0 2 0 1 0 ...
# $ VAR_0072: int  0 0 0 0 0 0 2 0 1 0 ...
# $ VAR_0074: int  NA 5208 NA NA NA NA NA NA 2487 NA ...
# $ VAR_0076: int  1 1 1 1 1 1 2 1 1 1 ...
# $ VAR_0077: int  1 2 1 1 1 1 2 1 1 1 ...
# $ VAR_0078: int  1 2 1 1 1 1 2 1 1 1 ...
# $ VAR_0079: int  1 2 1 1 1 1 2 1 2 1 ...
# $ VAR_0080: int  1 3 1 1 1 1 3 1 2 1 ...
# $ VAR_0081: int  1 3 1 1 1 1 3 1 2 1 ...
# $ VAR_0082: int  1 3 1 1 1 1 3 1 2 1 ...
# $ VAR_0083: int  1 1 1 1 1 1 2 1 1 1 ...
# $ VAR_0084: int  1 2 1 1 1 1 2 1 1 1 ...
# $ VAR_0085: int  1 2 1 1 1 1 2 1 1 1 ...
# $ VAR_0086: int  1 2 1 1 1 1 2 1 2 1 ...
# $ VAR_0087: int  1 4 1 1 1 1 3 1 2 1 ...
# $ VAR_0088: int  1 4 1 1 1 1 3 1 2 1 ...
# $ VAR_0089: int  1 4 1 2 1 1 3 1 2 1 ...
# $ VAR_0090: int  0 1 1 1 1 1 1 1 1 1 ...
# $ VAR_0091: int  0 1 1 1 1 1 1 1 1 1 ...
# $ VAR_0092: int  0 1 1 1 1 1 1 1 1 1 ...
# $ VAR_0093: int  0 1 1 1 1 1 1 1 1 1 ...
# $ VAR_0094: int  0 1 1 1 1 1 1 1 1 1 ...
# $ VAR_0095: int  0 1 1 1 1 1 1 1 1 1 ...
# $ VAR_0096: int  0 1 1 1 1 1 1 1 1 1 ...
# $ VAR_0097: int  0 1 1 1 1 1 1 1 1 1 ...
# $ VAR_0098: int  0 0 0 0 0 0 0 0 0 0 ...
# $ VAR_0099: int  0 0 0 0 0 0 0 0 0 0 ...
# $ VAR_0100: int  0 0 0 0 0 0 0 0 0 0 ...
# $ VAR_0101: int  0 0 0 0 0 0 1 0 0 0 ...
# $ VAR_0102: int  0 0 0 0 0 0 1 0 0 0 ...
# $ VAR_0103: int  0 0 0 0 0 0 1 0 0 0 ...
# $ VAR_0104: int  0 1 0 0 0 0 1 0 0 0 ...
# $ VAR_0105: int  0 1 0 0 0 0 1 0 1 0 ...
# $ VAR_0106: int  0 0 0 0 0 0 0 0 0 0 ...
# $ VAR_0107: int  0 0 0 0 0 0 0 0 0 0 ...
# $ VAR_0108: int  0 0 0 0 0 0 0 0 0 0 ...
# $ VAR_0109: int  0 0 0 0 0 0 0 0 0 0 ...
CheckColNAs(dtRawTrain[, names(charColClassInt), with = F]) # many rows are missing entry together
# VAR_0006 VAR_0007 VAR_0013 VAR_0014 VAR_0015 VAR_0016 VAR_0017 VAR_0018 
# 56       56       56       56       56       56       56       56 
# VAR_0019 VAR_0020 VAR_0021 VAR_0022 VAR_0023 VAR_0024 VAR_0025 VAR_0026 
# 56       56       56       56       56       56       56       56 
# VAR_0027 VAR_0028 VAR_0029 VAR_0030 VAR_0031 VAR_0032 VAR_0033 VAR_0034 
# 56       56       56       56       56       56       56       56 
# VAR_0035 VAR_0036 VAR_0037 VAR_0038 VAR_0039 VAR_0040 VAR_0041 VAR_0042 
# 56       56       56       56       56       56       56       56 
# VAR_0045 VAR_0046 VAR_0047 VAR_0048 VAR_0049 VAR_0050 VAR_0051 VAR_0052 
# 56       56       56       56       56       56       56       56 
# VAR_0053 VAR_0054 VAR_0055 VAR_0056 VAR_0057 VAR_0058 VAR_0059 VAR_0060 
# 56       56       56       56       56       56       56       56 
# VAR_0061 VAR_0062 VAR_0063 VAR_0064 VAR_0065 VAR_0066 VAR_0067 VAR_0068 
# 56       56       56       56       56       56       56       56 
# VAR_0069 VAR_0070 VAR_0071 VAR_0072 VAR_0074 VAR_0076 VAR_0077 VAR_0078 
# 56       56       56       56   101127       56       56       56 
# VAR_0079 VAR_0080 VAR_0081 VAR_0082 VAR_0083 VAR_0084 VAR_0085 VAR_0086 
# 56       56       56       56       56       56       56       56 
# VAR_0087 VAR_0088 VAR_0089 VAR_0090 VAR_0091 VAR_0092 VAR_0093 VAR_0094 
# 56       56       56       91       91       91       91       91 
# VAR_0095 VAR_0096 VAR_0097 VAR_0098 VAR_0099 VAR_0100 VAR_0101 VAR_0102 
# 91       91       91       91       91       91       91       91 
# VAR_0103 VAR_0104 VAR_0105 VAR_0106 VAR_0107 VAR_0108 VAR_0109 VAR_0110 
# 91       91       91       91       91       91       91       91 
# VAR_0111 VAR_0112 VAR_0113 VAR_0114 VAR_0115 VAR_0116 VAR_0117 VAR_0118 
# 91       91       91       91       91       91       91       91 
# VAR_0119 VAR_0120 VAR_0121 VAR_0122 VAR_0123 VAR_0124 VAR_0125 VAR_0126 
# 91       91       91       91       91       91       91       91 
# VAR_0127 VAR_0128 VAR_0129 VAR_0130 VAR_0131 VAR_0132 VAR_0133 VAR_0134 
# 91       91       91       91       91       91       91       91 
# VAR_0135 VAR_0136 VAR_0137 VAR_0138 VAR_0139 VAR_0140 VAR_0141 VAR_0142 
# 91       91       91       91       91       91       91       91 
# VAR_0143 VAR_0144 VAR_0145 VAR_0146 VAR_0147 VAR_0148 VAR_0149 VAR_0150 
# 91       91       91       89       89       89       89       89 
# VAR_0151 VAR_0152 VAR_0153 VAR_0154 VAR_0155 VAR_0160 VAR_0161 VAR_0162 
# 89       89       89       89       89       89       89       89 
# VAR_0163 VAR_0164 VAR_0165 VAR_0170 VAR_0171 VAR_0172 VAR_0173 VAR_0174 
# 89       89       89       89       89       89       89       89 
# VAR_0175 VAR_0180 VAR_0181 VAR_0182 VAR_0183 VAR_0184 VAR_0185 VAR_0186 
# 89       89       89       89       89       89       89       89 
# VAR_0187 VAR_0188 VAR_0189 VAR_0190 VAR_0191 VAR_0192 VAR_0193 VAR_0194 
# 89       89       89       89       89       89       89       89 
# VAR_0195 VAR_0197 VAR_0198 VAR_0199 VAR_0201 VAR_0203 VAR_0205 VAR_0206 
# 89       56       56       56       56       56   142974   142903 
# VAR_0208 VAR_0209 VAR_0210 VAR_0211 VAR_0215 VAR_0219 VAR_0220 VAR_0221 
# 125775   135844   125775   125775       56       56       56       56 
# VAR_0223 VAR_0224 VAR_0225 VAR_0227 VAR_0228 VAR_0231 VAR_0233 VAR_0234 
# 56       56       56       56       56       56       56       56 
# VAR_0235 VAR_0238 VAR_0241 VAR_0242 VAR_0243 VAR_0244 VAR_0245 VAR_0246 
# 56       56       56      918      918      918      918      918 
# VAR_0247 VAR_0248 VAR_0249 VAR_0250 VAR_0251 VAR_0252 VAR_0253 VAR_0254 
# 918      918      918      918      918      918      918      918 
# VAR_0255 VAR_0256 VAR_0257 VAR_0258 VAR_0259 VAR_0260 VAR_0261 VAR_0262 
# 918      918      918      918      918      918      918      918 
# VAR_0263 VAR_0264 VAR_0265 VAR_0266 VAR_0267 VAR_0268 VAR_0269 VAR_0270 
# 918      918      918      918      918      918      918      918 
# VAR_0271 VAR_0272 VAR_0273 VAR_0275 VAR_0276 VAR_0277 VAR_0278 VAR_0279 
# 918      918      918      918      918      918      918      918 
# VAR_0280 VAR_0281 VAR_0282 VAR_0284 VAR_0285 VAR_0286 VAR_0287 VAR_0288 
# 918      918      918      918      918      918      918      918 
# VAR_0289 VAR_0290 VAR_0291 VAR_0292 VAR_0293 VAR_0294 VAR_0295 VAR_0296 
# 918      918      918      918      918      918      918      918 
# VAR_0297 VAR_0298 VAR_0302 VAR_0303 VAR_0304 VAR_0306 VAR_0307 VAR_0308 
# 918      918      918      918      918      918      918      918 
# VAR_0309 VAR_0310 VAR_0311 VAR_0312 VAR_0313 VAR_0314 VAR_0315 VAR_0316 
# 918      918      918      918      918      918      918      918 
# VAR_0317 VAR_0318 VAR_0322 VAR_0323 VAR_0324 VAR_0326 VAR_0327 VAR_0328 
# 918      918      918      918      918      918      918      918 
# VAR_0329 VAR_0330 VAR_0331 VAR_0332 VAR_0333 VAR_0334 VAR_0338 VAR_0339 
# 918      918      918      918      918      918      918      918 
# VAR_0340 VAR_0341 VAR_0343 VAR_0344 VAR_0345 VAR_0346 VAR_0347 VAR_0348 
# 918      918      918      918      918      918      918      918 
# VAR_0349 VAR_0350 VAR_0351 VAR_0355 VAR_0356 VAR_0357 VAR_0358 VAR_0359 
# 918      927      918      918      918      918      918      918 
# VAR_0360 VAR_0361 VAR_0362 VAR_0363 VAR_0364 VAR_0365 VAR_0366 VAR_0367 
# 918      918      918      918      918      918      918      918 
# VAR_0368 VAR_0369 VAR_0371 VAR_0372 VAR_0373 VAR_0374 VAR_0375 VAR_0376 
# 918      918      918      918      918      918      918      918 
# VAR_0377 VAR_0378 VAR_0379 VAR_0380 VAR_0381 VAR_0382 VAR_0383 VAR_0384 
# 918      918      918      918      918      918      918      918 
# VAR_0385 VAR_0386 VAR_0387 VAR_0388 VAR_0389 VAR_0390 VAR_0391 VAR_0392 
# 918      918      918      918      918      918      918      918 
# VAR_0393 VAR_0394 VAR_0395 VAR_0396 VAR_0397 VAR_0398 VAR_0399 VAR_0400 
# 918      918      918      918      918      918      918      918 
# VAR_0401 VAR_0402 VAR_0403 VAR_0405 VAR_0406 VAR_0407 VAR_0408 VAR_0409 
# 918      918      918      918      918      918      918      918 
# VAR_0410 VAR_0411 VAR_0412 VAR_0413 VAR_0414 VAR_0415 VAR_0416 VAR_0417 
# 918      918      918      918      918      918      918      918 
# VAR_0418 VAR_0419 VAR_0420 VAR_0421 VAR_0422 VAR_0423 VAR_0424 VAR_0425 
# 918      918      918      918      918      918      918      918 
# VAR_0426 VAR_0427 VAR_0428 VAR_0429 VAR_0430 VAR_0431 VAR_0432 VAR_0433 
# 918      918      918      918      918      918      918      918 
# VAR_0434 VAR_0435 VAR_0436 VAR_0437 VAR_0438 VAR_0439 VAR_0440 VAR_0441 
# 918      918      918      918      918      918      918      918 
# VAR_0442 VAR_0443 VAR_0444 VAR_0445 VAR_0446 VAR_0447 VAR_0448 VAR_0449 
# 918      918      918      918      918      918      918      918 
# VAR_0450 VAR_0451 VAR_0452 VAR_0453 VAR_0454 VAR_0455 VAR_0456 VAR_0457 
# 918      918      918      918      918      918      918      918 
# VAR_0458 VAR_0459 VAR_0460 VAR_0461 VAR_0462 VAR_0463 VAR_0464 VAR_0465 
# 918      918      918      918      918      918      918      918 
# VAR_0468 VAR_0469 VAR_0470 VAR_0471 VAR_0472 VAR_0473 VAR_0474 VAR_0475 
# 918      918      918      918      918      918      918      918 
# VAR_0476 VAR_0477 VAR_0478 VAR_0479 VAR_0480 VAR_0481 VAR_0482 VAR_0483 
# 918      918      918      918      918      918      918      918 
# VAR_0484 VAR_0485 VAR_0486 VAR_0487 VAR_0488 VAR_0489 VAR_0490 VAR_0491 
# 918      918      918      918      918      918      918      918 
# VAR_0492 VAR_0494 VAR_0495 VAR_0496 VAR_0497 VAR_0498 VAR_0499 VAR_0500 
# 918      918      918      918      918      918      918      918 
# VAR_0501 VAR_0502 VAR_0503 VAR_0504 VAR_0505 VAR_0506 VAR_0507 VAR_0508 
# 918      918      918      918      918      918      918      918 
# VAR_0509 VAR_0510 VAR_0511 VAR_0512 VAR_0513 VAR_0514 VAR_0515 VAR_0516 
# 918      918      918      918      918      918      918      918 
# VAR_0517 VAR_0518 VAR_0519 VAR_0520 VAR_0521 VAR_0522 VAR_0523 VAR_0524 
# 918      918      918      918      918      918      918      918 
# VAR_0525 VAR_0526 VAR_0527 VAR_0528 VAR_0529 VAR_0530 VAR_0531 
# 918      917      918      918      917      918      917 

##################################
## 2.3 chech which columns could #
## be convereted into factor #####
##################################
## character columns
intColLevelsChar.Train <- CheckColLevels(dtRawTrain, names(charColClassChar)) # train set
# VAR_0001 VAR_0005 VAR_0008 VAR_0009 VAR_0010 VAR_0011 VAR_0012 VAR_0043 
# 3        4        1        1        1        1        1        1 
# VAR_0044 VAR_0073 VAR_0075 VAR_0156 VAR_0157 VAR_0158 VAR_0159 VAR_0166 
# 0     1458     2371      730      424      407      650     2145 
# VAR_0167 VAR_0168 VAR_0169 VAR_0176 VAR_0177 VAR_0178 VAR_0179 VAR_0196 
# 853     1645     1908     2163      945     1648     1875        1 
# VAR_0200 VAR_0202 VAR_0204 VAR_0214 VAR_0216 VAR_0217 VAR_0222 VAR_0226 
# 12386        1     1192       12        1      397        1        2 
# VAR_0229 VAR_0230 VAR_0232 VAR_0236 VAR_0237 VAR_0239 VAR_0274 VAR_0283 
# 1        2        2        2       45        1       57        7 
# VAR_0305 VAR_0325 VAR_0342 VAR_0352 VAR_0353 VAR_0354 VAR_0404 VAR_0466 
# 8        9       50        4        4        4     1823        2 
# VAR_0467 VAR_0493 VAR_1934 
# 4      608        5 
intColLevelsChar.Test <- CheckColLevels(dtRawTest, names(charColClassChar)) # test set
# VAR_0001 VAR_0005 VAR_0008 VAR_0009 VAR_0010 VAR_0011 VAR_0012 VAR_0043 
# 3        4        1        1        1        1        1        1 
# VAR_0044 VAR_0073 VAR_0075 VAR_0156 VAR_0157 VAR_0158 VAR_0159 VAR_0166 
# 0     1452     2369      701      387      390      635     2153 
# VAR_0167 VAR_0168 VAR_0169 VAR_0176 VAR_0177 VAR_0178 VAR_0179 VAR_0196 
# 857     1678     1935     2169      944     1676     1917        1 
# VAR_0200 VAR_0202 VAR_0204 VAR_0214 VAR_0216 VAR_0217 VAR_0222 VAR_0226 
# 12457        1     1190       16        1      397        1        2 
# VAR_0229 VAR_0230 VAR_0232 VAR_0236 VAR_0237 VAR_0239 VAR_0274 VAR_0283 
# 1        2        2        2       45        1       57        8 
# VAR_0305 VAR_0325 VAR_0342 VAR_0352 VAR_0353 VAR_0354 VAR_0404 VAR_0466 
# 8        9       50        4        4        4     1873        2 
# VAR_0467 VAR_0493 VAR_1934 
# 4      619        5 
## integer columns
intColLevelsInt.Train <- CheckColLevels(dtRawTrain, names(charColClassInt)) # way too many
intColLevelsInt.Test <- CheckColLevels(dtRawTest, names(charColClassInt)[-length(charColClassInt)]) # way too many
## numeric columns
intColLevelsNum.Train <- CheckColLevels(dtRawTrain, names(charColClassNum)) # train set
# VAR_0299 VAR_0300 VAR_0301 VAR_0319 VAR_0320 VAR_0321 VAR_0335 VAR_0336 
# 579      620      619      582      619      615      588      606 
# VAR_0337 VAR_0370 
# 620      493 
intColLevelsNum.Test <- CheckColLevels(dtRawTest, names(charColClassNum)) # test set
# VAR_0299 VAR_0300 VAR_0301 VAR_0319 VAR_0320 VAR_0321 VAR_0335 VAR_0336 
# 566      604      612      572      607      620      572      611 
# VAR_0337 VAR_0370 
# 601      493 
## integer64 column
intColLevelsInt64.Train <- CheckColLevels(dtRawTrain, names(charColClassInt64))
# VAR_0212 
# 128762
intColLevelsInt64.Test <- CheckColLevels(dtRawTest, names(charColClassInt64))
# VAR_0212 
# 128918











