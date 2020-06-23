traindata = read.csv(file.choose())
View(traindata)
attach(traindata)
traindata = traindata[-1]
is.na(traindata)
sum(is.na(traindata))



testdata = read.csv(file.choose())
View(testdata)
attach(testdata)
testdata = testdata[-1]
is.na(testdata)
sum(is.na(testdata))




#Measures of Central Tendency for train data

mean(traindata$ambient)        ##-0.00312
median(traindata$ambient)      ##0.266325

mean(traindata$coolant)        ##0.00477
median(traindata$coolant)      ##-0.1766

mean(traindata$u_d)            ##0.00427
median(traindata$u_d)          ##0.26750

mean(traindata$u_q)            ##-0.0058
median(traindata$u_q)          ##-0.0995

mean(traindata$motor_speed)    ##-0.0060
median(traindata$motor_speed)  ##-0.1402

mean(traindata$torque)         ##-0.0029
median(traindata$torque)       ##-0.1871

mean(traindata$i_d)            ##0.00579
median(traindata$i_d)          ##0.21318

mean(traindata$pm)             ##-0.0043
median(traindata$pm)           ##0.09413

mean(traindata$stator_yoke)    ##0.000622
median(traindata$stator_yoke)  ##-0.05726

mean(traindata$stator_tooth)   ##-0.0022
median(traindata$stator_tooth) ##0.0052

mean(traindata$stator_winding) ##-0.00398
median(traindata$stator_winding)##0.00689

mean(traindata$profile_id)     ##50.732
median(traindata$profile_id)   ##56



#Measures of Central Tendency for test data

mean(testdata$ambient)        ##-0.00571
median(testdata$ambient)      ##0.26575

mean(testdata$coolant)        ##0.00459
median(testdata$coolant)      ##-0.1781

mean(testdata$u_d)            ##0.00596
median(testdata$u_d)          ##0.26776

mean(testdata$u_q)            ##-0.00538
median(testdata$u_q)          ##-0.1007

mean(testdata$motor_speed)    ##-0.00706
median(testdata$motor_speed)  ##-0.14024

mean(testdata$torque)         ##-0.00414
median(testdata$torque)       ##-0.18745

mean(testdata$i_d)            ##0.006622
median(traindata$i_d)         ##0.21318

mean(testdata$stator_yoke)    ##0.000577
median(testdata$stator_yoke)  ##-0.05715

mean(testdata$stator_tooth)   ##-0.00219
median(testdata$stator_tooth) ##0.004695

mean(testdata$stator_winding) ##-0.00381
median(testdata$stator_winding) ##0.00560

mean(testdata$profile_id)     ##50.718
median(testdata$profile_id)   ##56



#mode it is the output for train dataset

getmode <- function(x){
  uniquv <- unique(x)
  uniquv[which.max(tabulate(match(x,uniquv)))]
}
getmode(traindata$motor_speed) ## -1.22243



#mode it is the output for test dataset

getmode <- function(x){
  uniquv <- unique(x)
  uniquv[which.max(tabulate(match(x,uniquv)))]
}
getmode(testdata$motor_speed) ##-1.22243




#Measures of Dispersion for train dataset

var(traindata$ambient)          ##0.9854
sd(traindata$ambient)           ##0.9927
range(traindata$ambient)        ## -7.962 to 2.9671
rangevalue <- function(x){max(x)-min(x)}
rangevalue(traindata$ambient)   ##10.93


var(traindata$coolant)         ##1.004176
sd(traindata$coolant)          ##1.002086
range(traindata$coolant)       ## -1.3678 to 2.6490
rangevalue <- function(x){max(x)-min(x)}
rangevalue(traindata$coolant)  ##4.0168

var(traindata$u_d)             ## 0.99546
sd(traindata$u_d)              ## 0.99772
range(traindata$u_d)           ## -1.6548 to 2.2747
rangevalue <- function(x){max(x)-min(x)}
rangevalue(traindata$u_d)      ##3.9296

var(traindata$u_q)             ##1.004814
sd(traindata$u_q)              ##1.002404
range(traindata$u_q)           ## -1.8614  to 1.739498
rangevalue <- function(x){max(x)-min(x)}
rangevalue(traindata$u_q)      ## 3.654961

var(traindata$motor_speed)     ##1.003404
sd(traindata$motor_speed)      ##1.0017
range(traindata$motor_speed)   ## -1.37152 to 2.02415
rangevalue <- function(x){max(x)-min(x)}
rangevalue(traindata$motor_speed) ##3.39568

var(traindata$torque)          ##0.99481
sd(traindata$torque)           ##0.99740
range(traindata$torque)        ## -3.3459 to 3.0168
rangevalue <- function(x){max(x)-min(x)}
rangevalue(traindata$torque)   ##6.3628

var(traindata$i_d)             ##0.99801
sd(traindata$i_d)              ##0.99900
range(traindata$i_d)           ## -3.2458 to 1.06093
rangevalue <- function(x){max(x)-min(x)}
rangevalue(traindata$i_d)      ##4.30681

var(traindata$i_q)             ##0.9947
sd(traindata$i_q)              ##0.99737
range(traindata$i_q)           ## -3.34163 to 2.9141
rangevalue <- function(x){max(x)-min(x)}
rangevalue(traindata$i_q)      ##6.2558

var(traindata$pm)              ##0.9909
sd(traindata$pm)               ##0.9954
range(traindata$pm)            ## -2.6319 to 2.9174
rangevalue <- function(x){max(x)-min(x)}
rangevalue(traindata$pm)       ## 5.5494

var(traindata$stator_yoke)     ##1.0022
sd(traindata$stator_yoke)      ##1.0011
range(traindata$stator_yoke)   ## -1.8346 to 2.4491
rangevalue <- function(x){max(x)-min(x)}
rangevalue(traindata$stator_yoke) ## 4.2838

var(traindata$stator_tooth)    ##0.9994
sd(traindata$stator_tooth)     ##0.9997
range(traindata$stator_tooth)  ## -2.06614 to 2.3266
rangevalue <- function(x){max(x)-min(x)}
rangevalue(traindata$stator_tooth) ##4.3928

var(traindata$stator_winding)  ##0.9968
sd(traindata$stator_winding)   ##0.9984
range(traindata$stator_winding)## -2.0199 to 2.6518
rangevalue <- function(x){max(x)-min(x)}
rangevalue(traindata$stator_winding) ##4.6717

var(traindata$profile_id)      ##487.213
sd(traindata$profile_id)       ##22.0729
range(traindata$profile_id)    ## 4 to 81
rangevalue <- function(x){max(x)-min(x)}
rangevalue(traindata$profile_id) ##77


#Measures of Dispersion for test dataset

var(testdata$ambient)          ##0.9882
sd(testdata$ambient)           ##0.9940
range(testdata$ambient)        ## -8.573 to 2.9544
rangevalue <- function(x){max(x)-min(x)}
rangevalue(testdata$ambient)   ##11.52862


var(testdata$coolant)         ##1.00643
sd(testdata$coolant)          ##1.00321
range(testdata$coolant)       ## -1.4293 to 2.2968
rangevalue <- function(x){max(x)-min(x)}
rangevalue(testdata$coolant)  ##3.7262

var(testdata$u_d)             ## 0.99645
sd(testdata$u_d)              ## 0.99822
range(testdata$u_d)           ## -1.6553 to 2.2736
rangevalue <- function(x){max(x)-min(x)}
rangevalue(testdata$u_d)      ##3.9289

var(testdata$u_q)             ##1.004323
sd(testdata$u_q)              ##1.002159
range(testdata$u_q)           ## -1.8601  to 1.7933
rangevalue <- function(x){max(x)-min(x)}
rangevalue(testdata$u_q)      ## 3.6535

var(testdata$motor_speed)     ##1.00026
sd(testdata$motor_speed)      ##1.00013
range(testdata$motor_speed)   ## -1.2706 to 2.0241
rangevalue <- function(x){max(x)-min(x)}
rangevalue(testdata$motor_speed) ##3.294

var(testdata$torque)          ##0.9981
sd(testdata$torque)           ##0.9990
range(testdata$torque)        ## -3.3411 to 3.0169
rangevalue <- function(x){max(x)-min(x)}
rangevalue(testdata$torque)   ##6.3581

var(testdata$i_d)             ##0.9979
sd(testdata$i_d)              ##0.9989
range(testdata$i_d)           ## -3.2444 to 1.0602
rangevalue <- function(x){max(x)-min(x)}
rangevalue(testdata$i_d)      ##4.3047

var(testdata$i_q)             ##0.9983
sd(testdata$i_q)              ##0.9991
range(testdata$i_q)           ## -3.3368 to 2.9141
rangevalue <- function(x){max(x)-min(x)}
rangevalue(testdata$i_q)      ##6.2509

var(testdata$stator_yoke)     ##1.0017
sd(testdata$stator_yoke)      ##1.0008
range(testdata$stator_yoke)   ## -1.8335 to 2.4468
rangevalue <- function(x){max(x)-min(x)}
rangevalue(testdata$stator_yoke) ## 4.2803

var(testdata$stator_tooth)    ##0.9985
sd(testdata$stator_tooth)     ##0.9992
range(testdata$stator_tooth)  ## -2.0648 to 2.3257
rangevalue <- function(x){max(x)-min(x)}
rangevalue(testdata$stator_tooth) ##4.3905

var(testdata$stator_winding)  ##0.9962
sd(testdata$stator_winding)   ##0.9981
range(testdata$stator_winding)## -2.0192 to 2.6537
rangevalue <- function(x){max(x)-min(x)}
rangevalue(testdata$stator_winding) ##4.6730

var(testdata$profile_id)      ##487.245
sd(testdata$profile_id)       ##22.0736
range(testdata$profile_id)    ## 4 to 81
rangevalue <- function(x){max(x)-min(x)}
rangevalue(testdata$profile_id) ##77




#Measures of skewness of train dataset
install.packages("moments")
library(moments)

skewness(traindata$ambient)        ##-0.8472
skewness(traindata$coolant)        ##0.6282
skewness(traindata$u_d)            ##0.1952
skewness(traindata$u_q)            ##0.1998
skewness(traindata$motor_speed)    ##0.3328
skewness(traindata$torque)         ##-0.0435
skewness(traindata$i_d)            ##-0.6215
skewness(traindata$i_q)            ##-0.0763
skewness(traindata$pm)             ##-0.2314
skewness(traindata$stator_yoke)    ##0.2573
skewness(traindata$stator_tooth)   ##-0.0612
skewness(traindata$stator_winding) ##-0.0281
skewness(traindata$profile_id)     ##-0.6279


#Measures of skewness of test dataset


skewness(testdata$ambient)        ##-0.8528
skewness(testdata$coolant)        ##0.6282
skewness(testdata$u_d)            ##0.1932
skewness(testdata$u_q)            ##0.1998
skewness(testdata$motor_speed)    ##0.3344
skewness(testdata$torque)         ##-0.0405
skewness(testdata$i_d)            ##-0.6249
skewness(testdata$i_q)            ##-0.0740
skewness(testdata$stator_yoke)    ##0.2572
skewness(testdata$stator_tooth)   ##-0.0622
skewness(testdata$stator_winding) ##-0.0277
skewness(testdata$profile_id)     ##-0.6294



#Measures of Kurtosis of train dataset


kurtosis(traindata$ambient)        ##3.8178
kurtosis(traindata$coolant)        ##2.2408
kurtosis(traindata$u_d)            ##2.4777
kurtosis(traindata$u_q)            ##1.7286
kurtosis(traindata$motor_speed)    ##1.8314
kurtosis(traindata$torque)         ##3.7811
kurtosis(traindata$i_d)            ##2.2441
kurtosis(traindata$i_q)            ##3.7870
kurtosis(traindata$pm)             ##2.6530
kurtosis(traindata$stator_yoke)    ##2.2700
kurtosis(traindata$stator_tooth)   ##2.2195
kurtosis(traindata$stator_winding) ##2.2709
kurtosis(traindata$profile_id)     ##2.3470


#Measures of kurtosis of test dataset


kurtosis(testdata$ambient)        ##3.8329
kurtosis(testdata$coolant)        ##2.2382
kurtosis(testdata$u_d)            ##2.4757
kurtosis(testdata$u_q)            ##1.7288
kurtosis(testdata$motor_speed)    ##1.8367
kurtosis(testdata$torque)         ##3.7749
kurtosis(testdata$i_d)            ##2.2500
kurtosis(testdata$i_q)            ##3.7800
kurtosis(testdata$stator_yoke)    ##2.2734
kurtosis(testdata$stator_tooth)   ##2.2211
kurtosis(testdata$stator_winding) ##2.2729
kurtosis(testdata$profile_id)     ##2.3480


#Graphical Representation of train dataset

#Boxplot Representation of train dataset
boxplot(traindata, vertical = TRUE)
boxplot(traindata$ambient,horizontal = TRUE)
boxplot(traindata$coolant,horizontal = TRUE)
boxplot(traindata$u_d,horizontal = TRUE)
boxplot(traindata$u_q,horizontal = TRUE)
boxplot(traindata$motor_speed,horizontal = TRUE)
boxplot(traindata$torque,horizontal = TRUE)
boxplot(traindata$i_d,horizontal = TRUE)
boxplot(traindata$i_q,horizontal = TRUE)
boxplot(traindata$pm,horizontal = TRUE)
boxplot(traindata$stator_yoke,horizontal = TRUE)
boxplot(traindata$stator_tooth,horizontal = TRUE)
boxplot(traindata$stator_winding,horizontal = TRUE)
boxplot(traindata$profile_id,horizontal = TRUE)


#Histogram Representation of train dataset

hist(traindata$ambient)
hist(traindata$coolant)
hist(traindata$u_d)
hist(traindata$u_q)
hist(traindata$motor_speed)
hist(traindata$torque)
hist(traindata$i_d)
hist(traindata$i_q)
hist(traindata$pm)
hist(traindata$stator_yoke)
hist(traindata$stator_tooth)
hist(traindata$stator_winding)
hist(traindata$profile_id)


#Barplot Representation of train dataset


barplot(traindata$ambient)
barplot(traindata$coolant)
barplot(traindata$u_d)
barplot(traindata$u_q)
barplot(traindata$motor_speed)
barplot(traindata$torque)
barplot(traindata$i_d)
barplot(traindata$i_q)
barplot(traindata$pm)
barplot(traindata$stator_yoke)
barplot(traindata$stator_tooth)
barplot(traindata$stator_winding)
barplot(traindata$profile_id)


#Graphical Representation of train dataset

#Boxplot Representation of test dataset

boxplot(testdata, vertical = TRUE)
boxplot(testdata$ambient,horizontal = TRUE)
boxplot(testdata$coolant,horizontal = TRUE)
boxplot(testdata$u_d,horizontal = TRUE)
boxplot(testdata$u_q,horizontal = TRUE)
boxplot(testdata$motor_speed,horizontal = TRUE)
boxplot(testdata$torque,horizontal = TRUE)
boxplot(testdata$i_d,horizontal = TRUE)
boxplot(testdata$i_q,horizontal = TRUE)
boxplot(testdata$stator_yoke,horizontal = TRUE)
boxplot(testdata$stator_tooth,horizontal = TRUE)
boxplot(testdata$stator_winding,horizontal = TRUE)
boxplot(traindata$profile_id,horizontal = TRUE)


#Histogram Representation of train dataset

hist(testdata$ambient)
hist(testdata$coolant)
hist(testdata$u_d)
hist(testdata$u_q)
hist(testdata$motor_speed)
hist(testdata$torque)
hist(testdata$i_d)
hist(testdata$i_q)
hist(testdata$stator_yoke)
hist(testdata$stator_tooth)
hist(testdata$stator_winding)
hist(testdata$profile_id)


#Barplot Representation of test dataset


barplot(testdata$ambient)
barplot(testdata$coolant)
barplot(testdata$u_d)
barplot(testdata$u_q)
barplot(testdata$motor_speed)
barplot(testdata$torque)
barplot(testdata$i_d)
barplot(testdata$i_q)
barplot(testdata$stator_yoke)
barplot(testdata$stator_tooth)
barplot(testdata$stator_winding)
barplot(testdata$profile_id)

summary(traindata)
str(traindata)   ## 52 levels starting from 4 to 81
traindata$profile_id <- as.factor(traindata$profile_id)

summary(testdata)
str(testdata)   ## 52 levels starting from 4 to 81
testdata$profile_id <- as.factor(testdata$profile_id)



## install.packages(psych)
library(psych)

## QQ-plots Performing on all columns of train dataset

qqnorm(traindata$ambient)
qqline(traindata$ambient)

qqnorm(traindata$coolant)
qqline(traindata$coolant)

qqnorm(traindata$u_d)
qqline(traindata$u_d)

qqnorm(traindata$u_q)
qqline(traindata$u_q)

qqnorm(traindata$motor_speed)
qqline(traindata$motor_speed)

qqnorm(traindata$torque)
qqline(traindata$torque)

qqnorm(traindata$i_d)
qqline(traindata$i_d)

qqnorm(traindata$i_q)
qqline(traindata$i_q)

qqnorm(traindata$pm)
qqline(traindata$pm)

qqnorm(traindata$stator_yoke)
qqline(traindata$stator_yoke)

qqnorm(traindata$stator_tooth)
qqline(traindata$stator_tooth)

qqnorm(traindata$stator_winding)
qqline(traindata$stator_winding)



## QQ-plots Performing on all columns of test dataset

qqnorm(testdata$ambient)
qqline(testdata$ambient)

qqnorm(testdata$coolant)
qqline(testdata$coolant)

qqnorm(testdata$u_d)
qqline(testdata$u_d)

qqnorm(testdata$u_q)
qqline(testdata$u_q)

qqnorm(testdata$motor_speed)
qqline(testdata$motor_speed)

qqnorm(testdata$torque)
qqline(testdata$torque)

qqnorm(testdata$i_d)
qqline(testdata$i_d)

qqnorm(testdata$i_q)
qqline(testdata$i_q)

qqnorm(testdata$stator_yoke)
qqline(testdata$stator_yoke)

qqnorm(testdata$stator_tooth)
qqline(testdata$stator_tooth)

qqnorm(testdata$stator_winding)
qqline(testdata$stator_winding)

