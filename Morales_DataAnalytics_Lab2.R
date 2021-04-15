# Lab 2

if (!require("dplyr"))
  {
  install.packages("dplyr", dependencies = TRUE)
  library(dplyr)
  }
  
# Lab 2 Part 1
EPI_data <-read.csv("C:/Users/morale3/Desktop/Data_Analytics/Lab2/EPI_data.csv")
EPIvar <- EPI_data$EPI
EPI_mean <- mean(EPIvar)
EPI_mode <- mode(EPIvar)
EPI_median <- median(EPIvar)
hist(EPIvar, seq(30., 95.,1.0), prob = TRUE)

DALYvar <- EPI_data$DALY
DALY_mean <- mean(DALYvar)
DALY_mode <- mode(DALYvar)
DALY_median <- median(DALYvar)
hist(DALYvar, seq(0, 100,1.0), prob = TRUE)

EPIrand <- sample_n(EPI_data, 5, fac="EPI")
DALYrand <- sample_n(EPI_data, 5, fac = "DALY")

EPIfrac <- sample_frac(EPI_data, .1, fac = "EPI")
DALYfrac <- sample_frac(EPI_data, .1, fac = "DALY")

new_decs_EPI <- EPI_data %>% arrange(desc(EPI)) 
new_decs_DALY <- EPI_data %>% arrange(desc(DALY))

mutate(EPI_data, double_EPI = EPI_data$EPI * 2)

mutate(EPI_data, double_DALY = EPI_data$DALY *2)

summarise(EPI_data, EPI_mean = mean(EPIvar, na.rm = TRUE))
summarise(EPI_data, DALY_mean = mean(DALYvar, na.rm = TRUE))


boxplot(EPI_data$ENVHEALTH, EPI_data$ECOSYSTEM)
qqplot(EPI_data$ENVHEALTH, EPI_data$ECOSYSTEM)

EPI_europe <- EPI_data[EPI_data$EPI_regions == "Europe",]
head(EPI_europe)

drops <- c("code","ISO3V10","Country","EPI_regions","GEO_subregion", "GDPCAP07")
EPI_europe<- select (EPI_europe,-c("code","ISO3V10","Country","EPI_regions","GEO_subregion", "GDPCAP07"))

EURreg <- lm(EPI ~., data = EPI_europe)
#In Europe the single most important factor  in increasing the EPI is the ecosystem factor with the highest coefficient of 4.951 x 10^-1

attach(EPI_data);
boxplot(ENVHEALTH,DALY,AIR_H,WATER_H)
lmENVH<-lm(ENVHEALTH~DALY+AIR_H+WATER_H)
lmENVH
summary(lmENVH)
cENVH<-coef(lmENVH)
DALYNEW<-c(seq(5,95,5))
AIR_HNEW<-(seq(5,95,5))
NEW<-data.frame(DALYNEW,AIR_HNEW,WATER_HNEW)
pENV<-predict(lmENVH,NEW,interval="prediction")
cENV<-predict(lmENVH,NEW,interval="confidence")

# Repeat for AIR_E and CLIMATE
boxplot(EPI_data$ENVHEALTH, EPI_dataDALY, EPI_dataAIR_E, EPI_dataWATER_E)
lmAIR_E <- lm(EPI_data$AIR_E~ EPI_data$ENVHEALTH + EPI_data$DALY + EPI_data$WATER_E)
lmAIR_E
summary(lmAIR_E)
cAIR_E <- coef(lmAIR_E)

ENVH_NEW <- c(seq(5,95,5))
WATERE_NEW <- c(seq(5,95,5))
NEW <- data.frame(ENVH_NEW, DALYNEW, WATERE_NEW)

pAIR_E <-predict(lmAIR_E, NEW, interval = "prediction")
cAIR_E <- predict(lmAIR_E, NEW, intrval = "confidence")

boxplot(EPI_data$ENVHEALTH, EPI_dataDALY, EPI_dataAIR_E, EPI_dataCLIMATE)
lmCLIMATE <- lm(EPI_dataCLIMATE~ EPI_dataENVHEALTH + EPI_dataDALY + EPI_dataAIR_E)
lmCLIMATE
summary(lmCLIMATE)
cCLIMATE <- coef(lmCLIMATE)

ENVH_NEW <- c(seq(5,95,5))
AIR_E_NEW <- c(seq(5,95,5))
NEW <- data.frame(ENVH_NEW, DALYNEW, AIR_E_NEW)

pAIR_E <-predict(lmCLIMATE, NEW, interval = "prediction")
cAIR_E <- predict(lmCLIMATE, NEW, intrval = "confidence")


# Lab 2 Part 2
## Regression
multreg <- read.csv("C:/Users/morale3/Desktop/Data_Analytics/Lab2/dataset_multipleRegression.csv")

abalone <- read.csv("C:/Users/morale3/Desktop/Data_Analytics/Lab2/abalone.csv")

head(multreg)
unem <- multreg$UNEM
hgrad <- multreg$HGRAD
mltr <- lm(multreg$ROLL~ unem + hgrad)
mltr
predmltr <- predict(mltr, newdata = data.frame(unem = .07, hgrad=90000))
predmltr

inc <-multreg$INC
MltrInc <- lm(multreg$ROLL ~ unem + hgrad + inc)
MltrInc
predMltrInc <- predict(MltrInc, newdata = data.frame(unem = .07, hgrad = 90000, inc = 25000))
predMltrInc

## Classification
abalone <- read.csv(url("https://archive.ics.uci.edu/ml/machine-learning-databases/abalone/abalone.data"), header=FALSE, sep = ",")
colnames(abalone) <- c("sex", "length", 'diameter', 'height', 'whole_weight', 'shucked_weight', 'viscera_weight', 'shell_weight', 'rings')

head(abalone)
str(abalone)
abalone$rings <- as.numeric(abalone$rings)
#abalone$rings <- cut(abalone$rings,  br = c(-1,8,11,35), labels = c("young", "adult", "old"))
abalone$rings <- as.factor(abalone$rings)
aba <- abalone
aba$sex <- NULL
normalize <- function(x){
  return((x-min(x))/ (max(x)-min(x)))
}

aba[1:7]<- as.data.frame(lapply(aba[1:7],normalize))
ind<- sample(2,nrow(aba), replace = TRUE, prob=c(.7,.3))
knntrain<-aba[ind==1,]
knntest <-aba[ind==2,]
sqrt(2918)
library(class)
help("knn")
knnpred <- knn(train = knntrain[1:7], test = knntest[1:7], cl=knntrain$rings, k=55)
knnpred
table(knnpred)

## Clustering
str(iris)
summary(iris)
sapply(iris[,-5],var)
ggplot(iris,aes(x=Sepal.Length, y=Sepal.Width, col = Species)) + geom_point()
ggplot(iris, aes(x=Petal.Length, y=Petal.Width, col=Species)) + geom_point()

set.seed(300)
k.max<-12
wss<-sapply(1:k.max,function(k){kmeans(iris[,3:4],k,nstart=20,iter.max=1000)$tot.withinss})
wss
plot(1:k.max,wss,type="b", xlab = "Number of cluster(k)", ylab = "Within cluster sum of squares")
icluster <-kmeans(iris[,3:4],3,nstart=20)
table(iris[,5], icluster$cluster)