#1 data----
setwd("/Users/yinluli/Desktop/555 project")
data<- read.csv("Medicalpremium.csv", header = TRUE)
which(is.na(data))
dim(data)
#convert qualitative data
target_col <- "BloodPressureProblems"
data[data[[target_col]] == 0, target_col] <- "no"
data[data[[target_col]] == 1, target_col] <- "yes"
target_col1 <- "AnyTransplants"
data[data[[target_col1]] == 0, target_col1] <- "no"
data[data[[target_col1]] == 1, target_col1] <- "yes"
target_col2 <- "AnyChronicDiseases"
data[data[[target_col2]] == 0, target_col2] <- "no"
data[data[[target_col2]] == 1, target_col2] <- "yes"
target_col3 <- "KnownAllergies"
data[data[[target_col3]] == 0, target_col3] <- "no"
data[data[[target_col3]] == 1, target_col3] <- "yes"
target_col4 <- "Diabetes"
data[data[[target_col4]] == 0, target_col4] <- "no"
data[data[[target_col4]] == 1, target_col4] <- "yes"
target_col5 <- "HistoryOfCancerInFamily"
data[data[[target_col5]] == 0, target_col5] <- "no"
data[data[[target_col5]] == 1, target_col5] <- "yes"
write.csv(data, "clean_data.csv", row.names = TRUE)

#2 multi-linear regression---- 
#cor
demo<- data[c(1,6,7,11)]
round(cor(demo),3)
pairs.panels(demo, ellipse=F, col="hotpink", lwd=2, hist.col = "lightblue", stars = TRUE,  cex=0.6)
#f-test
qf(0.95, df1 = 2, df2 = 983)
lmdemo<- lm(demo$PremiumPrice ~ demo$Age + demo$Weight, data = demo)
lmdemo
summary(lmdemo)
anova(lmdemo)
#t-test
qt(0.975, df = 983)
#residual plot
par(mfrow=c(1,2))
plot(lmdemo$fitted.values, lmdemo$residuals, main = "Residual Plot", xlab = "Fitted Values",
     ylab = "Residuals", col = "skyblue", pch = 19, cex = 1.2)
abline(h=0, col = "gold", lwd = 2)
hist(lmdemo$residuals, col = "skyblue", border = "darkgrey", main = "Histogram of Residuals", xlab = "Residuals",
     breaks = 20)
#outliers
outlier_iqr <- function(x){
  iqr <- IQR(x,na.rm = T,type = 7)
  q <- quantile(x)
  upper_bound = q[4]+(iqr*1.5)
  lower_bound = q[2]-(iqr*1.5)
  outliers <- which ((x > upper_bound) | (x < lower_bound))
  return(outliers)
}
print(outlier_iqr(demo$age))
print(outlier_iqr(demo$Weight))
print(outlier_iqr(demo$PremiumPrice))

#3 all correlations----
library(corrplot)
cor <- cor(data)
corrplot(cor, method="shade", shade.col=NA, tl.col="black", tl.srt=45, 
         addCoef.col="black", addcolorlabel="no", title = "correlation graph")

#4 ANOVA 1----

is.factor(data$BloodPressureProblems)
group <- as.factor(data$BloodPressureProblems)
levels(group)
summary(group == "no")
aggregate(data$PremiumPrice, by =list(group), FUN=summary)

# boxplot
par(mfrow=c(1,1))
boxplot(data$PremiumPrice ~ group, data = data,
        main = "Boxplot of PremiumPrice by Group", xlab = "Group", ylab = "Premium Price",
        col = "lightblue", border = "gray", 
        medcol = "white", whisklty = 2, staplelty = 0,
        outcol = "hotpink", outpch = 16, outcex = 1.2)
#anova
qf(0.95, df1 = 1, df2 = 984)
qt(0.975, df = 984)
#dummy variable
data$g0<-ifelse(group == "no", 1, 0)
data$g1<-ifelse(group == "yes", 1, 0)
lm<- lm(data$PremiumPrice ~ data$g1, data = data)
summary(lm)

#5 ANOVA 2----
is.factor(data$AnyTransplants)
group1 <- as.factor(data$AnyTransplants)
levels(group1)
summary(group1 == "no")
aggregate(data$PremiumPrice, by =list(group1), FUN=summary)

# boxplot
boxplot(data$PremiumPrice ~ group1, data = data,
        main = "Boxplot of PremiumPrice by Group", xlab = "Group", ylab = "Premium Price",
        col = "lightblue", border = "gray", 
        medcol = "white", whisklty = 2, staplelty = 0,
        outcol = "hotpink", outpch = 16, outcex = 1.2)
#anova
qf(0.95, df1 = 1, df2 = 984)
qt(0.975, df = 984)
#dummy variable
data$g2<-ifelse(group1 == "no", 1, 0)
data$g3<-ifelse(group1 == "yes", 1, 0)
lm1<- lm(data$PremiumPrice ~ data$g3, data = data)
summary(lm1)

#6 ANOVA 3----
is.factor(data$AnyChronicDiseases)
group2 <- as.factor(data$AnyChronicDiseases)
levels(group2)
aggregate(data$AnyChronicDiseases, by =list(group2), FUN=summary)
aggregate(data$PremiumPrice, by =list(group2), FUN=summary)

# boxplot
boxplot(data$PremiumPrice ~ group2, data = data,
        main = "Boxplot of PremiumPrice by Group", xlab = "Group", ylab = "Premium Price",
        col = "lightblue", border = "gray", 
        medcol = "white", whisklty = 2, staplelty = 0,
        outcol = "hotpink", outpch = 16, outcex = 1.2)

#dummy variable
data$g4<-ifelse(group2 == "no", 1, 0)
data$g5<-ifelse(group2 == "yes", 1, 0)
lm2<- lm(data$PremiumPrice ~ data$g5, data = data)
summary(lm2)

#7 ANOVA 4----
is.factor(data$NumberOfMajorSurgeries)
group3 <- as.factor(data$NumberOfMajorSurgeries)
levels(group3)
summary(group3 == "0")
summary(group3 == "1")
summary(group3 == "2")
summary(group3 == "3")
aggregate(data$PremiumPrice, by =list(group3), FUN=summary)

# boxplot
boxplot(data$PremiumPrice ~ group3, data = data,
        main = "Boxplot of PremiumPrice by Group", xlab = "Group", ylab = "Premium Price",
        col = "lightblue", border = "gray", 
        medcol = "white", whisklty = 2, staplelty = 0,
        outcol = "hotpink", outpch = 16, outcex = 1.2)
qf(0.95, df1 = 3, df2 = 982)
#dummy variable
data$g6<-ifelse(group3 == "0", 1, 0)
data$g7<-ifelse(group3 == "1", 1, 0)
data$g8<-ifelse(group3 == "2", 1, 0)
data$g9<-ifelse(group3 == "3", 1, 0)
lm3<- lm(data$PremiumPrice ~ data$g7 + data$g8 +data$g9, data = data)
summary(lm3)

