data3 <- read.csv2("dielectron3.csv",stringsAsFactors=FALSE)
data3$Run <- as.numeric(data3$Run)
data3$Event <- as.factor(data3$Event)

data3$E1 <- as.numeric(data3$E1)
data3$px1 <- as.numeric(data3$px1)
data3$py1 <- as.numeric(data3$py1)
data3$pz1 <- as.numeric(data3$pz1)
data3$pt1 <- as.numeric(data3$pt1)
data3$eta1 <- as.numeric(data3$eta1)
data3$phi1 <- as.numeric(data3$phi1)

data3$Q1 <- as.factor(data3$Q1)

data3$E2 <- as.numeric(data3$E2)
data3$px2 <- as.numeric(data3$px2)
data3$py2 <- as.numeric(data3$py2)
data3$pz2 <- as.numeric(data3$pz2)
data3$pt2 <- as.numeric(data3$pt2)
data3$eta2 <- as.numeric(data3$eta2)
data3$phi2 <- as.numeric(data3$phi2)

data3$Q2 <- as.factor(data3$Q2)

data3$M <- as.numeric(data3$M)
data4 <- data3[sample(1:nrow(data3),1000),]
str(data4)

summary(data4)
library(car)
scatterplot(E1 ~ pt1, data = data4,main="Scatter Plot of E1 and pt1")

scatterplot(phi1 ~ py1, data = data4,col= "orange",main="Scatter Plot of phi1 and py1")

scatterplot(eta1 ~ pz1, data = data4, col="purple",main="Scatter Plot of eta1 and pz1")

scatterplot(pt2 ~ E2, data = data4, col="green",main="Scatter Plot of pt2 and E2")

scatterplot(phi2 ~ py2, data = data4,col="red",main="Scatter Plot of py2 and phi2")