shapiro.test(data4$Run)
shapiro.test(data4$E1)
shapiro.test(data4$px1)
shapiro.test(data4$py1)
shapiro.test(data4$pz1)
shapiro.test(data4$pt1)
shapiro.test(data4$eta1)
shapiro.test(data4$phi1)
shapiro.test(data4$E2)
shapiro.test(data4$px2)
shapiro.test(data4$py2)
shapiro.test(data4$pz2)
shapiro.test(data4$pt2)
shapiro.test(data4$eta2)
shapiro.test(data4$phi2)
shapiro.test(data4$M)
library("bestNormalize")
#for E1;
e1 <- bestNormalize(data4$E1)
ge1 <- predict(e1)
x_e1 <- predict(e1, newdata = ge1, inverse = TRUE)
shapiro.test(ge1)

#for px1; 
px1_1 <- bestNormalize(data4$px1)
gx1 <- predict(px1_1)
x_x1 <- predict(px1_1, newdata = gx1,inverse = TRUE)
shapiro.test(gx1)

#for py1;
py1_1 <- bestNormalize(data4$py1)
gy1 <- predict(py1_1)
x_y1 <- predict(py1_1, newdata = gy1,inverse = TRUE)
shapiro.test(gy1)

#for pz1;
pz1 <- bestNormalize(data4$pz1)
gz1 <- predict(pz1)
x_z1 <- predict(pz1,newdata=gz1,inverse = T)
shapiro.test(gz1)

#for pt1;
pt1 <- bestNormalize(data4$pt1)
gt1 <- predict(pt1)
x_t1 <- predict(pt1,newdata=gt1,inverse = T)
shapiro.test(gt1)

#for eta1;
eta1 <- bestNormalize(data4$eta1)
geta1 <- predict(eta1)
x_eta1 <- predict(eta1,newdata=geta1,inverse = T)
shapiro.test(geta1)

#for phi1;

phi1 <- bestNormalize(data4$phi1)
gphi1 <- predict(phi1)
x_phi1 <- predict(phi1,newdata=gphi1,inverse = T)
shapiro.test(gphi1)

#for E2;
e2 <- bestNormalize(data4$E2)
ge2 <- predict(e2)
x_e2 <- predict(e2, newdata = ge2, inverse = TRUE)
shapiro.test(ge2)

#for px2;
px2 <- bestNormalize(data4$px2)
gx2 <- predict(px2)
x_x2 <- predict(px2, newdata = gx2,inverse = TRUE)
shapiro.test(gx2)

#for py2;
py2 <- bestNormalize(data4$py2)
gy2 <- predict(py2)
x_y2 <- predict(py2, newdata = gy2,inverse = TRUE)
shapiro.test(gy2)

#for pz2;
pz2 <- bestNormalize(data4$pz2)
gz2 <- predict(pz2)
x_z2 <- predict(pz2,newdata=gz2,inverse = T)
shapiro.test(gz2)

#for pt2;
pt2 <- bestNormalize(data4$pt2)
gt2 <- predict(pt2)
x_t2 <- predict(pt2,newdata=gt2,inverse = T)
shapiro.test(gt2)

#for eta2;

eta2 <- bestNormalize(data4$eta2)
geta2 <- predict(eta2)
x_eta2 <- predict(eta2,newdata=geta2,inverse = T)
shapiro.test(geta2)

#for phi2;

phi2 <- bestNormalize(data4$phi2)
gphi2 <- predict(phi2)
x_phi2 <- predict(phi2,newdata=gphi2,inverse = T)
shapiro.test(gphi2)


#for M;

m <- bestNormalize(data4$M)
gm <- predict(m)
xm <- predict(m,newdata=gm,inverse = T)
shapiro.test(gm)

data4$E1 <- ge1
data4$px1 <- gx1
data4$py1 <- gy1
data4$pz1 <- gz1
data4$pt1 <- gt1
data4$eta1 <- geta1
data4$phi1 <- gphi1
data4$E2 <- ge2
data4$px2 <- gx2
data4$py2 <- gy2
data4$pz2 <- gz2
data4$pt2 <- gt2
data4$eta2 <- geta2
data4$phi2 <- gphi2
data4$M <- gm


library(dplyr)

data5 <- data4 %>% mutate(Sign =case_when(
  c(Q1==1 & Q2==1) ~ "mass",
  c(Q1==-1 & Q2== -1) ~ "mass",
  c(Q1==-1 & Q2==1)~ "dielectron",
  c(Q1==1 & Q2==-1) ~ "dielectron"))

library(heplots)
library(ICSNP)

a <- data5[c(3,4,5,6,11,12,13,14,20)]
