HotellingsT2(cbind(a$px1,a$py1,a$pz1,a$E1,a$px2,a$py2,a$pz2,a$E2)~a$Sign)
#According to the result, they are not equal.
#First of all,we are lots variable in order to explain our responde and most of them are insignificant or quite correlated each other. we remove non-numeric variables and, looked all correlations and then we took the most correlated ones and desired subsets for principal component regression,just have dielectrons;
data0 <- data5 %>% subset(Sign=="dielectron")
cor(data0[c(1,3,4,5,6,7,8,9,11,12,13,14,15,17)])

data6 <- data0[,c(3,5,6,7,8,9,11,13,15,17)]

res <- cor(data6, method="pearson")
corrplot::corrplot(res, method= "color", order = "hclust")
#And then, we scaled our data,and looked the scaled correlations again. The response variable that we choose is M (invariant mass of two electrons), and we dont add the pca analysis.After finding pca values, we found the eigenvalues and eigenvectors. Accorrding to the scree plot, first 5 components explain the almost 84% of the variability. And then, we checked independency. Because all of them independent, we can continue the analysis.
data7<-scale(data6)
cor(data7)

pca1 <- prcomp(data7)
summary(pca1)
library(dplyr)
pca1$x %>% head(6)
as.matrix(data7)%*%as.matrix(pca1$rotation) %>% head(6) 
pca1$rotation
pca1$sdev

library(factoextra) 
fviz_eig(pca1,addlabels=TRUE) 
pca<-pca1$x[,1:5]

head(pca)
res1 <- cor(pca, method="pearson")
corrplot::corrplot(res1, method= "color")

cor(data7,pca)


#The first 5 variables having the with high contribution;


fviz_pca_var(pca1,axes = c(1, 2))
fviz_pca_var(pca1,axes = c(2, 3))

fviz_pca_var(pca1, col.var = "contrib")+ scale_color_gradient2( low="red", mid="green",
                                                                high="blue", midpoint=96, space = "Lab")


ols.data <- data.frame(a=data0[,19],pca)
lmodel <- lm(a ~ ., data = ols.data)
summary(lmodel)
#According to the summary, pca's are significant.




