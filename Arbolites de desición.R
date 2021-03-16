setwd("C:/Users/LENOVO/Desktop/Clases/Minería de datos/Github/HT3.Arboles_de_decision")
library(corrplot)
library(nortest)
library(cluster) 
library(e1071)
library(mclust) 
library(fpc) 
library(NbClust) 
library(factoextra)
library(arsenal)
library(tidyverse)


data <- read.csv("house-prices-advanced-regression-techniques/test.csv")
cuantitativas <- data.frame(data$Id)

#data <-na.omit(data) #data[TRUE !=is.na(data),]

str(data)
bins = floor(sqrt(nrow(data)))
summary(data)
nombres <-names(data)
#### Variables cualitativas: caracteres
for (val in nombres  ){
  if(typeof(data[,val]) == 'character'){
    cat("\n")
    cat( val,"\n" )
    print(sort(table(na.exclude(data[,val])),decreasing = TRUE))
    barplot(table(na.exclude(data[,val])), main = val)}
}

### variables cualitativas:  intergers 

cat("MSSubClass")
print(sort(table(na.omit(data$MSSubClass)),decreasing = TRUE))
barplot(table(na.exclude(data$MSSubClass)), main ="MSSubClass" )
cat("\n") 

cat("OverallQual")
print(sort(table(na.omit(data$OverallQual)),decreasing = TRUE))
barplot(table(na.exclude(data$OverallQual)), main ="OverallQual" )
cat("\n") 

cat("OverallCond")
print(sort(table(data$OverallCond),decreasing = TRUE))
barplot(table(data$OverallCond), main ="OverallCond" )
cat("\n")

# Variales cuantitativas

for (val in nombres  ){
  if(typeof(data[,val]) == 'integer'){
    cat("\n")
    cat( val,"\n" )
    bins=floor(sqrt(length(na.omit(data[,val]))))
    hist(data[,val], main =val, breaks = bins)
    qqnorm(data[,val], pch = 1, frame = FALSE,main = val)
    qqline(data[,val], col = "steelblue", lwd = 2)
    print(lillie.test(data[,val]))
    
    }
}


### Comparando

bins=floor(sqrt(length(na.omit(data$X1stFlrSF))))
h1<-hist(data$X1stFlrSF,  breaks = bins, plot = FALSE)

bins=floor(sqrt(length(na.omit(data$X2ndFlrSF))))
h2<-hist(data$X2ndFlrSF, breaks = bins,plot = FALSE)

plot(h2,col  = rgb(0.1,0.7,0.2,0.3), xlab = "m^2", main = " Comparación de  áreas entre el primer y segundo nivel")
plot(h1, add = TRUE, col=rgb(0.7, 0.2, 0.1, 0.3))
legend("topright", 
       c("2nd floor", "1st floor"), 
       lty=c(1, 2), 
       col=c(rgb(0.1,0.7,0.2,0.3),rgb(0.7, 0.2, 0.1, 0.3)), 
       bty = "n")

nums <- sapply(data, is.numeric)
cuantitativas <- data[ , nums]
cuantitativas<-na.omit(cuantitativas)
###### 
cuantitativas$data.Id<- NULL
cuantitativas[, c('MSSubClass','OverallQual','OverallCond','YearBuilt', 'YearRemodAdd', 'LowQualFinSF','KitchenAbvGr')]<- NULL
cuantitativas<-na.omit(cuantitativas)

## Elbow graph
wss <- (nrow(cuantitativas)-1)*sum(apply(cuantitativas[,2:30],2,var))

for (i in 2:10) 
  wss[i] <- sum(kmeans(cuantitativas[,2:30], centers=i)$withinss)
plot(1:10, wss, type="b", xlab="Number of Clusters",  ylab="Within groups sum of squares")

Datos_clusters <- pam(cuantitativas, k = 5)
cuantitativas$cluster <- Datos_clusters$cluster


## Analizando clúster.
c1<-tableby(cluster ~ . ,data = cuantitativas)
print(table(summary(c1)))
