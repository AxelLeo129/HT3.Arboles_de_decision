setwd("C:/Users/LENOVO/Desktop/Clases/Minería de datos/Github/HT3.Arboles_de_decision")
library(corrplot)
library(nortest)
data <- read.csv("house-prices-advanced-regression-techniques/test.csv")


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

#######################
cat("LotFrontage")
bins=floor(sqrt(length(na.omit(data$LotFrontage))))
## Histograma
hist(data$LotFrontage, main ="LotFrontage", breaks = bins)
##  QQ-quantil normal
qqnorm(data$LotFrontage, pch = 1, frame = FALSE)
qqline(data$LotFrontage, col = "steelblue", lwd = 2)
## Lilliefonds
lillie.test(data$LotFrontage)
cat("\n")


#######################
cat("LotArea")
bins=floor(sqrt(length(na.omit(data$LotArea))))
## Histograma
hist(data$LotArea, main ="LotArea", breaks = bins)
##  QQ-quantil normal
qqnorm(data$LotArea, pch = 1, frame = FALSE)
qqline(data$LotArea, col = "steelblue", lwd = 2)
## Lilliefonds
lillie.test(data$LotArea)
cat("\n")

#######################
cat("YearBuilt")
bins=floor(sqrt(length(na.omit(data$YearBuilt))))
## Histograma
hist(data$YearBuilt, main ="YearBuilt", breaks = bins)
##  QQ-quantil normal
qqnorm(data$YearBuilt, pch = 1, frame = FALSE)
qqline(data$YearBuilt, col = "steelblue", lwd = 2)
## Lilliefonds
lillie.test(table(data$YearBuilt))
cat("\n")


#######################
cat("YearRemodAdd")
bins=floor(sqrt(length(na.omit(data$YearRemodAdd))))
## Histograma
hist(data$YearRemodAdd, main ="YearRemodAdd", breaks = bins)
##  QQ-quantil normal
qqnorm(data$YearRemodAdd, pch = 1, frame = FALSE)
qqline(data$YearRemodAdd, col = "steelblue", lwd = 2)
## Lilliefonds
lillie.test(table(data$YearRemodAdd))
cat("\n")


#######################
cat("MasVnrArea")
bins=floor(sqrt(length(na.omit(data$MasVnrArea))))
## Histograma
hist(data$MasVnrArea, main ="MasVnrArea", breaks = bins)
##  QQ-quantil normal
qqnorm(data$MasVnrArea, pch = 1, frame = FALSE)
qqline(data$MasVnrArea, col = "steelblue", lwd = 2)
## Lilliefonds
lillie.test(table(data$MasVnrArea))
cat("\n")

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




