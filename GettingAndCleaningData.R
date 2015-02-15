set.seet(13435)
X <- data.frame("var1" = sample(1:5), "var2" = sample(6:10), "var3" = sample(11:15))
X
X <- X[sample(1:5), ]
X
X$var2[c(1,3)] = NA
## Subsetting
X
X[,1]
X[, "var1"]
X[1:2, "var2"]
## Logical ands and ors
X[(X$var1 <= 3 & X$var3 > 11), ]
X[(X$var1 <= 2 | X$var3 >= 14), ]
X[(is.na(X$var2)), ]
X
X[which(X$var2 >= 8), ]
## Sorting
sort(X$var1)
sort(X$var1, decreasing = TRUE)
sort(X$var2, na.last = TRUE)
X
X[order(X$var1), ]
X[order(X$var1, X$var3), ]
## Sorting using plyer
library(plyr)
library(dplyr) ## Load plyer after dplyer
arrange(X, var1)
arrange(X, desc(var1))

## Adding Columns and Rows
X$var4 <- rnorm(5)
X
Y <- cbind(X, rnorm(5))
Y
Y <- cbind(rnorm(5), X)
Y
## SUMMARIZING DATA
fileUrl <- "https://data.baltimorecity.gov/api/views/k5ry-ef3g/rows.csv?accessType=DOWNLOAD"

if (getwd() != "/Users/gvpinto/R/GettingAndCleaningData") {
    setwd('/Users/gvpinto/R/GettingAndCleaningData')
}

if (!file.exists("data")) {
    dir.create("data")
}

if (file.exists("./data/restaurants.csv")) {
    file.remove("./data/restaurants.csv")
}

## Downlond csv file
download.file(fileUrl, destfile = "./data/restaurants.csv", method = "curl")
restData <- read.csv("./data/restaurants.csv")
head(restData, n=3)
tail(restData, n=3)
summary(restData)
str(restData)
quantile(restData$councilDistrict, na.rm=TRUE)
quantile(restData$councilDistrict, probs=c(0.5, 0.65, 0.9))
table(restData$zipCode, useNA = "ifany" )
table(restData$councilDistrict, restData$zipCode)

## Check for missing values NA
sum(is.na(restData$councilDistrict))
any(is.na(restData$councilDistrict))
all(restData$zipCode >0)

## Row and Column Sums
colSums(is.na(restData))
all(colSums(is.na(restData)) ==0)

##Values with specific characteristics
table(restData$zipCode %in% c("21212"))
table(restData$zipCode %in% c("21212", "21213"))
restData[restData$zipCode %in% c("21212", "21213"), ]

## Cross tabs
data(UCBAdmissions)
DF = as.data.frame(UCBAdmissions)
summary(DF)
DF
xt <- xtabs(Freq ~ Gender + Admit, data=DF)
xt

warpbreaks
warpbreaks$replicate <- rep(1:9, len = 54)
xt <- xtabs(breaks ~ ., data = warpbreaks)
xt

## Flat Table
ftable(xt)

## Size of the data set
fakeData = rnorm(1e5)
object.size(fakeData)
print(object.size(fakeData), units = "Mb")
?table