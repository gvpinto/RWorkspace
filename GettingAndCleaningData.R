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


## CREATING NEW VARIABLES ##
restData <- read.csv("./data/restaurants.csv")

## Creating Sequences
s1 <- seq(1, 10, by = 2); s1
s2 <- seq(1, 10, length = 3); s2

## Subsetting by resturants
restData$nearMe = restData$neighborhood %in% c("Roland Park", "Homeland")
restData$nearMe
table(restData$nearMe)

restData$zipWrong = ifelse(restData$zipCode < 0, TRUE, FALSE)
table(restData$zipWrong)
table(restData$zipWrong, restData$zipCode < 0)

## Creating Categorical data CUT
restData$zipGroups = cut(restData$zipCode, breaks = quantile(restData$zipCode))
table(restData$zipGroups)
table(restData$zipGroups, restData$zipCode)

install.packages("Hmisc")
library(Hmisc)
restData$zipGroups = cut2(restData$zipCode, g = 4)
table(restData$zipGroups)

## Create Factor Variables
restData$zcf <- factor(restData$zipCode)
restData$zcf[1:10]
class(restData$zcf)

## Levels of factor variables
yesno <- sample(c("yes", "no"), size = 10, replace = TRUE)
yesno
yesnofac = factor(yesno, levels=c("yes", "no"))
yesnofac
relevel(yesnofac, ref = "yes")
yesnofac
as.numeric(yesnofac)

## Cutting produces factor variables
library(Hmisc)
restData$zipGroups = cut2(restData$zipCode, g=4)
table(restData$zipGroups)
class(restData$zipGroups)

library(Hmisc); library(plyr)
restData2 = mutate(restData, zipGroups = cut2(zipCode, g = 4))
table(restData2$zipGroups)

## RESHAPING THE DATA ##
library(reshape2)
head(mtcars)
mtcars$carname <- rownames(mtcars)
carmelt <- melt(mtcars, id=c("carname", "gear", "cyl"), measure.vars=c("mpg", "hp"))
head(carmelt, n=3)
tail(carmelt, n=3)
## Length
cylData <- dcast(carmelt, cyl ~ variable)
cylData <- dcast(carmelt, cyl ~ variable, mean)
cylData

## Averaging Values
head(InsectSprays)
tapply(InsectSprays$count, InsectSprays$spray, sum)

spIns = split(InsectSprays$count, InsectSprays$spray)
spIns
sprCount = lapply(spIns, sum)
unlist(sprCount)
sapply(spIns, sum)

# Using Plyr packages
ddply(InsectSprays, .(spray), summarize, sum=sum(count))

# Creating a new Variable using ddply
spraySums <- ddply(InsectSprays, .(spray), summarize, sum=ave(count, FUN=sum))
spraySums

## MANAGING DATA FRAMES WITH DPLYR ##
options(width = 105)
chicago <- readRDS("chicago.rds")


library(dplyr)
chicago <- readRDS("chicago.rds")
dim(chicago)
str(chicago)
names(chicago)
head(select(chicago, city:dptp))
head(select(chicago, -(city:dptp)))
i <- match("city", names(chicago))
i <- match("dptp", names(chicago))
head(chicago[, -(i:j)])
chic.f <- filter(chicago, pm25tmean2 > 30)
head(chic.f, 10)
chic.f <- filter(chicago, pm25tmean2 > 30 & tmpd > 80)
head(chic.f, 10)
chicago <- arrange(chicago, date)
head(chicago)
tail(chicago)
chicago <- arrange(chicago, desc(date))
head(chicago)
tail(chicago)
chicago <- rename(chicago, pm25 = pm25tmean2, dewpoint = dptp)
head(chicago)
chicago <- mutate(chicago, pm25detrend = pm25 - mean(pm25, na.rm = TRUE))
head(select(chicago, pm25, pm25detrend))
chicago <- mutate(chicago, tempcat = factor(1 * (tmpd > 80), labels = c("cold", "hot")))
hotcold <- group_by(chicago, tempcat)
summarize(hotcold, pm25 = mean(pm25, na.rm = TRUE), o3 = max(o3tmean2), no2 = median(no2tmean2))
chicago <- mutate(chicago, year = as.POSIXlt(date)$year + 1900)
years <- group_by(chicago, year)
summarize(years, pm25 = mean(pm25, na.rm = TRUE), o3 = max(o3tmean2), no2 = median(no2tmean2))
chicago %>% mutate(month = as.POSIXlt(date)$mon + 1) %>% group_by(month) %>% summarize(pm25 = mean(pm25, na.rm = TRUE), o3 = max(o3tmean2), no2 = median(no2tmean2))

## MERGING DATA ##
url <- "http://www.plosone.org/article/info:doi/10.1371/journal.pone.0026895"
if (!file.exists("./data")) {dir.create("./data")}
fileUrl1 = "https://dl.dropboxusercontent.com/u/7710864/data/reviews-apr29.csv"
fileUrl2 = "https://dl.dropboxusercontent.com/u/7710864/data/solutions-apr29.csv"
download.file(fileUrl1, destfile = "./data/reviews.csv", method = "curl")
download.file(fileUrl2, destfile = "./data/solutions.csv", method = "curl")
reviews = read.csv("./data/reviews.csv"); solutions = read.csv("./data/solutions.csv")
head(reviews, 2)
head(solutions, 2)
# merge() x, y, by, by.x, by.y, all
names(reviews)
names(solutions)

mergedData = merge(reviews, solutions, by.x = "solution_id", by.y = "id", all = TRUE)
head(mergedData)
intersect(names(solutions), names(reviews))
mergedData2 = merge(reviews, solutions, all = TRUE)
head(mergedData2)

## Join in a plyr package
df1 = data.frame(id = sample(1:10), x = rnorm(10))
df2 = data.frame(id = sample(1:10), y = rnorm(10))
df3 = data.frame(id = sample(1:10), z = rnorm(10))
arrange(join(df1, df2), id)
dflist = list(df1, df2, df3)
join_all(dflist)

### WEEK 3 QUIZ ###
## QUESTION 1 ##

