add2 <- function(x, y) {
  x + y
}

above10 <- function(x) {
  use <- x > 10
  x[use]
}

above <- function(x, n) {
  use <- x > n
  x[use]
}

above <- function(x, n = 10) {
  use <- x > n
  x[use]
}

columnmean <- function(x, removeNA = T) {
  nc <- ncol(x)
  means <- numeric(nc)
  for (i in 1:nc) {
    means[i] <- mean(x[ , i], na.rm = removeNA)
  }
  means
}

f <- function(a) {
  print(a)
  print(b)
}

myplot <- function(x, y, type = 'I', ...) {
  plot(x, y, type = type, ...)
}

make.power <- function(n) {
  pow <- function(x) {
    x^n
  }
  pow
}



y <- 10

f <- function(x) {
  y <- 2
  y^2 + g(x)
}

g <- function(x) {
  x * y
}


x2 <- pi * 100^(-1:3)
round(x2, 3)
signif(x2, 3)


x <- 5
y <- if(x < 3) {
    NA
} else {
    10
}


f <- function(x) {
    g <- function(y) {
        y + z
    }
    z <- 4
    x + g(x)
}

x <- 1:10
if(x > 5) {
    x <- 0
} else {
    x <- 1
}

cube <- function(x, n) {
    x^3
}

h <- function(x, y = NULL, d = 3L) {
    z <- cbind(x, d)
    if(!is.null(y))
        z <- z + y
    else
        z <- z + f
    g <- x + y / z
    if(d == 3L)
        return(g)
    g <- g + 10
    g
}

printmessage <- function(x) {
    
    if (is.na(x)) {
        print("x is a missing valu")
    } else if (x > 0) {
        print("x greater than 0")
    } else {
        print("x less than 0")
    }
    invisible(x)
}


hilbert <- function(n) {
    i <- 1:n
    1 / outer(i-1, i, "+")
}


checkRank <- function(rank) {
    aRank <- as.numeric()
    if (is.character(rank) && (rank == "best" || rank == "worst")) {
        if (rank == "best") {
            aRank <- 1
        } else {
            aRank <- 2
        }
    } else if(is.numeric(rank)) {
        aRank <- rank
    } else {
        stop("invalid rank")
    }
    
    aRank
}


downloadData <- function() {
    setwd('R/GettingAndCleaningData')
    if (!file.exists("data")) {
        dir.create("data")
    }
    
    fileUrl <- "https://data.baltimorecity.gov/api/views/dz54-2aru/rows.csv?accessType=DOWNLOAD"
    download.file(fileUrl, destfile = "cameras.csv", method = "curl")
    dateDownloaded <- date()
}

cameraData <- read.table('./data/cameras.csv', sep = ',', header = TRUE)

downloadDataXlsx <- function() {
    if (getwd() != "/Users/gvpinto/R/GettingAndCleaningData") {
        setwd('/Users/gvpinto/R/GettingAndCleaningData')
    }

    if (!file.exists("data")) {
        dir.create("data")
    }
    
    fileUrl <- "https://data.baltimorecity.gov/api/views/dz54-2aru/rows.xlsx?accessType=DOWNLOAD"
    download.file(fileUrl, destfile = "./data/cameras.xslx", method = "curl")
    dateDownloaded <- date()

    library(xlsx)
    cameraData <- read.xlsx('./data/cameras.xlsx', sheetIndex = 1, header = TRUE)
    cameraData
    
}

install.packages('XML')
library(XML)
fileUrl <- 'http://www.w3schools.com/xml/simple.xml'
doc <- xmlTreeParse(fileUrl, useInternal = TRUE)

rootNode <-xmlRoot(doc)
xmlName(rootNode)

names(rootNode)

rootNode[[1]]

rootNode[[1]][[1]]

xpathSApply(rootNode, "//name", xmlValue)

xpathSApply(rootNode, "//price", xmlValue)

## Read data from Baltimore Ravens site
fileUrl <- "http://espn.go.com/nfl/team/_/name/bal/baltimore-ravens"
doc <- htmlTreeParse(fileUrl, useInternal = TRUE)
scores <-xpathSApply(doc, "//li[@class='score']", xmlValue)
teams <-xpathSApply(doc, "//li[@class='team-name']", xmlValue)

## JSON

install.packages("jsonlite")
library(jsonlite)
fileUrl <- https://api.github.com/users/jtleek/repos
jsonData <- fromJSON("https://api.github.com/users/jtleek/repos")
names(jsonData)
names(jsonData$owner)
names(jsonData$owner$login)
jsonData$owner$login

myjson <-toJSON(iris, pretty = TRUE)
cat(myjson)
jsonData <- fromJSON(myjson)
head(jsonData)

## Contcatenate
?cat

## http://wwww.r-bloggers.com/new-package-jsonlite-a-smarter-json-encoderdecoder/
#install
install.packages("jsonlite", repos="http://cran.r-project.org")

#load
library(jsonlite)

#convert object to json
myjson <- toJSON(iris, pretty=TRUE)
cat(myjson)

#convert json back to object
iris2 <- fromJSON(myjson)
print(iris2)

## data.table
install.packages("data.table")
library(data.table)
DF <- data.frame(x = rnorm(9), y = rep(c("a", "b", "c"), each = 3), z = rnorm(9))
head(DF)

DT <- data.table(x = rnorm(9), y = rep(c("a", "b", "c"), each = 3), z = rnorm(9))
head(DT)

## All the data tables in memory
tables()

## Subsetting date.frame
DT[2,]
DT[DT$y == "a", ] ## Subset Rows
DT[c(2,3)]  ## Subset Rows
DT[c(2,3),] ## Subset Rows
DT[,c(2,3)] ## Does not work for data tables

## List of functions
DT[, list(mean(x), sum(z))]

## 
?table

## Adding new Columns
DT[, w:= z^2]
tables()

## Copy does not create a new data.table
DT2 <- DT
DT[, y := 2]
tables()
head(DT, n=3)
head(DT2, n=3)

## Multiple operations
DT[, m := {tmp <- (x+z); log2(tmp + 5)}]
DT

## Plyer like operations
DT[, a := x > 0]
DT

DT[, b := mean(x + w), by = a]
DT

## Special Variables .N
set.seed(123)
DT <- data.table(x =  sample(LETTERS[1:3], 1E5, TRUE))

## Keys
DT <- data.table(x = rep(c("a", "b", "c"), each = 100), y = rnorm(300) )
setkey(DT, x)
DT["a"]

## Merging data.tables
DT1 <- data.table(x = c("a", "a", "b", "dt1"), y = 1:4)
DT2 <- data.table(x = c("a", "b", "dt2"), y = 5:7)
setkey(DT1, x); setkey(DT2, x)
merge(DT1, DT2)
tables()

## Fast reading
big_df <-  data.frame(x= rnorm(1E6), y = rnorm(1E6))
file <- tempfile()
write.table(big_df, file, row.names = FALSE, col.names = TRUE, sep = "\t", quote = FALSE)
system.time(fread(file))
system.time(read.table(file, header = TRUE, sep = "\t"))
tables()

## Quiz Week 1

## 1.
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv"
if (getwd() != "/Users/gvpinto/R/GettingAndCleaningData") {
    setwd('/Users/gvpinto/R/GettingAndCleaningData')
}

if (!file.exists("week1-quiz")) {
    dir.create("week1-quiz")
}
download.file(fileUrl, destfile = "./week1-quiz/microdata-survey.csv", method="curl")
microData <- read.table("./week1-quiz/microdata-survey.csv", sep = ",", header = TRUE)
object.size(microData)
head(microData)
names(microData)
summary(microData)
str(microData)

x <- microData[microData["VAL"] == 24, "VAL"]
length(x[!is.na(x)])
y <- microData[complete.cases(microData[, "VAL"]), ]
nrow(y)
nrow(y[y["VAL"] == 24, ])
?sum

## .2

unique(microData[, "FES"])

vignette("datatable-intro")
head(microData[, "FES", drop = FALSE], n = 500)

## .3

fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FDATA.gov_NGAP.xlsx"
if (getwd() != "/Users/gvpinto/R/GettingAndCleaningData") {
    setwd('/Users/gvpinto/R/GettingAndCleaningData')
}

if (getwd() != "C:\\Users\\bb2t1872\\R\\GettingAndCleaningData") {
  setwd('C:\\Users\\bb2t1872\\R\\GettingAndCleaningData')
}

if (!file.exists("week1-quiz")) {
    dir.create("week1-quiz")
}

download.file(fileUrl, destfile = "./week1-quiz/fdata_gov_NGAP.xlsx", method="curl", mode="wb")
download.file(fileUrl, destfile = ".\\week1-quiz\\fdata_gov_NGAP.xlsx", mode="wb")
dateDownloaded <- date()
Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jdk1.6.0_45\\jre')
install.packages("xlsx")
library(xlsx)
dat <- read.xlsx(".\\week1-quiz\\fdata_gov_NGAP.xlsx", sheetIndex = 1, header = TRUE, colIndex = c(7:15), rowIndex = c(18:23))
?read.xlsx
sum(dat$Zip*dat$Ext,na.rm=T)

## .4
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Frestaurants.xml"
?xmlTreeParse
doc <- xmlTreeParse(fileUrl, userInternal = TRUE)
install.packages("XmlTreeParse")
