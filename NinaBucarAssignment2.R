#0
NinaBucarAssignment2 <- list(
  firstName = "Nina",
  lastName  = "Bucar",
  email     = "nbucar@ucsc.edu",
  studentID = 1188635
)

#1
library(RCurl) ; #install.packages("RCurl")
diamondsURL <- getURL("https://raw.githubusercontent.com/EconomiCurtis/econ294_2015/master/data/diamonds.CSV")
diamonds <- read.csv(text=diamondsURL)
rm(diamondsURL)

NinaBucarAssignment2$s1a <- nrow(diamonds)
NinaBucarAssignment2$s1b <- ncol(diamonds)
NinaBucarAssignment2$s1c <- names(diamonds)
NinaBucarAssignment2$s1d <- summary(diamonds$price)

#2
df.td <- read.delim("https://github.com/EconomiCurtis/econ294_2015/raw/master/data/NHIS_2007_TSV.txt")

NinaBucarAssignment2$s2a <- nrow(df.td)
NinaBucarAssignment2$s2b <- ncol(df.td)
NinaBucarAssignment2$s2c <- names(df.td)
NinaBucarAssignment2$s2d <- mean(df.td$weight, na.rm=T)
NinaBucarAssignment2$s2e <- median(df.td$weight, na.rm=T)

hist(df.td$weight, col="pink")

df.td$weight.adj <- ifelse(df.td$weight>=996, NA, df.td$weight)

hist(df.td$weight.adj, col="pink")

NinaBucarAssignment2$s2f <- mean(df.td$weight.adj, na.rm=T)
NinaBucarAssignment2$s2g <- median(df.td$weight.adj, na.rm=T)

NinaBucarAssignment2$s2h <- summary(df.td[df.td$SEX==1,]$weight.adj)
NinaBucarAssignment2$s2i <- summary(df.td[df.td$SEX==2,]$weight.adj)

#3
vec <- c(letters, LETTERS)

NinaBucarAssignment2$s3a <- vec[(1:26)*2]  # or vec[seq(2,52,2)]
NinaBucarAssignment2$s3b <- paste(vec[c(14+26,9,14)], collapse="")

arr <- array( c(letters, LETTERS), dim = c(3,3,3))

NinaBucarAssignment2$s3c <- arr[,1,2]
NinaBucarAssignment2$s3d <- arr[2,2,]
NinaBucarAssignment2$s3e <- paste(arr[2,2,2],arr[3,3,1],arr[2,2,2], sep = "")

#4
library(foreign)
org_example <- read.dta("https://github.com/EconomiCurtis/econ294_2015/raw/master/data/org_example.dta")

# workaround because the read.dta above sometimes times out
#setwd("~/RStudio/rlab")
#org_example <- read.dta("org_example.dta")

rw_mean_df <- aggregate(
  org_example$rw, 
  by=list(org_example$educ,org_example$month,org_example$year), 
  FUN=mean, na.rm=T
)

rw_mean_df <- rw_mean_df[, c(3,2,1,4)] # reorder the columns
names(rw_mean_df) <- c("year", "month", "educ", "rw")

NinaBucarAssignment2$s4 <- rw_mean_df

#print(NinaBucarAssignment2)
#setwd("~/RStudio/rlab")
#save(NinaBucarAssignment2, file ="NinaBucarAssignment2.RData")
