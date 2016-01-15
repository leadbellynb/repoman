# 0. 
firstName <- "Nina"
lastName  <- "Bucar"

print(
  paste(            
    firstName,
    lastName
  )
)

studentID <- "1188635"
print(studentID)

# 1.
library(foreign)
df.dta <- read.dta(file = "https://github.com/EconomiCurtis/econ294_2015/raw/master/data/NHIS_2007_dta.dta")
df.csv <- read.csv(file = "https://github.com/EconomiCurtis/econ294_2015/raw/master/data/NHIS_2007_CSV.csv")
df.td <- read.delim(file = "https://github.com/EconomiCurtis/econ294_2015/raw/master/data/NHIS_2007_TSV.txt")
load(url("https://github.com/EconomiCurtis/econ294_2015/raw/master/data/NHIS_2007_RData.RData"))
print("The name assigned to this RData file is NHIS_2007_RData.")

#2
setwd("~/RStudio/rlab") #NEED TO SET THIS FOR THE FOLLOWING TO WORK
print(file.info("NHIS_2007_dta.dta")$size/1000)
print(file.info("NHIS_2007_CSV.csv")$size/1000)
print(file.info("NHIS_2007_TSV.txt")$size/1000)
print(file.info("NHIS_2007_RData.RData")$size/1000)
#these numbers were reported in my windows folder.
print(1545)
print(1030)
print(34)
print(34)
print("NHIS_2007_dta.dta is the smallest, followed by NHIS_2007_RData.RData")
print("The .csv and .txt files are in a large, human-readable text format")
print("The .RData file is in a small, machine-readable/binary? format")

#3
df.rdata <- NHIS_2007_RData #rename copy and remove
rm(NHIS_2007_RData)
print(typeof(df.rdata))
print(class(df.rdata))
print("df.rdata is a list and a data.frame")
print(length(df.rdata))
print(dim(df.rdata))
print(nrow(df.rdata))
print(ncol(df.rdata))
print(summary(df.rdata))

#4
df <- read.dta("https://github.com/EconomiCurtis/econ294_2015/raw/master/data/org_example.dta")
print(str(df))
print("There are 1119754 observations and 30 variables")
print(summary(df$rw))

#5
v <- c(1:7, 4, NULL, NA)
print(length(v))
print("There is one less value in v because NULL is not a valid value in vectors;")
print("R uses NULL for special purposes like removing values.")
print(mean(v, na.rm = TRUE))

#6
x <- matrix(data = 1:9, nrow = 3, ncol = 3, byrow = TRUE)
print(x)
xt <- t(x)
print(xt)
xe <- eigen(x)
print(xe$val)
print(xe$vec)
print(xe)
y <- matrix(data = c(1,2,3,3,2,1,2,3,0), nrow = 3, ncol = 3, byrow = TRUE)
print(y)
yi <- solve(y)
print(yi)
print(y%*%yi)
print("This is called an identity matrix.")

#7
carat <- c(5,2,0.5,1.5,5,NA,3)
cut <- c('fair', 'good', 'very good', 'good', 'fair', 'Ideal', 'fair')
clarity <- c("SI1", "I1", "VI1", "VS1", "IF", "VVS2", NA)
price <- c(850, 450, 450, NA , 750, 980, 420)
diamonds <- data.frame(carat, cut, clarity, price)
print(diamonds)
print(mean(diamonds$price, na.rm = TRUE))
dfair <- diamonds[diamonds$cut == 'fair', ]
print(dfair)
print(mean(dfair$price), na.rm = TRUE)
notfair <- diamonds[diamonds$cut != 'fair', ]
print(notfair)
print(mean(notfair$price, na.rm = TRUE))
d2 <- diamonds[diamonds$carat > 2 & (diamonds$cut == 'Ideal' | diamonds$cut == 'very good'), ]
print(d2)
print(median(d2$price))
print("The mean is NA because the filter conditions result in no rows")
