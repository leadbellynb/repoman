#0 Identifying info
print('Nina Bucar')
print('1188635')
print('nbucar@ucsc.edu')
 
#1 Load
library(foreign)
df.ex <- read.dta(file ="https://github.com/EconomiCurtis/econ294_2015/raw/master/data/org_example.dta")

#2 Filter
library(dplyr)
df.ex.dec2013 <- filter(df.ex, year==2013, month==12)
print(nrow(df.ex.dec2013))

#3 Arrange
df.ex.3a <- arrange(df.ex, year, month)
print(head(df.ex.3a))

#Select
df.ex.4a <- select(df.ex, year:age)
print(head(df.ex.4a))
df.ex.4b <- select(df.ex, year, month, starts_with('i'))
print(head(df.ex.4b))
state_distinct <- distinct(select(df.ex, state))

#5 Mutate
stndz <- function(x) {
  (x - mean(x, na.rm=T))  /  sd(x, na.rm=T)
}

nrmlz <- function(x) {
  xmin <- min(x, na.rm=T)
  (x - xmin) / (max(x, na.rm=T) - xmin)
}

df.ex5b <- mutate(df.ex, rw.stndz = stndz(rw), rw.nrmlz = nrmlz(rw))

df.ex.5b <- df.ex %>% 
  group_by(year, month) %>%
  mutate(
    rw.stndz = stndz(rw),
    rw.nrmlz = nrmlz(rw),
    rw.count = n()
  )
df.ex.5b %>% head %>% print
df.ex.5b %>% str %>% print
df.ex.5b %>% 
  filter(
    year==2013, 
    month==1) %>% 
  select(
    rw, 
    rw.stndz, 
    rw.nrmlz,
    rw.count) %>% 
  head %>% print

#6
df.ex.6 <- df.ex %>%
  group_by(year, month, state) %>%
  summarise(
    rw.min = min(rw, na.rm=T),
    rw.1st_quar = quantile(rw, 0.25, na.rm=T),
    rw.mean = mean(rw, na.rm=T),
    rw.median = median(rw, na.rm=T),
    rw.max = max(rw, na.rm=T),
    rw.count = n()
  ) 

df.ex.6 %>% nrow %>% print
highest.mean.rw <- arrange(df.ex.6, -rw.mean)
highest.mean.rw[1,]$rw.mean %>% print

#7
df.ex.7a <- arrange(df.ex, year, month, desc(as.character(df.ex$state)))
select(df.ex.7a, year, month, state) %>% head %>% print
