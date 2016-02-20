#0
print('Nina Bucar')
print('1188635')

library(foreign)
library(dplyr)
library(tidyr)

#1
flights <- read.csv("flights.csv", stringsAsFactors = FALSE)
planes <- read.csv("planes.csv", stringsAsFactors = FALSE)
weather <- read.csv("weather.csv", stringsAsFactors = FALSE)
airports <- read.csv("airports.csv", stringsAsFactors = FALSE)


#2
flights$date <- as.Date(flights$date)
weather$date <- as.Date(weather$date)
flights %>% head %>% print


#3
flights.2a <- flights %>% filter(dest == 'SFO' | dest == 'OAK')
flights.2a %>% nrow %>% print

flights.2b <- flights %>% filter(dep_delay >= 60)
flights.2b %>% nrow %>% print

flights.2c <- flights %>% filter(arr_delay > 2*dep_delay)
flights %>% nrow %>% print


#4
flights %>% select(ends_with('delay')) %>% head %>% print
flights %>% select(contains("_delay")) %>% head %>% print
flights %>% select(matches("(.*)_delay")) %>% head %>% print


#5a.
flights %>% arrange(-dep_delay) %>% head(5) %>% print
#5b.
flights %>% arrange(-(dep_delay - arr_delay)) %>% head(5) %>% print


#6
flights <- flights %>% mutate(speed = dist / (time/60))
flights %>% head %>% print

flights <- flights %>% mutate(delta = dep_delay - arr_delay)
flights %>% head %>% print

#6a.
flights %>% arrange(-speed) %>% head(5) %>% print

#6b.
flights %>% arrange(-delta) %>% head(5) %>% print

#6c.
flights %>% arrange(delta) %>% head(5) %>% print

#7
flights.7a <- flights %>%
  group_by(carrier) %>%
  summarize(
  cancelled.no = sum(cancelled, na.rm = TRUE),
  total = n(),
  cancelled.pct = (cancelled.no / total) * 100,
  delta.min = min(delta, na.rm = TRUE),
  delta.quart1 = quantile(delta, .25, na.rm = TRUE),
  delta.median = median(delta, na.rm = TRUE),
  delta.mean = mean(delta, na.rm = TRUE),
  delta.quart3 = quantile(delta, .75, na.rm = TRUE),
  delta.quant90 = quantile(delta, .90, na.rm = TRUE),
  delta.max = max(delta, na.rm = TRUE)
)

flights.7a %>% arrange(-cancelled.pct) %>% print

day_delay <- dplyr::filter(
  summarize(
    group_by(
      dplyr::filter(
        flights,
        !is.na(dep_delay)
      ),
      date
    ),
    delay = mean(dep_delay),
    n = n()
  ),
  n > 10
)

cat(
  "The code filters out (removes) all rows whose departure delay (dep_delay) is NA,",
  "  then groups the rows by increasing date,",
  "  then summarizes the mean of the departure delay and the total count for each mean,",
  "  then filters out (removes) groups with 10 or less.",
  sep="\n")


#8
day_delay <- day_delay %>% 
  arrange(delay) %>%
  mutate(delay.lag = delay - lag(delay)) %>%
  arrange(-delay.lag)

day_delay %>% head(5) %>% print

#9
dest_delay <- flights %>%
  group_by(dest) %>%
  summarize(
    arr_delay.mean = mean(arr_delay, na.rm = TRUE),
    arr_count = n()
  )
dest_delay %>% head(10) %>% print

airports <- airports %>% 
  select(dest = iata, name = airport, city, state, lat, long)

df.9a <- dest_delay %>% left_join(airports, by = "dest")

df.9a %>% arrange(-arr_delay.mean) %>% head(5) %>% print

df.9b <- dest_delay %>% inner_join(airports, by = "dest")


nrow(df.9a) %>% print
nrow(df.9b) %>% print
print("No, the number of observations do not match.")
df.9c <- dest_delay %>% right_join(airports, by = "dest")
nrow(df.9c) %>% print
df.9c$arr_delay.mean %>% is.na %>% sum %>% print
cat("Yes, NAs do appear in the average arr_delay column in df.9c.",
    "  They appear because the airports dataframe has airports",
    "  that are not destinations in the flights dataframe and,",
    "  therfore, do not have matching average arr_delay values.",
    "  This is happening because the right_join on airports",
    "  includes ALL of the airports, not just the destination",
    "  airports in the flights dataframe.",
    sep = "\n")

df.9d <- dest_delay %>% full_join(airports, by = "dest")
nrow(df.9d) %>% print

cat("Yes, NAs do appear in the average arr_delay column",
    "  They appear for the same reason as with the right join:",
    "  because the destination airports do not match",
    "  destinations in dest_delay and so there is no average arr_delay",
    "  to match up with.", sep = "\n")

#10
hourly_delay <- flights %>% 
  filter(!is.na(dep_delay)) %>%
  group_by(
    date, 
    hour
  ) %>%
  summarize(
    delay = mean(dep_delay),
    n = n()
  ) %>%
  filter(n > 10)

hourly_delay %>% head(10) %>% print

hourly_delay_weather <- hourly_delay %>% left_join(weather, by = c("date", "hour"))

hourly_delay_weather %>% 
  group_by(conditions) %>%
  summarise(
    delay_max = max(delay, na.rm = TRUE),
    delay_mean = mean(delay, na.rm = TRUE)
  ) %>%
  arrange(-delay_max) %>%
  head(15) %>% print

#11a
df1 <- data.frame(treatment = c("a", "b"), subject1 = c(3, 4), subject2 = c(5, 6))
df1 %>% print

df1.tidy <- df1 %>% 
  gather(subjectx, value, -treatment) %>%
  mutate(subject = ifelse(subjectx == "subject1", 1, 2)) %>%
  select(subject, treatment, value) %>%
  arrange(subject, treatment)
df1.tidy %>% print

#11b
df2 <- data.frame(
  subject = c(1,1,2,2),
  treatment = c("a","b","a","b"),
  value = c(3,4,5,6)
)
df2 %>% print

df2.tidy <- df2 %>%
  spread(subject, value) %>%
  rename(subject1 = `1`, subject2 = `2`)
df2.tidy %>% print

#11c
df3 <- data.frame(
  subject = c(1,2,3,4),
  demo = c("f_15_CA","f_50_NY","m_45_HI","m_18_DC"),
  value = c(3,4,5,6)
)
df3 %>% print

df3.tidy <- df3 %>%
  separate(demo, c("sex", "age", "state"), sep = "_")
df3.tidy %>% print

#11d
df4 <- data.frame(
  subject = c(1,2,3,4),
  sex = c("f","f","m",NA),
  age = c(11,55,65,NA),
  city = c("DC","NY","WA",NA),
  value = c(3,4,5,6)
)
df4 %>% print

df4.tidy <- df4 %>%
  unite(demo, sex, age, city, sep = ".")
df4.tidy$demo <- replace(df4.tidy$demo, 4, NA)
df4.tidy %>% print