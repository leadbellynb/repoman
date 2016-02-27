
print("Nina")
print("Bucar")
print("1188635")

library(ggplot2)
library(foreign)
library(RCurl)
library(dplyr)

# 1

data(diamonds)

plot1 <- ggplot(diamonds) + 
  aes(x = x * y * z, y = price, color = clarity, size = carat) + 
  geom_point(alpha = 0.22) +
  scale_x_log10() + 
  scale_y_log10() +
  scale_size(range = c(3, 16)) +
  theme(
    axis.title = element_text(size = 28),
    axis.text = element_text(size = 22),
    legend.title = element_text(size = 24),
    legend.text = element_text(size = 20),
    legend.key.size = unit(1.3, "cm")) + 
  guides(colour = guide_legend(override.aes = list(size=5, alpha=1)))

print(plot1)


# 2

plot2 <- ggplot(diamonds) + 
  aes(x = carat, fill = clarity) + 
  geom_histogram(
    aes(y = ..density..), binwidth = .2) + 
  facet_grid(cut ~ .) +
  xlab("Carat") +
  ylab("Density") +
  theme(
    axis.title = element_text(size = 26),
    axis.text = element_text(size = 20),
    legend.title = element_text(size = 24),
    legend.text = element_text(size = 18),
    legend.key.size = unit(1.3, "cm"),
    strip.text.y = element_text(size = 20)) + 
  guides(fill = guide_legend(title = "Clarity"))

print(plot2)


# 3

plot3 <- ggplot(diamonds) + 
  aes(x = cut, y = price) + 
  geom_violin(size = 1.25) + 
  geom_jitter(alpha = 0.0075, size = 5) +
  theme(
    axis.title = element_text(size = 26),
    axis.text = element_text(size = 20))

print(plot3)


# 4

load(url("https://github.com/EconomiCurtis/econ294_2015/raw/master/data/org_example.RData"))



oe <- org_example %>%
  filter(!is.na(rw)) %>%
  mutate(
    date = paste(year, month, "01", sep = "-"),
    date = as.Date(date, format = "%Y-%m-%d")
  ) %>%
  group_by(date) %>%
  summarize(
    rw.median = median(rw),
    rw.q1 = quantile(rw, probs = .25),
    rw.q3 = quantile(rw, probs = .75),
    rw.d1 = quantile(rw, probs = .1),
    rw.d9 = quantile(rw, probs = .9)
  ) 

plot4 <- ggplot(oe) +
  aes(x = date, y = rw.median) + 
  geom_line(size = 1.2) +
  ylim(0, 50) +
  ylab("Median.RW") +
  theme(
    axis.title = element_text(size = 26),
    axis.text = element_text(size = 22)) +
  geom_ribbon(aes(x = date, ymin = rw.d1, ymax = rw.d9), alpha = .2) +
  geom_ribbon(aes(x = date, ymin = rw.q1, ymax = rw.q3), alpha = .4)

print(plot4)


# 5

oe.educ <- org_example %>%
  filter(!is.na(rw)) %>%
  mutate(
    date = paste(year, month, "01", sep = "-"),
    date = as.Date(date, format = "%Y-%m-%d")
  ) %>%
  group_by(date, educ) %>%
  summarize(
    rw.median = median(rw)
  ) 

plot5 <- ggplot(oe.educ) +
  aes(x = date, y = rw.median, color = educ, group = educ,
    scale_y_continuous(breaks = c(5, 35, 5))) +
  geom_line(size = 1.2) +
  ylab("Median.RW") +
  theme(
    axis.title = element_text(size = 28),
    axis.text = element_text(size = 24),
    legend.title = element_text(size = 24),
    legend.text = element_text(size = 20),
    legend.key.size = unit(1.3, "cm"))

print(plot5)

