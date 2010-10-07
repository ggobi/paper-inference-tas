set.seed(141079)
library(nullabor)
library(ggplot2)
library(plyr)
library(lubridate)
library(MASS)

# Data from http://www.basketballgeek.com/data/
load("lal.rdata")

scoring <- subset(lal, (!is.na(num) | !is.na(points)) & result == "made")
scoring <- scoring[c(".id", "period", "time", "etype", "num", "outof",
 "points", "possession", "reason")]

# Is the distribution of points Poisson? -------------------------------------
scores <- ddply(scoring, ".id", summarise, 
  points = sum(points, na.rm = T) + sum(etype == "free throw"))
qplot(points, ..density.., data = scores, binwidth = 2, geom = "histogram") %+%
  lineup(distribution("points", "poisson"), n = 10) +
  facet_wrap(~ .sample, ncol = 5) + 
  ylab("Proportion")
ggsave("poisson.pdf", width = 8, height = 3)

# Compare to an overlaid density
lambda <- fitdistr(scores$points, "poisson")$estimate[1]
grid <- summarise(scores, points = seq(min(points), max(points), by = 1))
grid$density <- dpois(grid$points, lambda)

ggplot(mapping = aes(x = points)) + 
  geom_histogram(aes(y = ..density..), data = scores, binwidth = 2) + 
  geom_pointrange(data = grid, aes(ymin = 0, y = density, ymax = density),
    colour = "red")
ggsave("poisson-overlay.pdf", width = 8, height = 3)

# Is the distribution of durations exponential? ------------------------------

# Calculate duration between scoring events
scoring <- ddply(scoring, c(".id", "period"), transform, 
  dur = c(NA, as.duration(diff(rev(time)))))
scoring <- subset(scoring, !is.na(dur))

qplot(dur, ..density.., data = scoring, binwidth = 5, geom = "histogram") %+%
  lineup(distribution("dur", "exponential"), n = 10) + 
  scale_y_continuous("Proportion") +
  facet_wrap(~ .sample, ncol = 5)
ggsave("exponential.pdf", width = 8, height = 3)

# What if we removed free throws? 

fg <- subset(lal, !is.na(points))
fg <- ddply(fg, c(".id", "period"), transform, 
  dur = c(NA, as.duration(diff(rev(time)))))
fg <- subset(fg, !is.na(dur))

qplot(dur, ..density.., data = fg, binwidth = 5, geom = "histogram") %+%
  lineup(distribution("dur", "gamma"), n = 10) +
  facet_wrap(~ .sample, ncol = 5) + 
  ylab("Proportion")
ggsave("gamma.pdf", width = 8, height = 3)

fitdistr(fg$dur, "gamma")$estimate
