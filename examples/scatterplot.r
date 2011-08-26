set.seed(141079)
library(ggplot2)
library(nullabor)
load("lal.rdata")

pos <- subset(lal, !is.na(x) & !is.na(y))

top_types <- names(sort(-table(pos$type)))[1:6]
top <- subset(pos, type %in% top_types)
top$type <- as.character(top$type)

# 4561 plays:
#            Var1 Freq
#            jump 2114
#             3pt 1430
#           layup 1017
#   driving layup  307
#            hook  174
# turnaround jump  174
qplot(x, y, data = top, geom = "jitter", alpha = I(1/5), colour = success) + facet_wrap(~ type) + coord_equal()

# Three pointer: 1430 points ------------------------------------------------
threept <- subset(lal, type == "3pt" & !is.na(x) & !is.na(y))
threept <- threept[c(".id", "period", "time", "team", "etype", "player", "points", "result", "x", "y")]

threept <- transform(threept, 
  x = x + runif(length(x), -0.5, 0.5),
  y = y + runif(length(y), -0.5, 0.5))
threept <- transform(threept, 
  r = sqrt((x - 25) ^ 2 + y ^ 2),
  angle = atan2(y, x - 25))
threept <- subset(threept, r > 20 & r < 39)

qplot(x, y, data = threept) + coord_equal()
ggsave("x-y.pdf", width = 6, height = 4)
qplot(angle, r, data = threept)
ggsave("angle-r.pdf", width = 6, height = 4)

qplot(angle * 180 / pi, r, data = threept, alpha = I(1/3)) +
  scale_x_continuous("Angle (degrees)",  breaks = c(0, 45, 90, 135, 180),
    limits = c(0, 180))

last_plot() %+% 
  lineup(null_model(r ~ poly(angle, 2)), threept, n = 10, pos = 7) + 
  facet_wrap(~ .sample, ncol = 5)
ggsave("quadratic.pdf", width = 8, height = 3)

last_plot() %+% 
  lineup(null_model(r ~ angle * (angle < pi / 2)), threept, n = 10, pos = 3) + 
  facet_wrap(~ .sample, ncol = 5)
ggsave("broken-stick.pdf", width = 8, height = 3)


# Doesn't seem to be any relationship between success and radius or distance
# that I can find.
