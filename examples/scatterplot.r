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
qplot(angle, r, data = threept)

qplot(angle * 180 / pi, r, data = threept, alpha = I(1/3)) +
  scale_x_continuous("Angle (degrees)",  breaks = c(0, 45, 90, 135, 180),
    limits = c(0, 180))

last_plot() %+% 
  lineup(null_model(r ~ poly(angle, 2)), threept, n = 10, pos = 5) + 
  facet_wrap(~ .sample, ncol = 5)

# Doesn't seem to be any relationship between success and radius or distance
# that I can find.

# Jump: 2114 points ---------------------------------------------------------
jump <- subset(top, type == "jump")

qplot(x, y, data = jump) + coord_equal()
qplot(x, y, data = jump, geom = "jitter") + coord_equal()

ggplot(jump, aes(x, y)) + 
  geom_jitter(colour = "grey70") + 
  geom_density2d()  + 
  coord_equal()
ggsave("13-jump-jitter-density.pdf", width = 8, height = 6)
qplot(x, y, data = jump, geom = "density2d") + coord_equal()
ggsave("13-jump-density.pdf", width = 8, height = 6)

qplot(x, y, data = jump, geom = "density2d") + 
  facet_wrap(~ result) + 
  coord_equal()
ggsave("13-jump-density-result.pdf", width = 8, height = 6)

jump$x5 <- cut_interval(jump$x, length = 10)
jump$y5 <- cut_interval(jump$y, length = 5)
qplot(x, data = jump, binwidth = 1) + facet_wrap(~ y5)
ggsave("13-jump-x-given-y.pdf", width = 8, height = 6)

qplot(x, y, data = jump, geom = "density2d") + coord_equal() +
  geom_vline(xintercept = 0:5 * 10) + xlim(0, 50) + ylim(0, 30)
ggsave("13-jump-x-given-y2.pdf", width = 8, height = 6)
qplot(x, y, data = jump, geom = "density2d") + coord_equal() +
  geom_hline(yintercept = 0:6 * 5) + xlim(0, 50) + ylim(0, 30)
ggsave("13-jump-y-given-x2.pdf", width = 8, height = 6)


qplot(y, data = jump, binwidth = 1) + facet_wrap(~ x5)
ggsave("13-jump-y-given-x.pdf", width = 8, height = 6)

jump$dist <- with(jump, sqrt((x - 25)^2 + y^2))
qplot(dist, data = jump, binwidth = 1)

