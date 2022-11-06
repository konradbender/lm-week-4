setwd("/Users/konrad/code/school/MT/practicals/lm-week-4")

d <- read.csv("data/swim.csv", header = TRUE, sep = ",")
d <- data.frame(d)

d <- subset(d, select = -event)
for (col in colnames(d)) {
  if (col %in% c("time", "dist")) {
    next
  }
  if (col == "stroke") {
    d[, "stroke"] <- as.factor(d[, "stroke"])
    d <- within(d, stroke <- relevel(stroke, ref = "Breaststroke"))
  }
  d[, col] <- as.factor(d[, col])
}

# let's not do the type of course yet
mod1.simple <- lm(time ~ dist, data = d)
mod1.complex <- lm(time ~ dist + dist * stroke + dist * sex + dist * course -
  stroke -
  sex -
  course, data = d)
mod1 <- lm(time ~ dist * stroke + dist * sex -
  stroke -
  sex -
  stroke * sex, data = d)


# now let's think about adding the term with the number of turns
d[, "n.splits"] <- (d[, "dist"] / 25)
d[d$course == "Long", "n.splits"] <- d[d$course == "Long", "n.splits"] / 2
d[, "n.turns"] <- d[, "n.splits"] - 1

baseline <- lm(time ~ dist * stroke + dist * sex -
  stroke -
  sex -
  stroke * sex + n.turns, data = d)
# this makes sense - the model says that with more turns, swimmers are faster - which makes sense
# because you push off the wall. Then you get inertia. I asked a swimmer and she confirmed this fact.

# this is a good model next step: Show that adding more features is not relevant.
# For example, direct effect of the models
# Adding direct effect of the other variables in addition to just distance
# adding cross effect of stroks and sex (e.g. men lose more speed by changing the stroke they
# are using)

models <- list(
  mod1 = lm(time ~ dist * stroke + dist * sex -
    stroke -
    sex -
    stroke * sex + n.turns, data = d),
  mod2 = lm(time ~ (dist + stroke + sex + n.splits), data = d),
  mod3 = lm(time ~ (dist + stroke + sex + n.splits)^2, data = d),
  mod4 = lm(time ~ (dist + stroke + sex + n.splits)^3, data = d),
  mod5 = lm(time ~ (dist + stroke + sex + n.splits)^4, data = d),
  mod6 = lm(time ~ (dist + stroke + sex + n.splits)^3
    - dist:stroke:n.splits
    - dist:sex:n.splits
    - stroke:sex:n.splits, data = d)
)

# let's compute AIC for those models
# we choose the model that minimizes the AIC
for (mod in 1:length(models)) {
  print(paste("AIC for model", mod, ":"))
  print(extractAIC(models[[mod]])[2])
}

# let's stick with model 6! It has an AIC of  523.0823
chosen.model <- models[["mod6"]]
# if we look at the anova table for the model with only second order terms, all terms are significant.
# So, removing one of them will likely not improve predictions. Thus, we _do_ need third order terms.
# then we can start looking which ones of thos are correct.

## Model analysis and improvement
pdf("plots/model_analysis.pdf")
par(mfcol = c(2,2))
plot(chosen.model)
dev.off()

# We saw that short races are, on average, worse predicted than long ones. So let's try box cox
library(MASS)
boxcox(chosen.model, lambda = seq(-2, 2, length = 10))

# The likelihood is pretty much maximized at lambda = 1 so let's leave it there.
# What if we added distance^2 as extra variable?

mod6.extra <- lm(time ~ (dist + stroke + sex + n.splits)^3
  - dist:stroke:n.splits
  - dist:sex:n.splits
  - stroke:sex:n.splits + I(dist^2)*stroke, data = d)

extractAIC(mod6.extra)

# Taking logs of the distance makes the RSE WORSE!!

# without the new term, we have AIC = 523, RSE = 1.746.

# with the new term, we have RSE = 1.648 and AIC = 472
# With dist^2 * stroke, we have RSE = 1.576 and AIC = 436
# Taking exp instead of ^2 we have RSE = 1.621 and AIC = 460

# Also, the p value of the x^2 term is very small, so it is relevant!

par(mfcol = c(3, 2))
plot(mod6.extra, which = 1:6, main = "Model 6 Extra")
plot(chosen.model, which = 1:6, main = "Model 6")

weighted_model <- lm(time ~ (dist + stroke + sex + n.splits)^3
  - dist:stroke:n.splits
  - dist:sex:n.splits
  - stroke:sex:n.splits + I(dist^2)*stroke, data = d, weights = 1/d[, "time"])

plot(weighted_model, main = "with weights", which = 1:6)

weighted_model_sq <- lm(time ~ (dist + stroke + sex + n.splits)^3
  - dist:stroke:n.splits
  - dist:sex:n.splits
  - stroke:sex:n.splits + I(dist^2)*stroke, data = d, weights = 1/(d$time*d$time))

summary(weighted_model_sq)
extractAIC(weighted_model_sq)

par(mfcol = c(1,3))
plot(mod6.extra, which = 3, main = "model 6")
plot(weighted_model, which = 3, main = "model 6 with weigths")
plot(weighted_model_sq, which = 3, main = "model 6 with squared weigths")


par(mfcol = c(3,2))
plot(weighted_model_sq, which = 1:6)


par(mfcol = c(1,3))
pdf("plots/q_plots_of_different_models.pdf")
plot(mod6.extra, which = 2, main = "model 6")
plot(weighted_model, which = 2, main = "model 6 with weigths")
plot(weighted_model_sq, which = 2, main = "model 6 with squared weigths")
dev.off()
## OUTLIER ANALYSIS

n <- dim(d)[1]
p <- length(weighted_model_sq$coefficients)
cook <- 8/(n-2*p)
dev.off()
dev.new()
par(mfcol=c(1,1))
pdf("plots/cooks_distance_for_all.pdf")
plot(weighted_model_sq, which = 4, sub.caption = "")
abline(h=cook, col="red")
dev.off()
# From the plot we see we need to remove observations 280, 376, 424.

d.clean <- d[ -c(280, 376, 424), ]
weighted_model_sq.clean <- lm(time ~ (dist + stroke + sex + n.splits)^3
  - dist:stroke:n.splits
  - dist:sex:n.splits
  - stroke:sex:n.splits + I(dist^2)*stroke, data = d.clean, weights = 1/(d.clean$time*d.clean$time))

## Predictions - keep in mind they are weighted.
new.d <- data.frame(
  name = c("RaceA", "RaceB", "RaceC", "RaceD"),
  dist = c(400, 50, 400, 100),
  stroke = c("Freestyle", "Backstroke", "Butterfly", "Medley"),
  sex = c("F", "F", "F", "F"),
  course = c("Long", "Long", "Long", "Long")
)

for (col in colnames(new.d)) {
  if (col %in% c("time", "dist")) {
    next
  }
  if (col == "stroke") {
    new.d[, "stroke"] <- as.factor(new.d[, "stroke"])
  }
  new.d[, col] <- as.factor(new.d[, col])
}

# now let's think about anew.dnew.ding the term with the number of turns
new.d[, "n.splits"] <- (new.d[, "dist"] / 25)
new.d[new.d$course == "Long", "n.splits"] <- new.d[new.d$course == "Long", "n.splits"] / 2
new.d[, "n.turns"] <- new.d[, "n.splits"] - 1

preds <- predict(weighted_model_sq.clean, newdata = new.d, weights = 1/(new.d$time*new.d$time))
preds.no.weights <- predict(weighted_model_sq.clean, newdata = new.d)

write.csv(preds, "preds.csv")