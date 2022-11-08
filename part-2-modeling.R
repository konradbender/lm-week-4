setwd("/Users/konrad/code/school/MT/practicals/lm-week-4")
library(MASS)

d <- read.csv("data/swim.csv", header = TRUE, sep = ",")
d <- data.frame(d)

process_dataframe <- function(df, relevel=TRUE) {
  if ("event" %in% colnames(df)){
    d <- subset(df, select = -event)
  } else {
    d <- df
  }
  for (col in colnames(d)) {
    if (col %in% c("time", "dist")) {
      next
    }
    if (col == "stroke") {
      d[, "stroke"] <- as.factor(d[, "stroke"])
      if (relevel){
        d <- within(d, stroke <- relevel(stroke, ref = "Breaststroke"))
      }
    }
    d[, col] <- as.factor(d[, col])
  }

  # now let's think about adding the term with the number of turns
  d[, "n.splits"] <- (d[, "dist"] / 25)
  d[d$course == "Long", "n.splits"] <- d[d$course == "Long", "n.splits"] / 2
  d[, "n.turns"] <- floor(d[, "n.splits"]) - 1
  return(d)
}

d <- process_dataframe(d)

mod.1 <- lm(time ~ dist * stroke + dist * sex -
  stroke -
  sex -
  stroke * sex + n.turns, data = d)

print("1. P Values for Baseline Model:")
print(data.frame(summary(mod.1)[["coefficients"]][, 4]))
# this makes sense - the model says that with more turns, swimmers are faster - which makes sense
# because you push off the wall. Then you get inertia. I asked a swimmer and she confirmed this fact.

# this is a good model next step: Show that adding more features is not relevant.
# For example, direct effect of the models
# Adding direct effect of the other variables in addition to just distance
# adding cross effect of stroks and sex (e.g. men lose more speed by changing the stroke they
# are using)

models <- list(
  mod1 = mod.1,
  mod2 = lm(time ~ (dist + stroke + sex + n.turns), data = d),
  mod3 = lm(time ~ (dist + stroke + sex + n.turns)^2, data = d),
  mod4 = lm(time ~ (dist + stroke + sex + n.turns)^3, data = d),
  mod5 = lm(time ~ (dist + stroke + sex + n.turns)^4, data = d)
)

# let's compute AIC for those models
# we choose the model that minimizes the AIC
for (mod in 1:length(models)) {
  print(paste("AIC for model", mod, ":"))
  print(extractAIC(models[[mod]])[2])
}

# let's stick with model 6! It has an AIC of  523.0823
mod.2 <- models[["mod3"]]
mod.3 <- models[["mod4"]]
mod.4 <- models[["mod5"]]
mod.5 <- update(mod.3, .~. - dist:sex:n.turns - stroke:sex:n.turns )

## Model analysis and improvement
plot.residuals <- function(m, path = "plots/model_analysis.png") {
  png(path, width = 10, height = 5, units = "in", res = 600)
  par(mfcol = c(1,3))
  plot(m, which = 1:3)
  dev.off()
}
plot.residuals(mod.5)

# We saw that short races are, on average, worse predicted than long ones. So let's try box cox
png("plots/box_cox_analysis.png", width = 10, height = 4, res = 600, units = "in")
par()
p <- boxcox(mod.5, lambda = seq(-2, 2, length = 10))
print(p)
dev.off()
# The likelihood is pretty much maximized at lambda = 1 so let's leave it there.

# What if we added distance^2 as extra variable?
mod.6 <- update(mod.5, .~.  + I(dist^2)*stroke, evaluate = TRUE)
plot.residuals(mod.6, "plots/residuals_with_squared.png")

# without the new term, we have AIC = 523, RSE = 1.746.

# with the new term, we have RSE = 1.648 and AIC = 472
# With dist^2 * stroke, we have RSE = 1.576 and AIC = 436
# Taking exp instead of ^2 we have RSE = 1.621 and AIC = 460

# Also, the p value of the x^2 term is very small, so it is relevant!

mod.7 <- update(mod.6, weights = 1/d[, "time"])

# plot(mod.5, main = "with weights", which = 1:6)

mod.8 <- update(mod.6, weights = 1/(d$time*d$time))

plot.residuals(mod.8, "plots/residuals_model_8.png")

## OUTLIER ANALYSIS

n <- dim(d)[1]
p <- length(mod.8$coefficients)
cook <- 8/(n-2*p)
dev.new()
par(mfcol=c(1,1))
png("plots/cooks_distance_for_all.png", width = 10, height = 6, units = "in", res = 600)
plot(mod.8, which = 4, sub.caption = "")
abline(h=cook, col="red")
dev.off()
# From the plot we see we need to remove observations 280, 376, 424.

delete <- cooks.distance(mod.8) > cook
d.clean <- d[ -delete[delete], ]
mod.8.clean <- update(mod.8, data = d.clean, weights = 1/(d.clean$time*d.clean$time))

## Predictions - keep in mind they are weighted.
new.d <- data.frame(
  name = c("RaceA", "RaceB", "RaceC", "RaceD"),
  dist = c(400, 50, 400, 100),
  stroke = c("Freestyle", "Backstroke", "Butterfly", "Medley"),
  sex = c("F", "F", "F", "F"),
  course = c("Long", "Long", "Long", "Long")
)

new.d <- process_dataframe(new.d, relevel = FALSE)

preds <- predict(mod.8.clean, newdata = new.d, weights = 1/(new.d$time*new.d$time))
preds.no.weights <- predict(mod.8.clean, newdata = new.d)

write.csv(preds, "preds.csv")


models <- list(mod.1, mod.2, mod.3, mod.4, mod.5, mod.6, mod.7, mod.8)
for (m in 1:length(models) ){
  aic <- extractAIC(models[[m]])[2]
  mse <- 1/n * sum(summary(models[[m]])$residuals^2)
  print(paste(m, "AIC:", aic, "MSE;", mse))
}