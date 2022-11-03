## one of the introductory examples
plot(dist ~ speed, data = cars)
cars0.lm <- lm(dist ~ speed, data = cars)
cars1.lm <- lm(dist ~ speed + I(speed^2), data = cars)
cars2.lm <- lm(dist ~ speed + speed^2, data = cars)
summary(cars0.lm)
summary(cars1.lm)
summary(cars2.lm)
## cars2.lm is the same as cars0.lm and is probably not what was intended

## the boxcox() function is in the MASS package
library(MASS)
trees0.lm <- lm(Volume ~ log(Height) + log(Girth), data = trees)
par(mfrow = c(2, 2))
plot(trees0.lm)
## see ?boxcox
boxcox(Volume ~ log(Height) + log(Girth), data = trees,
       lambda = seq(-0.25, 0.25, length = 10))
abline(v = 0, col = "red")
trees1.lm <- lm(log(Volume) ~ log(Height) + log(Girth), data = trees)
par(mfrow = c(2, 2))
plot(trees1.lm)
# Need to reverse the transformation!! So here, y_hat = exp(y_out)

lmod <- lm(sr ~ pop15 + pop75 + dpi + ddpi, data = LifeCycleSavings)
boxcox(lmod)
boxcox(lmod, lambda = seq(0.5, 1.5, by = 0.1))
## Faraway: "no good reason to transform"
## need the faraway package for the gala data
library(faraway)
lmod <- lm(Species ~ Area + Elevation + Nearest + Scruz + Adjacent, data = gala)
boxcox(lmod, lambda = seq(-0.25, 0.75, by = 0.05))
## Faraway:
## "... perhaps a cube root transformation might be best here.
## A square root is also a possibility ...
## Certainly there is a strong need to transform."
