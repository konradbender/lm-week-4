# Produce 3 Plots
# 1. Influence that gender has on times
# 2. Men only: Influece that stroke and distance has on times (colorful plot(
# 3. Men only: For some (stroke, distance) pairs, show what influence the number of turns has.

library(dplyr)
library(ggplot2)
library(reshape2)
setwd("/Users/konrad/code/school/MT/practicals/lm-week-4")

d <- read.csv("data/swim.csv", header = TRUE, sep = ",")
d <- data.frame(d)

d <- subset(d, select = -event)
for (col in colnames(d)) {
  if (col == "time" | col == "dist") {
    next
  }
  d[, col] <- as.factor(d[, col])
}

d[, "avg.time"] <- d[, "time"] / d[, "dist"]

### PLOTTING ###

# make scatter plot distance vs. time for all categories
# make boxplot plots for

# First assumption: The Stroke and Distance has an influence on the average pace

for (sex in c("M", "F")) {
  # png(file = paste0("plots/boxplots_", sex, ".png"), width = 20, height = 7.5, units = "in", res = 300)
  par(mfcol = c(1, 2))
  for (col in c("stroke", "dist")) {
    data <- d[d$sex == sex, c(col, "avg.time")]
    boxplot(avg.time ~ ., data = data)
  }
  title(paste("Boxplots of data for sex = ", sex), line = -2, outer = TRUE)
  # dev.off()
}

# So, that is a good start. There is obviously some relationship between the distance,
# and the stroke and the pace. Notably, the mean and the variance are different.



# How _exactly_ is the data distributed? Let's plot it for two groups
for (sex in c("M", "F")) {
  # png(file = paste0("plots/histogram_", sex, ".png"), width = 7.5, height = 7.5, units = "in", res = 300)
  par(mfcol = c(2, 2))
  stroke <- "Freestyle"
  dist <- "Long"
  for (dist in c(50, 100, 200, 400)) {
    data <- d[(d$sex == sex) &
                (d$stroke == stroke) &
                (d$dist == dist), c("time", "course")]
    hist(data[, "time"], breaks = 10)
  }
  title(paste("Histograms of Freestyle long; sex = ", sex), line = -2, outer = TRUE)
  # dev.off()

  # png(file = paste0("plots/qq_", sex, ".png"), width = 7.5, height = 7.5, units = "in", res = 300)
  for (dist in c(50, 100, 200, 400)) {
    data <- d[(d$sex == sex) &
                (d$stroke == stroke) &
                (d$dist == dist), c("time", "course")]
    qqnorm(data[, "time"], pch = 1, frame = FALSE)
  }
  title(paste("QQ Plot of Freestyle long; sex = ", sex), line = -2, outer = TRUE)
  # dev.off()
}

#  So we see that the times are somewhat normal distributed

### DESCRIPTIVE SUMMARY ###
d.male <- d[d$sex == "M", ]
means <- aggregate(time ~ stroke + dist + sex + course,
                   data = d.male,
                   FUN = mean)
colnames(means)[colnames(means) == 'time'] <- 'time.mean'

vars <- aggregate(time ~ stroke + dist + sex + course,
                  data = d.male,
                  FUN = var)

colnames(vars)[colnames(vars) == 'time'] <- 'time.var'

skews <- aggregate(time ~ stroke + dist + sex + course,
                  data = d.male,
                  FUN = skewness)

colnames(skews)[colnames(vars) == 'time'] <- 'time.skew'


stats <- merge(means, vars, by = c("stroke", "dist", "sex", "course"))
stats <- merge(stats, skews, by = c("stroke", "dist", "sex", "course"))

write.csv(stats, "descriptive_stats.csv", row.names = FALSE)

by_group <- d %>% group_by(stroke, sex)
fun <- function(x) {
  x <- data.frame(x)
  plot(x[, "dist"], x[, "time"], xlab = "Distance [m]", ylab = "Time [s]",
       main = paste("Scatterplot for",
                    x[1, "stroke"],
                    x[1, "sex"]))

  return(x)
}
# by_group %>% do(fun(.))

# First Plot: The effect of the gender on the times
data <- d[d$dist == 200, ]
data[, "stroke"] <- factor(data[, "stroke"], levels = c("Freestyle", "Butterfly",
                                                        "Backstroke", "Medley", "Breaststroke"))
data[, "sex"] <- factor(data[, "sex"], levels=c("M", "F"))
p <- ggplot(data, aes(sex, avg.time), ylab("Average time [s]")) +
  geom_boxplot() +
  facet_grid(~ stroke, scales = "free", space = "free") +
  theme(text = element_text(size=12), legend.position = "top", legend.title=element_blank())  +
  labs(y = "Average time [s]", x="Gender of Athlete", title = "Average times for different genders and stroke (200 m only)")
print(p)
ggsave("plots/boxplot_gender_effect.pdf", device = "pdf")
# So, we see that men are in fact faster than women in all strokes.

# Let's make one nice plot for each gender that summarizes all data
male <- d[d$sex == "M", ]
male[, "dist"] <- factor(paste(male[, "dist"], "m"), levels = c("50 m", "100 m", "200 m", "400 m"))
male[, "stroke"] <- factor(male[, "stroke"], levels = c("Freestyle", "Butterfly",
                                                           "Backstroke", "Medley", "Breaststroke"))

male[, "dummy"] <- 1
p <- ggplot(male, aes(stroke, avg.time), ylab("Average time [s]")) +
   geom_boxplot(aes(fill=stroke)) +
   facet_wrap(~ dist) +
   theme(text = element_text(size=12), legend.position = "top", legend.title=element_blank())  +
   labs(y = "Average time [s]", x="Stroke", title = "Average times for different distances and strokes (men only)")
print(p)
ggsave("plots/overall_boxplot_M.pdf", device = "pdf")

# Next, let's see how the number of turns people are doing affects the pace
# png(file = paste0("plots/n_of_turns_", sex, ".png"), width = 10, height = 7.5, units = "in", res = 300)
data <- d[d$sex == "M" & d$stroke == "Freestyle", ]
data[, "course"] <- factor(data$course, levels = c("Short", "Long"))
p <- ggplot(data, aes(course, avg.time), ylab("Average time [s]")) +
  geom_boxplot() +
  facet_grid(~ dist, scales = "free", space = "free") +
  theme(text = element_text(size=12), legend.position = "top", legend.title=element_blank())  +
  labs(y = "Average time [s]", x="Course Type", title = "Average times for different course lengths and distances (men's Freestyle only)")
print(p)
ggsave("plots/boxplot_freestyle_n_turns_men.pdf", device = "pdf")
# Nice, we also see that the number of turns they do has an effect on the time.
# And the influence it has seems to depend on the distance as well!