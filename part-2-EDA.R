setwd("/Users/konrad/code/school/MT/practicals/lm-week-4")

d <- read.csv("data/swim.csv", header = TRUE, sep = ",")
d <- data.frame(d)

d <- subset(d, select=-event)
for (col in colnames(d)) {
  if (col == "time" | col == "dist"){
    next
  }
  d[, col] <- as.factor(d[, col])
}

d[, "avg.time"] <- d[, "time"] / d[, "dist"]

### PLOTTING ###

# make scatter plot distance vs. time for all categories
# make boxplot plots for

# First assumption: The Stroke and Distance has an influence on the average pace

for (sex in c("M", "F")){
  png(file = paste0("plots/boxplots_", sex, ".png"), width = 10, height = 7.5, units =  "in", res = 300)
  par(mfcol=c(1,2))
  for (col in c("stroke", "dist")){
    data <- d[d$sex == sex, c(col, "avg.time")]
    boxplot(avg.time ~ . , data = data)
  }
  title(paste("Boxplots of data for sex = ", sex), line = -2, outer = TRUE)
  dev.off()
}

# So, that is a good start. There is obviously some relationship between the distance,
# and the stroke and the pace. Notably, the mean and the variance are different.

# Next, let's see how the number of turns people are doing affects the pace
for (sex in c("M", "F")){
png(file = paste0("plots/n_of_turns_", sex, ".png"), width = 10, height = 7.5, units =  "in", res = 300)
par(mfcol=c(1,4))
for (dist in c(50, 100, 200, 400)){
  data <- d[(d$sex == sex) & (d$stroke == "Freestyle") & (d$dist == dist), c("avg.time", "course")]
  boxplot(avg.time ~ . , data = data)
}
title(paste("Boxplots of data for sex = ", sex), line = -2, outer = TRUE)
dev.off()
}
# Nice, we also see that the number of turns they do has an effect on the time.
# And the influence it has seems to depend on the distance as well.

# How _exactly_ is the data distributed? Let's plot it for two groups
for (sex in c("M", "F")){
  png(file = paste0("plots/histogram_", sex, ".png"), width = 7.5, height = 7.5, units =  "in", res = 300)
  par(mfcol=c(2,2))
  stroke <- "Freestyle"
  dist <- "Long"
  for (dist in c(50, 100, 200, 400)){
    data <- d[(d$sex == sex) &
                (d$stroke == stroke) &
                 (d$dist == dist), c("time", "course")]
    hist(data[, "time"], breaks = 10)
  }
  title(paste("Histograms of Freestyle long; sex = ", sex), line = -2, outer = TRUE)
  dev.off()

  png(file = paste0("plots/qq_", sex, ".png"), width = 7.5, height = 7.5, units =  "in", res = 300)
  for (dist in c(50, 100, 200, 400)){
    data <- d[(d$sex == sex) &
                (d$stroke == stroke) &
                (d$dist == dist), c("time", "course")]
    qqnorm(data[, "time"], pch = 1, frame = FALSE)
  }
  title(paste("QQ Plot of Freestyle long; sex = ", sex), line = -2, outer = TRUE)
  dev.off()
}

#  So we see that the times are somewhat normal distributed

### DESCRIPTIVE SUMMARY ###
means <- aggregate(time ~ stroke + dist + sex + course,
                   data = d,
                   FUN = mean)
colnames(means)[colnames(means) == 'time'] <- 'time.mean'

vars <- aggregate(time ~ stroke + dist + sex + course,
                   data = d,
                   FUN = var)

colnames(vars)[colnames(vars) == 'time'] <- 'time.var'

stats <- merge(means, vars, by=c("stroke", "dist", "sex", "course"))






