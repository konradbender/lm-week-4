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
