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


# male, female
# 50, 100, 200, 400
# freestyle, back, breast, butterfly, medley
# long, short

boxplot(time ~ . , data = d)