
library("ranger")
library("tidyr")
library("dplyr")
library("ggplot2")

library(modeldata)
data(credit_data)
credit_data <- credit_data[complete.cases(credit_data), ]

x = credit_data[, setdiff(colnames(credit_data), "Status")]
y = credit_data$Status

xy <- cbind(.yy = y, x)
xy <- as.data.frame(xy)

num.trees.vals <- c(100, 200, 300, 400, 500, 600, 700, 800, 900, 1000)
mtry.vals      <- c(2, 3, 4, 5, 6, 7, 10)
min.node.size  <- c(1, 5, 10, 15)

TUNE_N <- 20

tune_grid <- data.frame(
  num.trees = sample(rep(num.trees.vals, length.out = TUNE_N), size = TUNE_N),
  mtry      = sample(rep(mtry.vals, length.out = TUNE_N), size = TUNE_N),
  min.node.size = sample(rep(min.node.size, length.out = TUNE_N), size = TUNE_N),
  cost = NA_real_
)

for (i in seq_len(nrow(tune_grid))) {
  mdl_i <- ranger::ranger(.yy ~ ., data = xy, probability = TRUE,
                          num.trees = tune_grid[i, "num.trees"])
  tune_grid$cost[i] <- mdl_i$prediction.error
}

plot(tune_grid$num.trees, tune_grid$cost)
plot(tune_grid$mtry, tune_grid$cost)
plot(tune_grid$min.node.size, tune_grid$cost)

# How much does OOB error vary by iteration?
# It makes no sense to pick min value when this could be error

library(modeldata)
data(credit_data)
credit_data <- credit_data[complete.cases(credit_data), ]

x = credit_data[, setdiff(colnames(credit_data), "Status")]
y = credit_data$Status

xy <- cbind(.yy = y, x)
xy <- as.data.frame(xy)

one_grid <- function(tune_grid, xy) {
  cost <- vector("numeric", length = nrow(tune_grid))
  for (i in seq_len(nrow(tune_grid))) {
    mdl_i <- ranger::ranger(.yy ~ ., data = xy, probability = TRUE,
                            num.trees = tune_grid[i, "num.trees"])
    cost[i] <- mdl_i$prediction.error
  }
  cost
}

hp_cost <- replicate(50, one_grid(tune_grid, xy))

tune_grid$mean <- apply(hp_cost, 1, mean)
tune_grid$std.error <- apply(hp_cost, 1, function(x) sqrt(var(x)/length(x)))

ggplot(tune_grid,
       aes(x = num.trees, group = interaction(num.trees, mtry, min.node.size))) +
  geom_point(aes(y = mean),
             position = position_dodge(width = 20)) +
  geom_errorbar(aes(ymin = mean - std.error, ymax = mean + std.error),
                position = position_dodge(width = 20),
                width = 20)



