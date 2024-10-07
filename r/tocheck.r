
library(faircause)
library(ggplot2)

data <- get(data("gov_census", package = "faircause"))
data <- as.data.frame(data[seq_len(20000), ])
knitr::kable(head(data), caption = "Census dataset.")

mean_sal <- tapply(data$salary, data$sex, mean)
tv <- mean_sal[2] - mean_sal[1]


set.seed(2022)
mdata <- SFM_proj("census")

fc_census <- fairness_cookbook(
  as.data.frame(data), X = mdata$X, Z = mdata$Z, W = mdata$W,
  Y = mdata$Y, x0 = mdata$x0, x1 = mdata$x1
)

autoplot(fc_census, decompose = "xspec", dataset = "Census")

fc_census$measures
# print the unique values in the measure column of the fairness cookbook object
unique(fc_census$measures$measure)
# take only the measures  with measure = "nie"
nie <- fc_census$measures[fc_census$measures$measure == "nie", ]
# show mean and standard deviation of the NIE values
mean(nie$value)
sd(nie$value)

