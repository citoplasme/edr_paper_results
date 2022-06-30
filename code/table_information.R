load("/Users/joaopimentel/Documents/GitHub/edr_paper_results/data/datasets/base/datasets_with_preds_discretized.RData")
library(ggplot2)
library(e1071)

load("/Users/joaopimentel/Documents/GitHub/edr_paper_results/data/datasets/masters/dataset_with_preds_discretized.RData")
summary(df2$Chance.of.Admit)
mean(df2$Chance.of.Admit)
sd(df2$Chance.of.Admit)
kurtosis(df2$Chance.of.Admit)
skewness(df2$Chance.of.Admit)

vector <- datasets_with_preds_discretized$boston$HousValue
kurtosis(vector)
skewness(vector)

values <- c()
for (name in names(datasets_with_preds_discretized)) {
  vector <- datasets_with_preds_discretized[[name]][[tail(names(datasets_with_preds_discretized[[name]]),1)]]
  values <- c(
    values,
    paste0(
      name, " ",
      tail(names(datasets_with_preds_discretized[[name]]),1), " ",
      format(round(mean(vector), 3), nsmall = 3), " & ",
      format(round(sd(vector), 3), nsmall = 3), " & ",
      format(round(kurtosis(vector), 3), nsmall = 3), " & ",
      format(round(skewness(vector), 3), nsmall = 3))
    )
}
sort(values)

for (name in sort(names(datasets_with_preds_discretized))) {
  vector <- datasets_with_preds_discretized[[name]][[tail(names(datasets_with_preds_discretized[[name]]),1)]]
  print(paste0(name, " ", tail(names(datasets_with_preds_discretized[[name]]),1)))
  print(summary(vector))
}
