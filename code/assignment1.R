pdata = read.csv("code/performance_data.csv")
attach(pdata)

f <- function(x) {
  return(as.numeric(substr(x, 3, nchar(x))))
}
f2 <- function(x) {
  return(as.numeric(x))
}

data_nostrategy <- subset(pdata, !grepl("A,", indivA) & !grepl("B,", indivB))
data_nostrategy[c("indivA2")] <- lapply(data_nostrategy[c("indivA")], f2)
data_nostrategy[c("indivB2")] <- lapply(data_nostrategy[c("indivB")], f2)

data_strategyA <- subset(pdata, grepl("A,", indivA))
data_strategyA[c("indivA2")] <- lapply(data_strategyA[c("indivA")], f)
# data_strategyA[c("indivB2")] <- lapply(data_strategyA[c("indivB")], f2)

data_strategyB <- subset(pdata, grepl("B,", indivB))
data_strategyB[c("indivB2")] <- lapply(data_strategyB[c("indivB")], f)
# data_strategyB[c("indivA2")] <- lapply(data_strategyB[c("indivA")], f2)

# average performance for strategy A
print(mean(data_strategyA$indivA2)) # -0.4576037
# average performance no strategy for indivA
print(mean(data_nostrategy$indivA2)) # -0.05852352
# Diff StrategyA - No Strategy: -0.3990802 => Decrease

# average performance for strategy B
print(mean(data_strategyB$indivB2)) # 0.3834611
# average performance no strategy for indivB
print(mean(data_nostrategy$indivB2)) # 0.02506828
# Diff StrategyB - No Strategy: 0.3583928 => Improvement





### Ignore erroneous approach below!!! ###

# data_strategyAB <- subset(pdata, grepl("A,", indivA) & grepl("B,", indivB))
# data_strategyAB[c("indivA2", "indivB2")] <- lapply(data_strategyAB[c("indivA", "indivB")], f)
# 
# data_strategyA$diffA = data_strategyA$indivA2 - data_strategyA$indiv0
# # average improvement for strategy A vs status quo
# print(mean(data_strategyA$diffA))
# # -0.4344436
# # percentage or improvement
# print(sum(data_strategyA$diffA > 0)/length(data_strategyA$diffA))
# # 0.3469388 - 35%
# 
# data_strategyB$diffB = data_strategyB$indivB2 - data_strategyB$indiv0
# # average improvement for strategy B vs status quo
# print(mean(data_strategyB$diffB))
# # 0.3222293
# # percentage or improvement
# print(sum(data_strategyB$diffB > 0)/length(data_strategyB$diffB))
# # 0.5714286 - 57%

