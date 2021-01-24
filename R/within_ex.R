library(ez)
data <- structure(list(Sub = structure(c(3L, 3L, 3L, 4L, 4L, 4L, 1L,
                                         1L, 1L, 2L, 2L, 2L), .Label = c("A7011", "A7022", "B13", "B14"
                                         ), class = "factor"), Depvariable = c(0.375, 0.066667, 0.15,
                                                                               0.275, 0.025, 0.78333, 0.24167, 0.058333, 0.14167, 0.19167, 0.5,
                                                                               0), Group = structure(c(2L, 2L, 2L, 2L, 2L, 2L, 1L, 1L, 1L, 1L,
                                                                                                       1L, 1L), .Label = c("A", "B"), class = "factor"), WithinFactor = c(0.6,
                                                                                                                                                                          0, -0.3, 0.6, 0, -0.3, 0.6, 0, -0.3, 0.6, 0, -0.3)), .Names = c("Sub",
                                                                                                                                                                                                                                         "Depvariable", "Group", "WithinFactor"), row.names = c(NA, 12L
                                                                                                                                                                                                                                          ), class = "data.frame")
data
mod.ez <- ezANOVA(
  data,
  dv = .(Depvariable),
  wid = .(Sub),  # subject
  within = .(WithinFactor),
  between = .(Group),
  type = 3,
  detailed = TRUE,
  return_aov = TRUE)
wtf_is(mod.ez)


library(afex)
model2 <- aov_ez(
  id = "Sub",  # subject
  dv = "Depvariable",
  data = data,
  between = c("Group"),
  within = c("WithinFactor"),
  type = "III"  # or 3; type III sums of squares
)
anova(model2)
summary(model2)
residuals(model2$lm)
library(tidyverse)

# https://stackoverflow.com/questions/26169153/how-to-get-residuals-from-repeated-measures-anova-model-in-r
data("npk")
npk
npk.aovE <- aov(yield ~  N*P*K + Error(block), npk)
npk.aovE
summary(npk.aovE)
npk.pr <- proj(npk.aovE)
npk.pr[[3]][, "Residuals"]
# qqnorm(npk.pr[[3]][, "Residuals"])

# https://stats.stackexchange.com/questions/182988/plotting-to-check-homoskedasticity-assumption-for-repeated-measures-anova-in-r/185368

wtf_is(model2)
data
model2$lm$residuals
model2$aov$`Sub:WithinFactor`$residuals
model2$aov$Sub$residuals
