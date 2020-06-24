# job crafting with lavaan package
# except JC6, JC7 because path was not acceptable

# install.packages(c("haven", "dplyr", "corrplot", "PerformanceAnalytics"), dependencies = TRUE)

for( i in c("lavaan", "semPlot", "semTools", "haven")){
  library(i, character.only = TRUE)
}

data <- read.csv("data/data.csv")

jc <- data[,13:33]

#classify job crafting subfactors by 4 groups

one_factor_model <- '
str =~ JC1 + JC2 + JC3 + JC4 + JC5

soc =~ JC12 + JC13 + JC14 + JC15 + JC16

cha =~ JC17 + JC18 + JC19 + JC20 + JC21

hind =~ JC8 + JC9 + JC10 + JC11
'

one_model_fit <- cfa(one_factor_model, data = jc)
summary(one_model_fit, standardized = TRUE)

semPlot::semPaths(one_model_fit,
                  "std",
                  edge.label.cex = 1.2,
                  rotation = 4,
                  )


two_factor_model <- '
posi_jc =~ str + soc + cha

str =~ JC1 + JC2 + JC3 + JC4 + JC5

soc =~ JC12 + JC13 + JC14 + JC15 + JC16

cha =~ JC17 + JC18 + JC19 + JC20 + JC21

nega_jc =~ hind

hind =~ JC8 + JC9 + JC10 + JC11
'

# test
two_model_fit <- cfa(two_factor_model, data = jc)

summary(two_model_fit, standardized = TRUE)

semPlot::semPaths(two_model_fit,
                  "std",
                  nCharNodes = 7,
                  edge.label.cex = 1,
                  rotation = 4,
                  )

