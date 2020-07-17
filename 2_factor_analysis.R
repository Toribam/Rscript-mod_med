# job crafting with lavaan package
# except JC6, JC7 because path was not acceptable

if(!require('lavaan')){
  install.packages('lavaan', repos = 'https://cran.asia')
}

if(!require('semPlot')){
  install.packages('semPlot', repos = 'https://cran.asia')
}
if(!require('semTools')){
  install.packages('semTools', repos = 'https://cran.asia')
}
if(!require('haven')){
  install.packages('haven', repos = 'https://cran.asia')
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

one_model_fit <- lavaan::cfa(one_factor_model, data = jc, std.lv = T) # 특정 조건 (Intel Math Kernel Library)에서 vcov가 음수가 나옵니다.
lavaan::summary(one_model_fit, standardized = TRUE, fit.measures = T)

semPlot::semPaths(one_model_fit,
                  "std",
                  edge.label.cex = 1.2,
                  rotation = 4,
                  )  # plot을 svg로 저장해 제시하면 좋습니다.


two_factor_model <- '
posi_jc =~ str + soc + cha

str =~ JC1 + JC2 + JC3 + JC4 + JC5

soc =~ JC12 + JC13 + JC14 + JC15 + JC16

cha =~ JC17 + JC18 + JC19 + JC20 + JC21

nega_jc =~ hind

hind =~ JC8 + JC9 + JC10 + JC11
'

# test
two_model_fit <- lavaan::cfa(two_factor_model, data = jc) # 오류: hind 단일만 nega_jc가 될 수 없음, cha는 분산이 음분산이 나타나 추정 불능

lavaan::summary(two_model_fit, standardized = TRUE)

semPlot::semPaths(two_model_fit,
                  "std",
                  nCharNodes = 7,
                  edge.label.cex = 1,
                  rotation = 4,
                  )  # plot을 svg로 저장해 제시하면 좋습니다.

