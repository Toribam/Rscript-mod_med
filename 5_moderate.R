# moderation effect

for( i in c("sjplot", "sjmisc", "ggplot2")){
  library(i, character.only = TRUE)
}


data <- read.csv("data/data.csv")

attach(data)
# Moderation effect of conscientiousness between challenge stressor and work engagement

# step 1 control variables
ch_mod1 <- lm(WE ~ sex + age + edu + tenure)
summary(ch_mod1)

# step 2 independent variables
ch_mod2 <- update(ch_mod1, .~. + ch_str + C)
summary(ch_mod2)

# step 3 interaction effect
ch_mod3 <- update(ch_mod2, .~. + ch_str:C)
summary(ch_mod3)

plot_model(ch_mod3, type = "pred", terms = c("ch_str", "C"))

rm(list=ls()[grepl(c("ch"), ls())])

# Moderation effect of neuroticism between hindrance streesor and work engagement

# step 1 control varialbes
hi_mod1 <- lm(WE ~ sex + age + edu + tenure)
summary(hi_mod1)

hi_mod2 <- update(hi_mod1, .~. + hi_str + N)
summary(hi_mod2)

hi_mod3 <- update(hi_mod2, .~. + hi_str:C)
summary(hi_mod3)

plot_model(hi_mod3, type = "pred", terms = c("hi_str", "N"))



rm(list=ls()[grepl(c("hi"), ls())])
