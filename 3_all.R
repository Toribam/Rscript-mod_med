# mean centering

data <- read.csv("data/data.csv")

attach(data)


# modmed_test

# ch
# step 1
posi_dv_model <- lm(posi_jc ~ sex + age + edu + tenure)

posi_dv_model1 <- update(posi_dv_model, .~. + mc_ch + mc_c + mc_ch:mc_c)
summary(posi_dv_model1)

# step 2
posi_med_model <- lm(mc_enga ~ sex + age + edu + tenure)

posi_med_model2 <- update(posi_med_model, .~. + mc_ch + mc_c + mc_ch:mc_c)
summary(posi_med_model2)

#step 3
posi_dv_model3 <- update(posi_dv_model, .~. + mc_ch + mc_c + mc_ch:mc_c + mc_enga + mc_c:mc_enga)
summary(posi_dv_model3)

rm(list=ls()[grepl("posi", ls())])


# hi
# step 1
nega_dv_model <- lm(nega_jc ~ sex + age + edu + tenure)

nega_dv_model1 <- update(nega_dv_model, .~. + mc_hi + mc_n + mc_hi:mc_n)
summary(nega_dv_model1)
# step 2
nega_med_model <- lm(mc_enga ~ sex + age + edu + tenure)

nega_med_model2 <- update(nega_med_model, .~. +mc_hi + mc_n + mc_hi:mc_n)
summary(nega_med_model2)

# step 3
nega_dv_model3 <- update(nega_dv_model, .~. + mc_hi + mc_n + mc_hi:mc_n + mc_enga + mc_c:mc_enga)
summary(nega_dv_model3)

rm(list=ls()[grepl("nega", ls())])

rm(list=ls())
