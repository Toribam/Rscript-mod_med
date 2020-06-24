# moderated mediation effect

data <- read.csv("data/data.csv")

library(mediation)

attach(data)

# ch modmed

ch_med.fit <- lm(WE ~ sex + age + edu + tenure + ch_str + C + ch_str:C)
summary(ch_med.fit)

ch_out.fit <- glm(posi_jc ~ sex + age + edu + tenure + ch_str + WE)
summary(ch_out.fit)

ch_med.out <- mediate(ch_med.fit,
                      ch_out.fit,
                      treat = "ch_str",
                      mediator = "WE",
                      robustSE = TRUE,
                      sims = 5000)
summary(ch_med.out)
plot(ch_med.out)

# hi modmed

hi_med.fit <- lm(WE ~ sex + age + edu +tenure + hi_str + N + hi_str:N)
summary(hi_med.fit)

hi_out.fit <- glm(nega_jc ~ sex + age + edu + tenure + hi_str + WE)
summary(hi_out.fit)

hi_out.fit <- mediate(hi_med.fit,
                      hi_out.fit,
                      treat = "hi_str",
                      mediator = "WE",
                      robustSE = TRUE,
                      sims = 5000)
summary(hi_out.fit)
plot(hi_out.fit)

rm(list=ls())


