# mediation effect analysis by Hayes

data <- read.csv("data.csv")

library(mediation)

attach(data)

# mediation effect of work engagement between challenge stressor and positive job crafting

# step1
ch_med.fit <- lm(WE ~ ch_str + sex + age + edu + tenure)
summary(ch_med.fit)

# step 2
ch_out.fit <- glm(posi_jc ~ WE + ch_str + sex + age + edu + tenure, family = gaussian(link = identity))
summary(ch_out.fit)

# direct(ADE; average direct effect) and indirect(ACME; average casual mediation effect) effect
ch_med.out <- mediate(ch_med.fit,
                      ch_out.fit,
                      treat = "ch_str",
                      mediator = "WE",
                      robustSE = TRUE,
                      sims = 5000)
summary(ch_med.out)
plot(ch_med.out)

rm(list=ls()[grepl("ch", ls())])


# mediation effect of work engagement between challenge stressor and positive job crafting

# step 1
hi_med.fit <- lm(WE ~ hi_str + sex + age + edu + tenure)
summary(hi_med.fit)

# step 2
hi_out.fit <- glm(nega_jc ~ WE + hi_str + sex + age + edu + tenure, family = gaussian(link = identity))
summary(hi_out.fit)

# direct(ADE; average direct effect) and indirect(ACME; average casual mediation effect) effect
hi_med.out <- mediate(hi_med.fit,
                      hi_out.fit,
                      treat = "hi_str",
                      mediator = "WE",
                      robustSE = TRUE,
                      sims = 5000)
summary(hi_med.out)
plot(hi_med.out)

rm(list=ls()[grepl("hi", ls())])

