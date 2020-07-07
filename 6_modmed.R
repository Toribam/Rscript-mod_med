# moderated mediation effect
  data <- read.csv("data/data.csv")

# ch modmed
  ch_med.fit <- lm(WE ~ sex + age + edu + tenure + ch_str + C + ch_str:C, data = data)
  summary(ch_med.fit)
  
  ch_out.fit <- glm(posi_jc ~ sex + age + edu + tenure + ch_str + WE, data = data)
  summary(ch_out.fit)
  
  ch_med.out <- mediation:: mediate(ch_med.fit,
                                    ch_out.fit,
                                    treat = "ch_str",
                                    mediator = "WE",
                                    robustSE = TRUE,
                                    sims = 5000) # 논문에 Robust SE 사용을 밝히는 게 유리합니다.
  summary(ch_med.out)
  plot(ch_med.out) # plot을 svg로 저장해 제시하면 좋습니다.

# hi modmed
  hi_med.fit <- lm(WE ~ sex + age + edu +tenure + hi_str + N + hi_str:N, data = data)
  summary(hi_med.fit)
  
  hi_out.fit <- glm(nega_jc ~ sex + age + edu + tenure + hi_str + WE, data = data)
  summary(hi_out.fit)
  
  hi_out.fit <- mediation::mediate(hi_med.fit,
                                   hi_out.fit,
                                   treat = "hi_str",
                                   mediator = "WE",
                                   robustSE = TRUE,
                                   sims = 5000) # 논문에 Robust SE 사용을 밝히는 게 유리합니다.
  summary(hi_out.fit)
  plot(hi_out.fit) # plot을 svg로 저장해 제시하면 좋습니다.
  
  # remove all objects
  rm(list=ls())
