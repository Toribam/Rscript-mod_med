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
                                    sims = 5000)
  summary(ch_med.out)
  
  svg(filename = "figure/5_plot_ch_modmed.svg",
      width = 8,
      height = 6,
      pointsize = 12,
      )
  plot(ch_med.out)
  dev.off()

# hi modmed
  hi_med.fit <- lm(WE ~ sex + age + edu +tenure + hi_str + N + hi_str:N, data = data)
  summary(hi_med.fit)
  
  hi_out.fit <- glm(nega_jc ~ sex + age + edu + tenure + hi_str + WE, data = data)
  summary(hi_out.fit)
  
  hi_med.out <- mediation::mediate(hi_med.fit,
                                   hi_out.fit,
                                   treat = "hi_str",
                                   mediator = "WE",
                                   robustSE = TRUE,
                                   sims = 5000)
  summary(hi_med.out)
  
  svg(filename = "figure/5_plot_hi_modmed.svg",
      width = 8,
      height = 6,
      pointsize = 12,
  )
  plot(hi_med.out)
  dev.off()
  
  
  # remove all objects
  rm(list=ls())
