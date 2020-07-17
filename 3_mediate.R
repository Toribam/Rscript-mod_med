# mediation effect analysis by Hayes
  data <- read.csv("data/data.csv")
  

# mediation effect of work engagement between challenge stressor and positive job crafting
  
  # step1
    ch_med.fit <- lm(WE ~ ch_str + sex + age + edu + tenure, data = data)
    summary(ch_med.fit)
  
  # step 2
    ch_out.fit <- glm(posi_jc ~ WE + ch_str + sex + age + edu + tenure, family = gaussian(link = identity), data = data)
    summary(ch_out.fit)
  
  # direct(ADE; average direct effect) and indirect(ACME; average casual mediation effect) effect
    ch_med.out <- mediation::mediate(ch_med.fit,
                                     ch_out.fit,
                                     treat = "ch_str",
                                     mediator = "WE",
                                     robustSE = TRUE,
                                     sims = 5000) # 논문에 Robust SE 사용을 밝히는 게 유리합니다.
    summary(ch_med.out)
    svg(filename = "figure/3_plot_ch_med.svg",
        width = 8,
        height = 6,
        pointsize = 12)
    plot(ch_med.out)
    dev.off()
  
  # remove objects where starts with ch
    rm(list=ls()[grepl("^ch", ls())])


# mediation effect of work engagement between challenge stressor and positive job crafting

  # step 1
    hi_med.fit <- lm(WE ~ hi_str + sex + age + edu + tenure, data = data)
    summary(hi_med.fit)
  
  # step 2
    hi_out.fit <- glm(nega_jc ~ WE + hi_str + sex + age + edu + tenure, family = gaussian(link = identity), data = data)
    summary(hi_out.fit)
  
  # direct(ADE; average direct effect) and indirect(ACME; average casual mediation effect) effect
    hi_med.out <- mediation::mediate(hi_med.fit,
                                     hi_out.fit,
                                     treat = "hi_str",
                                     mediator = "WE",
                                     robustSE = TRUE,
                                     sims = 5000)
    summary(hi_med.out)
    svg(filename = "figure/3_plot_hi_med.svg",
        width = 8,
        height = 6,
        pointsize = 12)
    plot(hi_med.out)
    dev.off()
    
    # remove objects where starts with hi
      rm(list=ls()[grepl("^hi", ls())])
  
