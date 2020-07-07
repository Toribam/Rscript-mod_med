# moderation effect
  
  data <- read.csv("data/data.csv")

# Moderation effect of conscientiousness between challenge stressor and work engagement

  # step 1 control variables
    ch_mod1 <- lm(WE ~ sex + age + edu + tenure, data = data)
    summary(ch_mod1)
    
  # step 2 independent variables
    ch_mod2 <- update(ch_mod1, .~. + ch_str + C, data = data)
    summary(ch_mod2)
    
  # step 3 interaction effect
    ch_mod3 <- update(ch_mod2, .~. + ch_str:C, data = data)
    summary(ch_mod3)
    
    sjPlot::plot_model(ch_mod3, type = "pred", terms = c("ch_str", "C"))
    
    # simple slope test with plot
      interactions::probe_interaction(ch_mod3, pred = 'ch_str', modx = "C")
      # JOHNSON-NEYMAN INTERVAL 
      # 
      # When C is OUTSIDE the interval [0.67, 3.77], the slope of ch_str is p < .05.
      # 
      # Note: The range of observed values of C is [2.50, 5.00]
      # 
      # SIMPLE SLOPES ANALYSIS 
      # 
      # Slope of ch_str when C = 3.36 (- 1 SD): 
      #   
      #   Est.   S.E.   t val.      p
      # ------ ------ -------- ------
      #   0.03   0.10     0.28   0.78
      # 
      # Slope of ch_str when C = 3.87 (Mean): 
      #   
      #   Est.   S.E.   t val.      p
      # ------ ------ -------- ------
      #   0.18   0.07     2.43   0.02
      # 
      # Slope of ch_str when C = 4.39 (+ 1 SD): 
      #   
      #   Est.   S.E.   t val.      p
      # ------ ------ -------- ------
      #   0.33   0.09     3.49   0.00
    
    # remove objects where starts with ch
    rm(list=ls()[grepl(c("^ch"), ls())])
    
# Moderation effect of neuroticism between hindrance streesor and work engagement
  
  # step 1 control varialbes
    hi_mod1 <- lm(WE ~ sex + age + edu + tenure, data = data)
    summary(hi_mod1)
    
    hi_mod2 <- update(hi_mod1, .~. + hi_str + N, data = data)
    summary(hi_mod2)
    
    hi_mod3 <- update(hi_mod2, .~. + hi_str:C, data = data)
    summary(hi_mod3)
    
    sjPlot::plot_model(hi_mod3, type = "pred", terms = c("hi_str", "N")) # 상호작용항은 C와 만들었는데 왜 N과 상호작용을 계산하지요?
    
    # simple slope test with plot
    interactions::probe_interaction(hi_mod3, pred = 'hi_str', modx = "N") # C와 상호작용을 만들었으므로 N과 상호작용 효과 계산 불능
    interactions::probe_interaction(hi_mod3, pred = 'hi_str', modx = "C") # C와 상호작용을 만들었으므로 C를 조절변수로 지정해야 계산 가능
    
    # JOHNSON-NEYMAN INTERVAL 
    # 
    # When C is OUTSIDE the interval [-2.47, 3.22], the slope of hi_str is p < .05.
    # 
    # Note: The range of observed values of C is [2.50, 5.00]
    # 
    # SIMPLE SLOPES ANALYSIS 
    # 
    # Slope of hi_str when C = 3.36 (- 1 SD): 
    #   
    #   Est.   S.E.   t val.      p
    # ------- ------ -------- ------
    #   -0.24   0.09    -2.67   0.01
    # 
    # Slope of hi_str when C = 3.87 (Mean): 
    #   
    #   Est.   S.E.   t val.      p
    # ------- ------ -------- ------
    #   -0.38   0.06    -5.96   0.00
    # 
    # Slope of hi_str when C = 4.39 (+ 1 SD): 
    #   
    #   Est.   S.E.   t val.      p
    # ------- ------ -------- ------
    #   -0.52   0.08    -6.47   0.00
  
  
    # remove objects where starts with hi
      rm(list=ls()[grepl(c("hi"), ls())])
