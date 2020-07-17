# mean centering
  data <- read.csv("data/data.csv")

# modmed_test

  # ch
    # step 1
      posi_dv_model <- lm(posi_jc ~ sex + age + edu + tenure, data = data)
      
      posi_dv_model1 <- update(posi_dv_model, .~. + mc_ch + mc_c + mc_ch:mc_c, data = data)
      summary(posi_dv_model1)
    
    # step 2
      posi_med_model <- lm(mc_enga ~ sex + age + edu + tenure, data = data)
      
      posi_med_model2 <- update(posi_med_model, .~. + mc_ch + mc_c + mc_ch:mc_c, data = data)
      summary(posi_med_model2)
    
    #step 3
      posi_dv_model3 <- update(posi_dv_model, .~. + mc_ch + mc_c + mc_ch:mc_c + mc_enga + mc_c:mc_enga, data = data)
      summary(posi_dv_model3)
    
    # remove objects where starts with posi
      rm(list=ls()[grepl("^posi", ls())])
    
    
  # hi
    # step 1
      nega_dv_model <- lm(nega_jc ~ sex + age + edu + tenure, data = data)
    
      nega_dv_model1 <- update(nega_dv_model, .~. + mc_hi + mc_n + mc_hi:mc_n, data = data)
      summary(nega_dv_model1)
    # step 2
      nega_med_model <- lm(mc_enga ~ sex + age + edu + tenure, data = data)
    
      nega_med_model2 <- update(nega_med_model, .~. +mc_hi + mc_n + mc_hi:mc_n, data = data)
      summary(nega_med_model2)
    
    # step 3
      nega_dv_model3 <- update(nega_dv_model, .~. + mc_hi + mc_n + mc_hi:mc_n + mc_enga + mc_c:mc_enga, data = data)
      summary(nega_dv_model3)
      
      
    # remove objects where starts with nega
      rm(list=ls()[grepl("^nega", ls())])
    
    # remove all  
      rm(list=ls())
