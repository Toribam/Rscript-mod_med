# run.R
  if(length(list.files('data/', 'sav$'))>0){
    source('1_refine_data.R')
  }
  source('2_factor_analysis.R')
  source('3_all.R')
  source('4_mediate.R')
  source('5_moderate.R')
  source('6_modmed.R')