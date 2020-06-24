# refining data

# install.packages(c("haven", "dplyr", "corrplot", "PerformanceAnalytics"), dependencies = TRUE)



for(i in c("haven", "dplyr", "corrplot", "PerformanceAnalytics", "corrplot")){
  library(i, character.only = TRUE)
}

setwd(getwd())

df <- read_spss("data/motor.sav")

attach(df)

ID <- df$ID

# personality

C_var <- df %>%
  select(C1, C2, C3, C4, C5, C6)
C <- apply(C_var[1:6], 1, mean)
mc_c <- C -mean(C)

summary(C)
summary(mc_c)


N_var <- df %>%
  select(N_r1, N2, N3, N4, N5, N6)
N1 <- (6 - N_var$N_r1)
N_var <- cbind(N_var, N1)
head(N_var)
N_var <- N_var[,-1]
head(N_var)
N <- apply(N_var[1:6], 1, mean)
mc_n <- N - mean(N)

summary(N)
summary(mc_n)

rm(N1)

names(df)

# Stressor

c_stressor_var <- df$C_Stressor
h_stressor_var <- df$H_Stressor

mc_ch <- c_stressor_var - mean(c_stressor_var)
mc_hi <- h_stressor_var - mean(h_stressor_var)


# Work Engagement

WE_var <- df$enga
mc_enga <- WE_var - mean(WE_var)

# Job Crafting

posi_jc <- df$posi_jc
nega_jc <- df$jc_hi

total_job_crafting <- df[,66:86]

data <- data.frame(cbind(ID, sex, age, edu, tenure, C, N, c_stressor_var, h_stressor_var, WE_var, posi_jc, nega_jc))

names(data) <- c("ID", "sex", "age", "edu", "tenure", "C", "N", "ch_str", "hi_str", "WE", "posi_jc", "nega_jc")

mean_centered <- data.frame(cbind(mc_c, mc_n, mc_ch, mc_hi, mc_enga))

data <- data.frame(cbind(data, total_job_crafting, mean_centered))

names(data)

plot(data[,2:12])
correlation_data <- cor(data[,2:12])
corrplot(correlation_data, method = "number")

#from "PerformanceAnalytics" package
chart.Correlation(correlation_data, histogram = TRUE, ch = 18)

#rename variables

# save data
write.csv(data, "data/data.csv", row.names = FALSE)

data <- read.csv("data/data.csv")
names(data)

rm(list=ls())
