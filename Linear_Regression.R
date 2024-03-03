rm(list=ls())
library(dplyr)
library(corrplot)
data <- read.csv("Master_Stats.csv")

#omit irrelevant stats, such as batter age, plate appears
realdata <- select(data, -X.Bat, -G,PAge, -L, -W.L., -G.1, -GS, -GF, -CG, 
                   -tSho, -cSho, -BK, -BatAge, -R,
                    -AB, -PA, -PAge,)

colnames(realdata)

#offense: start at 5 stats and iterate to see different models
base  <- lm(W.1~ BA + HR + RBI + H + SB, data=realdata)
b_2B  <- lm(W.1~ BA + HR + RBI + H + SB + X2B, data=realdata)            
b_3B  <- lm(W.1~ BA + HR + RBI + H + SB + X2B + X3B, data=realdata)       
b_CS  <- lm(W.1~ BA + HR + RBI + H + SB + X2B + X3B + CS, data=realdata)       
b_BB  <- lm(W.1~ BA + HR + RBI + H + SB + X2B + X3B + CS + BB, data=realdata)       
b_SO  <- lm(W.1~ BA + HR + RBI + H + SB + X2B + X3B + CS + BB + SO, data=realdata)       
b_OBP <- lm(W.1~ BA + HR + RBI + H + SB + X2B + X3B + CS + BB + SO + OBP, data=realdata)       
b_SLG <- lm(W.1~ BA + HR + RBI + H + SB + X2B + X3B + CS + BB + SO + OBP + SLG, data=realdata)       
b_OPS <- lm(W.1~ BA + HR + RBI + H + SB + X2B + X3B + CS + BB + SO + OBP + SLG + OPS, data=realdata)       
b_TB  <- lm(W.1~ BA + HR + RBI + H + SB + X2B + X3B + CS + BB + SO + OBP + SLG + OPS + TB, data=realdata)       
b_GDP <- lm(W.1~ BA + HR + RBI + H + SB + X2B + X3B + CS + BB + SO + OBP + SLG + OPS + TB + GDP, data=realdata)       
b_HBP <- lm(W.1~ BA + HR + RBI + H + SB + X2B + X3B + CS + BB + SO + OBP + SLG + OPS + TB + GDP + HBP, data=realdata)
b_SH  <- lm(W.1~ BA + HR + RBI + H + SB + X2B + X3B + CS + BB + SO + OBP + SLG + OPS + TB + GDP + HBP + SH, data=realdata)
b_SF  <- lm(W.1~ BA + HR + RBI + H + SB + X2B + X3B + CS + BB + SO + OBP + SLG + OPS + TB + GDP + HBP + SH + SF, data=realdata)
b_IBB <- lm(W.1~ BA + HR + RBI + H + SB + X2B + X3B + CS + BB + SO + OBP + SLG + OPS + TB + GDP + HBP + SH + SF + IBB, data=realdata)
b_LOB <- lm(W.1~ BA + HR + RBI + H + SB + X2B + X3B + CS + BB + SO + OBP + SLG + OPS + TB + GDP + HBP + SH + SF + IBB + LOB, data=realdata)
b_remove <- lm(W.1~ BA + HR + RBI + H + SB + X2B + X3B + CS + BB + SO + GDP + HBP + SH + SF + IBB + LOB, data=realdata)

base_d <- lm(W.1 ~ ERA + SV + BB.1 + HR.1 + SO.1, data= realdata)

#load most important Offensive stats into a model
offense <- lm(W.1 ~ BA + HR + H + SB + CS + BB + GDP + IBB + SLG, data = realdata)

#load most important Defensive stats into a model
defense <- lm(W.1 ~ ERA + SV + BB.1 + HR.1 + SO.1 + IBB.1 + IP + HBP.1+ WP, data=realdata)

#try combining both stats (led to overfit data)
combined  <- lm(W.1 ~ BA + HR + H + SB + CS + BB + GDP + IBB + SLG + ERA + SV + BB.1 + HR.1 + SO.1 + IBB.1 + WP + X.P, data=realdata)
summary(offense)
summary(defense)
summary(combined)

#check correlation between offensive variables
stats_o<-c("HR", "H", "SB", "BA", "CS", "BB", "GDP", "SLG", "IBB")

#check correlation between defensive variables
stats_d<- c("ERA", "SV", "BB.1", "HR.1", "SO.1", "IBB.1", "IP", "HBP.1",  "WP")

correlation_matrix <- cor(realdata[, stats_o])
corrplot(cor(realdata[, stats_o]))     
print(correlation_matrix)

correlation_matrix <- cor(realdata[, stats_d])
corrplot(cor(realdata[, stats_d])) 
print(correlation_matrix)
