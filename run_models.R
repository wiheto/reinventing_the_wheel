##
## This script runs the contrast whether 
## score ~ generation + (1 | chain)
## (i.e.) score modelled by generaiton accounting for each chain
## Score can be both theory or knowledge
## Also run a model where first four trials are run. 
##
library(xlsx)
library(rstanarm)
library(ggplot2)
library(performance)
library(bayestestR)

# Load data
setwd("./")
wb <- "./data/data_combined.xlsx"
df  <- read.xlsx(wb, sheetName  = "data")

## Conversion set factors/numerics
df$Chain <- as.factor(df$Chain)
df$Group <- as.factor(df$Group)
df$Participant <- as.factor(df$Participant)
df$Treatment <- as.factor(df$Treatment)
df$Generation <- as.numeric(df$Generation)

# replace 0 speed with na
df$Trial1.Speed[df$Trial1.Speed == 0] <- NA
df$Trial2.Speed[df$Trial2.Speed == 0] <- NA
df$Trial3.Speed[df$Trial3.Speed == 0] <- NA
df$Trial4.Speed[df$Trial4.Speed == 0] <- NA
df$Trial5.Speed[df$Trial5.Speed == 0] <- NA

df_firstfour <- subset(df, Generation != 5)

# All trials theory vs generation -> pooling all three conditions
m_theory <- stan_glmer(Total.Score ~ Generation + (1 | Chain), cores=4, data=df, diagnostic_file = file.path(tempdir(), "df2.csv"), seed=23, iter=40000)
mpost_theory <- describe_posterior(m_theory, test = c("pd", "ROPE", "BF"), ci=0.95, rope_ci=0.95)
model_performance(m_theory)

# First four trials only
m_theory_f4 <- stan_glmer(Total.Score ~ Generation + (1 | Chain), cores=4, data=df_firstfour, diagnostic_file = file.path(tempdir(), "dftmp3.csv"), seed=23, iter=40000)
mpost_theory_f4 <- describe_posterior(m_theory_f4, test = c("pd", "ROPE", "BF"), ci=0.95, rope_ci=0.95)
model_performance(m_theory_f4)

# All trials knowledge vs generation -> pooling all three conditions
m_knowledge <- stan_glmer(Test.Score ~ Generation + (1 | Chain), cores=4, data=df, diagnostic_file = file.path(tempdir(), "df2.csv"), seed=23, iter=40000)
mpost_knowledge <- describe_posterior(m_knowledge, test = c("pd", "ROPE", "BF"), ci=0.95, rope_ci=0.95)
model_performance(m_knowledge)

