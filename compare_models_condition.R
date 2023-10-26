##
## This script runs the contrasts several models to see if condition should
## be included in some way as fixed effect or interaction.
## M1 without condition was slightly better and presented in main text. 
##
library(xlsx)
library(plyr)
library(dplyr)
library(rstanarm)
library(rstan)
library(ggplot2)
library(matrixStats)
library(BayesFactor)
library(bayestestR)
library(reshape)

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

# Reshape the matrix so each trial is a row
trial_df <- melt(df, id.vars = c("Chain", 'Treatment', "Generation"), measure.vars = c("Trial1.Speed", "Trial2.Speed", "Trial3.Speed","Trial4.Speed","Trial5.Speed"))
names(trial_df)[names(trial_df) == "value"] <- "speed"
trial_df$TrialN <- as.numeric(substr(trial_df$variable, 6, 6))
trial_df$trial <- as.numeric(trial_df$TrialN + (trial_df$Generation - 1) * 5)

# Remove any rows that are nan
trial_df <- na.omit(trial_df)

# Run the models
m0 <- stan_glmer(speed ~ 1 + (1 | Chain),
                 data = trial_df, cores=2, chains = 4, iter = 40000,
                 diagnostic_file = "null_model.csv", seed=23)
m1 <- stan_glmer(speed ~ 1 + trial + (1 | Chain),
                 data = trial_df, cores=2, chains = 4, iter = 40000,
                 diagnostic_file = "m1.csv", seed=23)
m2 <- stan_glmer(speed ~ 1 + trial:Treatment + (1 | Chain),
                 data = trial_df, cores=2, chains = 4, iter = 40000,
                 diagnostic_file = "m2.csv", seed=23)
m3 <- stan_glmer(speed ~ 1 + trial + Treatment + (1 | Chain),
                 data = trial_df, cores=2, chains = 4, iter = 40000,
                 diagnostic_file = "m3.csv", seed=23)

# Test 1 - null model where trial is not included
bayesfactor_models(m0, m1, m2, m3)
# Test 2 - null model where condition is not included (but trial is always inlcuded)
bayesfactor_models(m1, m2, m3)


describe_posterior(m1, test = c("pd", "ROPE", "BF"), ci=0.95, rope_ci=0.95)
