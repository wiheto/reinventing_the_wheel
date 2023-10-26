# Notes on excel column names I got:
# Treatment -> Conditions
# Test.Score -> Knowledge test score
# Total.Score -> Theory-quality score

# Code plots figures 3bcd, 4abc. 
# Further it calculates the statistics for 4abc

# Import libraries
library(xlsx)
library(ggplot2)
library(rstanarm)
library(matrixStats)
library(dplyr)
library(reshape)
library(performance)
library(bayestestR)



# Load data
setwd("./")
wb <- "./data/data_combined.xlsx"
df  <- read.xlsx(wb, sheetName  = "data")

# Functions (these were taken from Osiurak's code)
mysumm <- function (x) c(Mean=mean(x), SD = sd(x), N = length(x), SE = sd(x)/sqrt(length(x)))
mysumm_noNA <- function (x) c(Mean=mean(x,na.rm=T), SD = sd(x,na.rm=T), N = sum(!is.na(x)), SE = sd(x,na.rm=T)/sqrt(sum(!is.na(x))), NbNA=sum(is.na(x)))

#####################
### PREPROCESSING ###
#####################
# There is a better way to do this. 
# But data is first made into the correct data format
# Then 0s are made to NAs
# Then, split the data into three different groups. 
# Then, Calculate the mean/sd each group. Then combine again into a dataframe
# The final step is repeated at the start of each figure for the relevant data

## Conversion Chain & Participant - EXPERIMENT 1
df$Chain <- as.factor(df$Chain)
df$Group <- as.factor(df$Group)
df$Participant <- as.factor(df$Participant)
df$Treatment <- as.factor(df$Treatment)
df$Generation <- as.numeric(df$Generation)
df$Test.Score <- as.numeric(df$Test.Score)
df$Total.Score <- as.numeric(df$Total.Score)

df$maxspeed <- rowMaxs(as.matrix(df[, c('Trial1.Speed', 'Trial2.Speed', 'Trial3.Speed', 'Trial4.Speed', 'Trial5.Speed')]))

# replace 0 speed with na
df$Trial1.Speed[df$Trial1.Speed == 0] <- NA
df$Trial2.Speed[df$Trial2.Speed == 0] <- NA
df$Trial3.Speed[df$Trial3.Speed == 0] <- NA
df$Trial4.Speed[df$Trial4.Speed == 0] <- NA
df$Trial5.Speed[df$Trial5.Speed == 0] <- NA

# Group the three different treatments
df_treatment1 <- subset(df, Treatment==1)
df_treatment2 <- subset(df, Treatment==2)
df_treatment3 <- subset(df, Treatment==3)

# For Fig 3c
df_descriptive_knowledge_all = do.call(data.frame, aggregate(Test.Score~Generation, df, mysumm))
df_descriptive_knowledge_t1 = do.call(data.frame, aggregate(Test.Score~Generation, df_treatment1, mysumm))
df_descriptive_knowledge_t1 %>% rename_with(.fn = ~ paste0("Treatment1.", .x)) -> df_descriptive_knowledge_t1
df_descriptive_knowledge_t2 = do.call(data.frame, aggregate(Test.Score~Generation, df_treatment2, mysumm))
df_descriptive_knowledge_t2 %>% rename_with(.fn = ~ paste0("Treatment2.", .x)) -> df_descriptive_knowledge_t2
df_descriptive_knowledge_t3 = do.call(data.frame, aggregate(Test.Score~Generation, df_treatment3, mysumm))
df_descriptive_knowledge_t3 %>% rename_with(.fn = ~ paste0("Treatment3.", .x)) -> df_descriptive_knowledge_t3


df_descriptive_knowledge=cbind(df_descriptive_knowledge_all, df_descriptive_knowledge_t1, df_descriptive_knowledge_t2, df_descriptive_knowledge_t3)
# Select only the relevant columns
df_descriptive_knowledge = df_descriptive_knowledge[c('Generation', 'Test.Score.Mean', 
                                       'Treatment1.Test.Score.Mean', 'Treatment2.Test.Score.Mean', 
                                       'Treatment3.Test.Score.Mean', 'Test.Score.SE',
                                       'Treatment1.Test.Score.SE', 'Treatment2.Test.Score.SE', 'Treatment3.Test.Score.SE')]
df_descriptive_knowledge[order(df_descriptive_knowledge$Generation), ]
# Save descriptive output
write.csv(df_descriptive_knowledge, "./tables/descriptivestatas_generation_vs_knowledgetest.csv", row.names=FALSE)

##############
### FIG 3c ###
##############

# Figure where each treatment
fig_3c <- ggplot(df_descriptive_knowledge, aes(x=Generation, y=Test.Score.Mean)) +  
  geom_line(aes(x=Generation-0.05, y = Treatment1.Test.Score.Mean ,color='Condition 1'), linewidth=0.3) + 
  geom_errorbar(aes(x=Generation-0.05, ymin=Treatment1.Test.Score.Mean-Treatment1.Test.Score.SE, ymax=Treatment1.Test.Score.Mean+Treatment1.Test.Score.SE,  width=.1), col = "blue" )+
  geom_line(aes(x=Generation+0.05, y = Treatment2.Test.Score.Mean, col="Condition 2"), linewidth=0.3) + 
  geom_errorbar(aes(Generation+0.05, ymin=Treatment2.Test.Score.Mean-Treatment2.Test.Score.SE, ymax=Treatment2.Test.Score.Mean+Treatment2.Test.Score.SE,  width=.1), col = "darkgreen" )+
  geom_line(aes(x=Generation+0.15, y = Treatment3.Test.Score.Mean, col="Condition 3"), linewidth=0.3) + 
  geom_errorbar(aes(Generation+0.15, ymin=Treatment3.Test.Score.Mean-Treatment3.Test.Score.SE, ymax=Treatment3.Test.Score.Mean+Treatment3.Test.Score.SE,  width=.1), col = "darkred" )+
  geom_line(aes(x=Generation-0.15, y = Test.Score.Mean, color='All conditions'), linewidth=0.8) + 
  geom_errorbar(aes(x=Generation-0.15, ymin=Test.Score.Mean-Test.Score.SE, ymax=Test.Score.Mean+Test.Score.SE,  width=.3), col = "black" )+
  ylim(0, 9)+
  scale_color_manual(name='', breaks=c('All conditions', 'Condition 1', 'Condition 2', 'Condition 3'),
                     values=c('All conditions'='black', 'Condition 1'='darkblue', 'Condition 2'='darkgreen', 'Condition 3'='darkred')) +
  labs(x='Generation', y='Knowledge test score') +
  
  theme_classic()+
  theme(panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(colour = "grey"),
        panel.background = element_rect(fill = NA),
        legend.position="bottom") 

ggsave("./figures/fig_3c.png", plot=fig_3c, units="in", width=5, height=4, dpi=300)


##############
### FIG 3d ###
##############

## FIGURE - THEORY SCORE vs GENERATION
df_descriptive_theory_all = do.call(data.frame, aggregate(Total.Score~Generation, df, mysumm))
df_descriptive_theory_t1 = do.call(data.frame, aggregate(Total.Score~Generation, df_treatment1, mysumm))
df_descriptive_theory_t1 %>% rename_with(.fn = ~ paste0("Treatment1.", .x)) -> df_descriptive_theory_t1
df_descriptive_theory_t2 = do.call(data.frame, aggregate(Total.Score~Generation, df_treatment2, mysumm))
df_descriptive_theory_t2 %>% rename_with(.fn = ~ paste0("Treatment2.", .x)) -> df_descriptive_theory_t2
df_descriptive_theory_t3 = do.call(data.frame, aggregate(Total.Score~Generation, df_treatment3, mysumm))
df_descriptive_theory_t3 %>% rename_with(.fn = ~ paste0("Treatment3.", .x)) -> df_descriptive_theory_t3

df_descriptive_theory=cbind(df_descriptive_theory_all, df_descriptive_theory_t1, df_descriptive_theory_t2, df_descriptive_theory_t3)

df_descriptive_theory = df_descriptive_theory[c('Generation', 'Total.Score.Mean', 'Treatment1.Total.Score.Mean', 'Treatment2.Total.Score.Mean', 
                                           'Treatment3.Total.Score.Mean', 'Total.Score.SE',
                                           'Treatment1.Total.Score.SE', 'Treatment2.Total.Score.SE', 'Treatment3.Total.Score.SE')]
df_descriptive_theory[order(df_descriptive_theory$Generation), ]
write.csv(df_descriptive_theory, "./tables/descriptivestats_generation_vs_theory.csv", row.names=FALSE)


fig_3d <- ggplot(df_descriptive_theory, aes(x=Generation, y=Total.Score.Mean)) +  
  geom_line(aes(x=Generation-0.05, y = Treatment1.Total.Score.Mean ,color='Condition 1'), linewidth=0.3) + 
  geom_errorbar(aes(x=Generation-0.05, ymin=Treatment1.Total.Score.Mean-Treatment1.Total.Score.SE, ymax=Treatment1.Total.Score.Mean+Treatment1.Total.Score.SE,  width=.1), col = "blue" )+
  geom_line(aes(x=Generation+0.05, y = Treatment2.Total.Score.Mean, col="Condition 2"), linewidth=0.3) + 
  geom_errorbar(aes(Generation+0.05, ymin=Treatment2.Total.Score.Mean-Treatment2.Total.Score.SE, ymax=Treatment2.Total.Score.Mean+Treatment2.Total.Score.SE,  width=.1), col = "darkgreen" )+
  geom_line(aes(x=Generation+0.15, y = Treatment3.Total.Score.Mean, col="Condition 3"), linewidth=0.3) + 
  geom_errorbar(aes(Generation+0.15, ymin=Treatment3.Total.Score.Mean-Treatment3.Total.Score.SE, ymax=Treatment3.Total.Score.Mean+Treatment3.Total.Score.SE,  width=.1), col = "darkred" )+
  geom_line(aes(x=Generation-0.15, y = Total.Score.Mean, color='All conditions'), linewidth=0.8) + 
  geom_errorbar(aes(x=Generation-0.15, ymin=Total.Score.Mean-Total.Score.SE, ymax=Total.Score.Mean+Total.Score.SE,  width=.3), col = "black" )+
  ylim(0, 3)+
  scale_color_manual(name='', breaks=c('All conditions', 'Condition 1', 'Condition 2', 'Condition 3'),
                     values=c('All conditions'='black', 'Condition 1'='darkblue', 'Condition 2'='darkgreen', 'Condition 3'='darkred')) +
  labs(x='Generation', y='Theory score') +
  theme_classic()+
  theme(panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(colour = "grey"),
        panel.background = element_rect(fill = NA),
        legend.position="bottom") 

ggsave("./figures/fig_3d.png", plot=fig_3d, units="in", width=5, height=4, dpi=300)

##############
### FIG 3b ###
##############

## FIGURE - SPEED VS TRIAL
df_descriptive_trial_speed<-melt(df[,c("Generation", "Trial1.Speed", "Trial2.Speed", "Trial3.Speed","Trial4.Speed","Trial5.Speed")],id=c("Generation"))
df_descriptive_trial_speed["Try"]=as.numeric(substr(as.character(df_descriptive_trial_speed$variable), 6, 6))
df_descriptive_trial_speed=df_descriptive_trial_speed[,-c(2)]
df_descriptive_trial_speed=do.call(data.frame,aggregate(df_descriptive_trial_speed,list(df_descriptive_trial_speed$Generation,df_descriptive_trial_speed$Try), mysumm_noNA))

df_descriptive_trial_speed_t1<-melt(df_treatment1[,c("Generation", "Trial1.Speed", "Trial2.Speed", "Trial3.Speed","Trial4.Speed","Trial5.Speed")],id=c("Generation"))
df_descriptive_trial_speed_t1["Try"]=as.numeric(substr(as.character(df_descriptive_trial_speed_t1$variable), 6, 6))
df_descriptive_trial_speed_t1=df_descriptive_trial_speed_t1[,-c(2)]
df_descriptive_trial_speed_t1=do.call(data.frame,aggregate(df_descriptive_trial_speed_t1,list(df_descriptive_trial_speed_t1$Generation,df_descriptive_trial_speed_t1$Try), mysumm_noNA))
df_descriptive_trial_speed_t1 %>% rename_with(.fn = ~ paste0("Treatment1.", .x)) -> df_descriptive_trial_speed_t1

df_descriptive_trial_speed_t2<-melt(df_treatment2[,c("Generation", "Trial1.Speed", "Trial2.Speed", "Trial3.Speed","Trial4.Speed","Trial5.Speed")],id=c("Generation"))
df_descriptive_trial_speed_t2["Try"]=as.numeric(substr(as.character(df_descriptive_trial_speed_t2$variable), 6, 6))
df_descriptive_trial_speed_t2=df_descriptive_trial_speed_t2[,-c(2)]
df_descriptive_trial_speed_t2=do.call(data.frame,aggregate(df_descriptive_trial_speed_t2,list(df_descriptive_trial_speed_t2$Generation,df_descriptive_trial_speed_t2$Try), mysumm_noNA))
df_descriptive_trial_speed_t2 %>% rename_with(.fn = ~ paste0("Treatment2.", .x)) -> df_descriptive_trial_speed_t2

df_descriptive_trial_speed_t3<-melt(df_treatment3[,c("Generation", "Trial1.Speed", "Trial2.Speed", "Trial3.Speed","Trial4.Speed","Trial5.Speed")],id=c("Generation"))
df_descriptive_trial_speed_t3["Try"]=as.numeric(substr(as.character(df_descriptive_trial_speed_t3$variable), 6, 6))
df_descriptive_trial_speed_t3=df_descriptive_trial_speed_t3[,-c(2)]
df_descriptive_trial_speed_t3=do.call(data.frame,aggregate(df_descriptive_trial_speed_t3,list(df_descriptive_trial_speed_t3$Generation,df_descriptive_trial_speed_t3$Try), mysumm_noNA))
df_descriptive_trial_speed_t3 %>% rename_with(.fn = ~ paste0("Treatment3.", .x)) -> df_descriptive_trial_speed_t3

df_descriptive_trial_speed["Trial"]=(df_descriptive_trial_speed$Generation.Mean-1)*5+df_descriptive_trial_speed$Try.Mean
df_descriptive_trial_speed=cbind(df_descriptive_trial_speed, df_descriptive_trial_speed_t1, df_descriptive_trial_speed_t2, df_descriptive_trial_speed_t3)

df_descriptive_trial_speed = df_descriptive_trial_speed[c('Trial', 'value.Mean', 'Treatment1.value.Mean', 'Treatment2.value.Mean', 'Treatment3.value.Mean', 'Treatment1.value.SE', 'Treatment2.value.SE', 'Treatment3.value.SE')]
df_descriptive_trial_speed[order(df_descriptive_trial_speed$Trial), ]
write.csv(df_descriptive_trial_speed, "./tables/descriptivestats_trial_vs_speed.csv", row.names=FALSE)

fig3b <- ggplot(df_descriptive_trial_speed, aes(x = Trial)) + 
  geom_point(size=1, aes(y=Treatment1.value.Mean, color = "Condition 1")) +
  geom_line(aes(x=Trial, y = Treatment1.value.Mean, color="Condition 1"), size=0.3) + 
  geom_errorbar(aes(ymin=Treatment1.value.Mean-Treatment1.value.SE, ymax=Treatment1.value.Mean+Treatment1.value.SE,  width=.1, color = "Treatment 1")) +
  geom_point(size=1, aes(y=Treatment2.value.Mean, color = "Condition 2")) +
  geom_line(aes(x=Trial, y = Treatment2.value.Mean, color="Condition 2"), size=0.3) + 
  geom_errorbar(aes(ymin=Treatment2.value.Mean-Treatment2.value.SE, ymax=Treatment2.value.Mean+Treatment2.value.SE,  width=.1, color = "Treatment 2")) +
  geom_point(size=1, aes(y=Treatment3.value.Mean, color = "Condition 3")) +
  geom_line(aes(x=Trial, y = Treatment3.value.Mean, color="Condition 3"), size=0.3) + 
  geom_vline(xintercept=c(5,10,15,20,25))+
  geom_errorbar(aes(ymin=Treatment3.value.Mean-Treatment3.value.SE, ymax=Treatment3.value.Mean+Treatment3.value.SE,  width=.1, color = "Treatment 3")) +
  theme_classic()+
  geom_hline(yintercept=212, linetype='dashed', color="gray", size=0.3) + 
  geom_text(aes(x = 23, y = 209,
                label = "Theoretical\nmax speed"),
            lineheight = 0.9,
            stat = "unique",
            size = 3, color = "darkgray") +
  labs(x='Trial number', y='Wheel speed') +
  scale_color_manual(name='', breaks=c('', 'All', 'Condition 1', 'Condition 2', 'Condition 3'),
                     values=c('All'='black', 'Condition 1'='darkblue', 'Condition 2'='darkgreen', 'Condition 3'='darkred')) +
  scale_y_continuous(breaks = seq(150,210,by = 20)) +
  scale_x_continuous(breaks = seq(0,25,by = 5)) +
  theme(panel.grid.major = element_line(colour = NA),
        panel.background = element_rect(fill = NA),
        legend.position="bottom")

ggsave("./figures/speed_trials_treatment.png", plot=fig3b, units="in", width=8, height=4, dpi=300)

##############
### FIG 4a ###
##############

model4a <- stan_glm(Test.Score ~ maxspeed, data = df, iter=40000, seed=23)
model4a$coefficients[2]
model_performance(model4a)
m2bf <- bayesfactor(model4a)
m2post <- describe_posterior(model4a, test = c("pd", "ROPE", "BF"), ci=0.95, rope_ci=0.95)


plot4a <- ggplot(df, aes(x = maxspeed, y = Test.Score)) + 
  geom_point(size = 2, col = "orange") +
  scale_size_continuous(range = c(1, 6), guide = 'none') +
  geom_abline(intercept = model4a$coefficients[1], slope = model4a$coefficients[2])+ 
  theme_classic()+
  theme(panel.grid.major = element_line(colour = NA),
        panel.background = element_rect(fill = NA)) +
  labs(y='Knowledge-test score', x='Maximum speed') +
  scale_x_continuous(breaks=c(160,180,200,220)) +
  xlim(150,225)+
  theme(aspect.ratio=1)

##############
### FIG 4b ###
##############

model4b <- stan_glm(Total.Score ~ maxspeed, data = df, iter=40000, seed=23)
model4b$coefficients[2]
model_performance(model4b)
m3bf <- bayesfactor(model4b)
m3post <- describe_posterior(model4b, test = c("pd", "ROPE", "BF"), ci=0.95, rope_ci=0.95)


plot4b <- ggplot(df, aes(x = maxspeed, y = Total.Score)) + 
  geom_point(size = 2, col = "orange") +
  scale_size_continuous(range = c(1, 6), guide = 'none') +
  geom_abline(intercept = model4b$coefficients[1], slope = model4b$coefficients[2])+ 
  theme_classic()+
  theme(panel.grid.major = element_line(colour = NA),
        panel.background = element_rect(fill = NA)) +
  labs(y='Theory-quality score', x='Maximum speed') +
  scale_x_continuous(breaks=c(160,180,200,220)) +
  xlim(150,225) +
  theme(aspect.ratio=1)

##############
### FIG 4c ###
##############

model4c <- stan_glm(Total.Score ~ Test.Score, data = df, iter=40000, seed=23)
model4c$coefficients[2]
model_performance(model4c)
m4bf <- bayesfactor(model4c)
m4post <- describe_posterior(model4c, test = c("pd", "ROPE", "BF"), ci=0.95, rope_ci=0.95)

plot4c <- ggplot(df, aes(x = Test.Score, y = Total.Score)) + 
  geom_point(size = 2, col = "orange") +
  scale_size_continuous(range = c(1, 6), guide = 'none') +
  geom_count(col='orange') +
  scale_size_area() +
  geom_abline(intercept = model4c$coefficients[1], slope = model4c$coefficients[2])+ 
  theme_classic()+
  theme(panel.grid.major = element_line(colour = NA),
        panel.background = element_rect(fill = NA)) +
  labs(y='Theory-quality score', x='Knowledge test score') 
  
#####################
### SAVE FIGURE 4 ###
#####################

ggsave("./figures/4a.png", plot=plot4a, units="in", width=4, height=4, dpi=300)
ggsave("./figures/4a.svg", device='svg', plot=plot4a, units="in", width=4, height=4, dpi=300)

ggsave("./figures/4b.png", plot=plot4b, units="in", width=4, height=4, dpi=300)
ggsave("./figures/4b.svg", device='svg', plot=plot4b, units="in", width=4, height=4, dpi=300)

ggsave("./figures/4c.png", plot=plot4c, units="in", width=4, height=4, dpi=300)
ggsave("./figures/4c.svg", device='svg', plot=plot4c, units="in", width=4, height=4, dpi=300)

