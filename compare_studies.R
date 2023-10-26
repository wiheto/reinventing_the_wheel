# Data that makes the figure that compares studies.
# Data for the other studies from Osiurak OSF page. 
library(xlsx)
library(ggplot2)
library(gtools)
library(gridExtra)

# Load data
setwd("./")
wb <- "./data/data_combined.xlsx"
df  <- read.xlsx(wb, sheetName  = "data")

# Data obtained from Osiurak OSF page. 
wb <- "./data/2020_Osiurak_NHB_data.xlsx"
derex <- read.xlsx(wb, sheetName =  "DEREX ET AL. (2019)")
osi_exp1  <- read.xlsx(wb, sheetName  = "EXPERIMENT_1")
osi_exp2  <- read.xlsx(wb, sheetName  = "EXPERIMENT_2")
derex_wheel <- read.xlsx(wb, sheetName ="DEREX_WHEEL")


derex["Participant"]=seq(1:nrow(derex))
derex$Chain <- as.factor(derex$Chain)
derex$Participant <- as.factor(derex$Participant)
derex$Treatment <- as.factor(derex$Treatment)
derex$Treatment <- as.numeric(derex$Treatment) - 1
derex_valid_speed<-subset(derex, Speed >0)

## Conversion Chain & Participant - EXPERIMENT 1
osi_exp1$Chain <- as.factor(osi_exp1$Chain)
osi_exp1$Group <- as.factor(osi_exp1$Group)
osi_exp1["Participant"]=seq(1:nrow(osi_exp1))
osi_exp1$Participant <- as.factor(osi_exp1$Participant)
osi_exp1["Generation"]= na.replace(osi_exp1$Generation, "Control")
osi_exp1_exp=subset(osi_exp1,Group=="Experimental" )
osi_exp1_exp$Generation=as.numeric(osi_exp1_exp$Generation)
osi_exp1_cont=subset(osi_exp1,Group=="Control" )

# Reshape derex_wheel into derex
# Create a new data frame with the desired format
new_df <- data.frame(matrix(nrow = nrow(derex), ncol = 5))
# Loop through each row in the new data frame
for (i in 1:nrow(new_df)) {
  
  # Loop through each trial in the row
  for (j in 1:5) {
    
    # Calculate the index of the speed value in the original data frame
    index <- (i - 1) * 5 + j
    
    # Fill the value in the new data frame
    new_df[i, j] <- derex_wheel$Speed[index]
  }
}

# Rename the columns in the new data frame
colnames(new_df) <- paste0("Wheel.Speed.T", 1:5)
derex <- cbind(derex, new_df)
# replace 0 speed with na
df$Trial1.Speed[df$Trial1.Speed == 0] <- NA
df$Trial2.Speed[df$Trial2.Speed == 0] <- NA
df$Trial3.Speed[df$Trial3.Speed == 0] <- NA
df$Trial4.Speed[df$Trial4.Speed == 0] <- NA
df$Trial5.Speed[df$Trial5.Speed == 0] <- NA

df_treatment1 <- subset(df, Treatment==1)
df_treatment2 <- subset(df, Treatment==2)
df_treatment3 <- subset(df, Treatment==3)


df_speed<-melt(df[,c("Generation", "Trial1.Speed", "Trial2.Speed", "Trial3.Speed","Trial4.Speed","Trial5.Speed")],id=c("Generation"))
df_speed["Try"]=as.numeric(substr(as.character(df_speed$variable), 6, 6))
df_speed=df_speed[,-c(2)]
df_speed=do.call(data.frame,aggregate(df_speed,list(df_speed$Generation,df_speed$Try), mysumm_noNA))
df_speed["Trial"]=(df_speed$Generation.Mean-1)*5+df_speed$Try.Mean


df_speed_osi<-melt(osi_exp1_exp[,c("Generation", "Wheel.Speed.T1", "Wheel.Speed.T2", "Wheel.Speed.T3","Wheel.Speed.T4","Wheel.Speed.T5")],id=c("Generation"))
df_speed_osi[df_speed_osi==0]=NA
df_speed_osi["Try"]=as.numeric(substr(as.character(df_speed_osi$variable), nchar(as.character(df_speed_osi$variable)), nchar(as.character(df_speed_osi$variable))))
df_speed_osi=df_speed_osi[,-c(2)]
df_speed_osi=do.call(data.frame,aggregate(df_speed_osi,list(df_speed_osi$Generation,df_speed_osi$Try), mysumm_noNA))
df_speed_osi["Trial"]=(df_speed_osi$Generation.Mean-1)*5+df_speed_osi$Try.Mean

df_speed_derex<-melt(derex[,c("Generation", "Wheel.Speed.T1", "Wheel.Speed.T2", "Wheel.Speed.T3","Wheel.Speed.T4","Wheel.Speed.T5")],id=c("Generation"))
df_speed_derex[df_speed_derex==0]=NA
df_speed_derex["Try"]=as.numeric(substr(as.character(df_speed_derex$variable), nchar(as.character(df_speed_derex$variable)), nchar(as.character(df_speed_derex$variable))))
df_speed_derex=df_speed_derex[,-c(2)]
df_speed_derex=do.call(data.frame,aggregate(df_speed_derex,list(df_speed_derex$Generation,df_speed_derex$Try), mysumm_noNA))
df_speed_derex["Trial"]=(df_speed_derex$Generation.Mean-1)*5+df_speed_derex$Try.Mean





a<-ggplot(df_speed, aes(x = Trial)) + 
  geom_point(size=1, aes(y = value.Mean), col='black') +
  geom_line(aes(x=Trial, y = value.Mean), col='black') + 
  geom_vline(xintercept=c(5,10,15,20,25))+
  geom_errorbar(aes(ymin=value.Mean-value.SE, ymax=value.Mean+value.SE,  width=.2), col = "black") +
  theme_classic()+
  labs(x='Trial number', y='Wheel speed') +
  scale_y_continuous(breaks = seq(150,210,by = 20)) +
  geom_hline(yintercept=212, linetype='dashed', color="gray", size=0.3) + 
  geom_text(aes(x = 22, y = 210,
                label = "Theoretical max speed"),
            lineheight = 0.9,
            stat = "unique",
            size = 2, color = "darkgray") +
  theme(panel.grid.major = element_line(colour = NA),
        panel.background = element_rect(fill = NA)) + ggtitle("Our experiment")

b<-ggplot(df_speed_derex, aes(x = Trial)) + 
  geom_point(size=1, aes(y = value.Mean), col='red') +
  geom_line(aes(x=Trial, y = value.Mean), col='red') + 
  geom_vline(xintercept=c(5,10,15,20,25))+
  geom_errorbar(aes(ymin=value.Mean-value.SE, ymax=value.Mean+value.SE,  width=.2), col = "red") +
  theme_classic()+
  labs(x='Trial number', y='Wheel speed') +
  scale_y_continuous(breaks = seq(120,156,by = 10), limits=c(120,156)) +  
  geom_hline(yintercept=154, linetype='dashed', color="gray", size=0.3) + 
  geom_text(aes(x = 22, y = 152,
                label = "Theoretical max speed"),
            lineheight = 0.9,
            stat = "unique",
            size = 2, color = "darkgray") +
  
  theme(panel.grid.major = element_line(colour = NA),
        panel.background = element_rect(fill = NA)) + ggtitle("Derex et al.") 

c<-ggplot(df_speed_osi, aes(x = Trial)) + 
  geom_point(size=1, aes(y = value.Mean), col='blue') +
  geom_line(aes(x=Trial, y = value.Mean), col='blue') + 
  geom_vline(xintercept=c(5,10,15,20,25))+
  geom_errorbar(aes(ymin=value.Mean-value.SE, ymax=value.Mean+value.SE,  width=.2), col = "blue") +
  theme_classic()+
  scale_y_continuous(breaks = seq(190,240,by = 20), limits=c(190,240)) +
  geom_hline(yintercept=235, linetype='dashed', color="gray", size=0.3) + 
  geom_text(aes(x = 22, y = 232,
                label = "Theoretical max speed"),
            lineheight = 0.9,
            stat = "unique",
            size = 2, color = "darkgray") +
  labs(x='Trial number', y='Wheel speed') +
  theme(panel.grid.major = element_line(colour = NA),
        panel.background = element_rect(fill = NA)) + ggtitle("Osiurak et al.") 


trial_speed_allstudies <- grid.arrange(a, b, c,  nrow=3, ncol=1)
ggsave("./figures/fig3a.png", plot=trial_speed_allstudies, units="in", width=6, height=5, dpi=300)
ggsave("./figures/fig3a.svg", device='svg', plot=plot_4a_alt, units="in", width=4, height=4, dpi=300)





