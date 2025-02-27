# PERMUTATIONAL REPEATED MEASURES ANOVA - EXP. TIDE x SHALLOW x DEEP
# 2 between-subjetcs factors: origin site (local) , treatment (trat)
# 1 within-subjects factor: time (tempo)

setwd("C:/Users/olive/Documents/Mestrado/Manuscrito/Paper/Dados")

#call packages
library(lmPerm) 
library(rstatix)
library(tidyverse)
library(ggpubr)


pam <- read.csv2("PAM.csv", dec = ".") #open the data 
summary(pam) #general data info
View(pam) #view data

anova2 <- aovp(YII ~ local*trat*tempo + Error(ID/tempo), data=pam) #use aovp function to run a permutational RM ANOVA, because data is not normally distributed 
get_anova_table(anova2) #get results
summary(anova2) #get results

#GRAPH 
dados <- pam %>% 
  unite(data, c(ano, mes, dia), sep="-") %>%
  unite(codigo, c(trat,local), remove=FALSE) %>%
  #separate(tempo, into = c("T", "time"), sep=1, convert=TRUE) %>% 
  select(codigo, trat, local, YIIdec, dia_exp, temp)

pam_ing <- ggplot(dados, aes(x=dia_exp, y=YIIdec, color=trat, shape=local, group=codigo))+
  stat_summary(fun="mean", geom="line", aes(color=trat), size=1, alpha=0.6, linetype=2)+
  stat_summary(geom="errorbar", aes(color=trat), size=0.5, alpha=0.8)+
  stat_summary(aes(color=trat), size=0.8)+
  xlab("Days of experiment")+
  ylab("Photosyntethic efficiency (YII    SE)")+
  theme_classic(base_size = 11.5)+
  theme(legend.position="bottom")+
  scale_color_manual(values=c("#D53000", "#004E4D"), name="Treatment",
                     breaks =c("A", "C"),
                     labels=c("Heated", "Control"))+
  scale_shape_discrete(name="Origin site",
                       breaks=c("P","R","F"),
                       labels=c("Tide pool", "Shallow reef", "Deep reef"))
pam_ing

ggsave("pam_ing.jpg",plot= pam_ing, width = 6.85, height = 5 )


# 2 axis graph (PAM + temperature)
pam_temp <- ggplot(dados, aes(x=dia_exp, y=YIIdec, group=codigo))+
  geom_col(aes(y=temp * 1/32, fill=temp), width=1)+
  stat_summary(fun="mean", geom="line", aes(color=local, linetype=trat), size=1, alpha=0.6)+
  stat_summary(geom="errorbar", aes(color=local), size=0.5, alpha=0.8)+
  stat_summary(aes(color=local), size=0.6)+
  scale_y_continuous(limits = c(0, 1), sec.axis = sec_axis(trans = ~ . * (32/1), name = "Temperature (°C)"))+
  xlab("Days of experiment")+
  ylab("Photochemical efficiency (YII    SE)")+
  theme_classic(base_size = 12)+
  theme(legend.position="bottom")+
  scale_color_manual(values=c("#012B2B", "#009898", "#9DF7F7"), name="Origin",
                     breaks =c("F", "R", "P"),
                   labels=c("Deep", "Shallow", "Tide"))+
  scale_linetype_discrete(name="Treatment",
                       breaks=c("A","C"),
                       labels=c("Heated", "Control"))+
  scale_fill_gradient(low="#ffcc9e", high="#f86742", name="Temperature")

pam_temp

View(dados)

ggsave("pam_temp.jpg",plot= pam_temp, width = 6.85, height = 5 )
