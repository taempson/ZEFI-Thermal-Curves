library(tidyverse)
library(MASS)
library(lme4)

Tamara.data.2025.02.03 <- read.csv("~/GitHub/ZEFI-Thermal-Curves/code/Empson/Tamara data 2025-02-03.csv")

# Tamara.data.2025.02.03 %>% 
#   ggplot(aes(x=as.factor(temp_target), y=total_songs, fill=as.factor(temp_target)))+
#   geom_boxplot()+
#   geom_point(position = position_jitterdodge(dodge.width = 1))+
#   theme_light()

Tamara.data.2025.02.03 %>% 
  filter(round!=3) %>% 
  ggplot(aes(x=as.factor(temp_target), y=total_songs, fill=as.factor(temp_target)))+
  geom_boxplot()+
  geom_point(position = position_jitterdodge(dodge.width = 1))+
  theme_light()

# Tamara.data.2025.02.03 %>% 
#   ggplot(aes(x=temp_mean, y=total_songs))+
#   geom_point()+
#   theme_light()+
#   geom_smooth()

Tamara.data.2025.02.03 %>% 
  filter(round!=3) %>% 
  ggplot(aes(x=as.factor(temp_target), y=total_songs, col=male))+
  geom_point()+
  geom_line(aes(group=male))+
  theme_light()+
  facet_wrap(~male, ncol = 3)+
  gghighlight::gghighlight(use_direct_label = FALSE)

Tamara.data.2025.02.03 %>% 
  filter(round!=3) %>% 
  ggplot(aes(x=temp_mean, y=total_songs, col=male))+
  geom_point()+
  geom_line(aes(group=male))+
  theme_light()+
  facet_wrap(~male, ncol = 3)+
  gghighlight::gghighlight(use_direct_label = FALSE)

Tamara.data.2025.02.03 %>% 
  filter(round!=3) %>% 
  ggplot(aes(x=temp_mean, y=total_songs))+
  geom_point()+
  theme_light()+
  geom_smooth()

filtered.data<- Tamara.data.2025.02.03 %>% filter(round!=3)

resid(glm(filtered.data$total_songs~filtered.data$temp_mean, family=poisson)) %>% shapiro.test();hist(resid(glm(filtered.data$total_songs~filtered.data$temp_mean, family=poisson)))
glm(filtered.data$total_songs~filtered.data$temp_mean, family=poisson) %>% car::Anova()
summary(glm(filtered.data$total_songs~filtered.data$temp_mean, family=poisson))
#Residual deviance: 2429.4  on 123  degrees of freedom
#2429.4>>>123 deviance is way too big. overdispersed. 

summary(glm(filtered.data$total_songs~filtered.data$temp_mean, family=quasipoisson))

summary(glm.nb(total_songs ~ temp_mean, data = filtered.data))

summary(glm.nb(total_songs ~ temp_mean + male, data = filtered.data))

summary(glmer.nb(total_songs ~ temp_mean + (1|male)+ (1|chamber), data = filtered.data))
# AIC      BIC   logLik deviance df.resid 
# 1017.3   1031.5   -503.7   1007.3      120 
pchisq(1007.3,120 ,lower.tail = F)
# 4.028503e-140

filtered.data %>% 
  ggplot(aes(x=temp_mean, y=total_songs))+
  geom_point(aes(col=as.factor(temp_target)))+
  theme_light()+
  geom_smooth()
