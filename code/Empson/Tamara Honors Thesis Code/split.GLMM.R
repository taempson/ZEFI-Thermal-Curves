library(lme4)
library(car)
library(segmented)

dat <- `2025.11.25_count_summaries`

# NUll model####
null.mod <- lmer(log(total_songs + 1) ~ 1 + (1 | male), data = dat)
AIC(null.mod) #AIC= 445.2552

#treat as categorical
dat$temp_target <- as.factor(dat$temp_target)

# Fit GLMM ########
#this is the one in my poster
model.total.song <- lmer(log(total_songs + 1) ~ temp_mean + (1 | male), data = dat)
summary(model.total.song)
Anova(model.total.song) #0.04916
AIC(model.total.song) #447.6923


#finding total songs split
mod<-glm(total_songs~temp_mean, data = dat)
summary(segmented::segmented(mod,seg.Z=~temp_mean)) #41.484

dat$split_group <- ifelse(dat$temp_mean <= 41.484, "below41", "above41")

#below 41
mod.below41 <- lmer(log(total_songs+1) ~ temp_mean + (1 | male),
                    data = subset(dat, split_group == "below41"))
summary(mod.below41)
Anova(mod.below41) #p=0.7965
AIC(mod.below41) #AIC=335.8973

#above 41
mod.above41 <- lmer(log(total_songs +1) ~ temp_mean + (1 | male),
                    data = subset(dat, split_group == "above41"))
summary(mod.above41)
Anova(mod.above41) #p=0.03215
AIC(mod.above41) #AIC=122.4153

#graph-making ########
library(dplyr)

ds <- dat %>%
  mutate(break.point= case_when(temp_mean<41.484~"Below",.default = "Above"))

library(ggplot2)

ds %>%
  ggplot(aes(x = temp_mean, y = log(total_songs+1), colour = break.point)) +
  geom_point(size = 3) +                        # bigger points
  geom_smooth(method = "lm", size = 2) +        # thicker smooth line
  theme_light(base_size = 20) +                 # enlarge base text
  geom_vline(xintercept = 41.484, linetype = 5, alpha = .5, size = 1.5) +  # thicker vertical line
  scale_colour_manual(values = c("brown1", "cornflowerblue")) +
  labs(x = "Mean Temperature", y = "log (Total Songs + 1)") +
  annotate("text", x = 41.484,
           y = mean(log(ds$total_songs+1), na.rm = TRUE),
           label = "41.48",
           size = 7,                    # annotation text ~20 pt
           angle = 360, vjust = -9, hjust = -0.2) +
  theme(
    legend.position = "none",
    text = element_text(size = 20),     # all text set to 20 pt
    axis.title = element_text(size = 20, face = "bold"),  # bold axis titles if desired
    axis.text = element_text(size = 20) # tick labels enlarged
  )

#Checking other models AIC
#GLMM no log trans ########
AIC(lmer(total_songs ~ temp_mean + (1 | male),data =dat))#1374.783
#below 41
nolog.mod.below41 <- lmer(total_songs ~ temp_mean + (1 | male),
                          data = subset(dat, split_group == "below41"))
summary(nolog.mod.below41) 
Anova(nolog.mod.below41) #p=0.9484
AIC(nolog.mod.below41) #AIC=1087.268

#above 41
nolog.mod.above41 <- lmer(total_songs ~ temp_mean + (1 | male),
                          data = subset(dat, split_group == "above41"))
summary(nolog.mod.above41)
Anova(nolog.mod.above41) #p=0.2408
AIC(nolog.mod.above41) #AIC=287.4851

#Checking other models AIC
#POISSON GLMM no log trans########
AIC(glmer(total_songs ~ temp_mean + (1 | male),data =dat, family=poisson)) #2359.878

#below 41
p.nolog.mod.below41 <- glmer(total_songs ~ temp_mean + (1 | male),data = subset(dat, split_group == "below41"), family=poisson)
summary(p.nolog.mod.below41) 
Anova(p.nolog.mod.below41) #p=0.6202
AIC(p.nolog.mod.below41) #AIC=1861.513

#above 41
p.nolog.mod.above41 <- glmer(total_songs ~ temp_mean + (1 | male),data = subset(dat, split_group == "above41"), family=poisson)
summary(p.nolog.mod.above41)
Anova(p.nolog.mod.above41) #p=3.026e-05***
AIC(p.nolog.mod.above41) #AIC=376.3461

#POISSON GLMM log trans########
AIC(glmer(log(total_songs+1) ~ temp_mean + (1 | male),data =dat, family=poisson)) #Inf?

#below 41
p.log.mod.below41 <- glmer(log(total_songs+1) ~ temp_mean + (1 | male),data = subset(dat, split_group == "below41"), family=poisson)
summary(p.log.mod.below41) 
Anova(p.log.mod.below41) #p=0.9078
AIC(p.log.mod.below41) #AIC=Inf

#above 41
p.log.mod.above41 <- glmer(log(total_songs+1) ~ temp_mean + (1 | male),data = subset(dat, split_group == "above41"), family=poisson)
summary(p.log.mod.above41)
Anova(p.log.mod.above41) #p=0.1352
AIC(p.log.mod.above41) #AIC=Inf


#GLM.NB ########
AIC(glmer.nb(total_songs ~ temp_mean + (1 | male),data =dat))
# below 41
nb.mod.below41 <- glmer.nb(total_songs ~ temp_mean + (1 | male),
                           data = subset(dat, split_group == "below41"))
summary(nb.mod.below41) #AIC=994.5
Anova(nb.mod.below41) #p=0.654

# above 41
nb.mod.above41 <- glmer.nb(total_songs ~ temp_mean + (1 | male),
                           data = subset(dat, split_group == "above41"))
summary(nb.mod.above41) #AIC=271.2
Anova(nb.mod.above41) #p=0.5161

#GLM.NB w/ log transformation ########

AIC(glmer.nb(log(total_songs+1) ~ temp_mean + (1 | male),data =dat))
# below 41
lognb.mod.below41 <- glmer.nb(log(total_songs+1) ~ temp_mean + (1 | male),
                              data = subset(dat, split_group == "below41"))
summary(lognb.mod.below41) #AIC=414.2
Anova(lognb.mod.below41) #p=0.8665

# above 41
lognb.mod.above41 <- glmer.nb(log(total_songs+1) ~ temp_mean + (1 | male),
                              data = subset(dat, split_group == "above41"))
summary(lognb.mod.above41) #AIC=128.5
Anova(lognb.mod.above41) #p=0.0002099



#finding total motif split ########
mod.motif<-glm(total_motifs~temp_mean, data = dat)
summary(segmented::segmented(mod.motif,seg.Z=~temp_mean)) #38.704

dat$split_group <- ifelse(dat$temp_mean <= 38.704, "below38", "above38")

#below 38
mod.below38 <- lmer(log(total_motifs+1) ~ temp_mean + (1 | male),
                    data = subset(dat, split_group == "below38"))
summary(mod.below38) 
Anova(mod.below38) #p=0.96
AIC(mod.below38) #AIC=238.568

#above 38
mod.above38 <- lmer(log(total_motifs +1) ~ temp_mean + (1 | male),
                    data = subset(dat, split_group == "above38"))
summary(mod.above38)
Anova(mod.above38) #p=0.002369
AIC(mod.above38) #AIC=301.4167

#graph-making ########
ds.motif <- dat %>% 
  mutate(break.point= case_when(temp_mean<38.704~"Below",.default = "Above"))

ds.motif %>% 
  ggplot(aes(x = temp_mean, y = log(total_motifs+1), colour = break.point)) +
  geom_point(size = 3) +                        # bigger points
  geom_smooth(method = "lm", size = 2) +        # thicker smooth line
  theme_light(base_size = 20) +                 # enlarge base text
  geom_vline(xintercept = 38.704, linetype = 5, alpha = .5, size = 1.5) +  # thicker vertical line
  scale_colour_manual(values = c("brown1", "cornflowerblue")) +
  labs(x = "Mean Temperature", y = "log (Total Motifs + 1)") +
  annotate("text", x = 38.704, 
           y = mean(log(ds$total_motifs+1), na.rm = TRUE), 
           label = "38.70", 
           size = 7,                    # annotation text ~20 pt
           angle = 360, vjust = 7, hjust = -0.2) +
  theme(
    legend.position = "none",
    text = element_text(size = 20),     # all text set to 20 pt
    axis.title = element_text(size = 20, face = "bold"),  # bold axis titles if desired
    axis.text = element_text(size = 20) # tick labels enlarged
  )


#Checking other models AIC
#GLMM no log trans Motif ########
#below 38
nolog.mod.below38 <- lmer(total_motifs ~ temp_mean + (1 | male),
                          data = subset(dat, split_group == "below38"))
summary(nolog.mod.below38) 
Anova(nolog.mod.below38) #p=0.6404
AIC(nolog.mod.below38) #AIC=784.2741

#above 38
nolog.mod.above38 <- lmer(total_motifs ~ temp_mean + (1 | male),
                          data = subset(dat, split_group == "above38"))
summary(nolog.mod.above38)
Anova(nolog.mod.above38) #p=0.1086
AIC(nolog.mod.above38) #AIC=945.9149

#GLM.NB ########
# below 38
nb.mod.below38 <- glmer.nb(total_motifs ~ temp_mean + (1 | male),
                           data = subset(dat, split_group == "below38"))
summary(nb.mod.below38) #AIC=738.5
Anova(nb.mod.below38) #p=0.7355

# above 38
nb.mod.above38 <- glmer.nb(total_motifs ~ temp_mean + (1 | male),
                           data = subset(dat, split_group == "above38"))
summary(nb.mod.above38) #AIC=911.3
Anova(nb.mod.above38) #p=0.04934

#GLM.NB w/ log transformation ########
# below 38
lognb.mod.below38 <- glmer.nb(log(total_motifs+1) ~ temp_mean + (1 | male),
                              data = subset(dat, split_group == "below38"))
summary(lognb.mod.below38) #AIC=276.5
Anova(lognb.mod.below38) #p=0.9195

# above 38
lognb.mod.above38 <- glmer.nb(log(total_motifs+1) ~ temp_mean + (1 | male),
                              data = subset(dat, split_group == "above38"))
summary(lognb.mod.above38) #AIC=341.5
Anova(lognb.mod.above38) #p=0.005034





#finding avg duration split ########
mod.duration<-glm(average_duration~temp_mean, data = dat)
summary(segmented::segmented(mod.duration,seg.Z=~temp_mean)) #42.487

dat$split_group <- ifelse(dat$temp_mean <= 42.487, "below42", "above42")

#below 42
mod.below42 <- lmer(log(average_duration+1) ~ temp_mean + (1 | male),
                    data = subset(dat, split_group == "below42"))
summary(mod.below42) 
Anova(mod.below42) #p=0.7018
AIC(mod.below42) #AIC=117.6857

#above 42
mod.above42 <- lmer(log(average_duration +1) ~ temp_mean + (1 | male),
                    data = subset(dat, split_group == "above42"))
summary(mod.above42)
Anova(mod.above42) #p=0.111
AIC(mod.above42) #AIC=63.22885

#graph-making ########
ds.duration <- dat %>% 
  mutate(break.point= case_when(temp_mean<42.487~"Below",.default = "Above"))

ds.duration %>% ggplot(aes(x=temp_mean, y=log(average_duration+1), colour = break.point))+
  geom_point()+geom_smooth(method="lm") + theme_light() + geom_vline(xintercept = 42.487, linetype=5, alpha=.5)


#Checking other models AIC
#GLMM no log trans ########
#below 42
nolog.mod.below42 <- lmer(average_duration ~ temp_mean + (1 | male),
                          data = subset(dat, split_group == "below42"))
summary(nolog.mod.below42) 
Anova(nolog.mod.below42) #p=0.4562
AIC(nolog.mod.below42) #AIC=363.4632

#above 42
nolog.mod.above42 <- lmer(average_duration ~ temp_mean + (1 | male),
                          data = subset(dat, split_group == "above42"))
summary(nolog.mod.above42)
Anova(nolog.mod.above42) #p=0.1197
AIC(nolog.mod.above42) #AIC=108.7273

#GLM.NB ########
# below 42
nb.mod.below42 <- glmer.nb(average_duration ~ temp_mean + (1 | male),
                           data = subset(dat, split_group == "below42"))
summary(nb.mod.below42) #AIC=443.4
Anova(nb.mod.below42) #p=0.687

# above 42
nb.mod.above42 <- glmer.nb(average_duration ~ temp_mean + (1 | male),
                           data = subset(dat, split_group == "above42"))
summary(nb.mod.above42) #AIC=113.9
Anova(nb.mod.above42) #p=0.001271

#GLM.NB w/ log transformation########
# below 42
lognb.mod.below42 <- glmer.nb(log(average_duration+1) ~ temp_mean + (1 | male),
                              data = subset(dat, split_group == "below42"))
summary(lognb.mod.below42) #AIC=308.3
Anova(lognb.mod.below42) #p=0.844

# above 42
lognb.mod.above42 <- glmer.nb(log(average_duration+1) ~ temp_mean + (1 | male),
                              data = subset(dat, split_group == "above42"))
summary(lognb.mod.above42) #AIC=74.4
Anova(lognb.mod.above42) #p=0.001609


#stacked graph
library(patchwork)

# Songs plot########
p1 <- ds %>% 
  ggplot(aes(x = temp_mean, y = log(total_songs+1), colour = break.point)) +
  geom_point() +                        # points stay at default size
  geom_smooth(method = "lm") +
  theme_light(base_size = 20) +
  geom_vline(xintercept = 41.484, linetype = 5, alpha = .5) +
  scale_colour_manual(values = c("brown1", "cornflowerblue")) +
  labs(y = "Total Songs") +
  annotate("text", x = 41.484, 
           y = mean(log(ds$total_songs+1), na.rm = TRUE), 
           label = "41.48", 
           size = 6,                    # annotation text ~20 pt
           angle = 360, vjust = -10, hjust = -0.2) +
  theme(
    legend.position = "none",
    axis.title.x = element_blank(),
    text = element_text(size = 20)      # all text set to 20 pt
  )

# Motifs plot########
p2 <- ds.motif %>% 
  ggplot(aes(x = temp_mean, y = log(total_motifs+1), colour = break.point)) +
  geom_point() +                        # points stay at default size
  geom_smooth(method = "lm") +
  theme_light(base_size = 20) +
  geom_vline(xintercept = 38.704, linetype = 5, alpha = .5) +
  scale_colour_manual(values = c("brown1", "cornflowerblue")) +
  labs(y = "Total Motifs") +
  annotate("text", x = 38.704, 
           y = mean(log(ds$total_motifs+1), na.rm = TRUE), 
           label = "38.70", 
           size = 6,                    # annotation text ~20 pt
           angle = 360, vjust = -10, hjust = -0.2) +
  theme(
    legend.position = "none",
    axis.title.x = element_blank(),
    text = element_text(size = 20)      # all text set to 20 pt
  )

# Combine vertically with shared x-axis label at the bottom
(p1 / p2) & labs(x = "Mean Temperature") & theme(text = element_text(size = 20))

#Holm-Bonferroni correction########
p_values <- c(0.002369, 0.007363, 0.01117, 0.03215, 0.04916) 
adjusted_p_values <- p.adjust(p_values, method = "holm")
print(adjusted_p_values) #0.011845 0.029452 0.033510 0.064300 0.064300
