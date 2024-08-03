# respo plots
# tena
# 7/28/24

#####------load packages-----#####
library('tidyverse')
library('here')
library("lme4")
library("lmerTest")

#load data
respo <- read.csv(here("Data",
                      "PR_2024",
                      "PR_final_normalized_clean.csv"))
calc <- read.csv(here("Data",
                      "TA",
                      "NEC.csv"))

#####------cleaning up outliers-----#####
respo <- respo[-141,]
respo <- respo[-293,]
respo <- respo[-62,]
respo <- respo[-188,]
respo <- respo[-168,]
respo <- respo[-198,]
respo <- respo[-187,]
respo <- respo[-283,]

#####------stdev-----#####


#####------stats-----#####

##tegula
respo$temp_treatment <- as.factor(respo$temp_treatment)
respo$experiment <- as.numeric(respo$experiment)
tegrespo <- respo %>%
  filter(Species == "Tegula")
teganova_reduced <- lm(Respiration ~ poly(pH_treatment,2) * temp_treatment, data=tegrespo)
#plot(teganova)
anova(teganova_reduced)

#with random effect
teganova_full <- lmer(Respiration ~ poly(pH_treatment,2) * temp_treatment + (1|experiment), data=tegrespo)
anova(teganova_full)
anova(teganova_full,teganova_reduced) #reduced model is better

#prediction for model
tegrespo <- tegrespo %>%
  mutate(model = predict(teganova_reduced))


##rockweed
rwrespo <- respo %>%
  filter(Species == "Rockweed")
rwanova_reduced <- lm(Respiration ~ pH_treatment * temp_treatment, data=rwrespo)
#plot(rwanova)
anova(rwanova_reduced)
#with random effect
rwanova_full <- lmer(Respiration ~ pH_treatment * temp_treatment + (1| experiment), data=rwrespo)
anova(rwanova_full)
anova(rwanova_full, rwanova_reduced) #reduced is better

rwrespo <- rwrespo %>%
  mutate(model = predict(rwanova_reduced))

rwgp<- rwrespo %>% 
  mutate(gp=Light+Respiration)
gpanova_reduced <- lm(gp ~ pH_treatment * temp_treatment, data=rwgp)
gpanova_full <- lmer(gp ~ pH_treatment * temp_treatment + (1|experiment), data=rwgp)
#plot(gpanova)
anova(gpanova_full,gpanova_reduced)
anova(gpanova_reduced)
anova(gpanova_full)

rwgp <- rwgp %>%
  mutate(model = predict(gpanova_reduced))

#calcification

#polynomial ancova
calc$temp_treatment <-as.factor(calc$temp_treatment)
calc$experiment <- as.numeric (calc$experiment)
calcpoly <- lm(umol.cm2.hr ~ poly(pH_treatment,2) * temp_treatment, data = calc)
#plot(calcpoly)
anova(calcpoly)

#with random effect for experiment
calcpolyrando <-lmer(umol.cm2.hr ~ poly(pH_treatment,2) * temp_treatment + (1|experiment), data = calc)
is_singular <- isSingular(calcpolyrando, tol = 1e-05)
print(is_singular)
anova(calcpolyrando)

anova(calcpolyrando,calcpoly) #model without random effect is better!

calc <- calc %>%
  mutate(model = predict(calcpoly)) #model prediction for plot

#####-----plots-----#####
#gp
rwgp %>% 
  ggplot(aes(x=as.factor(pH_treatment), y=gp, fill=as.factor(temp_treatment)))+
  geom_dotplot(binaxis = 'y', stackdir = 'center', dotsize = 0.8, alpha = 0.3) +
  stat_summary(aes(color = as.factor(temp_treatment)), fun.data = mean_se, fun.args = list(mult = 1), 
               geom = "pointrange", size = 0.5) +
  geom_line(aes(y = model, group = temp_treatment, color = as.factor(temp_treatment))) +
  theme_bw() +
  theme(strip.background = element_rect(fill = "white"),
        text = element_text(size = 14)) +
  scale_fill_manual(values = c("seagreen3", "seagreen4")) +
  scale_color_manual(values = c("seagreen3", "seagreen4")) +
  labs(x = expression(pH[T]),
       color = "Temperature",
       fill = "Temperature",
       y = expression(Gross~Photosynthetic~rate~(μmol~O[2]~g^-1~hr^-1)))

##respo

#tegula only
tegrespo %>%
  ggplot(aes(x=as.factor(pH_treatment), y=Respiration, fill=as.factor(temp_treatment)))+
  geom_dotplot(binaxis = 'y', stackdir = 'center', dotsize = 0.8, alpha = 0.3) +
  stat_summary(aes(color = as.factor(temp_treatment)), fun.data = mean_se, fun.args = list(mult = 1), 
               geom = "pointrange", size = 0.5) +
  geom_line(aes(y = model, group = temp_treatment, color = as.factor(temp_treatment))) +
  theme_bw() +
  theme(strip.background = element_rect(fill = "white"),
        text = element_text(size = 14)) +
  scale_fill_manual(values = c("salmon", "salmon4")) +
  scale_color_manual(values = c("salmon", "salmon4")) +
  labs(x = expression(pH[T]),
       color = "Temperature",
       fill = "Temperature",
       y = expression(Respiration~rate~(μmol~O[2]~g^-1~hr^-1)))
  
#rockweed only
rwrespo %>%
  ggplot(aes(x=as.factor(pH_treatment), y=Respiration, fill=as.factor(temp_treatment)))+
  geom_dotplot(binaxis = 'y', stackdir = 'center', dotsize = 0.8, alpha = 0.3) +
  stat_summary(aes(color = as.factor(temp_treatment)), fun.data = mean_se, fun.args = list(mult = 1), 
               geom = "pointrange", size = 0.5) +
  geom_line(aes(y = model, group = temp_treatment, color = as.factor(temp_treatment))) +
  theme_bw() +
  theme(strip.background = element_rect(fill = "white"),
        text = element_text(size = 14)) +
  scale_fill_manual(values = c("seagreen3", "seagreen4")) +
  scale_color_manual(values = c("seagreen3", "seagreen4")) +
  labs(x = expression(pH[T]),
       color = "Temperature",
       fill = "Temperature",
       y = expression(Respiration~rate~(μmol~O[2]~g^-1~hr^-1)))

#p v r
respo %>% 
  mutate(gp=Light+Respiration, 
         pr=gp/Respiration) %>%
  ggplot(aes(x=as.factor(pH_treatment), y=pr, color=as.factor(temp_treatment)))+
  geom_boxplot()

#calcification
ggplot(data = calc, aes(x = pH_treatment, y = umol.cm2.hr, group = interaction(pH_treatment, temp_treatment), fill = as.factor(temp_treatment))) +
  geom_hline(yintercept = 0, linetype = 2) +
  geom_dotplot(binaxis = 'y', stackdir = 'center', dotsize = 0.8, alpha = 0.3) +
  stat_summary(aes(color = as.factor(temp_treatment)), fun.data = mean_se, fun.args = list(mult = 1), 
               geom = "pointrange", size = 0.5) +
  geom_line(aes(y = model, group = temp_treatment, color = as.factor(temp_treatment))) +
  theme_bw() +
  theme(strip.background = element_rect(fill = "white"),
       text = element_text(size = 14)) +
  scale_fill_manual(values = c("salmon", "salmon4")) +
  scale_color_manual(values = c("salmon", "salmon4")) +
  labs(x = expression(pH[T]),
       color = "Temperature",
       fill = "Temperature",
       y = expression(Calcification~rate~(μmol~CO[3]~g^-1~hr^-1)))
#r v c

