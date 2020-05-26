# We do not excluded #205, #212, 
# the first data point for #203 are excluded 
# after discussing with Dr. Thompson on May 25.
# We stop the regression line at 27 27 and 19

library(tidyverse)
library(ggplot2)

library(lme4)
library(sjPlot)
#########################################
# load data
weight_data = read_csv("./data/dataclean.csv",col_types = "ccfdddcdffff") %>% 
  janitor::clean_names() %>% drop_na(id)
# View(weight_data)
# dim(weight_data)

### process data ####
### data befroe intervention ###
weight_before = weight_data %>% 
  filter(tnp == 0 & feeding_tube == 0 & bm_transplant == 0 & l_transplant == 0) %>% 
  mutate(years = age - first_age) 

weight_before2 = weight_before %>% 
  select(-dob,-age,-first_age,-dom) %>% 
  select(id,num_measure,years,weight,everything()) %>% 
  mutate(years = as.numeric(years),weight = as.numeric(weight)) # id = as character
View(weight_before2)
ids = unique(weight_before2$id)
# length(ids) # 24 subjects
### subjuct only have one data before intervention ###
no_id = data.frame(count(weight_before2, id))  # number of data point before intervention
no_1 = filter(no_id, n == 1) # 98 only has one data point
weight_before2 = weight_before2 %>% filter(!(id == 98))  # No.98 is excluded, 23 subjects left

ids = unique(weight_before2$id) #23 subjects

### seperate by id ###
# Group 1
weight_before2_c1 = weight_before2 %>% 
  mutate(id = as.numeric(id)) %>% 
  filter(id < 200)
View(weight_before2_c1)
unique(weight_before2_c1$id)
# Group 2
weight_before2_c2 = weight_before2 %>%
  mutate(id = as.numeric(id)) %>% 
  filter(id > 200)
View(weight_before2_c2)
unique(weight_before2_c2$id)


#####################################################
# speghetti plot

Weight_plot = ggplot(data = weight_before2, aes(x = years, y = weight, group = id)) +
  geom_line(aes(color = id)) +
  geom_point(aes(color = id)) +
  scale_color_hue(name = "Subjects", h = c(100, 300)) +
  ggtitle("Weight changes over time (years) for patients in cohort 1 and 2")+ 
  labs(x = "Time (years)", y = "Weight (kg)")+
  theme_bw() + 
  theme(legend.position = "bottom")

# individually

# for c1
#dev.off()
par(mar = c(4,4,2,2))
par(mfrow = c(3,3))
idc1 = unique(weight_before2_c1$id)
for(i in idc1){
  df = subset(weight_before2_c1, weight_before2_c1$id == i)
  plot( df$years, df$weight,main = i , type = "b",
        xlab = "Time (years)",ylab = "weight (kg)")
}


idc2 = unique(weight_before2_c2$id)
#par(mar = c(4,4,2,2))
#par(mfrow = c(3,3))
for(i in idc2){
  df = subset(weight_before2_c2, weight_before2_c2$id == i)
  plot( df$years, df$weight, main = i, 
        type = "b",xlab = "Time (years)",ylab = "weight (kg)")
}

#############################
#     Mixed effect models   #
#############################

# FIt a model fot cohort 1 and 2
mod = lme4::lmer(weight ~ years+ (1|id),REML= FALSE, data=weight_before2)
summary(mod)
sjPlot::plot_model(mod,
                   show.values = TRUE, show.p=TRUE,
                   title ="Weight changes over time (years)")
jPlot:: tab_model(mod,show.re.var= F, 
                  dv.labels= "Weight changes over time (years) in cohort 1 and 2")
effects_years <- effects::effect(term= "years", mod= mod,xlevels = 27)
summary(effects_years) 
# Save the effects values as a df:
x_years <- as.data.frame(effects_years)
x_years <- x_years[c(1,9,18,27),]
years_plot = ggplot() + 
  #2
  geom_line(data=weight_before2, aes(x = years, y = weight,group = id)) + 
  #3
  geom_point(data=x_years, aes(x=years, y=fit), color="blue") +
  #4
  geom_line(data=x_years, aes(x=years, y=fit), color="blue") +
  #5
  geom_ribbon(data= x_years, aes(x=years, ymin=lower, ymax=upper), alpha= 0.3, fill="blue") +
  #6
  labs(x="Weight changes over time (years) in cohort 1 and 2", y="Weight (kg)")

##########
# fit a model for cohort 1
mod_c1 = lme4::lmer(weight ~ years+ (1|id),REML= FALSE, data=weight_before2_c1)
summary(mod_c1)
sjPlot::plot_model(mod_c1,
                   show.values = TRUE, show.p=TRUE,
                   title ="Weight changes over time (years) in cohort 1")
sjPlot:: tab_model(mod_c1,show.re.var= F, 
                   dv.labels= "Weight changes over time (years) in cohort 1")
effects_years_c1 <- effects::effect(term= "years", mod= mod_c1,xlevels = 27)
summary(effects_years_c1) 
# Save the effects values as a df:
x_years_c1 <- as.data.frame(effects_years)
x_years_c1 <- x_years_c1[c(1,9,18,27),]
years_plot_c1 = ggplot() + 
  #2
  geom_line(data=weight_before2_c1, aes(x = years, y = weight,group = id)) + 
  #3
  geom_point(data=x_years_c1, aes(x=years, y=fit), color="blue") +
  #4
  geom_line(data=x_years_c1, aes(x=years, y=fit), color="blue") +
  #5
  geom_ribbon(data= x_years_c1, aes(x=years, ymin=lower, ymax=upper), alpha= 0.3, fill="blue") +
  #6
  labs(x="Weight changes over time (years) in cohort 1", y="Weight (kg)")

################
# fit a model for cohort 2
mod_c2 = lme4::lmer(weight ~ years+ (1|id),REML= FALSE, data=weight_before2_c2)
summary(mod_c2)
sjPlot::plot_model(mod_c2,
                   show.values = TRUE, show.p=TRUE,
                   title ="Weight changes over time (years)")
sjPlot:: tab_model(mod_c2,show.re.var= F, 
                   dv.labels= "Weight changes over time (years) in cohort 2")
effects_years_c2 <- effects::effect(term= "years", mod= mod_c2,xlevels = 19)
summary(effects_years_c2) 
# Save the effects values as a df:
x_years_c2 <- as.data.frame(effects_years_c2)
x_years_c2 <- x_years_c2[c(1,5,10,15,19),]
years_plot_c2   = ggplot() + 
  #2
  geom_line(data=weight_before2_c2, aes(x = years, y = weight,group = id)) + 
  #3
  geom_point(data=x_years_c2, aes(x=years, y=fit), color="blue") +
  #4
  geom_line(data=x_years_c2, aes(x=years, y=fit), color="blue") +
  #5
  geom_ribbon(data= x_years_c2, aes(x=years, ymin=lower, ymax=upper), alpha= 0.3, fill="blue") +
  #6
  labs(x ="Weight changes over time (years) in cohort 2", y ="Weight (kg)")

# A model includs "group"

mod2 = lme4::lmer(weight ~ years+ group+ (1|id),REML= FALSE, data=weight_before2 )
summary(mod2)
sjPlot:: tab_model(mod2,show.re.var= FALSE, 
                   dv.labels= "Weight change over time (years) in cohort 1 and 2")


#####################################
# sensitivity analysis             #
# chunk data                       #
#####################################
weight_chunk = weight_before2 %>% filter(years <= 20)

view(weight_chunk)
weight_chunk_c1 = weight_chunk %>% 
  mutate(id = as.numeric(id)) %>% 
  filter(id < 200)
weight_chunk_c2 = weight_chunk %>% 
  mutate(id = as.numeric(id)) %>% 
  filter(id > 200) %>% 
  filter(years <= 11)

# fit models
# cohrt 1 and 2 ########3
Weight_plot_ch = ggplot(data = weight_chunk, aes(x = years, y = weight, group = id)) +
  geom_line(aes(color = id)) +
  geom_point(aes(color = id)) +
  scale_color_hue(name = "Subjects", h = c(100, 300)) +
  ggtitle("Sensitivity Analysis: Weight changes over time (years) for patients in cohort 1 and 2")+ 
  labs(x = "Time (years)", y = "Weight (kg)")+
  theme_bw() + 
  theme(legend.position = "bottom")

mod_sen = lme4::lmer(weight ~ years+ (1|id),REML= FALSE, data=weight_chunk)
summary(mod_sen)

sjPlot:: tab_model(mod_sen,show.re.var= F, 
                   dv.labels= "Sensitivity analysis: Weight changes over time (years) in cohort 1 and 2")
effects_years_sen <- effects::effect(term = "years", mod = mod_sen,xlevels = 19)
summary(effects_years_sen) 
# Save the effects values as a df:
x_years_sen <- as.data.frame(effects_years_sen)[c(1,4,7,10,13,18),]

years_plot_sen = ggplot() + 
  #2
  geom_point(data=weight_chunk, aes(x = years, y = weight,group = id,color = id)) +
  geom_line(data=weight_chunk, aes(x = years, y = weight,group = id,color = id)) + 
  #3
  geom_point(data=x_years_sen, aes(x=years, y=fit), color="blue") +
  #4
  geom_line(data=x_years_sen, aes(x=years, y=fit), color="blue") +
  #5
  geom_ribbon(data= x_years_sen, aes(x=years, ymin=lower, ymax=upper), alpha= 0.3, fill="blue") +
  #6
  labs(x="Sensitivity analysis: Weight changes over time (years) in cohort 1 and 2", y="Weight (kg)") +
  theme_bw() + 
  theme(legend.position = "bottom")

# cohort 1 ########3

Weight_plot_ch1 = ggplot(data = weight_chunk_c1 %>% mutate(id = as.character(id)), aes(x = years, y = weight, group = id)) +
  geom_line(aes(color = id)) +
  geom_point(aes(color = id)) +
  scale_color_hue(name = "Subjects", h = c(100, 300)) +
  ggtitle("Sensitivity Analysis: Weight changes over time (years) for patients in cohort 1")+ 
  labs(x = "Time (years)", y = "Weight (kg)") +
  theme_bw() + 
  theme(legend.position = "bottom")

mod_sen_c1 = lme4::lmer(weight ~ years+ (1|id),REML= FALSE, data=weight_chunk_c1)
summary(mod_sen_c1)

sjPlot:: tab_model(mod_sen_c1,show.re.var= F, 
                   dv.labels= "Sensitivity analysis: Weight changes over time (years) in cohort 1")
effects_years_sen_c1 <- effects::effect(term= "years", mod= mod_sen_c1)
summary(effects_years_sen_c1) 
# Save the effects values as a df:
x_years_sen_c1 <- as.data.frame(effects_years_sen_c1)

years_plot_sen_c1 = ggplot() + 
  #2
  geom_point(data=weight_chunk_c1 %>% mutate(id = as.character(id)), aes(x = years, y = weight,group = id,color = id)) + 
  geom_line(data=weight_chunk_c1 %>% mutate(id = as.character(id)), aes(x = years, y = weight,group = id,color = id)) + 
  #3
  geom_point(data=x_years_sen_c1, aes(x=years, y=fit), color="blue") +
  #4
  geom_line(data=x_years_sen_c1, aes(x=years, y=fit), color="blue") +
  #5
  geom_ribbon(data= x_years_sen_c1, aes(x=years, ymin=lower, ymax=upper), alpha= 0.3, fill="blue") +
  #6
  labs(x="Sensitivity analysis: Weight changes over time (years) in cohort 1", y="Weight (kg)") +
  theme_bw() + 
  theme(legend.position = "bottom")

# cohort 2

Weight_plot_ch2 = ggplot(data = weight_chunk_c2 %>% mutate(id = as.character(id)), aes(x = years, y = weight, group = id)) +
  geom_line(aes(color = id)) +
  geom_point(aes(color = id)) +
  scale_color_hue(name = "Subjects", h = c(100, 300)) +
  ggtitle("Sensitivity Analysis: Weight changes over time (years) for patients in cohort 2")+ 
  labs(x = "Time (years)", y = "Weight (kg)") +
  theme_bw() + 
  theme(legend.position = "bottom")


mod_sen_c2 = lme4::lmer(weight ~ years+ (1|id),REML= FALSE, data=weight_chunk_c2)
summary(mod_sen_c2)

sjPlot:: tab_model(mod_sen_c2,show.re.var= F, 
                   dv.labels= "Sensitivity analysis: Weight changes over time (years) in cohort 2")
effects_years_sen_c2 <- effects::effect(term= "years", mod= mod_sen_c2)
summary(effects_years_sen_c2) 
# Save the effects values as a df:
x_years_sen_c2 <- as.data.frame(effects_years_sen_c2)

years_plot_sen_c2 = ggplot() + 
  #2
  geom_point(data=weight_chunk_c2 %>% mutate(id = as.character(id)), aes(x = years, y = weight,group = id,color = id)) + 
  geom_line(data=weight_chunk_c2 %>% mutate(id = as.character(id)), aes(x = years, y = weight,group = id,color = id)) + 
  #3
  geom_point(data=x_years_sen_c2, aes(x=years, y=fit), color="blue") +
  #4
  geom_line(data=x_years_sen_c2, aes(x=years, y=fit), color="blue") +
  #5
  geom_ribbon(data= x_years_sen_c2, aes(x=years, ymin=lower, ymax=upper), alpha= 0.3, fill="blue") +
  #6
  labs(x="Sensitivity analysis: Weight changes over time (years) in cohort 2", y="Weight (kg)") +
  theme_bw() + 
  theme(legend.position = "bottom")

# speghetti plot
data_ggp = weight_before2 %>%  mutate(id = as.character(id))
Weight_plot_sc2 = ggplot(data = weight_chunk_c2 %>% mutate(id = as.character(id)), aes(x = years, y = weight, group = id)) +
  geom_line(aes(color = id)) +
  scale_color_hue(name = "Subjects", h = c(100, 300)) +
  ggtitle("Weight changes over time (years) for patients in cohort 2")+ 
  labs(x = "Time (years)", y = "Weight (kg)")+
  theme_bw() + 
  theme(legend.position = "bottom")
