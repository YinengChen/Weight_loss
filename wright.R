library(tidyverse)
library(readxl)
library(nlme)
library(ggplot2)
###################################3
# load data
weight_data = read_csv("./data/dataclean.csv",col_types = "ccfdddcdffff") %>% 
  janitor::clean_names() %>% drop_na(id)
#############################
# process data
years = weight_data$age - weight_data$first_age 

weight_data1 = select(weight_data, -dob,-age,-first_age,-dom) %>% 
  mutate(years = years) %>% select(id,num_measure,years,weight,everything()) %>% 
  mutate(id = as.numeric(id),years = as.numeric(years),weight = as.numeric(weight))
View(weight_data1)

# data befroe intervention
weight_before = weight_data1 %>% 
  filter(tnp == 0 & feeding_tube == 0 & bm_transplant == 0 & l_transplant == 0)
ids = unique(weight_before$id) 
length(ids) # 24 subjects

# subjuct only have one data before intervention
no_id = data.frame(count(weight_before, id))  # number of data point before intervention
no_1 = filter(no_id, n == 1) # 98 only has one data point
weight_before2 = weight_before %>% filter(!(id == 98))  # 23 subjects left

#seperate by id
weight_before2_c1 = weight_before2 %>% filter(id < 200)
View(weight_before2_c1)
unique(weight_before2_c1$id)

weight_before2_c2 = weight_before2 %>% filter(id > 200)
View(weight_before2_c2)
unique(weight_before2_c2$id)

# speghetti plot
data_ggp = weight_before2 %>%  mutate(id = as.character(id))
Weight_plot = ggplot(data = data_ggp, aes(x = years, y = weight, group = id)) +
  geom_line(aes(color = id)) +
  scale_color_hue(name = "Subjects", h = c(100, 300)) +
  ggtitle("Weight change over time(year)")+ 
  theme_bw() + 
  theme(legend.position = "bottom")

Weight_smo = Weight_plot +
  stat_smooth(aes(group = 1),se = FALSE) 

# individually

# for c1
#dev.off()
par(mar = c(4,4,2,2))
par(mfrow = c(3,3))
idc1 = unique(weight_before2_c1$id)
for(i in idc1){
  df = subset(weight_before2_c1, weight_before2_c1$id == i)
  plot( df$years, df$weight,main = i , type = "b")
}


idc2 = unique(weight_before2_c2$id)
par(mar = c(4,4,2,2))
par(mfrow = c(3,3))
for(i in idc2){
  df = subset(weight_before2_c2, weight_before2_c2$id == i)
  plot( df$years, df$weight, main = i, 
        type = "b",xlab = "years",ylab = "weight(kg)")
}

 

#######################
weight_new <- groupedData (weight ~ years | id, data = weight_before2)
head(weight_new)
plot(weight_new)
#################
#ggplot(data = weight_before, aes(x = years, y = weight, group = id)) +
  #geom_line()+
  #stat_smooth(aes(group = 1),se = FALSE)

#ggplot(weight_before) + 
  #geom_path(aes(x = years, y = weight, group = id)) +
  #ggtitle("Weight change over time(year)")
#######################

# fit LMM with random intercept 
LMM1 <- lme(weight ~ years, random = ~  1 | id, data = weight_before2)
summary(LMM1)
random.effects(LMM1)
fixed.effects(LMM1)

# who has overall better performance? 
#best
subj1=rownames(random.effects(LMM1))[which.max(random.effects(LMM1)[,1])]
ggplot(weight_before) + 
  geom_path(aes(x = years, y = weight, group = id)) +
  geom_line(data=weight_before[weight_before$id==subj1,],aes(x=years, y=weight,color=subj1))+
  ggtitle("Score change after intervention")

# who gain
subj3=rownames(random.effects(LMM1))[random.effects(LMM1)[,2]>0]
ggplot(weight_before) + 
  geom_path(aes(x = years, y = weight, group = id)) +
  geom_line(data=weight_before[weight_before$id==subj3,],aes(x=years, y=weight,color=subj3))+
  ggtitle("Score change after intervention")

# who gain weight over time
subj2=rownames(random.effects(LMM1))[fixed.effects(LMM1)[2]+random.effects(LMM1)[,2]>0] #null
ggplot(data2) + 
  geom_path(aes(x = days, y = score, group = doc)) +
  geom_line(data=data2[data2$doc==subj2,],aes(x=days, y=score,color=subj2))+
  ggtitle("Score change after intervention")
####################
library(cowplot) 
library(lme4)
library(sjPlot)
library(sjmisc) 
library(effects)
library(sjstats)

# random intercept model
mod = lme4::lmer(weight ~ years+ (1|id),REML= FALSE, data=weight_before2 )
summary(mod)

random.effects(mod)

sjPlot::plot_model(mod,
                   show.values = TRUE, show.p=TRUE,
                   title ="Weight change over time(years)")
sjPlot:: tab_model(mod,show.re.var= TRUE, 
                   dv.labels= "Weight change over time(years)")
effects_years <- effects::effect(term= "years", mod= mod)
summary(effects_years)
# Save the effects values as a df:
x_years <- as.data.frame(effects_years)

years_plot = ggplot() + 
  #2
  geom_point(data=data_ggp, aes(x = years, y = weight,group = id)) + 
  #3
  geom_point(data=x_years, aes(x=years, y=fit), color="blue") +
  #4
  geom_line(data=x_years, aes(x=years, y=fit), color="blue") +
  #5
  geom_ribbon(data= x_years, aes(x=years, ymin=lower, ymax=upper), alpha= 0.3, fill="blue") +
  #6
  labs(x="Weight change over time(years)", y="Weight(kg)")


###############
# random intercept and slopt
mod_ris = lme4::lmer(weight ~ years + (years +1|id), data=weight_before2 )
summary(mod_ris)
random.effects(mod_ris)

sjPlot::plot_model(mod_ris,
                   show.values = TRUE, show.p=TRUE,
                   title ="Weight change over time(years)")
sjPlot:: tab_model(mod_ris,show.re.var= TRUE, 
                   dv.labels= "Weight change over time(years)")
effects_years_ris <- effects::effect(term= "years", mod= mod_ris)
summary(effects_years_ris)
x_years_ris <- as.data.frame(effects_years_ris)
years_plot = ggplot() + 
  #2
  geom_line(data=data_ggp, aes(x = years, y = weight,group = id)) + 
  #3
  geom_point(data=x_years_ris, aes(x=years, y=fit), color="blue") +
  #4
  geom_line(data=x_years_ris, aes(x=years, y=fit), color="blue") +
  #5
  geom_ribbon(data= x_years_ris, aes(x=years, ymin=lower, ymax=upper), alpha= 0.3, fill="blue") +
  #6
  labs(x="Weight change over time(years)", y="Weight(kg)")

#################
randoms = as.data.frame(random.effects(mod_ris))
subj1=rownames(random.effects(mod_ris))[which.max(random.effects(mod_ris)[,1])]