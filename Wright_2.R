#####################################################
#   Body weight change analysis on MINGLE patients  #
#      22 May          Yineng Chen                  #
#####################################################

# This data site contains the weight data of 24 patinets. 
# # 98 only has one data point and there for is excluded
# we have 23 patinets ready for analysis. 
# 9 of them come from Natural History study
# 14 of them do not come from Natural History study
# #205, #212, and the first data point for #203 are excluded 
# after discussing with Dr. Hirano.

# I use mexed effect model because the number of measurment 
# and the timing of each petient are different

library(tidyverse)
library(readxl)
library(nlme)
library(ggplot2)

library(lme4)
library(sjPlot)
###################################
# load data
weight_data = read_csv("./data/dataclean.csv",col_types = "ccfdddcdffff") %>% 
  janitor::clean_names() %>% drop_na(id)
# View(weight_data)
# dim(weight_data)
#############################

### process data ####
years = weight_data$age - weight_data$first_age 

weight_data1 = select(weight_data, -dob,-age,-first_age,-dom) %>% 
  mutate(years = years) %>% 
  select(id,num_measure,years,weight,everything()) %>% 
  mutate(years = as.numeric(years),weight = as.numeric(weight)) # ? id = as.numeric(id),
View(weight_data1)

### data befroe intervention ###
weight_before = weight_data1 %>% 
  filter(tnp == 0 & feeding_tube == 0 & bm_transplant == 0 & l_transplant == 0)
ids = unique(weight_before$id)
# length(ids) # 24 subjects

### subjuct only have one data before intervention ###
no_id = data.frame(count(weight_before, id))  # number of data point before intervention
no_1 = filter(no_id, n == 1) # 98 only has one data point
weight_exc1 = weight_before %>% filter(!(id == 98))  # No.98 is excluded, 23 subjects left

### Exclude No.205 and No.212 ###
weight_exc2 = weight_exc1 %>% 
  filter(!(id == 205) & !(id == 212))

### Exclude first 5 point of No.203 ###
# This patient had 5 mearment within a week at age 18


weight_before2 = weight_exc2
id2 = unique(weight_before2$id)

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

## Baseline ############
first_m <- function(id1) {
  if (id1 %in% unique(weight_before2$id)){
    
  } else {print(paste("Error in best(",id1,", ) : invalid id")) } 
  
  data_in = weight_before2 %>% filter(id == id1) %>% 
    select(id,num_measure,years,weight)
  first_data = as.numeric(data_in[1,4])
  first_data
}

weights = numeric(length = length(id2))
id = numeric(length = length(id2))
idx <- 1

for(i in id2){
  id[idx] <- i
  weights[idx] <- first_m(i)
  idx <- idx + 1 
}

baseline_weight = data.frame(id,weights) %>% mutate(id = as.character(id))

baseline_weight_c1 = baseline_weight %>% 
  mutate(id = as.numeric(id)) %>% 
  filter(id < 200)

baseline_weight_c2 = baseline_weight %>% 
  mutate(id = as.numeric(id)) %>% 
  filter(id > 200)

f = var.test(baseline_weight_c2$weights,baseline_weight_c1$weights, alternative = "two.sided")
# F = 1.1871, num df = 6, denom df = 13, p-value = 0.7427 
# Fail to reject the null, evidence that variances are equal.
t = t.test(baseline_weight_c2$weights,baseline_weight_c1$weights, var.equal= TRUE, paired=FALSE)
# t = -0.73991, df = 19, p-value = 0.4684
# the means weight are not sig diff b/w the two groups.
v1 = var(baseline_weight_c1$weights)
v2 = var(baseline_weight_c2$weights)
m1 = mean(baseline_weight_c1$weights)
m2 = mean(baseline_weight_c2$weights)
out1 = cbind(m1,m2,t$statistic,t$p.value)
out2 = cbind(v1,v2,f$statistic,f$p.value)
out = rbind(out1,out2)
colnames(out)=c('Group 1','Group 2',"t-statistic","p value")
rownames(out)=c('Mean Weight(Kg)',"Variance")

# speghetti plot
data_ggp = weight_before2 %>%  mutate(id = as.character(id))
Weight_plot = ggplot(data = data_ggp, aes(x = years, y = weight, group = id)) +
  geom_line(aes(color = id)) +
  scale_color_hue(name = "Subjects", h = c(100, 300)) +
  ggtitle("Weight change over time(year)")+ 
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
        xlab = "years",ylab = "weight(kg)")
}


idc2 = unique(weight_before2_c2$id)
#par(mar = c(4,4,2,2))
#par(mfrow = c(3,3))
for(i in idc2){
  df = subset(weight_before2_c2, weight_before2_c2$id == i)
  plot( df$years, df$weight, main = i, 
        type = "b",xlab = "years",ylab = "weight(kg)")
}

##########

mod = lme4::lmer(weight ~ years+ (1|id),REML= FALSE, data=weight_before2 )
summary(mod)
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

##########
mod2 = lme4::lmer(weight ~ years+ group+ (1|id),REML= FALSE, data=weight_before2 )
summary(mod2)
sjPlot:: tab_model(mod2,show.re.var= FALSE, 
                   dv.labels= "Weight change over time(years)")
# Group covariate is not significant