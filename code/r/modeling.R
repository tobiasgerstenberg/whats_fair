#' ---
#' title: What's fair - Model fits and plots
#' author: Tobias Gerstenberg
#' date: September 10, 2017
#' output:
#'    html_document:
#'      toc: true
#'      toc_depth: 3
#'      toc_float: true
#'      theme: default
#'      highlight: tango
#' ---

#+ General settings, echo = FALSE, results = 'hide' ------------------------------------------------------------------------------

knitr::opts_chunk$set(fig.width=12, fig.height=8, warning=FALSE, message=FALSE)
figure.path = "../../figures/"

#+ PREPARATION ---------------------------------------------------------------------------------
#' # PREPARATION
#+ Load packages -------------------------------------------------------------------------------
#' ## Load packages 
library(lme4)
library(stringr)
library(Hmisc)
library(knitr)
library(tidyverse)
rm(list = ls())

#+ Helper functions  ---------------------------------------------------------------------------
#' ## Helper functions  

theme_set(theme_bw() +
            theme(text = element_text(size=20),
                  panel.grid = element_blank(),
                  legend.text = element_text(size=14),
                  legend.title = element_text(size=16),
                  axis.title.y = element_text(size=16),
                  strip.placement = 'inside',
                  strip.background = element_blank(),
                  legend.background = element_rect(fill = 'transparent')
            )
)

#+ Read aggregate data and model predictions  --------------------------------------------------
#' ## Read aggregate data and model predictions  
df.long = read.csv("../../data/data_aggregate.csv")
df.model = read.csv("model_prediction.csv")

df.long = df.long %>% 
  left_join(df.model,by = c("condition","trial"))

#+ Run different models on individual participants  -------------------------------------------
#' ## Run different models on individual participants  

f_regression = function(data,formula){
  fit = lm(formula,data = data)
  fits = summary(fit)
  
  return(list(r = fits$r.squared^(1/2),
              sigma = fits$sigma,
              values = fit$fitted.values,
              fit = fit))
}

list.modelfit = list()

# set of models 
model.names = c("null_model","matching","counterfactual","structural","outcome","individual","outcome_matching")
model.formulas = c("rating~1",paste0("rating~",str_replace_all(model.names[-1],"_","+")))

parameter.names = c("r","sigma","fit","values","BIC")

for (i in model.names){
  for (j in parameter.names){
    list.modelfit[[i]][[j]] = list()
  }
}

for (i in unique(df.long$participant)){
  for (j in 1:length(model.names)){
    tmp = f_regression(df.long %>% filter(participant == i),formula(model.formulas[j]))
    list.modelfit[[model.names[j]]][["r"]][i] = tmp$r
    list.modelfit[[model.names[j]]][["sigma"]][i] = tmp$sigma
    list.modelfit[[model.names[j]]][["fit"]][[i]] = tmp$fit
    list.modelfit[[model.names[j]]][["BIC"]][[i]] = BIC(tmp$fit)
  }
}

# compare BIC scores 
df.bic = data.frame(matrix(NA, ncol= length(model.names)+1, nrow = length(unique(df.long$participant)))) %>% 
  setNames(c("participant",model.names)) %>% 
  mutate(participant = unique(df.long$participant))

for (i in 1:length(model.names)){
  df.bic[[model.names[i]]] = list.modelfit[[model.names[i]]][["BIC"]] %>% unlist()
}

# check for negative coefficients and then replace corresponding values with Inf 
for(i in model.names){
  for (j in 1:nrow(df.bic)){
    if(any(list.modelfit[[i]][["fit"]][[j]]$coefficients[-1] < 0,na.rm=T)){
      df.bic[[i]][j]  = Inf
    }
  }
}

# assign best-fitting model to each participant (using BIC as criterion) 
tmp = df.bic %>% 
  gather(model,bic,-participant) %>% 
  group_by(participant) %>% 
  filter(bic == min(bic)) %>% 
  filter(row_number() == 1) %>%
  arrange(participant,model) %>% 
  ungroup

df.long = df.long %>% 
  left_join(tmp, by = "participant")

# re-assign participants in the additive condition who were assigned to the counterfactual, or structural model 
# to the outcome model 
df.long = df.long %>% 
  mutate(model = ifelse(condition == 'additive' & model %in% c("counterfactual","structural"),"outcome",model))

rm(list = setdiff(ls(),c("df.long","df.model","df.bic","list.modelfit")))

#+ PLOTS ---------------------------------------------------------------------------------
#' # PLOTS
#+ Plot: Results - Experiment 1 ----------------------------------------------------------
#' ## Plot: Results - Experiment 1 
experiment.name = "constrained"
# experiment.name = "unconstrained"

trial.labels = c('oo','om','mo','oc','mm','co','mc','cm','cc')

df.plot = df.long %>% 
  filter(experiment == experiment.name) %>% 
  select(participant,trial,rating,condition,age) %>% 
  mutate(rating = rating-10) %>% 
  mutate(age = factor(age,labels = c('4-5 years', '6-7 years', 'Adults'))) %>% 
  mutate(trial = factor(trial, levels = trial.labels, labels = toupper(trial.labels)))

df.text = data.frame(age = '4-5 years', x = c(2,5,8), y = rep(12,3), label = c('Negative outcomes', 'Mixed outcomes', 'Positive outcomes'),condition=NA)

ggplot(df.plot,aes(x=trial,y=rating))+
  scale_x_discrete()+
  geom_hline(yintercept = 0,linetype = 2, color = 'gray')+
  geom_vline(xintercept = 3.5,linetype = 2)+
  geom_vline(xintercept = 6.5,linetype = 2)+
  geom_text(data=df.text,aes(x = x, y = y,label = label),color = 'gray40',size=5)+
  stat_summary(fun.y = mean, geom = 'line', size = 1, aes(group=condition,color=condition,linetype=condition))+
  stat_summary(data = df.plot %>% filter(condition == 'additive'), color = 'indianred3', fun.data = mean_cl_boot, geom = "errorbar",width=0,show.legend = F)+
  stat_summary(data = df.plot %>% filter(condition == 'additive'), color = 'indianred3', fun.y = mean, geom = 'point', size = 2)+
  stat_summary(data = df.plot %>% filter(condition == 'conjunctive'), color = 'green3', fun.data = mean_cl_boot, geom = "errorbar",width=0,show.legend = F)+
  stat_summary(data = df.plot %>% filter(condition == 'conjunctive'), color = 'green3', fun.y = mean, geom = 'point', size = 2)+
  stat_summary(data = df.plot %>% filter(condition == 'disjunctive'), color = 'royalblue3', fun.data = mean_cl_boot, geom = "errorbar",width=0,show.legend = F)+
  stat_summary(data = df.plot %>% filter(condition == 'disjunctive'), color = 'royalblue3', fun.y = mean, geom = 'point', size = 2)+
  labs(y = "Mean reward assigned to target player\n(-10 = none, 10 = all)",
       x = "Pattern of throws: target player, other player")+
  coord_cartesian(xlim = c(0.5, 9.5), ylim = c(-10,14),expand=F)+
  facet_wrap(~age,ncol=1)+
  scale_color_manual(values = c("indianred3", "green3","royalblue3"))+
  scale_linetype_manual(values = c('solid','longdash','dotdash'))+
  theme(axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0, unit = "pt")),
        legend.box.background = element_rect(color='gray'),
        legend.position = c(0.01,0.125),
        legend.justification = c(0,0),
        legend.key.width = unit(1.6,"cm")
  )

# ggsave(paste0("../../figures/means_",experiment.name,".pdf"),width=8,height=8)

#+ Plot: Results - Experiment 2 ----------------------------------------------------------
#' ## Plot: Results - Experiment 2 
# experiment.name = "constrained"
experiment.name = "unconstrained"

trial.labels = c('oo','om','mo','oc','mm','co','mc','cm','cc')

df.plot = df.long %>% 
  filter(experiment == experiment.name) %>% 
  select(participant,trial,rating,condition,age) %>% 
  mutate(rating = rating-10) %>% 
  mutate(age = factor(age,labels = c('4-5 years', '6-7 years', 'Adults'))) %>% 
  mutate(trial = factor(trial, levels = trial.labels, labels = toupper(trial.labels)))

df.text = data.frame(age = '4-5 years', x = c(2,5,8), y = rep(12,3), label = c('Negative outcomes', 'Mixed outcomes', 'Positive outcomes'),condition=NA)

ggplot(df.plot,aes(x=trial,y=rating))+
  scale_x_discrete()+
  geom_hline(yintercept = 0,linetype = 2, color = 'gray')+
  geom_vline(xintercept = 3.5,linetype = 2)+
  geom_vline(xintercept = 6.5,linetype = 2)+
  geom_text(data=df.text,aes(x = x, y = y,label = label),color = 'gray40',size=5)+
  stat_summary(fun.y = mean, geom = 'line', size = 1, aes(group=condition,color=condition,linetype=condition))+
  stat_summary(data = df.plot %>% filter(condition == 'additive'), color = 'indianred3', fun.data = mean_cl_boot, geom = "errorbar",width=0,show.legend = F)+
  stat_summary(data = df.plot %>% filter(condition == 'additive'), color = 'indianred3', fun.y = mean, geom = 'point', size = 2)+
  stat_summary(data = df.plot %>% filter(condition == 'conjunctive'), color = 'green3', fun.data = mean_cl_boot, geom = "errorbar",width=0,show.legend = F)+
  stat_summary(data = df.plot %>% filter(condition == 'conjunctive'), color = 'green3', fun.y = mean, geom = 'point', size = 2)+
  stat_summary(data = df.plot %>% filter(condition == 'disjunctive'), color = 'royalblue3', fun.data = mean_cl_boot, geom = "errorbar",width=0,show.legend = F)+
  stat_summary(data = df.plot %>% filter(condition == 'disjunctive'), color = 'royalblue3', fun.y = mean, geom = 'point', size = 2)+
  labs(y = "Mean reward assigned to target player\n(-10 = none, 10 = all)",
       x = "Pattern of throws: target player, other player")+
  coord_cartesian(xlim = c(0.5, 9.5), ylim = c(-10,14),expand=F)+
  facet_wrap(~age,ncol=1)+
  scale_color_manual(values = c("indianred3", "green3","royalblue3"))+
  scale_linetype_manual(values = c('solid','longdash','dotdash'))+
  theme(axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0, unit = "pt")),
        legend.box.background = element_rect(color='gray'),
        legend.position = c(0.01,0.125),
        legend.justification = c(0,0),
        legend.key.width = unit(1.6,"cm")
  )

# ggsave(paste0("../../figures/means_",experiment.name,".pdf"),width=8,height=8)

#+ Plot: Defiant responses -----------------------------------------------------------------------
#' ## Plot: Defiant responses 

trial.labels = c('oo','om','mo','oc','mm','co','mc','cm','cc')

df.plot = df.long %>% 
  filter(experiment == 'unconstrained') %>% 
  mutate(outcome = ifelse(condition == 'additive' & trial %in% c('oo','om','mo'),0,1),
         outcome = ifelse(condition == 'conjunctive' & trial %in% c('oo','om','mo','oc','co'),0,outcome),
         outcome = ifelse(condition == 'disjunctive' & trial %in% c('oo','om','mo','mm'),0,outcome),
         defiant = ifelse(outcome == 1 & rating < 10,1,0),
         defiant = ifelse(outcome == 0 & rating > 10,1,defiant)) %>% 
  group_by(condition,trial,outcome) %>%
  summarise(percentage = sum(defiant)/n()) %>% 
  ungroup() %>% 
  mutate(trial = factor(trial, levels = trial.labels, labels = toupper(trial.labels))) %>%  
  mutate(outcome = factor(outcome,levels = c(0,1), labels = c("loss", "win")))

ggplot(df.plot,aes(x = trial, y = percentage,group = condition, color = condition, fill = outcome))+
  geom_line(aes(linetype = condition),size=1)+
  geom_point(size=4,shape=21,stroke=2)+
  labs(x = "Pattern of throws: target player, other player", 
       y = "Percentage of participants responding defiantly")+
  scale_color_manual(values = c("indianred3", "green3","royalblue3"))+
  scale_fill_manual(values = c("white","black"))+
  scale_y_continuous(breaks = seq(0,1,0.2), labels = paste0(seq(0,100,20),'%'))+
  scale_linetype_manual(values = c('solid','longdash','dotdash'))+
  coord_cartesian(ylim = c(0,0.8))+
  theme(panel.grid = element_blank(),
        strip.placement = 'inside',
        strip.background = element_blank(),
        axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0, unit = "pt")),
        legend.box.background = element_rect(color='gray'),
        legend.background = element_rect(fill = 'transparent'),
        legend.position = c(0,1),
        legend.justification = c(-0.1,1.1),
        legend.key.width = unit(1.6,"cm")
  )

# ggsave(paste0("../../figures/defiant_responses.pdf"),width=8,height=6)

#+ Plot: Model predictions ----------------------------------------------------------------------
#' ## Plot: Model predictions 

trial.labels = c('oo','om','mo','oc','mm','co','mc','cm','cc')

df.plot = df.model %>% 
  gather(model,rating,-c(trial,condition)) %>% 
  mutate(rating = rating * 10) %>% 
  mutate(trial = factor(trial, levels = trial.labels, labels = toupper(trial.labels))) %>% 
  mutate(model = factor(model, levels = c('matching','counterfactual','structural','outcome','hybrid','individual'),labels = c('Performance','Counterfactual','Structural','Outcome','Hybrid: Performance/Outcome','Individual Win')))

df.text = data.frame(x = c(2,5,8), y = rep(12,3), label = c('Negative outcomes', 'Mixed outcomes', 'Positive outcomes'),condition=NA,model=rep(c('Performance','Counterfactual'),each=3))

ggplot(df.plot,aes(x=trial,y=rating,group=condition,color=condition,linetype=condition))+
  scale_x_discrete()+
  geom_hline(yintercept = 0,linetype = 2, color = 'gray')+
  geom_vline(xintercept = 3.5,linetype = 2)+
  geom_vline(xintercept = 6.5,linetype = 2)+
  geom_text(data=df.text,aes(x = x, y = y,label = label),color = 'gray40',size=5)+
  geom_line(size=1)+
  geom_point(size=2)+
  facet_wrap(~model,ncol=2)+
  labs(y = expression(atop(paste(bold("Predicted reward "), "to target player"), paste("(-10 = none, 10 = all)"))),
       x = "Pattern of throws: target player, other player",
       title = "Model predictions")+
  coord_cartesian(xlim = c(0.5, 9.5), ylim = c(-11,14),expand=F)+
  scale_color_manual(values = c("indianred3", "green3","royalblue3"))+
  scale_linetype_manual(values = c('solid','longdash','dotdash'))+
  theme(axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0, unit = "pt")),
        plot.title = element_text(hjust = 0.5,face='bold'),
        legend.position = c(0.01,0.49),
        legend.justification = c(0,0),
        legend.key.width = unit(1.6,"cm")
  )

# ggsave(paste0("../../figures/model_predictions.pdf"),width=12,height=8)

#+ Plot: Average ratings per participant cluster ----------------------------------------------------------------------
#' ## Plot: Average ratings per participant cluster 

trial.labels = c('oo','om','mo','oc','mm','co','mc','cm','cc')

df.plot = df.long %>% 
  filter(model != 'null_model',
         experiment == 'unconstrained') %>% 
  mutate(rating = rating-10) %>% 
  group_by(condition,trial,model) %>% 
  summarise(mean = mean(rating),
            low = smean.cl.boot(rating)[2],
            high = smean.cl.boot(rating)[3]) %>% 
  ungroup() %>% 
  mutate(trial = factor(trial, levels = trial.labels, labels = toupper(trial.labels))) %>% 
  mutate(model = factor(model, levels = c('matching','counterfactual','structural','outcome','outcome_matching','individual'),labels = c('Performance','Counterfactual','Structural','Outcome','Hybrid: Performance/Outcome','Individual Win')))

df.text = data.frame(x = c(2,5,8), y = rep(12,3), label = c('Negative outcomes', 'Mixed outcomes', 'Positive outcomes'),condition=NA,model=rep(c('Performance','Counterfactual'),each=3))


df.groupsize = df.long %>% 
  filter(experiment == 'unconstrained', trial == 'cc') %>% 
  count(model) %>%
  filter(model != 'null_model') %>% 
  mutate(model = factor(model, levels = c('matching','counterfactual','structural','outcome','outcome_matching','individual'),labels = c('Performance','Counterfactual','Structural','Outcome','Hybrid: Performance/Outcome','Individual Win')),
         label = paste0('n = ',n),
         x = 8,
         y = -8,
         condition=NA) %>% 
  select(-n) %>% 
  as.data.frame()

ggplot(df.plot,aes(x=trial,y=mean,group=condition,color=condition))+
  scale_x_discrete()+
  geom_hline(yintercept = 0,linetype = 2, color = 'gray')+
  geom_vline(xintercept = 3.5,linetype = 2)+
  geom_vline(xintercept = 6.5,linetype = 2)+
  geom_text(data=df.text,aes(x = x, y = y,label = label),color = 'gray40',size=5)+
  geom_text(data=df.groupsize,aes(x = x, y = y,label = label),color = 'gray40',size=8)+
  geom_linerange(aes(ymin = low, ymax = high),size=1)+
  geom_line(size=1,aes(linetype=condition))+
  geom_point(size=2)+
  facet_wrap(~model,ncol=2)+
  labs(y = expression(atop(paste(bold("Mean assigned reward "), "to target player"), paste("(-10 = none, 10 = all)"))),
       x = "Pattern of throws: target player, other player",
       title = "Mean judgments per cluster")+
  coord_cartesian(xlim = c(0.5, 9.5), ylim = c(-11,14),expand=F)+
  # facet_wrap(~age,ncol=1)+
  scale_color_manual(values = c("indianred3", "green3","royalblue3"))+
  scale_linetype_manual(values = c('solid','longdash','dotdash'))+
  theme(axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0, unit = "pt")),
        legend.background = element_rect(fill = 'transparent'),
        legend.position = c(0.001,0.48),
        legend.justification = c(0,0),
        legend.key.width = unit(1.6,"cm"),
        plot.title = element_text(hjust = 0.5,face='bold')
  )

# ggsave(paste0("../../figures/cluster_judgments.pdf"),width=12,height=8)

#+ TABLES ---------------------------------------------------------------------------------
#' # TABLES
#+ Table: Model results (individual participants) ---------------------------------------------------------------------
#' ## Table: Model results (individual participants) 

df.bic %>% 
  left_join(df.long %>% 
              filter(trial == 'cc') %>% 
              select(experiment,participant,condition,model,age),
            by = "participant") %>% 
  filter(experiment == "unconstrained") %>% 
  mutate(model = factor(model, levels = c('matching','counterfactual','structural','outcome','outcome_matching','individual','null_model'),labels = c('Performance','Counterfactual','Structural','Outcome','Hybrid','Individual','Null')),
         age = factor(age,labels = c('4-5 years', '6-7 years', 'Adults'))) %>% 
  mutate_at(vars('matching','counterfactual','structural','outcome','outcome_matching','individual','null_model'),funs(round(.,2))) %>%
  mutate_at(vars('matching','counterfactual','structural','outcome','outcome_matching','individual','null_model'),funs(ifelse(.<0,0,.))) %>%
  arrange(age,condition,model) %>% 
  mutate(participant = 1:nrow(.)) %>%
  select(participant,age,condition,everything()) %>% 
  select(-c(experiment)) %>% 
  head() %>% 
  kable() 
  

#+ Table: Model results (condition and age groups), results='asis' -------------------------------------------
#' ## Table: Model results (condition and age groups) 

df.long %>% 
  filter(experiment == 'unconstrained') %>% 
  group_by(participant) %>%
  filter(row_number()==1) %>%
  ungroup() %>%
  filter(!(condition == 'additive' & model %in% c('outcome','counterfactual','structural'))) %>%
  count(condition,age,model) %>% 
  mutate(model = factor(model, levels = c('matching','counterfactual','structural','outcome','outcome_matching','individual','null_model'),labels = c('Performance','Counterfactual','Structural','Outcome','Hybrid','Individual', 'Null'))) %>%
  spread(model,n) %>% 
  ungroup %>% 
  mutate_at(vars(-c(condition,age)),funs(ifelse(is.na(.),0,.))) %>% 
  rename(Condition = condition,
         Age = age) %>% 
  select(Age,Condition,everything()) %>% 
  mutate(Age = factor(Age,levels = c('4_years','6_years','adults'),labels = c('4-5 years','6-7 years','Adults'))) %>% 
  arrange(Age,Condition) %>% 
  kable()
