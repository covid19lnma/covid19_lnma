library(gemtc)
library(readxl)
library(tidybayes)
library(ggdist)
library(stringr)
library(ggplot2)
library(dplyr)
library(viridis)

theme_set(theme_linedraw() + theme(panel.grid = element_blank()))

###### ICU length of stay days ##################

data=read_excel("drugs/data/All continuous outcomes_long data for analysis_20210326.xlsx",range = "L2:Q8")

data=data %>% select(study,treatment_recoded,mean,std.dev,sampleSize) %>% rename(treatment=treatment_recoded) %>% 
  mutate(treatment=gsub("[^A-Za-z]","\\1",treatment)) %>% as.data.frame()

network=mtc.network(data)

model=mtc.model(network,type = "consistency",
                   likelihood="normal",link="identity",
                   linearModel="random", n.chain =3,
                   powerAdjust=NA, dic=TRUE,
                   hy.prior=mtc.hy.prior("var", "dlnorm", -2.06, 0.4386))

plot(model)

results=mtc.run(model, n.adapt=10000, n.iter=50000)

out_all=gather_draws(results$samples,`d.*`,regex = T) %>%
  mutate(t1=str_extract(.variable,"(?<=\\.)(?:(?!\\.).)*$"),
         t2=str_extract(.variable,"(?<=\\.)(.*)(?=\\.)")) %>% group_by(t1) %>% 
  mean_qi(.value)

gather_draws(results$samples,`d.*`,sd.d,regex = T) %>%
  mutate(t1=str_extract(.variable,"(?<=\\.)(?:(?!\\.).)*$"),
         t2=str_extract(.variable,"(?<=\\.)(.*)(?=\\.)")) %>% 
  ggplot(aes(fill=as.factor(.chain),group=.chain,x=.value))+
  geom_density(alpha=0.7)+
  facet_wrap(~.variable,scales ="free")+
  scale_fill_viridis(discrete=T)

gather_draws(results$samples,`d.*`,sd.d,regex = T) %>%
  mutate(t1=str_extract(.variable,"(?<=\\.)(?:(?!\\.).)*$"),
         t2=str_extract(.variable,"(?<=\\.)(.*)(?=\\.)")) %>%
  ggplot(aes(x=.iteration,y=.value,color=as.factor(.chain)))+
  geom_line(alpha=0.7)+
  facet_wrap(~.variable,scale="free_y")+labs(color="chain")+
  scale_color_viridis(discrete=T)

gather_draws(results$samples,`d.*`,regex = T) %>%
  mutate(t1=str_extract(.variable,"(?<=\\.)(?:(?!\\.).)*$"),
         t2=str_extract(.variable,"(?<=\\.)(.*)(?=\\.)")) %>% 
  ggplot(aes(.value, t1)) +
  geom_vline(xintercept = 0, size = .25, lty = 2) +
  stat_pointinterval(.width = c(.8, .95),point_colour="#FDE725FF",interval_colour="#481567FF",
                     alpha=1) + geom_text(
                       data = mutate_if(out_all, is.numeric, round, 2),
                       aes(label = str_glue("{.value} [{.lower}, {.upper}]"), x = 3.5),
                       hjust = "inward"
                     )

################## hospital admission ########################

data=read_excel("drugs/data/All binary outcomes_long data for analysis_20210326.xlsx",range = "X2:AC23")

data=data %>% select(stauthor,treatment,responder,sampleSize) %>% rename(study=stauthor,responders=responder) %>% 
  mutate(treatment=gsub("[^a-z]","\\1",treatment)) %>% as.data.frame()

network=mtc.network(data)

model=mtc.model(network,
                type = "consistency",
                likelihood = "binom",
                link="logit",
                linearModel = "random",
                n.chain = 3,
                powerAdjust=NA,
                dic = TRUE,
                hy.prior=mtc.hy.prior("var", "dlnorm", -2.34, 0.3303))

plot(model)

results=mtc.run(model, n.adapt=10000, n.iter=50000) # corre simulaciones



t=spread_draws(results$samples,`d.*`,regex = T)


out_all=gather_draws(results$samples,`d.*`,regex = T) %>%
  mutate(t1=str_extract(.variable,"(?<=\\.)(?:(?!\\.).)*$"),
         t2=str_extract(.variable,"(?<=\\.)(.*)(?=\\.)")) %>% group_by(t1) %>% 
  mean_qi(.value)

gather_draws(results$samples,`d.*`,sd.d,regex = T) %>%
  mutate(t1=str_extract(.variable,"(?<=\\.)(?:(?!\\.).)*$"),
         t2=str_extract(.variable,"(?<=\\.)(.*)(?=\\.)")) %>% 
  ggplot(aes(fill=as.factor(.chain),group=.chain,x=.value))+
  geom_density(alpha=0.7)+
  facet_wrap(~.variable,scales ="free")+
  scale_fill_viridis(discrete=T)

gather_draws(results$samples,`d.*`,sd.d,regex = T) %>%
  mutate(t1=str_extract(.variable,"(?<=\\.)(?:(?!\\.).)*$"),
         t2=str_extract(.variable,"(?<=\\.)(.*)(?=\\.)")) %>%
  ggplot(aes(x=.iteration,y=.value,color=as.factor(.chain)))+
  geom_line(alpha=0.7)+
  facet_wrap(~.variable,scale="free_y")+labs(color="chain")+
  scale_color_viridis(discrete=T)

gather_draws(results$samples,`d.*`,regex = T) %>%
  mutate(t1=str_extract(.variable,"(?<=\\.)(?:(?!\\.).)*$"),
         t2=str_extract(.variable,"(?<=\\.)(.*)(?=\\.)")) %>% 
  ggplot(aes(.value, t1)) +
  geom_vline(xintercept = 0, size = .25, lty = 2) +
  stat_pointinterval(.width = c(.8, .95),point_colour="#FDE725FF",interval_colour="#481567FF",
                     alpha=1) + geom_text(
                       data = mutate_if(out_all, is.numeric, round, 2),
                       aes(label = str_glue("{.value} [{.lower}, {.upper}]"), x = 8.5),
                       hjust = "inward"
                     )

######

gather_draws(results$samples,`d.*`,regex = T) %>% group_by(.variable) %>% 
  summarise(median=median(.value),lower=quantile(.value,.025),upper=quantile(.value,0.75))

for(k in 1:nt){
  cr[k] <- exp(d[k] + log(prob.ref) - log(1 + prob.ref*(OR.ref[k] - 1)))  # cr: corresponding risk for treatment k (PMID: 11004419)
}
for (c in 1:(nt-1)) {
  for (k in (c+1):nt) {
    RD[k, c] <- cr[k] - cr[c]    
  } 
}