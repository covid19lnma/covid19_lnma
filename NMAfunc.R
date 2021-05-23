library(gemtc)
library(readxl)
library(tidybayes)
library(ggdist)
library(stringr)
library(ggplot2)
library(dplyr)
library(viridis)


model.processing <- function(model){
  plot(model)
  
  results=mtc.run(model, n.adapt=10000, n.iter=50000) # corre simulaciones
  
  # out_all=gather_draws(results$samples,`d.*`,regex = T) %>%
  #   mutate(t1=str_extract(.variable,"(?<=\\.)(?:(?!\\.).)*$"),
  #          t2=str_extract(.variable,"(?<=\\.)(.*)(?=\\.)")) %>% group_by(t1) %>%
  #   mean_qi(.value)
  # 
  # gather_draws(results$samples,`d.*`,sd.d,regex = T) %>%
  #   mutate(t1=str_extract(.variable,"(?<=\\.)(?:(?!\\.).)*$"),
  #          t2=str_extract(.variable,"(?<=\\.)(.*)(?=\\.)")) %>%
  #   ggplot(aes(fill=as.factor(.chain),group=.chain,x=.value))+
  #   geom_density(alpha=0.7)+
  #   facet_wrap(~.variable,scales ="free")+
  #   scale_fill_viridis(discrete=T)
  # 
  # gather_draws(results$samples,`d.*`,sd.d,regex = T) %>%
  #   mutate(t1=str_extract(.variable,"(?<=\\.)(?:(?!\\.).)*$"),
  #          t2=str_extract(.variable,"(?<=\\.)(.*)(?=\\.)")) %>%
  #   ggplot(aes(x=.iteration,y=.value,color=as.factor(.chain)))+
  #   geom_line(alpha=0.7)+
  #   facet_wrap(~.variable,scale="free_y")+labs(color="chain")+
  #   scale_color_viridis(discrete=T)
  # 
  # gather_draws(results$samples,`d.*`,regex = T) %>%
  #   mutate(t1=str_extract(.variable,"(?<=\\.)(?:(?!\\.).)*$"),
  #          t2=str_extract(.variable,"(?<=\\.)(.*)(?=\\.)")) %>%
  #   ggplot(aes(.value, t1)) +
  #   geom_vline(xintercept = 0, size = .25, lty = 2) +
  #   stat_pointinterval(.width = c(.8, .95),point_colour="#FDE725FF",interval_colour="#481567FF",
  #                      alpha=1) + geom_text(
  #                        data = mutate_if(out_all, is.numeric, round, 2),
  #                        aes(label = str_glue("{.value} [{.lower}, {.upper}]"), x = 8.5),
  #                        hjust = "inward"
  #                      )
  results
}

#theme_set(theme_linedraw() + theme(panel.grid = element_blank()))

###### ICU length of stay days ##################

data=read_excel("drugs/data/Mortlaity - NMA IL6 & steroids- long data for analysis.xlsx",range = "A1:E105")

#filter(stauthor!="REMAP-CAP_2_NCT02735707") 

data=data %>% select(stauthor,treatment,responder,samplesize) %>% rename(study=stauthor,responders=responder,sampleSize=samplesize) %>% 
  mutate(treatment=gsub("[^A-Za-z]","\\1",treatment)) %>% filter(sampleSize!=0) %>% as.data.frame() 

network=mtc.network(data)

model=mtc.model(network,type = "consistency",
                likelihood="binom",link="logit",
                linearModel="fixed", n.chain =3,
                powerAdjust=NA, dic=TRUE,
                hy.prior=mtc.hy.prior("var", "dlnorm",-1.87, 0.4328))

plot(model)
results <- model.processing(model)

list.estimates=list()

for (i in 1:nrow(relative.effect.table(results))) {
  temp=round(exp(relative.effect.table(results)),2)[i,,] %>% as_tibble(rownames = "t1") %>% as.data.frame()
  temp$t2=temp[i,1]
  temp=temp[-c(1:i),]
  list.estimates[[i]] <- temp
}

NMA=bind_rows(list.estimates) %>% select(t1,t2,`50%`,`2.5%`,`50%`,`97.5%`) %>% rename(ci.l.net=`2.5%`,pe.net=`50%`,ci.u.net=`97.5%`) %>%
  filter(!is.na(pe.net))

result.node <- mtc.nodesplit(network, likelihood="binom",link="logit",
                             linearModel="fixed", n.chain =3,
                             n.adapt=10000, n.iter=50000, thin=10,
                             hy.prior=mtc.hy.prior("var", "dlnorm", -1.87, 0.4328))

exp1 <- function(x) (round(exp(x),2))

summary(result.node)[[2]] %>% mutate_at(c("pe","ci.l","ci.u"),exp1) %>% select(t2,t1,pe,ci.l,ci.u) %>% 
  rename(t1=t2,t2=t1,ci.l.ind=ci.l,pe.ind=pe,ci.u.ind=ci.u) %>% 
  right_join(NMA,by=c("t1","t2")) %>% as.data.frame() %>% write.csv(file="pairwise/drugs/output/GRADE_mort_fixed_IL6.csv")
