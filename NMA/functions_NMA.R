library(gemtc)
library(readxl)
library(tidybayes)
library(ggdist)
library(stringr)
library(ggplot2)
library(dplyr)
library(viridis)
library(rjags)


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