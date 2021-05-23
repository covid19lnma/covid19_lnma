source("functions_MA.R")

############################################################################################
#######################Mortality ###########################
##########################################################################################

data=read.csv("prophylaxis/AEs - wide data format.csv")
data=data %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2))

# determine corresponding prior parameters(?TurnerEtAlPrior to help):
TP <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "placebo / control")

TP1 <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "pharma")

baseline=data %>% filter(t1=="standard care/placebo" | t2=="standard care/placebo") %>%
  mutate(rate=c.events/c.total) %>%
  summarise(median=median(rate)) %>% as.numeric()

measure <- "OR"
name <- "AE_prophylaxis.csv"
folder <- "prophylaxis"


list.estimates <- getestimates(data, TP, TP1, baseline, measure, name,folder)

write.estimates.csv(list.estimates, folder,name)
