source("functions_MA.R")

############################################################################################
#######################Mortality ###########################
##########################################################################################

data=read.csv("blood/mortality - wide data format.csv")
data=data %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2)) %>% 
  filter(t1=="casirivimab, imdevimab")



# determine corresponding prior parameters(?TurnerEtAlPrior to help):
TP <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "placebo / control")

TP1 <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "pharma")

baseline=data %>% filter(t1=="placebo/standard care" | t2=="placebo/standard care") %>%
  mutate(rate=c.events/c.total) %>%
  summarise(median=median(rate)) %>% as.numeric()

measure <- "OR"
name <- "mortality_regn.csv"
folder <- "blood"

list.estimates <- getestimates(data, TP, TP1, baseline, measure, name,folder)

write.estimates.csv(list.estimates,folder, name)

############################################################################################
#######################Mechanical Ventilation ###########################
##########################################################################################

data=read.csv("blood/mechanical ventilation - wide data format.csv")
data=data %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2)) %>% 
  filter(t1=="casirivimab, imdevimab")



# determine corresponding prior parameters(?TurnerEtAlPrior to help):
TP <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "placebo / control")

TP1 <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "pharma")

baseline=data %>% filter(t1=="placebo/standard care" | t2=="placebo/standard care") %>%
  mutate(rate=c.events/c.total) %>%
  summarise(median=median(rate)) %>% as.numeric()

measure <- "OR"
name <- "mv_regn.csv"
folder <- "blood"

list.estimates <- getestimates(data, TP, TP1, baseline, measure, name,folder)

write.estimates.csv(list.estimates,folder, name)

############################################################################################
#######################Duration of hosp ###########################
##########################################################################################

data=read.csv("blood/Duration of hospitalization - wide data.csv")
data=data %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2)) %>% 
  filter(t1=="casirivimab, imdevimab")



# determine corresponding prior parameters(?TurnerEtAlPrior to help):
TP <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "placebo / control")

TP1 <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "pharma")

baseline=data %>% filter(t1=="placebo/standard care" | t2=="placebo/standard care") %>%
  summarise(median=median(mean2)) %>% as.numeric()

measure <- "MD"
name <- "Duration_of_hosp_regn.csv"
folder <- "blood"

list.estimates <- getestimates(data, TP, TP1, baseline, measure, name,folder)

write.estimates.csv(list.estimates,folder, name)

############################################################################################
######################admission to hosp ###########################
##########################################################################################

data=read.csv("blood/admission to hospital - wide data format.csv")
data=data %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2)) %>% 
  filter(t1=="casirivimab, imdevimab")



# determine corresponding prior parameters(?TurnerEtAlPrior to help):
TP <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "placebo / control")

TP1 <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "pharma")

baseline=data %>% filter(t1=="placebo/standard care" | t2=="placebo/standard care") %>%
  mutate(rate=c.events/c.total) %>%
  summarise(median=median(rate)) %>% as.numeric()

measure <- "OR"
name <- "admission_hosp_regn.csv"
folder <- "blood"

list.estimates <- getestimates(data, TP, TP1, baseline, measure, name,folder)

write.estimates.csv(list.estimates,folder, name)

############################################################################################
######################AE ###########################
##########################################################################################

data=read.csv("blood/adverse effects leading to discontinuation - wide data format.csv")
data=data %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2)) %>% 
  filter(t1=="casirivimab, imdevimab")



# determine corresponding prior parameters(?TurnerEtAlPrior to help):
TP <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "placebo / control")

TP1 <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "pharma")

baseline=data %>% filter(t1=="placebo/standard care" | t2=="placebo/standard care") %>%
  mutate(rate=c.events/c.total) %>%
  summarise(median=median(rate)) %>% as.numeric()

measure <- "OR"
name <- "AE_disc_regn.csv"
folder <- "blood"

list.estimates <- getestimates(data, TP, TP1, baseline, measure, name,folder)

write.estimates.csv(list.estimates,folder, name)

############################################################################################
######################Allergic ###########################
##########################################################################################

data=read.csv("blood/allergic reactions - wide data format.csv")
data=data %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2)) %>% 
  filter(t1=="casirivimab, imdevimab")



# determine corresponding prior parameters(?TurnerEtAlPrior to help):
TP <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "placebo / control")

TP1 <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "pharma")

baseline=data %>% filter(t1=="placebo/standard care" | t2=="placebo/standard care") %>%
  mutate(rate=c.events/c.total) %>%
  summarise(median=median(rate)) %>% as.numeric()

measure <- "OR"
name <- "allergic_regn.csv"
folder <- "blood"

list.estimates <- getestimates(data, TP, TP1, baseline, measure, name,folder)

write.estimates.csv(list.estimates,folder, name)


############################################################################################
######################Time to symptom ###########################
##########################################################################################

data=read.csv("blood/Time to symptom resolution - wide data.csv")
data=data %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2)) %>% 
  filter(t1=="casirivimab, imdevimab")



# determine corresponding prior parameters(?TurnerEtAlPrior to help):
TP <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "placebo / control")

TP1 <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "pharma")

baseline=data %>% filter(t1=="placebo/standard care" | t2=="placebo/standard care") %>%
  summarise(median=median(mean2)) %>% as.numeric()

measure <- "ROM"
name <- "Time_symptom_regn.csv"
folder <- "blood"

list.estimates <- getestimates(data, TP, TP1, baseline, measure, name,folder)

write.estimates.csv(list.estimates,folder, name)


################## 7-17-2021 ##################################################

##### admission to hospital ########

data=read.csv("blood/admission to hospital - wide data format.csv")
data=data %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2))



# determine corresponding prior parameters(?TurnerEtAlPrior to help):
TP <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "placebo / control")

TP1 <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "pharma")

baseline=data %>% filter(t1=="placebo/standard care" | t2=="placebo/standard care") %>%
  mutate(rate=c.events/c.total) %>%
  summarise(median=median(rate)) %>% as.numeric()


measure <- "OR"
name <- "admission_hop.csv"
folder <- "blood"

list.estimates <- getestimates(data, TP, TP1, baseline, measure, name,folder)

write.estimates.csv(list.estimates,folder, name)


##### Time to viral clereaance ########

data=read.csv("blood/Time to viral clearance_wide.csv")
data=data %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2))



# determine corresponding prior parameters(?TurnerEtAlPrior to help):
TP <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "placebo / control")

TP1 <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "pharma")

baseline=data %>% filter(t1=="placebo/standard care" | t2=="placebo/standard care") %>%
  summarise(median=median(mean2)) %>% as.numeric()


measure <- "ROM"
name <- "time_clear.csv"
folder <- "blood"

list.estimates <- getestimates(data, TP, TP1, baseline, measure, name,folder)

write.estimates.csv(list.estimates,folder, name)
