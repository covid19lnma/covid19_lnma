# wd <- "/home/antonio/covid19_lnma"
# setwd(wd)
source("pairwise/functions_MA.R")

mainDir <- paste0(getwd(),"/pairwise/blood")
subDir <- "output"

output_dir <- file.path(mainDir, subDir)

if (!dir.exists(output_dir)){
  dir.create(output_dir)
}
##### Safety outcomes#####

#######################AE###########################

data=read.csv("pairwise/blood/adverse effects leading to discontinuation - wide data format.csv")
data=data %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2)) 

# determine corresponding prior parameters(?TurnerEtAlPrior to help):
TP <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "placebo / control")

TP1 <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "pharma")

placebo <- "placebo/standard care"
baseline=data %>% 
  filter(t1==placebo | t2==placebo) %>%
  mutate(rate=c.events/c.total) %>%
  summarise(median=median(rate)) %>% as.numeric()

measure <- "RD"
name <- "AE_sotro.csv"

list.estimates <- getestimates(data, TP, TP1, baseline, measure, placebo, name, mainDir, folderROM="blood")

write.estimates.csv(list.estimates,mainDir, name)

#######################Allergic ###########################

data=read.csv("pairwise/blood/allergic reactions - wide data format.csv")
data=data %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2)) 

# determine corresponding prior parameters(?TurnerEtAlPrior to help):
TP <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "placebo / control")

TP1 <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "pharma")

placebo <- "placebo/standard care"
baseline=data %>% 
  filter(t1==placebo | t2==placebo) %>%
  mutate(rate=c.events/c.total) %>%
  summarise(median=median(rate)) %>% as.numeric()

measure <- "RD"
name <- "allergic_reac_sotro.csv"

list.estimates <- getestimates(data, TP, TP1, baseline, measure, placebo, name, mainDir, folderROM="blood")

write.estimates.csv(list.estimates,mainDir, name)

##### non severe####
######## Dichotomous #####
#######################Mortality ###########################

data=read.csv("pairwise/blood/not severe/mortality - wide data format.csv")
data=data %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2)) 

# determine corresponding prior parameters(?TurnerEtAlPrior to help):
TP <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "placebo / control")

TP1 <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "pharma")

placebo <- "placebo/standard care"
baseline=data %>%
  filter(t1==placebo | t2==placebo) %>%
  mutate(rate=c.events/c.total) %>%
  summarise(median=median(rate)) %>% as.numeric()

measure <- "OR"
name <- "mortality_sotro.csv"

list.estimates <- getestimates(data, TP, TP1, baseline, measure, placebo, name, mainDir)

write.estimates.csv(list.estimates,mainDir, name)

#######################MV###########################

data=read.csv("pairwise/blood/not severe/mechanical ventilation - wide data format.csv")
data=data %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2)) 

# determine corresponding prior parameters(?TurnerEtAlPrior to help):
TP <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "placebo / control")

TP1 <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "pharma")

placebo <- "placebo/standard care"
baseline=data %>%
  filter(t1==placebo | t2==placebo) %>%
  mutate(rate=c.events/c.total) %>%
  summarise(median=median(rate)) %>% as.numeric()

measure <- "OR"
name <- "MV_sotro.csv"

list.estimates <- getestimates(data, TP, TP1, baseline, measure, placebo, name, mainDir)

write.estimates.csv(list.estimates,mainDir, name)

####################### admission to hospital ###########################

data=read.csv("pairwise/blood/not severe/admission to hospital - wide data format.csv")
data=data %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2),
                     t2=if_else(t2=="","standard care/placebo",t2)) 

# determine corresponding prior parameters(?TurnerEtAlPrior to help):
TP <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "placebo / control")

TP1 <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "pharma")

placebo <- "placebo/standard care"
baseline=data %>% 
  filter(t1==placebo | t2==placebo) %>%
  mutate(rate=c.events/c.total) %>%
  summarise(median=median(rate)) %>% as.numeric()

measure <- "OR"
name <- "admission_to_hospital_sotro.csv"

list.estimates <- getestimates(data, TP, TP1, baseline, measure, placebo, name, mainDir)

write.estimates.csv(list.estimates,mainDir, name)

######## Continuous #####

####################### Duration of Hospitalization ###########################

data=read.csv("pairwise/blood/not severe/Duration of hospitalization_wide data.csv")
data=data %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2)) 

# determine corresponding prior parameters(?TurnerEtAlPrior to help):
TP <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "placebo / control")

TP1 <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "pharma")

placebo <- "placebo/standard care"
baseline=data %>% 
  filter(t1==placebo | t2==placebo) %>%
  summarise(median=median(mean2)) %>% as.numeric()

measure <- "MD"
name <- "Duration_of_hospitalization_sotro.csv"

list.estimates <- getestimates(data, TP, TP1, baseline, measure, placebo, name, mainDir)

write.estimates.csv(list.estimates,mainDir, name)

####################### Time to symptom resolution ###########################

data=read.csv("pairwise/blood/not severe/Time to symptom resolution_wide data.csv")
data=data %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2)) 

# determine corresponding prior parameters(?TurnerEtAlPrior to help):
TP <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "placebo / control")

TP1 <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "pharma")

placebo <- "placebo/standard care"
baseline=data %>% 
  filter(t1==placebo | t2==placebo) %>%
  summarise(median=median(mean2)) %>% as.numeric()

measure <- "ROM"
name <- "Time_to_symptom_resolution_sotro.csv"

list.estimates <- getestimates(data, TP, TP1, baseline, measure, placebo, name, mainDir, folderROM="blood")

write.estimates.csv(list.estimates,mainDir, name)
