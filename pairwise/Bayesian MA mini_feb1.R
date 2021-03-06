# wd <- "/home/antonio/covid19_lnma"
# setwd(wd)
source("pairwise/functions_MA.R")

mainDir <- paste0(getwd(),"/pairwise/drugs")
subDir <- "output"

output_dir <- file.path(mainDir, subDir)

if (!dir.exists(output_dir)){
  dir.create(output_dir)
}

######## Dichotomous #####
#######################Mortality ###########################

data=read.csv("pairwise/drugs/mortality - wide data format.csv")
data=data %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2)) %>% 
  filter(study!="Wang_1")

# determine corresponding prior parameters(?TurnerEtAlPrior to help):
TP <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "placebo / control")

TP1 <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "pharma")

placebo <- "standard care/placebo"
# baseline=data %>%
#   filter(t1==placebo | t2==placebo) %>%
#   filter(c.total!=0) %>%
#   mutate(rate=c.events/c.total) %>%
#   summarise(median=median(rate)) %>% as.numeric()

baseline=.0019

measure <- "OR"
name <- "mortality.csv"

list.estimates <- getestimates(data, TP, TP1, baseline, measure, placebo, name, mainDir)

write.estimates.csv(list.estimates,mainDir, name)

#######################MV###########################

data=read.csv("pairwise/drugs/mechanical ventilation - wide data format.csv")
data=data %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2)) %>% 
  filter(study!="Wang_1") 

# determine corresponding prior parameters(?TurnerEtAlPrior to help):
TP <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "placebo / control")

TP1 <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "pharma")

placebo <- "standard care/placebo"
# baseline=data %>%
#   filter(t1==placebo | t2==placebo) %>%
#   filter(c.total!=0) %>%
#   mutate(rate=c.events/c.total) %>%
#   summarise(median=median(rate)) %>% as.numeric()

baseline=.0106

measure <- "OR"
name <- "MV.csv"

list.estimates <- getestimates(data, TP, TP1, baseline, measure, placebo, name, mainDir)

write.estimates.csv(list.estimates,mainDir, name)

#######################AE###########################

data=read.csv("pairwise/drugs/Adverse effects leading to discont - wide data format.csv")
data=data %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2)) %>% 
  filter(study!="Wang_1")

# determine corresponding prior parameters(?TurnerEtAlPrior to help):
TP <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "placebo / control")

TP1 <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "pharma")

placebo <- "standard care/placebo"
baseline=data %>%
  filter(t1==placebo | t2==placebo) %>%
  filter(c.total!=0) %>%
  mutate(rate=c.events/c.total) %>%
  summarise(median=median(rate)) %>% as.numeric()

measure <- "RD"
name <- "AE.csv"

list.estimates <- getestimates(data, TP, TP1, baseline, measure, placebo, name, mainDir,folderROM="drugs")

write.estimates.csv(list.estimates,mainDir, name)

####################### admission to hospital ######

data=read.csv("pairwise/drugs/admission to hospital - wide data format.csv")
data=data %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2)) %>% 
  filter(study!="Wang_1")

# determine corresponding prior parameters(?TurnerEtAlPrior to help):
TP <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "placebo / control")

TP1 <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "pharma")

placebo <- "standard care/placebo"
# baseline=data %>%
#   filter(t1==placebo | t2==placebo) %>%
#   filter(c.total!=0) %>%
#   mutate(rate=c.events/c.total) %>%
#   summarise(median=median(rate)) %>% as.numeric()
baseline=.0213

measure <- "OR"
name <- "admission_to_hospital.csv"

list.estimates <- getestimates(data, TP, TP1, baseline, measure, placebo, name, mainDir)

write.estimates.csv(list.estimates,mainDir, name)

######## Continuous #####

####################### Time to symptom resolution ###########################

data=read.csv("pairwise/drugs/Time to symptom resolution_wide data.csv")
data=data %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2)) %>% 
  filter(study!="Wang_1")

# determine corresponding prior parameters(?TurnerEtAlPrior to help):
TP <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "placebo / control")

TP1 <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "pharma")

placebo <- "standard care/placebo"
# baseline=data %>% 
#   filter(t1==placebo | t2==placebo) %>%
#   summarise(median=median(mean2)) %>% as.numeric()

baseline=12.34

measure <- "ROM"
name <- "Time_to_symptom_resolution.csv"

list.estimates <- getestimates(data, TP, TP1, baseline, measure, placebo, name, mainDir,folderROM="drugs")

write.estimates.csv(list.estimates,mainDir, name)