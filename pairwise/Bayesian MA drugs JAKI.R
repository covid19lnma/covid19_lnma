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
data=data %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2)) 

# determine corresponding prior parameters(?TurnerEtAlPrior to help):
TP <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "placebo / control")

TP1 <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "pharma")

placebo <- "standard care/placebo"
baseline=data %>% 
  filter(t1==placebo | t2==placebo) %>%
  mutate(rate=c.events/c.total) %>%
  summarise(median=median(rate)) %>% as.numeric()

measure <- "OR"
name <- "mortality_jaki.csv"

list.estimates <- getestimates(data, TP, TP1, baseline, measure, placebo, name, mainDir)

write.estimates.csv(list.estimates,mainDir, name)

#######################MV###########################

data=read.csv("pairwise/drugs/mechanical ventilation - wide data format.csv")
data=data %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2)) 

# determine corresponding prior parameters(?TurnerEtAlPrior to help):
TP <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "placebo / control")

TP1 <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "pharma")

placebo <- "standard care/placebo"
baseline=data %>% 
  filter(t1==placebo | t2==placebo) %>%
  mutate(rate=c.events/c.total) %>%
  summarise(median=median(rate)) %>% as.numeric()

measure <- "OR"
name <- "MV_jaki.csv"

list.estimates <- getestimates(data, TP, TP1, baseline, measure, placebo, name, mainDir)

write.estimates.csv(list.estimates,mainDir, name)

#######################AE###########################

data=read.csv("pairwise/drugs/Adverse effects leading to discont - wide data format.csv")
data=data %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2)) 

# determine corresponding prior parameters(?TurnerEtAlPrior to help):
TP <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "placebo / control")

TP1 <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "pharma")

placebo <- "standard care/placebo"
baseline=data %>% 
  filter(t1==placebo | t2==placebo) %>%
  mutate(rate=c.events/c.total) %>%
  summarise(median=median(rate)) %>% as.numeric()

measure <- "OR"
name <- "AE_jaki.csv"

list.estimates <- getestimates(data, TP, TP1, baseline, measure, placebo, name, mainDir)

write.estimates.csv(list.estimates,mainDir, name)
######## Continuous #####
####################### Time to symptom resolution ###########################

data=read.csv("pairwise/drugs/Time to symptom resolution_wide data.csv")
data=data %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2)) 

# determine corresponding prior parameters(?TurnerEtAlPrior to help):
TP <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "placebo / control")

TP1 <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "pharma")

placebo <- "standard care/placebo"
baseline=data %>% 
  filter(t1==placebo | t2==placebo) %>%
  summarise(median=median(mean2)) %>% as.numeric()

measure <- "ROM"
name <- "Time_to_symptom_resolution_jaki.csv"

list.estimates <- getestimates(data, TP, TP1, baseline, measure, placebo, name, mainDir,folderROM="drugs")

write.estimates.csv(list.estimates,mainDir, name)

####################### Time to viral clearance ###########################

data=read.csv("pairwise/drugs/Time to viral clearance_wide data.csv")
data=data %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2)) 

# determine corresponding prior parameters(?TurnerEtAlPrior to help):
TP <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "placebo / control")

TP1 <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "pharma")

placebo <- "standard care/placebo"
baseline=data %>% 
  filter(t1==placebo | t2==placebo) %>%
  summarise(median=median(mean2)) %>% as.numeric()

measure <- "ROM"
name <- "Time_to_viral_clearance_jaki.csv"

list.estimates <- getestimates(data, TP, TP1, baseline, measure, placebo, name, mainDir,folderROM="drugs")

write.estimates.csv(list.estimates,mainDir, name)

####################### Duration of Hospitalization ###########################

data=read.csv("pairwise/drugs/Duration of hospitalization_wide data.csv")
data=data %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2)) 

# determine corresponding prior parameters(?TurnerEtAlPrior to help):
TP <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "placebo / control")

TP1 <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "pharma")

placebo <- "standard care/placebo"
baseline=data %>% 
  filter(t1==placebo | t2==placebo) %>%
  summarise(median=median(mean2)) %>% as.numeric()

measure <- "MD"
name <- "Duration_of_hospitalization_jaki.csv"

list.estimates <- getestimates(data, TP, TP1, baseline, measure, placebo, name, mainDir,folderROM="drugs")

write.estimates.csv(list.estimates,mainDir, name)

####################### Duration of ventilation ###########################

data=read.csv("pairwise/drugs/Duration of ventilation_wide data.csv")
data=data %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2)) 

# determine corresponding prior parameters(?TurnerEtAlPrior to help):
TP <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "placebo / control")

TP1 <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "pharma")

placebo <- "standard care/placebo"
baseline=data %>% 
  filter(t1==placebo | t2==placebo) %>%
  summarise(median=median(mean2)) %>% as.numeric()

measure <- "MD"
name <- "Duration_of_ventilation_jaki.csv"

list.estimates <- getestimates(data, TP, TP1, baseline, measure, placebo, name, mainDir,folderROM="drugs")

write.estimates.csv(list.estimates,mainDir, name)

####################### ICU lenght of stay ###########################

data=read.csv("pairwise/drugs/Duration of hospitalization_wide data.csv")
data=data %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2)) 

# determine corresponding prior parameters(?TurnerEtAlPrior to help):
TP <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "placebo / control")

TP1 <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "pharma")

placebo <- "standard care/placebo"
baseline=data %>% 
  filter(t1==placebo | t2==placebo) %>%
  summarise(median=median(mean2)) %>% as.numeric()

measure <- "MD"
name <- "ICU_stay_jaki.csv"

list.estimates <- getestimates(data, TP, TP1, baseline, measure, placebo, name, mainDir,folderROM="drugs")

write.estimates.csv(list.estimates,mainDir, name)

####################### Ventilator free days ###########################

data=read.csv("pairwise/drugs/Ventilator-free days_wide data.csv")
data=data %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2)) 

# determine corresponding prior parameters(?TurnerEtAlPrior to help):
TP <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "placebo / control")

TP1 <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "pharma")

placebo <- "standard care/placebo"
baseline=data %>% 
  filter(t1==placebo | t2==placebo) %>%
  summarise(median=median(mean2)) %>% as.numeric()

measure <- "MD"
name <- "Ventilator_free_days_jaki.csv"

list.estimates <- getestimates(data, TP, TP1, baseline, measure, placebo, name, mainDir,folderROM="drugs")

write.estimates.csv(list.estimates,mainDir, name)