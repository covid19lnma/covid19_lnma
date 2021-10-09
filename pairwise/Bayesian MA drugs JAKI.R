wd <- "/home/antonio/covid19_lnma"
setwd(wd)
source("pairwise/functions_MA.R")

mainDir <- paste0(getwd(),"/pairwise/drugs")
subDir <- "output"

output_dir <- file.path(mainDir, subDir)

if (!dir.exists(output_dir)){
  dir.create(output_dir)
}

######## Dichotomous #####
#######################Mortality no oxygen###########################

data=read_excel("pairwise/drugs/Dichotomous outcomes_wide format.xlsx", sheet =1)
data=data %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2)) 

# determine corresponding prior parameters(?TurnerEtAlPrior to help):
TP <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "placebo / control")

TP1 <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "pharma")

placebo <- "placebo"
baseline=data %>% 
  filter(t1==placebo | t2==placebo) %>%
  mutate(rate=c.events/c.total) %>%
  summarise(median=median(rate)) %>% as.numeric()

measure <- "OR"
name <- "mortality_jaki_no_oxygen.csv"

list.estimates <- getestimates(data, TP, TP1, baseline, measure, placebo, name, mainDir)

write.estimates.csv(list.estimates,mainDir, name)

#######################Mortality oxygen###########################

data=read_excel("pairwise/drugs/Dichotomous outcomes_wide format.xlsx", sheet =2)
data=data %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2)) 

# determine corresponding prior parameters(?TurnerEtAlPrior to help):
TP <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "placebo / control")

TP1 <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "pharma")

placebo <- "placebo"
baseline=data %>% 
  filter(t1==placebo | t2==placebo) %>%
  mutate(rate=c.events/c.total) %>%
  summarise(median=median(rate)) %>% as.numeric()

measure <- "OR"
name <- "mortality_jaki_oxygen.csv"

list.estimates <- getestimates(data, TP, TP1, baseline, measure, placebo, name, mainDir)

write.estimates.csv(list.estimates,mainDir, name)

#######################MV no oxygen###########################

data=read_excel("pairwise/drugs/Dichotomous outcomes_wide format.xlsx", sheet =3)
data=data %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2)) 

# determine corresponding prior parameters(?TurnerEtAlPrior to help):
TP <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "placebo / control")

TP1 <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "pharma")

placebo <- "placebo"
baseline=data %>% 
  filter(t1==placebo | t2==placebo) %>%
  mutate(rate=c.events/c.total) %>%
  summarise(median=median(rate)) %>% as.numeric()

measure <- "OR"
name <- "MV_jaki_no_oxygen.csv"

list.estimates <- getestimates(data, TP, TP1, baseline, measure, placebo, name, mainDir)

write.estimates.csv(list.estimates,mainDir, name)

#######################MV oxygen###########################

data=read_excel("pairwise/drugs/Dichotomous outcomes_wide format.xlsx", sheet =4)
data=data %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2)) 

# determine corresponding prior parameters(?TurnerEtAlPrior to help):
TP <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "placebo / control")

TP1 <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "pharma")

placebo <- "placebo"
baseline=data %>% 
  filter(t1==placebo | t2==placebo) %>%
  mutate(rate=c.events/c.total) %>%
  summarise(median=median(rate)) %>% as.numeric()

measure <- "OR"
name <- "MV_jaki_oxygen.csv"

list.estimates <- getestimates(data, TP, TP1, baseline, measure, placebo, name, mainDir)

write.estimates.csv(list.estimates,mainDir, name)

#######################Mortality &MV no oxygen###########################

data=read_excel("pairwise/drugs/Dichotomous outcomes_wide format.xlsx", sheet =5)
data=data %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2)) 

# determine corresponding prior parameters(?TurnerEtAlPrior to help):
TP <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "placebo / control")

TP1 <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "pharma")

placebo <- "placebo"
baseline=data %>% 
  filter(t1==placebo | t2==placebo) %>%
  mutate(rate=c.events/c.total) %>%
  summarise(median=median(rate)) %>% as.numeric()

measure <- "OR"
name <- "mortality_MV_jaki_no_oxygen.csv"

list.estimates <- getestimates(data, TP, TP1, baseline, measure, placebo, name, mainDir)

write.estimates.csv(list.estimates,mainDir, name)

#######################Mortality &MV oxygen###########################

data=read_excel("pairwise/drugs/Dichotomous outcomes_wide format.xlsx", sheet =6)
data=data %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2)) 

# determine corresponding prior parameters(?TurnerEtAlPrior to help):
TP <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "placebo / control")

TP1 <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "pharma")

placebo <- "placebo"
baseline=data %>% 
  filter(t1==placebo | t2==placebo) %>%
  mutate(rate=c.events/c.total) %>%
  summarise(median=median(rate)) %>% as.numeric()

measure <- "OR"
name <- "mortality_MV_jaki_oxygen.csv"

list.estimates <- getestimates(data, TP, TP1, baseline, measure, placebo, name, mainDir)

write.estimates.csv(list.estimates,mainDir, name)

#######################Mortality steroid###########################

data=read_excel("pairwise/drugs/Dichotomous outcomes_wide format.xlsx", sheet =7)
data=data %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2)) 

# determine corresponding prior parameters(?TurnerEtAlPrior to help):
TP <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "placebo / control")

TP1 <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "pharma")

placebo <- "placebo"
baseline=data %>% 
  filter(t1==placebo | t2==placebo) %>%
  mutate(rate=c.events/c.total) %>%
  summarise(median=median(rate)) %>% as.numeric()

measure <- "OR"
name <- "mortality_jaki_steroid.csv"

list.estimates <- getestimates(data, TP, TP1, baseline, measure, placebo, name, mainDir)

write.estimates.csv(list.estimates,mainDir, name)

#######################Mortality no steroid###########################

data=read_excel("pairwise/drugs/Dichotomous outcomes_wide format.xlsx", sheet =8)
data=data %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2)) 

# determine corresponding prior parameters(?TurnerEtAlPrior to help):
TP <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "placebo / control")

TP1 <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "pharma")

placebo <- "placebo"
baseline=data %>% 
  filter(t1==placebo | t2==placebo) %>%
  mutate(rate=c.events/c.total) %>%
  summarise(median=median(rate)) %>% as.numeric()

measure <- "OR"
name <- "mortality_jaki_no_steroid.csv"

list.estimates <- getestimates(data, TP, TP1, baseline, measure, placebo, name, mainDir)

write.estimates.csv(list.estimates,mainDir, name)

#######################Mortality remdesivir###########################

data=read_excel("pairwise/drugs/Dichotomous outcomes_wide format.xlsx", sheet =9)
data=data %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2)) 

# determine corresponding prior parameters(?TurnerEtAlPrior to help):
TP <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "placebo / control")

TP1 <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "pharma")

placebo <- "placebo"
baseline=data %>% 
  filter(t1==placebo | t2==placebo) %>%
  mutate(rate=c.events/c.total) %>%
  summarise(median=median(rate)) %>% as.numeric()

measure <- "OR"
name <- "mortality_jaki_remdesivir.csv"

list.estimates <- getestimates(data, TP, TP1, baseline, measure, placebo, name, mainDir)

write.estimates.csv(list.estimates,mainDir, name)

#######################Mortality no remdesivir###########################

data=read_excel("pairwise/drugs/Dichotomous outcomes_wide format.xlsx", sheet =10)
data=data %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2)) 

# determine corresponding prior parameters(?TurnerEtAlPrior to help):
TP <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "placebo / control")

TP1 <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "pharma")

placebo <- "placebo"
baseline=data %>% 
  filter(t1==placebo | t2==placebo) %>%
  mutate(rate=c.events/c.total) %>%
  summarise(median=median(rate)) %>% as.numeric()

measure <- "OR"
name <- "mortality_jaki_no_remdesivir.csv"

list.estimates <- getestimates(data, TP, TP1, baseline, measure, placebo, name, mainDir)

write.estimates.csv(list.estimates,mainDir, name)

#######################Mortality less than 65###########################

data=read_excel("pairwise/drugs/Dichotomous outcomes_wide format.xlsx", sheet =11)
data=data %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2)) 

# determine corresponding prior parameters(?TurnerEtAlPrior to help):
TP <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "placebo / control")

TP1 <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "pharma")

placebo <- "placebo"
baseline=data %>% 
  filter(t1==placebo | t2==placebo) %>%
  mutate(rate=c.events/c.total) %>%
  summarise(median=median(rate)) %>% as.numeric()

measure <- "OR"
name <- "mortality_jaki_65_less.csv"

list.estimates <- getestimates(data, TP, TP1, baseline, measure, placebo, name, mainDir)

write.estimates.csv(list.estimates,mainDir, name)

#######################Mortality more than 65###########################

data=read_excel("pairwise/drugs/Dichotomous outcomes_wide format.xlsx", sheet =12)
data=data %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2)) 

# determine corresponding prior parameters(?TurnerEtAlPrior to help):
TP <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "placebo / control")

TP1 <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "pharma")

placebo <- "placebo"
baseline=data %>% 
  filter(t1==placebo | t2==placebo) %>%
  mutate(rate=c.events/c.total) %>%
  summarise(median=median(rate)) %>% as.numeric()

measure <- "OR"
name <- "mortality_jaki_65_more.csv"

list.estimates <- getestimates(data, TP, TP1, baseline, measure, placebo, name, mainDir)

write.estimates.csv(list.estimates,mainDir, name)

######## Continuous #####
####################### Time to recovery no oxygen ###########################

data=read_excel("pairwise/drugs/Continuous outcomes_wide format.xlsx", sheet =1)
data=data %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2)) 

# determine corresponding prior parameters(?TurnerEtAlPrior to help):
TP <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "placebo / control")

TP1 <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "pharma")

placebo <- "placebo"
baseline=data %>% 
  filter(t1==placebo | t2==placebo) %>%
  summarise(median=median(mean2)) %>% as.numeric()

measure <- "ROM"
name <- "Time_to_recovery_jaki_no_oxygen.csv"

list.estimates <- getestimates(data, TP, TP1, baseline, measure, placebo, name, mainDir,folderROM="drugs")

write.estimates.csv(list.estimates,mainDir, name)

####################### Time to recovery oxygen ###########################

data=read_excel("pairwise/drugs/Continuous outcomes_wide format.xlsx", sheet =2)
data=data %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2)) 

# determine corresponding prior parameters(?TurnerEtAlPrior to help):
TP <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "placebo / control")

TP1 <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "pharma")

placebo <- "placebo"
baseline=data %>% 
  filter(t1==placebo | t2==placebo) %>%
  summarise(median=median(mean2)) %>% as.numeric()

measure <- "ROM"
name <- "Time_to_recovery_jaki_oxygen.csv"

list.estimates <- getestimates(data, TP, TP1, baseline, measure, placebo, name, mainDir,folderROM="drugs")

write.estimates.csv(list.estimates,mainDir, name)

####################### Time to recovery moderate ###########################

data=read_excel("pairwise/drugs/Continuous outcomes_wide format.xlsx", sheet =3)
data=data %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2)) 

# determine corresponding prior parameters(?TurnerEtAlPrior to help):
TP <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "placebo / control")

TP1 <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "pharma")

placebo <- "placebo"
baseline=data %>% 
  filter(t1==placebo | t2==placebo) %>%
  summarise(median=median(mean2)) %>% as.numeric()

measure <- "ROM"
name <- "Time_to_recovery_jaki_moderate.csv"

list.estimates <- getestimates(data, TP, TP1, baseline, measure, placebo, name, mainDir,folderROM="drugs")

write.estimates.csv(list.estimates,mainDir, name)

####################### Time to recovery severe ###########################

data=read_excel("pairwise/drugs/Continuous outcomes_wide format.xlsx", sheet =4)
data=data %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2)) 

# determine corresponding prior parameters(?TurnerEtAlPrior to help):
TP <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "placebo / control")

TP1 <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "pharma")

placebo <- "placebo"
baseline=data %>% 
  filter(t1==placebo | t2==placebo) %>%
  summarise(median=median(mean2)) %>% as.numeric()

measure <- "ROM"
name <- "Time_to_recovery_jaki_severe.csv"

list.estimates <- getestimates(data, TP, TP1, baseline, measure, placebo, name, mainDir,folderROM="drugs")

write.estimates.csv(list.estimates,mainDir, name)

####################### Duration of Hospitalization steroid ###########################

data=read_excel("pairwise/drugs/Continuous outcomes_wide format.xlsx", sheet =5)
data=data %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2)) 

# determine corresponding prior parameters(?TurnerEtAlPrior to help):
TP <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "placebo / control")

TP1 <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "pharma")

placebo <- "placebo"
baseline=data %>% 
  filter(t1==placebo | t2==placebo) %>%
  summarise(median=median(mean2)) %>% as.numeric()

measure <- "MD"
name <- "Duration_of_hospitalization_jaki_steroid.csv"

list.estimates <- getestimates(data, TP, TP1, baseline, measure, placebo, name, mainDir,folderROM="drugs")

write.estimates.csv(list.estimates,mainDir, name)

####################### Duration of Hospitalization no steroid ###########################

data=read_excel("pairwise/drugs/Continuous outcomes_wide format.xlsx", sheet =6)
data=data %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2)) 

# determine corresponding prior parameters(?TurnerEtAlPrior to help):
TP <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "placebo / control")

TP1 <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "pharma")

placebo <- "placebo"
baseline=data %>% 
  filter(t1==placebo | t2==placebo) %>%
  summarise(median=median(mean2)) %>% as.numeric()

measure <- "MD"
name <- "Duration_of_hospitalization_jaki_no_steroid.csv"

list.estimates <- getestimates(data, TP, TP1, baseline, measure, placebo, name, mainDir,folderROM="drugs")

write.estimates.csv(list.estimates,mainDir, name)

####################### Duration of Hospitalization no remdesivir ###########################

data=read_excel("pairwise/drugs/Continuous outcomes_wide format.xlsx", sheet =7)
data=data %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2)) 

# determine corresponding prior parameters(?TurnerEtAlPrior to help):
TP <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "placebo / control")

TP1 <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "pharma")

placebo <- "placebo"
baseline=data %>% 
  filter(t1==placebo | t2==placebo) %>%
  summarise(median=median(mean2)) %>% as.numeric()

measure <- "MD"
name <- "Duration_of_hospitalization_jaki_no_remdesivir.csv"

list.estimates <- getestimates(data, TP, TP1, baseline, measure, placebo, name, mainDir,folderROM="drugs")

write.estimates.csv(list.estimates,mainDir, name)

####################### Duration of Hospitalization less than 65 ###########################

data=read_excel("pairwise/drugs/Continuous outcomes_wide format.xlsx", sheet =8)
data=data %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2)) 

# determine corresponding prior parameters(?TurnerEtAlPrior to help):
TP <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "placebo / control")

TP1 <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "pharma")

placebo <- "placebo"
baseline=data %>% 
  filter(t1==placebo | t2==placebo) %>%
  summarise(median=median(mean2)) %>% as.numeric()

measure <- "MD"
name <- "Duration_of_hospitalization_jaki_65_less.csv"

list.estimates <- getestimates(data, TP, TP1, baseline, measure, placebo, name, mainDir,folderROM="drugs")

write.estimates.csv(list.estimates,mainDir, name)

####################### Duration of Hospitalization more than 65 ###########################

data=read_excel("pairwise/drugs/Continuous outcomes_wide format.xlsx", sheet =9)
data=data %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2)) 

# determine corresponding prior parameters(?TurnerEtAlPrior to help):
TP <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "placebo / control")

TP1 <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "pharma")

placebo <- "placebo"
baseline=data %>% 
  filter(t1==placebo | t2==placebo) %>%
  summarise(median=median(mean2)) %>% as.numeric()

measure <- "MD"
name <- "Duration_of_hospitalization_jaki_65_more.csv"

list.estimates <- getestimates(data, TP, TP1, baseline, measure, placebo, name, mainDir,folderROM="drugs")

write.estimates.csv(list.estimates,mainDir, name)

####################### ICU lenght of stay steroid ###########################

data=read_excel("pairwise/drugs/Continuous outcomes_wide format.xlsx", sheet =10)
data=data %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2)) 

# determine corresponding prior parameters(?TurnerEtAlPrior to help):
TP <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "placebo / control")

TP1 <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "pharma")

placebo <- "placebo"
baseline=data %>% 
  filter(t1==placebo | t2==placebo) %>%
  summarise(median=median(mean2)) %>% as.numeric()

measure <- "MD"
name <- "ICU_stay_jaki_steroid.csv"

list.estimates <- getestimates(data, TP, TP1, baseline, measure, placebo, name, mainDir,folderROM="drugs")

write.estimates.csv(list.estimates,mainDir, name)

####################### ICU lenght of stay no steroid ###########################

data=read_excel("pairwise/drugs/Continuous outcomes_wide format.xlsx", sheet =11)
data=data %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2)) 

# determine corresponding prior parameters(?TurnerEtAlPrior to help):
TP <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "placebo / control")

TP1 <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "pharma")

placebo <- "placebo"
baseline=data %>% 
  filter(t1==placebo | t2==placebo) %>%
  summarise(median=median(mean2)) %>% as.numeric()

measure <- "MD"
name <- "ICU_stay_jaki_no_steroid.csv"

list.estimates <- getestimates(data, TP, TP1, baseline, measure, placebo, name, mainDir,folderROM="drugs")

write.estimates.csv(list.estimates,mainDir, name)

####################### ICU lenght of stay no remdesivir ###########################

data=read_excel("pairwise/drugs/Continuous outcomes_wide format.xlsx", sheet =12)
data=data %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2)) 

# determine corresponding prior parameters(?TurnerEtAlPrior to help):
TP <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "placebo / control")

TP1 <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "pharma")

placebo <- "placebo"
baseline=data %>% 
  filter(t1==placebo | t2==placebo) %>%
  summarise(median=median(mean2)) %>% as.numeric()

measure <- "MD"
name <- "ICU_stay_jaki_no_remdesivir.csv"

list.estimates <- getestimates(data, TP, TP1, baseline, measure, placebo, name, mainDir,folderROM="drugs")

write.estimates.csv(list.estimates,mainDir, name)

####################### ICU lenght of stay less than 65 ###########################

data=read_excel("pairwise/drugs/Continuous outcomes_wide format.xlsx", sheet =13)
data=data %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2)) 

# determine corresponding prior parameters(?TurnerEtAlPrior to help):
TP <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "placebo / control")

TP1 <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "pharma")

placebo <- "placebo"
baseline=data %>% 
  filter(t1==placebo | t2==placebo) %>%
  summarise(median=median(mean2)) %>% as.numeric()

measure <- "MD"
name <- "ICU_stay_jaki_65_less.csv"

list.estimates <- getestimates(data, TP, TP1, baseline, measure, placebo, name, mainDir,folderROM="drugs")

write.estimates.csv(list.estimates,mainDir, name)

####################### ICU lenght of stay more than 65 ###########################

data=read_excel("pairwise/drugs/Continuous outcomes_wide format.xlsx", sheet =14)
data=data %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2)) 

# determine corresponding prior parameters(?TurnerEtAlPrior to help):
TP <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "placebo / control")

TP1 <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "pharma")

placebo <- "placebo"
baseline=data %>% 
  filter(t1==placebo | t2==placebo) %>%
  summarise(median=median(mean2)) %>% as.numeric()

measure <- "MD"
name <- "ICU_stay_jaki_65_more.csv"

list.estimates <- getestimates(data, TP, TP1, baseline, measure, placebo, name, mainDir,folderROM="drugs")

write.estimates.csv(list.estimates,mainDir, name)

####################### Ventilator free days steroids ###########################

data=read_excel("pairwise/drugs/Continuous outcomes_wide format.xlsx", sheet =15)
data=data %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2)) 

# determine corresponding prior parameters(?TurnerEtAlPrior to help):
TP <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "placebo / control")

TP1 <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "pharma")

placebo <- "placebo"
baseline=data %>% 
  filter(t1==placebo | t2==placebo) %>%
  summarise(median=median(mean2)) %>% as.numeric()

measure <- "MD"
name <- "Ventilator_free_days_jaki_steroids.csv"

list.estimates <- getestimates(data, TP, TP1, baseline, measure, placebo, name, mainDir,folderROM="drugs")

write.estimates.csv(list.estimates,mainDir, name)

####################### Ventilator free days no steroids ###########################

data=read_excel("pairwise/drugs/Continuous outcomes_wide format.xlsx", sheet =16)
data=data %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2)) 

# determine corresponding prior parameters(?TurnerEtAlPrior to help):
TP <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "placebo / control")

TP1 <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "pharma")

placebo <- "placebo"
baseline=data %>% 
  filter(t1==placebo | t2==placebo) %>%
  summarise(median=median(mean2)) %>% as.numeric()

measure <- "MD"
name <- "Ventilator_free_days_jaki_no_steroids.csv"

list.estimates <- getestimates(data, TP, TP1, baseline, measure, placebo, name, mainDir,folderROM="drugs")

write.estimates.csv(list.estimates,mainDir, name)

####################### Ventilator free days no remdesivir ###########################

data=read_excel("pairwise/drugs/Continuous outcomes_wide format.xlsx", sheet =17)
data=data %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2)) 

# determine corresponding prior parameters(?TurnerEtAlPrior to help):
TP <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "placebo / control")

TP1 <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "pharma")

placebo <- "placebo"
baseline=data %>% 
  filter(t1==placebo | t2==placebo) %>%
  summarise(median=median(mean2)) %>% as.numeric()

measure <- "MD"
name <- "Ventilator_free_days_jaki_no_remdesivir.csv"

list.estimates <- getestimates(data, TP, TP1, baseline, measure, placebo, name, mainDir,folderROM="drugs")

write.estimates.csv(list.estimates,mainDir, name)

####################### Ventilator free days less than 65 ###########################

data=read_excel("pairwise/drugs/Continuous outcomes_wide format.xlsx", sheet =18)
data=data %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2)) 

# determine corresponding prior parameters(?TurnerEtAlPrior to help):
TP <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "placebo / control")

TP1 <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "pharma")

placebo <- "placebo"
baseline=data %>% 
  filter(t1==placebo | t2==placebo) %>%
  summarise(median=median(mean2)) %>% as.numeric()

measure <- "MD"
name <- "Ventilator_free_days_jaki_65_less.csv"

list.estimates <- getestimates(data, TP, TP1, baseline, measure, placebo, name, mainDir,folderROM="drugs")

write.estimates.csv(list.estimates,mainDir, name)

####################### Ventilator free days more than 65 ###########################

data=read_excel("pairwise/drugs/Continuous outcomes_wide format.xlsx", sheet =19)
data=data %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2)) 

# determine corresponding prior parameters(?TurnerEtAlPrior to help):
TP <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "placebo / control")

TP1 <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "pharma")

placebo <- "placebo"
baseline=data %>% 
  filter(t1==placebo | t2==placebo) %>%
  summarise(median=median(mean2)) %>% as.numeric()

measure <- "MD"
name <- "Ventilator_free_days_jaki_65_more.csv"

list.estimates <- getestimates(data, TP, TP1, baseline, measure, placebo, name, mainDir,folderROM="drugs")

write.estimates.csv(list.estimates,mainDir, name)