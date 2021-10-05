wd <- "/home/antonio/covid19_lnma"
setwd(wd)
source("pairwise/functions_MA.R")

mainDir <- paste0(getwd(),"/pairwise/drugs")
subDir <- "output"

output_dir <- file.path(mainDir, subDir)

if (!dir.exists(output_dir)){
  dir.create(output_dir)
}

#######################Mortality ###########################

data=read.csv("pairwise/drugs/mortality - wide data format.csv")
data=data %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2)) 

# determine corresponding prior parameters(?TurnerEtAlPrior to help):
TP <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "placebo / control")

TP1 <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "pharma")

baseline=.13

measure <- "OR"
name <- "mortality.csv"

list.estimates <- getestimates(data, TP, TP1, baseline, measure, name, mainDir)

write.estimates.csv(list.estimates,mainDir, name)

####################### admission to hospital ###########################

data=read.csv("pairwise/drugs/admission to hospital - wide data format.csv")
data=data %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2),
                     t2=if_else(t2=="","standard care/placebo",t2)) 

# determine corresponding prior parameters(?TurnerEtAlPrior to help):
TP <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "placebo / control")

TP1 <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "pharma")

baseline=data %>% 
  filter(t1=="placebo/standard care" | t1=="standard care/placebo"| t2=="standard care/placebo" |t2=="placebo/standard care") %>%
  mutate(rate=c.events/c.total) %>%
  summarise(median=median(rate)) %>% as.numeric()

measure <- "OR"
name <- "admission_to_hospital.csv"

list.estimates <- getestimates(data, TP, TP1, baseline, measure, name, mainDir)

write.estimates.csv(list.estimates,mainDir, name)

####################### adverse effects disc ###########################

data=read.csv("pairwise/drugs/Adverse effects leading to discont - wide data format.csv")
data=data %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2)) 

# determine corresponding prior parameters(?TurnerEtAlPrior to help):
TP <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "placebo / control")

TP1 <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "pharma")

baseline=data %>% 
  filter(t1=="placebo/standard care" | t1=="standard care/placebo"| t2=="standard care/placebo" |t2=="placebo/standard care") %>%
  mutate(rate=c.events/c.total) %>%
  summarise(median=median(rate)) %>% as.numeric()

measure <- "OR"
name <- "adverse_effects.csv"

list.estimates <- getestimates(data, TP, TP1, baseline, measure, name, mainDir)

write.estimates.csv(list.estimates,mainDir, name)

####################### clinically important bleeding ###########################

data=read.csv("pairwise/drugs/clinically important bleeding - wide data format.csv")
data=data %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2)) 

# determine corresponding prior parameters(?TurnerEtAlPrior to help):
TP <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "placebo / control")

TP1 <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "pharma")

baseline=data %>% 
  filter(t1=="placebo/standard care" | t1=="standard care/placebo"| t2=="standard care/placebo" |t2=="placebo/standard care") %>%
  mutate(rate=c.events/c.total) %>%
  summarise(median=median(rate)) %>% as.numeric()

measure <- "OR"
name <- "clinically_important_bleeding.csv"

list.estimates <- getestimates(data, TP, TP1, baseline, measure, name, mainDir)

write.estimates.csv(list.estimates,mainDir, name)

####################### mechanical ventilation ###########################

data=read.csv("pairwise/drugs/mechanical ventilation - wide data format.csv")
data=data %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2)) 

# determine corresponding prior parameters(?TurnerEtAlPrior to help):
TP <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "placebo / control")

TP1 <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "pharma")

baseline=data %>% 
  filter(t1=="placebo/standard care" | t1=="standard care/placebo"| t2=="standard care/placebo" |t2=="placebo/standard care") %>%
  mutate(rate=c.events/c.total) %>%
  summarise(median=median(rate)) %>% as.numeric()

measure <- "OR"
name <- "mechanical_ventilation.csv"

list.estimates <- getestimates(data, TP, TP1, baseline, measure, name, mainDir)

write.estimates.csv(list.estimates,mainDir, name)

####################### viral clearance ###########################

data=read.csv("pairwise/drugs/viral clearance - wide data format.csv")
data=data %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2)) 

# determine corresponding prior parameters(?TurnerEtAlPrior to help):
TP <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "placebo / control")

TP1 <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "pharma")

baseline=data %>% 
  filter(t1=="placebo/standard care" | t1=="standard care/placebo"| t2=="standard care/placebo" |t2=="placebo/standard care") %>%
  mutate(rate=c.events/c.total) %>%
  summarise(median=median(rate)) %>% as.numeric()

measure <- "OR"
name <- "viral_clearance.csv"

list.estimates <- getestimates(data, TP, TP1, baseline, measure, name, mainDir)

write.estimates.csv(list.estimates,mainDir, name)

####################### VTE ###########################

data=read.csv("pairwise/drugs/VTE - wide data format.csv")
data=data %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2)) 

# determine corresponding prior parameters(?TurnerEtAlPrior to help):
TP <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "placebo / control")

TP1 <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "pharma")

baseline=data %>% 
  filter(t1=="placebo/standard care" | t1=="standard care/placebo"| t2=="standard care/placebo" |t2=="placebo/standard care") %>%
  mutate(rate=c.events/c.total) %>%
  summarise(median=median(rate)) %>% as.numeric()

measure <- "OR"
name <- "VTE.csv"

list.estimates <- getestimates(data, TP, TP1, baseline, measure, name, mainDir)

write.estimates.csv(list.estimates,mainDir, name)

####################### Duration of hospitalization ###########################

data=read.csv("pairwise/drugs/Duration of hospitalization_wide data.csv")
data=data %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2)) 

# determine corresponding prior parameters(?TurnerEtAlPrior to help):
TP <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "placebo / control")

TP1 <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "pharma")

baseline=data %>% 
  filter(t1=="placebo/standard care" | t1=="standard care/placebo"| t2=="standard care/placebo" |t2=="placebo/standard care") %>%
  summarise(median=median(mean2)) %>% as.numeric()

measure <- "MD"
name <- "Duration_of_hospitalization.csv"

list.estimates <- getestimates(data, TP, TP1, baseline, measure, name, mainDir)

write.estimates.csv(list.estimates,mainDir, name)

####################### Duration of ventilation ###########################

data=read.csv("pairwise/drugs/Duration of ventilation_wide data.csv")
data=data %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2)) 

# determine corresponding prior parameters(?TurnerEtAlPrior to help):
TP <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "placebo / control")

TP1 <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "pharma")

baseline=data %>% 
  filter(t1=="placebo/standard care" | t1=="standard care/placebo"| t2=="standard care/placebo" |t2=="placebo/standard care") %>%
  summarise(median=median(mean2)) %>% as.numeric()

measure <- "MD"
name <- "Duration_of_ventilation.csv"

list.estimates <- getestimates(data, TP, TP1, baseline, measure, name, mainDir)

write.estimates.csv(list.estimates,mainDir, name)

####################### ICU length of stay ###########################

data=read.csv("pairwise/drugs/ICU length of stay_wide data.csv")
data=data %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2)) 

# determine corresponding prior parameters(?TurnerEtAlPrior to help):
TP <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "placebo / control")

TP1 <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "pharma")

baseline=data %>% 
  filter(t1=="placebo/standard care" | t1=="standard care/placebo"| t2=="standard care/placebo" |t2=="placebo/standard care") %>%
  summarise(median=median(mean2)) %>% as.numeric()

measure <- "MD"
name <- "ICU_length.csv"

list.estimates <- getestimates(data, TP, TP1, baseline, measure, name, mainDir)

write.estimates.csv(list.estimates,mainDir, name)

####################### Time to symptom resolution ###########################

data=read.csv("pairwise/drugs/Time to symptom resolution_wide data.csv")
data=data %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2)) 

# determine corresponding prior parameters(?TurnerEtAlPrior to help):
TP <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "placebo / control")

TP1 <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "pharma")

baseline=data %>% 
  filter(t1=="placebo/standard care" | t1=="standard care/placebo"| t2=="standard care/placebo" |t2=="placebo/standard care") %>%
  summarise(median=median(mean2)) %>% as.numeric()

measure <- "ROM"
name <- "Time_to_symptom_resolution.csv"

list.estimates <- getestimates(data, TP, TP1, baseline, measure, name, mainDir,folderROM="drugs")

write.estimates.csv(list.estimates,mainDir, name)

####################### Time to viral clearance ###########################

data=read.csv("pairwise/drugs/Time to viral clearance_wide data.csv")
data=data %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2)) 

# determine corresponding prior parameters(?TurnerEtAlPrior to help):
TP <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "placebo / control")

TP1 <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "pharma")

baseline=data %>% 
  filter(t1=="placebo/standard care" | t1=="standard care/placebo"| t2=="standard care/placebo" |t2=="placebo/standard care") %>%
  summarise(median=median(mean2)) %>% as.numeric()

measure <- "ROM"
name <- "Time_to_viral_clearance.csv"

list.estimates <- getestimates(data, TP, TP1, baseline, measure, name, mainDir,folderROM="drugs")

write.estimates.csv(list.estimates,mainDir, name)

####################### Ventilator-free days ###########################

data=read.csv("pairwise/drugs/Ventilator-free days_wide data.csv")
data=data %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2)) %>% 
  filter(study!="Self")

# determine corresponding prior parameters(?TurnerEtAlPrior to help):
TP <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "placebo / control")

TP1 <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "pharma")

baseline=data %>% 
  filter(t1=="placebo/standard care" | t1=="standard care/placebo"| t2=="standard care/placebo" |t2=="placebo/standard care") %>%
  summarise(median=median(mean2)) %>% as.numeric()

measure <- "MD"
name <- "Ventilator_free_days.csv"

list.estimates <- getestimates(data, TP, TP1, baseline, measure, name, mainDir)

write.estimates.csv(list.estimates,mainDir, name)

####################### clinically important bleeding -RD ###########################

data=read.csv("pairwise/drugs/clinically important bleeding - wide data format.csv")
data=data %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2)) 

# determine corresponding prior parameters(?TurnerEtAlPrior to help):
TP <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "placebo / control")

TP1 <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "pharma")

baseline=data %>% 
  filter(t1=="placebo/standard care" | t1=="standard care/placebo"| t2=="standard care/placebo" |t2=="placebo/standard care") %>%
  mutate(rate=c.events/c.total) %>%
  summarise(median=median(rate)) %>% as.numeric()

measure <- "RD"
name <- "clinically_important_bleeding_rd.csv"

list.estimates <- getestimates(data, TP, TP1, baseline, measure, name, mainDir)

write.estimates.csv(list.estimates,mainDir, name)
#######################jaki################
#######################Mortality no oxygen###########################

data=read_excel("pairwise/drugs/All outcomes_wide format.xlsx", sheet =1)
data=data %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2)) 

# determine corresponding prior parameters(?TurnerEtAlPrior to help):
TP <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "placebo / control")

TP1 <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "pharma")

baseline=.13

measure <- "OR"
name <- "mortality_jaki_no_oxygen.csv"

list.estimates <- getestimates(data, TP, TP1, baseline, measure, name, mainDir)

write.estimates.csv(list.estimates,mainDir, name)

#######################Mortality oxygen###########################

data=read_excel("pairwise/drugs/All outcomes_wide format.xlsx", sheet =2)
data=data %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2)) 

# determine corresponding prior parameters(?TurnerEtAlPrior to help):
TP <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "placebo / control")

TP1 <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "pharma")

baseline=.13

measure <- "OR"
name <- "mortality_jaki_oxygen.csv"

list.estimates <- getestimates(data, TP, TP1, baseline, measure, name, mainDir)

write.estimates.csv(list.estimates,mainDir, name)

#######################Mortality &MV no oxygen###########################

data=read_excel("pairwise/drugs/All outcomes_wide format.xlsx", sheet =3)
data=data %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2)) 

# determine corresponding prior parameters(?TurnerEtAlPrior to help):
TP <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "placebo / control")

TP1 <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "pharma")

baseline=.13

measure <- "OR"
name <- "mortality_MV_jaki_no_oxygen.csv"

list.estimates <- getestimates(data, TP, TP1, baseline, measure, name, mainDir)

write.estimates.csv(list.estimates,mainDir, name)

#######################Mortality &MV oxygen###########################

data=read_excel("pairwise/drugs/All outcomes_wide format.xlsx", sheet =4)
data=data %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2)) 

# determine corresponding prior parameters(?TurnerEtAlPrior to help):
TP <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "placebo / control")

TP1 <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "pharma")

baseline=.13

measure <- "OR"
name <- "mortality_MV_jaki_oxygen.csv"

list.estimates <- getestimates(data, TP, TP1, baseline, measure, name, mainDir)

write.estimates.csv(list.estimates,mainDir, name)

####################### Time to recovery no oxygen ###########################

data=read_excel("pairwise/drugs/All outcomes_wide format.xlsx", sheet =5)
data=data %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2)) 

# determine corresponding prior parameters(?TurnerEtAlPrior to help):
TP <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "placebo / control")

TP1 <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "pharma")

baseline=data %>% 
  filter(t1=="placebo/standard care" | t1=="standard care/placebo"| t2=="standard care/placebo" |t2=="placebo/standard care") %>%
  summarise(median=median(mean2)) %>% as.numeric()

measure <- "ROM"
name <- "Time_to_recovery_no_oxygen.csv"

list.estimates <- getestimates(data, TP, TP1, baseline, measure, name, mainDir,folderROM="drugs")

write.estimates.csv(list.estimates,mainDir, name)

####################### Time to recovery oxygen ###########################

data=read_excel("pairwise/drugs/All outcomes_wide format.xlsx", sheet =6)
data=data %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2)) 

# determine corresponding prior parameters(?TurnerEtAlPrior to help):
TP <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "placebo / control")

TP1 <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "pharma")

baseline=data %>% 
  filter(t1=="placebo/standard care" | t1=="standard care/placebo"| t2=="standard care/placebo" |t2=="placebo/standard care") %>%
  summarise(median=median(mean2)) %>% as.numeric()

measure <- "ROM"
name <- "Time_to_recovery_oxygen.csv"

list.estimates <- getestimates(data, TP, TP1, baseline, measure, name, mainDir,folderROM="drugs")

write.estimates.csv(list.estimates,mainDir, name)