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
#######################Mortality ###########################

# data=read.csv("pairwise/drugs/Mortality - wide data format.csv")
data=read.csv("input/Mortality - wide data format.csv")
data=data %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2))

# determine corresponding prior parameters(?TurnerEtAlPrior to help):
TP <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "placebo / control")

TP1 <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "pharma")

placebo <- "standard care/placebo"
# baseline=data %>%
#   filter(t1==placebo | t2==placebo) %>%
#   filter(c.total!=0) %>%
#   mutate(rate=c.events/c.total) %>%
#   summarise(median=median(rate)) %>% as.numeric()

baseline=.001

measure <- "RD"
name <- "mortality.csv"

list.estimates <- getestimates(data, TP, TP1, baseline, measure, placebo, name, mainDir,folderROM="drugs")

write.estimates.csv(list.estimates,mainDir, name)

#######################Infection with COVID-19###########################

# data=read.csv("pairwise/drugs/Mechanical ventilation - wide data format.csv")
data=read.csv("input/Infection with COVID-19 (laboratory confirmed) - wide data format.csv")
data=data %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2))

# determine corresponding prior parameters(?TurnerEtAlPrior to help):
TP <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "placebo / control")

TP1 <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "pharma")

placebo <- "standard care/placebo"
baseline=data %>%
  filter(t1==placebo | t2==placebo) %>%
  filter(c.total!=0) %>%
  mutate(rate=c.events/c.total) %>%
  summarise(median=median(rate)) %>% as.numeric()

# baseline=.1160

measure <- "RD"
name <- "Infection with COVID-19 (laboratory confirmed) output.csv"

list.estimates <- getestimates(data, TP, TP1, baseline, measure, placebo, name, mainDir,folderROM="drugs")

write.estimates.csv(list.estimates,mainDir, name)

#######################Infection with COVID-19 suspected###########################

# data=read.csv("pairwise/drugs/Mechanical ventilation - wide data format.csv")
data=read.csv("input/Infection with COVID-19 (laboratory confirmed and suspected) - wide data format.csv")
data=data %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2))

# determine corresponding prior parameters(?TurnerEtAlPrior to help):
TP <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "placebo / control")

TP1 <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "pharma")

placebo <- "standard care/placebo"
baseline=data %>%
  filter(t1==placebo | t2==placebo) %>%
  filter(c.total!=0) %>%
  mutate(rate=c.events/c.total) %>%
  summarise(median=median(rate)) %>% as.numeric()

# baseline=.1160

measure <- "RD"
name <- "Infection with COVID-19 (laboratory confirmed and suspected) output.csv"

list.estimates <- getestimates(data, TP, TP1, baseline, measure, placebo, name, mainDir,folderROM="drugs")

write.estimates.csv(list.estimates,mainDir, name)

#######################MV###########################
#
# # data=read.csv("pairwise/drugs/Mechanical ventilation - wide data format.csv")
# data=read.csv("input/Mechanical ventilation - wide data format.csv")
# data=data %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2))
#
# # determine corresponding prior parameters(?TurnerEtAlPrior to help):
# TP <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "placebo / control")
#
# TP1 <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "pharma")
#
# placebo <- "standard care/placebo"
# # baseline=data %>%
# #   filter(t1==placebo | t2==placebo) %>%
# #   filter(c.total!=0) %>%
# #   mutate(rate=c.events/c.total) %>%
# #   summarise(median=median(rate)) %>% as.numeric()
#
# baseline=.1160
#
# measure <- "OR"
# name <- "MV.csv"
#
# list.estimates <- getestimates(data, TP, TP1, baseline, measure, placebo, name, mainDir)
#
# write.estimates.csv(list.estimates,mainDir, name)

#######################AE###########################

# data=read.csv("pairwise/drugs/Adverse effects leading to discontinuation - wide data format.csv")
data=read.csv("input/Adverse effects leading to discontinuation - wide data format.csv")
data=data %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2))

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

# data=read.csv("pairwise/drugs/Admission to hospital - wide data format.csv")
data=read.csv("input/Admission to hospital - wide data format.csv")
data=data %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2))

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
name <- "admission_to_hospital.csv"

list.estimates <- getestimates(data, TP, TP1, baseline, measure, placebo, name, mainDir,folderROM="drugs")

write.estimates.csv(list.estimates,mainDir, name)

####################### viral clearance ###########################

# # data=read.csv("pairwise/drugs/Viral clearance - wide data format.csv")
# data=read.csv("input/Viral clearance - wide data format.csv")
# data=data %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2))
#
# # determine corresponding prior parameters(?TurnerEtAlPrior to help):
# TP <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "placebo / control")
#
# TP1 <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "pharma")
#
# placebo <- "standard care/placebo"
# baseline=data %>%
#   filter(t1==placebo | t2==placebo) %>%
#   mutate(rate=c.events/c.total) %>%
#   summarise(median=median(rate)) %>% as.numeric()
#
# measure <- "OR"
# name <- "viral_clearance.csv"
#
# list.estimates <- getestimates(data, TP, TP1, baseline, measure, placebo, name, mainDir)
#
# write.estimates.csv(list.estimates,mainDir, name)
#
# ####################### VTE ###########################
#
# data=read.csv("pairwise/drugs/VTE - wide data format.csv")
# data=data %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2))
#
# # determine corresponding prior parameters(?TurnerEtAlPrior to help):
# TP <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "placebo / control")
#
# TP1 <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "pharma")
#
# placebo <- "standard care/placebo"
# baseline=data %>%
#   filter(t1==placebo | t2==placebo) %>%
#   filter(c.total!=0) %>%
#   mutate(rate=c.events/c.total) %>%
#   summarise(median=median(rate)) %>% as.numeric()
#
# measure <- "OR"
# name <- "VTE.csv"
#
# list.estimates <- getestimates(data, TP, TP1, baseline, measure, placebo, name, mainDir)
#
# write.estimates.csv(list.estimates,mainDir, name)

####################### clinically important bleeding ###########################

# data=read.csv("pairwise/drugs/clinically important bleeding - wide data format.csv")
# data=data %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2))
#
# # determine corresponding prior parameters(?TurnerEtAlPrior to help):
# TP <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "placebo / control")
#
# TP1 <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "pharma")
#
# placebo <- "standard care/placebo"
# baseline=data %>%
#   filter(t1==placebo | t2==placebo) %>%
#   filter(c.total!=0) %>%
#   mutate(rate=c.events/c.total) %>%
#   summarise(median=median(rate)) %>% as.numeric()
#
# measure <- "OR"
# name <- "clinically_important_bleeding.csv"
#
# list.estimates <- getestimates(data, TP, TP1, baseline, measure, placebo, name, mainDir)
#
# write.estimates.csv(list.estimates,mainDir, name)

######## Continuous #####

####################### Duration of hospitalization ###########################

# # data=read.csv("pairwise/drugs/Duration of hospitalization - wide data format.csv")
# data=read.csv("input/Duration of hospitalization - wide data format.csv")
# data=data %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2))
#
# # determine corresponding prior parameters(?TurnerEtAlPrior to help):
# TP <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "placebo / control")
#
# TP1 <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "pharma")
#
# placebo <- "standard care/placebo"
# # baseline=data %>%
# #   filter(t1==placebo | t2==placebo) %>%
# #   summarise(median=median(mean2)) %>% as.numeric()
# baseline=12.8
#
# measure <- "MD"
# name <- "Duration_of_hospitalization.csv"
#
# list.estimates <- getestimates(data, TP, TP1, baseline, measure, placebo, name, mainDir)
#
# write.estimates.csv(list.estimates,mainDir, name)

####################### Time to symptom resolution ###########################

# data=read.csv("input/Time to symptom resolution - wide data format.csv")
# data=data %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2))
# 
# # determine corresponding prior parameters(?TurnerEtAlPrior to help):
# TP <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "placebo / control")
# 
# TP1 <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "pharma")
# 
# placebo <- "standard care/placebo"
# baseline=data %>%
#   filter(t1==placebo | t2==placebo) %>%
#   summarise(median=median(mean2)) %>% as.numeric()
# 
# measure <- "ROM"
# name <- "Time_to_symptom_resolution.csv"
# 
# list.estimates <- getestimates(data, TP, TP1, baseline, measure, placebo, name, mainDir,folderROM="drugs")
# 
# write.estimates.csv(list.estimates,mainDir, name)

####################### ICU lenght of stay ###########################

# data=read.csv("pairwise/drugs/ICU length of stay - wide data format.csv")
# data=data %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2))
#
# # determine corresponding prior parameters(?TurnerEtAlPrior to help):
# TP <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "placebo / control")
#
# TP1 <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "pharma")
#
# placebo <- "standard care/placebo"
# # baseline=data %>%
# #   filter(t1==placebo | t2==placebo) %>%
# #   summarise(median=median(mean2)) %>% as.numeric()
#
# baseline=13.3
#
# measure <- "MD"
# name <- "ICU_stay.csv"
#
# list.estimates <- getestimates(data, TP, TP1, baseline, measure, placebo, name, mainDir)
#
# write.estimates.csv(list.estimates,mainDir, name)

####################### Duration of ventilation ###########################

# data=read.csv("pairwise/drugs/Duration of ventilation_wide data.csv")
# data=data %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2))
#
# # determine corresponding prior parameters(?TurnerEtAlPrior to help):
# TP <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "placebo / control")
#
# TP1 <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "pharma")
#
# placebo <- "standard care/placebo"
# # baseline=data %>%
# #   filter(t1==placebo | t2==placebo) %>%
# #   summarise(median=median(mean2)) %>% as.numeric()
#
# baseline=14.7
#
# measure <- "MD"
# name <- "Duration_of_ventilation.csv"
#
# list.estimates <- getestimates(data, TP, TP1, baseline, measure, placebo, name, mainDir)
#
# write.estimates.csv(list.estimates,mainDir, name)
