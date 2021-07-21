wd <- "/home/antonio/covid19_lnma/pairwise"
setwd(wd)
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

baseline=data %>% 
  filter(t1=="placebo/standard care" | t1=="standard care/placebo"| t2=="standard care/placebo" |t2=="placebo/standard care") %>%
  mutate(rate=c.events/c.total) %>%
  summarise(median=median(rate)) %>% as.numeric()

measure <- "OR"
name <- "mortality_regn.csv"
folder <- "blood"

output_dir <- file.path(folder, "output")

if (!dir.exists(output_dir)){
  dir.create(output_dir)
}

list.estimates <- getestimates(data, TP, TP1, baseline, measure, name, folder)

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

output_dir <- file.path(folder, "output")

if (!dir.exists(output_dir)){
  dir.create(output_dir)
}

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

baseline=data %>% 
  filter(t1=="placebo/standard care" | t1=="standard care/placebo"| t2=="standard care/placebo" |t2=="placebo/standard care") %>%
  mutate(rate=c.events/c.total) %>%
  summarise(median=median(rate)) %>% as.numeric()


measure <- "OR"
name <- "admission_hop.csv"
folder <- "blood"

output_dir <- file.path(folder, "output")

if (!dir.exists(output_dir)){
  dir.create(output_dir)
}

list.estimates <- getestimates(data, TP, TP1, baseline, measure, name,folder)

write.estimates.csv(list.estimates,folder, name)


######################AE ###########################


data=read.csv("blood/adverse effects leading to discontinuation - wide data format.csv")
data=data %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2)) 

# determine corresponding prior parameters(?TurnerEtAlPrior to help):
TP <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "placebo / control")

TP1 <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "pharma")

baseline=data %>% 
  filter(t1=="placebo/standard care" | t1=="standard care/placebo"| t2=="standard care/placebo" |t2=="placebo/standard care") %>%
  mutate(rate=c.events/c.total) %>%
  summarise(median=median(rate)) %>% as.numeric()

measure <- "OR"
name <- "AE_disc.csv"
folder <- "blood"

output_dir <- file.path(folder, "output")

if (!dir.exists(output_dir)){
  dir.create(output_dir)
}

list.estimates <- getestimates(data, TP, TP1, baseline, measure, name,folder)

write.estimates.csv(list.estimates,folder, name)


######################Allergic ###########################


data=read.csv("blood/allergic reactions - wide data format.csv")
data=data %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2)) 

# determine corresponding prior parameters(?TurnerEtAlPrior to help):
TP <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "placebo / control")

TP1 <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "pharma")

baseline=data %>% 
  filter(t1=="placebo/standard care" | t1=="standard care/placebo"| t2=="standard care/placebo" |t2=="placebo/standard care") %>%
  mutate(rate=c.events/c.total) %>%
  summarise(median=median(rate)) %>% as.numeric()

measure <- "OR"
name <- "allergic.csv"
folder <- "blood"

output_dir <- file.path(folder, "output")

if (!dir.exists(output_dir)){
  dir.create(output_dir)
}

list.estimates <- getestimates(data, TP, TP1, baseline, measure, name,folder)

write.estimates.csv(list.estimates,folder, name)


#######################Mechanical Ventilation ###########################


data=read.csv("blood/mechanical ventilation - wide data format.csv")
data=data %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2)) 

# determine corresponding prior parameters(?TurnerEtAlPrior to help):
TP <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "placebo / control")

TP1 <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "pharma")

baseline=data %>% 
  filter(t1=="placebo/standard care" | t1=="standard care/placebo"| t2=="standard care/placebo" |t2=="placebo/standard care") %>%
  mutate(rate=c.events/c.total) %>%
  summarise(median=median(rate)) %>% as.numeric()

measure <- "OR"
name <- "mv.csv"
folder <- "blood"

output_dir <- file.path(folder, "output")

if (!dir.exists(output_dir)){
  dir.create(output_dir)
}

list.estimates <- getestimates(data, TP, TP1, baseline, measure, name,folder)

write.estimates.csv(list.estimates,folder, name)


#######################Mortality ###########################


data=read.csv("blood/mortality - wide data format.csv")
data=data %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2)) 

# determine corresponding prior parameters(?TurnerEtAlPrior to help):
TP <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "placebo / control")

TP1 <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "pharma")

baseline=data %>% 
  filter(t1=="placebo/standard care" | t1=="standard care/placebo"| t2=="standard care/placebo" |t2=="placebo/standard care") %>%
  mutate(rate=c.events/c.total) %>%
  summarise(median=median(rate)) %>% as.numeric()

measure <- "OR"
name <- "mortality.csv"
folder <- "blood"

output_dir <- file.path(folder, "output")

if (!dir.exists(output_dir)){
  dir.create(output_dir)
}

list.estimates <- getestimates(data, TP, TP1, baseline, measure, name, folder)

write.estimates.csv(list.estimates,folder, name)

#######################transfusion-associated circulatory overload ###########################


data=read.csv("blood/transfusion-associated circulatory overload - wide data format.csv")
data=data %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2)) 

# determine corresponding prior parameters(?TurnerEtAlPrior to help):
TP <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "placebo / control")

TP1 <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "pharma")

baseline=data %>% 
  filter(t1=="placebo/standard care" | t1=="standard care/placebo"| t2=="standard care/placebo" |t2=="placebo/standard care") %>%
  mutate(rate=c.events/c.total) %>%
  summarise(median=median(rate)) %>% as.numeric()

measure <- "OR"
name <- "circulatory_overload.csv"
folder <- "blood"

output_dir <- file.path(folder, "output")

if (!dir.exists(output_dir)){
  dir.create(output_dir)
}

list.estimates <- getestimates(data, TP, TP1, baseline, measure, name, folder)

write.estimates.csv(list.estimates,folder, name)

#######################transfusion-related acute lung injury ###########################


data=read.csv("blood/transfusion-related acute lung injury - wide data format.csv")
data=data %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2)) 

# determine corresponding prior parameters(?TurnerEtAlPrior to help):
TP <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "placebo / control")

TP1 <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "pharma")

baseline=data %>% 
  filter(t1=="placebo/standard care" | t1=="standard care/placebo"| t2=="standard care/placebo" |t2=="placebo/standard care") %>%
  mutate(rate=c.events/c.total) %>%
  summarise(median=median(rate)) %>% as.numeric()

measure <- "OR"
name <- "acute_lung_injury.csv"
folder <- "blood"

output_dir <- file.path(folder, "output")

if (!dir.exists(output_dir)){
  dir.create(output_dir)
}

list.estimates <- getestimates(data, TP, TP1, baseline, measure, name, folder)

write.estimates.csv(list.estimates,folder, name)

#######################viral clearance ###########################


data=read.csv("blood/viral clearance - wide data format.csv")
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
folder <- "blood"

output_dir <- file.path(folder, "output")

if (!dir.exists(output_dir)){
  dir.create(output_dir)
}

list.estimates <- getestimates(data, TP, TP1, baseline, measure, name, folder)

write.estimates.csv(list.estimates,folder, name)


########### Ventilation free days ####

data=read.csv("blood/Ventilator-free days_wide data.csv")
data=data %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2)) 

# determine corresponding prior parameters(?TurnerEtAlPrior to help):
TP <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "placebo / control")

TP1 <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "pharma")

baseline=data %>% 
  filter(t1=="placebo/standard care" | t1=="standard care/placebo"| t2=="standard care/placebo" |t2=="placebo/standard care") %>%
  mutate(rate=c.events/c.total) %>%
  summarise(median=median(rate)) %>% as.numeric()

measure <- "MD"
name <- "vent_free.csv"
folder <- "blood"

output_dir <- file.path(folder, "output")

if (!dir.exists(output_dir)){
  dir.create(output_dir)
}

list.estimates <- getestimates(data, TP, TP1, baseline, measure, name, folder)

write.estimates.csv(list.estimates,folder, name)


########### Duration of hospitalization ####

data=read.csv("blood/Duration of hospitalization_wide data.csv")
data=data %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2)) 

# determine corresponding prior parameters(?TurnerEtAlPrior to help):
TP <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "placebo / control")

TP1 <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "pharma")

baseline=data %>% 
  filter(t1=="placebo/standard care" | t1=="standard care/placebo"| t2=="standard care/placebo" |t2=="placebo/standard care") %>%
  mutate(rate=c.events/c.total) %>%
  summarise(median=median(rate)) %>% as.numeric()

measure <- "MD"
name <- "duration_hospitalization.csv"
folder <- "blood"

output_dir <- file.path(folder, "output")

if (!dir.exists(output_dir)){
  dir.create(output_dir)
}

list.estimates <- getestimates(data, TP, TP1, baseline, measure, name, folder)

write.estimates.csv(list.estimates,folder, name)


########### ICU ####

data=read.csv("blood/ICU length of stay_wide data.csv")
data=data %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2)) 

# determine corresponding prior parameters(?TurnerEtAlPrior to help):
TP <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "placebo / control")

TP1 <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "pharma")

baseline=data %>% 
  filter(t1=="placebo/standard care" | t1=="standard care/placebo"| t2=="standard care/placebo" |t2=="placebo/standard care") %>%
  mutate(rate=c.events/c.total) %>%
  summarise(median=median(rate)) %>% as.numeric()

measure <- "MD"
name <- "ICU.csv"
folder <- "blood"

output_dir <- file.path(folder, "output")

if (!dir.exists(output_dir)){
  dir.create(output_dir)
}

list.estimates <- getestimates(data, TP, TP1, baseline, measure, name, folder)

write.estimates.csv(list.estimates,folder, name)

########### Time to viral ####

data=read.csv("blood/Time to viral clearance_wide data.csv")
data=data %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2)) 

# determine corresponding prior parameters(?TurnerEtAlPrior to help):
TP <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "placebo / control")

TP1 <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "pharma")

baseline=data %>% 
  filter(t1=="placebo/standard care" | t1=="standard care/placebo"| t2=="standard care/placebo" |t2=="placebo/standard care") %>%
  mutate(rate=c.events/c.total) %>%
  summarise(median=median(rate)) %>% as.numeric()

measure <- "ROM"
name <- "time_viral_clearance.csv"
folder <- "blood"

output_dir <- file.path(folder, "output")

if (!dir.exists(output_dir)){
  dir.create(output_dir)
}

list.estimates <- getestimates(data, TP, TP1, baseline, measure, name, folder)

write.estimates.csv(list.estimates,folder, name)


########### Time to symptom resolution ####

data=read.csv("blood/Time to symptom resolution_wide data.csv")
data=data %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2)) 

# determine corresponding prior parameters(?TurnerEtAlPrior to help):
TP <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "placebo / control")

TP1 <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "pharma")

baseline=data %>% 
  filter(t1=="placebo/standard care" | t1=="standard care/placebo"| t2=="standard care/placebo" |t2=="placebo/standard care") %>%
  mutate(rate=c.events/c.total) %>%
  summarise(median=median(rate)) %>% as.numeric()

measure <- "ROM"
name <- "symptom_resolution.csv"
folder <- "blood"

output_dir <- file.path(folder, "output")

if (!dir.exists(output_dir)){
  dir.create(output_dir)
}

list.estimates <- getestimates(data, TP, TP1, baseline, measure, name, folder)

write.estimates.csv(list.estimates,folder, name)