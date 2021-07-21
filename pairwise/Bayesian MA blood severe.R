source("functions_MA.R")

########### Ventilation free days ####

data=read.csv("blood/severe/Ventilator-free days_wide data.csv")
data=data %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2)) 

# determine corresponding prior parameters(?TurnerEtAlPrior to help):
TP <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "placebo / control")

TP1 <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "pharma")

baseline=data %>% filter(t1=="placebo/standard care" | t2=="placebo/standard care") %>%
  summarise(median=median(mean2)) %>% as.numeric()

measure <- "MD"
name <- "vent_free_severe.csv"
folder <- "blood"

output_dir <- file.path(folder, "output")

if (!dir.exists(output_dir)){
  dir.create(output_dir)
}

list.estimates <- getestimates(data, TP, TP1, baseline, measure, name, folder)

write.estimates.csv(list.estimates,folder, name)

########### Time to symptom resolution ####

data=read.csv("blood/severe/Time to symptom resolution_wide data.csv")
data=data %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2)) 

# determine corresponding prior parameters(?TurnerEtAlPrior to help):
TP <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "placebo / control")

TP1 <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "pharma")

baseline=data %>% filter(t1=="placebo/standard care" | t2=="placebo/standard care") %>%
  summarise(median=median(mean2)) %>% as.numeric()

measure <- "ROM"
name <- "symptom_resolution_severe.csv"
folder <- "blood"

output_dir <- file.path(folder, "output")

if (!dir.exists(output_dir)){
  dir.create(output_dir)
}

list.estimates <- getestimates(data, TP, TP1, baseline, measure, name, folder)

write.estimates.csv(list.estimates,folder, name)

########### ICU length of stay ####

data=read.csv("blood/severe/ICU length of stay_wide data.csv")
data=data %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2)) 

# determine corresponding prior parameters(?TurnerEtAlPrior to help):
TP <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "placebo / control")

TP1 <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "pharma")

baseline=data %>% filter(t1=="placebo/standard care" | t2=="placebo/standard care") %>%
  summarise(median=median(mean2)) %>% as.numeric()

measure <- "MD"
name <- "ICU_length_severe.csv"
folder <- "blood"

output_dir <- file.path(folder, "output")

if (!dir.exists(output_dir)){
  dir.create(output_dir)
}

list.estimates <- getestimates(data, TP, TP1, baseline, measure, name, folder)

write.estimates.csv(list.estimates,folder, name)

########### Duration of hospitalization ####

data=read.csv("blood/severe/Duration of hospitalization_wide data.csv")
data=data %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2)) 

# determine corresponding prior parameters(?TurnerEtAlPrior to help):
TP <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "placebo / control")

TP1 <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "pharma")

baseline=data %>% filter(t1=="placebo/standard care" | t2=="placebo/standard care") %>%
  summarise(median=median(mean2)) %>% as.numeric()

measure <- "MD"
name <- "duration_hosp_severe.csv"
folder <- "blood"

output_dir <- file.path(folder, "output")

if (!dir.exists(output_dir)){
  dir.create(output_dir)
}

list.estimates <- getestimates(data, TP, TP1, baseline, measure, name, folder)

write.estimates.csv(list.estimates,folder, name)

#######################transfusion-related acute lung injury ###########################


data=read.csv("blood/severe/transfusion-related acute lung injury - wide data format.csv")
data=data %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2)) 

# determine corresponding prior parameters(?TurnerEtAlPrior to help):
TP <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "placebo / control")

TP1 <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "pharma")

baseline=data %>% 
  filter(t1=="placebo/standard care" | t1=="standard care/placebo"| t2=="standard care/placebo" |t2=="placebo/standard care") %>%
  mutate(rate=c.events/c.total) %>%
  summarise(median=median(rate)) %>% as.numeric()

measure <- "OR"
name <- "acute_lung_injury_severe.csv"
folder <- "blood"

output_dir <- file.path(folder, "output")

if (!dir.exists(output_dir)){
  dir.create(output_dir)
}

list.estimates <- getestimates(data, TP, TP1, baseline, measure, name, folder)

write.estimates.csv(list.estimates,folder, name)

#######################transfusion-associated circulatory overload ###########################


data=read.csv("blood/severe/transfusion-associated circulatory overload - wide data format.csv")
data=data %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2)) 

# determine corresponding prior parameters(?TurnerEtAlPrior to help):
TP <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "placebo / control")

TP1 <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "pharma")

baseline=data %>% 
  filter(t1=="placebo/standard care" | t1=="standard care/placebo"| t2=="standard care/placebo" |t2=="placebo/standard care") %>%
  mutate(rate=c.events/c.total) %>%
  summarise(median=median(rate)) %>% as.numeric()

measure <- "OR"
name <- "circulatory_overload_severe.csv"
folder <- "blood"

output_dir <- file.path(folder, "output")

if (!dir.exists(output_dir)){
  dir.create(output_dir)
}

list.estimates <- getestimates(data, TP, TP1, baseline, measure, name, folder)

write.estimates.csv(list.estimates,folder, name)

#######################Mortality ###########################


data=read.csv("blood/severe/mortality - wide data format.csv")
data=data %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2)) 

# determine corresponding prior parameters(?TurnerEtAlPrior to help):
TP <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "placebo / control")

TP1 <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "pharma")

baseline=data %>% 
  filter(t1=="placebo/standard care" | t1=="standard care/placebo"| t2=="standard care/placebo" |t2=="placebo/standard care") %>%
  mutate(rate=c.events/c.total) %>%
  summarise(median=median(rate)) %>% as.numeric()

measure <- "OR"
name <- "mortality_severe.csv"
folder <- "blood"

output_dir <- file.path(folder, "output")

if (!dir.exists(output_dir)){
  dir.create(output_dir)
}

list.estimates <- getestimates(data, TP, TP1, baseline, measure, name, folder)

write.estimates.csv(list.estimates,folder, name)

#######################Mechanical Ventilation ###########################


data=read.csv("blood/severe/mechanical ventilation - wide data format.csv")
data=data %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2)) 

# determine corresponding prior parameters(?TurnerEtAlPrior to help):
TP <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "placebo / control")

TP1 <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "pharma")

baseline=data %>% 
  filter(t1=="placebo/standard care" | t1=="standard care/placebo"| t2=="standard care/placebo" |t2=="placebo/standard care") %>%
  mutate(rate=c.events/c.total) %>%
  summarise(median=median(rate)) %>% as.numeric()

measure <- "OR"
name <- "mv_severe.csv"
folder <- "blood"

output_dir <- file.path(folder, "output")

if (!dir.exists(output_dir)){
  dir.create(output_dir)
}

list.estimates <- getestimates(data, TP, TP1, baseline, measure, name,folder)

write.estimates.csv(list.estimates,folder, name)

