#wd <- "/home/antonio/covid19_lnma"
#setwd(wd)
source("pairwise/functions_MA.R")

mainDir <- paste0(getwd(),"/pairwise/blood")
subDir <- "output"

output_dir <- file.path(mainDir, subDir)

if (!dir.exists(output_dir)){
  dir.create(output_dir)
}


########### Ventilation free days ####

data=read.csv("pairwise/blood/severe/Ventilator-free days_wide data.csv")
data=data %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2)) 

# determine corresponding prior parameters(?TurnerEtAlPrior to help):
TP <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "placebo / control")

TP1 <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "pharma")

baseline=data %>% filter(t1=="placebo/standard care" | t2=="placebo/standard care") %>%
  summarise(median=median(mean2)) %>% as.numeric()

measure <- "MD"
name <- "vent_free_severe.csv"


list.estimates <- getestimates(data, TP, TP1, baseline, measure, name, mainDir)

write.estimates.csv(list.estimates,mainDir, name)

########### Time to symptom resolution ####

data=read.csv("pairwise/blood/severe/Time to symptom resolution_wide data.csv")
data=data %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2)) 

# determine corresponding prior parameters(?TurnerEtAlPrior to help):
TP <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "placebo / control")

TP1 <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "pharma")

baseline=data %>% filter(t1=="placebo/standard care" | t2=="placebo/standard care") %>%
  summarise(median=median(mean2)) %>% as.numeric()

measure <- "ROM"
name <- "symptom_resolution_severe.csv"


list.estimates <- getestimates(data, TP, TP1, baseline, measure, name, mainDir,"blood")

write.estimates.csv(list.estimates,mainDir, name)

########### ICU length of stay ####

data=read.csv("pairwise/blood/severe/ICU length of stay_wide data.csv")
data=data %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2)) 

# determine corresponding prior parameters(?TurnerEtAlPrior to help):
TP <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "placebo / control")

TP1 <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "pharma")

baseline=data %>% filter(t1=="placebo/standard care" | t2=="placebo/standard care") %>%
  summarise(median=median(mean2)) %>% as.numeric()

measure <- "MD"
name <- "ICU_length_severe.csv"


list.estimates <- getestimates(data, TP, TP1, baseline, measure, name, mainDir)

write.estimates.csv(list.estimates,mainDir, name)

########### Duration of hospitalization ####

data=read.csv("pairwise/blood/severe/Duration of hospitalization_wide data.csv")
data=data %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2)) 

# determine corresponding prior parameters(?TurnerEtAlPrior to help):
TP <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "placebo / control")

TP1 <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "pharma")

baseline=data %>% filter(t1=="placebo/standard care" | t2=="placebo/standard care") %>%
  summarise(median=median(mean2)) %>% as.numeric()

measure <- "MD"
name <- "duration_hosp_severe.csv"


list.estimates <- getestimates(data, TP, TP1, baseline, measure, name, mainDir)

write.estimates.csv(list.estimates,mainDir, name)

#######################transfusion-related acute lung injury ###########################


data=read.csv("pairwise/blood/severe/transfusion-related acute lung injury - wide data format.csv")
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


list.estimates <- getestimates(data, TP, TP1, baseline, measure, name, mainDir)

write.estimates.csv(list.estimates,mainDir, name)


#######################transfusion-related acute lung injury RD ###########################


data=read.csv("pairwise/blood/severe/transfusion-related acute lung injury - wide data format.csv")
data=data %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2)) 

# determine corresponding prior parameters(?TurnerEtAlPrior to help):
TP <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "placebo / control")

TP1 <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "pharma")

baseline=data %>% 
  filter(t1=="placebo/standard care" | t1=="standard care/placebo"| t2=="standard care/placebo" |t2=="placebo/standard care") %>%
  mutate(rate=c.events/c.total) %>%
  summarise(median=median(rate)) %>% as.numeric()

measure <- "RD"
name <- "acute_lung_injury_severe_RD.csv"


list.estimates <- getestimates(data, TP, TP1, baseline, measure, name, mainDir,"blood")

write.estimates.csv(list.estimates,mainDir, name)

#######################transfusion-associated circulatory overload ###########################


data=read.csv("pairwise/blood/severe/transfusion-associated circulatory overload - wide data format.csv")
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


list.estimates <- getestimates(data, TP, TP1, baseline, measure, name, mainDir)

write.estimates.csv(list.estimates,mainDir, name)

#######################transfusion-associated circulatory overload RD ###########################


data=read.csv("pairwise/blood/severe/transfusion-associated circulatory overload - wide data format.csv")
data=data %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2)) 

# determine corresponding prior parameters(?TurnerEtAlPrior to help):
TP <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "placebo / control")

TP1 <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "pharma")

baseline=data %>% 
  filter(t1=="placebo/standard care" | t1=="standard care/placebo"| t2=="standard care/placebo" |t2=="placebo/standard care") %>%
  mutate(rate=c.events/c.total) %>%
  summarise(median=median(rate)) %>% as.numeric()

measure <- "RD"
name <- "circulatory_overload_severe_RD.csv"


list.estimates <- getestimates(data, TP, TP1, baseline, measure, name, mainDir,"blood")

write.estimates.csv(list.estimates,mainDir, name)



#######################Mortality ###########################


data=read.csv("pairwise/blood/severe/mortality - wide data format.csv")
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


list.estimates <- getestimates(data, TP, TP1, baseline, measure, name, mainDir)

write.estimates.csv(list.estimates,mainDir, name)

data=data %>% filter(t1=="convalescent plasma",t2=="placebo/standard care")
effsize=escalc(measure = measure,
               ai = e.events,  n1i = e.total,
               ci = c.events, n2i = c.total,
               slab = study,
               data = data)

rma.random.DL=rma.uni(effsize, method="DL")

pathname <- paste0(mainDir,"/output/", gsub(".{4}$", "", name),"_funnelplot",".pdf")

# output_dir <- file.path(folder, "output")
# 
# if (!dir.exists(output_dir)){
#   dir.create(output_dir)
# }
# 
pdf(pathname, width = 8, height = 5, pointsize = 6)

funnel(rma.random.DL, atransf=exp, label="out")
title("Convalescent plasma vs. standard care/placebo - mortality for severe patients", line=0)

dev.off()

#######################Mechanical Ventilation ###########################


data=read.csv("pairwise/blood/severe/mechanical ventilation - wide data format.csv")
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


list.estimates <- getestimates(data, TP, TP1, baseline, measure, name,mainDir)

write.estimates.csv(list.estimates,mainDir, name)

########### Time to viral cle ####

data=read.csv("pairwise/blood/not severe/Time to viral clearance_wide data.csv")
data=data %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2)) 

# determine corresponding prior parameters(?TurnerEtAlPrior to help):
TP <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "placebo / control")

TP1 <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "pharma")

baseline=data %>% filter(t1=="placebo/standard care" | t2=="placebo/standard care") %>%
  summarise(median=median(mean2)) %>% as.numeric()

measure <- "ROM"
name <- "time_viral_clear_notsevere.csv"


list.estimates <- getestimates(data, TP, TP1, baseline, measure, name, mainDir,"blood")

write.estimates.csv(list.estimates,mainDir, name)

########### Time to symptom resolution ####

data=read.csv("pairwise/blood/not severe/Time to symptom resolution_wide data.csv")
data=data %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2)) 

# determine corresponding prior parameters(?TurnerEtAlPrior to help):
TP <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "placebo / control")

TP1 <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "pharma")

baseline=data %>% filter(t1=="placebo/standard care" | t2=="placebo/standard care") %>%
  summarise(median=median(mean2)) %>% as.numeric()

measure <- "ROM"
name <- "symptom_resolution_notsevere.csv"


list.estimates <- getestimates(data, TP, TP1, baseline, measure, name, mainDir,folderROM="blood")

write.estimates.csv(list.estimates,mainDir, name)

########### Duration of hospitalization ####

data=read.csv("pairwise/blood/not severe/Duration of hospitalization_wide data.csv")
data=data %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2)) 

# determine corresponding prior parameters(?TurnerEtAlPrior to help):
TP <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "placebo / control")

TP1 <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "pharma")

baseline=data %>% filter(t1=="placebo/standard care" | t2=="placebo/standard care") %>%
  summarise(median=median(mean2)) %>% as.numeric()

measure <- "MD"
name <- "duration_hosp_notsevere.csv"


list.estimates <- getestimates(data, TP, TP1, baseline, measure, name, mainDir)

write.estimates.csv(list.estimates,mainDir, name)

#######################Viral Clear ###########################


data=read.csv("pairwise/blood/not severe/viral clearance - wide data format.csv")
data=data %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2)) 

# determine corresponding prior parameters(?TurnerEtAlPrior to help):
TP <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "placebo / control")

TP1 <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "pharma")

baseline=data %>% 
  filter(t1=="placebo/standard care" | t1=="standard care/placebo"| t2=="standard care/placebo" |t2=="placebo/standard care") %>%
  mutate(rate=c.events/c.total) %>%
  summarise(median=median(rate)) %>% as.numeric()

measure <- "OR"
name <- "viral_clear_notsevere.csv"


list.estimates <- getestimates(data, TP, TP1, baseline, measure, name, mainDir)

write.estimates.csv(list.estimates,mainDir, name)

#######################Mortality ###########################


data=read.csv("pairwise/blood/not severe/mortality - wide data format.csv")
data=data %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2)) 

# determine corresponding prior parameters(?TurnerEtAlPrior to help):
TP <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "placebo / control")

TP1 <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "pharma")

baseline=data %>% 
  filter(t1=="placebo/standard care" | t1=="standard care/placebo"| t2=="standard care/placebo" |t2=="placebo/standard care") %>%
  mutate(rate=c.events/c.total) %>%
  summarise(median=median(rate)) %>% as.numeric()

measure <- "OR"
name <- "mortality_notsevere.csv"


list.estimates <- getestimates(data, TP, TP1, baseline, measure, name, mainDir)

write.estimates.csv(list.estimates,mainDir, name)

data=data %>% filter(t1=="convalescent plasma",t2=="placebo/standard care")
effsize=escalc(measure = measure,
                  ai = e.events,  n1i = e.total,
                  ci = c.events, n2i = c.total,
                  slab = study,
                  data = data)

rma.random.DL=rma.uni(effsize, method="DL")

pathname <- paste0(mainDir,"/output/", gsub(".{4}$", "", name),"_funnelplot",".pdf")

# output_dir <- file.path(folder, "output")
# 
# if (!dir.exists(output_dir)){
#   dir.create(output_dir)
# }
# 
pdf(pathname, width = 8, height = 5, pointsize = 6)

funnel(rma.random.DL, atransf=exp, label="out")
title("Convalescent plasma vs. standard care/placebo - mortality for non-severe patients", line=0)

dev.off()

#######################Mechanical Ventilation ###########################


data=read.csv("pairwise/blood/not severe/mechanical ventilation - wide data format.csv")
data=data %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2)) 

# determine corresponding prior parameters(?TurnerEtAlPrior to help):
TP <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "placebo / control")

TP1 <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "pharma")

baseline=data %>% 
  filter(t1=="placebo/standard care" | t1=="standard care/placebo"| t2=="standard care/placebo" |t2=="placebo/standard care") %>%
  mutate(rate=c.events/c.total) %>%
  summarise(median=median(rate)) %>% as.numeric()

measure <- "OR"
name <- "mv_notsevere.csv"


list.estimates <- getestimates(data, TP, TP1, baseline, measure, name,mainDir)

write.estimates.csv(list.estimates,mainDir, name)

#######################admission to hosp ###########################


data=read.csv("pairwise/blood/not severe/admission to hospital - wide data format.csv")
data=data %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2)) 

# determine corresponding prior parameters(?TurnerEtAlPrior to help):
TP <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "placebo / control")

TP1 <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "pharma")

baseline=data %>% 
  filter(t1=="placebo/standard care" | t1=="standard care/placebo"| t2=="standard care/placebo" |t2=="placebo/standard care") %>%
  mutate(rate=c.events/c.total) %>%
  summarise(median=median(rate)) %>% as.numeric()

measure <- "OR"
name <- "admission_hosp_notsevere.csv"


list.estimates <- getestimates(data, TP, TP1, baseline, measure, name,mainDir)

write.estimates.csv(list.estimates,mainDir, name)

#######################Allergic ###########################


data=read.csv("pairwise/blood/allergic reactions - wide data format.csv")
data=data %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2)) 

# determine corresponding prior parameters(?TurnerEtAlPrior to help):
TP <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "placebo / control")

TP1 <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "pharma")

baseline=data %>% 
  filter(t1=="placebo/standard care" | t1=="standard care/placebo"| t2=="standard care/placebo" |t2=="placebo/standard care") %>%
  mutate(rate=c.events/c.total) %>%
  summarise(median=median(rate)) %>% as.numeric()

measure <- "OR"
name <- "allergic_reac.csv"


list.estimates <- getestimates(data, TP, TP1, baseline, measure, name,mainDir)

write.estimates.csv(list.estimates,mainDir, name)

#######################AE ###########################


data=read.csv("pairwise/blood/adverse effects leading to discontinuation - wide data format.csv")
data=data %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2)) 

# determine corresponding prior parameters(?TurnerEtAlPrior to help):
TP <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "placebo / control")

TP1 <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "pharma")

baseline=data %>% 
  filter(t1=="placebo/standard care" | t1=="standard care/placebo"| t2=="standard care/placebo" |t2=="placebo/standard care") %>%
  mutate(rate=c.events/c.total) %>%
  summarise(median=median(rate)) %>% as.numeric()

measure <- "OR"
name <- "AEs.csv"


list.estimates <- getestimates(data, TP, TP1, baseline, measure, name,mainDir)

write.estimates.csv(list.estimates,mainDir, name)

#######################AE RD ###########################


data=read.csv("pairwise/blood/adverse effects leading to discontinuation - wide data format.csv")
data=data %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2)) 

# determine corresponding prior parameters(?TurnerEtAlPrior to help):
TP <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "placebo / control")

TP1 <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "pharma")

baseline=data %>% 
  filter(t1=="placebo/standard care" | t1=="standard care/placebo"| t2=="standard care/placebo" |t2=="placebo/standard care") %>%
  mutate(rate=c.events/c.total) %>%
  summarise(median=median(rate)) %>% as.numeric()

measure <- "RD"
name <- "AEs_RD.csv"


list.estimates <- getestimates(data, TP, TP1, baseline, measure, name,mainDir,"blood")

write.estimates.csv(list.estimates,mainDir, name)

