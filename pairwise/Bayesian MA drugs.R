source("functions_MA.R")

############################################################################################
#######################Mortality ###########################
##########################################################################################

data=read.csv("drugs/mortality - IL6 - wide data format.csv")
data=data %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2))

# determine corresponding prior parameters(?TurnerEtAlPrior to help):
TP <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "placebo / control")

TP1 <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "pharma")

baseline=data %>% filter(t1=="standard care/placebo" | t2=="standard care/placebo") %>%
  mutate(rate=c.events/c.total) %>%
  summarise(median=median(rate)) %>% as.numeric()

measure <- "OR"
name <- "mortality_IL6.csv"

list.estimates <- getestimates(data, TP, TP1, baseline, measure, name)

write.estimates.csv(list.estimates, name)


###### Subgroups_drugs ####################


data=read.csv("drugs/mortality - IL6 (subgroups_drug) - wide data format.csv")
data=data %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2))

# determine corresponding prior parameters(?TurnerEtAlPrior to help):
TP <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "placebo / control")

TP1 <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "pharma")

baseline=data %>% filter(t1=="standard care/placebo" | t2=="standard care/placebo") %>%
  mutate(rate=c.events/c.total) %>%
  summarise(median=median(rate)) %>% as.numeric()

measure <- "OR"
name <- "mortality_IL6_sub_drug.csv"

list.estimates <- getestimates(data, TP, TP1, baseline, measure, name)

write.estimates.csv(list.estimates, name)

###### Subgroups_steroid ####################


data=read_xlsx("drugs/mortality - (subgroups_steroids, drugs and steriods) - wide data format.xlsx",sheet = "steroids")
data=data %>% rename(study=refid,t1=treatment1,t2=treatment2,e.events=r1,e.total=n1,c.events=r2,c.total=n2) %>%
  mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2)) %>% filter(subgroups=="Steroids")

# determine corresponding prior parameters(?TurnerEtAlPrior to help):
TP <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "placebo / control")

TP1 <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "pharma")

baseline=data %>% filter(t1=="standard care/placebo" | t2=="standard care/placebo") %>%
  mutate(rate=c.events/c.total) %>%
  summarise(median=median(rate)) %>% as.numeric()

measure <- "OR"
name <- "mortality_IL6_sub_steroid.csv"

list.estimates <- getestimates(data, TP, TP1, baseline, measure, name)

write.estimates.csv(list.estimates, name)

###### Subgroups_no_steroid ####################


data=read_xlsx("drugs/mortality - (subgroups_steroids, drugs and steriods) - wide data format.xlsx",sheet = "steroids")
data=data %>% rename(study=refid,t1=treatment1,t2=treatment2,e.events=r1,e.total=n1,c.events=r2,c.total=n2) %>%
  mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2)) %>% filter(subgroups=="No steroids")

# determine corresponding prior parameters(?TurnerEtAlPrior to help):
TP <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "placebo / control")

TP1 <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "pharma")

baseline=data %>% filter(t1=="standard care/placebo" | t2=="standard care/placebo") %>%
  mutate(rate=c.events/c.total) %>%
  summarise(median=median(rate,na.rm = T)) %>% as.numeric()

measure <- "OR"
name <- "mortality_IL6_sub_nosteroid.csv"

list.estimates <- getestimates(data, TP, TP1, baseline, measure, name)

write.estimates.csv(list.estimates, name)

############################################################################################
#######################Duration of MV ###########################
##########################################################################################

data=read.csv("drugs/duration of ventilation - IL6 - wide data format.csv")
data=data %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2))

# determine corresponding prior parameters(?TurnerEtAlPrior to help):
TP <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "placebo / control")

TP1 <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "pharma")

baseline=data %>% filter(t1=="standard care/placebo" | t2=="standard care/placebo") %>%
  summarise(median=median(mean2)) %>% as.numeric()

measure <- "MD"
name <- "MV_IL6.csv"

list.estimates <- getestimates(data, TP, TP1, baseline, measure, name)

write.estimates.csv(list.estimates, name)


###### Subgroups_drugs ####################


data=read_xlsx("drugs/duration of ventilation - IL6 (subgroup_drug) - wide data format.xlsx")
data=data %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2))

# determine corresponding prior parameters(?TurnerEtAlPrior to help):
TP <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "placebo / control")

TP1 <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "pharma")

baseline=data %>% filter(t1=="standard care/placebo" | t2=="standard care/placebo") %>%
  summarise(median=median(mean2)) %>% as.numeric()


measure <- "MD"
name <- "MV_IL6_sub_drug.csv"

list.estimates <- getestimates(data, TP, TP1, baseline, measure, name)

write.estimates.csv(list.estimates, name)

###### Subgroups_steroid ####################


data=read.csv("drugs/duration of ventilation - (subgroup_steroids) - wide data.csv")
data=data %>%
  mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2)) %>% filter(subgroups=="Steroids")

# determine corresponding prior parameters(?TurnerEtAlPrior to help):
TP <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "placebo / control")

TP1 <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "pharma")

baseline=data %>% filter(t1=="standard care/placebo" | t2=="standard care/placebo") %>%
  summarise(median=median(mean2)) %>% as.numeric()

measure <- "MD"
name <- "MV_IL6_sub_drug_steroid.csv"

list.estimates <- getestimates(data, TP, TP1, baseline, measure, name)

write.estimates.csv(list.estimates, name)

###### Subgroups_no_steroid ####################

data=read.csv("drugs/duration of ventilation - (subgroup_steroids) - wide data.csv")
data=data %>%
  mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2)) %>% filter(subgroups=="No steroids",!is.na(sd2))

# determine corresponding prior parameters(?TurnerEtAlPrior to help):
TP <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "placebo / control")

TP1 <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "pharma")

baseline=data %>% filter(t1=="standard care/placebo" | t2=="standard care/placebo") %>%
  summarise(median=median(mean2)) %>% as.numeric()

measure <- "MD"
name <- "MV_IL6_sub_drug_nosteroid.csv"

list.estimates <- getestimates(data, TP, TP1, baseline, measure, name)

write.estimates.csv(list.estimates, name)

############################################################################################
#######################Bacterial infection ###########################
##########################################################################################

data=read.csv("drugs/secondary bacterial infection - IL6 - wide data format.csv")
data=data %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2))

# determine corresponding prior parameters(?TurnerEtAlPrior to help):
TP <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "placebo / control")

TP1 <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "pharma")

baseline=data %>% filter(t1=="standard care/placebo" | t2=="standard care/placebo") %>%
  mutate(rate=c.events/c.total) %>%
  summarise(median=median(rate)) %>% as.numeric()

measure <- "OR"
name <- "bacterial_inf_IL6.csv"

list.estimates <- getestimates(data, TP, TP1, baseline, measure, name)

write.estimates.csv(list.estimates, name)


###### Subgroups_drugs ####################


data=read.csv("drugs/secondary bacterial infection - IL6 (subgroup_drug) - wide data format.csv")
data=data %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2))

# determine corresponding prior parameters(?TurnerEtAlPrior to help):
TP <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "placebo / control")

TP1 <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "pharma")

baseline=data %>% filter(t1=="standard care/placebo" | t2=="standard care/placebo") %>%
  mutate(rate=c.events/c.total) %>%
  summarise(median=median(rate)) %>% as.numeric()

measure <- "OR"
name <- "bacterial_inf_sub_drug.csv"

list.estimates <- getestimates(data, TP, TP1, baseline, measure, name)

write.estimates.csv(list.estimates, name) #


############### GRADE ######################

data=read.csv("drugs/Mortlaity - NMA IL6 & steroids- wide data format.csv")
data=data %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2))

# determine corresponding prior parameters(?TurnerEtAlPrior to help):
TP <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "placebo / control")

TP1 <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "pharma")

baseline=data %>% filter(t1=="standard care/placebo" | t2=="standard care/placebo") %>%
  mutate(rate=c.events/c.total) %>%
  summarise(median=median(rate)) %>% as.numeric()

measure <- "OR"
name <- "GRADE_mort_direct_IL6.csv"

list.estimates <- getestimates(data, TP, TP1, baseline, measure, name)

write.estimates.csv(list.estimates, name)



