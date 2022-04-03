# wd <- "/home/antonio/covid19_lnma"
# setwd(wd)
source("NMA/functions_NMA.R")

mainDir <- paste0(getwd(),"/NMA/drugs")
subDir <- "output"

output_dir <- file.path(mainDir, subDir)

if (!dir.exists(output_dir)){
  dir.create(output_dir)
}

##########Dichotomous##########
########### Mortality ####

data=read_excel("NMA/drugs/All binary outcomes_long data for analysis_20220131.xlsx", range = "A2:F42") %>%
  as.data.frame() %>% rename(study=stauthor,responders=responder) %>% 
  filter(study!="Wang_1")

pairwise_data=as_tibble(read.csv("pairwise/drugs/output/mortality.csv", stringsAsFactors = F))

measure = "OR"
likelihood = "binom"
link = "logit"
linearModel = "random"

hy.prior1 = -1.87
hy.prior2 = 0.4328

placebo = "standard care/placebo"

file_name = "Mortality"

data.baseline=read.csv("pairwise/drugs/mortality - wide data format.csv", stringsAsFactors = F) 
data.baseline=data.baseline %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2)) %>% 
  filter(study!="Wang_1") %>%  as_tibble()

# prob.ref.value=data.baseline %>%
#   filter(t1==placebo | t2==placebo) %>%
#   filter(c.total!=0) %>%
#   mutate(rate=c.events/c.total) %>%
#   summarise(median=median(rate)) %>% as.numeric()

prob.ref.value=.0019


getestimatesnma(data,
                pairwise_data,
                measure,
                likelihood,
                link,
                linearModel,
                hy.prior1,
                hy.prior2,
                output_dir,
                file_name,
                prob.ref.value,
                placebo)

get.network.pdf(data.baseline, measure, placebo, mainDir, file_name)

# ########### mechanical ventilation ####

data=read_excel("NMA/drugs/All binary outcomes_long data for analysis_20220131.xlsx", range = "M2:R18") %>%
  as.data.frame() %>% rename(study=stauthor,responders=responder) %>% 
  filter(study!="Wang_1")

pairwise_data=as_tibble(read.csv("pairwise/drugs/output/MV.csv", stringsAsFactors = F))

measure = "OR"
likelihood = "binom"
link = "logit"
linearModel = "random"

hy.prior1 = -1.87
hy.prior2 = 0.4328

placebo = "standard care/placebo"

file_name = "mechanical ventilation"

data.baseline=read.csv("pairwise/drugs/mechanical ventilation - wide data format.csv")
data.baseline=data.baseline %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2)) %>% 
  filter(study!="Wang_1")

# prob.ref.value=data.baseline %>%
#   filter(t1==placebo | t2==placebo) %>%
#   filter(c.total!=0) %>%
#   mutate(rate=c.events/c.total) %>%
#   summarise(median=median(rate)) %>% as.numeric()

prob.ref.value=.0106

get.network.pdf(data.baseline, measure, placebo, mainDir, file_name)

getestimatesnma(data,
                pairwise_data,
                measure,
                likelihood,
                link,
                linearModel,
                hy.prior1,
                hy.prior2,
                output_dir,
                file_name,
                prob.ref.value,
                placebo)

########### Adverse effects leading to discont ####

data=read_csv("NMA/drugs/AE.csv") %>%
  as.data.frame() %>% 
  filter(study!="Wang_1")

pairwise_data=as_tibble(read.csv("pairwise/drugs/output/AE.csv", stringsAsFactors = F))

measure = "RD"
likelihood = "normal"
link = "identity"
linearModel = "fixed"

hy.prior1 = -2.34
hy.prior2 = 0.3303


placebo = "standard care/placebo"

file_name = "Adverse effects"

data.baseline=read.csv("pairwise/drugs/Adverse effects leading to discont - wide data format.csv")
data.baseline=data.baseline %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2))%>% 
  filter(study!="Wang_1")

prob.ref.value=data.baseline %>%
  filter(t1==placebo | t2==placebo) %>%
  filter(c.total!=0) %>%
  mutate(rate=c.events/c.total) %>%
  summarise(median=median(rate)) %>% as.numeric()

get.network.pdf(data.baseline, measure, placebo, mainDir, file_name)

getestimatesnma(data,
                pairwise_data,
                measure,
                likelihood,
                link,
                linearModel,
                hy.prior1,
                hy.prior2,
                output_dir,
                file_name,
                prob.ref.value,
                placebo)

########### Admission to hospital #####

data=read_excel("NMA/drugs/All binary outcomes_long data for analysis_20220131.xlsx", range = "X2:AC30") %>%
  as.data.frame() %>% rename(study=stauthor,responders=responder) %>% 
  filter(study!="Wang_1")

pairwise_data=as_tibble(read.csv("pairwise/drugs/output/admission_to_hospital.csv", stringsAsFactors = F))

measure = "OR"
likelihood = "binom"
link = "logit"
linearModel = "random"

hy.prior1 = -1.87
hy.prior2 = 0.4328

placebo = "standard care/placebo"
file_name = "Admission to hosp"

data.baseline=read.csv("pairwise/drugs/admission to hospital - wide data format.csv")
data.baseline=data.baseline %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2)) %>% 
  filter(study!="Wang_1")
# 
# prob.ref.value=data.baseline %>%
#   filter(t1==placebo | t2==placebo) %>%
#   filter(c.total != 0) %>%
#   mutate(rate=c.events/c.total) %>%
#   summarise(median=median(rate)) %>% as.numeric()

prob.ref.value=.0213

get.network.pdf(data.baseline, measure, placebo, mainDir, file_name)

getestimatesnma(data,
                pairwise_data,
                measure,
                likelihood,
                link,
                linearModel,
                hy.prior1,
                hy.prior2,
                output_dir,
                file_name,
                prob.ref.value,
                placebo)

##########Continuous###########
########### Time to symptom resolution ####

data=read_csv("NMA/drugs/Time_to_symptom_resolution.csv") %>%
  as.data.frame() %>% 
  filter(study!="Wang_1")


pairwise_data=as_tibble(read.csv("pairwise/drugs/output/Time_to_symptom_resolution.csv", stringsAsFactors = F))

measure = "ROM"
likelihood = "normal"
link = "identity"
linearModel = "random"

hy.prior1 = -2.34
hy.prior2 = 0.3303

placebo = "standard care/placebo"
file_name = "Time to symptom"

data.baseline=read.csv("pairwise/drugs/Time to symptom resolution_wide data.csv")
data.baseline=data.baseline %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2)) %>% 
  filter(study!="Wang_1")

# prob.ref.value=data.baseline %>%
#   filter(t1==placebo | t2==placebo) %>%
#   summarise(median=median(mean2)) %>% as.numeric()

prob.ref.value=12.34

getestimatesnma(data,
                pairwise_data,
                measure,
                likelihood,
                link,
                linearModel,
                hy.prior1,
                hy.prior2,
                output_dir,
                file_name,
                prob.ref.value,
                placebo)

get.network.pdf(data.baseline, measure, placebo, mainDir, file_name)