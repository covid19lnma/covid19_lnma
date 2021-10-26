# wd <- "/home/antonio/covid19_lnma"
# setwd(wd)
source("NMA/functions_NMA.R")
source("NMA/functions_NMA_2.R")
source("NMA/functions_NMA_3.R")

mainDir <- paste0(getwd(),"/NMA/drugs")
subDir <- "output"

output_dir <- file.path(mainDir, subDir)

if (!dir.exists(output_dir)){
  dir.create(output_dir)
}

##########Dichotomous##########
########### Mortality ####

data=read_excel("NMA/drugs/All binary outcomes_long data for analysis_20211025.xlsx", range = "A2:F72") %>%
  as.data.frame() %>% rename(study=stauthor,responders=responder)

pairwise_data=as_tibble(read.csv("pairwise/drugs/output/mortality_jaki.csv", stringsAsFactors = F))

measure = "OR"
likelihood = "binom"
link = "logit"
linearModel = "random"

hy.prior1 = -1.87
hy.prior2 = 0.4328

placebo = "standard care/placebo"

file_name = "Mortality"

data.baseline=read.csv("pairwise/drugs/mortality - wide data format.csv", stringsAsFactors = F)
data.baseline=data.baseline %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2)) %>% as_tibble()

prob.ref.value=data.baseline %>% 
  filter(t1==placebo | t2==placebo) %>%
  mutate(rate=c.events/c.total) %>%
  summarise(median=median(rate)) %>% as.numeric()

getestimatesnma(data,
                pairwise_data,
                measure,
                likelihood, 
                link, 
                #linearModel, 
                hy.prior1, 
                hy.prior2,
                output_dir,
                file_name,
                prob.ref.value,
                placebo)

#get.network.pdf <- function(pairwise, measure, folder, name){

# pathname <- paste0(mainDir,"/output/", file_name,".pdf")
# pdf(pathname, width = 8, height = 5, pointsize = 6)
# 
# if (measure=="MD" | measure == "ROM") {
#   contrast_df=pairwise(list(t1,t2), mean = list(mean1,mean2), n = list(n1,n2),sd=list(sd1,sd2),studlab = study, data = data.baseline, sm = measure)
# } else if (measure == "OR"){
#   contrast_df=pairwise(list(t1,t2), event = list(e.events,c.events), n = list(e.total,c.total),studlab = study, data = data.baseline, sm = measure) 
# } 
# 
# network=netmeta(contrast_df,reference.group = "standard care/placebo",details.chkmultiarm = T,comb.fixed = F)
# netgraph(network,multiarm = F)
# dev.off()
#}

# data.baseline %<>% filter((e.events != 0 & c.events !=0))
get.network.pdf(data.baseline, measure, placebo, mainDir, file_name)

########### Adverse effects leading to discont ####

data=read_excel("NMA/drugs/All binary outcomes_long data for analysis_20211025.xlsx", range = "Q2:V24") %>%
  as.data.frame() %>% rename(study=stauthor,responders=responder)

pairwise_data=as_tibble(read.csv("pairwise/drugs/output/AE_jaki.csv", stringsAsFactors = F))

measure = "OR"
likelihood = "binom"
link = "logit"
linearModel = "random"

hy.prior1 = -1.87
hy.prior2 = 0.4328

placebo = "standard care/placebo"

file_name = "Adverse effects"

data.baseline=read.csv("pairwise/drugs/Adverse effects leading to discont - wide data format.csv")
data.baseline=data.baseline %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2)) 

prob.ref.value=data.baseline %>% 
  filter(t1==placebo | t2==placebo) %>%
  mutate(rate=c.events/c.total) %>%
  summarise(median=median(rate)) %>% as.numeric()

# data.baseline %<>% filter((e.events != 0 & c.events !=0))
# nc1 <- netconnection(t1, t2, study, data = data.baseline)
# dist = nc1$D.matrix
get.network.pdf(data.baseline, measure, placebo, mainDir, file_name)

getestimatesnma(data,
                pairwise_data,
                measure,
                likelihood, 
                link, 
                #linearModel, 
                hy.prior1, 
                hy.prior2,
                output_dir,
                file_name,
                prob.ref.value,
                placebo)


########### mechanical ventilation ####

data=read_excel("NMA/drugs/All binary outcomes_long data for analysis_20211025.xlsx", range = "I2:N32") %>%
  as.data.frame() %>% rename(study=stauthor,responders=responder)

pairwise_data=as_tibble(read.csv("pairwise/drugs/output/MV_jaki.csv", stringsAsFactors = F))

measure = "OR"
likelihood = "binom"
link = "logit"
linearModel = "random"

hy.prior1 = -1.87
hy.prior2 = 0.4328

placebo = "standard care/placebo"

file_name = "mechanical ventilation"

data.baseline=read.csv("pairwise/drugs/mechanical ventilation - wide data format.csv")
data.baseline=data.baseline %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2)) 

prob.ref.value=data.baseline %>% 
  filter(t1==placebo | t2==placebo) %>%
  mutate(rate=c.events/c.total) %>%
  summarise(median=median(rate)) %>% as.numeric()

get.network.pdf(data.baseline, measure, placebo, mainDir, file_name)

getestimatesnma(data,
                pairwise_data,
                measure,
                likelihood, 
                link, 
                #linearModel, 
                hy.prior1, 
                hy.prior2,
                output_dir,
                file_name,
                prob.ref.value,
                placebo)

##########Continuous###########
########### Duration of hospitalization  ####

data=read_excel("NMA/drugs/All continuous outcomes_long data for analysis_20211025.xlsx", range = "A2:F28") %>%
  as.data.frame() #%>% rename(study=stauthor,responders=responder)

pairwise_data=as_tibble(read.csv("pairwise/drugs/output/Duration_of_hospitalization_jaki.csv", stringsAsFactors = F))

measure = "MD"
likelihood = "normal"
link = "identity"
linearModel = "random"

hy.prior1 = -2.34
hy.prior2 = 0.3303

placebo = "standard care/placebo"

file_name = "Duration of hospitalization"

data.baseline=read.csv("pairwise/drugs/Duration of hospitalization_wide data.csv")
data.baseline=data.baseline %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2)) 

prob.ref.value=data.baseline %>% 
  filter(t1==placebo | t2==placebo) %>%
  summarise(median=median(mean2)) %>% as.numeric()

getestimatesnma(data,
                pairwise_data,
                measure,
                likelihood, 
                link, 
                #linearModel, 
                hy.prior1, 
                hy.prior2,
                output_dir,
                file_name,
                prob.ref.value,
                placebo)

get.network.pdf(data.baseline, measure, placebo, mainDir, file_name)

########### ICU length of stay  ####

data=read_excel("NMA/drugs/All continuous outcomes_long data for analysis_20211025.xlsx", range = "I2:N16") %>%
  as.data.frame() #%>% rename(study=stauthor,responders=responder)

pairwise_data=as_tibble(read.csv("pairwise/drugs/output/ICU_stay_jaki.csv", stringsAsFactors = F))

measure = "MD"
likelihood = "normal"
link = "identity"
linearModel = "random"

hy.prior1 = -2.34
hy.prior2 = 0.3303

placebo = "standard care/placebo"

file_name = "ICU length"

data.baseline=read.csv("pairwise/drugs/ICU length of stay_wide data.csv")
data.baseline=data.baseline %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2)) 

prob.ref.value=data.baseline %>% 
  filter(t1==placebo | t2==placebo) %>%
  summarise(median=median(mean2)) %>% as.numeric()

getestimatesnma(data,
                pairwise_data,
                measure,
                likelihood, 
                link, 
                #linearModel, 
                hy.prior1, 
                hy.prior2,
                output_dir,
                file_name,
                prob.ref.value,
                placebo)

get.network.pdf(data.baseline, measure, placebo, mainDir, file_name)

########### Ventilator-free days  ####

data=read_excel("NMA/drugs/All continuous outcomes_long data for analysis_20211025.xlsx", range = "Q2:V14") %>%
  as.data.frame() #%>% filter(study!="Self") #%>% rename(study=stauthor,responders=responder)

pairwise_data=as_tibble(read.csv("pairwise/drugs/output/Ventilator_free_days_jaki.csv", stringsAsFactors = F))

measure = "MD"
likelihood = "normal"
link = "identity"
linearModel = "random"

hy.prior1 = -2.34
hy.prior2 = 0.3303

placebo = "standard care/placebo"

file_name = "Ventilator-free days"

data.baseline=read.csv("pairwise/drugs/Ventilator-free days_wide data.csv")
data.baseline=data.baseline %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2)) 

prob.ref.value=data.baseline %>% 
  filter(t1==placebo | t2==placebo) %>%
  summarise(median=median(mean2)) %>% as.numeric()

getestimatesnma(data,
                pairwise_data,
                measure,
                likelihood, 
                link, 
                #linearModel, 
                hy.prior1, 
                hy.prior2,
                output_dir,
                file_name,
                prob.ref.value,
                placebo)

get.network.pdf(data.baseline, measure, placebo, mainDir, file_name)

########### Duration of ventilation  ####

data=read_excel("NMA/drugs/All continuous outcomes_long data for analysis_20211025.xlsx", range = "Y2:AD28") %>%
  as.data.frame() #%>% rename(study=stauthor,responders=responder)

pairwise_data=as_tibble(read.csv("pairwise/drugs/output/Duration_of_ventilation_jaki.csv", stringsAsFactors = F))

measure = "MD"
likelihood = "normal"
link = "identity"
linearModel = "random"

hy.prior1 = -2.34
hy.prior2 = 0.3303

placebo = "standard care/placebo"

file_name = "Duration of ventilation"

data.baseline=read.csv("pairwise/drugs/Duration of ventilation_wide data.csv")
data.baseline=data.baseline %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2)) 

prob.ref.value=data.baseline %>% 
  filter(t1==placebo | t2==placebo) %>%
  summarise(median=median(mean2)) %>% as.numeric()

getestimatesnma(data,
                pairwise_data,
                measure,
                likelihood, 
                link, 
                #linearModel, 
                hy.prior1, 
                hy.prior2,
                output_dir,
                file_name,
                prob.ref.value,
                placebo)

get.network.pdf(data.baseline, measure, placebo, mainDir, file_name)

########### Time to symptom resolution ####

data=read.csv("NMA/drugs/Time_to_symptom_resolution_jaki.csv", stringsAsFactors = F) %>%
  as.data.frame() #%>% rename(study=stauthor,responders=responder)

pairwise_data=as_tibble(read.csv("pairwise/drugs/output/Time_to_symptom_resolution_jaki.csv", stringsAsFactors = F))

measure = "ROM"
likelihood = "normal"
link = "identity"
linearModel = "random"

hy.prior1 = -2.34
hy.prior2 = 0.3303

placebo = "standard care/placebo"

file_name = "Time to symptom resolution"

data.baseline=read.csv("pairwise/drugs/Time to symptom resolution_wide data.csv")
data.baseline=data.baseline %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2)) 

prob.ref.value=data.baseline %>% 
  filter(t1==placebo | t2==placebo) %>%
  summarise(median=median(mean2)) %>% as.numeric()

getestimatesnma(data,
                pairwise_data,
                measure,
                likelihood, 
                link, 
                #linearModel, 
                hy.prior1, 
                hy.prior2,
                output_dir,
                file_name,
                prob.ref.value,
                placebo)

get.network.pdf(data.baseline, measure, placebo, mainDir, file_name)

########### Time to viral clearance ####

data=read.csv("NMA/drugs/Time_to_viral_clearance_jaki.csv", stringsAsFactors = F) %>%
  as.data.frame()# %>% filter(study!="Arabi",study!="Elogary") #%>% rename(study=stauthor,responders=responder)

pairwise_data=as_tibble(read.csv("pairwise/drugs/output/Time_to_viral_clearance_jaki.csv", stringsAsFactors = F))

measure = "ROM"
likelihood = "normal"
link = "identity"
linearModel = "random"

hy.prior1 = -2.34
hy.prior2 = 0.3303

placebo = "standard care/placebo"

file_name = "Time to viral clearance"

data.baseline=read.csv("pairwise/drugs/Time to viral clearance_wide data.csv")
data.baseline=data.baseline %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2))# %>% filter(study!="Arabi",study!="Elogary")

prob.ref.value=data.baseline %>% 
  filter(t1==placebo | t2==placebo) %>%
  summarise(median=median(mean2)) %>% as.numeric()

getestimatesnma(data,
                pairwise_data,
                measure,
                likelihood, 
                link, 
                #linearModel, 
                hy.prior1, 
                hy.prior2,
                output_dir,
                file_name,
                prob.ref.value,
                placebo)

get.network.pdf(data.baseline, measure, placebo, mainDir, file_name)