#wd <- "/home/antonio/covid19_lnma"
#setwd(wd)
source("NMA/functions_NMA.R")
source("NMA/functions_NMA_2.R")

mainDir <- paste0(getwd(),"/NMA/blood")
subDir <- "output"

output_dir <- file.path(mainDir, subDir)

if (!dir.exists(output_dir)){
  dir.create(output_dir)
}


########### Duration of Hospi ####

data=read_excel("NMA/blood/Continuous outcomes_severe_long data for analysis_20210715.xlsx", range = "A2:F24") %>%
  as.data.frame() #%>% rename(study=stauthor,responders=responder)

pairwise_data=as_tibble(read.csv("pairwise/blood/output/duration_hosp_severe.csv"))

measure = "MD"
likelihood = "normal"
link = "identity"
linearModel = "random"

hy.prior1 = -2.34
hy.prior2 = 0.3303

placebo = "placebo/standard care"

file_name = "duration hospitalization severe"

prob.ref.value <- 11.67

getestimatesnmacontinuous(data,
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
########### Mortality ####

data=read_excel("NMA/blood/Binary outcomes_severe_long data for analysis_20210715.xlsx", range = "A2:E40") %>%
  as.data.frame() %>% rename(study=stauthor,responders=responder)

pairwise_data=as_tibble(read.csv("pairwise/blood/output/mortality_severe.csv"))


likelihood = "binom"
link = "logit"
linearModel = "random"

hy.prior1 = -1.87
hy.prior2 = 0.4328

placebo = "placebo/standard care"

file_name = "Mortality severe"

prob.ref.value <- 0.22

getestimatesnma(data,
                pairwise_data,
                # study = "study",
                # treatment = "treatment",
                # responders = "responders",
                # sampleSize = "sampleSize",
                likelihood, 
                link, 
                #linearModel, 
                hy.prior1, 
                hy.prior2,
                output_dir,
                file_name,
                prob.ref.value,
                placebo)

########### Mechanical ventilation ####

data=read_excel("NMA/blood/Binary outcomes_severe_long data for analysis_20210715.xlsx", range = "K2:O18") %>%
  as.data.frame() %>% rename(study=stauthor,responders=responder)

pairwise_data=as_tibble(read.csv("pairwise/blood/output/mv_severe.csv"))


likelihood = "binom"
link = "logit"
linearModel = "random"

hy.prior1 = -1.87
hy.prior2 = 0.4328

placebo = "placebo/standard care"
file_name = "MV severe"

prob.ref.value <- 0.30

getestimatesnma(data,
                pairwise_data,
                # study = "study",
                # treatment = "treatment",
                # responders = "responders",
                # sampleSize = "sampleSize",
                likelihood, 
                link, 
                #linearModel, 
                hy.prior1, 
                hy.prior2,
                output_dir,
                file_name,
                prob.ref.value,
                placebo)

########### Time to viral ####

data=read_csv("NMA/blood/time_viral_clear_notsevere.csv") %>% 
  as.data.frame()#%>% rename(study=stauthor,responders=responder)

pairwise_data=as_tibble(read.csv("pairwise/blood/output/time_viral_clear_notsevere.csv"))

measure = "ROM"
likelihood = "normal"
link = "identity"
linearModel = "random"

hy.prior1 = -2.34
hy.prior2 = 0.3303

placebo = "placebo/standard care"
file_name = "Time to viral not severe"

prob.ref.value <- 24

getestimatesnmacontinuous(data,
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

########### Time to symptom ####

data=read_csv("NMA/blood/symptom_resolution_notsevere.csv") %>% 
  as.data.frame()#%>% rename(study=stauthor,responders=responder)

pairwise_data=as_tibble(read.csv("pairwise/blood/output/symptom_resolution_notsevere.csv"))

measure = "ROM"
likelihood = "normal"
link = "identity"
linearModel = "random"

hy.prior1 = -2.34
hy.prior2 = 0.3303

placebo = "placebo/standard care"
file_name = "Time to symptom not severe"

prob.ref.value <- 9

getestimatesnmacontinuous(data,
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

########### Viral clearance ####

data=read_excel("NMA/blood/Binary outcomes_non severe_long data for analysis_20210715.xlsx", range = "AO2:AS11") %>%
  as.data.frame() %>% rename(study=stauthor,responders=responder)

pairwise_data=as_tibble(read.csv("pairwise/blood/output/viral_clear_notsevere.csv"))


likelihood = "binom"
link = "logit"
linearModel = "random"

hy.prior1 = -1.87
hy.prior2 = 0.4328

placebo = "placebo/standard care"
file_name = "Viral clearance not severe"

prob.ref.value <- .16

getestimatesnma(data,
                pairwise_data,
                # study = "study",
                # treatment = "treatment",
                # responders = "responders",
                # sampleSize = "sampleSize",
                likelihood, 
                link, 
                #linearModel, 
                hy.prior1, 
                hy.prior2,
                output_dir,
                file_name,
                prob.ref.value,
                placebo)

########### Mortality ####

data=read_excel("NMA/blood/Binary outcomes_non severe_long data for analysis_20210715.xlsx", range = "A2:E28") %>%
  as.data.frame() %>% rename(study=stauthor,responders=responder)

pairwise_data=as_tibble(read.csv("pairwise/blood/output/mortality_notsevere.csv"))


likelihood = "binom"
link = "logit"
linearModel = "random"

hy.prior1 = -1.87
hy.prior2 = 0.4328

placebo = "placebo/standard care"
file_name = "Mortality not severe"

prob.ref.value <- 0.003

getestimatesnma(data,
                pairwise_data,
                # study = "study",
                # treatment = "treatment",
                # responders = "responders",
                # sampleSize = "sampleSize",
                likelihood, 
                link, 
                #linearModel, 
                hy.prior1, 
                hy.prior2,
                output_dir,
                file_name,
                prob.ref.value,
                placebo)

########### Mechanical ventilation ####

data=read_excel("NMA/blood/Binary outcomes_non severe_long data for analysis_20210715.xlsx", range = "K2:O16") %>%
  as.data.frame() %>% rename(study=stauthor,responders=responder)

pairwise_data=as_tibble(read.csv("pairwise/blood/output/mv_notsevere.csv"))


likelihood = "binom"
link = "logit"
linearModel = "random"

hy.prior1 = -1.87
hy.prior2 = 0.4328

placebo = "placebo/standard care"
file_name = "MV not severe"

prob.ref.value <- 0.006

getestimatesnma(data,
                pairwise_data,
                # study = "study",
                # treatment = "treatment",
                # responders = "responders",
                # sampleSize = "sampleSize",
                likelihood, 
                link, 
                #linearModel, 
                hy.prior1, 
                hy.prior2,
                output_dir,
                file_name,
                prob.ref.value,
                placebo)

########### Admission to ####

data=read_excel("NMA/blood/Binary outcomes_non severe_long data for analysis_20210715.xlsx", range = "U2:Y18") %>%
  as.data.frame() %>% rename(study=stauthor,responders=responder) #%>% 
  #mutate(treatment=if_else(treatment=="placebo/standard care","a",treatment))


pairwise_data=as_tibble(read.csv("pairwise/blood/output/admission_hosp_notsevere.csv"))


likelihood = "binom"
link = "logit"
linearModel = "random"

hy.prior1 = -1.87
hy.prior2 = 0.4328

placebo = "placebo/standard care"
file_name = "Admission to hosp not severe"

prob.ref.value <- 0.06

getestimatesnma(data,
                pairwise_data,
                # study = "study",
                # treatment = "treatment",
                # responders = "responders",
                # sampleSize = "sampleSize",
                likelihood, 
                link, 
                #linearModel, 
                hy.prior1, 
                hy.prior2,
                output_dir,
                file_name,
                prob.ref.value,
                placebo)

########### Allergic ####

data=read_excel("NMA/blood/Binary outcomes_20210714_long data for analysis (1).xlsx", range = "BS2:BW41") %>%
  as.data.frame() %>% rename(study=stauthor,responders=responder)

pairwise_data=as_tibble(read.csv("pairwise/blood/output/allergic_reac.csv"))


likelihood = "binom"
link = "logit"
linearModel = "random"

hy.prior1 = -1.87
hy.prior2 = 0.4328

placebo = "placebo/standard care"
file_name = "Allergic react"

prob.ref.value <- 0.003

getestimatesnma(data,
                pairwise_data,
                # study = "study",
                # treatment = "treatment",
                # responders = "responders",
                # sampleSize = "sampleSize",
                likelihood, 
                link, 
                #linearModel, 
                hy.prior1, 
                hy.prior2,
                output_dir,
                file_name,
                prob.ref.value,
                placebo)


########### AEs ####

data=read_excel("NMA/blood/Binary outcomes_20210714_long data for analysis (1).xlsx", range = "AE2:AI22") %>%
  as.data.frame() %>% rename(study=stauthor,responders=responder)

pairwise_data=as_tibble(read.csv("pairwise/blood/output/AEs.csv"))


likelihood = "binom"
link = "logit"
linearModel = "random"

hy.prior1 = -1.87
hy.prior2 = 0.4328

placebo = "placebo/standard care"
file_name = "AEs disc"

prob.ref.value <- 0.000

getestimatesnma(data,
                pairwise_data,
                # study = "study",
                # treatment = "treatment",
                # responders = "responders",
                # sampleSize = "sampleSize",
                likelihood, 
                link, 
                #linearModel, 
                hy.prior1, 
                hy.prior2,
                output_dir,
                file_name,
                prob.ref.value,
                placebo)

