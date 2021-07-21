wd <- "/home/antonio/covid19_lnma"
setwd(wd)
source("NMA/functions_NMA.R")
source("NMA/functions_NMA_2.R")

mainDir <- paste0(getwd(),"/NMA/blood")
subDir <- "output"

output_dir <- file.path(mainDir, subDir)

if (!dir.exists(output_dir)){
  dir.create(output_dir)
}

############## Binary outcomes ######

################ mortality ###########

data=read_excel("NMA/blood/Binary outcomes_20210714_long data for analysis.xlsx", range = "A2:E72") %>%
  as.data.frame() %>% rename(study=stauthor,responders=responder)

pairwise_data=as_tibble(read.csv("pairwise/blood/output/mortality.csv"))


likelihood = "normal"
link = "identity"
linearModel = "random"

hy.prior1 = -2.06
hy.prior2 = 0.4386

file_name = "Mortality"

prob.ref.value <- 0.23

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
                prob.ref.value)


################ mechanical ventilation ###########

data=read_excel("NMA/blood/Binary outcomes_20210714_long data for analysis.xlsx", range = "K2:O40") %>%
  as.data.frame() %>% rename(study=stauthor,responders=responder)

pairwise_data=as_tibble(read.csv("pairwise/blood/output/mv.csv"))


likelihood = "normal"
link = "identity"
linearModel = "random"

hy.prior1 = -2.06
hy.prior2 = 0.4386

file_name = "Mechanical Ventilation"

prob.ref.value <- 0.23

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
                prob.ref.value)


################ Admission to hospital ###########

data=read_excel("NMA/blood/Binary outcomes_20210714_long data for analysis.xlsx", range = "U2:Y18") %>%
  as.data.frame() %>% rename(study=stauthor,responders=responder)

pairwise_data=as_tibble(read.csv("pairwise/blood/output/admission_hop.csv"))


likelihood = "normal"
link = "identity"
linearModel = "random"

hy.prior1 = -2.06
hy.prior2 = 0.4386

file_name = "Admission to Hospital"

prob.ref.value <- 0.23

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
                prob.ref.value)


################ adverse events ###########

data=read_excel("NMA/blood/Binary outcomes_20210714_long data for analysis.xlsx", range = "AE2:AI22") %>%
  as.data.frame() %>% rename(study=stauthor,responders=responder)

pairwise_data1=as_tibble(read.csv("pairwise/blood/output/AE_disc.csv"))


likelihood = "normal"
link = "identity"
linearModel = "random"

hy.prior1 = -2.06
hy.prior2 = 0.4386

file_name = "Adverse Events"

prob.ref.value <- 0.23

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
                prob.ref.value)


################ viral cleareance ###########

data=read_excel("NMA/blood/Binary outcomes_20210714_long data for analysis.xlsx", range = "AO2:AS21") %>%
  as.data.frame() %>% rename(study=stauthor,responders=responder)

pairwise_data=as_tibble(read.csv("pairwise/blood/output/viral_clearance.csv"))


likelihood = "normal"
link = "identity"
linearModel = "random"

hy.prior1 = -2.06
hy.prior2 = 0.4386

file_name = "Viral clearance"

prob.ref.value <- 0.23

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
                prob.ref.value)


################ transfussion related acute lung injury ###########

data=read_excel("NMA/blood/Binary outcomes_20210714_long data for analysis.xlsx", range = "AY2:BC10") %>%
  as.data.frame() %>% rename(study=stauthor,responders=responder)

pairwise_data=as_tibble(read.csv("pairwise/blood/output/acute_lung_injury.csv"))


likelihood = "normal"
link = "identity"
linearModel = "random"

hy.prior1 = -2.06
hy.prior2 = 0.4386

file_name = "T. acute lung injury"

prob.ref.value <- 0.23

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
                prob.ref.value)


################ transfussion related circulatory overload ###########

data=read_excel("NMA/blood/Binary outcomes_20210714_long data for analysis.xlsx", range = "BI2:BM10") %>%
  as.data.frame() %>% rename(study=stauthor,responders=responder)

pairwise_data=as_tibble(read.csv("pairwise/blood/output/circulatory_overload.csv"))


likelihood = "normal"
link = "identity"
linearModel = "random"

hy.prior1 = -2.06
hy.prior2 = 0.4386

file_name = "T. circulatory overload"

prob.ref.value <- 0.23

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
                prob.ref.value)


################ Allergic reaction ###########

data=read_excel("NMA/blood/Binary outcomes_20210714_long data for analysis.xlsx", range = "BS2:BW41") %>%
  as.data.frame() %>% rename(study=stauthor,responders=responder)

pairwise_data=as_tibble(read.csv("pairwise/blood/output/AE_disc.csv"))


likelihood = "normal"
link = "identity"
linearModel = "random"

hy.prior1 = -2.06
hy.prior2 = 0.4386

file_name = "Allergic reaction"

prob.ref.value <- 0.23

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
                prob.ref.value)

########### Continuous outcomes #######

########### Duration of Hospi ####

data=read_excel("NMA/blood/Continuous outcomes_20210715_long data for analysis.xlsx", range = "A2:F42") %>%
  as.data.frame() #%>% rename(study=stauthor,responders=responder)

pairwise_data=as_tibble(read.csv("pairwise/blood/output/duration_hospitalization.csv"))

measure = "MD"
likelihood = "normal"
link = "identity"
linearModel = "random"

hy.prior1 = -2.06
hy.prior2 = 0.4386

file_name = "duration hospitalization"

prob.ref.value <- 0.23

getestimatesnmacontinuous(data,
                          measure,
                          pairwise_data,
                          likelihood, 
                          link, 
                          #linearModel, 
                          hy.prior1, 
                          hy.prior2,
                          output_dir,
                          file_name,
                          prob.ref.value)

########### ICU ####

data=read_excel("NMA/blood/Continuous outcomes_20210715_long data for analysis.xlsx", range = "K2:P6") %>%
  as.data.frame() #%>% rename(study=stauthor,responders=responder)

pairwise_data=as_tibble(read.csv("pairwise/blood/output/ICU.csv"))

measure = "MD"
likelihood = "normal"
link = "identity"
linearModel = "random"

hy.prior1 = -2.06
hy.prior2 = 0.4386

file_name = "ICU"

prob.ref.value <- 0.23

getestimatesnmacontinuous(data,
                          measure,
                          pairwise_data,
                          likelihood, 
                          link, 
                          #linearModel, 
                          hy.prior1, 
                          hy.prior2,
                          output_dir,
                          file_name,
                          prob.ref.value)

########### Ventilation free days ####

data=read_excel("NMA/blood/Continuous outcomes_20210715_long data for analysis.xlsx", range = "V2:AA10") %>%
  as.data.frame() #%>% rename(study=stauthor,responders=responder)

pairwise_data=as_tibble(read.csv("pairwise/blood/output/vent_free.csv"))

measure = "MD"
likelihood = "normal"
link = "identity"
linearModel = "random"

hy.prior1 = -2.06
hy.prior2 = 0.4386

file_name = "ventilation free"

prob.ref.value <- 0.23

getestimatesnmacontinuous(data,
                          measure,
                          pairwise_data,
                          likelihood, 
                          link, 
                          #linearModel, 
                          hy.prior1, 
                          hy.prior2,
                          output_dir,
                          file_name,
                          prob.ref.value)

########### TIME SIMPTOM RES ####

data=read_excel("NMA/blood/Continuous outcomes_20210715_long data for analysis.xlsx", range = "AG2:AL21") %>%
  as.data.frame() #%>% rename(study=stauthor,responders=responder)

pairwise_data=as_tibble(read.csv("pairwise/blood/output/symptom_resolution.csv"))

measure = "ROM"
likelihood = "normal"
link = "identity"
linearModel = "random"

hy.prior1 = -2.06
hy.prior2 = 0.4386

file_name = "time for symptom resolution"

prob.ref.value <- 0.23

getestimatesnmacontinuous(data,
                          measure,
                          pairwise_data,
                          likelihood, 
                          link, 
                          #linearModel, 
                          hy.prior1, 
                          hy.prior2,
                          output_dir,
                          file_name,
                          prob.ref.value)


########### TIME viral clearence ####

data=read_excel("NMA/blood/Continuous outcomes_20210715_long data for analysis.xlsx", range = "AR2:AW7") %>%
  as.data.frame() #%>% rename(study=stauthor,responders=responder)

pairwise_data=as_tibble(read.csv("pairwise/blood/output/time_viral_clearance.csv"))

measure = "ROM"
likelihood = "normal"
link = "identity"
linearModel = "random"

hy.prior1 = -2.06
hy.prior2 = 0.4386

file_name = "time to viral clearance"

prob.ref.value <- 0.23

getestimatesnmacontinuous(data,
                          measure,
                          pairwise_data,
                          likelihood, 
                          link, 
                          #linearModel, 
                          hy.prior1, 
                          hy.prior2,
                          output_dir,
                          file_name,
                          prob.ref.value)
