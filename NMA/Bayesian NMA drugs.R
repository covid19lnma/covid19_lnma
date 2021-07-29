#wd <- "/home/antonio/covid19_lnma"
#setwd(wd)
source("NMA/functions_NMA.R")
source("NMA/functions_NMA_2.R")

mainDir <- paste0(getwd(),"/NMA/drugs")
subDir <- "output"

output_dir <- file.path(mainDir, subDir)

if (!dir.exists(output_dir)){
  dir.create(output_dir)
}


########### Duration of Hospi ####

data=read_excel("NMA/drugs/Continuous outcomes_severe_long data for analysis_20210715.xlsx", range = "A2:F24") %>%
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
