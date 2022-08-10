wd <- "/home/antonio/covid19_lnma"
drugs_or_blood <- "drugs"
# placebo <- "placebo/standard care"
placebo <- "standard care/placebo"

setwd(wd)
source("NMA/functions_NMA.R")

if (drugs_or_blood == "drugs"){
  mainDir <- paste0(getwd(),"/NMA/drugs")
} else if (drug_or_blood == "blood"){
  mainDir <- paste0(getwd(),"/NMA/blood")
}

subDir <- "output"
output_dir <- file.path(mainDir, subDir)

if (!dir.exists(output_dir)){
  dir.create(output_dir)
}

outputs = read_excel("input/outputs.xlsx", range = "A:F") %>%
  as.data.frame()

for (row in 1:nrow(outputs)) {
  output = outputs[row, "output"]
  measure = outputs[row, "measure"]
  likelihood = outputs[row, "likelihood"]
  link = outputs[row, "link"]
  linearModel = outputs[row, "linearModel"]
  nma_ouput(output,
            measure,
            likelihood,
            link,
            linearModel,
            placebo,
            drugs_or_blood,
            mainDir)
}