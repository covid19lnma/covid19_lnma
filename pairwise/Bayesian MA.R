wd <- "/home/antonio/covid19_lnma"
drugs_or_blood <- "drugs"
# placebo <- "placebo/standard care"
placebo <- "standard care/placebo"
folderROM = drugs_or_blood

setwd(wd)
source("pairwise/functions_MA.R")

if (drugs_or_blood == "drugs"){
  mainDir <- paste0(getwd(),"/pairwise/drugs")
} else if (drug_or_blood == "blood"){
  mainDir <- paste0(getwd(),"/pairwise/blood")
}

subDir <- "output"

output_dir <- file.path(mainDir, subDir)

if (!dir.exists(output_dir)){
  dir.create(output_dir)
}

outputs = read_excel("input/outputs.xlsx", range = "A:C") %>%
  as.data.frame()

for (row in 1:nrow(outputs)) {
  output = outputs[row, "output"]
  measure = outputs[row, "measure"]
  digits = outputs[row, "digit_precision"]
  pairwise_ouput(output, measure, digits, placebo, mainDir, folderROM)
}