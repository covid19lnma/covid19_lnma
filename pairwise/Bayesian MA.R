source("setup.R")
source("pairwise/functions_MA.R")


if (drugs_or_blood == "drugs"){
  mainDir <- paste0(getwd(),"/pairwise/drugs")
} else if (drugs_or_blood == "blood"){
  mainDir <- paste0(getwd(),"/pairwise/blood")
} else if (drugs_or_blood == "prohylaxis"){
  mainDir <- paste0(getwd(),"/pairwise/prohylaxis")
}

if (!dir.exists(paste0(mainDir,"/output"))){
  dir.create(paste0(mainDir,"/output"))
}

outputs = read_excel("input/outputs.xlsx") %>%
  as.data.frame()

for (row in 1:nrow(outputs)) {
  output = outputs[row, "output"]
  measure = outputs[row, "measure"]
  digits = outputs[row, "digit_precision"]
  pairwise_ouput(output, measure, digits, placebo, mainDir, folderROM)
}
