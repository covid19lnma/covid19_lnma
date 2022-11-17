source("NMA/functions_NMA.R")

if (drugs_or_blood == "drugs"){
  mainDir <- "NMA/drugs"
} else if (drugs_or_blood == "blood"){
  mainDir <- "NMA/blood"
} else if (drugs_or_blood == "prohylaxis"){
  mainDir <-"NMA/prohylaxis"
}

if (!dir.exists(paste0(mainDir,"/output"))){
  dir.create(paste0(mainDir,"/output"))
}

outputs = read_excel("input/outputs.xlsx") %>%
  as.data.frame()

for (row in 1:nrow(outputs)) {
  output = outputs[row, "output"]
  measure = outputs[row, "measure"]
  likelihood = outputs[row, "likelihood"]
  link = outputs[row, "link"]
  linearModel = outputs[row, "linearModel"]
  nma_output(output,
            measure,
            likelihood,
            link,
            linearModel,
            placebo,
            drugs_or_blood,
            mainDir)
}
