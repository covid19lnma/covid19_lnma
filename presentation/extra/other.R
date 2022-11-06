library(multinma)
library(readr)
data=read_csv("data.csv")
smknet <- set_agd_arm(data, 
                      study = study,
                      trt = treatment,
                      r = responders, 
                      n = sampleSize,
                      trt_ref = "a")
plot(smknet, weight_edges = TRUE, weight_nodes = TRUE)

