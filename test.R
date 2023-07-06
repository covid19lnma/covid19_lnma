library(readr)
library(multinma)
library(dplyr)
library(magrittr)

set_up_net=function(df,treatment,binary=F){
  if (binary==T){
    arm_net=set_agd_arm(df, study = refid,
                        trt = {{treatment}},
                        r = responder, 
                        n = sampleSize)
  } else {
    arm_net=set_agd_arm(df, 
                        study = stid,
                        trt = {{treatment}},
                        y = m, 
                        se = se,
                        sample_size = n)
  }
  return(arm_net)
}

network_df=read_csv("input/Mortality - long data format.csv")
arm_net=set_up_net(network_df,interventionname,binary = T)

temp=plot(arm_net, weight_edges = TRUE, weight_nodes = TRUE,layout = "star") 

# +
#   ggraph::scale_edge_colour_manual("Data", values = c(AgD = "#586F8A"),
#                                    guide = "none") +
#   ggplot2::theme(legend.position = "bottom",legend.box = "vertical",
#                  legend.margin = ggplot2::margin(0, 0, 0, 0),
#                  legend.spacing = ggplot2::unit(0.5, "lines"))+
#   # ggplot2::geom_text(ggplot2::aes(x=x,y=y,label = name),data=legend_trt,hjust=0,
#   #                    size=3)+
#   ggplot2::xlim(NA, 1)
# 
# plot.nma_data(arm_net, weight_edges = TRUE, weight_nodes = TRUE,layout = "star")
g <- ggraph::ggraph(igraph::as.igraph(arm_net))
