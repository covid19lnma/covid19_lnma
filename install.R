packages=c("netmeta","meta","bayesmeta","numDeriv","forestplot","checkmate","metafor","forcats","purrr","tidyr","tibble","tidyverse","readr","magrittr","rjags","viridis","viridisLite","dplyr",
  "ggplot2","stringr","ggdist","tidybayes","readxl","gemtc","coda")
version=c("1.4-0","4.18-1","2.6","2016.8-1.1","1.10.1","2.0.0","3.0-2","0.5.1","0.3.4","1.1.3","3.1.2","1.3.1","1.4.0","2.0.1","4-10","0.6.1","0.4.0","1.0.7","3.3.5",
          "1.4.0","2.4.0","2.3.1","1.3.1","1.0-1","0.19-4")

packages=c("gemtc")
version=c("1.0-1")


install.versions(packages,version)

packageurl <- "https://cran.r-project.org/src/contrib/Archive/Matrix/Matrix_1.3-3.tar.gz"
install.packages(packageurl, repos=NULL, type="source")
