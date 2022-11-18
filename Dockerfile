FROM rocker/tidyverse:4.2

COPY setup.sh /rocker_scripts/setup.sh
COPY install.R /rocker_scripts/install.R

RUN bash /rocker_scripts/setup.sh

# docker build -t jadm333/covid19_lnma .