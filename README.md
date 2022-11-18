# COVID-19: living systematic review and network meta-analysis

This repository includes the [R](https://www.r-project.org) code for our network meta-analyses and pairwise meta-analyses using a bayesian framework for:

+ Drug treatments
+ Prophylaxis
+ Antibodies and blood products

## Run
```bash
docker run -d --name rstudio_covid -e USERID=$(id -u) -p 8888:8787 -e ROOT=TRUE -e PASSWORD=pass  -v "$HOME/Documents:/home/rstudio" jadm333/covid19_lnma
```

## Questions or bugs

If you have a question or find a bug, feel free to contact [Anila Qasim](mailto:qasima@mcmaster.ca). Also feel free to submit a pull request if you find and fix a bug.

## References

Siemieniuk R A, Bartoszko J J, Ge L, Zeraatkar D, Izcovich A, Kum E et al. Drug treatments for covid-19: living systematic review and network meta-analysis *BMJ 2020*; 370 :m2980 doi:10.1136/bmj.m2980 [Link](https://www.bmj.com/content/370/bmj.m2980)

Bartoszko J J, Siemieniuk R A C, Kum E, Qasim A, Zeraatkar D, Ge L et al. Prophylaxis against covid-19: living systematic review and network meta-analysis *BMJ 2021*; 373 :n949 doi:10.1136/bmj.n949 [Link](https://www.bmj.com/content/373/bmj.n949)
