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

nma_ouput <- function(output, 
                      measure,
                      likelihood,
                      link,
                      linearModel,
                      placebo,
                      drugs_or_blood,
                      mainDir){
  
  subDir <- "output"
  output_dir <- file.path(mainDir, subDir)

  if (output == "Mortality"){
    data=read.csv("input/Mortality - long data format.csv") %>%
      as.data.frame() %>% rename(study=stauthor,responders=responder)
    
    if (drugs_or_blood == "drugs"){
      pairwise_data=as_tibble(read.csv("pairwise/drugs/output/mortality.csv", stringsAsFactors = F))
    } else if (drugs_or_blood == "blood"){
      pairwise_data=as_tibble(read.csv("pairwise/blood/output/mortality.csv", stringsAsFactors = F))
    }
    hy.prior1 = -1.87
    hy.prior2 = 0.4328
    
    file_name = "Mortality"
    
    data.baseline=read.csv("input/Mortality - wide data format.csv", stringsAsFactors = F)
    data.baseline=data.baseline %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2)) %>% as_tibble()
    
    prob.ref.value=data.baseline %>%
      filter(t1==placebo | t2==placebo) %>%
      filter(c.total!=0) %>%
      mutate(rate=c.events/c.total) %>%
      summarise(median=median(rate)) %>% as.numeric()
    
  } else if (output == "COVID-19 (laboratory confirmed)"){
    data=read.csv("input/Infection with COVID-19 (laboratory confirmed) - long data format.csv") %>%
      as.data.frame() %>% rename(study=stauthor,responders=responder)
    
    if (drugs_or_blood == "drugs"){
      pairwise_data=as_tibble(read.csv("pairwise/drugs/output/Infection_COVID-19_(laboratory_confirmed).csv", stringsAsFactors = F))
    } else if (drugs_or_blood == "blood"){
      pairwise_data=as_tibble(read.csv("pairwise/blood/output/Infection_COVID-19_(laboratory_confirmed).csv", stringsAsFactors = F))
    }
    hy.prior1 = -1.87
    hy.prior2 = 0.4328
    
    file_name = "laboratory confirmed"
    
    data.baseline=read.csv("input/Infection with COVID-19 (laboratory confirmed) - wide data format.csv")
    data.baseline=data.baseline %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2))
    
    prob.ref.value=data.baseline %>%
      filter(t1==placebo | t2==placebo) %>%
      filter(c.total!=0) %>%
      mutate(rate=c.events/c.total) %>%
      summarise(median=median(rate)) %>% as.numeric()
    
  } else if (output == "COVID-19 (confirmed and suspected)"){
    data=read.csv("input/Infection with COVID-19 (laboratory confirmed and suspected) - long data format.csv") %>%
      as.data.frame() %>% rename(study=stauthor,responders=responder)
    
    if (drugs_or_blood == "drugs"){
      pairwise_data=as_tibble(read.csv("pairwise/drugs/output/Infection_COVID-19_(laboratory_confirmed_and_suspected).csv", stringsAsFactors = F))
    } else if (drugs_or_blood == "blood"){
      pairwise_data=as_tibble(read.csv("pairwise/blood/output/Infection_COVID-19_(laboratory_confirmed_and_suspected).csv", stringsAsFactors = F))
    }
    hy.prior1 = -1.87
    hy.prior2 = 0.4328
    
    file_name = "laboratory confirmed and suspected"
    
    data.baseline=read.csv("input/Infection with COVID-19 (laboratory confirmed and suspected) - wide data format.csv")
    data.baseline=data.baseline %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2))
    
    prob.ref.value=data.baseline %>%
      filter(t1==placebo | t2==placebo) %>%
      filter(c.total!=0) %>%
      mutate(rate=c.events/c.total) %>%
      summarise(median=median(rate)) %>% as.numeric()
    
  } else if (output == "Mechanical ventilation"){
    data=read.csv("input/Mechanical ventilation - long data format.csv") %>%
      as.data.frame() %>% rename(study=stauthor,responders=responder)
    
    if (drugs_or_blood == "drugs"){
      pairwise_data=as_tibble(read.csv("pairwise/drugs/output/MV.csv", stringsAsFactors = F))
    } else if (drugs_or_blood == "blood"){
      pairwise_data=as_tibble(read.csv("pairwise/blood/output/MV.csv", stringsAsFactors = F))
    }
    hy.prior1 = -1.87
    hy.prior2 = 0.4328
    
    file_name = "MV"
    
    data.baseline=read.csv("input/Mechanical ventilation - wide data format.csv", stringsAsFactors = F)
    data.baseline=data.baseline %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2)) %>% as_tibble()
    
    prob.ref.value=data.baseline %>%
      filter(t1==placebo | t2==placebo) %>%
      filter(c.total!=0) %>%
      mutate(rate=c.events/c.total) %>%
      summarise(median=median(rate)) %>% as.numeric()
    
  } else if (output == "Admission to hospital"){
    data=read.csv("input/Admission to hospital - long data format.csv") %>%
      as.data.frame() %>% rename(study=stauthor,responders=responder)
    
    if (drugs_or_blood == "drugs"){
      pairwise_data=as_tibble(read.csv("pairwise/drugs/output/admission_to_hospital.csv", stringsAsFactors = F))
    } else if (drugs_or_blood == "blood"){
      pairwise_data=as_tibble(read.csv("pairwise/blood/output/admission_to_hospital.csv", stringsAsFactors = F))
    }
    hy.prior1 = -1.87
    hy.prior2 = 0.4328
    file_name = "Admission to hosp"
    
    data.baseline=read.csv("input/Admission to hospital - wide data format.csv")
    data.baseline=data.baseline %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2))
    
    prob.ref.value=data.baseline %>%
      filter(t1==placebo | t2==placebo) %>%
      filter(c.total != 0) %>%
      mutate(rate=c.events/c.total) %>%
      summarise(median=median(rate)) %>% as.numeric()
  
  } else if (output == "Adverse effects leading to discontinuation"){
    data=read.csv("input/Adverse effects leading to discontinuation - long data format.csv") %>%
      as.data.frame() %>% rename(study=stauthor,responders=responder)
    
    if (drugs_or_blood == "drugs"){
      pairwise_data=as_tibble(read.csv("pairwise/drugs/output/AE.csv", stringsAsFactors = F))
    } else if (drugs_or_blood == "blood"){
      pairwise_data=as_tibble(read.csv("pairwise/blood/output/AE.csv", stringsAsFactors = F))
    }
    hy.prior1 = -2.34
    hy.prior2 = 0.3303
    
    file_name = "Adverse effects"
    
    data.baseline=read.csv("input/Adverse effects leading to discontinuation - wide data format.csv")
    data.baseline=data.baseline %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2))
    
    prob.ref.value=data.baseline %>%
      filter(t1==placebo | t2==placebo) %>%
      filter(c.total!=0) %>%
      mutate(rate=c.events/c.total) %>%
      summarise(median=median(rate)) %>% as.numeric()
  
  } else if (output == "Viral clearance"){
    data=read.csv("input/Viral clearance - long data format.csv") %>%
      as.data.frame() %>% rename(study=stauthor,responders=responder)
    
    if (drugs_or_blood == "drugs"){
      pairwise_data=as_tibble(read.csv("pairwise/drugs/output/viral_clearance.csv", stringsAsFactors = F))
    } else if (drugs_or_blood == "blood"){
      pairwise_data=as_tibble(read.csv("pairwise/blood/output/viral_clearance.csv", stringsAsFactors = F))
    }
    hy.prior1 = -1.87
    hy.prior2 = 0.4328
    
    file_name = "Viral clearance"
    
    data.baseline=read.csv("input/Viral clearance - wide data format.csv")
    data.baseline=data.baseline %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2))
    
    prob.ref.value=data.baseline %>%
      filter(t1==placebo| t2==placebo) %>%
      filter(c.total!=0) %>%
      mutate(rate=c.events/c.total) %>%
      summarise(median=median(rate)) %>% as.numeric()
    
  } else if (output == "Venous thromboembolism"){
    data=read.csv("input/Venous thromboembolism - long data format.csv") %>%
      as.data.frame() %>% rename(study=stauthor,responders=responder)
    
    if (drugs_or_blood == "drugs"){
      pairwise_data=as_tibble(read.csv("pairwise/drugs/output/VTE.csv", stringsAsFactors = F))
    } else if (drugs_or_blood == "blood"){
      pairwise_data=as_tibble(read.csv("pairwise/blood/output/VTE.csv", stringsAsFactors = F))
    }
    hy.prior1 = -1.87
    hy.prior2 = 0.4328
    file_name = "VTE"
    
    data.baseline=read.csv("input/VTE - wide data format.csv")
    data.baseline=data.baseline %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2))
    
    prob.ref.value=data.baseline %>%
      filter(t1==placebo | t2==placebo) %>%
      filter(c.total != 0) %>%
      mutate(rate=c.events/c.total) %>%
      summarise(median=median(rate)) %>% as.numeric()
    
  } else if (output == "TRALI"){
    data=read.csv("input/Transfusion-related acute lung injury - long data format.csv") %>%
      as.data.frame() %>% rename(study=stauthor,responders=responder)
    
    if (drugs_or_blood == "drugs"){
      pairwise_data=as_tibble(read.csv("pairwise/drugs/output/Transfusion_lung_injury.csv", stringsAsFactors = F))
    } else if (drugs_or_blood == "blood"){
      pairwise_data=as_tibble(read.csv("pairwise/blood/output/Transfusion_lung_injury.csv", stringsAsFactors = F))
    }
    hy.prior1 = -2.34
    hy.prior2 = 0.3303
    file_name = "Transfusion-related acute lung injury"
    
    data.baseline=read.csv("input/Transfusion-related acute lung injury - wide data format.csv")
    data.baseline=data.baseline %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2))
    
    prob.ref.value=data.baseline %>%
      filter(t1==placebo | t2==placebo) %>%
      filter(c.total!=0) %>%
      mutate(rate=c.events/c.total) %>%
      summarise(median=median(rate)) %>% as.numeric()
    
  } else if (output == "TACO"){
    data=read.csv("input/Transfusion-associated circulatory overload - long data format.csv") %>%
      as.data.frame() %>% rename(study=stauthor,responders=responder)
    
    if (drugs_or_blood == "drugs"){
      pairwise_data=as_tibble(read.csv("pairwise/drugs/output/Transfusion_circulatory_overload.csv", stringsAsFactors = F))
    } else if (drugs_or_blood == "blood"){
      pairwise_data=as_tibble(read.csv("pairwise/blood/output/Transfusion_circulatory_overload.csv", stringsAsFactors = F))
    }
    hy.prior1 = -2.34
    hy.prior2 = 0.3303
    file_name = "Transfusion circulatory overload"
    
    data.baseline=read.csv("input/Transfusion-associated circulatory overload - wide data format.csv")
    data.baseline=data.baseline %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2))
    
    prob.ref.value=data.baseline %>%
      filter(t1==placebo | t2==placebo) %>%
      filter(c.total!=0) %>%
      mutate(rate=c.events/c.total) %>%
      summarise(median=median(rate)) %>% as.numeric()
    
  } else if (output == "Clinically important bleeding"){
    data=read.csv("input/Clinically important bleeding - long data format.csv") %>%
      as.data.frame() %>% rename(study=stauthor,responders=responder)
    
    if (drugs_or_blood == "drugs"){
      pairwise_data=as_tibble(read.csv("pairwise/drugs/output/clinically_important_bleeding.csv", stringsAsFactors = F))
    } else if (drugs_or_blood == "blood"){
      pairwise_data=as_tibble(read.csv("pairwise/blood/output/clinically_important_bleeding.csv", stringsAsFactors = F))
    }
    hy.prior1 = -1.87
    hy.prior2 = 0.4328
    file_name = "clinically important bleeding"
    
    data.baseline=read.csv("pairwise/drugs/clinically important bleeding - wide data format.csv")
    data.baseline=data.baseline %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2))
    
    prob.ref.value=data.baseline %>%
      filter(t1==placebo | t2==placebo) %>%
      filter(c.total != 0) %>%
      mutate(rate=c.events/c.total) %>%
      summarise(median=median(rate)) %>% as.numeric()
    
  } else if (output == "Allergic reactions"){
    data=read.csv("input/Allergic reactions - long data format.csv") %>%
      as.data.frame() %>% rename(study=stauthor,responders=responder)
    
    if (drugs_or_blood == "drugs"){
      pairwise_data=as_tibble(read.csv("pairwise/drugs/output/Allergic_reactions.csv", stringsAsFactors = F))
    } else if (drugs_or_blood == "blood"){
      pairwise_data=as_tibble(read.csv("pairwise/blood/output/Allergic_reactions.csv", stringsAsFactors = F))
    }
    hy.prior1 = -2.34
    hy.prior2 = 0.3303
    file_name = "Allergic reactions"
    
    data.baseline=read.csv("input/Allergic reactions - wide data format.csv")
    data.baseline=data.baseline %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2))
    
    prob.ref.value=data.baseline %>%
      filter(t1==placebo | t2==placebo) %>%
      filter(c.total!=0) %>%
      mutate(rate=c.events/c.total) %>%
      summarise(median=median(rate)) %>% as.numeric()
  } else if (output == "Graft vs. host disease"){
    data=read.csv("input/Graft vs. host disease - long data format.csv") %>%
      as.data.frame() %>% rename(study=stauthor,responders=responder)
    
    if (drugs_or_blood == "drugs"){
      pairwise_data=as_tibble(read.csv("pairwise/drugs/output/Graft.csv", stringsAsFactors = F))
    } else if (drugs_or_blood == "blood"){
      pairwise_data=as_tibble(read.csv("pairwise/blood/output/Graft.csv", stringsAsFactors = F))
    }
    hy.prior1 = -2.34
    hy.prior2 = 0.3303
    file_name = "Graft vs. host disease"
    
    data.baseline=read.csv("input/Graft vs. host disease - wide data format.csv")
    data.baseline=data.baseline %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2))
    
    prob.ref.value=data.baseline %>%
      filter(t1==placebo | t2==placebo) %>%
      filter(c.total!=0) %>%
      mutate(rate=c.events/c.total) %>%
      summarise(median=median(rate)) %>% as.numeric()
  } else if (output == "Duration of hospitalization"){
    if (drugs_or_blood == "drugs"){
      if(measure == "ROM" || measure == "RD"){
        data=read_csv("NMA/drugs/Duration_of_hospitalization.csv") %>%
          as.data.frame()
      } else {
        data=read.csv("input/Duration of hospitalization - long data format.csv") %>%
          as.data.frame()
      }
      pairwise_data=as_tibble(read.csv("pairwise/drugs/output/Duration_of_hospitalization.csv", stringsAsFactors = F))
    } else if (drugs_or_blood == "blood"){
      if(measure == "ROM" || measure == "RD"){
        data=read_csv("NMA/blood/Duration_of_hospitalization.csv") %>%
          as.data.frame()
      } else {
        data=read.csv("input/Duration of hospitalization - long data format.csv") %>%
          as.data.frame()
      }
      pairwise_data=as_tibble(read.csv("pairwise/blood/output/Duration_of_hospitalization.csv", stringsAsFactors = F))
    }
    hy.prior1 = -2.34
    hy.prior2 = 0.3303
    file_name = "Duration of hospitalization"
    
    data.baseline=read.csv("input/Duration of hospitalization - wide data format.csv")
    data.baseline=data.baseline %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2)) %>% filter(study!="Dorward")
    
    prob.ref.value=data.baseline %>%
      filter(t1==placebo | t2==placebo) %>%
      summarise(median=median(mean2)) %>% as.numeric()
    
  } else if (output == "ICU length of stay"){
    if (drugs_or_blood == "drugs"){
      if(measure == "ROM" || measure == "RD"){
        data=read_csv("NMA/drugs/ICU_stay.csv") %>%
          as.data.frame()
      } else {
        ddata=read.csv("input/ICU length of stay - long data format.csv") %>%
          as.data.frame()
      }
      pairwise_data=as_tibble(read.csv("pairwise/drugs/output/ICU_stay.csv", stringsAsFactors = F))
    } else if (drugs_or_blood == "blood"){
      if(measure == "ROM" || measure == "RD"){
        data=read_csv("NMA/blood/ICU_stay.csv") %>%
          as.data.frame()
      } else {
        data=read.csv("input/ICU length of stay - long data format.csv") %>%
          as.data.frame()
      }
      pairwise_data=as_tibble(read.csv("pairwise/blood/output/ICU_stay.csv", stringsAsFactors = F))
    }
    hy.prior1 = -2.34
    hy.prior2 = 0.3303
    file_name = "ICU length"
    
    data.baseline=read.csv("input/ICU length of stay - wide data format.csv")
    data.baseline=data.baseline %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2))
    
    prob.ref.value=data.baseline %>%
      filter(t1==placebo | t2==placebo) %>%
      summarise(median=median(mean2)) %>% as.numeric()
    
  } else if (output == "Ventilator-free days"){
    if (drugs_or_blood == "drugs"){
      if(measure == "ROM" || measure == "RD"){
        data=read_csv("NMA/drugs/Ventilator_free_days.csv") %>%
          as.data.frame()
      } else {
        ddata=read.csv("input/Ventilator-free days - long data format.csv") %>%
          as.data.frame()
      }
      pairwise_data=as_tibble(read.csv("pairwise/drugs/output/Ventilator_free_days.csv", stringsAsFactors = F))
    } else if (drugs_or_blood == "blood"){
      if(measure == "ROM" || measure == "RD"){
        data=read_csv("NMA/blood/Ventilator_free_days.csv") %>%
          as.data.frame()
      } else {
        data=read.csv("input/Ventilator-free days - long data format.csv") %>%
          as.data.frame()
      }
      pairwise_data=as_tibble(read.csv("pairwise/blood/output/Ventilator_free_days.csv", stringsAsFactors = F))
    }
    hy.prior1 = -2.34
    hy.prior2 = 0.3303
    file_name = "Ventilator-free days"
    
    data.baseline=read.csv("input/Ventilator-free days - wide data format.csv")
    data.baseline=data.baseline %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2))
    
    prob.ref.value=data.baseline %>%
      filter(t1==placebo | t2==placebo) %>%
      summarise(median=median(mean2)) %>% as.numeric()
    
  } else if (output == "Duration of ventilation"){
    if (drugs_or_blood == "drugs"){
      if(measure == "ROM" || measure == "RD"){
        data=read_csv("NMA/drugs/Duration_of_ventilation.csv") %>%
          as.data.frame()
      } else {
        ddata=read.csv("input/Duration of ventilation - long data format.csv") %>%
          as.data.frame()
      }
      pairwise_data=as_tibble(read.csv("pairwise/drugs/output/Duration_of_ventilation.csv", stringsAsFactors = F))
    } else if (drugs_or_blood == "blood"){
      if(measure == "ROM" || measure == "RD"){
        data=read_csv("NMA/blood/Duration_of_ventilation.csv") %>%
          as.data.frame()
      } else {
        data=read.csv("input/Duration of ventilation - long data format.csv") %>%
          as.data.frame()
      }
      pairwise_data=as_tibble(read.csv("pairwise/blood/output/Duration_of_ventilation.csv", stringsAsFactors = F))
    }
    hy.prior1 = -2.34
    hy.prior2 = 0.3303
    file_name = "Duration of ventilation"
    
    data.baseline=read.csv("pairwise/drugs/Duration of ventilation_wide data.csv")
    data.baseline=data.baseline %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2))
    
    prob.ref.value=data.baseline %>%
      filter(t1==placebo | t2==placebo) %>%
      summarise(median=median(mean2)) %>% as.numeric()
    
  } else if (output == "Time to symptom resolution"){
    if (drugs_or_blood == "drugs"){
      if(measure == "ROM" || measure == "RD"){
        data=read_csv("NMA/drugs/Time_to_symptom_resolution.csv") %>%
          as.data.frame()
      } else {
        ddata=read.csv("input/Time to symptom resolution - long data format.csv") %>%
          as.data.frame()
      }
      pairwise_data=as_tibble(read.csv("pairwise/drugs/output/Time_to_symptom_resolution.csv", stringsAsFactors = F))
    } else if (drugs_or_blood == "blood"){
      if(measure == "ROM" || measure == "RD"){
        data=read_csv("NMA/blood/Time_to_symptom_resolution.csv") %>%
          as.data.frame()
      } else {
        data=read.csv("input/Time to symptom resolution - long data format.csv") %>%
          as.data.frame()
      }
      pairwise_data=as_tibble(read.csv("pairwise/blood/output/Time_to_symptom_resolution.csv", stringsAsFactors = F))
    }
    hy.prior1 = -2.34
    hy.prior2 = 0.3303
    file_name = "Time to symptom"
    
    data.baseline=read.csv("input/Time to symptom resolution - wide data format.csv")
    data.baseline=data.baseline %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2))
    
    prob.ref.value=data.baseline %>%
      filter(t1==placebo | t2==placebo) %>%
      summarise(median=median(mean2)) %>% as.numeric()
    
  } else if (output == "Time to to viral clearance"){
    if (drugs_or_blood == "drugs"){
      if(measure == "ROM" || measure == "RD"){
        data=read_csv("NMA/drugs/Time_to_viral_clearance.csv") %>%
          as.data.frame()
      } else {
        ddata=read.csv("input/Time to viral clearance - long data format.csv") %>%
          as.data.frame()
      }
      pairwise_data=as_tibble(read.csv("pairwise/drugs/output/Time_to_viral_clearance.csv", stringsAsFactors = F))
    } else if (drugs_or_blood == "blood"){
      if(measure == "ROM" || measure == "RD"){
        data=read_csv("NMA/blood/Time_to_viral_clearance.csv") %>%
          as.data.frame()
      } else {
        data=read.csv("input/Time to viral clearance - long data format.csv") %>%
          as.data.frame()
      }
      pairwise_data=as_tibble(read.csv("pairwise/blood/output/Time_to_viral_clearance.csv", stringsAsFactors = F))
    }
    hy.prior1 = -2.34
    hy.prior2 = 0.3303
    file_name = "Time to viral clearance"
    
    data.baseline=read.csv("input/Time to viral clearance - wide data format.csv")
    data.baseline=data.baseline %>% mutate(t1=gsub("^\\d+_(.*$)","\\1",t1),t2=gsub("^\\d+_(.*$)","\\1",t2))
    
    prob.ref.value=data.baseline %>%
      filter(t1==placebo | t2==placebo) %>%
      summarise(median=median(mean2)) %>% as.numeric()
  }
  if (prob.ref.value ==0){
    prob.ref.value = 0.001
  }
  
  getestimatesnma(data,
                  pairwise_data,
                  measure,
                  likelihood,
                  link,
                  linearModel,
                  hy.prior1,
                  hy.prior2,
                  output_dir,
                  file_name,
                  prob.ref.value,
                  placebo)
  
  get.network.pdf(data.baseline, measure, placebo, mainDir, file_name)
    
}

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