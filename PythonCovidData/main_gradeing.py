#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Mon Feb 15 18:21:17 2021

@author: antonio
"""

import pandas as pd            #for creating the spreadsheet
import numpy as np             #for nan
import re as re                #for sub

from Functions import *

from inputs_gradeing import *

# Import the input excel file

if type(old_Name_File_Data) != int:
    #OldTrialPrim = pd.read_excel(old_Name_File_Data, header = None, sheet_name = Trial_Data)
    OldRoBPrim = pd.read_excel(old_Name_File_Data, header = None, sheet_name = Trial_Data2)
    OldDichPrim = pd.read_excel(old_Name_File_Data, header = None, sheet_name = Dichotomous)
    OldContPrim = pd.read_excel(old_Name_File_Data, header = None, sheet_name = Continuous)

#TrialPrim = pd.read_excel(Name_File_Data, header = None, sheet_name = Trial_Data)
RoBPrim = pd.read_excel(Name_File_Data, header = None, sheet_name = Trial_Data2)
DichPrim = pd.read_excel(Name_File_Data, header = None, sheet_name = Dichotomous)
ContPrim = pd.read_excel(Name_File_Data, header = None, sheet_name = Continuous)

if nodes_name == 0:
    nodes_file = 0
    
else:
    nodes_file = pd.read_excel(nodes_name)
    nodes_file = nodes_file.iloc[:, 0:2]

# obtains the data on the "Trial "characteristics" sheet ready to process

# obtains de data from risk of bias sheet

RoB = get_risk_of_bias_ready(RoBPrim, Trial_Data2)
RoB = RoB.replace("NR", np.nan)
RoB = find_int_in_string(RoB, start_column = 2, end_column = 9)

if type(old_Name_File_Data) != int:
    OldRoB = get_risk_of_bias_ready(OldRoBPrim, Trial_Data2)
    OldRoB = OldRoB.replace("NR", np.nan)
    OldRoB = find_int_in_string(OldRoB, start_column = 2, end_column = 9)

# obtains the data on the "Dichotomous outcome" sheet ready to process

if gradeing_severity:
    Dich = get_outcomes_ready_severity(DichPrim, Dichotomous, directory_file = nodes_file)
else:
    Dich = get_outcomes_ready(DichPrim, Dichotomous, directory_file = nodes_file)
Dich.rename(columns = {"Outcome" : "Dichotomous Outcome"}, inplace = True)


partial2 = get_partial(RoB, Dich, "Dichotomous Outcome")

if type(old_Name_File_Data) != int:
    if gradeing_severity:
        OldDich = get_outcomes_ready_severity(OldDichPrim, Dichotomous, directory_file = nodes_file)
    else:
        OldDich = get_outcomes_ready(OldDichPrim, Dichotomous, directory_file = nodes_file)
    OldDich.rename(columns = {"Outcome" : "Dichotomous Outcome"}, inplace = True)
    Oldpartial2 = get_partial(RoB, OldDich, "Dichotomous Outcome")

# obtains the data on the "Continuous outcome" sheet ready to process
if gradeing_severity:
    Cont = get_outcomes_ready_severity(ContPrim, Continuous, directory_file = nodes_file)
else:
    Cont = get_outcomes_ready(ContPrim, Continuous, directory_file = nodes_file)
Cont.rename(columns = {"Outcome" : "Continuous Outcome"}, inplace = True)

partial1 = get_partial(RoB, Cont, "Continuous Outcome")

if type(old_Name_File_Data) != int:
    if gradeing_severity:
        OldCont = get_outcomes_ready_severity(OldContPrim, Continuous, directory_file = nodes_file)
    else:
        OldCont = get_outcomes_ready(OldContPrim, Continuous, directory_file = nodes_file)
    OldCont.rename(columns = {"Outcome" : "Continuous Outcome"}, inplace = True)

    Oldpartial1 = get_partial(RoB, OldCont, "Continuous Outcome")

if gradeing_severity:
    partial1 = partial1[partial1["Severe"] == severity]
    partial2 = partial2[partial2["Severe"] == severity]
    
    if type(old_Name_File_Data) != int:
        
        Oldpartial1 = Oldpartial1[Oldpartial1["Severe"] == severity]
        
        Oldpartial2 = Oldpartial2[Oldpartial2["Severe"] == severity]
# get the inner join from both dataframes

# get the merged dataframe with Dichotomous and Continuous

inner_join_precursors = literally_a_black_box_that_gets_us_what_we_need_lol(partial1, partial2)

if type(old_Name_File_Data) != int:
    # do the same for the old version

    old_inner_join_precursors = literally_a_black_box_that_gets_us_what_we_need_lol(Oldpartial1, Oldpartial2)

else:
    
    old_inner_join_precursors = 0

#find the difference and the number

inner_join_precursors = number_of_new_treatments_column(old_inner_join_precursors, inner_join_precursors)

correct_column_naming(inner_join_precursors)

inner_join_precursors = convert_bias_from_numbers(inner_join_precursors)

#insert the empty columns we might need in the future
insert_empty_columns(inner_join_precursors, 6)

# the same, but for the adverse effects sheet we need. We just repeat a lot of the steps. Might define a function later
#starts here
if gradeing_severity:
    adverse_Dich = get_outcomes_ready_severity(DichPrim, Dichotomous, adverse_events = True, directory_file = nodes_file)
else:
    adverse_Dich = get_outcomes_ready(DichPrim, Dichotomous, adverse_events = True, directory_file = nodes_file)
adverse_Dich.rename(columns = {"Outcome" : "Dichotomous Outcome"}, inplace = True)


adverse_partial2 = get_partial(RoB, adverse_Dich, "Dichotomous Outcome")

if type(old_Name_File_Data) != int:
    if gradeing_severity:
        adverse_OldDich = get_outcomes_ready_severity(OldDichPrim, Dichotomous, adverse_events = True, directory_file = nodes_file)
    else:    
        adverse_OldDich = get_outcomes_ready(OldDichPrim, Dichotomous, adverse_events = True, directory_file = nodes_file)
    adverse_OldDich.rename(columns = {"Outcome" : "Dichotomous Outcome"}, inplace = True)

    adverse_Oldpartial2 = get_partial(RoB, adverse_OldDich, "Dichotomous Outcome")

#continuous
if gradeing_severity:
    adverse_Cont = get_outcomes_ready_severity(ContPrim, Continuous, adverse_events = True, directory_file = nodes_file)
else:
    adverse_Cont = get_outcomes_ready(ContPrim, Continuous, adverse_events = True, directory_file = nodes_file)
adverse_Cont.rename(columns = {"Outcome" : "Continuous Outcome"}, inplace = True)


adverse_partial1 = get_partial(RoB, adverse_Cont, "Continuous Outcome")

if type(old_Name_File_Data) != int:
    if gradeing_severity:
        adverse_OldCont = get_outcomes_ready_severity(OldContPrim, Continuous, adverse_events = True, directory_file = nodes_file)
    else:
        adverse_OldCont = get_outcomes_ready(OldContPrim, Continuous, adverse_events = True, directory_file = nodes_file)
    adverse_OldCont.rename(columns = {"Outcome" : "Continuous Outcome"}, inplace = True)


    adverse_Oldpartial1 = get_partial(RoB, adverse_OldCont, "Continuous Outcome")

if gradeing_severity:
    adverse_partial1 = adverse_partial1[adverse_partial1["Severe"] == severity]
    adverse_partial2 = adverse_partial2[adverse_partial2["Severe"] == severity]
    
    if type(old_Name_File_Data) != int:
        
        Oldpartial1 = Oldpartial1[Oldpartial1["Severe"] == severity]
        
        Oldpartial2 = Oldpartial2[Oldpartial2["Severe"] == severity]

#merge

adverse_inner_join_precursors = literally_a_black_box_that_gets_us_what_we_need_lol(adverse_partial1, adverse_partial2)

#old merge

if type(old_Name_File_Data) != int:
    adverse_old_inner_join_precursors = literally_a_black_box_that_gets_us_what_we_need_lol(adverse_Oldpartial1, adverse_Oldpartial2)

else:
    
    adverse_old_inner_join_precursors = 0
    
adverse_inner_join_precursors = number_of_new_treatments_column(adverse_old_inner_join_precursors, adverse_inner_join_precursors)

correct_column_naming(adverse_inner_join_precursors)

adverse_inner_join_precursors = convert_bias_from_numbers(adverse_inner_join_precursors)

insert_empty_columns(adverse_inner_join_precursors, 6)

#ends here

if drugs_or_blood == "drugs":
    #mortality sheet extracted by Dichotomous Outcome = 1
    
    mortality = gradeing_sheet_parse(inner_join_precursors, "Dichotomous Outcome", 1)
    
    #ventilation sheet extracted by Dichotomous outcome = 2
    
    ventilation = gradeing_sheet_parse(inner_join_precursors, "Dichotomous Outcome", 2)
    
    #hospital admission sheet extracted by Dichotomous outcome = 3 and all the rest
    
    hospital_admission = gradeing_sheet_parse(inner_join_precursors, "Dichotomous Outcome", 3)
    
    #for adverse events we need to not combine the chloroquines
    adverse_events = gradeing_sheet_parse(adverse_inner_join_precursors, "Dichotomous Outcome", 4)
    
    viral_clearance = gradeing_sheet_parse(inner_join_precursors, "Dichotomous Outcome", 5)
    
    venous_thromboembolism = gradeing_sheet_parse(inner_join_precursors, "Dichotomous Outcome", 6)
    
    bleeding = gradeing_sheet_parse(inner_join_precursors, "Dichotomous Outcome", 7)
    
    hospitalization_los = gradeing_sheet_parse(inner_join_precursors, "Continuous Outcome", 1)
    
    icu_los = gradeing_sheet_parse(inner_join_precursors, "Continuous Outcome", 2)
    
    ventilator_free_days = gradeing_sheet_parse(inner_join_precursors, "Continuous Outcome", 3)
    
    duration_ventilation = gradeing_sheet_parse(inner_join_precursors, "Continuous Outcome", 4)
    
    symptom_resolution = gradeing_sheet_parse(inner_join_precursors, "Continuous Outcome", 5)
    
    clearance_time = gradeing_sheet_parse(inner_join_precursors, "Continuous Outcome", 6)
    
elif drugs_or_blood == "blood":
    
    #mortality sheet extracted by Dichotomous Outcome = 1

    mortality = gradeing_sheet_parse(inner_join_precursors, "Dichotomous Outcome", 1)

    #ventilation sheet extracted by Dichotomous outcome = 2

    ventilation = gradeing_sheet_parse(inner_join_precursors, "Dichotomous Outcome", 2)

    #hospital admission sheet extracted by Dichotomous outcome = 3 and all the rest

    hospital_admission = gradeing_sheet_parse(inner_join_precursors, "Dichotomous Outcome", 3)

    #for adverse events we need to not combine the chloroquines
    adverse_events = gradeing_sheet_parse(inner_join_precursors, "Dichotomous Outcome", 4)

    viral_clearance = gradeing_sheet_parse(inner_join_precursors, "Dichotomous Outcome", 5)

    TRALI = gradeing_sheet_parse(inner_join_precursors, "Dichotomous Outcome", 6)

    TACO = gradeing_sheet_parse(inner_join_precursors, "Dichotomous Outcome", 7)

    allergy = gradeing_sheet_parse(inner_join_precursors, "Dichotomous Outcome", 8)
    
    graft = gradeing_sheet_parse(inner_join_precursors, "Dichotomous Outcome", 9)

    hospitalization_los = gradeing_sheet_parse(inner_join_precursors, "Continuous Outcome", 1)

    icu_los = gradeing_sheet_parse(inner_join_precursors, "Continuous Outcome", 2)

    ventilator_free_days = gradeing_sheet_parse(inner_join_precursors, "Continuous Outcome", 3)

    duration_ventilation = gradeing_sheet_parse(inner_join_precursors, "Continuous Outcome", 4)

    symptom_resolution = gradeing_sheet_parse(inner_join_precursors, "Continuous Outcome", 5)

    clearance_time = gradeing_sheet_parse(inner_join_precursors, "Continuous Outcome", 6)

#exporting to excel part
if severity == 1:
    severity = "Severe-critical"
elif severity == 2:
    severity = "Mild-moderate"
    
if gradeing_severity:
    name_excel = "COVID19 NMA - RoB ratings " + severity + " (created from " + Name_File_Data + ").xlsx"
else:
    name_excel = "COVID19 NMA - RoB ratings for GRADEing (created from " + Name_File_Data + ").xlsx"
notes_list = ["* dexamethasone and methylprednisolone should be one node: glucocorticoids.", \
              "* chloroquine and hydroxychloroquine should be one node for all outcomes, except adverse events leading to discontinuation.", \
              "* interferon subtypes should be lumped in the same node. For example, interferon beta-1a and interferon beta-1b would be classified under the node interferon beta."]
notes = pd.Series(notes_list)

writer = pd.ExcelWriter(name_excel, engine='xlsxwriter')

if drugs_or_blood == "drugs":
    export_df_to_gradeing_sheet(notes, writer, "Notes", name_excel)
    export_df_to_gradeing_sheet(mortality, writer, "Mortality", name_excel)
    export_df_to_gradeing_sheet(ventilation, writer, "Ventilation", name_excel)
    export_df_to_gradeing_sheet(hospital_admission, writer, "Hospital admission", name_excel)
    export_df_to_gradeing_sheet(adverse_events, writer, "Adverse events", name_excel)
    export_df_to_gradeing_sheet(viral_clearance, writer, "Viral clearance", name_excel)
    export_df_to_gradeing_sheet(venous_thromboembolism, writer, "Venous thromboembolism", name_excel)
    export_df_to_gradeing_sheet(bleeding, writer, "Clinically important bleeding", name_excel)
    export_df_to_gradeing_sheet(hospitalization_los, writer, "Hospitalization LOS", name_excel)
    export_df_to_gradeing_sheet(icu_los, writer, "ICU LOS", name_excel)
    export_df_to_gradeing_sheet(ventilator_free_days, writer, "Ventilator-free days", name_excel)
    export_df_to_gradeing_sheet(duration_ventilation, writer, "Duration of ventilation", name_excel)
    export_df_to_gradeing_sheet(symptom_resolution, writer, "Symptom resolution", name_excel)
    export_df_to_gradeing_sheet(clearance_time, writer, "Time to clearance", name_excel)
    
elif drugs_or_blood == "blood":
    export_df_to_gradeing_sheet(notes, writer, "Notes", name_excel)
    export_df_to_gradeing_sheet(mortality, writer, "Mortality", name_excel)
    export_df_to_gradeing_sheet(ventilation, writer, "Ventilation", name_excel)
    export_df_to_gradeing_sheet(hospital_admission, writer, "Hospital admission", name_excel)
    export_df_to_gradeing_sheet(adverse_events, writer, "Adverse events", name_excel)
    export_df_to_gradeing_sheet(viral_clearance, writer, "Viral clearance", name_excel)
    export_df_to_gradeing_sheet(TRALI, writer, "TRALI", name_excel)
    export_df_to_gradeing_sheet(TACO, writer, "TACO", name_excel)
    export_df_to_gradeing_sheet(allergy, writer, "Allergic reactions", name_excel)
    export_df_to_gradeing_sheet(graft, writer, "Graft vs host disease", name_excel)
    export_df_to_gradeing_sheet(hospitalization_los, writer, "Hospitalization LOS", name_excel)
    export_df_to_gradeing_sheet(icu_los, writer, "ICU LOS", name_excel)
    export_df_to_gradeing_sheet(ventilator_free_days, writer, "Ventilator-free days", name_excel)
    export_df_to_gradeing_sheet(duration_ventilation, writer, "Duration of ventilation", name_excel)
    export_df_to_gradeing_sheet(symptom_resolution, writer, "Symptom resolution", name_excel)
    export_df_to_gradeing_sheet(clearance_time, writer, "Time to clearance", name_excel)

writer.save()