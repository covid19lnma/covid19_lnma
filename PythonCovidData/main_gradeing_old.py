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

OldPrim = pd.read_excel(old_Name_File_Data, header = None, sheet_name = Trial_Data)
OldPrim2 = pd.read_excel(old_Name_File_Data, header = None, sheet_name = Trial_Data2)

TrialsPrim = pd.read_excel(Name_File_Data, header = None, sheet_name = Trial_Data)
TrialsPrim2 = pd.read_excel(Name_File_Data, header = None, sheet_name = Trial_Data2)

if nodes_name == 0:
    nodes_file = 0
    
else:
    nodes_file = pd.read_excel(nodes_name)

# obtains the data on the "Trial "characteristics" sheet ready to process

old_Precursor_1 = get_trial_characteristics_ready(OldPrim, Trial_Data, directory_file = nodes_file)

Precursor_1 = get_trial_characteristics_ready(TrialsPrim, Trial_Data, directory_file = nodes_file)

# obtains de data from risk of bias sheet

old_Precursor_2 = get_risk_of_bias_ready(OldPrim2, Trial_Data2)

Precursor_2 = get_risk_of_bias_ready(TrialsPrim2, Trial_Data2)

#cleans the strings off from the coded columns

old_Precursor_2 = find_int_in_string(old_Precursor_2, start_column = 2, end_column = 9)

Precursor_2 = find_int_in_string(Precursor_2, start_column = 2, end_column = 9)

# get the inner join from both dataframes

old_inner_join_precursors = pdmerge_id(old_Precursor_1, old_Precursor_2, on = ["Ref ID"])

inner_join_precursors = pdmerge_id(Precursor_1, Precursor_2, on = ["Ref ID"])

#find the difference

df = differences_on_new_doc(old_inner_join_precursors, inner_join_precursors)

#count the amount of treatment convinations that are new between gradeing studies

new_treatments_number = df.value_counts().reset_index()
new_treatments_number.rename(columns = {0 : "# of new trials"}, inplace = True)

# finally gets the dataframe ready to export in an excel, ready for formatting

inner_join_precursors = pd.merge(inner_join_precursors, new_treatments_number, how = 'left', on = ["Treatment 1", "Treatment 2"])

inner_join_precursors["# of new trials"].fillna(value = 0, inplace = True)

#change column of number of new trials of place and group by treatment combination

new_trials_column = inner_join_precursors.pop("# of new trials") 
inner_join_precursors.insert(2, "# of new trials", new_trials_column) 
inner_join_precursors.sort_values(by = ["Treatment 1", "Treatment 2"], inplace = True)

correct_column_naming(inner_join_precursors)

inner_join_precursors = convert_bias_from_numbers(inner_join_precursors)

#insert the empty columns we might need in the future
insert_empty_columns(inner_join_precursors, 6)

# the same, but for the adverse effects sheet we need. We just repeat a lot of the steps. Might define a function later
#starts here

old_adverse_Precursor_1 = get_trial_characteristics_ready(OldPrim, Trial_Data, adverse_events = True, directory_file = nodes_file)

adverse_Precursor_1 = get_trial_characteristics_ready(TrialsPrim, Trial_Data, adverse_events = True, directory_file = nodes_file)

old_adverse_inner_join_precursors = pdmerge_id(old_adverse_Precursor_1, old_Precursor_2, on = ["Ref ID"])

adverse_inner_join_precursors = pdmerge_id(adverse_Precursor_1, Precursor_2, on = ["Ref ID"])

adverse_df = differences_on_new_doc(old_adverse_inner_join_precursors, adverse_inner_join_precursors)

adverse_new_treatments_number = adverse_df.value_counts().reset_index()

adverse_new_treatments_number.rename(columns = {0 : "# of new trials"}, inplace = True)

adverse_inner_join_precursors = pd.merge(adverse_inner_join_precursors, adverse_new_treatments_number, \
                                how = 'left', on = ["Treatment 1", "Treatment 2"])

adverse_inner_join_precursors["# of new trials"].fillna(value = 0, inplace = True)

adverse_new_trials_column = adverse_inner_join_precursors.pop("# of new trials") 
adverse_inner_join_precursors.insert(2, "# of new trials", adverse_new_trials_column) 
adverse_inner_join_precursors.sort_values(by = ["Treatment 1", "Treatment 2"], inplace = True)

correct_column_naming(adverse_inner_join_precursors)

adverse_inner_join_precursors = convert_bias_from_numbers(adverse_inner_join_precursors)

insert_empty_columns(adverse_inner_join_precursors, 6)

#ends here

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

#exporting to excel part

name_excel = "COVID19 NMA - RoB ratings for GRADEing (created from " + Name_File_Data + ").xlsx"
notes_list = ["* dexamethasone and methylprednisolone should be one node: glucocorticoids.", \
              "* chloroquine and hydroxychloroquine should be one node for all outcomes, except adverse events leading to discontinuation.", \
              "* interferon subtypes should be lumped in the same node. For example, interferon beta-1a and interferon beta-1b would be classified under the node interferon beta."]
notes = pd.Series(notes_list)

writer = pd.ExcelWriter(name_excel, engine='xlsxwriter')

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
export_df_to_gradeing_sheet(symptom_resolution, writer, "Symptom resolution", name_excel)
export_df_to_gradeing_sheet(clearance_time, writer, "Time to clearance", name_excel)

writer.save()