#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Mon Mar  1 06:39:14 2021

@author: antonio
"""

import pandas as pd            #for creating the spreadsheet
import numpy as np             #for nan
import re as re                #for sub

from Functions import cleandf, id_order, find_int_in_string, clean_treatments_names

from Functions_2 import *

from inputs_gradeing import *

# Import the input excel file

TrialsPrim = pd.read_excel(Name_File_Data, header = None, sheet_name = Trial_Data)
TrialsPrim2 = pd.read_excel(Name_File_Data, header = None, sheet_name = Trial_Data2)

if nodes_name == 0:
    nodes_file = 0
else:
    nodes_file = pd.read_excel(nodes_name)
    nodes_file = nodes_file.iloc[:, 0:2]

# gets the dataframes cleaned and re-indexed, this functions are found in "Functions.py"

#filter treatments by nodes but without replacing the displayed entry
if replace_node == True:
    Precursor_1 = id_order(clean_node_treatments_names(cleandf(TrialsPrim),filter_treat,directory_file=nodes_file))
else:
    Precursor_1 = id_order(clean_treatments_names(cleandf(TrialsPrim), directory_file=nodes_file))

Precursor_2 = id_order(cleandf(TrialsPrim2))

# From this point forward, we are using functions from "Functions_2.py"
# First we process the trial characteristics sheet in preparation for the merge

# We do so extracting the data into different columns with the correct cell format we desire, one column at a time

Precursor_1 = clean_trial_name(Precursor_1)
#Precursor_1_aux = find_int_in_string(Precursor_1, start_column = 5, end_column = 6)

# bandaid 2 to get integers from the n randomized columns

for n in range(1, 20):
            #print(n)
    try:
        Precursor_1["Intervention {}".format(n)]
        Precursor_1 = find_int_in_string(Precursor_1, start_column = first_intervention_column + 3*n, end_column = first_intervention_column +3*n)
        
    except KeyError:
        break

# n is the number of intervention columns there is minus 1  

#filtra por la lista de tratamientos
Precursor_1 = filter_treatment_pair(Precursor_1, filter_treat, n, intersection = intersection, node = replace_node)

# Next we get the subdataframe, dropping all the columns we aren't going to use, for easier readability

PSc = "Publication/Study characteristics"
Bpc = "Baseline patient characteristics"
Bcc = "Baseline clinical characteristics"

Precursor_1 = study_column(Precursor_1)

Precursor_1 = publication_status_registration_column(Precursor_1)

Precursor_1 = number_of_participants_column(Precursor_1)

Precursor_1 = country_column(Precursor_1)

Precursor_1 = mean_age_column(Precursor_1)

Precursor_1 = percentage_male_column(Precursor_1)

Precursor_1 = comorbidities_column(Precursor_1)

Precursor_1 = type_of_care_column(Precursor_1)

#find string in severity columns
Precursor_1 = find_int_in_string(Precursor_1, start_column = severity_columns_number_range[0], end_column = severity_columns_number_range[1])

Precursor_1 = severity_column(Precursor_1)

Precursor_1 = percentage_mechanical_ventilation_column(Precursor_1)

Precursor_1 = ventilation_percentages_column(Precursor_1)

Precursor_1 = treatments_column(Precursor_1, Name_File_Data)

# after getting all the columns we want, we get the subdataframe of the reference id and 1st author
# with the columns we obtained from the chain of functions previously applied

subdf_trial = [("Ref ID", "Ref ID"), ("1st Author", "1st Author"), ("Study", "Study"), ("Publication status\nRegistration", "Publication status\nRegistration"), \
               ("Number of\nparticipants", "Number of\nparticipants"), ("Country", "Country"), ("Mean\nage", "Mean\nage"), \
               ("% Male", "% Male"), ("Comorbidities", "Comorbidities"), ("Type of care", "Type of care"), ("Severity", "Severity"), \
               ("% Mechanical \nventilation \n(at baseline)", "% Mechanical \nventilation \n(at baseline)"), \
               ("Detailed ventilation (%)", "Detailed ventilation (%)"), ("Treatments (dose and duration)", "Treatments (dose and duration)")]

# subdf_trial = [("Ref ID", "Ref ID"), ("1st Author", "1st Author"), ("Study", "Study"), ("Publication status\nRegistration", "Publication status\nRegistration"), \
#                ("Number of\nparticipants", "Number of\nparticipants"), ("Country", "Country"), ("Mean\nage", "Mean\nage"), \
#                ("% Male", "% Male"), ("Comorbidities", "Comorbidities"), ("Treatments (dose and duration)", "Treatments (dose and duration)")]

Precursor_1 = Precursor_1[subdf_trial]

# and we simplify the multiindexed column names to just one level, since we dropped the columns which needed the multiindex

Precursor_1.columns = Precursor_1.columns.droplevel(0)

# We process the risk of bias sheet next

# dropping some columns

Rob = "Risk of bias (Use the RoB guidance)"

drop_columns2 = [Rob, "Comments"]

Precursor_2 = Precursor_2.drop(drop_columns2, level = 0, axis = 1)

if drugs_or_blood == "drugs":
    Precursor_2 = precursor_column_outcomes(Precursor_2)
elif drugs_or_blood == "blood":
    Precursor_2 = precursor_column_outcomes_blood(Precursor_2)
#simplifying multiindex column names

poc = "Outcomes"

Precursor_2.columns = Precursor_2.columns.droplevel(0)

# Here we group by the same reference id and authors the column of the outcomes we got
# aggregating with a in-line lambda function which concatenates the cells in different columns

Precursor_2 = Precursor_2.groupby(["Ref ID", "1st Author"], as_index = False)[poc].agg(lambda col: "\n".join(col))

# inner join. This means we mere over the similar columns the 2 dataframes

inner_join_precursors = stronger_left_join_trial_characteristics(Precursor_1, Precursor_2)

#list of row heights
heights = set_row_heights(inner_join_precursors)

#export to excel
#Where the table starts
start_row = 2
start_column = 2

if bool(filter_treat):
    
    if filter_treat[0] == "standard care/placebo":
        
        filter_treat[0] = "placebo"
        
    if filter_treat[1] == "standard care/placebo":
        
        filter_treat[1] = "placebo"
    
    if filter_treat[0] == "placebo/standard care":
        
        filter_treat[0] = "placebo"
        
    if filter_treat[1] == "placebo/standard care":
        
        filter_treat[1] = "placebo"
        
    if filter_treat[0] == "ACEi/ARB":
        
        filter_treat[0] = "ACEi-ARB"
        
    if filter_treat[1] == "ACEi/ARB":
        
        filter_treat[1] = "ACEi-ARB"
    
    if drugs_or_blood == "drugs":
        name_excel = "COVID19 NMA - Table of study characteristics - Drug, filtered for " + filter_treat[0] + " and " + filter_treat[1] + " (created from " + Name_File_Data + ").xlsx"
    elif drugs_or_blood == "blood":
        name_excel = "COVID19 NMA - Table of study characteristics - Blood products filtered for " + filter_treat[0] + " and " + filter_treat[1] + " (created from " + Name_File_Data + ").xlsx"
else:
    if drugs_or_blood == "drugs":
        name_excel = "COVID19 NMA - Table of study characteristics - Drug (created from " + Name_File_Data + ").xlsx"
    elif drugs_or_blood == "blood":
        name_excel = "COVID19 NMA - Table of study characteristics - Blood products (created from " + Name_File_Data + ").xlsx"

writer = pd.ExcelWriter(name_excel, engine='xlsxwriter')

inner_join_precursors.to_excel(writer, sheet_name = 'Therapy - Pharma', index = False, header = False, startrow = start_row + 1, startcol = start_column)

#create book and sheet
workbook = writer.book
worksheet = writer.sheets['Therapy - Pharma']  # pull worksheet 

worksheet.set_zoom(90)

format_left = workbook.add_format({'align': 'left', 'valign': 'top'})
format_center = workbook.add_format({'align': 'center', 'valign': 'top'})
#set widt and alignment of all the columns
#study column
worksheet.set_column(start_column, start_column, 33, format_center)
#reg column
worksheet.set_column(start_column + 1, start_column + 1, 20, format_left)
#participants column
worksheet.set_column(start_column + 2, start_column + 2, 18, format_center)
#country column
worksheet.set_column(start_column + 3, start_column + 3, 23, format_center)
#age column
worksheet.set_column(start_column + 4, start_column + 4, 7, format_center)
#gender male column
worksheet.set_column(start_column + 5, start_column + 5, 7, format_center)
#comorbidities column
worksheet.set_column(start_column + 6, start_column + 6, 48, format_left)
#type of care column
worksheet.set_column(start_column + 7, start_column + 7, 23, format_center)
#severity column
worksheet.set_column(start_column + 8, start_column + 8, 25, format_center)
#mech ventilation column
worksheet.set_column(start_column + 9, start_column + 9, 15, format_center)
#in detail vent column
worksheet.set_column(start_column + 10, start_column + 10, 30, format_left)
#treatments column
worksheet.set_column(start_column + 11, start_column + 11, 80, format_left)
#outcomes
worksheet.set_column(start_column + 12, start_column + 12, 45, format_left)

#establish header format
header_format = workbook.add_format({
    'bold': True,
    'align': 'center',
    'fg_color': '#d0cece',
    'border': 1})
header_format.set_align('vcenter')

# Write the column headers with the defined format.
for col_num, value in enumerate(inner_join_precursors.columns.values):
    worksheet.write(start_row, col_num + start_column, value, header_format)

#header height
worksheet.set_row(2, 45)

#rows height
for index in range(0, len(heights)):
    
    worksheet.set_row(index + 3, heights[index])

writer.save()
