#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Tue Mar  9 17:24:53 2021

@author: antonio
"""


import pandas as pd            #for creating the spreadsheet
import numpy as np             #for nan
import re as re                #for sub

from Functions import cleandf, id_order, find_int_in_string

from Functions_2 import clean_trial_name

from Functions_condensed import *

from inputs_gradeing import *


# Import the input excel file

TrialsPrim = pd.read_excel(Name_File_Data, header = None, sheet_name = Trial_Data)

#gets the dataframes cleaned and re-indexed, this functions are found in "Functions.py"

Precursor_1 = id_order(cleandf(TrialsPrim))

#First we process the trial characteristics sheet in preparation for the merge

#We do so extracting the data into different columns with the correct cell format we desire, one column at a time

Precursor_1 = clean_trial_name(Precursor_1)

Precursor_1 = find_float_in_string(Precursor_1, start_column = 22, end_column = 28)

n1 = registered_cell(Precursor_1)

n2 = publication_cells(Precursor_1)

n3 = country_cells(Precursor_1)

n4 = intensity_of_care_cells(Precursor_1)

n5 = severity_cells(Precursor_1)

n6 = ventilation_cell(Precursor_1)

n7 = number_patients_cell(Precursor_1)
    
header = pd.Series({"Study characteristics" : "n (%) or median [iqr]"})

df_to_excel = pd.concat([header, n1, n2, n7, n3, n4, n5, n6], axis = 0)

name_excel = "Avreviated table 1 (created from " + Name_File_Data + ").xlsx"

writer = pd.ExcelWriter(name_excel, engine='xlsxwriter')

df_to_excel.to_excel(writer, sheet_name = 'Table', index = True, header = False, startrow = 1, startcol = 0)

workbook = writer.book
worksheet = writer.sheets['Table']  # pull worksheet 

format_left = workbook.add_format({'align': 'left', 'valign': 'top'})
worksheet.set_column(0, 1, 35, format_left)

writer.save()