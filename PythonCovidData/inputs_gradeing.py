#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Mon Feb 15 18:21:17 2021

@author: antonio
"""

# Definicion de variables globales

#old_Name_File_Data is used for main_grading
#it is the file it compares against, if there is no file, it is equal to 0
old_Name_File_Data = 0
# old_Name_File_Data = "COVID19 NMA Therapy Data (22-11-2021) - revised.xlsx"
Name_File_Data = "COVID19 NMA Blood Products (13-06-2022) - CP trials.xlsx" #nombre de archivo

gradeing_severity = True

Trial_Data = "Trial characteristics" #nombre de una hoja
Trial_Data2 = "Risk of bias"

Dichotomous = "Dichotomous outcomes"
Continuous = "Continuous outcomes"

drugs_or_blood = "drugs"

severity = 2

#Nodes 
#Replace with ( nodes_name = 0 ) if the coded grouping is desired to be used, instead of the file
# nodes_name = 0
nodes_name = "COVID19 NMA Blood Products Table of Nodes (28-10-2021).xlsx"

######trial characteristics table
#filter by treatment pair
filter_treat = []
# filter_treat = ["nirmatrelvir", "remdesivir"]#["molnupiravir", "fluvoxamine"]
replace_node = False
first_intervention_column = 34
intersection = False # si filtra solo los que incluyen ambos tratamientos o no
severity_columns_number_range = [25, 28]