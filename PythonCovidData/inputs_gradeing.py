#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Mon Feb 15 18:21:17 2021

@author: antonio
"""

# Definicion de variables globales
old_Name_File_Data = 0
# old_Name_File_Data = "COVID19 NMA Therapy Data (22-11-2021) - revised.xlsx"
Name_File_Data = "COVID19 NMA Blood Products (13-06-2022) - CP trials.xlsx" #nombre de archivo

Trial_Data = "Trial characteristics" #nombre de una hoja
Trial_Data2 = "Risk of bias"

Dichotomous = "Dichotomous outcomes"
Continuous = "Continuous outcomes"

severity = 2

#Nodes 
#Replace with ( nodes_name = 0 ) if the coded grouping is desired to be used, instead of the file
# nodes_name = 0
nodes_name = "COVID19 NMA Blood Products Table of Nodes (28-10-2021).xlsx"

#trial characteristics table
#filter by treatment pair
filter_treat = []
# filter_treat = ["nirmatrelvir", "remdesivir"]#["molnupiravir", "fluvoxamine"]