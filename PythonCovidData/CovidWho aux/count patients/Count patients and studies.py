#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Mon Sep 20 04:38:30 2021

@author: antonio
"""

import pandas as pd            #for creating the spreadsheet
import numpy as np             #for nan
#import re as re

Name_File_Data = "All continuous outcomes_long data for analysis_20211122.xlsx"
Prim = pd.read_excel(Name_File_Data, header = [0,1])

Prim.rename(columns=lambda x:x.split(" (")[0], inplace = True)

#column_names = Prim.columns.levels[0].values.tolist()
df_list = []

for column in Prim.columns.levels[0]:
    dfr = Prim[column]
    df = dfr.copy()
    df = df.groupby("treatment").agg({"sampleSize":"sum","RefID":"count"})
    df = df.rename(columns={"sampleSize":"No. of patients in each node","refid":"No. of studies in each node"})
    df.columns = pd.MultiIndex.from_product([[column], df.columns])
    df_list.append(df)
    
    
out = pd.concat(df_list, axis = 1, join = 'outer')

name_excel = "Patients and studies by treament (created from " + Name_File_Data + ").xlsx"

writer = pd.ExcelWriter(name_excel, engine='xlsxwriter')

out.to_excel(writer, sheet_name = 'Table', index = True, header = True, startrow = 0, startcol = 0)

workbook = writer.book
worksheet = writer.sheets['Table']  # pull worksheet 

format_left = workbook.add_format({'align': 'left', 'valign': 'top'})
worksheet.set_column(0, 0, 30, format_left)
worksheet.set_column(1, 2*len(df_list), 8, format_left)

writer.save()




