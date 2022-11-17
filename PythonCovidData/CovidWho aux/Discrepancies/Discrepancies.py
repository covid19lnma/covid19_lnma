#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Wed Oct  6 19:53:00 2021

@author: antonio
"""

import pandas as pd            #for creating the spreadsheet
import numpy as np             #for nan

import sys
sys.path.append("../..")

from Functions import cleandf

#####name of inputs
Name_File_Data = "COVID19 NMA Therapy Data (06-12-2021) - revised.xlsx"
log_File_Data = "Included Log (10).xlsx"

####import and basic cleaning
Prim_NMA = pd.read_excel(Name_File_Data, header = None, sheet_name = "Trial characteristics")
Prim_log = pd.read_excel(log_File_Data, header = [0])

Prim_NMA =cleandf(Prim_NMA, total_nan = True)
Prim_log = Prim_log.rename(columns={"Status of Trial Being Used (0=using, 1=preprint, 2=duplicate(diff version of preprint, final copy of journal proof), 3=correction, 4=preliminary report (separate publication) of final report, 5=systematic review/MA, 6=post-hoc infection, 7=unpublished data of now preprint/peerreviewed version, 8=retraction":"Status of Trial"})
Prim_log = Prim_log.replace("NR", np.nan)
    
Prim_log = Prim_log.astype({"Study ID": str})
Prim_NMA = Prim_NMA.astype({("Ref ID","Ref ID"): str})

#filter column u to 0 or 5

Prim_log = Prim_log[(Prim_log["Status of Trial"]==0) | (Prim_log["Status of Trial"]==5)]

#clean columns
Log_df = Prim_log.copy()
NMA_df = Prim_NMA.copy()

NMA_df[("Trial registration","Trial registration")] = \
    NMA_df[("Trial registration","Trial registration")].str.replace(" ", "")
NMA_df[("Trial registration","Trial registration")] = \
    NMA_df[("Trial registration","Trial registration")].str.replace(",", ", ")
    
Log_df["Trial Registry Number"] = Log_df["Trial Registry Number"].str.replace(", and", ",")
Log_df["Trial Registry Number"] = Log_df["Trial Registry Number"].str.replace("and", ",")
Log_df["Trial Registry Number"] = Log_df["Trial Registry Number"].str.replace(";", ",")
Log_df["Trial Registry Number"] = Log_df["Trial Registry Number"].str.replace(" ", "")
Log_df["Trial Registry Number"] = Log_df["Trial Registry Number"].str.replace(",", ", ")

NMA_df.reset_index(inplace = True, drop = True)
Log_df.reset_index(inplace = True, drop = True)

def list_string_containment(l1,l2):
    out=[]
    for x, y in zip(l1,l2):
        if (x in y):
            out.append(x)
        elif (y in x) and (x not in y):
            out.append(y)
    return out

####### "empty" vars for tests, look at what is getting matched
#NMA_empty = NMA_df[0:0]
#Log_empty = Log_df[0:0]

for i in range(0, len(Log_df.index)):
    
    if pd.isna(Log_df.loc[i,"Study ID"]) == False:
        log_ID = Log_df.loc[i,"Study ID"].split(", ")
    else:
        log_ID = ["-$"]
        
    if pd.isna(Log_df.loc[i,"Trial Registry Number"]) == False:
        log_Registry = Log_df.loc[i,"Trial Registry Number"].split(", ")
    else:
        log_Registry = ["+%"]
        
    if pd.isna(Log_df.loc[i,"First Author"]) == False:
        log_Author = Log_df.loc[i,"First Author"]
    else:
        log_Author = "+&%"
    
    NMA_df.reset_index(inplace = True, drop = True)
    
    for j in range(0, len(NMA_df.index)):
        
        if pd.isna(NMA_df.loc[j,("Ref ID", "Ref ID")]) == False:
            trial_ID = NMA_df.loc[j,("Ref ID", "Ref ID")].split(", ")
        else:
            trial_ID = ["*("]
                        
        if pd.isna(NMA_df.loc[j,("Trial registration","Trial registration")]) == False:
            trial_Registry = NMA_df.loc[j,("Trial registration","Trial registration")].split(", ")
        else:
            trial_Registry = ["=!"]
            
        if pd.isna(NMA_df.loc[j,("1st Author","1st Author")]) == False:
            trial_Author = NMA_df.loc[j,("1st Author","1st Author")]
        else:
            trial_Author = "=)!"
            
        if (bool(set(log_ID).intersection(trial_ID)) or \
            bool(list_string_containment(log_Registry,trial_Registry)) or \
            log_Author == trial_Author):
            
            #NMA_empty = NMA_empty.append(NMA_df.loc[j], ignore_index = True)
            #Log_empty = Log_empty.append(Log_df.loc[i], ignore_index = True)
            
            NMA_df.drop(j, inplace = True)
            Log_df.drop(i, inplace = True)
            break
        
#NMA_empty=NMA_empty[["Ref ID","1st Author","Trial registration"]]
#Log_empty=Log_empty[["Study ID","First Author","Trial Registry Number"]]

NMA_df.reset_index(inplace = True, drop = True)
Log_df.reset_index(inplace = True, drop = True)
NMA_df.columns = NMA_df.columns.get_level_values(1)

NMA_aux=NMA_df[["Ref ID","1st Author","Trial registration"]]
Log_aux=Log_df[["Study ID","First Author","Trial Registry Number"]]

##### export
name_excel = "Discrepancies between (" + Name_File_Data + ") and (" + log_File_Data + ").xlsx"

writer = pd.ExcelWriter(name_excel, engine='xlsxwriter')

NMA_aux.to_excel(writer, sheet_name = 'rows not in log file', index = False, header = True)
Log_aux.to_excel(writer, sheet_name = 'rows not in NMA file', index = False, header = True)

workbook = writer.book
format_left = workbook.add_format({'align': 'left', 'valign': 'top'})

worksheet = writer.sheets['rows not in log file']  # pull worksheet 

#set widt and alignment of all the columns
worksheet.set_column(0, 2, 45, format_left)

worksheet = writer.sheets['rows not in NMA file']  # pull worksheet 

#set widt and alignment of all the columns
worksheet.set_column(0, 2, 45, format_left)

#create book and sheet
writer.save()