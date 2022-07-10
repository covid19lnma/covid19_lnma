#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Wed Nov 10 14:36:20 2021

@author: antonio
"""

import pandas as pd            #for creating the spreadsheet
import numpy as np             #for nan
import glob
import itertools
#import re as re                


####Functions
def create_columns(dfPrim):
    dfsP = np.split(dfPrim, [3,6,9,12], axis = 1)

    drop_list = []
    index = -1
    for df in dfsP:
        index += 1
        df.dropna(how = "all", inplace = True)
    
        if df.iloc[1,0] != "None":
        
            df["Effect"] = df.iloc[0,0]
            df.drop([0], inplace = True)
            mapping = {df.columns[0]:'Treatment', df.columns[1]: 'Values', df.columns[2]:'Certainty'}
            df.rename(columns=mapping, inplace = True)
        
        else:
            drop_list.append(index)
        
    for index in sorted(drop_list, reverse=True):
        del dfsP[index]
    
    df = pd.concat(dfsP)
    return df

def bg_color(subrow):
    blank = '' 

    subrow_out =  pd.Series(blank, index=subrow.index)
    
    if (subrow[1] =="HIGH" or subrow[1] =="MODERATE") and subrow[2] == "Best":
        subrow_out[0] = 'background-color: #465c1e'
        return subrow_out
    
    if (subrow[1] =="HIGH" or subrow[1] =="MODERATE") and subrow[2] == "Better than SC":
        subrow_out[0] = 'background-color: #6aa84f'
        return subrow_out
    
    if (subrow[1] =="HIGH" or subrow[1] =="MODERATE") and subrow[2] == "Not different than SC":
        subrow_out[0] = 'background-color: #ffd966'
        return subrow_out
    
    if (subrow[1] =="HIGH" or subrow[1] =="MODERATE") and subrow[2] == "Harmful":
        subrow_out[0] = 'background-color: #e69138'
        return subrow_out
    
    if (subrow[1] =="HIGH" or subrow[1] =="MODERATE") and subrow[2] == "Worst":
        subrow_out[0] = 'background-color: #cc0000'
        return subrow_out
    
    if subrow[1] =="LOW" and subrow[2] == "Best":
        subrow_out[0] = 'background-color: #d7e8b1'
        return subrow_out
    
    if subrow[1] =="LOW" and subrow[2] == "Better than SC":
        subrow_out[0] = 'background-color: #d9ead3'
        return subrow_out
    
    if subrow[1] =="LOW" and subrow[2] == "Not different than SC":
        subrow_out[0] = 'background-color: #fff2cc'
        return subrow_out
    
    if subrow[1] =="LOW" and subrow[2] == "Harmful":
        subrow_out[0] = 'background-color: #f9cb9c'
        return subrow_out
    
    if subrow[1] =="LOW" and subrow[2] == "Worst":
        subrow_out[0] = 'background-color: #f4cccc'
        return subrow_out
    
    if subrow[1] =="VERY LOW":
        subrow_out[0] = 'background-color: #efefef'
        return subrow_out
     
    else:
         return subrow_out

def round_value(df, name, dich = True, risk_difference = False):
    
    dfr = df.copy()
    
    for i in range(0, len(df.index)):
        
        aux_str = df.loc[i, (name, "Values")]
        aux_list = aux_str.split(' (')
        value = aux_list[0]
        interval = " (" + aux_list[1]
        if dich:
            if risk_difference:
                value = str(round(1000*float(value)))
                aux_interval = interval.split(" to ")
                interval1 = str(round(1000*float(aux_interval[0].replace(" (",""))))
                interval2 = str(round(1000*float(aux_interval[1].replace(")",""))))
                interval = " (" + interval1 + " to " + interval2 + ")"
            else:
                value = str(round(float(value)))
                aux_interval = interval.split(" to ")
                interval1 = str(round(float(aux_interval[0].replace(" (",""))))
                interval2 = str(round(float(aux_interval[1].replace(")",""))))
                interval = " (" + interval1 + " to " + interval2 + ")"
        else:
            value = str("{0:.1f}".format(float(value)))
            aux_interval = interval.split(" to ")
            interval1 = str("{0:.1f}".format(float(aux_interval[0].replace(" (",""))))
            interval2 = str("{0:.1f}".format(float(aux_interval[1].replace(")",""))))
            interval = " (" + interval1 + " to " + interval2 + ")"
        dfr.loc[i, (name, "Values")] = value + interval
    return dfr

def higher_certainty_estimate(df, name):
    
    dfr = df.copy()
    
    dfr.loc[dfr[(name, "Higher certainty estimate")] == "DIRECT", (name, "Values")] = dfr[(name, "Values")] + " *"
    dfr.loc[dfr[(name, "Higher certainty estimate")] == "INDIRECT", (name, "Values")] = dfr[(name, "Values")] + " **"
    
    return dfr

#####import whole directory xlsx files
list_files = glob.glob('./*.xlsx')
sheet = "Automatic classification"
ATsheet = "AT class"
#list_name_dich =[]
#list_name_cont =[]

list_df_dich = []
list_df_cont = []

name_excel = "Figure 2.xlsx"

list_dichotomous = ["Mortality", "Ventilation", "Mechanical Ventilation", "Hospital admission", "Admission to hospital", "Adverse effects", "Adverse events", "Viral clearance", \
                    "Venous thromboembolism", "VTE", "Bleeding", "Clinically important bleeding", "Mortality_Prophylaxis", "Infection with COVID-19 (laboratory confirmed)_Prophylaxis", \
                    "Infection with COVID-19 (lab confirmed+suspected)_Prophylaxis", "Adverse effects_Prophylaxis", "Admission to hospital_Prophylaxis"]
    
list_continuous = ["Hospitalization LOS", "Duration of hospitalization", "ICU LOS", "Ventilator-free days", "Duration of ventilation", "Duration of Mechanical ventilation", \
                   "Symptom resolution", "Time to symptom resolution", "Time to clearance"]

try:
    list_files.remove("./" + name_excel)
except ValueError:
    5

for file in list_files:
    name1 = file.split(' - ')[0]
    name = name1.split("./")[1]
    
    Name_File_Data = file

    dfPrim = pd.read_excel(Name_File_Data, header = None, sheet_name = sheet)
    
    dfATclass = pd.read_excel(Name_File_Data, header = None, sheet_name = ATsheet)
    dfATclass.columns = dfATclass.iloc[1]
    
    dfATclass = dfATclass[["Intervention", "Higher certainty estimate"]]    
    dfATclass.dropna(how = "all", inplace = True)
    dfATclass.drop(dfATclass.index[0], inplace = True)

    df = create_columns(dfPrim)
    df = pd.merge(df, dfATclass, left_on = "Treatment", right_on = "Intervention")
    df.drop("Intervention", axis = 1, inplace = True)
    
    df.columns = pd.MultiIndex.from_product([[name], df.columns])
    df.reset_index(inplace = True, drop = True)

    
    if len(df.loc[df[(name,"Effect")] == "Best"].index) == 0:
        
        df.loc[df[(name,"Effect")] == "Better than SC", (name,"Effect")] = "Best"
        
    if len(df.loc[df[(name,"Effect")] == "Worst"].index) == 0:
        
        df.loc[df[(name,"Effect")] == "Harmful", (name,"Effect")] = "Worst"
    
    if name in list_dichotomous:
        
        #list_name_dich.append(name)
        if name == "Adverse effects" or name == "Adverse events" or name == "Adverse events_Prophylaxis":
            df = round_value(df, name, risk_difference = True)
        else:
            df = round_value(df, name)
        df = higher_certainty_estimate(df, name)
        list_df_dich.append(df)
        
    elif name in list_continuous:
        
        #list_name_cont.append(name)
        df = round_value(df, name, dich = False)
        df = higher_certainty_estimate(df, name)
        list_df_cont.append(df)


list_df_dich= sorted(list_df_dich, key=lambda x:len(x), reverse = True)
list_df_cont= sorted(list_df_cont, key=lambda x:len(x), reverse = True)

list_df = list_df_dich + list_df_cont

### join all the dataframes by treatment
merge_df = pd.concat([df.set_index(df.columns[0]) for df in list_df], axis=1).reset_index().sort_values('index', key = lambda col: col.str.lower())
merge_df.reset_index(inplace = True, drop = True)
#merge_df = merge_df.rename(columns={'': 'Values'})

out_df = merge_df.copy()
out_df.set_index(("index",""), drop = True, inplace = True)

#### cartesian products for correct ordering

values = ["Values"]
outcomes = out_df.iloc[:, out_df.columns.get_level_values(1)=='Values'].columns.get_level_values(0).tolist()
cols_out = list(itertools.product(outcomes, values))

elses = ["Certainty","Effect"]
cols_else = list(itertools.product(outcomes, elses))
cols_order = cols_out + cols_else

out_df = out_df[cols_order]

writer = pd.ExcelWriter(name_excel, engine='xlsxwriter')

styled_df = out_df.copy().style
styled_df.set_properties(**{'text-align': 'center'})#, subset = pd.IndexSlice[:, list(set(out_df.columns.get_level_values(0).tolist()))])

for output in list(set(styled_df.columns.get_level_values(0))):

    styled_df = styled_df.apply(bg_color, subset = output, axis = 1)

styled_df.to_excel(writer, sheet_name = 'Summary', columns = cols_out, header = True)

workbook = writer.book
worksheet = writer.sheets['Summary']  # pull worksheet 

worksheet.set_zoom(80)

format_left = workbook.add_format({'align': 'left', 'valign': 'top', 'bold' : False})
format_center = workbook.add_format({'align': 'center', 'valign': 'top'})

worksheet.set_column(0, 0, 35, format_left)
worksheet.set_column(1, len(list_df), 25, format_center)

writer.save()