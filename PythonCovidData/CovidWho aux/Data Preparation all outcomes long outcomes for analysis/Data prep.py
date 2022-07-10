#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Fri Dec 8 08:28:58 2022

@author: antonio
"""

import pandas as pd            #for creating the spreadsheet
import numpy as np             #for nan
import re as re                #for sub
from scipy.stats import norm, t
#import math 

import sys
sys.path.append("../..")

from Functions import cleandf, clean_treatments_names, check_spelling_manually_lol, id_order, find_int_in_string, order_treatments_on_2_columns

def data_prep_subdf(df, sheet):
    dfr = df.copy()
    if sheet == "Dichotomous outcomes" or sheet == "Dichtomous outcomes":

        dfr.drop("Comments", axis = 1, level = 0, inplace = True)
        dfr.drop("Follow-up time (days)", axis = 1, level = 1, inplace = True)
        
    if sheet == "Continuous outcomes":    
        
        dfr.drop("Comments", axis = 1, level = 0, inplace = True)
        dfr.drop("Follow-up time (days)", axis = 1, level = 1, inplace = True)
        dfr.drop("Time to symptom resolution or time to clinical improvement criteria", axis = 1, level = 1, inplace = True)
        
    return dfr

def find_float_in_string(df, start_column = 0, end_column = 1):
    
    dfr = df.copy()
    
    for i in range(0, len(df.index)):
        
        for j in range(start_column, end_column + 1):
            
            cell = dfr.iloc[i, j]
            
            if type(cell) == str:
                
                if any(c.isdigit() for c in cell):
                
                    aux_list = re.findall(r"[-+]?\d*\.\d+|\d+", df.iloc[i, j])
                    aux_list = [float(x) for x in aux_list]
                    
                    if len(tuple(aux_list)) > 1:
                        
                        dfr.iat[i, j] = tuple(aux_list)
                        
                    else:
                    
                        dfr.iloc[i,j] = aux_list[0]
                    
                else:
                    
                    dfr.iloc[i, j] = np.nan
                
    return dfr

def get_outcome(df, dichotomous_or_continuous, n_outcome):
    
    dfr = df.copy()
    
    dfr = dfr[dfr[dichotomous_or_continuous] == n_outcome]
    dfr = dfr.drop(["Outcome"], axis = 1)
    
    return dfr

def pop_variability_columns(df):
    
    dfr = df.copy()
    dfr["Variability Interval"] = [(0, 0)]*len(dfr)
    dfr["Variability Interval 1"] = 0
    dfr["Variability Interval 2"] = 0
    dfr["Variability Value"] = 0
    
    dfr.loc[dfr["Variability"].apply(type) == tuple, "Variability Interval"] = dfr["Variability"]
    dfr.loc[dfr["Variability"].apply(type) != tuple, "Variability Value"] = dfr["Variability"]
       
    dfr["Variability Interval 1"] = dfr["Variability Interval"].map(lambda x: x[0])
    dfr["Variability Interval 2"] = dfr["Variability Interval"].map(lambda x: x[1])
    
    dfr.drop("Variability Interval", axis = 1, inplace = True)
    dfr = dfr.astype({'Variability Value': 'float64'})
    
    return dfr

def estimate_mean_sd(df, better_CI_estimate = True):
    
    dfr = df.copy()
    
    dfr["Mean"] = np.nan
    dfr["Standard Deviation"] = np.nan
    
    #dfr = dfr.astype({'N analyzed': 'float64'})
    dfr.loc[dfr["Measure of variability"] == np.nan, "Measure of variability"] = 10
    
    dfr.loc[dfr["Measure of central tendency"] == 1, "Mean"] = dfr["Central tendency"]
    
    dfr["Mean"] = np.where(((dfr["Measure of central tendency"].astype(int) == 2) & (dfr["Measure of variability"].astype(int) == 1)), dfr["Central tendency"], dfr["Mean"])
    
    dfr["Mean"] = np.where(((dfr["Measure of central tendency"].astype(int) == 2) & (dfr["Measure of variability"].astype(int) == 2)), dfr["Central tendency"], dfr["Mean"])
    
    dfr["Mean"] = np.where(((dfr["Measure of central tendency"].astype(int) == 2) & (dfr["Measure of variability"].astype(int) == 3)), dfr["Central tendency"], dfr["Mean"])
    
    dfr["Mean"] = np.where(((dfr["Measure of central tendency"].astype(int) == 2) & (dfr["Measure of variability"].astype(int) == 4) &
                            ((dfr["Variability Interval 2"] == 0) & (dfr["Variability Interval 2"] == 0))), dfr["Central tendency"], dfr["Mean"])
    #get_outcome
    dfr["Mean"] = np.where(((dfr["Measure of central tendency"].astype(int) == 2) & (dfr["Measure of variability"].astype(int) == 5) &
                            ((dfr["Variability Interval 2"] == 0) & (dfr["Variability Interval 2"] == 0))), dfr["Central tendency"], dfr["Mean"])
    
    dfr["Mean"] = np.where(((dfr["Measure of central tendency"].astype(int) == 2) & (dfr["Measure of variability"].astype(int) == 4) &
                            ((dfr["Variability Interval 2"] != 0) | (dfr["Variability Interval 2"] != 0))), (dfr["Central tendency"] + \
                                                                                                             dfr["Variability Interval 2"] + \
                                                                                                             dfr["Variability Interval 1"])/3, \
                                                                                                             dfr["Mean"])
        
    dfr["Mean"] = np.where(((dfr["Measure of central tendency"].astype(int) == 2) & (dfr["Measure of variability"].astype(int) == 5) &
                            ((dfr["Variability Interval 2"] != 0) | (dfr["Variability Interval 2"] != 0))), (2*dfr["Central tendency"] + \
                                                                                                             dfr["Variability Interval 2"] + \
                                                                                                             dfr["Variability Interval 1"])/4, \
                                                                                                             dfr["Mean"])
    
    dfr["Mean"] = np.where(((dfr["Measure of central tendency"].astype(int) == 2) & (dfr["Measure of variability"].astype(int) == 10)), dfr["Central tendency"], dfr["Mean"])    
    
    dfr.loc[dfr["Measure of variability"] == 1, "Standard Deviation"] = dfr["Variability Value"]
    
    dfr.loc[dfr["Measure of variability"] == 2, "Standard Deviation"] = (dfr["Variability Value"] * \
                                                                         np.sqrt(dfr["N analyzed"]))
    if better_CI_estimate:
        
        dfr.loc[dfr["Measure of variability"] == 3, "Standard Deviation"] = np.abs(dfr["Variability Value"] + \
                                                                             dfr["Variability Interval 2"] - \
                                                                             dfr["Variability Interval 1"]) * \
                                                                             np.sqrt(dfr["N analyzed"]) /\
                                                                             (2*t.ppf(0.975, dfr["N analyzed"] - 1))
                                                                          
    else:
        
        dfr.loc[dfr["Measure of variability"] == 3, "Standard Deviation"] = np.abs(dfr["Variability Value"] + \
                                                                             dfr["Variability Interval 2"] - \
                                                                             dfr["Variability Interval 1"]) * \
                                                                             np.sqrt(dfr["N analyzed"]) /\
                                                                             (3.92)
                                                                                                         
    dfr.loc[dfr["Measure of variability"] == 4, "Standard Deviation"] = np.abs(dfr["Variability Value"] + \
                                                                         dfr["Variability Interval 2"] - \
                                                                         dfr["Variability Interval 1"])/(2* \
                                                                         norm.ppf((0.75*dfr["N analyzed"] - 0.125)/\
                                                                                  (dfr["N analyzed"] + 0.25)))
                                                                                                         
    dfr.loc[dfr["Measure of variability"] == 5, "Standard Deviation"] = np.abs(dfr["Variability Value"] + \
                                                                         dfr["Variability Interval 2"] - \
                                                                         dfr["Variability Interval 1"])/(2* \
                                                                         norm.ppf((dfr["N analyzed"] - 0.375)/\
                                                                                  (dfr["N analyzed"] + 0.25)))
    return dfr

def global_mean(df, sample_size_column = "N_analyzed_x", means_column = "Mean"):
    
    dfr = df.copy()
    
    total_size = dfr[sample_size_column].sum()
    
    mean = (dfr[sample_size_column] * dfr[means_column]).sum()
    
    if total_size != 0:
        mean = mean/total_size
    
    return mean

def global_sd(df, sample_size_column = "N_analyzed_x", means_column = "Mean_x", sd_column = "Standard_Deviation"):
    
    sd = df.copy()
    
    mean = global_mean(sd, sample_size_column = sample_size_column, means_column = means_column)
    
    sd["Weighted SD"] = (sd[sample_size_column] - 1)*(sd[sd_column]**2)
    sd_value = sd["Weighted SD"].sum()
    
    sd["2nd term"] = (sd[sample_size_column]*(sd[means_column]**2))
    second_term = sd["2nd term"].sum()
                      
    third_term = (-1)*(mean**2)*sd[sample_size_column].sum()
    
    N_value = sd[sample_size_column].sum()
    
    if N_value > len(sd) + 0.0000001:
        
        N_value = N_value - len(sd)
    
    if N_value != 0:
        global_sd = np.sqrt((sd_value + second_term + third_term)/N_value)
        
    else:
        global_sd = np.sqrt((sd_value + second_term + third_term))
    
    return global_sd

def sd_imputation(df, sample_size_column = "N_analyzed", means_column = "Mean", sd_column = "Standard_Deviation"):
    
    dfr = df.copy()
    
    sdc = dfr[(dfr["Intervention"] == "standard care/placebo") & (dfr["Standard_Deviation"].notna())]
    
    sd_value = global_sd(sdc, sample_size_column = sample_size_column, means_column = means_column, sd_column = sd_column)
    
    dfr.loc[dfr["Standard_Deviation"].isna(), "Standard_Deviation"] = sd_value
    
    return dfr

#####name of inputs
Name_File_Data = "COVID19 NMA Data extraction form - Drug Therapy - Subgroups - remdesivir - severe vs critical(2).xlsx"
nodes_name = "Table of nodes (31-03-2022).xlsx"
Dichotomous = "Dichotomous outcomes"
Continuous = "Continuous outcomes"

filter_small =  False
filter_dich_total = 100
filter_dich_event = 20
filter_cont_total = 100


if nodes_name == 0:
    nodes_file = 0
    
else:
    nodes_file = pd.read_excel(nodes_name)
    nodes_file = nodes_file[["Intervention name", "Node"]]

DichPrim = pd.read_excel(Name_File_Data, header = None, sheet_name = Dichotomous)
ContPrim = pd.read_excel(Name_File_Data, header = None, sheet_name = Continuous)

dich_list = DichPrim.iloc[2,3]

Dich =cleandf(DichPrim, total_nan = True)
Cont =cleandf(ContPrim, total_nan = True)

Dich.dropna(axis=0, how='all', inplace=True)
Cont.dropna(axis=0, how='all', inplace=True)

Dich.reset_index(inplace = True, drop = True)
Cont.reset_index(inplace = True, drop = True)

# Dich =id_order(Dich)
# Cont =id_order(Cont)
Dich["Ref ID"] = Dich["Ref ID"].astype(str)
Cont["Ref ID"] = Cont["Ref ID"].astype(str)

Dich =data_prep_subdf(Dich, Dichotomous)
Cont =data_prep_subdf(Cont, Continuous)

Dich=clean_treatments_names(Dich, sheet = Dichotomous, directory_file = nodes_file, node_mask_inplace = False)
Cont =clean_treatments_names(Cont, sheet = Continuous, directory_file = nodes_file, node_mask_inplace = False)

Dich =find_int_in_string(Dich, start_column = 4, end_column = 6)
Cont =find_int_in_string(Cont, start_column = 4, end_column = 6)
# Cont =find_int_in_string(Cont, start_column = 7, end_column = 9)
# Cont =find_int_in_string(Cont, start_column = 7, end_column = 7)

# Dich.drop("Definition of infection", axis = 1, level = 1, inplace = True)

Cont =find_float_in_string(Cont, start_column = 3, end_column = 8)
# Cont =find_float_in_string(Cont, start_column = 9, end_column = 9)

Dich.columns = Dich.columns.get_level_values(1)
Cont.columns = Cont.columns.get_level_values(1)  

############# Subgroups from trial characteristics sheet
# TrialPrim = pd.read_excel(Name_File_Data, header = None, sheet_name = "Trial characteristics")
# Trial=cleandf(TrialPrim, total_nan = True)
# Trial.dropna(axis=0, how='all', inplace=True)
# Trial.reset_index(inplace = True, drop = True)
# Trial["Ref ID"] = Trial["Ref ID"].astype(str)

# Trial.columns = Trial.columns.get_level_values(1)
# Trial = Trial[["Ref ID", "1st Author", "Pre-exposure (%)", "Post-exposure (%)"]]
# Trial =find_float_in_string(Trial, start_column = 2, end_column = 3)

# Dich = pd.merge(Dich, Trial, how = "left", left_on = ["Ref ID", "1st Author"], right_on = ["Ref ID", "1st Author"])
# Cont = pd.merge(Cont, Trial, how = "left", left_on = ["Ref ID", "1st Author"], right_on = ["Ref ID", "1st Author"])

# Dich = Dich[(Dich["Post-exposure (%)"] == 100)]
# Cont = Cont[(Cont["Post-exposure (%)"] == 100)]

# Dich.drop(["Pre-exposure (%)", "Post-exposure (%)"], axis = 1, inplace = True)
# Cont.drop(["Pre-exposure (%)", "Post-exposure (%)"], axis = 1, inplace = True)

############# Subgroup section. Do a function dummy
Groups_Dich = pd.read_excel(Name_File_Data, sheet_name = "Subgroups_Dichotomous outcomes")
Groups_Cont = pd.read_excel(Name_File_Data, sheet_name = "Subgroups_Continuous outcomes")

Groups_Dich.rename(columns = {"RefID" : "Ref ID"}, inplace = True)
Groups_Cont.rename(columns = {"RefID" : "Ref ID"}, inplace = True)

Groups_Dich["Ref ID"] = Groups_Dich["Ref ID"].astype(str)
Groups_Cont["Ref ID"] = Groups_Cont["Ref ID"].astype(str)

Dich = pd.merge(Dich, Groups_Dich, how = "left", left_on = ["Ref ID", "1st Author", "Subgroups"], right_on = ["Ref ID", "First author", "Subgroup description"])
Cont = pd.merge(Cont, Groups_Cont, how = "left", left_on = ["Ref ID", "1st Author", "Subgroups"], right_on = ["Ref ID", "First author", "Subgroup description"])

Dich.drop(["Subgroups", "Subgroup description", "First author"], axis = 1, inplace = True)
Cont.drop(["Subgroups", "Subgroup description", "First author"], axis = 1, inplace = True)

Dich = Dich[(Dich["Group for analysis"] == "severe")]
Cont = Cont[(Cont["Group for analysis"] == "severe")]
# | (Dich["Group for analysis"] == "critical")   (Dich["Group for analysis"] == "severe")  | (Dich["Group for analysis"] == "critical") | (Dich["Group for analysis"] == "severe/critical")
# | (Cont["Group for analysis"] == "critical")   (Cont["Group for analysis"] == "severe")  | (Cont["Group for analysis"] == "critical") | (Cont["Group for analysis"] == "severe/critical")
Dich.drop("Group for analysis", axis = 1, inplace = True)
Cont.drop("Group for analysis", axis = 1, inplace = True)

####Dich
# Dich_Outcome_dict = ["Mortality", \
#                       "Infection with COVID-19 (laboratory confirmed)", \
#                       "Infection with COVID-19 (laboratory confirmed and suspected)", \
#                       "Admission to hospital", \
#                       "Adverse effects leading to discontinuation"]

Dich_Outcome_dict = ["Mortality", \
                      "Mechanical ventilation", \
                      "Admission to hospital", \
                      "Adverse effects leading to discontinuation", \
                      "Viral clearance", \
                      "Venous thromboembolism", \
                      "Clinically important bleeding"]

Dich["Number of events"] = pd.to_numeric(Dich["Number of events"])

Dichlong = Dich.groupby(["Ref ID", "1st Author", "Intervention 1 name node", "Outcome"], as_index = False)\
    [["N analyzed", "Number of events", "Intervention name"]].agg(lambda x: x.sum())

if filter_small:
    Dichlong = Dichlong.\
        groupby(["Intervention 1 name node", "Outcome"], as_index = False).filter(lambda x: (sum(x["N analyzed"]) >= filter_dich_total) | \
                                                                                  (sum(x["Number of events"]) >= filter_dich_event))

Dichlong = Dichlong.groupby(["Ref ID", "1st Author", "Outcome"]).filter(lambda d: len(d) > 1)

Dichlong.rename(columns = {"Ref ID" : "refid", \
                            "1st Author" : "stauthor", \
                            "Intervention 1 name node" : "interventionname", \
                            "Intervention name" : "treatment", \
                            "N analyzed" : "sampleSize", \
                            "Number of events" : "responder"}, inplace = True)

Dichlong = Dichlong[Dichlong.columns[[0,1,2,6,3,4,5]]]

Dichlong = Dichlong[Dichlong['sampleSize'].notna()]
Dichlong = Dichlong[Dichlong['responder'].notna()]

Dichlong = Dichlong.sort_values(by = ['stauthor', 'interventionname'])

#####
Dichwide = pd.merge(Dichlong, Dichlong, on = ["refid", "stauthor", "Outcome"])
Dichwide.drop(Dichwide[Dichwide['interventionname_x'] == Dichwide['interventionname_y']].index, inplace = True)
Dichwide = Dichwide.drop(["treatment_x", "treatment_y"], axis = 1)
Dichwide = order_treatments_on_2_columns(Dichwide, treatment1 = 'interventionname_x', treatment2 = 'interventionname_y', associated_vars1 = ["sampleSize_x", "responder_x"], associated_vars2 = ["sampleSize_y", "responder_y"], Total_N = False)
Dichwide.drop_duplicates(inplace=True, ignore_index=False)

Dichwide.rename(columns = {"stauthor" : "study", \
                           "interventionname_x" : "t1", \
                           "interventionname_y" : "t2", \
                           "sampleSize_x" : "e.total", \
                           "sampleSize_y" : "c.total", \
                           "responder_x" : "e.events", \
                           "responder_y" : "c.events"}, inplace = True)
    
Dichwide = Dichwide.drop(["refid"], axis = 1)
Dichwide = Dichwide[Dichwide.columns[[0,1,5,4,3,7,6,2]]]
    
for outcome in Dich_Outcome_dict:
    
    index = Dich_Outcome_dict.index(outcome) + 1
    
    outcome_long_df = get_outcome(Dichlong, "Outcome", index)
    outcome_wide_df = get_outcome(Dichwide, "Outcome", index)
    
    if len(outcome_long_df) > 0:
        
        outcome_long_df.to_csv(outcome + " - long data format.csv", index = False, )
        outcome_wide_df.to_csv(outcome + " - wide data format.csv", index = False, )
    
###Cont group

if len(Cont.index) > 0:
    to_numeric_columns = ["Outcome", "Central tendency", "Measure of variability"]
    for column in to_numeric_columns:
        
        Cont[column] = pd.to_numeric(Cont[column])
        
    Contlong = Cont[Cont["N analyzed"].notna()]
    Contlong = Contlong[Contlong["Measure of central tendency"].notna()]
    Contlong = Contlong[Contlong["Central tendency"].notna()]
    
    Contlong.loc[Contlong["Measure of variability"] > 5, "Measure of variability"] = np.nan
    
    Contlong = pop_variability_columns(Contlong)
    Contlong.loc[Contlong["Measure of variability"].isna(), "Measure of variability"] = 10
    
    Contlong = estimate_mean_sd(Contlong, better_CI_estimate = False)
    
    Contlong = Contlong[Contlong["N analyzed"] > 0]
    
    non_space_names = {"Ref ID" : "Ref_ID", \
                       "1st Author" : "1st_Author", \
                       "Intervention 1 name node" : "Intervention", \
                       "N analyzed" : "N_analyzed", \
                       "Standard Deviation" : "Standard_Deviation"}
    
    Contlong.rename(columns = non_space_names, inplace=True)
    
    #####Comparison between sd estimations from 95% CI
    # Contdev = estimate_mean_sd(Contlong)
    # Contdev2 = estimate_mean_sd(Contlong, better_CI_estimate = False)
    
    # Contdev = Contdev[Contdev["Measure of variability"] == 3]
    # Contdev2 = Contdev2[Contdev2["Measure of variability"] == 3]
    
    # Contdev = Contdev[["1st Author", "N analyzed", "Standard Deviation"]]
    # Contdev2 = Contdev2[["1st Author", "N analyzed", "Standard Deviation"]]
    
    # Comparison = pd.merge(Contdev2, Contdev, left_index=True, right_index=True)
    # Comparison.drop("1st Author_y", axis = 1, inplace = True)
    # Comparison.drop("N analyzed_y", axis = 1, inplace = True)
    
    # Comparison["Relative difference"] = 100*(Comparison["Standard Deviation_x"] - Comparison["Standard Deviation_y"]) / Comparison["Standard Deviation_x"]
    
    # Comparison.plot(x = "N analyzed_x", y = "Relative difference", style = '.', ylabel = "Relative dfference (%)")
    
    Contlong = sd_imputation(Contlong)
    
    select_continuous_columns = ["Ref_ID", "1st_Author", "Intervention", "Outcome", "N_analyzed", "Mean", "Standard_Deviation"]
    
    Contlong = Contlong[select_continuous_columns]
    
    Contgroup_N = Contlong.groupby(["Ref_ID", "1st_Author", "Intervention", "Outcome"], as_index = False).agg({"N_analyzed" : 'sum'})
    Contlong_N = pd.merge(Contlong, Contgroup_N, how = "left", on = ["Ref_ID", "1st_Author", "Intervention", "Outcome"])
    #Contlong_N.drop("N_analyzed_x", axis = 1, inplace = True)
    Contlong_N.rename(columns={'N_analyzed_y': 'N_analyzed'}, inplace=True)
    
    Contgroup_mean = Contlong_N.groupby(["Ref_ID", "1st_Author", "Intervention", "Outcome", "N_analyzed"], as_index = False).apply(global_mean)
    Contgroup_mean.columns.values[5] = "Mean"
    Contlong_mean = pd.merge(Contlong_N, Contgroup_mean, how = "left", on = ["Ref_ID", "1st_Author", "Intervention", "Outcome", "N_analyzed"])
    Contlong_mean.rename(columns={"Mean_y": "Mean"}, inplace=True)
    
    Contgroup = Contlong_mean.groupby(["Ref_ID", "1st_Author", "Intervention", "Outcome", "N_analyzed", "Mean"], as_index = False).apply(global_sd)
    Contgroup.columns.values[6] = "Standard_Deviation"
    
    if filter_small:
        Contgroup = Contgroup.groupby(["Intervention", "Outcome"], as_index = False).filter(lambda x: sum(x["N_analyzed"]) >= filter_cont_total)
    
    Contgroup = Contgroup.groupby(["Ref_ID", "1st_Author", "Outcome"]).filter(lambda d: len(d) > 1)
    
    Contgroup.rename(columns = {"Ref_ID" : "RefID", \
                                "1st_Author" : "study", \
                                "Intervention" : "treatment", \
                                "Mean" : "mean", \
                                "Standard_Deviation" : "std.dev", \
                                "N_analyzed" : "sampleSize"}, inplace = True)
        
    Contgroup = Contgroup[Contgroup.columns[[0,1,2,3,5,6,4]]]
    
    Contgroup = Contgroup.sort_values(by = ['study', 'treatment'])
    
    #####
    Contwide = pd.merge(Contgroup, Contgroup, on = ["RefID", "study", "Outcome"])
    Contwide.drop(Contwide[Contwide['treatment_x'] == Contwide['treatment_y']].index, inplace = True)
    Contwide = order_treatments_on_2_columns(Contwide, treatment1 = 'treatment_x', treatment2 = 'treatment_y', associated_vars1 = ["mean_x", "std.dev_x", "sampleSize_x"], associated_vars2 = ["mean_y", "std.dev_y", "sampleSize_y"], Total_N = False)
    Contwide.drop_duplicates(inplace=True, ignore_index=False)
    
    Contwide.rename(columns = {"stauthor" : "study", \
                               "treatment_x" : "t1", \
                               "treatment_y" : "t2", \
                               "mean_x" : "mean1", \
                               "mean_y" : "mean2", \
                               "std.dev_x" : "sd1", \
                               "std.dev_y" : "sd2", \
                               "sampleSize_x" : "n1", \
                               "sampleSize_y" : "n2"}, inplace = True)
        
    Contwide = Contwide.drop(["RefID"], axis = 1)
    Contwide = Contwide[Contwide.columns[[0,1,6,3,4,5,7,8,9,2]]]
    
    Cont_Outcome_dict = ["Time to symptom resolution"]
        
    # Cont_Outcome_dict = ["Duration of hospitalization", \
    #                       "ICU length of stay", \
    #                       "Ventilator-free days", \
    #                       "Duration of ventilation", \
    #                       "Time to symptom resolution", \
    #                       "Time to viral clearance"]
    
    for outcome in Cont_Outcome_dict:
        
        index = Cont_Outcome_dict.index(outcome) + 1
        
        outcome_long_df = get_outcome(Contgroup, "Outcome", index)
        outcome_wide_df = get_outcome(Contwide, "Outcome", index)
        
        if len(outcome_long_df) > 0:
            outcome_long_df.to_csv(outcome + " - long data format.csv", index = False, )
            outcome_wide_df.to_csv(outcome + " - wide data format.csv", index = False, )

