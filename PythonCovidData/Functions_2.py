#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Wed Feb 24 14:55:27 2021

@author: antonio
"""

import pandas as pd            #for creating the spreadsheet
import numpy as np             #for nan
import re as re                #for sub


""""Funciones compiladas de draft1_trial_characteristics y draft2_trial_characteristics"""

#cleans the study names column, weeding out the explanation and getting only the name of the study

def clean_trial_name(df):
    
    dfr = df.copy()
    
    for i in range(0, len(dfr.index)):
        
        aux_str = dfr.loc[i, ("Trial name", "Trial name")]
        
        if type(aux_str) == str:
        
            if "(" in aux_str and ")" in aux_str:           #if there are parentesis in the name gets what is inside the parentesis and assings it as the name
                
                aux_str = re.search('\(([^)]+)', aux_str).group(1)
            
        dfr.loc[i, ("Trial name", "Trial name")] = aux_str
        
    return dfr

#concatenates in a new column the 1st author, the year and the trial name if any
def study_column(df):
    
    dfr = df.copy()
    dfr[("Trial name", "Trial name")].replace("NR", np.nan, inplace = True)
    dfr[("Trial name", "Trial name")].fillna(value = "", inplace = True)
    
    if ("Year", "Year") in dfr.columns:
        
        dfr[("Study", "Study")] = dfr[("1st Author", "1st Author")].astype(str) + ", " + dfr[("Year", "Year")].astype(str) \
        + "\n" + dfr[("Trial name", "Trial name")].astype(str)


    if ("Year", "Year") not in dfr.columns:
            
        dfr[("Study", "Study")] = dfr[("1st Author", "1st Author")].astype(str) + ", " + "2020" \
        + "\n" + dfr[("Trial name", "Trial name")].astype(str)

    return dfr

#We transform the numerical code in the publication original column to an explicit status display
    
def publication_status_registration_column(df):
    
    dfr = df.copy()
    
    for i in range(0, len(dfr.index)):
        
        aux_str = dfr.loc[i, ("Publication/Study characteristics", "Publication status")]
        try:
            aux_str = int(aux_str)
            
        except:
            aux_str = str(aux_str).replace(",", "")
            aux_lst = [int(s) for s in aux_str.split() if s.isdigit()]
            if aux_lst == []:
                aux_str == str
            
            else:
                aux_str = min(aux_lst)
        
        if aux_str == 1:
            
            aux_str = "Published"
            
        if aux_str == 2:
            
            aux_str = "Pre-print"
            
        if aux_str == 3:
            
            aux_str = "Trial registration"
            
        if aux_str == 4:
            
            aux_str = "Data from authors"
            
        if aux_str == 5:
            
            aux_str = "Data from meta-analysis"
            
        dfr.loc[i, ("Publication/Study characteristics", "Publication status")] = aux_str
    
    dfr[("Trial registration", "Trial registration")].replace("NR", np.nan, inplace = True)
    dfr[("Trial registration", "Trial registration")].fillna(value = "", inplace = True)
    
    dfr[("Publication status\nRegistration", "Publication status\nRegistration")] = \
    dfr[("Publication/Study characteristics", "Publication status")] + "\n" + \
    dfr[("Trial registration", "Trial registration")]
    
    return dfr

#We just sum the total pacients in the different types of treatments using whole columns operations

def number_of_participants_column(df, n_trial = 7):
    
    dfr = df.copy()
    #n_trial = n_trial
    for n in range(1, 20):
            #print(n)
        try:
            dfr["Intervention {}".format(n)]
        except KeyError:
            break
    
    dfr[("Number of\nparticipants", "Number of\nparticipants")] = int(0)
    
    for i in range(1, n):
        
        dfr[(f"Intervention {i}", "N randomized")].replace("NR", - np.inf, inplace = True)
        dfr[(f"Intervention {i}", "N randomized")].replace("NA", - np.inf, inplace = True)
        dfr[(f"Intervention {i}", "N randomized")].fillna(value = int(0), inplace = True)
        
        dfr[("Number of\nparticipants", "Number of\nparticipants")] = \
        dfr[("Number of\nparticipants", "Number of\nparticipants")] + \
        dfr[(f"Intervention {i}", "N randomized")]
        
    dfr[("Number of\nparticipants", "Number of\nparticipants")].replace(- np.inf, "NR", inplace = True)
        
    return dfr

#This function cuts the string of set of countries into chunks with line break for easier readibility.
#grouping in 2 countries per line

def country_column(df):
    
    dfr = df.copy()
    
    #line_lenght = 26
    
    dfr[("Country", "Country")] = dfr[("Baseline patient characteristics", "Country")]
    dfr[("Country", "Country")].replace("NR", np.nan, inplace = True)
    dfr[("Country", "Country")].fillna(value = "", inplace = True)
    
    for j in range(0, len(dfr.index)):                          #Separa la linea con solo 2 paises por linea
        
        aux_str = dfr.loc[j, ("Country", "Country")]
        aux_lst = aux_str.split(", ")
        
        if len(aux_lst) > 1:                                    #if the amount of countries is 2 or more, it cuts it into lines of 2 countries
        
            for element in range(0, len(aux_lst)):
            
                if element % 2 == 0 and element != len(aux_lst) - 1:
                
                    aux_lst[element] = aux_lst[element] + ", "
                
                if element % 2 != 0 and element != len(aux_lst) - 1:
                
                    aux_lst[element] = aux_lst[element] + ",\n"
                    
            dfr.loc[j, ("Country", "Country")] = "".join(aux_lst)
                    
        else:
            
            dfr.loc[j, ("Country", "Country")] = aux_str
            
    return dfr

#If the thing is a number, make the format to 1 decimal. This works over series
def one_decimal(srs):
    
    srsr = srs.copy()
    
    for j in range(0, len(srsr.index)):
        
         cell = srsr.loc[j]
         
         if type(cell) == float or type(cell) == int:
             
             srsr.loc[j] = float("{:.1f}".format(cell))
             #print(srs.loc[j], srsr.loc[j])
    return srsr

#copy and renaming of mean age column

def mean_age_column(df):
    
    dfr = df.copy()
    
    dfr[("Mean\nage", "Mean\nage")] = dfr[("Baseline patient characteristics", "Age")]
    #dfr[("Mean\nage", "Mean\nage")] = one_decimal(dfr[("Mean\nage", "Mean\nage")])
    dfr[("Mean\nage", "Mean\nage")].fillna(value = "NR", inplace = True)
    dfr[("Mean\nage", "Mean\nage")] = one_decimal(dfr[("Mean\nage", "Mean\nage")])
    
    return dfr

#copy and renaming of male percentage column

def percentage_male_column(df):
    
    dfr = df.copy()
    
    dfr[("% Male", "% Male")] = dfr[("Baseline patient characteristics", "Male (%)")]
    #dfr[("% Male", "% Male")] = one_decimal(dfr[("% Male", "% Male")])
    dfr[("% Male", "% Male")].fillna(value = "NR", inplace = True)
    dfr[("% Male", "% Male")] = one_decimal(dfr[("% Male", "% Male")])
    
    return dfr

#Auxiliary function to reformat all the string elements of a list, from "percentage (condition)"
#to condition (percentage), and appending

def clean_comorbidities_sublist(cell, column_name):
    
    if type(cell) == str:
            
        lst = cell.split("), ")
            
    else:
            
        if pd.isna(cell) == False:
                
            lst = [float("{:.1f}".format(cell))]
                
        else:
                
            lst = []
    
    for i in range(0, len(lst)):

        content = lst[i]
        if type(content) == float or type(content) == int:
                    
            percentage = float("{:.1f}".format(content))
            lst[i] = column_name + " (" + str(percentage) + "%)"
        
        elif type(content) == str and content != "NR":
                
            if " (" not in content:

                num_character = content.find("(")
                lst[i] = content[:num_character] + " " + content[num_character:]
            condition = str(lst[i].split("(")[1])
            percentage = float(lst[i].split("(")[0])
                
            percentage = "{:.1f}".format(percentage)
            
            if ")" in condition:
                
                condition = condition.replace(")", "")
            
            condition = condition.capitalize()

            if column_name == "Diabetes":
                
                if "1" in condition or " I " in condition or " i " in condition:
                    
                    condition = "T1DM"
                    
                elif "2" in condition or " II " in condition or " ii " in condition:
                    
                    condition = "T2DM"
            
            lst[i] = condition + " (" + percentage + "%)"
            
    return lst

#leaves the column mostly the same
    
def ventilation_percentages_column(df):
    
    dfr = df.copy()
    Bcc = "Baseline clinical characteristics"

    if "Revised oxygen therapy (%)" in dfr.columns.get_level_values(1):
        
        Ot = "Revised oxygen therapy (%)"
        
    else:
        
        Ot = "Oxygen therapy (%)"
    
    dfr[("Detailed ventilation (%)", "Detailed ventilation (%)")] = "NR"
    
    for j in range(0, len(dfr.index)):
        
        list_to_join = []
        
        #we use the function we were using for the comorbidities column, it works for this part too!!
        aux_lst1 = clean_comorbidities_sublist(dfr.loc[j, (Bcc, Ot)], "Unspecified ventilation") 
        
        list_to_join.extend(aux_lst1)
        
        dfr.loc[j, ("Detailed ventilation (%)", "Detailed ventilation (%)")] = "\n".join(list(map(str, list_to_join)))
        
        if dfr.loc[j, ("Detailed ventilation (%)", "Detailed ventilation (%)")] == "":
            
            dfr.loc[j, ("Detailed ventilation (%)", "Detailed ventilation (%)")] = "NR"
        
    return dfr

#This function combines all the data from the comorbidities columns, respiratory conditions,
#cardiovascular, diabetes and hypertension in the correct format, which is concatenated string 
#cut into line breaks
def comorbidities_column(df):
    
    dfr = df.copy()
    Bpc = "Baseline patient characteristics"
    Rc = "Respiratory condition (%)"
    Cd = "Cardiovascular disease or coronary heart disease (%)"
    
    dfr[(Bpc, Rc)].replace("NR", np.nan, inplace = True)
    dfr[(Bpc, Cd)].replace("NR", np.nan, inplace = True)
    dfr[(Bpc, "Diabetes (%)")].replace("NR", np.nan, inplace = True)
    dfr[(Bpc, "Hypertension (%)")].replace("NR", np.nan, inplace = True)
    
    dfr[("Comorbidities", "Comorbidities")] = "NR"
        
    for j in range(0, len(dfr.index)):
        
        list_to_join = []
        #print(type(dfr.loc[j, (Bpc, "Respiratory condition (%)")]))
        
        #we get the correct formats applying the auxiliary function 
        aux_lst1 = clean_comorbidities_sublist(dfr.loc[j, (Bpc, Rc)], "Respiratory condition")
        aux_lst2 = clean_comorbidities_sublist(dfr.loc[j, (Bpc, Cd)], "Cardiovascular disease")
        aux_lst3 = clean_comorbidities_sublist(dfr.loc[j, (Bpc, "Diabetes (%)")], "Diabetes")
        aux_lst4 = clean_comorbidities_sublist(dfr.loc[j, (Bpc, "Hypertension (%)")], "Hypertension")
        
        #print(aux_lst1[i])            
        list_to_join.extend(aux_lst1)
        list_to_join.extend(aux_lst2)
        list_to_join.extend(aux_lst3)
        list_to_join.extend(aux_lst4)

        
        #print(list(map(str, list_to_join)))
        dfr.loc[j, ("Comorbidities", "Comorbidities")] = "\n".join(list(map(str, list_to_join)))
        
        if dfr.loc[j, ("Comorbidities", "Comorbidities")] == "":    #if after it all
            
            dfr.loc[j, ("Comorbidities", "Comorbidities")] = "NR"
        
    return dfr

#combines the columns of inpatient percentage and intensive care

def type_of_care_column(df):
    
    dfr = df.copy()
    Bcc = "Baseline clinical characteristics"
    
    dfr[(Bcc, "Inpatient (%)")].replace("NR", np.nan, inplace = True)
    dfr[(Bcc, "Intensive care (%)")].replace("NR", np.nan, inplace = True)
    
    dfr[("Type of care", "Type of care")] = ""
    
    for j in range(0, len(dfr.index)):
        
        inpatient = float("{:.1f}".format(dfr.loc[j, (Bcc, "Inpatient (%)")]))          #inpatient part, with different cases
        
        if inpatient == 100:
            
            dfr.loc[j, ("Type of care", "Type of care")] = "Inpatient\n"
            
        elif inpatient == 0:
            
            dfr.loc[j, ("Type of care", "Type of care")] = "Outpatient\n"
            
        elif 0 < inpatient < 100:
            
            dfr.loc[j, ("Type of care", "Type of care")] = "Inpatient (" + str(inpatient) + "%)\n"
            
        else:
            
            dfr.loc[j, ("Type of care", "Type of care")] = ""
            
        intensive = float("{:.1f}".format(dfr.loc[j, (Bcc, "Intensive care (%)")]))         #intensive care section
        
        if pd.isna(intensive) == False and type(intensive) != str and intensive != 0:
            
            dfr.loc[j, ("Type of care", "Type of care")] = dfr.loc[j, ("Type of care", "Type of care")] + \
                "Intensive care (" + str(intensive) + "%)"
                
        if dfr.loc[j, ("Type of care", "Type of care")] == "":
            
            dfr.loc[j, ("Type of care", "Type of care")] = "NR"
                
    return dfr

#combines the severity columns into 1 concatenated column

def severity_column(df):
    
    dfr = df.copy()
    Bcc = "Baseline clinical characteristics"
    
    dfr[(Bcc, "Mild illness (%)")].replace("NR", np.nan, inplace = True)
    dfr[(Bcc, "Moderate illness (%)")].replace("NR", np.nan, inplace = True)
    dfr[(Bcc, "Severe illness (%)")].replace("NR", np.nan, inplace = True)
    dfr[(Bcc, "Critical illness (%)")].replace("NR", np.nan, inplace = True)
    
    dfr[("Severity", "Severity")] = ""
    
    for j in range(0, len(dfr.index)):
        
        mild = float("{:.1f}".format(dfr.loc[j, (Bcc, "Mild illness (%)")]))
        moderate = float("{:.1f}".format(dfr.loc[j, (Bcc, "Moderate illness (%)")]))
        severe = float("{:.1f}".format(dfr.loc[j, (Bcc, "Severe illness (%)")] + dfr.loc[j, (Bcc, "Critical illness (%)")]))
        critical = float("{:.1f}".format(dfr.loc[j, (Bcc, "Critical illness (%)")]))
        
        if severe > 100:
            
            severe = float("{:.1f}".format(dfr.loc[j, (Bcc, "Severe illness (%)")]))
        
        if pd.isna(mild) == True and pd.isna(moderate) == True:     #combines mild and moderate and declares nan if both are not reported
            
            mild_moderate = np.nan
            
        else:
            
            mild_moderate = np.nan_to_num(mild, nan = 0.0) + np.nan_to_num(moderate, nan = 0)
            
        if np.isnan(mild_moderate) == False and mild_moderate != 0:                        #the whole concatenation
            
            dfr.loc[j, ("Severity", "Severity")] = dfr.loc[j, ("Severity", "Severity")] + "Mild/Moderate (" \
                + str(mild_moderate) + "%)\n"
                
        if np.isnan(severe) == False and severe != 0:
            
            dfr.loc[j, ("Severity", "Severity")] = dfr.loc[j, ("Severity", "Severity")] + "Severe (" \
                + str(severe) + "%)\n"
        
        if np.isnan(mild_moderate) == False and mild_moderate == 0:
            
            dfr.loc[j, ("Severity", "Severity")] = "Severe (100%)\n"
                
        if np.isnan(critical) == False and critical != 0:
            
            dfr.loc[j, ("Severity", "Severity")] = dfr.loc[j, ("Severity", "Severity")] + "Severe: Critical (" \
                + str(critical) + "%)"
                
        if dfr.loc[j, ("Severity", "Severity")] == "":              #if none are reported, we declare "NR"
            
            dfr.loc[j, ("Severity", "Severity")] = "NR"
            
    return dfr


#sums all the percentages that are associated to high flow or invasive or non invasive mechanical ventilation

def percentage_mechanical_ventilation_column(df):
    
    dfr = df.copy()
    Bcc = "Baseline clinical characteristics"
    
    if "Revised oxygen therapy (%)" in dfr.columns.get_level_values(1):
        
        Ot = "Revised oxygen therapy (%)"
        
    else:
        
        Ot = "Oxygen therapy (%)"
    
    dfr[(Bcc, Ot)].replace("NR", np.nan, inplace = True)
    
    dfr[("% Mechanical \nventilation \n(at baseline)", "% Mechanical \nventilation \n(at baseline)")] = ""
    
    for j in range(0, len(dfr.index)):
        
        list_to_join = []
        #print(type(dfr.loc[j, (Bpc, "Respiratory condition (%)")]))
        if type(dfr.loc[j, (Bcc, Ot)]) == str:              #gets the split list 
            
            aux_lst = dfr.loc[j, (Bcc, Ot)].split("), ")
            
        else:
            
            if pd.isna(dfr.loc[j, (Bcc, Ot)]) == False:     #if the cell contains a non string non nan, put the content as unique element of the list
                
                aux_lst = [dfr.loc[j, (Bcc, Ot)]]
                
            else:
                
                aux_lst = []
                
        for i in range(0, len(aux_lst)):                    #over all the elements of the cutting list, it finds the string elements

            content = aux_lst[i]
            if type(content) == float or type(content) == int:
                    
                amount = content
                list_to_join.append(amount)
        
            elif type(content) == str:
                
                if " (" not in content:                     #the format of the string elements, are a percentage and then a method of ventilation

                    num_character = content.find("(")           #so we cut this now from the "(" character
                    aux_lst[i] = content[:num_character] + " " + content[num_character:]

                condition = str(aux_lst[i].split("(")[1]).lower()
                percentage = float(aux_lst[i].split("(")[0])
            
                if ")" in condition:                            
                
                    condition = condition.replace(")", "")
                    #finally, we filter over keywords of the kinds of ventilation we want to sum over
                if "high-flow" in condition or "high flow" in condition or "non-invasive" in condition or "noninvasive" in condition or \
                    "Iv" in condition or "IV" in condition or "ventilation" in condition or "MV" in condition or "iv" in condition:
                
                    list_to_join.append(percentage)
                    
        if len(list_to_join) > 0:                               #checks if there is anything to report, if not, declares "NR"
            
            total = float("{:.1f}".format(sum(list_to_join)))
            
        else:
            
            total = "NR"
            
        dfr.loc[j, ("% Mechanical \nventilation \n(at baseline)", "% Mechanical \nventilation \n(at baseline)")] = total
        
    return dfr

#Concatenation of the columns "intervention n name" with the names of the treatments, with line breaks

def treatments_column(df, name = "", n_trial = 7):
    
    dfr = df.copy()
    #n_trial = n_trial
    
    for n in range(1, 20):
            #print(n)
        try:
            dfr["Intervention {}".format(n)]
        except KeyError:
            break
    
    col_name = "Treatments (dose and duration)"
    
    date_list = re.findall(r'\d+(?:,\d+)?', name)
    day_nma = int(date_list[-3])
    month_nma = int(date_list[-2])
    year_nma = int(date_list[-1])
    
    dfr[(col_name, col_name)] = ""
    
    if (year_nma < 2021) or (year_nma == 2021 and (month_nma < 4 and day_nma < 11)):
        
        for i in range(1, n):
            
            for j in range(0, len(dfr.index)):
            
                if pd.isna(dfr.loc[j, (f"Intervention {i}", f"Intervention {i} name")]) == False\
                    and dfr.loc[j, (f"Intervention {i}", f"Intervention {i} name")] != "NR":
            
                    dfr.loc[j, (col_name, col_name)] = dfr.loc[j, (col_name, col_name)] + \
                    str(dfr.loc[j, (f"Intervention {i}", f"Intervention {i} name")]) + "\n"
                    
    elif year_nma == 2021 and ((month_nma < 4 and day_nma < 13) and (month_nma > 2 and day_nma > 9)):
        
        for i in range(1, n):
        
            for j in range(0, len(dfr.index)):
            
                if pd.isna(dfr.loc[j, (f"Intervention {i}", f"Intervention {i} description (dose, duration) revised")]) == False\
                    and dfr.loc[j, (f"Intervention {i}", f"Intervention {i} description (dose, duration) revised")] != "NR":
            
                    dfr.loc[j, (col_name, col_name)] = dfr.loc[j, (col_name, col_name)] + \
                    str(dfr.loc[j, (f"Intervention {i}", f"Intervention {i} description (dose, duration) revised")]) + "\n"
                    
    elif year_nma == 2021 and ((month_nma < 4 and day_nma < 17) and (month_nma > 2 and day_nma > 15)):
        
        for i in range(1, n):
        
            for j in range(0, len(dfr.index)):
            
                if pd.isna(dfr.loc[j, (f"Intervention {i}", f"Intervention {i} name")]) == False\
                    and dfr.loc[j, (f"Intervention {i}", f"Intervention {i} name")] != "NR":
            
                    dfr.loc[j, (col_name, col_name)] = dfr.loc[j, (col_name, col_name)] + \
                    str(dfr.loc[j, (f"Intervention {i}", f"Intervention {i} name")]) + "\n"
    else:
        
        for i in range(1, n):
            
            for j in range(0, len(dfr.index)):
            
                if pd.isna(dfr.loc[j, (f"Intervention {i}", f"Intervention {i} name")]) == False:
                    
                    treatments_names = str(dfr.loc[j, (f"Intervention {i}", f"Intervention {i} name")]).split(", ")
                    descriptions = str(dfr.loc[j, (f"Intervention {i}", f"Intervention {i} description (dose, duration)")]).split("; ")
                    
                    while len(descriptions) < len(treatments_names):
                        
                        descriptions.append("")
                    
                    for treat, desc in zip(treatments_names, descriptions):
                        
                        if treat != "NR":
                        
                            if desc == "nan" or desc == "NR":
                            
                                dfr.loc[j, (col_name, col_name)] = dfr.loc[j, (col_name, col_name)] + treat
                            
                            else:
                        
                                dfr.loc[j, (col_name, col_name)] = dfr.loc[j, (col_name, col_name)] + \
                                    treat + " (" + desc + ") "

                    dfr.loc[j, (col_name, col_name)] = dfr.loc[j, (col_name, col_name)] + "\n"
            
    dfr[("Treatments (dose and duration)", "Treatments (dose and duration)")] = \
    dfr[("Treatments (dose and duration)", "Treatments (dose and duration)")].apply(lambda x: x[:-1]) #this only removes the last line break
    
    return dfr

#translates the number into the actual outcome

def precursor_column_outcomes(df, remove_redundant = True):
    
    dfr = df.copy()
    
    new_column = "Outcomes"
    dfr[(new_column, new_column)] = ""
    
    for j in range(0, len(dfr.index)):
        
        dichotomous = dfr.loc[j, ("Dichotomous Outcome", "Dichotomous Outcome")]
        continuous = dfr.loc[j, ("Continuous Outcome", "Continuous Outcome")]
        
        if dichotomous == 1:
            
            dfr.loc[j, (new_column, new_column)] = "Mortality"
            
        elif dichotomous == 2:
            
            dfr.loc[j, (new_column, new_column)] = "Mechanical ventilation"
        
        elif dichotomous == 3:
            
            dfr.loc[j, (new_column, new_column)] = "Admission to hospital"
        
        elif dichotomous == 4:
            
            dfr.loc[j, (new_column, new_column)] = "Adverse effects leading to discontinuation"
            
        elif dichotomous == 5:
            
            dfr.loc[j, (new_column, new_column)] = "Viral clearance"
            
        elif dichotomous == 6:
            
            dfr.loc[j, (new_column, new_column)] = "Venous thromboembolism"
            
        elif dichotomous == 7:
            
            dfr.loc[j, (new_column, new_column)] = "Clinically important bleeding"
            
        if continuous == 1:
            
            dfr.loc[j, (new_column, new_column)] = "Duration of hospitalization"
            
        elif continuous == 2:
            
            dfr.loc[j, (new_column, new_column)] = "ICU length of stay"
            
        elif continuous == 3:
            
            dfr.loc[j, (new_column, new_column)] = "Ventilator-free days"
            
        elif continuous == 4:
            
            dfr.loc[j, (new_column, new_column)] = "Duration of ventilation"
            
        elif continuous == 5:
            
            dfr.loc[j, (new_column, new_column)] = "Time to symptom/clinical improvement"

        elif continuous == 6:
            
            dfr.loc[j, (new_column, new_column)] = "Time to viral clearance"
            
    if remove_redundant == True:                #removes the columns we dont need anymore
        
        dfr.drop(["Dichotomous Outcome", "Continuous Outcome"], level = 0, inplace = True, axis = 1)
            
    return dfr

#same function but for blood files

def precursor_column_outcomes_blood(df, remove_redundant = True):
    
    dfr = df.copy()
    
    new_column = "Outcomes"
    dfr[(new_column, new_column)] = ""
    
    for j in range(0, len(dfr.index)):
        
        dichotomous = dfr.loc[j, ("Dichotomous Outcome", "Dichotomous Outcome")]
        continuous = dfr.loc[j, ("Continuous Outcome", "Continuous Outcome")]
        
        if dichotomous == 1:
            
            dfr.loc[j, (new_column, new_column)] = "Mortality"
            
        elif dichotomous == 2:
            
            dfr.loc[j, (new_column, new_column)] = "Mechanical ventilation"
        
        elif dichotomous == 3:
            
            dfr.loc[j, (new_column, new_column)] = "Admission to hospital"
        
        elif dichotomous == 4:
            
            dfr.loc[j, (new_column, new_column)] = "Adverse effects leading to discontinuation"
            
        elif dichotomous == 5:
            
            dfr.loc[j, (new_column, new_column)] = "Viral clearance"
            
        elif dichotomous == 6:
            
            dfr.loc[j, (new_column, new_column)] = "TRALI"
            
        elif dichotomous == 7:
            
            dfr.loc[j, (new_column, new_column)] = "TACO"
            
        elif dichotomous == 8:
            
            dfr.loc[j, (new_column, new_column)] = "Allergic reactions"
        
        elif dichotomous == 9:
            
            dfr.loc[j, (new_column, new_column)] = "Graft vs. host disease"
            
        if continuous == 1:
            
            dfr.loc[j, (new_column, new_column)] = "Duration of hospitalization"
            
        elif continuous == 2:
            
            dfr.loc[j, (new_column, new_column)] = "ICU length of stay"
            
        elif continuous == 3:
            
            dfr.loc[j, (new_column, new_column)] = "Ventilator-free days"
            
        elif continuous == 4:
            
            dfr.loc[j, (new_column, new_column)] = "Duration of ventilation"
            
        elif continuous == 5:
            
            dfr.loc[j, (new_column, new_column)] = "Time to symptom/clinical improvement"

        elif continuous == 6:
            
            dfr.loc[j, (new_column, new_column)] = "Time to to viral clearance"
            
    if remove_redundant == True:                #removes the columns we dont need anymore
        
        dfr.drop(["Dichotomous Outcome", "Continuous Outcome"], level = 0, inplace = True, axis = 1)
            
    return dfr


#find a list of heights in pixels where each list index is the row index of the dataframe
def set_row_heights(df):
    
    #dfr = df.copy()
    
    pixel_height = 15
    extra_rows = 1
    amount_of_linebreaks = []
    
    for i in range(0, len(df.index)):
        
        max_linebreaks_row = 1        
        
        for j in range(0, len(df.columns)):
            
            cell = df.iloc[i, j]
            
            if type(cell) == str: 
            
                count = cell.count("\n")
                
                if count > max_linebreaks_row:
                    
                    max_linebreaks_row = count
                    
        amount_of_linebreaks.append(pixel_height * (max_linebreaks_row + extra_rows))
        
    return amount_of_linebreaks

#to avoid getting incomplete tables due to typos on the cells of the columns we are merging over, we merge partially over id and author

def stronger_left_join_trial_characteristics(df1, df2):
    
    left_join_id = pd.merge(df1, df2, how = 'left', on = ["Ref ID"])
    left_join_id.drop(["Ref ID", "1st Author_x", "1st Author_y"], axis = 1, inplace = True)
    
    left_join_author = pd.merge(df1, df2, how = 'left', on = ["1st Author"])
    left_join_author.drop(["Ref ID_x", "Ref ID_y", "1st Author"], axis = 1, inplace = True)
    
    #concatenate over the partial merges
    left_join = pd.concat([left_join_id, left_join_author], axis = 0)
    
    left_join.drop_duplicates(inplace = True)
    
    left_join["Outcomes"].fillna("", inplace = True)
    
    #group by all the columns except the last, aggregating again the outcomes
    left_join = left_join.groupby(left_join.columns[:-1].tolist(), as_index = False)["Outcomes"].agg(lambda col: "\n".join(col))
    
    #replace the linebreak for "NR"
    left_join = left_join.replace("", "NR")
    
    return left_join


def filter_treatment_pair(df, filter_treat, n, intersection = True, node = False):
    
    if bool(filter_treat):
        
        Precursor_1_aux = df.copy()[0:0]
        
        if intersection == True:

            for i in range(1, n):
            
                treat1 = filter_treat[0]
                
                if node:
                    aux_df = df[(df[("Intervention node", f"Intervention {i} name node")] == treat1)]
                else:
                    aux_df = df[df[(f"Intervention {i}", f"Intervention {i} name")] == treat1]
        
                for j in range(1, n):
                
                    treat2 = filter_treat[1]
                    
                    if node:
                        aux_df_2 = aux_df[(aux_df[("Intervention node", f"Intervention {j} name node")] == treat2)]
                    else:
                        aux_df_2 = aux_df[aux_df[(f"Intervention {j}", f"Intervention {j} name")] == treat2]

                    Precursor_1_aux = Precursor_1_aux.append(aux_df_2, ignore_index = True)
        
        else:
            
            for i in range(1,n):
                
                treat1 = filter_treat[0]
                treat2 = filter_treat[1]
                
                if node:
                    aux_df = df[(df[("Intervention node", f"Intervention {i} name node")] == treat1) | \
                                (df[("Intervention node", f"Intervention {i} name node")] == treat2)]
                else:
                    aux_df = df[(df[(f"Intervention {i}", f"Intervention {i} name")] == treat1) | \
                                (df[(f"Intervention {i}", f"Intervention {i} name")] == treat2)]
                Precursor_1_aux = Precursor_1_aux.append(aux_df, ignore_index = True)
        return Precursor_1_aux
        
    else:
        
        return df
    
#filter treatments by nodes but without replacing the displayed entry

def clean_node_treatments_names(df, directory_file = 0):             
                                                                    #esta funcion limpia, con ayuda de treat_list_grouping, 
                                                                    #los nombres de los tratamientos, ignorando dias y dosis y ordenando los elementos
    dfr=df.copy()
    #n_trial = 7
    
    for n in range(1, 20):
            #print(n)
        try:
            dfr["Intervention {}".format(n)]
        except KeyError:
            break
        
    if n == 1:
        n=2
    
    for i in range(1, n):                                     #Check all cells of names
        
        dfr[("Intervention node", f"Intervention {i} name node")]= ""
        
        for j in range(0, len(dfr.index)):
                                                                    #revisamos una celd
            aux_str = dfr.loc[j, (f"Intervention {i}", f"Intervention {i} name")]
                
            if type(aux_str) == str:                                #vemos solo los strings
                    
                aux_str = re.sub(r"\([^)]*\)", "", aux_str)         #esto quita los parentesis
                aux_str = aux_str.strip()
                aux_list = aux_str.split(", ")
                                                                                #aqui va el combinador para entender distintas categorias como una, 
                #aux_list = check_spelling_manually_lol(aux_list)                #como placebo y standard care, en la forma de placebo/standard care
                
                if type(directory_file) != int:
                    
                    aux_list.sort()
                    
                    aux_str = ", ".join(aux_list)
                    
                    index_node_list = directory_file.loc[directory_file.isin([aux_str]).any(axis=1)].index.tolist()
                        
                    if len(index_node_list) == 0:

                        aux_str = aux_str
                            
                    else:
                            
                        index_node = index_node_list[0]
                        aux_str = directory_file.loc[index_node,"Node"]
                            
                    aux_list = aux_str.split(", ")
                    
                #aux_list = treat_list_grouping(aux_list, adverse_events)
            
                aux_list.sort()                 #reconstruye el string

                dfr.loc[j, ("Intervention node", f"Intervention {i} name node")] = ", ".join(aux_list)

    return dfr