#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Mon Feb 15 18:21:17 2021

@author: antonio
"""

import pandas as pd            #for creating the spreadsheet 
import numpy as np             #for nan
import re as re                #for sub


#funciones

#esta funcion debe llamarse primero
def cleandf(df, total_nan = False):                                                    #funcion para dejar la base de datos limpia y con los indices de columnas adecuados
    
    dfr = df.copy()
    max_title = 200                                                 #Para quitar los encabezados con descripciones largas
    
    for i in range(0, len(dfr.index)):                              #Esto limpia de espacios y enters todo el dataframe
        
        for j in range(0, len(dfr.columns)):               
            
            if type(dfr.iloc[i,j]) == str:                  
                
                dfr.iloc[i,j] = ' '.join(dfr.iloc[i,j].split())     #Esto limpia todas los strings de espacios de sobra y "\n"
                
                if i < 3 and len(dfr.iloc[i,j]) > max_title:        #revisando las primeras filas, quitamos los titulos demasiado largos

                    dfr.iloc[i,j] = np.nan
                
    aux_dfr = dfr.iloc[:2,:]                                        #Usamos un dataframe auxiliar para limpiar los nombres de las columnas
    
    aux_dfr = aux_dfr.fillna(method = "ffill", axis = 1)
    aux_dfr = aux_dfr.fillna(method = "ffill", axis = 0)
    dfr = dfr.drop([0, 1, 2])                                       #y borramos las filas de los encabezados del excel para tener algo mas limpio
    
    mult_index = aux_dfr.T                                          #Transpuesta
    dfr.columns = pd.MultiIndex.from_frame(mult_index)              #reindexea con multiindex de 2 niveles      
    
    if total_nan == True:
        
        dfr = dfr.replace("NR", np.nan)                                 #reemplaza nr con valores nan
    
    dfr = dfr.loc[:,~dfr.columns.duplicated()]                      #Comentar esto correctamente, creo que quita los duplicados de nombres de las columnas
    dfr.reset_index(inplace = True, drop = True)
    dfr = dfr.sort_index()
    
    return dfr


def subdf(df, sheet):                                               #obtener las columnas que nos interesen
                                                                    #usar despues de cleandf necesariamente
    name_columns =["Ref ID", "1st Author"]
    dfr = df.copy()
    #n_trial = 7
    n_risk = 7
    
    if sheet == "Trial characteristics":                            #Sacar subdataframe the trial  characteristics
        
        for n in range(1, 20):
            #print(n)
            try:
                dfr["Intervention {}".format(n)]
                name_columns.append(f"Intervention {n}")

            except KeyError:
                break
    
        #for i in range(1, n):
            
        #    name_columns.append(f"Intervention {i}")
        
        dfr = dfr[name_columns] 
        
        for i in range(1, n):
            
            dfr.drop(f"Intervention {i} description (dose, duration)", axis = 1, level = 1, inplace = True)
            
            if f"Intervention {i} description (dose, duration) revised" in dfr.columns.get_level_values(1):
                
                dfr.drop(f"Intervention {i} description (dose, duration) revised", axis = 1, level = 1, inplace = True)
        
    if sheet == "Dichotomous outcomes":
        
        dfr.drop("Comments", axis = 1, level = 0, inplace = True)
        dfr.drop("Follow-up time (days)", axis = 1, level = 1, inplace = True)
        dfr.drop("Number of events", axis = 1, level = 1, inplace = True)
        
    if sheet == "Dichotomous outcomes-severity":
        
        dfr.drop("Comments", axis = 1, level = 0, inplace = True)
        dfr.drop("Follow-up time (days)", axis = 1, level = 1, inplace = True)
        dfr.drop("Number of events", axis = 1, level = 1, inplace = True)
        dfr.drop("Justfication", axis = 1, level = 1, inplace = True)
        
    if sheet == "Continuous outcomes":    
        
        dfr.drop("Comments", axis = 1, level = 0, inplace = True)
        dfr.drop("Follow-up time (days)", axis = 1, level = 1, inplace = True)
        dfr.drop("Time to symptom resolution or time to clinical improvement criteria", axis = 1, level = 1, inplace = True)
        dfr.drop("Measure of central tendency", axis = 1, level = 1, inplace = True)
        dfr.drop("Central tendency", axis = 1, level = 1, inplace = True)
        dfr.drop("Measure of variability", axis = 1, level = 1, inplace = True)
        dfr.drop("Variability", axis = 1, level = 1, inplace = True)
        
    if sheet == "Continuous outcomes-severity":    
        
        dfr.drop("Comments", axis = 1, level = 0, inplace = True)
        dfr.drop("Follow-up time (days)", axis = 1, level = 1, inplace = True)
        dfr.drop("Time to symptom resolution or time to clinical improvement criteria", axis = 1, level = 1, inplace = True)
        dfr.drop("Measure of central tendency", axis = 1, level = 1, inplace = True)
        dfr.drop("Central tendency", axis = 1, level = 1, inplace = True)
        dfr.drop("Measure of variability", axis = 1, level = 1, inplace = True)
        dfr.drop("Variability", axis = 1, level = 1, inplace = True)
        dfr.drop("Justfication", axis = 1, level = 1, inplace = True)

    if sheet == "Risk of bias":                                     #saca subdate de risk of bias
        
        dfr.drop("Comments", axis = 1, level = 0, inplace = True)
        for i in range(1, n_risk):
            
            dfr.drop(f"Domain {i} justification", axis = 1, level = 1, inplace = True)
        #dfr.drop('6) Other biases (e.g., competing risks)', axis = 1, level = 1, inplace = True)
        
    return dfr


def check_spelling_manually_lol(lst):              #a falta de opciones mejores para spellcheck, y que no hay tantas faltas de ortografia, hacemos spellcheck manualmente
    
    n = len(lst)
    
    for m in range(0, n):
        
        lst[m] = "".join(lst[m].rstrip())
        name =lst[m]
        
        if name == "standard of care":
            
            lst[m] = "standard care/placebo"
        
        if name == "interferon-beta 1a (low-dose) (subcutaneous)" or name == "interferon beta-1a (low-dose) (subcutaneous)":
            
            lst[m] = "interferon beta-1a (low dose) (subcutaneous)"
            
        if name == "interferon-beta 1a (high-dose) (subcutaneous)" or name == "interferon beta-1a (high-dose) (subcutaneous)":
            
            lst[m] = "interferon beta-1a (high dose) (subcutaneous)"
        
        if name == "siltuxumab":
            
            lst[m] = "siltuximab"
            
        if name == "favipravir":
            
            lst[m] = "favipiravir"
            
        if name == "rivabirin":
            
            lst[m] = "ribavirin"
            
        if name == "α-lipoic acid":
            
            lst[m] = "alpha lipoic acid"
            
        if name == "ramipril":
            
            lst[m] = "rampiril"
            
        if name == "cacifediol":
            
            lst[m] = "calcifediol"
            
        if name == "tocfacitnib":
            
            lst[m] = "tofacitinib"
            #name == "chloroquine `" or 
        if "ychloroquine" in name:

            lst[m] = "hydroxychloroquine"
            
        if name == "nano-curcumin" or name == "nanocurcimin":
            
            lst[m] = "nanocurcumin"
        
        if name == "remdesivir (5 days)" or name == "remdesivir (10 days)":
            
            lst[m] = "remdesivir"
            
        if name == "usual care" or name == "control" or name == "Placebo" or name == "Standard care":
            
            lst[m] = "standard care/placebo"
            
    return lst
    

def treat_list_grouping(lst, adverse_events = False):              #esta funcion agrupa de acuerdo a las directrices que nos mandan
    #esta funcion esta interfiriendo con el correcto funcionamiento de los nodos. Se va a dehjar de usar
    n = len(lst)
    
    for m in range(0, n):
        
        lst[m] = "".join(lst[m].rstrip())
        name =lst[m]
        
        if name == "siltuximab" or name == "tocilizumab":
            
            lst[m] = "IL6 receptor antagonists"
            
        if name == "placebo" or name == "standard care":
            
            lst[m] = "standard care/placebo"
        
            
        if name == "dexamethasone" or name == "methylprednisolone":
            
            lst[m] = "corticosteroids"
            
        if adverse_events == False:                                #Para la pestaña de efectos adversos necesitamos dejar separado las cloroquinas       
            
            if name == "chloroquine":
            
                lst[m] = "hydroxychloroquine"
                
        if adverse_events == True:                                #Para la pestaña de efectos adversos necesitamos dejar separado las cloroquinas       
            
            if name == "barticitinib" or name == "ruxolitinib" or name =="tofacitinib":
            
                lst[m] = "JAKi"
            
        if "nterferon" in name:
            
            if "inhaled nebulised" not in name:
            
                lst[m] = lst[m].replace("-", " ")
                lst[m] = lst[m][0].lower() + lst[m][1:]
                aux_list = lst[m].split()
                index = [idx for idx, s in enumerate(aux_list) if 'nterferon' in s][0]
                
                if (index + 1) < len(aux_list):
                    lst[m] = ' '.join(aux_list[index:index + 2])
            
                else:
                    lst[m] = ' '.join(aux_list)

    return lst

    
def clean_treatments_names(df, sheet = "Trial characteristics", adverse_events = False, directory_file = 0, node_mask_inplace = True):             
                                                                    #esta funcion limpia, con ayuda de treat_list_grouping, 
                                                                    #los nombres de los tratamientos, ignorando dias y dosis y ordenando los elementos
    dfr=df.copy()
    #n_trial = 7
    
    exceptions_list = ["PNB001", "SNG001"]
    list_treatment_not_in_node_file = []
    
    for n in range(1, 20):

        try:
            dfr["Intervention {}".format(n)]
        except KeyError:
            break
        
    if n == 1:
        n=2
    
    for i in range(1, n):                                     #Check all cells of names
    
        if node_mask_inplace == False:
            
            dfr[("Intervention node", f"Intervention {i} name node")]= ""
        
        for j in range(0, len(dfr.index)):
                                                                    #revisamos una celda
            if sheet == "Trial characteristics":
                
                aux_str = dfr.loc[j, (f"Intervention {i}", f"Intervention {i} name")]
                
            elif sheet == "Dichotomous outcomes" or sheet == "Dichotomous outcomes-severity" or sheet == "Dichtomous outcomes":
                
                aux_str = dfr.loc[j, ("Intervention name", "Intervention name")]
            
            elif sheet == "Continuous outcomes" or sheet == "Continuous outcomes-severity":
                
                aux_str = dfr.loc[j, ("Intervention name", "Intervention name")]

            
            if type(aux_str) == str:                                #vemos solo los strings
                
                if "(" in aux_str:
                    
                    inside_parentheses = re.search(r'\((.*?)\)',aux_str).group(1)
                    parentheses_contains_digit = False
                
                    if type(inside_parentheses) == str:
                        for character in inside_parentheses:
                    
                            if character.isdigit():
                                parentheses_contains_digit = True
                
                    if parentheses_contains_digit:
                    
                        if inside_parentheses  not in exceptions_list:
                        
                            aux_str = re.sub(r"\([^)]*\)", "", aux_str)         #esto quita los parentesis
                        
                #aux_str = aux_str[0].lower() + aux_str[1:]
                aux_str = aux_str.strip(' `')
                aux_list = aux_str.split(", ")
                                                                                #aqui va el combinador para entender distintas categorias como una, 
                aux_list = check_spelling_manually_lol(aux_list)                #como placebo y standard care, en la forma de placebo/standard care
                
                if type(directory_file) != int:
                    
                    directory_file[directory_file.columns] = directory_file.apply(lambda x: x.str.strip())
                    
                    aux_str = ", ".join(aux_list)
                    aux_str_lower = aux_str[0].lower() + aux_str[1:]
                    aux_list.sort(key=lambda v: v.upper())
                    aux_str_order = ", ".join(aux_list)
                        
                    index_node_list = directory_file.loc[directory_file.isin([aux_str]).any(axis=1)].index.tolist()
                    index_node_list_lower = directory_file.loc[directory_file.isin([aux_str_lower]).any(axis=1)].index.tolist()
                    index_node_list_order = directory_file.loc[directory_file.isin([aux_str_order]).any(axis=1)].index.tolist()
                    
                    if len(index_node_list) == 0 and len(index_node_list_lower) == 0 and len(index_node_list_order) == 0:

                        aux_str = aux_str
                        list_treatment_not_in_node_file.append(aux_str)
                        
                    elif len(index_node_list) != 0:
                            
                        index_node = index_node_list[0]
                        aux_str = directory_file.loc[index_node,"Node"]
                        
                    elif len(index_node_list_lower) != 0:
                            
                        index_node = index_node_list_lower[0]
                        aux_str = directory_file.loc[index_node,"Node"]
                        
                    elif len(index_node_list_order) != 0:
                            
                        index_node = index_node_list_order[0]
                        aux_str = directory_file.loc[index_node,"Node"]
                            
                    aux_list = aux_str.split(", ")
                    
                #aux_list = treat_list_grouping(aux_list, adverse_events)
            
                aux_list.sort(key=lambda v: v.upper())
                                                                    #reconstruye el string
                                                                    
                if node_mask_inplace == False:
                    
                    dfr.loc[j, ("Intervention node", f"Intervention {i} name node")]= ", ".join(aux_list)
                    
                elif sheet == "Trial characteristics":
                
                    dfr.loc[j, (f"Intervention {i}", f"Intervention {i} name")] = ", ".join(aux_list)
                
                elif sheet == "Dichotomous outcomes" or sheet == "Dichotomous outcomes-severity":
                
                    dfr.loc[j, ("Intervention name", "Intervention name")] = ", ".join(aux_list)
            
                elif sheet == "Continuous outcomes" or sheet == "Continuous outcomes-severity":
                
                    dfr.loc[j, ("Intervention name", "Intervention name")] = ", ".join(aux_list)
    set_treatments_not_in_node_file = list(set(list_treatment_not_in_node_file))
    print(f"List of treatments not in table of nodes from sheet {sheet}")
    print(set_treatments_not_in_node_file)
    return dfr
                              

def id_order(df):                                                   #ordena RefId de forma que si tiene varios strings, los ordena con sort() de forma que todo salga igual
    
    dfr = df.copy()
    dfr["Ref ID"] = dfr["Ref ID"].astype(str)
    
    for i in range(0, len(dfr.index)):
        
        if type(dfr.iloc[i, 0]) == int or type(dfr.iloc[i, 0]) == float:
            
            dfr.iloc[i, 0] = int(dfr.iloc[i, 0])
            
        dfr.iloc[i, 0] = str(dfr.iloc[i, 0])
        aux_list = dfr.iloc[i, 0].split(", ")           #necesito el auxiliar ya que sin el no hace el sort()
        aux_list.sort(reverse = True)
        dfr.iloc[i, 0] = ", ".join(aux_list)

    return dfr



def treatmentsv2(df):                                               #funcion auxiliar para obtener una lista de los medicamentos en un solo dataframe con nombre y numero
    
    dfr = df.copy()
    srs = pd.DataFrame()
    #n_trial = 7
    
    for n in range(1, 20):
            #print(n)
        try:
            aux=dfr["Intervention {}".format(n)]
        except KeyError:
            break

    
    for i in range(1, n):                                     
        
        aux = dfr.loc[:, f"Intervention {i}"]
        aux = aux.rename(columns = { f"Intervention {i} name" : "name", "N randomized" : "N"})
        srs = pd.concat([srs, aux], ignore_index = True, axis = 0)
    
    srs.dropna(inplace = True)
    srs.reset_index(inplace = True, drop = True)
    
    return srs



def duplicated_in_study(df, sheet = "Trial characteristics"):                                        #Despues de limpiar todo, algunas columnas se repiten dado que quitamos dosis y dias
    
    dfr = df.copy()
    #n_trial = 7
    
    for n in range(1, 20):
            #print(n)
        try:
            dfr["Intervention {}".format(n)]
        except KeyError:
            break
        
    for i in range(1, n):
        
        dfr[(f"Intervention {i}", f"Intervention {i} name")].replace("NR", np.nan, inplace = True)
    
    for j in range(0, len(dfr.index)):                              #En todas las filas, revisamos si se repiten los nombres de tratamientos
        
        aux_list = []
        
        for i in range(1, n):                                 #checar si se repiten los nombres
            
            cell = dfr.loc[j, (f"Intervention {i}", f"Intervention {i} name")]
            
            if pd.isna(cell) == False and type(cell) == str:                              #solo nos preocupamos en celdas no vacias
                
                if cell not in aux_list:                            #si el nombre no esta en la lista, la agregamos
                
                    aux_list.append(cell)
                    
                else:                                               #si esta en la lista, encontramos en que celda y sumamos el numero de pacientes a la celda correspondiente
                    
                    index_of_column = list(dfr.loc[j, :]).index(cell)
                    dfr.iloc[j, index_of_column +1] += dfr.loc[j, (f"Intervention {i}", "N randomized")]
                                                                    #borramos el nombre y cantidad que se repiten, dado que ya lo sumamos
                    dfr.loc[j, (f"Intervention {i}", f"Intervention {i} name")] = float("NaN")
                    dfr.loc[j, (f"Intervention {i}", "N randomized")] = float("NaN")

    return dfr


def cross_treatments(df):                                           #saca nuevas filas relacionando diferentes intervenciones dentro del mismo estudio
    
    the_columns = ["Treatment 1", "Treatment 2", "Ref ID", "1st Author", "Total N"]
    dfr = pd.DataFrame(columns = the_columns)
    #n_trial = 7

    for n in range(1, 20):
            #print(n)
        try:
            dfr["Intervention {}".format(n)]
        except KeyError:
            break
        
    for i in range(0, len(df.index)):
                
        for j in range(1, n):
            
            cell1 = df.loc[i, (f"Intervention {j}", f"Intervention {j} name")]
            num1 = df.loc[i, (f"Intervention {j}", "N randomized")]
            
            if pd.isna(cell1) == False:
                
                for k in range(j+1, n):
                    
                    cell2 = df.loc[i, (f"Intervention {k}", f"Intervention {k} name")]
                    num2 = df.loc[i, (f"Intervention {k}", "N randomized")]
                    
                    if pd.isna(cell2) == False:
                        
                        row_to_append = pd.DataFrame(index =[0], columns = the_columns)                        
                        
                        aux_string = df.loc[i, ("Ref ID", "Ref ID")]
                        row_to_append.loc[0, "Ref ID"] = aux_string

                        aux_string = df.loc[i, ("1st Author", "1st Author")]
                        row_to_append.loc[0, "1st Author"] = aux_string
                        
                        if pd.isna(num1) == True or type(num1) == str:
                            
                            df.loc[i, (f"Intervention {j}", "N randomized")] = 0
                            
                        if pd.isna(num2) == True or type(num2) == str:
                            
                            df.loc[i, (f"Intervention {k}", "N randomized")] = 0
                        
                        total_patients = df.loc[i, (f"Intervention {j}", "N randomized")] + \
                        df.loc[i, (f"Intervention {k}", "N randomized")]       #int(filter(str.isdigit, num1)) + int(filter(str.isdigit, num2))
                        row_to_append.loc[0, "Total N"] = total_patients
                        
                        if (cell1 != "standard care/placebo" and cell1 != "placebo/standard care") and (cell2 != "standard care/placebo" and cell2 != "placebo/standard care"):
                            
                            sorting_list = [str(cell1), str(cell2)]
                            sorting_list.sort()
                            
                            row_to_append.loc[0, "Treatment 1"] = sorting_list[0]
                            row_to_append.loc[0, "Treatment 2"] = sorting_list[1]
                            
                        else:
                            
                            if cell2 == "standard care/placebo" or cell2 == "placebo/standard care":
                                
                                row_to_append.loc[0, "Treatment 1"] = cell1
                                row_to_append.loc[0, "Treatment 2"] = cell2
                                
                            else:
                                
                                row_to_append.loc[0, "Treatment 1"] = cell2
                                row_to_append.loc[0, "Treatment 2"] = cell1
                        
                        dfr = dfr.append(row_to_append, ignore_index = True)

    return dfr

def order_treatments_on_2_columns(df, treatment1 = 'Intervention name_x', treatment2 = 'Intervention name_y', associated_vars1 = [], associated_vars2 = [], Total_N = True):
    
    df.reset_index(inplace = True, drop = True)
    dfr = df.copy()
    treatments1 = treatment1
    treatments2 = treatment2
    #N1 = 'Total N_x'
    #N2 = 'Total N_y'
    if len(associated_vars1) != len(associated_vars2):
        print("number of associated variables to treatments do not match")
    
    for i in range(0, len(df.index)):
        
        cell1 = df.loc[i, treatments1]
        #num1 = df.loc[i, N1]
        
        cell2 = df.loc[i, treatments2]
        #num2 = df.loc[i, N2]
        
        if (cell1 != "standard care/placebo" and cell1 != "placebo/standard care") and (cell2 != "standard care/placebo" and cell2 != "placebo/standard care"):
                            
            sorting_list = [str(cell1), str(cell2)]
            sorting_list.sort(key=lambda v: v.upper())
                            
            dfr.loc[i, treatments1] = sorting_list[0]
            dfr.loc[i, treatments2] = sorting_list[1]
            
            if str(cell1) != sorting_list[0]:
                
                for column1, column2 in zip(associated_vars1, associated_vars2):
                    
                    aux_var =  dfr.loc[i, column1]
                    dfr.loc[i, column1] = dfr.loc[i, column2]
                    dfr.loc[i, column2] = aux_var
                            
        else:
                            
            if cell2 == "standard care/placebo" or cell2 == "placebo/standard care":
                                
                dfr.loc[i, treatments1] = cell1
                dfr.loc[i, treatments2] = cell2         
                                
            else:
                                
                dfr.loc[i, treatments1] = cell2
                dfr.loc[i, treatments2] = cell1
                
                for column1, column2 in zip(associated_vars1, associated_vars2):
                    
                    aux_var =  dfr.loc[i, column1]
                    dfr.loc[i, column1] = dfr.loc[i, column2]
                    dfr.loc[i, column2] = aux_var
        
        if Total_N:
            
            dfr["Total N"] = dfr["Total N_x"] + dfr["Total N_y"]

    return dfr

#get the merge of RoB and one of the other sheets, COnt or Dich
def get_partial(dfRoB, dfOut, dich_or_cont):
    
    partial_id = pd.merge(dfRoB, dfOut, on = ["Ref ID", dich_or_cont])
    partial_id.drop(["1st Author_y"], axis = 1, inplace = True)
    partial_id.rename(columns = {"1st Author_x" : "1st Author"}, inplace = True)
    
    partial_author = pd.merge(dfRoB, dfOut, on = ["1st Author", dich_or_cont])
    partial_author.drop(["Ref ID_y"], axis = 1, inplace = True)
    partial_author.rename(columns = {"Ref ID_x" : "Ref ID"}, inplace = True)
    
    # col_list = partial_author.columns.tolist()
    # col_list.remove("1st Author")
    
    # partial_author = partial_author.merge(partial_id.drop_duplicates(), on=col_list, how='left', indicator=True)
    # partial_author = partial_author[partial_author['_merge'] == 'left_only']
    # partial_author.drop(["1st Author_y"], axis = 1, inplace = True)
    # partial_author.rename(columns = {"1st Author_x" : "1st Author"}, inplace = True)
    # partial_author.drop(['_merge'], axis = 1, inplace = True)
    
    partial = pd.concat([partial_id, partial_author], axis = 0)
    partial.drop_duplicates(subset = partial.columns[1:].tolist(), inplace = True)
    partial.drop_duplicates(subset = partial.columns[0:1].tolist() + \
                            partial.columns[2:].tolist(), inplace = True)
    
    return partial


#this function takes both partial merges and vomits the df we need
def literally_a_black_box_that_gets_us_what_we_need_lol(df1, df2):
    
    column_order = ["Treatment 1", "Treatment 2", "Ref ID", "1st Author", "Total N", "1) Bias arising from the randomization process", \
                    "2) Bias due to deviations from the intended intervention", "3) Bias due to missing outcome data", \
                    "4) Bias in measurement of the outcome", "5) Bias in selection of the reported results", \
                    "6) Other biases (e.g., competing risks)", "Dichotomous Outcome", "Continuous Outcome"]

    inner_join_precursors = pd.concat([df1, df2], axis = 0)
    inner_join_precursors.rename(columns = {"N analyzed" : "Total N"}, inplace = True)
    inner_join_precursors.reset_index(inplace = True, drop = True)

    inner_join_precursors = pd.merge(inner_join_precursors, inner_join_precursors, on = inner_join_precursors.columns.values[0:10].tolist())
    inner_join_precursors.drop(inner_join_precursors[inner_join_precursors['Intervention name_x'] == inner_join_precursors['Intervention name_y']].index, inplace = True)

    inner_join_precursors = order_treatments_on_2_columns(inner_join_precursors)
    inner_join_precursors.drop(["Total N_x", "Total N_y"], axis = 1, inplace = True)
    inner_join_precursors = inner_join_precursors[inner_join_precursors.duplicated()]  
    inner_join_precursors.reset_index(inplace = True, drop = True)
    inner_join_precursors.rename(columns = {"Intervention name_x" : "Treatment 1", "Intervention name_y" : "Treatment 2"}, inplace = True)
    inner_join_precursors = inner_join_precursors[column_order]
    
    
    return inner_join_precursors

def get_outcomes_ready(df, sheet, adverse_events = False, directory_file = 0, total_nan = False):
    
    dfr = find_int_in_string(id_order(subdf(clean_treatments_names(cleandf(df, total_nan = False), sheet = sheet, adverse_events = adverse_events, directory_file = directory_file), sheet)), start_column = 3, end_column = 4)
    dfr.columns = dfr.columns.get_level_values(1) 
    dfr = dfr.groupby(["Ref ID", "1st Author", "Intervention name", "Outcome"], as_index = False)["N analyzed"].agg(lambda x: x.sum())
    
    return dfr

#variation of past function for when we can discriminate by severity
def get_outcomes_ready_severity(df, sheet, adverse_events = False, directory_file = 0, total_nan = False):
    
    dfr = find_int_in_string(id_order(subdf(clean_treatments_names(cleandf(df, total_nan = False), sheet = sheet, adverse_events = adverse_events, directory_file = directory_file), sheet)), start_column = 3, end_column = 5)
    dfr.columns = dfr.columns.get_level_values(1) 
    dfr = dfr.groupby(["Ref ID", "1st Author", "Intervention name", "Outcome", "Severe"], as_index = False)["N analyzed"].agg(lambda x: x.sum())
    
    return dfr
          
def get_trial_characteristics_ready(df, sheet, adverse_events = False, directory_file = 0, total_nan = False):     #aglomerado para sacar el gradeing
    
    
    dfr = cross_treatments(duplicated_in_study(id_order(subdf(clean_treatments_names(cleandf(df, total_nan = total_nan), adverse_events = adverse_events, directory_file = directory_file), sheet))))

    return dfr


def get_risk_of_bias_ready(df, sheet, total_nan = False): 
                                     #aglomerado para sacar el gradeing 2
    dfr = id_order(subdf(cleandf(df, total_nan = total_nan), sheet))

    bottom_level = dfr.columns.get_level_values(1).tolist()

    substring = "Outcome"

    outcome_not_in_bt_lvl = any(substring not in string for string in bottom_level)

    if outcome_not_in_bt_lvl:
        
        top_level = dfr.columns.get_level_values(0).tolist()
        
        outcome = [i for i, s in enumerate(top_level) if substring in s]
        
        for i in outcome:
            
            bottom_level[i] = top_level[i]
            
        dfr.columns = bottom_level
        
    else:
        
        dfr.columns = dfr.columns.get_level_values(1)
    
    return dfr

#difference between old and new data archives, to find "# new treatment column"

def differences_on_new_doc(df_old, df_new):
    
    if type(df_old) != int:
        # df = pd.concat([df_old, df_new])
    
        # df = df[["Treatment 1", "Treatment 2", "Ref ID", "1st Author", "Dichotomous Outcome", "Continuous Outcome"]]

        # df = df.drop_duplicates(keep = False)

        merge_columns = ["Treatment 1", "Treatment 2", "Ref ID", "1st Author", "Dichotomous Outcome", "Continuous Outcome"]
        df = pd.merge(df_old, df_new, how='outer', on = merge_columns, indicator=True)
        df = df[df["_merge"]=="right_only"]
        df = df[["Treatment 1", "Treatment 2", "Dichotomous Outcome", "Continuous Outcome"]]
        
    else:
        
        df = df_new.copy()
        df = df[["Treatment 1", "Treatment 2", "Dichotomous Outcome", "Continuous Outcome"]]
    
    return df

# df3 = pd.merge(df1, df2, how='outer', indicator=True)
#         .query('_merge=="left_only"')
#         .drop('_merge',1)
        
# titanic[titanic["Age"] > 35]

def number_of_new_treatments_column(df_old, df_new):
    
    df = differences_on_new_doc(df_old, df_new)

    #count the amount of treatment convinations that are new between gradeing studies

    new_treatments_number = df.value_counts(dropna = False).reset_index()
    new_treatments_number.rename(columns = {0 : "# of new trials"}, inplace = True)

    # finally gets the dataframe ready to export in an excel, ready for formatting

    inner_join_precursors = pd.merge(df_new, new_treatments_number, how = 'left', on = ["Treatment 1", "Treatment 2", "Dichotomous Outcome", "Continuous Outcome"])

    inner_join_precursors["# of new trials"].fillna(value = 0, inplace = True)

    #change column of number of new trials of place and group by treatment combination

    new_trials_column = inner_join_precursors.pop("# of new trials") 
    inner_join_precursors.insert(2, "# of new trials", new_trials_column) 
    inner_join_precursors.sort_values(by = ["Treatment 1", "Treatment 2"], inplace = True)
    
    return inner_join_precursors

#finds integers wrongly captured as strings and makes them integers again
        
def find_int_in_string(df, start_column = 0, end_column = 1):
    
    dfr = df.copy()
    
    for i in range(0, len(df.index)):
        
        for j in range(start_column, end_column + 1):
            
            cell = dfr.iloc[i, j]
            
            if type(cell) == str:
                
                if any(c.isdigit() for c in cell):
                
                    aux_thing = re.findall('[0-9]+', df.iloc[i, j])
                    dfr.iloc[i, j] = max([int(x) for x in aux_thing])
                    
                else:
                    
                    dfr.iloc[i, j] = np.nan
                
    return dfr    
# #finds integers wrongly captured as strings and makes them integers again
        
# def find_int_in_string(df, start_column = 0, end_column = 1, column_name_list = []):
    
#     dfr = df.copy()
    
#     if not column_name_list:
    
#         for i in range(0, len(df.index)):
        
#             for j in range(start_column, end_column + 1):
            
#                 cell = dfr.iloc[i, j]
            
#                 if type(cell) == str:
                
#                     if any(c.isdigit() for c in cell):
                
#                         dfr.iloc[i, j] = int(re.sub("\D", "", df.iloc[i, j]))
                    
#                     else:
                    
#                         dfr.iloc[i, j] = np.nan
                        
#     else:
        
#         for i in range(0, len(df.index)):
            
#             for column in column_name_list:
                
#                 cell = dfr.loc[dfr.index[i], column]
                
#                 if type(cell) == str:
                
#                     if any(c.isdigit() for c in cell):
                
#                         dfr.loc[dfr.index[i], column] = int(re.sub("\D", "", dfr.loc[dfr.index[i], column]))
                    
#                     else:
                    
#                         dfr.loc[dfr.index[i], column] = np.nan
        
                
#     return dfr    

#this function isnt used
def subheaders_treatments(df):
    
    dfr = df.copy()
    
    treatment_title = ("", "")
    
    #treatment_headers = pd.DataFrame().reindex(columns=dfr.columns)
    
    for i in range(0, len(df.index)):
        
        if (dfr.iloc[i, 0], dfr.iloc[i, 1]) != treatment_title:
            
            treatment_title = (dfr.iloc[i, 0], dfr.iloc[i, 1])
            
        else:
            
            pass
        
# call this function to style the risk of bias background colors
#not used
def risk_of_bias_styler(cell):
    
    if cell == "low risk of bias":
        
        color = "#70ad47"
        
    elif cell == "probably low risk of bias":
        
        color = "yellow"
        
    elif cell == "probably high risk of bias":
        
        color = "#ed7d31"
        
    elif cell == "either probably low or probably high risk of bias":
        
        color = "#ed7d31"
        
    elif cell == "high risk of bias":
        
        color = "red"

    return 'background-color: %s' % color     

#insert columns for future data we may want to include, but for now it is empty

def insert_empty_columns(df, n_column):
    
    df.insert(n_column, "Characteristic 5", "")
    df.insert(n_column, "Characteristic 4", "")
    df.insert(n_column, "Characteristic 3", "")
    df.insert(n_column, "Characteristic 2", "")   
    
#change the names of the dataframe
def correct_column_naming(df):
    
    df.rename(columns = {"Ref ID" : "Trial", "1st Author" : "First author", "Total N" : "N", \
                                        '1) Bias arising from the randomization process' : "Randomization", \
                                        '2) Bias due to deviations from the intended intervention' : "Deviations from the intended\nintervention", \
                                        '3) Bias due to missing outcome data' : "Missing outcome data", \
                                        '4) Bias in measurement of the outcome' : "Measurement of outcome", \
                                        '5) Bias in selection of the reported results' : "Selection of the reported\nresults", \
                                        '6) Other biases (e.g., competing risks)' : "Other"}, inplace = True)
    
#like the last function, but for the dataframe of the merge of dichotomous + continuous and RoB
def correct_column_naming_Outcomes(df):
    
    df.rename(columns = {"Ref ID" : "Trial", "1st Author" : "First author", "Total N" : "N", \
                                        '1) Bias arising from the randomization process' : "Randomization", \
                                        '2) Bias due to deviations from the intended intervention' : "Deviations from the intended\nintervention", \
                                        '3) Bias due to missing outcome data' : "Missing outcome data", \
                                        '4) Bias in measurement of the outcome' : "Measurement of outcome", \
                                        '5) Bias in selection of the reported results' : "Selection of the reported\nresults"}, inplace = True)


# Instead of a simple merge, we use this to get what we need, merging by the ID
#In the future we could "merge" using the union of both a 1st author merge and a ID merge

def pdmerge_id(df1, df2, on = ["Ref ID"]):
    
    inner_join_precursors_id = pd.merge(df1, df2, on = on)
    inner_join_precursors_id.drop(["1st Author_y"], axis = 1, inplace = True)
    inner_join_precursors_id.rename(columns = {"1st Author_x" : "1st Author"}, inplace = True)

    return inner_join_precursors_id    

#aux function to insert empty row 
def insert_empty_row(x):
    x.loc[-1] = pd.Series([])
    return x

#creates the dataframe for the sheets

def gradeing_sheet_parse(df, dichotomous_or_continuous, n_outcome):
    
    dfr = df.copy()
    
    dfr = dfr[dfr[dichotomous_or_continuous] == n_outcome]
    dfr = dfr.drop(["Dichotomous Outcome", "Continuous Outcome"], axis = 1)
    dfr.reset_index(drop=True, inplace=True)
    
    #this part does the grouped by treatments part
    
    treatments_groups = dfr.groupby(["Treatment 1", "Treatment 2", "# of new trials"], \
                                    as_index = False)["N"].sum()
        
    dfr = pd.concat([treatments_groups, dfr], axis = 0)
    
    if dichotomous_or_continuous == "Continuous Outcome":
        
        dfr = dfr[['Treatment 1', 'Treatment 2', '# of new trials', 'Trial', \
                   'First author', 'N', 'Characteristic 2', 'Characteristic 3', \
                   'Characteristic 4', 'Characteristic 5', 'Randomization', \
                   'Deviations from the intended\nintervention', 'Missing outcome data', \
                   'Measurement of outcome', 'Selection of the reported\nresults', 'Other']]
        
    else:
    
        dfr = dfr[['Treatment 1', 'Treatment 2', '# of new trials', 'Trial', \
                   'First author', 'N', 'Characteristic 2', 'Characteristic 3', \
                   'Characteristic 4', 'Characteristic 5', 'Randomization', \
                   'Deviations from the intended\nintervention', 'Missing outcome data', \
                   'Measurement of outcome', 'Selection of the reported\nresults']]
    
    dfr.sort_values(by = ["Treatment 1", "Treatment 2"], inplace = True)
    dfr.reset_index(drop=True, inplace=True)
    
    #insert empty rows as the format indicates
    dfr = dfr.groupby(['Treatment 1', 'Treatment 2'], as_index=False).apply(insert_empty_row)
    dfr.reset_index(drop=True, inplace=True)
    
    for i in range(0, len(dfr.index)):
        #print(dfr.loc[i, "Trial"])
        if type(dfr.loc[i, "First author"]) == str:
            
            dfr.loc[i, "Treatment 1"] = np.nan
            dfr.loc[i, "Treatment 2"] = np.nan
            dfr.loc[i, "# of new trials"] = np.nan
    
    return dfr

#replaces the number coding into the corresponding wording
    
def convert_bias_from_numbers(df):
    
    dfr = df.copy()
    
    columns = ["Randomization", "Deviations from the intended\nintervention", "Missing outcome data", \
               "Measurement of outcome", "Selection of the reported\nresults", "Other"]
    
    dfr[columns] = dfr[columns].mask(dfr == 1, "low risk of bias")
    dfr[columns] = dfr[columns].mask(dfr == 2, "probably low risk of bias")
    dfr[columns] = dfr[columns].mask(dfr == 3, "probably high risk of bias")
    dfr[columns] = dfr[columns].mask(dfr == 4, "high risk of bias")
    dfr[columns] = dfr[columns].mask(dfr == 23, "either probably low or probably high risk of bias")
    
    return dfr

def export_df_to_gradeing_sheet(df, writer, sheet_name = "unamed", name_excel = "unamed gradeing"):

    #writer = pd.ExcelWriter(name_excel, engine='xlsxwriter')
    #dfr = df.copy()
    df.to_excel(writer, sheet_name = sheet_name, index = False, startrow = 1)

    #create book and sheet
    workbook = writer.book
    worksheet = writer.sheets[sheet_name]  # pull worksheet 
    
    #hide columns without characteristics atm
    
    worksheet.set_column('G:J', None, None, {'hidden': True})

    rows = len(df.index)
    #formats
    #column width
    format_center = workbook.add_format({'align': 'center', 'valign': 'top'})
    #set widt and alignment of all the columns

    worksheet.set_column(0, 1, 30, format_center)
    worksheet.set_column(2, 3, 13, format_center)
    worksheet.set_column(4, 4, 20, format_center)
    worksheet.set_column(5, 5, 10, format_center)
    worksheet.set_column(10, 15, 25, format_center)
    
    #set title color
    
    treatment_header = workbook.add_format({'align': 'center', 'valign': 'top'})
    treatment_header.set_bg_color('#d9e2f3')
    
    if sheet_name != "Notes":
        
        for i in range(0, len(df.index)):
        
            if type(df.loc[i, "Treatment 1"]) == str:
            
                worksheet.set_row(i + 2, 15, treatment_header)

    #conditional format
    RoB_low_risk = workbook.add_format()
    RoB_low_risk.set_bg_color('#70ad47')

    RoB_prob_low_risk = workbook.add_format()
    RoB_prob_low_risk.set_bg_color('yellow')

    RoB_prob_high_risk = workbook.add_format()
    RoB_prob_high_risk.set_bg_color('#ed7d31')

    RoB_high_risk = workbook.add_format()
    RoB_high_risk.set_bg_color('red')
    
    format3 = workbook.add_format()

    worksheet.conditional_format(2, 10, rows + 2, 15, {'type': 'blanks',
                                                       'stop_if_true': True,
                                                       'format': format3})
    
    worksheet.conditional_format(2, 10, rows + 2, 15, {'type': 'text',
                                                       'criteria': 'begins with',
                                                       'value': 'low risk of bias',
                                                       'format': RoB_low_risk})
    worksheet.conditional_format(2, 10, rows + 2, 15, {'type': 'text',
                                                       'criteria': 'begins with',
                                                       'value': 'probably low risk of bias',
                                                       'format': RoB_prob_low_risk})
    worksheet.conditional_format(2, 10, rows + 2, 15, {'type': 'text',
                                                       'criteria': 'begins with',
                                                       'value': 'probably high risk of bias',
                                                       'format': RoB_prob_high_risk})
    worksheet.conditional_format(2, 10, rows + 2, 15, {'type': 'text',
                                                       'criteria': 'begins with',
                                                       'value': 'either probably low or probably high risk of bias',
                                                       'format': RoB_prob_high_risk})
    worksheet.conditional_format(2, 10, rows + 2, 15, {'type': 'text',
                                                       'criteria': 'begins with',
                                                       'value': 'high risk of bias',
                                                       'format': RoB_high_risk})  
    worksheet.set_zoom(90)

"""Functions for the second task"""