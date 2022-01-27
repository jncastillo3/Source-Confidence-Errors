#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Tue Dec 14 18:28:38 2021

@author: nicolecastillo
"""
import pandas as pd
import numpy as np
import glob
root = [insert root]

###############################
# Starns and Ksander - BY ENCODING STRENGTH
###############################
### Experiment 3

# This is only going to use "SData"
newResps = ['z','x','c']
oldResps = ['{,}','{.}','{/}']
femaleResps = ['z','x','c']
maleResps = ['{,}','{.}','{/}']

# For every file in the directory, load the csv. It will throw an error if the file is empty. 
# If this happens on a particular file, raise exception, and skip to next file. 
SK_e3 = []
for i in glob.glob(root+'/Ksander/data_e3/*SData*'):
    try:
        file = pd.read_csv(i, header = None)
        SK_e3.append(file)
    except pd.errors.EmptyDataError:
        print(f'{i} is empty. Did not concatenate.') #SIX subs here with empty data
        continue
    
# Source Accuracy. For every file, and every row, determine whether the source judgement 
# was right or not based on the study source, and possible responses corresponding to that source.
for j in range(0, len(SK_e3)):
    for i in range(0, len(SK_e3[j])):
        file = SK_e3[j]
        if (file.iloc[i,3] == 'F') & (file.iloc[i,7] in femaleResps):
            file.loc[i,10] = 1
        elif (file.iloc[i,3] == 'M') & (file.iloc[i,7] in maleResps):
            file.loc[i,10] = 1
        elif file.iloc[i,3] == 'N':
            file.loc[i,10] = 'nan'
        else:
            file.loc[i,10] = 0  

# Concatenate all the files, and add headers             
SK_e3 = pd.concat(SK_e3)
SK_e3.columns = ['subID', 'Cycle', 'TrialNum','TrialType','Exposure','Recog Resp','Recog RT','Source(M/F)Resp', 'Source RT','Item','SourceAcc']

# Add new columns that have confidence judgments recoded. 
SK_e3['RconfResp'] = SK_e3['Recog Resp']
SK_e3['SconfResp'] = SK_e3['Source(M/F)Resp']
SK_e3 = SK_e3.replace({'RconfResp':{'z':'H', '{/}':'H', 'x':'L', 'c':'L', '{,}':'L', '{.}':'L'}})
SK_e3 = SK_e3.replace({'SconfResp':{'z':'H', '{/}':'H', 'x':'L', 'c':'L', '{,}':'L', '{.}':'L'}})

# Now we determine how many hits and false alarms there are for recognition
# anything "w" or "p" is studied. "n" is new
strengths = ['W','P','N'] #W is only seen at study, P is pre-exposed and studied, N is new
studied = ['W','P']
SK_e3_sourceErr = []
for strength in strengths:
    for sub in np.unique(SK_e3['subID']):
        hitCount = SK_e3[(SK_e3['subID'] == sub) & 
                                 (SK_e3['Exposure'].isin(studied)) & 
                                 (SK_e3['Exposure'] == strength) & 
                                 (SK_e3['Recog Resp'].isin(oldResps))]
        FAcount = SK_e3[(SK_e3['subID'] == sub) & 
                                 (SK_e3['Exposure'] == 'N') & 
                                 (SK_e3['Recog Resp'].isin(oldResps))]
        HChitHCse = len(hitCount[(hitCount['RconfResp'] == 'H') & (hitCount['SourceAcc'] == 0) & (hitCount['SconfResp'] == 'H')])
        LChitHCse = len(hitCount[(hitCount['RconfResp'] == 'L') & (hitCount['SourceAcc'] == 0) & (hitCount['SconfResp'] == 'H')])
        HCFAHCse = len(FAcount[(FAcount['RconfResp'] == 'H') & (FAcount['SconfResp'] == 'H')])
        LCFAHCse = len(FAcount[(FAcount['RconfResp'] == 'L') & (FAcount['SconfResp'] == 'H')])
        HChitLCse = len(hitCount[(hitCount['RconfResp'] == 'H') & (hitCount['SourceAcc'] == 0) & (hitCount['SconfResp'] == 'L')])
        LChitLCse = len(hitCount[(hitCount['RconfResp'] == 'L') & (hitCount['SourceAcc'] == 0) & (hitCount['SconfResp'] == 'L')])
        HCFALCse = len(FAcount[(FAcount['RconfResp'] == 'H') & (FAcount['SconfResp'] == 'L')])
        LCFALCse = len(FAcount[(FAcount['RconfResp'] == 'L') & (FAcount['SconfResp'] == 'L')])
      
        SK_e3_sourceErr.append([sub, 'SK_e3', HChitHCse, LChitHCse, HCFAHCse, LCFAHCse, HChitLCse, LChitLCse, HCFALCse, LCFALCse]) 

# convert to dataFrame
    SK_3_sourceErr = pd.DataFrame(SK_e3_sourceErr, columns=['Sub', 'Study', 'HC hit/HC error', 'LC hit/HC error', 'HC FA/HC error', 'LC FA/HC error', 'HC hit/LC error', 'LC hit/LC error', 'HC FA/LC error', 'LC FA/LC error'])
# Save to file
    SK_3_sourceErr.to_csv(f'SK_ex3_strength_{strength}.csv',index = False)
    SK_e3_sourceErr = []
    
#####################
### SK Experiment 4 - 
#####################
# test 1: Old/new followed by Male/female
# test 2: Bird/Fish source

newResps = ['z','x','c']
oldResps = ['{,}','{.}','{/}']
femaleResps = ['z','x','c']
maleResps = ['{,}','{.}','{/}']

SK_e4 = []
for i in glob.glob(root+f'/Ksander/data_e4/*Test1*'):
    try:
        file = pd.read_csv(i, header = None)
        SK_e4.append(file)
    except pd.errors.EmptyDataError:
        print(f'{i} is empty. Did not concatenate.') #EIGHT empty subs here
        continue

# Keep only complete datasets 
SK_e4 = [SK_e4 for SK_e4 in SK_e4 if SK_e4.shape == (216, 10)]

# Add source Acc
for j in range(0, len(SK_e4)):
    for i in range(0, len(SK_e4[j])):
        file = SK_e4[j]
        if (file.iloc[i,3] == 'W') & (file.iloc[i,7] in femaleResps):
            file.loc[i,10] = 1
        elif (file.iloc[i,3] == 'M') & (file.iloc[i,7] in maleResps):
            file.loc[i,10] = 1
        elif file.iloc[i,3] == 'N':
            file.loc[i,10] = 'nan'
        else:
            file.loc[i,10] = 0
SK_e4_test1 = pd.concat(SK_e4)
SK_e4_test1.columns = ['subID', 'Cycle', 'TrialNum','TrialType','EncodingSeq','Recog Resp','Recog RT','Source Resp', 'Source RT','Item','SourceAcc']

# Add new columns that have confidence judgments recoded. 
SK_e4_test1['RconfResp'] = SK_e4_test1['Recog Resp']
SK_e4_test1['SconfResp'] = SK_e4_test1['Source Resp']
SK_e4_test1 = SK_e4_test1.replace({'RconfResp':{'z':'H', '{/}':'H', 'x':'L', 'c':'L', '{,}':'L', '{.}':'L'}})
SK_e4_test1 = SK_e4_test1.replace({'SconfResp':{'z':'H', '{/}':'H', 'x':'L', 'c':'L', '{,}':'L', '{.}':'L'}})
SK_e4_test1['Strength'] = SK_e4_test1['EncodingSeq']
SK_e4_test1 = SK_e4_test1.replace({'Strength':{'W':'NR', 'M':'NR', 'BBM':'DS', 'BBW':'DS', 'FFM':'DS', 'FFW':'DS', 'WWW':'SS', 'MMM':'SS'}})

studied = ['W','M']
strengths = ['N','NR','DS','SS']
SK_e4_T1_sourceErr = []
for strength in strengths:
    for sub in np.unique(SK_e4_test1['subID']):
        hitCount = SK_e4_test1[(SK_e4_test1['subID'] == sub) & 
                                 (SK_e4_test1['Strength'] == strength) &
                                 (SK_e4_test1['TrialType'].isin(studied)) & 
                                 (SK_e4_test1['Recog Resp'].isin(oldResps))]
        FAcount = SK_e4_test1[(SK_e4_test1['subID'] == sub) & 
                                 (SK_e4_test1['TrialType'] == 'N') & 
                                 (SK_e4_test1['Recog Resp'].isin(oldResps))]
        HChitHCse = len(hitCount[(hitCount['RconfResp'] == 'H') & (hitCount['SourceAcc'] == 0) & (hitCount['SconfResp'] == 'H')])
        LChitHCse = len(hitCount[(hitCount['RconfResp'] == 'L') & (hitCount['SourceAcc'] == 0) & (hitCount['SconfResp'] == 'H')])
        HCFAHCse = len(FAcount[(FAcount['RconfResp'] == 'H') & (FAcount['SconfResp'] == 'H')])
        LCFAHCse = len(FAcount[(FAcount['RconfResp'] == 'L') & (FAcount['SconfResp'] == 'H')])
        HChitLCse = len(hitCount[(hitCount['RconfResp'] == 'H') & (hitCount['SourceAcc'] == 0) & (hitCount['SconfResp'] == 'L')])
        LChitLCse = len(hitCount[(hitCount['RconfResp'] == 'L') & (hitCount['SourceAcc'] == 0) & (hitCount['SconfResp'] == 'L')])
        HCFALCse = len(FAcount[(FAcount['RconfResp'] == 'H') & (FAcount['SconfResp'] == 'L')])
        LCFALCse = len(FAcount[(FAcount['RconfResp'] == 'L') & (FAcount['SconfResp'] == 'L')])
        SK_e4_T1_sourceErr.append([sub, 'SK_e4_T1', HChitHCse, LChitHCse, HCFAHCse, LCFAHCse, HChitLCse, LChitLCse, HCFALCse, LCFALCse]) 
    # convert to dataFrame
    SK_4_sourceErr = pd.DataFrame(SK_e4_T1_sourceErr, columns=['Sub', 'Study', 'HC hit/HC error', 'LC hit/HC error', 'HC FA/HC error', 'LC FA/HC error', 'HC hit/LC error', 'LC hit/LC error', 'HC FA/LC error', 'LC FA/LC error'])
    # Save to file
    SK_4_sourceErr.to_csv(f'SK_ex4_t1_{strength}.csv',index = False)
    SK_e4_T1_sourceErr = []


###############################
# Starns and Pazzaglia
###############################
# Load e2data (experiment 2a - combined)
newResps = ['z','x','c']
oldResps = ['{,}','{.}','{/}']
femaleResps = ['z','x','c']
maleResps = ['{,}','{.}','{/}']

SP_e2 = []
for i in glob.glob(root+f'/Pazzaglia/e2data/*Data_E2_Source_Strength*'):
    try:
        file = pd.read_csv(i, header = None)
        SP_e2.append(file)
    except pd.errors.EmptyDataError:
        print(f'{i} is empty. Did not concatenate.') #ONE empty sub here
        continue
for j in range(0, len(SP_e2)): # Add source Accuracy
    for i in range(0, len(SP_e2[j])):
        file = SP_e2[j]
        if (file.iloc[i,4] == 'F') & (file.iloc[i,8] in femaleResps):
            file.loc[i,11] = 1
        elif (file.iloc[i,4] == 'M') & (file.iloc[i,8] in maleResps):
            file.loc[i,11] = 1
        elif (file.iloc[i,4] == 'L') & (file.iloc[i,6] in oldResps):
            file.loc[i,11] = 0
        elif (file.iloc[i,4] == 'L') & (file.iloc[i,6] in newResps):
            file.loc[i,11] = 'nan'
        else:
            file.loc[i,11] = 0    
SP_e2 = pd.concat(SP_e2)
SP_e2.columns = ['subID', 'Cycle', 'TrialNum','Test','TrialType','Strength', 'Recog Response', 'Recog RT','Source Response','Source RT','Item','SourceAcc']
            
SP_e2['RconfResp'] = SP_e2['Recog Response']
SP_e2['SconfResp'] = SP_e2['Source Response']
SP_e2 = SP_e2.replace({'RconfResp':{'z':'H', '{/}':'H', 'x':'L', 'c':'L', '{,}':'L', '{.}':'L'}})
SP_e2 = SP_e2.replace({'SconfResp':{'z':'H', '{/}':'H', 'x':'L', 'c':'L', '{,}':'L', '{.}':'L'}})

studied = ['F','M']
Strengths = ['S','W','L']
SP_e2_sourceErr = []
for strength in Strengths:
    for sub in np.unique(SP_e2['subID']):
        hitCount = SP_e2[(SP_e2['subID'] == sub) &
                                 (SP_e2['Strength'] == strength) &
                                 (SP_e2['TrialType'].isin(studied)) & 
                                 (SP_e2['Recog Response'].isin(oldResps))]
        FAcount = SP_e2[(SP_e2['subID'] == sub) & 
                                 (SP_e2['TrialType'] == 'L') & 
                                 (SP_e2['Recog Response'].isin(oldResps))]
        HChitHCse = len(hitCount[(hitCount['RconfResp'] == 'H') & (hitCount['SourceAcc'] == 0) & (hitCount['SconfResp'] == 'H')])
        LChitHCse = len(hitCount[(hitCount['RconfResp'] == 'L') & (hitCount['SourceAcc'] == 0) & (hitCount['SconfResp'] == 'H')])
        HCFAHCse = len(FAcount[(FAcount['RconfResp'] == 'H') & (FAcount['SconfResp'] == 'H')])
        LCFAHCse = len(FAcount[(FAcount['RconfResp'] == 'L') & (FAcount['SconfResp'] == 'H')])
        HChitLCse = len(hitCount[(hitCount['RconfResp'] == 'H') & (hitCount['SourceAcc'] == 0) & (hitCount['SconfResp'] == 'L')])
        LChitLCse = len(hitCount[(hitCount['RconfResp'] == 'L') & (hitCount['SourceAcc'] == 0) & (hitCount['SconfResp'] == 'L')])
        HCFALCse = len(FAcount[(FAcount['RconfResp'] == 'H') & (FAcount['SconfResp'] == 'L')])
        LCFALCse = len(FAcount[(FAcount['RconfResp'] == 'L') & (FAcount['SconfResp'] == 'L')])
     
        SP_e2_sourceErr.append([sub, 'SP_e2', HChitHCse, LChitHCse, HCFAHCse, LCFAHCse, HChitLCse, LChitLCse, HCFALCse, LCFALCse]) 
    
    # convert to dataFrame
    SP_sourceErr = pd.DataFrame(SP_e2_sourceErr, columns=['Sub', 'Study', 'HC hit/HC error', 'LC hit/HC error', 'HC FA/HC error', 'LC FA/HC error', 'HC hit/LC error', 'LC hit/LC error', 'HC FA/LC error', 'LC FA/LC error'])
    # Save to file
    SP_sourceErr.to_csv(f'Pazzaglia_E2_{strength}.csv',index = False)
    SP_e2_sourceErr = []

###############################
# Load E3 (experiment 2a) 
###############################

tests = ['RData','SData']
for test in tests:
    SP_e3 = []
    for i in glob.glob(root+f'/Pazzaglia/E3/*{test}*'):
        try:
            file = pd.read_csv(i, header = None)
            SP_e3.append(file)
        except pd.errors.EmptyDataError:
            print(f'{i} is empty. Did not concatenate.') #FOUR empty subs here
            continue
    if test == 'RData':
        SP_E3_recog = pd.concat(SP_e3)
        SP_E3_recog.columns = ['subID', 'Cycle', 'TrialNum','Test','TrialType','Strength', 'Response', 'RT','Item']
    else:
        SP_E3_source = pd.concat(SP_e3)
        SP_E3_source.columns = ['subID', 'Cycle', 'TrialNum','Test','TrialType','Strength', 'Response', 'RT','Item']

SP_E3_recog['RconfResp'] = SP_E3_recog['Response']
SP_E3_source['SconfResp'] = SP_E3_source['Response']
SP_E3_recog = SP_E3_recog.replace({'RconfResp':{'z':'H', '{/}':'H', 'x':'L', 'c':'L', '{,}':'L', '{.}':'L'}})
SP_E3_source = SP_E3_source.replace({'SconfResp':{'z':'H', '{/}':'H', 'x':'L', 'c':'L', '{,}':'L', '{.}':'L'}})

# Reset index
SP_E3_source = SP_E3_source.reset_index()

newResps = ['z','x','c']
oldResps = ['{,}','{.}','{/}']
femaleResps = ['z','x','c']
maleResps = ['{,}','{.}','{/}']

for i in range(0, len(SP_E3_source)):
    if (SP_E3_source.iloc[i,5] == 'F') & (SP_E3_source.iloc[i,7] in femaleResps):
        SP_E3_source.loc[i,11] = 1
    elif (SP_E3_source.iloc[i,5] == 'M') & (SP_E3_source.iloc[i,7] in maleResps):
        SP_E3_source.loc[i,11] = 1
    elif SP_E3_source.iloc[i,5] == 'L':
        SP_E3_source.loc[i,11] = 0
    else:
        SP_E3_source.loc[i,11] = 0
SP_E3_source = SP_E3_source.rename(columns={11: 'SourceAcc'})
        
#main loop for source errors 
studied = ['F','M']
Strengths = ['S','W','L']
SP_e3_sourceErr = []
for strength in Strengths:
    for sub in np.unique(SP_E3_recog['subID']):
        hitCount = SP_E3_recog[(SP_E3_recog['subID'] == sub) & 
                                 (SP_E3_recog['Strength'] == strength) &
                                 (SP_E3_recog['TrialType'].isin(studied)) & 
                                 (SP_E3_recog['Response'].isin(oldResps))]
        FAcount = SP_E3_recog[(SP_E3_recog['subID'] == sub) & 
                                 (SP_E3_recog['TrialType'] == 'L') & 
                                 (SP_E3_recog['Response'].isin(oldResps))]
        HCSourceErr = SP_E3_source[(SP_E3_source['subID'] == sub) & 
                                 (SP_E3_source['SourceAcc'] == 0) &
                                 (SP_E3_source['Strength'] == strength) &
                                 (SP_E3_source['SconfResp'] == 'H')]
        LCSourceErr = SP_E3_source[(SP_E3_source['subID'] == sub) & 
                                 (SP_E3_source['SourceAcc'] == 0) &
                                 (SP_E3_source['Strength'] == strength) &
                                 (SP_E3_source['SconfResp'] == 'L')]
        HChitHCse = len(hitCount[(hitCount['RconfResp'] == 'H') & (hitCount['Item'].isin(HCSourceErr['Item']))])
        LChitHCse = len(hitCount[(hitCount['RconfResp'] == 'L') & (hitCount['Item'].isin(HCSourceErr['Item']))])
        HCFAHCse = len(FAcount[(FAcount['RconfResp'] == 'H') & (FAcount['Item'].isin(HCSourceErr['Item']))])
        LCFAHCse = len(FAcount[(FAcount['RconfResp'] == 'L') & (FAcount['Item'].isin(HCSourceErr['Item']))])
        HChitLCse = len(hitCount[(hitCount['RconfResp'] == 'H') & (hitCount['Item'].isin(LCSourceErr['Item']))])
        LChitLCse = len(hitCount[(hitCount['RconfResp'] == 'L') & (hitCount['Item'].isin(LCSourceErr['Item']))])
        HCFALCse = len(FAcount[(FAcount['RconfResp'] == 'H') & (FAcount['Item'].isin(LCSourceErr['Item']))])
        LCFALCse = len(FAcount[(FAcount['RconfResp'] == 'L') & (FAcount['Item'].isin(LCSourceErr['Item']))])
        SP_e3_sourceErr.append([sub, 'SP_e3', HChitHCse, LChitHCse, HCFAHCse, LCFAHCse, HChitLCse, LChitLCse, HCFALCse, LCFALCse]) 
    
    # convert to dataFrame
    SP_sourceErr = pd.DataFrame(SP_e3_sourceErr, columns=['Sub', 'Study', 'HC hit/HC error', 'LC hit/HC error', 'HC FA/HC error', 'LC FA/HC error', 'HC hit/LC error', 'LC hit/LC error', 'HC FA/LC error', 'LC FA/LC error'])
    # Save to file
    SP_sourceErr.to_csv(f'Pazzaglia_E3_{strength}.csv',index = False)
    SP_e3_sourceErr = []

###############################
# SP EX 4 
###############################

tests = ['RData','SData']
for test in tests:
    SP_e4 = []
    for i in glob.glob(root+f'/Pazzaglia/E4/*{test}*'):
        try:
            file = pd.read_csv(i, header = None)
            SP_e4.append(file)
        except pd.errors.EmptyDataError:
            print(f'{i} is empty. Did not concatenate.')
            continue
    if test == 'RData':
        SP_E4_recog = pd.concat(SP_e4)
        SP_E4_recog.columns = ['subID', 'Cycle', 'TrialNum','Test','TrialType','Strength', 'Response', 'RT','conf','conf RT','Item']
    else:
        SP_E4_source = pd.concat(SP_e4)
        SP_E4_source.columns = ['subID', 'Cycle', 'TrialNum','Test','TrialType','Strength', 'Response', 'RT','conf','conf RT','Item']

SP_E4_recog['RconfResp'] = SP_E4_recog['Response']
SP_E4_source['SconfResp'] = SP_E4_source['Response']
SP_E4_recog = SP_E4_recog.replace({'RconfResp':{1:'H', 6:'H', 2:'L', 3:'L', 4:'L', 5:'L'}})
SP_E4_source = SP_E4_source.replace({'SconfResp':{1:'H', 6:'H', 2:'L', 3:'L', 4:'L', 5:'L'}})

# Reset index
SP_E4_source = SP_E4_source.reset_index()

newResps = [1,2,3]
oldResps = [4,5,6]
femaleResps = [1,2,3]
maleResps = [4,5,6]

for i in range(0, len(SP_E4_source)):
    if (SP_E4_source.iloc[i,5] == 'F') & (SP_E4_source.iloc[i,7] in femaleResps):
        SP_E4_source.loc[i,11] = 1
    elif (SP_E4_source.iloc[i,5] == 'M') & (SP_E4_source.iloc[i,7] in maleResps):
        SP_E4_source.loc[i,11] = 1
    elif SP_E4_source.iloc[i,5] == 'L':
        SP_E4_source.loc[i,11] = 0
    else:
        SP_E4_source.loc[i,11] = 0
SP_E4_source = SP_E4_source.rename(columns={11: 'SourceAcc'})
        
#main loop for source errors 
studied = ['F','M']
Strength = ['S','W','L']
SP_e4_sourceErr = []
for strength in Strength:
    for sub in np.unique(SP_E4_recog['subID']):
        hitCount = SP_E4_recog[(SP_E4_recog['subID'] == sub) &
                                 (SP_E4_recog['Strength'] == strength) &
                                 (SP_E4_recog['TrialType'].isin(studied)) & 
                                 (SP_E4_recog['Response'].isin(oldResps))]
        FAcount = SP_E4_recog[(SP_E4_recog['subID'] == sub) & 
                                 (SP_E4_recog['TrialType'] == 'L') & 
                                 (SP_E4_recog['Response'].isin(oldResps))]
        HCSourceErr = SP_E4_source[(SP_E4_source['subID'] == sub) & 
                                 (SP_E4_source['SourceAcc'] == 0) &
                                 (SP_E4_source['Strength'] == strength) &
                                 (SP_E4_source['SconfResp'] == 'H')]
        LCSourceErr = SP_E4_source[(SP_E4_source['subID'] == sub) & 
                                 (SP_E4_source['SourceAcc'] == 0) &
                                 (SP_E4_source['Strength'] == strength) &
                                 (SP_E4_source['SconfResp'] == 'L')]
        HChitHCse = len(hitCount[(hitCount['RconfResp'] == 'H') & (hitCount['Item'].isin(HCSourceErr['Item']))])
        LChitHCse = len(hitCount[(hitCount['RconfResp'] == 'L') & (hitCount['Item'].isin(HCSourceErr['Item']))])
        HCFAHCse = len(FAcount[(FAcount['RconfResp'] == 'H') & (FAcount['Item'].isin(HCSourceErr['Item']))])
        LCFAHCse = len(FAcount[(FAcount['RconfResp'] == 'L') & (FAcount['Item'].isin(HCSourceErr['Item']))])
        HChitLCse = len(hitCount[(hitCount['RconfResp'] == 'H') & (hitCount['Item'].isin(LCSourceErr['Item']))])
        LChitLCse = len(hitCount[(hitCount['RconfResp'] == 'L') & (hitCount['Item'].isin(LCSourceErr['Item']))])
        HCFALCse = len(FAcount[(FAcount['RconfResp'] == 'H') & (FAcount['Item'].isin(LCSourceErr['Item']))])
        LCFALCse = len(FAcount[(FAcount['RconfResp'] == 'L') & (FAcount['Item'].isin(LCSourceErr['Item']))])
        SP_e4_sourceErr.append([sub, 'SP_e4', HChitHCse, LChitHCse, HCFAHCse, LCFAHCse, HChitLCse, LChitLCse, HCFALCse, LCFALCse]) 
    
    # convert to dataFrame
    SP_sourceErr = pd.DataFrame(SP_e4_sourceErr, columns=['Sub', 'Study', 'HC hit/HC error', 'LC hit/HC error', 'HC FA/HC error', 'LC FA/HC error', 'HC hit/LC error', 'LC hit/LC error', 'HC FA/LC error', 'LC FA/LC error'])
    # Save to file
    SP_sourceErr.to_csv(f'Pazzaglia_E4_{strength}.csv',index = False)
    SP_e4_sourceErr = []

