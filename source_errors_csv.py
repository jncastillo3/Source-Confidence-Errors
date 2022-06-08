#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Mon Mar 21 12:30:01 2022

@author: nicolecastillo
"""
import pandas as pd
import numpy as np

root = "/Users/nicolecastillo/Documents/dataFiles_masters/SourceErrorConfidence/"
FO_1a = pd.read_csv("/Users/nicolecastillo/Documents/dataFiles_masters/SourceErrorConfidence/FoxOsth/raw/Fox and Osth (2021) - Data (Exp. 1a).csv")
FO_3a = pd.read_csv("/Users/nicolecastillo/Documents/dataFiles_masters/SourceErrorConfidence/FoxOsth/raw/Fox and Osth (2021) - Data (Exp. 3a).csv")

#Source Incorrect
sourceErr = []
se = []
newResps = [7,8,9]
oldResps = [1,2,3]
HCresp = [1,9]
LCresp = [2,3,7,8]
s = ["left","right"]

for sub in np.unique(FO_3a.loc[:,"subj"]):
    for source in s: 
        recog_subset = FO_3a.loc[(FO_3a['subj'] == sub) & (FO_3a['cond'] == "nonBlocked") & (FO_3a['task'] == "recog") & (FO_3a['source'] == source)] # cond - recogBlocked and task - recog
        source_subset = FO_3a.loc[(FO_3a['subj'] == sub) & (FO_3a['cond'] == "nonBlocked") & (FO_3a['task'] == "source") & (FO_3a['source'] == source)]
        
        hitCount = recog_subset[(recog_subset['type'] == 'target') & (recog_subset['response'].isin(oldResps))]
        FAcount = recog_subset[(recog_subset['type'] == 'lure') & (recog_subset['response'].isin(oldResps))]
        HCSourceErr = source_subset[(source_subset['corr'] == 0) & (source_subset['response'].isin(HCresp))]
        LCSourceErr = source_subset[(source_subset['corr'] == 0) & (source_subset['response'].isin(LCresp))]
        
        HChitHCse = len(hitCount[(hitCount['response'].isin(HCresp)) & (hitCount['item'].isin(HCSourceErr['item']))])
        LChitHCse = len(hitCount[(hitCount['response'].isin(LCresp)) & (hitCount['item'].isin(HCSourceErr['item']))])
        HCFAHCse = len(FAcount[(FAcount['response'].isin(HCresp)) & (FAcount['item'].isin(HCSourceErr['item']))])
        LCFAHCse = len(FAcount[(FAcount['response'].isin(LCresp)) & (FAcount['item'].isin(HCSourceErr['item']))])
        HChitLCse = len(hitCount[(hitCount['response'].isin(HCresp)) & (hitCount['item'].isin(LCSourceErr['item']))])
        LChitLCse = len(hitCount[(hitCount['response'].isin(LCresp)) & (hitCount['item'].isin(LCSourceErr['item']))])
        HCFALCse = len(FAcount[(FAcount['response'].isin(HCresp)) & (FAcount['item'].isin(LCSourceErr['item']))])
        LCFALCse = len(FAcount[(FAcount['response'].isin(LCresp)) & (FAcount['item'].isin(LCSourceErr['item']))])
        
        if source == "left":
            sourceErr.append([sub, 11, HChitHCse, LChitHCse, HCFAHCse, LCFAHCse, HChitLCse, LChitLCse, HCFALCse, LCFALCse]) 
        else:
            sourceErr.append([HChitHCse, LChitHCse, HCFAHCse, LCFAHCse, HChitLCse, LChitLCse, HCFALCse, LCFALCse]) 
            x = sourceErr[0] + sourceErr[1]
            se.append(x)
            sourceErr = []
        
sourceErr_df = pd.DataFrame(se, columns=['Sub', 'Study', 'HChit_HCE_S1', 'LChit_HCE_S1', 'HCFA_HCE_S1', 'LCFA_HCE_S1', 'HChit_LCE_S1', 'LChit_LCE_S1', 'HCFA_LCE_S1', 'LCFA_LCE_S1', 'HChit_HCE_S2', 'LChit_HCE_S2', 'HCFA_HCE_S2', 'LCFA_HCE_S2', 'HChit_LCE_S2', 'LChit_LCE_S2', 'HCFA_LCE_S2', 'LCFA_LCE_S2'])
sourceErr_df.to_csv(root+'FoxOsth_3a_simultaneous_asymSources.csv',index = False)

#Source Correct 
sourceCorr = []
newResps = [7,8,9]
oldResps = [1,2,3]
HCresp = [1,9]
LCresp = [2,3,7,8]
s = ["left","right"]
se = []

for sub in np.unique(FO_3a.loc[:,"subj"]):
    for source in s: 
        recog_subset = FO_3a.loc[(FO_3a['subj'] == sub) & (FO_3a['cond'] == "nonBlocked") & (FO_3a['task'] == "recog") & (FO_3a['source'] == source)] # cond - recogBlocked and task - recog
        source_subset = FO_3a.loc[(FO_3a['subj'] == sub) & (FO_3a['cond'] == "nonBlocked") & (FO_3a['task'] == "source") & (FO_3a['source'] == source)]
        
        hitCount = recog_subset[(recog_subset['type'] == 'target') & (recog_subset['response'].isin(oldResps))]
        FAcount = recog_subset[(recog_subset['type'] == 'lure') & (recog_subset['response'].isin(oldResps))]
        HCSourceErr = source_subset[(source_subset['corr'] == 1) & (source_subset['response'].isin(HCresp))]
        LCSourceErr = source_subset[(source_subset['corr'] == 1) & (source_subset['response'].isin(LCresp))]
        
        HChitHCse = len(hitCount[(hitCount['response'].isin(HCresp)) & (hitCount['item'].isin(HCSourceErr['item']))])
        LChitHCse = len(hitCount[(hitCount['response'].isin(LCresp)) & (hitCount['item'].isin(HCSourceErr['item']))])
        HCFAHCse = len(FAcount[(FAcount['response'].isin(HCresp)) & (FAcount['item'].isin(HCSourceErr['item']))])
        LCFAHCse = len(FAcount[(FAcount['response'].isin(LCresp)) & (FAcount['item'].isin(HCSourceErr['item']))])
        HChitLCse = len(hitCount[(hitCount['response'].isin(HCresp)) & (hitCount['item'].isin(LCSourceErr['item']))])
        LChitLCse = len(hitCount[(hitCount['response'].isin(LCresp)) & (hitCount['item'].isin(LCSourceErr['item']))])
        HCFALCse = len(FAcount[(FAcount['response'].isin(HCresp)) & (FAcount['item'].isin(LCSourceErr['item']))])
        LCFALCse = len(FAcount[(FAcount['response'].isin(LCresp)) & (FAcount['item'].isin(LCSourceErr['item']))])
        
        if source == "left":
            sourceCorr.append([sub, 11, HChitHCse, LChitHCse, HCFAHCse, LCFAHCse, HChitLCse, LChitLCse, HCFALCse, LCFALCse]) 
        else:
            sourceCorr.append([HChitHCse, LChitHCse, HCFAHCse, LCFAHCse, HChitLCse, LChitLCse, HCFALCse, LCFALCse]) 
            x = sourceCorr[0] + sourceCorr[1]
            se.append(x)
            sourceCorr = []
            
sourceCorr_df = pd.DataFrame(se, columns=['Sub', 'Study', 'HChit_HCC_S1', 'LChit_HCC_S1', 'HCFA_HCC_S1', 'LCFA_HCC_S1', 'HChit_LCC_S1', 'LChit_LCC_S1', 'HCFA_LCC_S1', 'LCFA_LCC_S1', 'HChit_HCC_S2', 'LChit_HCC_S2', 'HCFA_HCC_S2', 'LCFA_HCC_S2', 'HChit_LCC_S2', 'LChit_LCC_S2', 'HCFA_LCC_S2', 'LCFA_LCC_S2'])
sourceCorr_df.to_csv(root+'FoxOsth_3a_corr_simultaneous_asymSources.csv',index = False)       
        
