# High-Confidence-Source-Errors

Sometimes we feel really confident in things that we're actually wrong about. The following analysis files contain code that help us understand how this can happen. Using behavioral data from memory experiments, this project utilizes a bayesian estimation model to compare distinct theoretical models of decision-making.

1/16/2022: Contains one preprocessing file to clean behavioral data from a series recognition memory tasks that will be used in a bayesian hierarchhical model. Preprocessing requires computing a total count of all source errors made with high and low item confidence across varying levels of encoding strength. This is done across multiple datasets. A total of 12 datasets (utilizing similar task paradigms) were used in the analysis here. Running the 'DataFormat' script yeilds csv files with source error counts for each subject, across each experiment, and each level of encoding strength. 

6/7/2022: 2 "template" pre-processing scripts. These are used to extract relevant details from all of the datafiles that are included in analysis. These can be altered to accomodate correct source judgements, incorrect source judgements, and filter by item confidence as well. 

7/31/2022: final versions of all scripts included here. Added are additional models that aim to discover differences in source errors across levels of memory strength - as opposed to confidence in memory.

The main analysis for this study involves a hierarchical Baysian estimation model. The model is used to estimate the proportion of source errors made with high confidence at ranging levels item confidence. There are a few different versions of this model/code:

- ErrorModel_latest: the most recent working model with code for figures. note that this model assumes that each source is selected equally by participants; this is, symmetric source evidence distributions

- ErrorModel_asym: includes source bias as an added parameter in the model to test for asymmetries that may exists in source judgements

- dprime_model: estimates d'prime at low and high item confidence for correct source judgements 

- StrengthNodsEff: estimates the proportion of source errors made with high confidence across levels of memory strength. This is different from other versions of the model that vary across levels of memory confidence. 
