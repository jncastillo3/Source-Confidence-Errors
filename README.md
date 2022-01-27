# Source-Confidence-Errors
Analysis files for high confidence source errors utilizing behavioral data

*Note that this repository is currently incomplete as this is an ongoing project. 

as of 1/16/2022: Contains preprocessing files and code to clean behavioral data from a series recognition memory tasks that will be used in a bayesian hierarchhical model. Preprocessing requires computing a total count of all source errors made with high and low item confidence across varying levels of encoding strength. This is done across multiple datasets. A total of 12 datasets (utilizing similar task paradigms) were used in the analysis here. Running the 'DataFormat' script yeilds csv files with source error counts for each subject, across each experiment, and each level of encoding strength. 

The main analysis for this study involves a Baysian hierarchical model. The model is used to estimate the proportion of source errors expected at different levels of item confidence. The current script titled "Error Model" is a beginning framework.
