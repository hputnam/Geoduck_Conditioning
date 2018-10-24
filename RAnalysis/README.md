#Geoduck (_Panopea generos_) Conditioning to OA

This repository includes data and analysis scripts to accompany: (manuscript title pending)

Authors: Gurr, Samujel J., Putnam, Hollie M., Roberts, S., Vadopalas, B.

Funding: This work was funded through a grant from the Foundation for Food and Agriculture Research (_Development of environmental conditioning practices to decrease impacts of climate change on shellfish aquaculture_) awarded to Steven Roberts (University of Washington) and Hollie Putnam (University of Rhode Island)

Journal:

Link:

Description: This repository provides data and scripts to analyze the influence of conditining of juvenile geoduck to ocean acidification. We report our measurements on standard metabolic rate and shell length as a reponse to repeated exposure to OA trials

Contents: RAnalysis file containing three folders (Scripts, Data, Output) and a README.md file.

Scripts:
(1) Final summary
"Stats_all": R script that imports all data and runs all analyses for the maniscript and generates all figures and tables
Stats_all summarizes the following three scripts: Stats_chemsitry_tables, Stats_resp_analysis, Stats_size_analysis
(2) Respirometry data
(2a) Import = "SDR_LoLinR" + "SDR_LoLin_individually": R scripts use the package LoLin to obtain oxygen consumption rates in monotomic exmpirical data from multiple local linear regressions. "SDR_LoLin_individually" was used to run each PreSens vial individual to alter constant function criteria for optimal regression distrubutions. "SDR_LoLinR" ran all resp trial automatically based on 9 constant criteria for alpha and truncation. Output csv. files named by constants (found here: /Geoduck_Conditioning/RAnalysis/Data/SDR_data/All_data_csv/Cumulative_output)
(2b)Bank correction and standardization of standard metabolic rate = "SDR_resprate_calc": R script calls each ouput csv. (cumulative reference and automated ouputs in 2a), loops a blank correct and standardizes all values to mean size and geoduck counts (sampleID_number_size.csv). Ouputs cumulative dataframe "All_resp_calc_and_standardized.csv"
(2c) Analysis = "Stats_resp_analysis": R script compared automated outputs to reference with local polynomial and linear regression to choose the optimal LoLin constants represenative of the data. Runs linear mixed effects models on intial and secondary OA trials, outputs anova tables and graphs. (summarized script in "Stats_all")
(3) Shell length
"Stats_size_analysis": R script runs linear mixed effects models on treatment and time (random factor) from data in "All_groth_data.csv" (from ImageJ analysis)
this script is summarized in "Stats_all"
(4) Chemistry
(4a) Extract and and ouput conituous and discrete measures for cumulative table
"Apex_Data_Extract": extracts the continous data from the apex controller
"TotalAlk_wParsing": calculated TA for CRMs and samples - ouputs csv in TA folder for each data "Cumulatice_TA_Output.csv"
"pH_Tris_Curve": runs linear regression, r^2 >0.98 validates handheld discrete pH measurements
"CarbChem": R script calls "Cumulatice_TA_Output.csv", daily tris data, and daily discrete measurements (filled by hand in "Daily_Temp_pH_Sal.csv") to calculate complete carbonate chemistry. In Ouputs folder - "Seawater_chemistry_table_Output_All.csv", "Daily_Treatment_Measures.pdf", and "Water_Chem_withTA.pdf"
(4b) Analysis Daily Seawater chemistry
"Stats_chemistry_tables": R script calls "Seawater_chemistry_table_Output_All.csv" and ouputs mean SE tables for carbonate chemistry in the ouputs folder for OA_Exp1, OA_Exp2, and All_OA_Exp

Data:

Output: The directory containing the analysis results, figures, tables, and supplementary material.
