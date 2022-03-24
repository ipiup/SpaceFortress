---
title : "ReadMe - SpaceFortress"
author: "Caroline Hamery - ISAE-SUPAERO"
date: "2020-2021-2022"
output: html_document
---

# Space Fortress : Data extraction, cleaning and analysis.


Required libraries:"stringr","foreach","bestNormalize","SciViews","ggplot2","ggpubr","dplyr","broom"




Main.R file is the main file to use each time you open the SpaceFortress Project. With this file, you'll be able to clean the data and load the cleaned data in long or wide data frames. 

!!If the data are already cleaned, don't run the cleaning again, SF raw file cleaning takes time.

## 1. Extraction on Raw Data

If the SF .txt files are raw, please clean it in order to run further analyses.
The cleaning process will go through the raw .txt file of each game session in the specified folder and extract the required information for analysis. Depending on which analysis you'll run, the parameters of extraction can be changed (i.e ZScores, APM, .. may not be always required). 
To run the data cleaning, run the MAIN.R file and click "No" on the popup window "Are the data already cleaned". The selected files will be cleaned and the cleaned files saved in a chosen folder.
All cleaned files are .txt files, each corresponding to one game session (participant ID and game session are specified in the file as well as file name).

If the raw files are already cleaned, click on "Yes" on the popup "Are the data already cleaned.

All cleaning functions are written in the Clean_SF.R file. Documentation is provided (almost fully).
