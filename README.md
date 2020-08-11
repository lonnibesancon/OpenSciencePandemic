# OpenSciencePandemic

On this repository, readers, reviewers and co-signers of the article "Open Science Saves Lives: Lessons from the COVID-19 Pandemic" can find the scripts and data for the analysis presented in the article.
This repository is composed of two main parts: "conflict analysis" and "altmetric analysis" which are described below.
The file "PMC_data_parsing.m" is used to produce a list of COVID-19 related articles with their reviewing times. This list is then used to produce the two spreadsheets presented in "conflict_analysis". 

## Conflict analysis
This folder contains a list of all papers accepted in less than a day and a list of all papers accepted in a day (both in several formats).
The conflict analysis was done manually by the authors. We looked for editorial conflicts of interests: we took the authors of each paper and tried to find if they were or not part of the editors of the journals, editor(s) in chief or the editorial board. The results are directly visible in the files along with the type of manuscript.

## Altmetric analysis
This folder contains all the scripts (and the data they generate) for the altmetric analysis of preprints and retracted articles. 
The scripts to find all relevant articles and preprints are found in [TBC] and put the resulting data files in the folder "input" which then used by the python scripts generating all altmetric data.
The python script "start_altmetric.py" should be started with the command "python start_altmetric.py" and relies on the file "altmetric_fetch_all.py"
The resulting output from altmetric queries is available under the folder "output".