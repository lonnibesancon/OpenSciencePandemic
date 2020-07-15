'''
Python script starting all altmetrics queries for all datasets
Authors: Lonni Besançon
Date: 08/07/2020
License: CC-BY
'''

import os

print("1/ Analysis of the retraction watch dataset")
os.system('python altmetric_fetch_all.py input/RetractionWatchCOVID.csv output/altmetricRetractionWatch.tsv')
print("2/ Analysis of the Medrxiv and Biorxiv dataset")
os.system('python altmetric_fetch_all.py input/preprints_medbiorxiv_covid.csv output/altmetricMedBiorxiv.tsv')
print("3/ Analysis of the Arxiv dataset (COVID19)")
os.system('python altmetric_fetch_all.py input/preprints_arxiv_COVID.csv output/altmetricArxivCOVID.tsv')
print("4/ Analysis of the Arxiv dataset (no COVID19)")
os.system('python altmetric_fetch_all.py input/preprints_arxiv_nonCOVID.csv output/altmetricsArxivNonCOVID.tsv')

