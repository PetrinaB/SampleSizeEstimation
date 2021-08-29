# SampleSizeEstimation
**R-function: estimate.samplesize()**
This function extracts sample information from an abstract text of a psychological study. In a preceding preprocessing, **perform.preprocessing()**, the abstract is prepared by eliminating numbers that are related to bibliographic information, measures, time ... and further words that surely are not indicators to be a sample.

As a dependency, the R-package **JATSdecoder** has to be loaded.

This repository contains further material for the project of sample estimation. This is:

* csv-files with information about used articles and process of development of the estimation algorithm
* zip-archives with open access articles used for implementation

csv file with the results of a manual examination of sample size (semicolon separated):
- [sampStep1.csv](https://github.com/PetrinaB/SampleSizeEstimation/files/7038355/sampStep1.csv)

same csv file 'samples_step1.csv' with assigned cases 1 to 8:
- [sampStep1_ca.csv](https://github.com/PetrinaB/SampleSizeEstimation/files/7038358/sampStep1_ca.csv)

csv file with article abstracts used for training the algorithm (not open access journal, so only doi is provided in this semicolon separated csv file):
- [sampStep2_tr1.csv](https://github.com/PetrinaB/SampleSizeEstimation/files/7038360/sampStep2_tr1.csv)


two zip archives. Both with a set of articles in nxml file format used for further training and evaluation (journal is open access - source is PubMed Central database):
- [sampStep2_tr2.zip](https://github.com/PetrinaB/SampleSizeEstimation/files/7038364/sampStep2_tr2.zip)
- [sampStep3_ev.zip](https://github.com/PetrinaB/SampleSizeEstimation/files/7038368/sampStep3_ev.zip)



