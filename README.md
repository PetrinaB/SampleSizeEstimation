# SampleSizeEstimation
the function estimate.samplesize() extracts sample information from an abstract text of a psychological study. In a preceding preprocessing (function perform.preprocessin(), the abstract is prepared by eliminating numbers that are related to bibliographic information, measures, time ... and further words that surely are not indicators to be a sample.

As a required package JATSdecoder has to be loaded.

This repository contains further material for the project of sample estimation. This is:

* csv-files with information about used articles and process of development
* zip-archieves with articles used for implementation that are open access
* R-script files with all nesseccary functions:

csv file with the results of a manuel examination of sample size (seperated by semicolon):
- [samples_step1.csv](https://github.com/PetrinaB/SampleSizeEstimation/files/7017607/samples_step1.csv)

csv file with article abstracts used for training the algorithm (not open access journal, so only doi is provided in this smicolon seperated csv file):
- [samples_step2_training1.csv](https://github.com/PetrinaB/SampleSizeEstimation/files/7017603/samples_step2_training1.csv)

two zip archieves. Both with a set of articles in nxml file format used for further training and evaluation (journal is open access - source is PubMed Central database):
- [samples_step2_training_2.zip](https://github.com/PetrinaB/SampleSizeEstimation/files/7017608/samples_step2_training_2.zip)
- [samples_step3_evaluation.zip](https://github.com/PetrinaB/SampleSizeEstimation/files/7017610/samples_step3_evaluation.zip)


