# nlp-covid

This repository serves as the storage location for files concerning analysis of 
articles in European countries concerning the coronavirus pandemic.

Files and folders in repo:

## R

R folder contains R and RMd files for the analysis
CoronaSentiment contains main content, other files serves for high calculation capacity tasks

## Data

Data contains:
- raw text files from articles after translation by Google Translate
- topic_model and stm_topic_model files from topic_models.R and stm_topic_models.R
- modified bing dictionary by topic

## Dictionary app

Dictionary app serves as a checking tool for the modified Bing dictionary.
In order to check if sentiment added for each word in each topic the application 
should be run.

TODO list

- [x] data files check/delete
- [ ] word_frequency_by_topic.R check
- [ ] cleaning.R