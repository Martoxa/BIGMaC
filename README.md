# BIGMaC
Classification algorithm for depositional environments based on GDGTs. Branched and Isoprenoid GDGT Machine learning Classification algorithm (BIGMaC)
This code is intented to provide the R data format (RDS) file to apply the BIGMaC algorithm (Martinez-Sosa et al., in prep) to lipid data, as well as provide one example on the application of this algorithm. 
The file BIGMaC.RDS provides the R object to run the algorithm, example_BIGMaC.R runs the example using data from Giraffe_example.csv

To run BIGMaC in new data the names of the GDGTs should be in the format Ia, IIa, IIa', GDGT0, Cren, Cren', etc. The data for this columns should be in Fractional Abundance of ALL GDGTs (branched + iso).

To use the algorithm in new data that has been properly formated, and with the dependencies referenced in the example the algorithm should be run as:

## Load BIGMaC
BIGMaC<-readRDS(file('BIGMaC.RDS'))

## Run BIGMaC
predict(BIGMaC,new_data = NEWDATA)

Cite the code: 

[![DOI](https://zenodo.org/badge/470766120.svg)](https://zenodo.org/badge/latestdoi/470766120)


