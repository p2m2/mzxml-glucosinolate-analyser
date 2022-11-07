# mzxml-glucosinolate-analyser

[![p2m2](https://circleci.com/gh/p2m2/mzxml-glucosinolate-analyser.svg?style=shield)](https://app.circleci.com/pipelines/github/p2m2)
[![codecov](https://codecov.io/gh/p2m2/mzxml-glucosinolate-analyser/branch/develop/graph/badge.svg)](https://codecov.io/gh/p2m2/mzxml-glucosinolate-analyser)

Detection of Ions/Metabolites for Glucosinolates and Phenolics (organism Brassica napus).

[Identification and Quantification of Glucosinolates and Phenolics in a Large Panel of Brassica napus Highlight Valuable Genetic Resources for Chemical Ecology and Breeding](https://pubs.acs.org/doi/10.1021/acs.jafc.1c08118)

## Installation

### assembly

```bash
sbt assembly
```

## run 

### Detection step

mzXML should contains MS1/MS2 spectrum (Data Independent Acquisition)

```shell
java -cp ./assembly/pack.jar fr.inrae.metabolomics.p2m2.MainDetection ./src/test/resources/20181018-037.mzXML
```

## Clustering

```shell
java -cp ./assembly/pack.jar fr.inrae.metabolomics.p2m2.MainClustering 20181018-037_Glucosinolate 20181018-038_Glucosinolate
```


## Chebi Glucosinolate

1) https://www.ebi.ac.uk/chebi/advancedSearchFT.do?searchString=glucosinolate
2) "Download your results"
=>ChEBI_Results.tsv
