# Glucosinolates and Phenolics Brassica napus

[![p2m2](https://circleci.com/gh/p2m2/mzxml-glucosinolate-analyser.svg?style=shield)](https://app.circleci.com/pipelines/github/p2m2)
[![codecov](https://codecov.io/gh/p2m2/mzxml-glucosinolate-analyser/branch/develop/graph/badge.svg)](https://codecov.io/gh/p2m2/mzxml-glucosinolate-analyser)

## assembly

```bash
sbt assembly
```

## run 

```bash
java -jar ./assembly/pack.jar ./src/test/resources/test.mzXML
```

 - https://www.ebi.ac.uk/chebi/advancedSearchFT.do

[Identification and Quantification of Glucosinolates and Phenolics in a Large Panel of Brassica napus Highlight Valuable Genetic Resources for Chemical Ecology and Breeding](https://pubs.acs.org/doi/10.1021/acs.jafc.1c08118)


``` 
java -jar ./assembly/pack.jar -i 10000  ./src/test/resources/20181018-038.mzXML
java -jar ./assembly/pack.jar -i 10000 -s 3.0 -e 3.5 ./src/test/resources/20181018-038.mzXML
```

## Chebi Glucosinolate

1) https://www.ebi.ac.uk/chebi/advancedSearchFT.do?searchString=glucosinolate
2) "Download your results"
=>ChEBI_Results.tsv