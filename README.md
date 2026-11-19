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
java -cp ./assembly/pack.jar fr.inrae.metabolomics.p2m2.MainDetection ./src/test/resources/20181018-037.mzXML ./src/test/resources/20181018-038.mzXML
```

Options:

| Option | Description | Default |
|---|---|---|
| `-j, --jsonFamilyMetabolitesDetection <file>` | JSON configuration of the metabolite family to detect | `default.json` |
| `-i, --noiseIntensity <x>` | Keep ions above an absolute intensity (else noise is computed on warm-up time) | computed |
| `-w, --warm-up time <sec>` | Warm-up duration used to compute noise | `0.50` |
| `-s, --startRT <x>` | Start retention time (min) | - |
| `-e, --endRT <x>` | End retention time (min) | - |
| `-p, --overrepresentedPeak <n>` | Filter over represented peaks | `800` |
| `-m, --precisionMzh <n>` | Precision/rounded Mz (number of decimal digits) | `1000` |
| `-t, --toleranceMz <x>` | Tolerance accepted | `0.01` |
| `-o, --outputFile <file>` | Output path file | `<input>_<family>` |
| `--verbose` | Verbose mode | - |

## Clustering

```shell
java -cp ./assembly/pack.jar fr.inrae.metabolomics.p2m2.MainClustering 20181018-037_Glucosinolate 20181018-038_Glucosinolate
```


## Chebi Glucosinolate

1) https://www.ebi.ac.uk/chebi/advancedSearchFT.do?searchString=glucosinolate
2) "Download your results"
=>ChEBI_Results.tsv

## RDF

check [model](https://github.com/p2m2/igepp-metabolomics-rdf) .

Generation of Eligible Ion targeting Glucosinolate with Daughter Ion and Neutral Loss

```shell
java -cp ./assembly/pack.jar fr.inrae.metabolomics.p2m2.MainRdfGenerator 20181018-037_Glucosinolate 20181018-038_Glucosinolate
```

Options:

| Option | Description | Default |
|---|---|---|
| `-o, --outputFile <file>` | Output RDF file (`.ttl`, `.jsonld`, `.trig`, `.nt`, `.n3`, `.rdf`) | `export.ttl` |
| `-m, --minScoreThreshold <n>` | Minimal identification score to build the RDF file | `4` |

## Tests

```shell
sbt test
```


