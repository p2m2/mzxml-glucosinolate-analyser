# mzxml-glucosinolate-analyser

[![p2m2](https://circleci.com/gh/p2m2/mzxml-glucosinolate-analyser.svg?style=shield)](https://app.circleci.com/pipelines/github/p2m2)
[![codecov](https://codecov.io/gh/p2m2/mzxml-glucosinolate-analyser/branch/develop/graph/badge.svg)](https://codecov.io/gh/p2m2/mzxml-glucosinolate-analyser)

Detection of Ions/Metabolites for Glucosinolates and Phenolics (organism *Brassica napus*) from
mass spectrometry data.

[Identification and Quantification of Glucosinolates and Phenolics in a Large Panel of Brassica napus Highlight Valuable Genetic Resources for Chemical Ecology and Breeding](https://pubs.acs.org/doi/10.1021/acs.jafc.1c08118)

## Input file formats

The single detection entry point (`MainDetection`) reads raw MS data through a pluggable reader
abstraction (`fr.inrae.metabolomics.p2m2.fileformats.SpectrumFileReader`), selected by file
extension:

| Format | Selected for | Implementation |
|---|---|---|
| mzXML (MS1 + MS2, Data Independent Acquisition) | `.mzxml` | `MzXmlReader` (msftbx) |
| mzML | `.mzml` | `MzMLReader` (jmzml 1.7.11) |
| XCMS CSV export (columns `mz, intensity, rt`) | `.xcms`, `.csv` | `XcmsReader` (simplified) |

## Installation

Requires JDK 17+ and [sbt](https://www.scala-sbt.org/).

### Build the assembly jar

```bash
sbt assembly
```

This produces the self-contained fat jar `./assembly/pack.jar`.

### Run the tests

```bash
sbt test
```

## Run — Detection step

Input files must contain both MS1 (survey) and MS2 (fragmentation) spectra so that the sulfur
isotopic pattern, neutral losses and daughter ions can be detected.

```shell
java -cp ./assembly/pack.jar fr.inrae.metabolomics.p2m2.MainDetection ./src/test/resources/20181018-037.mzXML ./src/test/resources/20181018-038.mzXML
```

Options:

| Option | Description | Default |
|---|---|---|
| `<file>...` | Input raw files (mzXML / mzML / xcms) | — |
| `-j, --jsonFamilyMetabolitesDetection <file>` | JSON configuration of the metabolite family to detect | `default.json` |
| `-i, --noiseIntensity <x>` | Keep ions above an absolute intensity; when omitted the noise is computed from the warm-up time | computed |
| `-w, --warm-up time <sec>` | Warm-up duration used to compute the noise | `0.50` |

> **Noise estimation.** When `-i` is omitted, the noise intensity is computed as the mean MS2 peak
> intensity over the warm-up window (RT `[0, startDurationTime]`). If no MS2 spectrum falls inside
> that window (e.g. acquisition starts later than the warm-up duration), the whole MS2 scan set of
> the run is used as a fallback so a meaningful non-zero noise estimate is produced instead of 0.
| `-s, --startRT <x>` | Start retention time (min) | — |
| `-e, --endRT <x>` | End retention time (min) | — |
| `-p, --overrepresentedPeak <n>` | Filter out over-represented peaks (peaks found in more than *n* MS1 spectra) | `800` |
| `-m, --precisionMzh <n>` | Precision/rounded Mz (number of digits to the right of the decimal point) | `1000` |
| `-t, --toleranceMz <x>` | Tolerance accepted on m/z | `0.01` |
| `-o, --outputFile <file>` | Output path file | `<input>_<family>` |
| `--verbose` | Verbose mode | — |
| `--help` | Print usage text | — |

### Outputs

For each input file `<input>` and each metabolite `<family>` of the JSON configuration, the pipe
produces:

- `<input>_<family>` — serialized identifications (upickle dump, reused on re-run)
- `<input>_<family>.csv` — identifications as CSV
- `resume_<family>.txt` — candidate summary/resume across all input files

If both the dump and the CSV already exist, detection is skipped for that file and the dump is
reused.

## Configuration

Per-metabolite family detection parameters (isotopic pattern of carbon/sulfur, neutral losses,
daughter ions, minimum m/z of the core structure) are defined in a JSON configuration file — see
`src/main/resources/default.json` for the default glucosinolate/phenolics profiles.

## Glucosinolate reference data

1) https://www.ebi.ac.uk/chebi/advancedSearchFT.do?searchString=glucosinolate
2) "Download your results"
   => `ChEBI_Results.tsv`

## Project structure

- `src/main/scala/fr/inrae/metabolomics/p2m2/MainDetection.scala` — command line entry point
- `src/main/scala/fr/inrae/metabolomics/p2m2/builder/ScanLoader.scala` — scan/picking algorithms
  (legacy mzXML API kept for compatibility with the new `SpectrumData` API)
- `src/main/scala/fr/inrae/metabolomics/p2m2/fileformats/` — `SpectrumFileReader` abstraction and
  the mzXML / mzML / XCMS readers
- `src/main/scala/fr/inrae/metabolomics/p2m2/export/` — CSV, upickle and resume exports
- `src/test/scala/` — test suite (utest)
