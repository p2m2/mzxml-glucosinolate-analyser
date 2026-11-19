# Guide de développement — reprise

> État au **19 nov. 2026** — branche `clean-202611`.
> Ce document est le *handover* opérationnel : où en est le travail, comment valider,
> ce qui reste à faire. L'architecture et les pièges corrigés sont détaillés dans
> `SPECIFICATIONS.md` ; l'usage est dans `README.md`.

## 1. État actuel

- **Périmètre** : un seul point d'entrée, `MainDetection` ; lecture MS via
  `fr.inrae.metabolomics.p2m2.fileformats.SpectrumFileReader` (mzXML, mzML, xcms/CSV),
  sélectionnée par extension via `SpectrumFileReaderFactory`.
- **Branche** : `clean-202611`, poussée sur `origin` (`origin/clean-202611`).
- **Commits clés** :
  - `7b8984c` revert: restore original Main* entry point files (état avant le travail)
  - `5cf34c2` fix: robust mzXML/mzML reading and noise estimation; drop stale legacy code
  - `8fb58aa` docs: reflect implemented mzXML/mzML/xcms reader architecture
- **Qualité** : `sbt clean test assembly` passe **49/49 tests**, produit le fat-jar
  `./assembly/pack.jar`.
- **Ce qui marche** : mzXML (reqression 0-scan corrigée), mzML MsConvert/ThermoRawFileParser
  (y compris zlib + spectres UV exclus), traitement de bout en bout sur fichiers réels.

## 2. Commandes

### Build + tests

```bash
sbt clean test assembly      # 49/49 tests, produit ./assembly/pack.jar
```

Nettoyer le dossier de sortie (reuse de dump) avant une exécution « à froid » :

```bash
rm -f 20181018-037_Glucosinolate 20181018-037_Glucosinolate.csv \
      O24_2704_BO_S_48h_A_15_R1_Glucosinolate O24_2704_BO_S_48h_A_15_R1_Glucosinolate.csv \
      resume_Glucosinolate.txt
```

### Exécution de référence (2 fichiers, comme l'utilisateur)

```bash
java -Xmx4g -cp ./assembly/pack.jar fr.inrae.metabolomics.p2m2.MainDetection \
  ./src/test/resources/20181018-037.mzXML \
  ../../P2M2/MZXML/O24_2704_BO_S_48h_A_15_R1.mzML
```

Utiliser `-Xmx4g` pour les gros mzML (plus de 1,5 M de pics MS1).

## 3. Validation de référence (résultats attendus)

| Fichier | Origine | Scans (MS1/MS2) | Bruit | Ions après filtre over-repr. |
|---|---|---|---|---|
| `src/test/resources/20181018-037.mzXML` | mzXML (test) | 4496 (1124/3372) | `mean = 71.37` | 519 |
| `../../P2M2/MatchingFeaturesLRHR/output_convert/AppleJuice_LRMS_AD_QC6.mzML` | mzML ThermoRawFileParser, non compressé | — | `mean = 1995` | 187 (score > t : **0**) |
| `../../P2M2/MZXML/O24_2704_BO_S_48h_A_15_R1.mzML` | mzML MsConvert, zlib, 28 945 spectra dont ~18 000 UV/vis | 10 945 (999/9946) | `mean = 27632` | 8087 |

Résultat combiné attendu dans `resume_Glucosinolate.txt` :

- `NB FILES 2` (`O24_2704_BO_S_48h_A_15_R1.mzML, 20181018-037.mzXML`)
- `NB CANDIDATE 470` · `NB DAUGHTER IONS 14`
  (C2H3OS⁻(407), HSO4⁻_97(387), C6H11O9S_259(328), SO4⁻_95(320), C6H11O7S⁻_227(265),
  C6H11O5S⁻_195(264), C6H11O8S2_275(229), HO4S2⁻_128(144), C2H4O5NS⁻_153(127),
  C2H3O5S⁻_138(115), C6H11O2⁻_153(101), C2H2O4S⁻_135(98), C6H9NO8S_241(77), C6H10O8S⁻_241(37))

À vérifier aussi pendant le run : pas de `mean = 0.0 std = NaN` (bruit), `MS2 with empty peaks: 0`.

**Piège** : `AppleJuice` avec **0 ion au-dessus du seuil** est un **résultat scientifique
attendu** (jus de pomme : dans l'échantillon traité, tous les ions survivants ont un
score `≤ t = round((14di+7nl)*0.1) = 2`), pas un bug de détection.

## 4. Limites connues / TODO

1. **`XcmsReader` est un placeholder** : lit un CSV `mz,intensity,rt` (1 scan MS1/ligne).
   À adapter au format xcms réel, ou le retirer de la factory si non utilisé.
2. **API « legacy » mzXML** : `ScanLoader` conserve des overloads
   `MZXMLFile`/`MZXMLIndex` (lignes ~62–219 du fichier) pour compatibilité. Rationaliser
   une fois le chemin `SpectrumData` entièrement couvert par les tests.
3. **Tests lecteurs peu profonds** : `MzMLReaderTest`/`XcmsReaderTest` ne testent que
   `supports()`. Les exigences réelles (cvParam order, UV, RT) sont validées manuellement
   via les fichiers de validation (§3) mais pas par des fixtures committées.
4. **`-w` est en minutes** (RT interne en minutes) : `0.50` = 30 s. Ne pas le documenter en secondes.
5. **Fichiers de travail non commités** : les dumps `<input>_Glucosinolate`,
   `.csv`, `resume_*.txt` en racine sont des artefacts d'exécution (aucun `.gitignore`
   ne les couvre) — les supprimer avant une re-validation propre.
6. **jmzML 1.7.11** nécessite les exclusions JAXB dans `build.sbt` (`jaxb-xjc`, `jaxb-core`)
   pour cohabiter avec msftbx — ne pas les retirer sans retester l'assembly.

## 5. Journal des décisions

- **2026-11-19 — mzXML 0-scan** : `fetchSpectrum()` sur un scan parsé en STRUCTURE_ONLY
  renvoie un spectre vide sur certains fichiers. Correctif : re-parser
  `parseScan(num, true)` avant `fetchSpectrum()` (comportement legacy `ScanLoader`).
  → mzXML `20181018-037` : 4496 scans (1124 MS1 + 3372 MS2).
- **2026-11-19 — mzML MsConvert 0 pic** : le diagnostic (dev `DevInspect`, supprimé)
  a montré que l'accession m/z/intensité n'est pas le premier cvParam des
  `binaryDataArray` chez ProteoWizard (type de données `MS:1000523` et compression
  `MS:1000574` d'abord). Correctif : chercher `MS:1000514`/`MS:1000515` dans **tous**
  les cvParams. (Diagnostiqué sur `O24_2704_BO_S_48h_A_15_R1.mzML`.)
- **2026-11-19 — spectres UV/vis** : le même fichier entrelace ~18 000 spectres d'un
  second contrôleur (`controllerType=4`, `MS:1000804`, sans `MS:1000511`) → ils étaient
  classés MS1 par défaut (`getOrElse(1)`). Correctif : rejeter un spectre marqué
  `MS:1000804` et sans niveau MS ; défaut MS1 conservé sinon.
- **2026-11-19 — niveau MS au niveau scan** : `MS:1000511` peut n'être déclaré que dans
  `<scan>` ; recherché aux deux niveaux (spectrum + scan).
- **2026-11-19 — unités RT** : `MS:1000016` a pour défaut la seconde ; conversion en
  minutes selon l'unité déclarée (nom contenant « min » → minutes, sinon ÷60).
- **2026-11-19 — bruit nul (warm-up vide)** : si la fenêtre de warm-up n'a aucun MS2,
  repli sur **tous** les MS2 du run (sinon `mean = 0.0 / std = NaN`). Comportement
  documenté dans le README sous `-w` ; `mean=0/NaN` conservé uniquement si aucun MS2 du tout.
- **2026-11-19 — périmètre du code** : suppression de `MainClustering`,
  `MainRdfGenerator`, `BraChemDb` + tests associés (commit `5cf34c2`) ; tous les calculs
  passent par `SpectrumData`.
- **2026-11-19 — AppleJuice 0 ion** : investigué (scans/ pics MS2 présents aux RT des
  candidats) ; conclusion : résultat scientifique correct, pas un bug.

## 6. Pourquoi pas encore fait / prochaines pistes

- Fiabiliser `XcmsReader` ou le retirer (TODO n°1) ;
- ajouter des tests de lecture réels (fixtures mzML zlib / UV / ms-level scan) ;
- évaluer la suppression de l'API legacy mzXML de `ScanLoader`.

## 7. Références

- `README.md` — usage CLI, options, formats ;
- `SPECIFICATIONS.md` — architecture implémentée + points de vigilance détaillés ;
- `src/main/scala/fr/inrae/metabolomics/p2m2/fileformats/` — lecteurs ;
- `src/main/scala/fr/inrae/metabolomics/p2m2/builder/ScanLoader.scala` — pipeline.
