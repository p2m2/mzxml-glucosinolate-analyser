# Spécifications d'intégration : Formats de fichiers mass spectrométriques (mzXML, mzML, xcms)

> **Statut : implémenté et validé.** Ce document décrit l'architecture telle qu'elle a été
> réalisée (et non plus un plan cible) : modèle de données commun, lecteurs mzXML/mzML/xcms,
> intégration dans `ScanLoader`, `MainDetection` et `build.sbt`, ainsi que les points de
> vigilance révélés par les fichiers réels.

## Objectif

Étendre le projet pour prendre en charge les formats de fichiers mass spectrométriques
(mzXML, mzML, et un format simplifié « xcms/CSV ») tout en ne conservant que la fonction
principale `MainDetection` et en nettoyant le code inutile (`MainRdfGenerator`,
`MainClustering`, `BraChemDb`, …).

Toutes les entrées MS passent par une abstraction de lecteur unique
(`fr.inrae.metabolomics.p2m2.fileformats.SpectrumFileReader`), choisie par extension de
fichier via `SpectrumFileReaderFactory`, et converties en un modèle commun `SpectrumData`
(temps de rétention toujours exprimé **en minutes**, pics triés par m/z croissant).

## Architecture globale

### 1. Structure du projet (état final)

#### Fichiers conservés (cœur de l'application) :

- `src/main/scala/fr/inrae/metabolomics/p2m2/MainDetection.scala` — point d'entrée CLI
- `src/main/scala/fr/inrae/metabolomics/p2m2/builder/ScanLoader.scala` — pipeline de détection
- `src/main/scala/fr/inrae/metabolomics/p2m2/builder/IonsIdentificationBuilder.scala`
- `src/main/scala/fr/inrae/metabolomics/p2m2/builder/Peak.scala`, `PeakIdentification.scala`
- `src/main/scala/fr/inrae/metabolomics/p2m2/output/IonsIdentification.scala`
- `src/main/scala/fr/inrae/metabolomics/p2m2/config/ConfigReader.scala`
- `src/main/scala/fr/inrae/metabolomics/p2m2/database/ChemicalUtils.scala`, `Chebi.scala`
- `src/main/scala/fr/inrae/metabolomics/p2m2/diagnostic/DaughterIonsDiag.scala`
- `src/main/scala/fr/inrae/metabolomics/p2m2/export/` — `CandidateResume`, `CsvIonsIdentificationFile`,
  `IonsIdentificationFile`, `FormatIonsIdentification`
- `src/main/resources/` — `default.json` (glucosinolates), `phenolics.json`, `glucosinolate_ChEBI.tsv`,
  `BraChemDB.csv`

#### Nouvelles classes (`src/main/scala/fr/inrae/metabolomics/p2m2/fileformats/`) :

- `SpectrumFileReader.scala` — trait + modèle de données commun (`SpectrumData`, `Scan`, `Peak`, `Metadata`)
- `MzXmlReader.scala`, `MzMLReader.scala`, `XcmsReader.scala` — lecteurs par format
- `SpectrumFileReaderFactory.scala` — sélection du lecteur par extension

#### Fichiers supprimés (nettoyage) :

- `src/main/scala/fr/inrae/metabolomics/p2m2/MainRdfGenerator.scala`
- `src/main/scala/fr/inrae/metabolomics/p2m2/MainClustering.scala`
- `src/main/scala/fr/inrae/metabolomics/p2m2/database/BraChemDb.scala`
- tests associés (`MainRdfGeneratorTest`, `MainClusteringTest`, `BrachemDbTest`,
  `CandidateResumeTest`, `CsvIonsIdentificationFileTest`)

### 2. Modèle de données commun

**Fichier :** `src/main/scala/fr/inrae/metabolomics/p2m2/fileformats/SpectrumFileReader.scala`

```scala
trait SpectrumFileReader {
  def read(file: File): SpectrumData
  def supports(file: File): Boolean
}

case class SpectrumData(scans: Seq[Scan], metadata: Metadata)

case class Scan(num: Int, rt: Double, msLevel: Int, peaks: Seq[Peak])
case class Peak(mz: Double, intensity: Double)

case class Metadata(instrument: String, date: String, fileName: String, path: String = "")
```

Conventions communes aux lecteurs :

- `rt` est toujours en **minutes** ;
- un scan dont les pics sont absents ou vides reste présent, mais les étapes de bruit/détection
  filtrent les scans à vide ;
- `Metadata.path` contient le chemin absolu du fichier source.

### 3. Lecteurs

#### 3.1 mzXML — `MzXmlReader` (msftbx 1.8.8)

- Sélection : extension `.mzxml`.
- Lecture : `MZXMLFile` + `fetchIndex()`, puis fusion des scans MS1 et MS2.
- **Point important** : la structure est d'abord sondée avec `parseScan(num, false)`
  (STRUCTURE_ONLY), puis le spectre complet est **re-parsé** avec `parseScan(num, true)`
  avant `fetchSpectrum()`, comme le faisaient les méthodes historiques de `ScanLoader`.
  Sans ce re-parse, `fetchSpectrum()` renvoie un spectre vide et aucun pic n'est lu.
- Métadonnées : modèle + fabricant des instruments du `RunInfo`.

#### 3.2 mzML — `MzMLReader` (jmzml 1.7.11)

- Sélection : extension `.mzml`.
- Lecture : `MzMLUnmarshaller(file).unmarshall()` puis parcours de `Run.SpectrumList`.
- `extractScan` : ne retient un spectre que si son niveau MS est résolu (1 ou 2), lit les
  pics (triés par m/z croissant), le numéro (index, sinon le dernier nombre entier de l'id)
  et le RT de début (voir points de vigilance).
- Métadonnées : id de la configuration d'instrument par défaut.

Le lecteur gère les différences d'écriture observées entre convertisseurs :

| Point | Comportement |
|---|---|
| Ordre des cvParams des `binaryDataArray` | l'accession m/z (`MS:1000514`) / intensité (`MS:1000515`) est recherchée dans **tous** les cvParams du tableau, pas seulement le premier (ProteoWizard/MsConvert place d'abord le type de données `MS:1000523` et la compression `MS:1000574` zlib) |
| Compression zlib | décodée par jmzML (`getBinaryDataAsNumberArray`) — vérifiée sur fichier MsConvert réel |
| Niveau MS (`MS:1000511`) | recherché sur l'élément `spectrum` **et** dans les `<scan>` ; spectre sans cvParam ms-level et marqué non-MS (UV/vis d'un second contrôleur, `MS:1000804` « electromagnetic radiation spectrum ») → **rejeté** au lieu d'un défaut MS1 |
| RT (`MS:1000016`) | l'unité déclarée (nom ou accession) décide : nom contenant « min » → minutes, sinon conversion secondes → minutes (défaut : secondes) |

#### 3.3 xcms/CSV — `XcmsReader` (simplifié)

- Sélection : extension `.xcms` ou `.csv`.
- Implémentation volontairement simplifiée : lit un CSV `mz,intensity,rt` ligne à ligne et
  émet un scan MS1 par ligne (exemple de structure, à adapter au format xcms exact).

#### 3.4 Factory — `SpectrumFileReaderFactory`

```scala
object SpectrumFileReaderFactory {
  private val readers: Seq[SpectrumFileReader] = Seq(new MzXmlReader(), new MzMLReader(), new XcmsReader())
  def getReaderForFile(file: File): Option[SpectrumFileReader] = readers.find(_.supports(file))
  def getAllReaders: Seq[SpectrumFileReader] = readers
}
```

### 4. Point d'entrée `ScanLoader.readSpectrumData`

**Fichier :** `src/main/scala/fr/inrae/metabolomics/p2m2/builder/ScanLoader.scala`

```scala
def readSpectrumData(f: File): SpectrumData =
  SpectrumFileReaderFactory.getReaderForFile(f) match {
    case Some(reader) => reader.read(f)
    case None         => new MzXmlReader().read(f) // fallback sur le parseur mzXML
  }
```

Les algorithmes de détection de `ScanLoader` (sélection des ions éligibles par motif
isotopique C/S, fusion des m/z similaires, filtre des pics sur-représentés, recherche des
pertes neutres et ions fils) travaillent sur `SpectrumData`. Une API « legacy » mzXML
(`MZXMLFile`/`MZXMLIndex`) est conservée pour compatibilité et couverte par les tests.

#### Estimation du bruit de fond — `calcBackgroundNoisePeak`

- Bruit = intensité moyenne des **pics MS2** du scan (moyenne des moyennes par scan).
- Fenêtre : RT `[0, startDurationTime]` (minutes).
- **Repli (fallback)** : si aucun scan MS2 non vide ne tombe dans la fenêtre (ex. acquisition
  commençant après le warm-up), on utilise **tous** les scans MS2 du run afin de produire une
  estimation non nulle au lieu de `mean = 0.0 / std = NaN`.
- Si aucun MS2 du tout : `mean = 0.0`, `std = NaN` (comportement historique conservé).

### 5. `MainDetection`

**Fichier :** `src/main/scala/fr/inrae/metabolomics/p2m2/MainDetection.scala`

Flux, pour chaque famille du JSON de configuration :

1. Pour chaque fichier d'entrée : si le dump `<input>_<family>` **et** le CSV existent déjà,
   les identifications sont rechargées depuis le dump (pas de re-détection) ; sinon :
   `ScanLoader.readSpectrumData(f)` → estimation du bruit (option `-i` ou `calcBackgroundNoisePeak`
   avec `-w`) → `ionsDetection` (sélection des ions éligibles, fusion m/z, filtre des pics
   sur-représentés, pertes neutres + ions fils) → écriture CSV + dump upickle.
2. Filtre final par score : `scoreIdentification > t` avec
   `t = round((nbNeutralLosses + nbDaughterIons) * 0.1)`.
3. Regroupement par m/z arrondi à 2 décimales et écriture de `resume_<family>.txt`
   (`CandidateResume`).

### 6. `build.sbt`

- `scalaVersion := "2.13.8"` ; l'assembly produit le fat-jar `./assembly/pack.jar`
  (`assembly / target := file("assembly")`, `assembly / assemblyJarName := "pack.jar"`).
- Dépendances principales : `scopt` 4.1.0, `msftbx` 1.8.8, `jmzml` 1.7.11, `ujson`/`upickle`
  2.0.0, `commons-compress` 1.21, `slf4j-simple` 2.0.3, `utest` 0.8.1 (test).
- **jmzml 1.7.11** : exclusion des artefacts legacy JAXB (`jaxb-xjc`, `jaxb-core`) qui
  entrent en collision avec `org.glassfish.jaxb:txw2` tiré par msftbx (besoin de compilation
  de schéma uniquement, pas de runtime).
- Résolveur : EBI Maven repository (HTTP, `withAllowInsecureProtocol(true)`).
- Coverage : seuils stmt 70 % / branch 30 % (désactivables si besoin).

## Points de vigilance révélés par les fichiers réels (corrigés)

1. **mzXML « 0 scans »** : dans certains fichiers, `fetchSpectrum()` sans re-parse ne charge
   aucun pic. Correctif : `parseScan(num, true)` avant `fetchSpectrum()` (comportement legacy).
2. **mzML MsConvert « 0 pics »** : l'ordre des cvParams des `binaryDataArray` place
   type/compression devant l'accession m/z/intensité → `headOption` ne trouvait rien.
   Correctif : recherche de l'accession dans tous les cvParams du tableau.
3. **mzML MsConvert avec 2 contrôleurs** : ~18 000 spectres UV/vis (`controllerType=4`,
   sans `MS:1000511`) étaient classés MS1 par défaut. Correctif : rejet des spectres marqués
   `MS:1000804` sans niveau MS déclaré.
4. **Niveau MS au niveau scan** : certains convertisseurs ne déclarent `MS:1000511` que dans
   `<scan>`. Correctif : recherche aux deux niveaux (`spectrum` + `scan`).
5. **Unités RT** : `MS:1000016` a pour unité par défaut la seconde ; les temps sont convertis
   en minutes selon l'unité déclarée.
6. **Bruit nul dans la fenêtre de warm-up** : acquisition commençant après `-w` →
   repli sur tous les MS2 du run (documenté au §4).

## Tests

`src/test/scala/fr/inrae/metabolomics/p2m2/` (utest) — incluant :

- `fileformats/MzXmlReaderTest`, `MzMLReaderTest`, `XcmsReaderTest`,
  `SpectrumFileReaderFactoryTest` (sélection du lecteur par extension) ;
- `builder/ScanLoaderTest`, `RedundantPeaksTest`, `ScanLoaderDetectNeutralLossesTest`,
  `IonsIdentificationBuilderTest` ;
- `config/ConfigReaderTest`, `database/ChemicalUtilsTest`, `ChebiTest`,
  `diagnostic/DaughterIonsDiagTest`, `output/IonsIdentificationTest`,
  `export/IonsIdentificationFileTest`, `MainDetectionTest`.

Résultat : **49/49 tests passent** avec `sbt clean test assembly`.

## Critères d'acceptation & validation finale

1. **Fonctionnalité** : les trois formats sont supportés ; `sbt clean test assembly` passe.
2. **Compatibilité** : les fichiers mzXML existants continuent de fonctionner (régression du
   « 0 scan » corrigée) ; l'interface CLI (`MainDetection`) est inchangée.
3. **Validations sur fichiers réels** (jar `assembly/pack.jar` reconstruit) :

| Fichier | Résultat |
|---|---|
| `20181018-037.mzXML` (test) | 4496 scans (1124 MS1 + 3372 MS2) ; bruit `mean = 71.37` ; 519 ions après filtre sur-représentés |
| `AppleJuice_LRMS_AD_QC6.mzML` (ThermoRawFileParser, non compressé) | bruit `mean = 1995` ; 187 ions passent le filtre, 0 au-dessus du seuil de score (résultat scientifique attendu pour le jus de pomme) |
| `O24_2704_BO_S_48h_A_15_R1.mzML` (MsConvert, zlib, 28 945 spectres dont ~18 000 UV/vis) | 10 945 spectres MS (999 MS1 + 9946 MS2), UV exclus ; bruit `mean = 27632` ; 8087 ions |

   Exécution combinée `MainDetection 20181018-037.mzXML O24_..._R1.mzML` :
   `resume_Glucosinolate.txt` → **NB FILES 2**, **NB CANDIDATE 470**, **14 ions fils**
   (HSO4⁻, SO4⁻, C6H11O9S, …) — le mzML contribue réellement au résumé.
