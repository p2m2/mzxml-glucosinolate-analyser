package fr.inrae.metabolomics.p2m2.fileformats

import java.io.File

/**
 * Interface pour les lecteurs de fichiers mass spectrométriques
 */
trait SpectrumFileReader {
  /**
   * Lit un fichier et retourne les données extraites
   * @param file Le fichier à lire
   * @return Les données spectrales
   */
  def read(file: File): SpectrumData
  
  /**
   * Vérifie si ce lecteur peut lire le fichier donné
   * @param file Le fichier à tester
   * @return true si le lecteur supporte ce type de fichier
   */
  def supports(file: File): Boolean
}

/**
 * Données spectrales extraites d'un fichier
 * @param scans Séquence de scans
 * @param metadata Métadonnées du fichier
 */
case class SpectrumData(
  scans: Seq[Scan],
  metadata: Metadata
)

/**
 * Un scan mass spectrométrique
 * @param num Numéro du scan
 * @param rt Temps de rétention (en minutes)
 * @param msLevel Niveau MS (1 ou 2)
 * @param peaks Séquence de pics
 */
case class Scan(
  num: Int,
  rt: Double,
  msLevel: Int,
  peaks: Seq[Peak]
)

/**
 * Pic mass spectrométrique
 * @param mz Rapport m/z
 * @param intensity Intensité
 */
case class Peak(
  mz: Double,
  intensity: Double
)

/**
 * Métadonnées du fichier
 * @param instrument Modèle d'instrument
 * @param date Date d'acquisition
 * @param fileName Nom du fichier
 * @param path Chemin absolu du fichier source
 */
case class Metadata(
  instrument: String,
  date: String,
  fileName: String,
  path: String = ""
)