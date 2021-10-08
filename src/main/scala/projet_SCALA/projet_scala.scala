package projet_SCALA
import scala.io.Source
import scala.sys.exit


/* Création de la classe de la tondeuse, elle comprend son orientation vers un point cardinal, ses coordonnées (x,y),
* et dans un but pratique, la taille de la grille */
class Tondeuse(var Ori: Char, var x: Int, var y : Int, val y_grille: Int, val x_grille: Int) {
  var x_ : Int = x
  var y_ : Int = y
  var ori_ : Char = Ori

  /* Fonction qui s'occupe de la commande 'A', cette fonction modifie x ou y en fonction de l'orientation de la tondeuse.
 Celle ci n'enclenche pas de déplacement si celui-ci entraine la tondeuse hors de la grille */
  def deplacement(): Unit ={
    ori_ match {
      case 'N' if y_ < y_grille => y_ = y_ + 1
      case 'S' if y_ > 0 => y_ = y_ - 1
      case 'E' if x_ < x_grille => x_ = x_ + 1
      case 'W' if x_ > 0 => x_ = x_ - 1
      case _ =>
    }
  }

  /* Fonction qui s'occupe de changer l'orientation de la tondeuse en fonction de son orientation initiale et de la commande 'G' ou 'D' */
  def pivot(pivot : Char): Unit ={
    if (pivot=='G'){
      ori_ match {
        case 'N' => ori_ = 'W'
        case 'S' => ori_ = 'E'
        case 'E' => ori_ = 'N'
        case 'W' => ori_ = 'S'
        case _ =>
      }
    }
    else if (pivot=='D'){
      ori_ match {
        case 'N' => ori_ = 'E'
        case 'S' => ori_ = 'W'
        case 'E' => ori_ = 'S'
        case 'W' => ori_ = 'N'
        case _ =>
      }
    }
  }

}

object projet_scala extends App{

  /* Récupération du fichier de commandes depuis les ressources */
  val filename = "commandes"
  val Tab = Source.fromResource(filename).getLines().map(line => line.split("\\*").head).toArray

  /* Traitement de l'erreur: le fichier ne contient pas une ligne pour la taille de la grille et deux lignes par tondeuses */
  if (Tab.length % 2 == 0) {
    println("Commande invalide: vous devez entrer une ligne pour la taille de la grille et pour chaque tondeuse deux lignes (position et commandes)")
    exit()
  }

  /* Traitement de l'erreur: le fichier ne contient pas seulement 2 nombres pour l'initialisation de la grille séparés d'un espace*/
  if (Tab(0).split(" ").length != 2) {
    println("Mauvaise commande d'initialisation de grille, vous devez entrer deux nombres non nuls séparés d'un espace\n")
    exit()
  }

  /* Récupération de la longueur et largeur de la grille (le code ne traite que des grilles de taille 9x9 maximum) */
  var x_grille = Tab(0).split(" ")(0).toInt
  var y_grille = Tab(0).split(" ")(1).toInt

  /* Traitement de l'erreur: le fichier contient pas seulement 2 nombres pour l'initialisation de la grille séparés d'un espace*/
  if (x_grille <= 0 | y_grille <= 0) {
    println("Mauvaise commande d'initialisation de grille, vous devez entrer deux nombres non nuls séparés d'un espace\n")
    exit()
  }

  /* Détermination du nombre de tondeuses entrées par l'utilisateur */
  val nb_Tondeuses = (Tab.length - 1)/2

  /* Parcours de la liste des tondeuses: traitement des commandes et affichage de la position finale pour chaque tondeuse */
  for (i <- 0 until nb_Tondeuses){

    /* Récupération de la ligne de positionnement initial de la tondeuse avec un array des différents arguments de la ligne */
    val ligne_tondeuse = Tab((i*2)+1).split(" ")

    /* Traitement de l'erreur: le fichier à la premiere ligne n'a pas le nombre d'arguments exact de positionnement initial pour chaque tondeuse
    * ou n'entre pas un nombre en premier et deuxieme et un caractère (W, E, S ou N) en troisième */
    if (ligne_tondeuse.length != 3 | !ligne_tondeuse(0).matches("[0-9]+") | !ligne_tondeuse(1).matches("[0-9]+")
    | !ligne_tondeuse(2).matches("[WESN]") ){
      println("Mauvaise commande d'initialisation de positionnement initial de la tondeuse " + (i+1) + "\nRéessayez ")
      exit()
    }

    /* Création des paramètres initiaux des tondeuses (position et orientation de celles-ci) et récupération des commandes de déplacement  */
    val x = ligne_tondeuse(0).toInt
    val y = ligne_tondeuse(1).toInt
    val ori = ligne_tondeuse(2).charAt(0)
    val deplacement = Tab((i*2)+2)

    /* Traitement de l'erreur: au sein du fichier, le positionnement de la tondeuse x ou y dépasse la taille de la grille */
    if (x > x_grille | y > y_grille){
      println("Veuillez saisir un positionnement au sein de la grille pour la tondeuse " + (i+1))
      exit()
    }

    /* Traitement de l'erreur: l'utilisateur entre une commande contenant un autre caractère que A, G ou D */
    if(!deplacement.matches("[AGD]+")){
      println("Les commandes de déplacement doivent être uniquement composées de 'A' (avancer), 'G' (tourner à gauche) ou 'D' (tourner à droite). ")
      exit()
    }

    /* Création de la nouvelle tondeuse depuis la classe définie et des valeurs initialisées précédemment */
    val Tondeuse = new Tondeuse(Ori = ori, x = x, y = y, y_grille = y_grille, x_grille= x_grille)

    /* Boucle traitant les commandes une à une pour chaque lettre de commandes de la tondeuse traitée */
    for (j <- 0 until deplacement.length){
      /* Récupération de la lettre de commande */
      val lettre = deplacement.charAt(j)
      /* Matching de la lettre avec les différentes possibilités A, D ou G */
      lettre match {
        case 'A' => Tondeuse.deplacement()
        case 'D' | 'G' => Tondeuse.pivot(lettre)
        case _ =>
      }
    }
    /* Renvoi la réponse finale du programme avec positionnement et orientation finale de la tondeuse traitée*/
    println("Tondeuse " + (i+1) + " : " + Tondeuse.x_ + " " + Tondeuse.y_ + " " + Tondeuse.ori_ + "\n")
  }
}