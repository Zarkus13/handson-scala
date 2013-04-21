package premiers_pas

import support.HandsOnSuite

/**
*  Passons aux cases classes...
*
*  En Scala on peut définir des classes un peu particulières avec le mot-clé 'case', et on les
*  appelle des case classes.
*
*  Grâce au mot-clé 'case' devant votre classe, le compilateur va apporter
*  les fonctionnalités suivantes :
*
*     - le constructeur de la classe est rendu implicite
*        => pas besoin de mettre un 'new 'lors de la création d'une nouvelle instance
*     - les implémentations "naturelles" des méthodes equals, toString, hashcode sont rajoutées
*     - les getters deviennent implicites
*     - par défaut, les arguments passés en paramètres de classe sont des 'val' public
*     - ajout d'une méthode copy, pour copier un élément
*
*  Enfin, en plus de rendre le code plus concis, l'un des intérêts des cases classes est son
*  utilisation lors du Pattern Matching (notion que l'on verra plus tard).
*/
class e2_case_classes extends HandsOnSuiteP1{
  /**
  * Création d'une instance d'une case class
  */
  exercice("C'est facile de créer une case classe !!") {
    case class MonChien(nom: String, race: String) // par défaut les paramètres sont des 'val'

    val d1 = MonChien("Scooby", "Doberman")
    val d2 = MonChien("Rex", "Custom")
    val d3 = new MonChien("Scooby", "Doberman") // Cela marche aussi avec new, mais on peut s'en passer !
    val d4 = MonChien.apply("Rex", "Custom") // utilisation de la méthode apply

    (d1 == d3) should be(true)
    (d1 == d2) should be(false)
    (d2 == d3) should be(false)
    (d2 == d4) should be(true)
  }

  /**
  * Les égalités
  */
  exercice("Les case classes ont une méthode equals qui 'marche'") {
    case class Personne(prenom: String, nom: String)

    val p1 = new Personne("Martin", "Odersky")
    val p2 = new Personne("Emmanuel" , "Bernard")
    val p3 = new Personne("Martin", "Odersky")

    // en fait, == en Scala est un appel à .equals de Java
    (p1 == p2) should be(false)
    (p1 == p3) should be(true)

    // eq en Scala correspond à l'égalité par référence, c'est à dire le '==' de Java
    (p1 eq p2) should be(false)
    (p1 eq p3) should be(false)
  }

  /**
  * La méthode hashcode
  */
  exercice("Les case classes ont une méthode hascode qui marche (de base)") {
    case class Personne(prenom: String, nom: String)

    val p1 = new Personne("Iron", "Man")
    val p2 = new Personne("Super", "Man")
    val p3 = new Personne("Iron", "Man")

    (p1.hashCode == p2.hashCode) should be(false)
    (p1.hashCode == p3.hashCode) should be(true)
  }

  /**
  * La méthode toString
  */
  exercice("Les cases classes définissent de base la méthode to String") {
    case class MonChien(nom: String, race: String)
    val d1 = MonChien("Scooby", "Doberman")

    d1.toString should be("MonChien(Scooby,Doberman)")
  }

  /**
  * Les accesseurs
  */
  exercice("Les cases classes définissent automatiquement les accesseurs") {
    case class MonChien(nom: String, race: String)

    val d1 = MonChien("Scooby", "Doberman")
    d1.nom should be("Scooby")
    d1.race should be("Doberman")

    // Que se passe-t-il ?
    //d1.nom = "Scooby Doo"
    // Indice : la réponse se trouve dans le petit pâté du début
  }

  /**
   * les cases classes peuvent avoir des propriétés mutables, mais on n'a pas envie de vous montrer ça ;)
   */
  exercice("On peut modifier les cases classes par copie") {
    case class MonChien(nom: String, race: String)

    val d1 = MonChien("Scooby", "Doberman")

    val d2 = d1.copy(nom = "Scooby Doo") // copie de la classe, mais avec le nom qui change

    d1.nom should be("Scooby") // original est intact (immutabilité)
    d1.race should be("Doberman")

    d2.nom should be("Scooby Doo")
    d2.race should be("Doberman") // les autres propriétés sont copiées de l'original
  }

  /**
  * les cases classes peuvent avoir des paramètres nommés et des paramètres par défaut
  */
  exercice("les cases classes ont des paramètres par défaut et des paramètres nommés") {
    case class Personne(prenom: String, nom: String, age: Int = 0, tel: String = "")

    val p1 = Personne("Sherlock", "Holmes", 23, "06-XX-XX-XX-XX")

    // sans age ni tel
    val p2 = Personne("Doctor", "Watson")

    // l'ordre des paramètres peut changer, et il n'y a toujours pas l'âge
    val p3 = Personne(nom = "Professor", prenom = "Moriarty", tel = "01-XX-XX-XX-XX")

    // une copie avec des paramètres nommés
    val p4 = p3.copy(age = 23)

    p1.prenom should be("Sherlock")
    p1.nom should be("Holmes")
    p1.age should be(23)
    p1.tel should be("06-XX-XX-XX-XX")

    p2.prenom should be("Doctor")
    p2.nom should be("Watson")
    p2.age should be(0)
    p2.tel should be("")

    p3.prenom should be("Moriarty")
    p3.nom should be("Professor")
    p3.age should be(0)
    p3.tel should be("01-XX-XX-XX-XX")

    (p3 == p4) should be(false)
  }
}
