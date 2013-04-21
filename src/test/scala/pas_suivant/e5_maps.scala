package pas_suivant

import premiers_pas.HandsOnSuiteP1

/**
*   Comme dans les autres langages, une map de type Map[Key, Value] est une structure
*   de donnée qui à une clé de type Key associe une valeur de type Value.
*   Les clés ne sont pas obligatoirement du même type.
*
*   Puis aux MAP : http://www.scala-lang.org/api/current/index.html#scala.collection.concurrent.Map
*/

class e5_maps extends HandsOnSuiteP1 {

  /**
  * Création toute bête d’une map et première opérations
  */
  exercice("C’est facile de créer une map !") {
    val myMap = Map("PA" -> "Paris", "BE" -> "Besançon", "BL" -> "Belfort")
    myMap.size should be(3)
    //la tête
    myMap.head should be(("PA", "Paris"))
    // il n’y a pas de notion d’ordre dans une map
    val myMapBis = Map("BE" -> "Besançon", "BL" -> "Belfort", "PA" -> "Paris")
    myMap.equals(myMapBis) should be(true)
    myMap == myMapBis should be(true)

    // impact des 'doublons'
    val myOtherMap = Map("PA" -> "Paris", "BE" -> "Besançon", "PA" -> "Palo Alto")
    myOtherMap.size should be(2)
    myOtherMap("PA") should be("Palo Alto")
  }

  /**
  * L’addition de maps se fait assez naturellement et facilement avec +
  */
  exercice("Addition de map") {
    val myMap = Map("PA" -> "Paris", "BE" -> "Besançon", "NA" -> "Nantes")
    // ajout d’un élément
    val aNewMap = myMap + ("BL" -> "Belfort")

    myMap.contains("BL") should be (false) // les maps sont immuables par défaut
    aNewMap.contains("BL") should be(true)
  }

  /**
  * Les types de clé
  */
  exercice("On peut mixer les types de clé") {
    val myMap = Map("Ann Arbor" -> "MI", 49931 -> "MI")
    myMap("Ann Arbor") should be("MI")
    myMap(49931) should be("MI")
  }

  /**
  * Quelques opérations sur les maps
  */
  exercice("On peut accéder/supprimer les élément d’une map") {
    val myMap = Map("PA" -> "Paris", "BE" -> "Besançon", "NA" -> "Nantes", "BL" -> "Belfort")

    // suppression d’un élément
    val aNewMap = myMap - "NA"
    aNewMap.contains("NA") should be(false)
    // suppressions multiples
    val aNewOtherMap = myMap -- List("BE", "BL")
    aNewOtherMap.contains("BE") should be(false)
    aNewOtherMap.contains("BL") should be(false)
    // une exception est lancée dans le cas où l’élément n’est pas présent dans la map
    intercept[NoSuchElementException] {
      aNewOtherMap("BL") should be("Belfort")
    }
  }

}
