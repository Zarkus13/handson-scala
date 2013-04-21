package pas_suivant

import premiers_pas.HandsOnSuiteP1

  /**
  *   On passe aux Listes...
  *
  *   Le point d’entrée des collections en Scala est le trait Iterable.
  *   Les fonctions dite d’ordre supérieur (map, filter, foreach...) que l’on verra par la suite
  *   sont applicables aux classes de l’API Collections.
  *   Les collections sont par défaut immuables, mais il est possible en le précisant d’utiliser
  *   leur forme mutable.
  *   En fait en Scala, on vous encourage toujours à utiliser l’immutabilité (Scala a été codé
  *   dans un soucis de thread-safety).
  *
  *   Une Liste contenant les éléments x1, … , xn est notée List(x1, … , xn).
  *
  *   La classe List repose sur les deux case classes Nil (où Nil représente une liste vide)
  *   et :: (pour info :: se prononce “cons”), où x::xs représente la liste dont le premier
  *   élément est x, suivi d’une autre liste avec le reste des éléments xs.
  *   Les Listes ont une structure récursive, et leurs éléments doivent tous être du même type.
  *
  *   Enfin les Listes disposent d’un ensemble de fonctions assez riches (isEmpty, filter, head, tail...)
  *
  *   ça peut servir : http://www.scala-lang.org/api/current/index.html#scala.collection.immutable.List
  */

class e4_listes extends HandsOnSuiteP1{

  /**
  *   Nil est et restera la liste vide quelque soit le typage de la liste !
  */
  exercice("Les listes Nil, quelque soit leur type sont identiques") {
    val a: List[String] = Nil // Nil est la classe qui représente une liste vide
    val b: List[Int] = Nil // et oui en Scala tout est OBJET !

    (a == Nil) should be(true)
    (b == Nil) should be(true)
    (a == b) should be(true)

  }

  /**
  *   Attention, on a vu cet exemple dans l’exo 2 sur les cases classes !
  */
  exercice("Eq teste l’égalité des références d’objets") {
    val a = List(1, 2, 3)
    val b = List(1, 2, 3)

    (a eq b) should be(false)
  }

  /**
  *  Quelques méthodes utiles à retenir...
  */
  exercice("Les fonctions head et tail") {
    val a = List(1, 3, 5, 7, 9)

    // accès à l’élément 2 de la liste
    a(2) should equal(5)
    // ça prend la tête...
    a.head should equal(1)
    // ah non ça prend tout, sauf la tête
    a.tail should equal(List(3, 5, 7, 9))
    // longueur de la liste
    a.length should equal(5)
    // inverse de la liste
    a.reverse should equal(List(9, 7, 5, 3, 1))
    // converti une liste en String
    a.toString should equal("List(1, 3, 5, 7, 9)")

    // multiplie par 3 chaque élément de la liste
    // La fonction map permet de faire une opération sur chaque élément d’une collection.
    // De plus, map renvoie une copie de la collection appelante en appliquant la fonction
    // f passée en paramètre à chaque élément.
    a.map {v => v * 3} should equal(List(3, 9, 15, 21, 27))

    // conserve tous les multiple de 3
    a.filter {v => v % 3 == 0} should equal(List(3, 9))

    // conserve tous les multiple de 3
    val c = List(1, 2, 5, 8, 9)
    val b = c.filterNot(v => v == 5)
    c should equal(List(1, 2, 5, 8, 9)) // les listes sont immuables par défaut !
    b should equal(List(1, 2, 8, 9))
  }

  /**
  *   L’utilisation de "_" qui désigne tout ce qui n’a pas besoin d’être nommé
  */
  exercice("Les fonctions appliquées aux listes peuvent utiliser '_' ") {
    val a = List(1, 2, 3)
    //ici _ * 2 veut dire i => i * 2
    a.map(_ * 2) should equal(List(2, 4, 6))
    a.filter(_ % 2 != 0) should equal(List(1, 3))
  }
}
