package p1_1_premiers_pas

import support.HandsOnSuite

/**
*  Pour définir une classe en Scala, on utilise le mot-clé 'class'. 
*  A l'intérieur d'une classe, on peut définir des champs et des méthodes.
*  Par défaut une classe est 'public' et on a pas besoin de le spécifier. 
*  Jusque là rien d'extraordinaire...
* 
*  Il y a quand même quelques petites particularités : 
*     - il est possible de passer des paramètres val/var dans une classe (voir l'exemple ci-dessous).
*     - les getter/(setter) d'un paramètre passé dans une classe sont générés automatiquement.
* 
*  Tout ceci rend la syntaxe Scala plus concise. Quelques exemples pour vous en convaincre...
*/

  /**
  *   En Java, on aurait
  *
  *     class ClassJava {
  *
  *       final String name;
  *
  *       public String getName() {
  *         return this.name;
  *       }
  *       public void setName(String name) {
  *         this.name = name;
  *       }
  *     }
  */

  /**
  *   Et en Scala
  *
  *     class ClassScala(var name: String)
  */

class e1_classes extends HandsOnSuiteP1{

  // Remarquez que l'on peut se passer des '{}' !
  class ClassWithValParameter(val name: String)
  /**
  * Dans les cas où l'on a un paramètre immuable, on obtient naturellement un getter mais pas de setter
  */
  test("Le paramètre val définit un getter") {
    val aClass = new ClassWithValParameter("name goes here")
    aClass.name should be(__)
  }

  class ClassWithVarParameter(var description: String)
  /**
  * Dans les cas où l'on a un paramètre muable, on obtient naturellement un getter et un setter
  */
  test("Le paramètre var définit un getter et un setter") {
    val aClass = new ClassWithVarParameter("description goes here")
    aClass.description should be(__)

    aClass.description = "new description"
    aClass.description should be(__)
  }

  /**
  * Enfin on peut définir un paramètre privé
  */
  class ClassWithPrivateFields(name: String)

  test("champs privé d'une classe") {
    val aClass = new ClassWithPrivateFields("name")
    // NOTE: aClass.name n'est pas accessible
  }

}
