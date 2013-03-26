package type_classes

/*
* ici on défini notre arborescence de type json,
* avec string, number, seq et object.
* représentant tous les types json possibles
* et étendant le même trait JsValue
*/
sealed trait JsValue

case class JsString(s:String) extends JsValue{
  override def toString():String= ???
}
case class JsNumber(n:Number) extends JsValue{
  override def toString():String= ???
}
case class JsSeq(seq:Seq[JsValue]) extends JsValue{
  override def toString():String= ???
}
case class JsObject(properties:Map[String, JsValue]) extends JsValue{
  override def toString():String= ???
}

/*
* On crée un objet compagnon permettant de créer un JsObject
* à partir d'une liste de clé valeur de type String -> JsValue
*/
object JsObject{
  def apply(properties:(String,JsValue)*):JsObject= ???
}

/*
* Voici une type class
* C'est un trait générique qui permet de prendre un type A et de retourner
* la JsValue le représentant.
*/
trait Writer[A]{
  def write(value:A):JsValue
}

/*
* l'objet compagnon l'accompagnant sert à créer des writer sans à avoir à spécifier
* le type générique, car il est inféré à la compilation grâce au type de la fonction
* passée en paramètre à apply
*/
object Writer{
  def apply[A]( f: A=>JsValue ):Writer[A]={
    new Writer[A]{
      override def write(value:A)=f(value)
    }
  }
}

/*
* Dans cet objet on défini les writer pour les types qui nous intéresse.
* L'intérêt de les marquer implicit est que si on les importe à un endroit du code
* ils seront présent pour tout de scope du code.
*/
object Implicits {
  implicit val stringWriter=Writer { s:String=> ??? }
  implicit val intWriter=Writer { n:Int=> ??? }
  implicit val doubleWriter=Writer { n:Double=> ??? }
  implicit val bigDecimalWriter=Writer { n:BigDecimal=> ??? }
  implicit def seqWriter[B](implicit writer:Writer[B])= Writer { seq:Seq[B] => ??? }
}

/*
* Enfin le sérialiseur qui prend un objet de type A et implicitement
* un writer de type Writer[A]. Il n'a donc pas besoin d'importer ici
* les writers en question, il suffira de les avoir dans le scope implicit
* du cope appelant.
*/
object Json{
  def toJson[A](value:A)(implicit writer:Writer[A]):JsValue={
    writer.write(value)
  }
}

