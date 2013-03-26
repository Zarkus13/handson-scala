package type_classes

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
object JsObject{
  def apply(properties:(String,JsValue)*):JsObject= ???
}

trait Writer[A]{
  def write(value:A):JsValue
}
object Writer{
  def apply[A]( f: A=>JsValue ):Writer[A]={
    new Writer[A]{
      override def write(value:A)=f(value)
    }
  }
}

object Implicits {
  implicit val stringWriter=Writer { s:String=> ??? }
  implicit val intWriter=Writer { n:Int=> ??? }
  implicit val doubleWriter=Writer { n:Double=> ??? }
  implicit val bigDecimalWriter=Writer { n:BigDecimal=> ??? }
  implicit def seqWriter[B](implicit writer:Writer[B])= Writer { seq:Seq[B] => ??? }
}

object Json{
  def toJson[A](value:A)(implicit writer:Writer[A]):JsValue={
    writer.write(value)
  }
}

