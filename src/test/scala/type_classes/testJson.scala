package type_classes

import support.HandsOnSuite
import Implicits._

class testJson extends HandsOnSuite {
  exercice("a Json tree should print itself"){
    val jsonString=JsString("json")
    val jsonNumber=JsNumber(int2Integer(10))
    val jsonSequece=JsSeq(Seq(JsString("a"), JsString("b"), JsString("c")))
    val jsonObject=JsObject(Map("string"->jsonString, "number"->jsonNumber, "seq"->jsonSequece))

    val expected:String="""{
      |  "string":"json",
      |  "number":10,
      |  "seq":["a","b","c"]
      |}""".stripMargin

    val actual=jsonObject.toString()

    actual should equal(expected)
  }
  exercice("toJson should correctly convert a String"){
    val expected=JsString("json")
    val actual=Json.toJson("json")

    actual should equal(expected)
  }
  exercice("toJson should correctly convert an Integer"){
    val expected=JsNumber(12)
    val actual=Json.toJson(12)

    actual should equal(expected)
  }
  exercice("toJson should correctly convert a Double"){
    val expected=JsNumber(12.0)
    val actual=Json.toJson(12.0)

    actual should equal(expected)
  }
  exercice("toJson should correctly convert a BigDecimal"){
    val expected=JsNumber(BigDecimal("99999999999999999999999999999999999999999999999999999999999999999999999999"))
    val actual=Json.toJson(BigDecimal("99999999999999999999999999999999999999999999999999999999999999999999999999"))

    actual should equal(expected)
  }
  exercice("toJson should correctly convert a Sequence of strings"){
    val expected=JsSeq(Seq(JsString("a"),JsString("b"),JsString("c")))
    val actual=Json.toJson(Seq("a","b","c"))

    actual should equal(expected)
  }
}
package client {
  case class User(name:String,age:Int,friends:Seq[String])

  object User{
    implicit val userWrite:Writer[User] = Writer { u:User => ??? }
  }

  import User.userWrite
  class testJsonClient extends HandsOnSuite {
    exercice("toJson should correctly convert a user"){
      val user = User("Mathieu", 25, Seq("Jean","Jon","Ludwine"))

      val expected=JsObject(Map("name"->JsString("Mathieu"), "age"->JsNumber(25), "friends"->JsSeq(Seq(JsString("Jean"),JsString("Jon"),JsString("Ludwine")))))
      val actual=Json.toJson(user)

      actual should equal(expected)
    }
  }
}
