package p5_type_classes

import support.HandsOnSuite

class testJson extends HandsOnSuite {
	test("a Json tree should print itself"){
		val jsonString=JsString("json")
		val jsonNumber=JsNumber(int2Integer(10))
		val jsonSequece=JsSeq(Seq(JsString("a"), JsString("b"), JsString("c")))
		val jsonObject=JsObject(Map("string"->jsonString, "number"->jsonNumber, "seq"->jsonSequece))

    val expected:String="""{
      |  string:"json",
  	  |  number:10,
  	  |  seq:["a","b","c"]
			|}""".stripMargin

		val actual=jsonObject.toString()
		
		actual should equal(expected)
	}
  test("toJson should correctly convert a String"){
    import Implicits._
    val expected=JsString("json")
    val actual=Json.toJson("json")

    actual should equal(expected)
  }
  test("toJson should correctly convert an Integer"){
    import Implicits._
    val expected=JsNumber(12)
    val actual=Json.toJson(12)

    actual should equal(expected)
  }
  test("toJson should correctly convert a Double"){
    import Implicits._
    val expected=JsNumber(12.0)
    val actual=Json.toJson(12.0)

    actual should equal(expected)
  }
  test("toJson should correctly convert a BigDecimal"){
    import Implicits._
    val expected=JsNumber(BigDecimal("99999999999999999999999999999999999999999999999999999999999999999999999999"))
    val actual=Json.toJson(BigDecimal("99999999999999999999999999999999999999999999999999999999999999999999999999"))

    actual should equal(expected)
  }
  test("toJson should correctly convert a Sequence of strings"){
    import Implicits._
    val expected=JsSeq(Seq(JsString("a"),JsString("b"),JsString("c")))
    val actual=Json.toJson(Seq("a","b","c"))

    actual should equal(expected)
  }
}
package client {
  class testJsonClient extends HandsOnSuite {
    case class User(name:String,age:Double,friends:Seq[String])
    object User{
      implicit val userWrite:Writer[User] = Writer { u:User => ??? }
    }
    test("toJson should correctly convert a user"){
      import User.userWrite
      val user = User("theString", 42, Seq("a","b","c"))

      val expected=JsObject(Map("string"->JsString("theString"), "number"->JsNumber(42), "seq"->JsSeq(Seq(JsString("a"),JsString("b"),JsString("c")))))
      val actual=Json.toJson(user)

      actual should equal(expected)
    }
  }
}
