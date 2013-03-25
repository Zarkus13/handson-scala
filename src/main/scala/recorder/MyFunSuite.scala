package recorder

import org.scalatest.{Tag, FunSuite}

trait MyFunSuite extends FunSuite {


  val listener = new TestRecorderListener()

  def testPublic(testName: String)(testFun: => Unit) {
   test(testName)(testFun)
  }

}
