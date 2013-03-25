package recorder

import org.scalatest.{Tag, FunSuite}

trait MyFunSuite extends FunSuite {

  def testPublic(testName: String)(testFun: => Unit) {
   test(testName)(testFun)
  }

}
