package recorder

import org.scalatest.{Tag, FunSuite}

trait MyFunSuite extends FunSuite {

  def testPublic(testName: String)(testFun: => Unit) {
   test(testName)(testFun)
  }

}



object MyFunSuite  {


  def sourceProcessor(source:Array[String]):Array[String] = {
    def intLen(i:Int) = i.toString.length


    val len:Int = intLen(source.size)

    def completewithspace(i:Int):String = {
      (" " * (len - intLen(i)))  + i.toString
    }


    source.zipWithIndex.map(t => {
      " " + completewithspace(t._2 + 1) + " |" + t._1
    })
  }
}