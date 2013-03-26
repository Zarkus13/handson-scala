package recorder

import org.scalatest.{Tag, FunSuite}
import java.io.File
import collection.mutable.ArrayBuffer

trait MyFunSuite extends FunSuite {

  def testPublic(testName: String)(testFun: => Unit) {
   test(testName)(testFun)
  }

}



object MyFunSuite  {

  def prettyShow(source:Array[(String,Int)], errorLine:Int): Array[String] = {
    def intLen(i:Int) = i.toString.length

    val len:Int = 4

    def completewithspace(i:Int):String = {
      (" " * (len - intLen(i)))  + i.toString
    }

    source.map( t => {
      val prefix: String = if(t._2 == errorLine) " ->" else "   "
      prefix + completewithspace(t._2) + " |" + t._1
      })
  }

  def sourceProcessor(source:Array[String]):Array[(String,Int)] = {
    source.zipWithIndex.map( t => (t._1, t._2 +1)) /*.map(t => {
      val prefix: String = if(t._2 == line) " -> " else "    "
      prefix + completewithspace(t._2 + 1) + " |" + t._1
    })*/
  }


  def fileToArray(file:File):Array[String] = {
    scala.io.Source.fromFile(file, "utf-8").getLines().toArray
  }
}