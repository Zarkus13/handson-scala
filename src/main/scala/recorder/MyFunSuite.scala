package recorder

import org.scalatest.{Tag, FunSuite}
import java.io.File
import collection.mutable.ArrayBuffer
import org.scalatest.exceptions.TestFailedException

trait MyFunSuite extends FunSuite {


  implicit val anchorRecorder = new AnchorRecorder()


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
    source.zipWithIndex.map( t => (t._1, t._2 +1))
  }


  def testBody(testName: String, testSuite: MyFunSuite, anchorRecorder: AnchorRecorder)(testFun: => Unit)(context: TestContext) {

    val suite = testSuite


    suite.testPublic(testName)({

      val testExpressionLineStart = context.testStartLine
      val testExpressionLineEnd = context.testEndLine
      val content = context.source

      lazy val testSourceFile: Array[(String, Int)] = {


        MyFunSuite.sourceProcessor(content)
      }

      //lazy val fileTotalNumberLine = testSourceFile.size

      anchorRecorder.reset()


      def anchorsToMessages = {
        "\n\n" + anchorRecorder.records.map(_.toMessage).mkString("\n")
      }


      def ctx(errorLine: Int): String = {
        MyFunSuite.prettyShow(testSourceFile.drop(testExpressionLineStart - 1).take(testExpressionLineEnd - testExpressionLineStart + 2), errorLine).mkString("\n") + anchorsToMessages
      }
      def errorCtx(errorLine: Int): String = {
        MyFunSuite.prettyShow(testSourceFile.drop(errorLine - 2).take(3), errorLine).mkString("\n")
      }

      def completeContext(errorLine: Option[Int]): String = {
        errorLine.map(i => errorCtx(i) + "\n     ...\n" + ctx(i)).getOrElse("") + anchorsToMessages
      }

      val suitePackage = suite.getClass.getPackage.toString

      try {
        testFun
      } catch {
        case e: TestFailedException => {
          val mes = Option(e.getMessage).getOrElse("")



          val failedCtx = e.failedCodeLineNumber.map(ctx) //completeContext(e.failedCodeLineNumber)

          val location = e.failedCodeFileNameAndLineNumberString.map(suitePackage + java.io.File.separator + _)
          throw new MyTestFailedException(mes, failedCtx, e, location)

        }
        case e: NotImplementedError => {


          val mes = Option(e.getMessage).getOrElse("")
          mes match {
            case "__" =>
              val notimpl = e.getStackTrace()(2)
              val location = suitePackage + java.io.File.separator + notimpl.getFileName + ":" + notimpl.getLineNumber
              throw new MyTestPendingException(mes,
                //Some("     ...\n" + completeContext(Some(notimpl.getLineNumber)))
                Some(ctx(notimpl.getLineNumber))
                , e, Some(location))
            case _ =>
              val notimpl = e.getStackTrace()(1)
              val secondLocation = e.getStackTrace()(2).getLineNumber
              val location = suitePackage + java.io.File.separator + notimpl.getFileName + ":" + notimpl.getLineNumber
              throw new MyNotImplException(mes, Some("\n     ...\n" + errorCtx(notimpl.getLineNumber) + "\n     ...\n" + ctx(secondLocation)), e, Some(location))
          }
        }
        case e: Throwable => {

          //val ctx = e.getStackTrace.take(7).mkString("\n") //context.literal(getTexts(testFun.tree)).splice
          //val firstStackTrace = e.getStackTrace()(0)

          val firstGoodStackTrace = e.getStackTrace.find(
            st => st.getClassName.contains(suite.getClass.getName)
          )

          val location = firstGoodStackTrace.map(st => suitePackage + java.io.File.separator + st.getFileName + ":" + st.getLineNumber)
          val failContext = firstGoodStackTrace.map(st => ctx(st.getLineNumber) + "\n\n")
          val myctx = failContext.getOrElse("") + e.getStackTrace.take(7).mkString("\n")
          //val myctx = e.getStackTrace.take(7).mkString("\n")
          //val location = suitePackage + java.io.File.separator + firstStackTrace.getFileName + ":" + firstStackTrace.getLineNumber
          val mes = e.toString + location.map("\n    at " + _) // Option(e.getMessage).getOrElse("")
          throw new MyException(mes, Some(myctx), e, location)

        }
      }

    })
  }
}

class TestContext( laZysource: => Array[String], val testStartLine:Int,val testEndLine:Int)   {
  lazy val source = laZysource
}