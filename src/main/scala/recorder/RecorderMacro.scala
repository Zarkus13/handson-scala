package recorder


import reflect.macros.Context
import org.scalatest.exceptions._
import reflect.internal.Chars

class RecorderMacro[C <: Context](val context: C) {
  import context.universe._

  def apply(testName: context.Expr[String])
           (testFun: context.Expr[Unit])
           (suite: context.Expr[MyFunSuite]): context.Expr[Unit] = {

    val texts = getTexts(testFun.tree)

    reify(
      suite.splice.testPublic(testName.splice)({

        lazy val testSourceFile:Array[String] =  {

          val content:Array[String] = context.literal(texts._1).splice.split(RecorderMacro.lineSep)

          MyFunSuite.sourceProcessor(content)
        }

        val testExpressionLineStart:Int = context.literal(texts._2).splice

        val testExpressionLineEnd:Int  = context.literal(texts._3).splice


        lazy val ctx:String = {
          testSourceFile.drop(testExpressionLineStart).take(testExpressionLineEnd - testExpressionLineStart).mkString("\n")
        }

        def errorCtx(errorLine:Int):String = {
          testSourceFile.drop(errorLine - 2 ).take(3).mkString("\n")
        }

        def completeContext(errorLine:Option[Int]):String =  {
            errorLine.map(i => errorCtx(i) + "\n...\n").getOrElse("") + ctx
        }

        try {
          testFun.splice
        } catch {
          case e: TestFailedException => {
            val mes = Option(e.getMessage).getOrElse("")

            val failedCtx = completeContext(e.failedCodeLineNumber)

            val location = e.failedCodeFileNameAndLineNumberString.map( suite.splice.getClass.getPackage.getName + java.io.File.separator + _ )
            throw new MyTestFailedException(mes, Some(failedCtx), e, location)

          }
          case e: NotImplementedError => {



            val mes = Option(e.getMessage).getOrElse("")
            mes match {
              case "__" =>
                val notimpl = e.getStackTrace()(2)
                val location = suite.splice.getClass.getPackage.getName + java.io.File.separator + notimpl.getFileName + ":" + notimpl.getLineNumber
                throw new MyTestPendingException(mes, Some(completeContext(Some(notimpl.getLineNumber))), e, Some(location))
              case _ =>
                val notimpl = e.getStackTrace()(1)
                val location = suite.splice.getClass.getPackage.getName + java.io.File.separator + notimpl.getFileName + ":" + notimpl.getLineNumber
                throw new MyNotImplException(mes, None, e, Some(location))
            }
          }
          case e: Throwable => {

            val ctx = e.getStackTrace.take(7).mkString("\n") //context.literal(getTexts(testFun.tree)).splice
            val firstStackTrace = e.getStackTrace()(0)
            val location = suite.splice.getClass.getPackage.getName + java.io.File.separator + firstStackTrace.getFileName + ":" + firstStackTrace.getLineNumber
            val mes = e.toString + "\n    at " + location // Option(e.getMessage).getOrElse("")
            throw new MyException(mes, Some(ctx), e, Some(location))

          }
        }


      })
    )
  }


  def getTexts(recording:Tree):(String, Int,Int) = {
    def lines(rec : Tree):(Int,Int)  = {
      rec match {
        case Block(xs, y) => (rec.pos.line, y.pos.line)
        case _ => (rec.pos.line, rec.pos.line)
      }

    }
    val chars = new Chars {}
    val (lstart, lend) = lines(recording)

    val source = recording.pos.source

    val sourceContent = source.content.map(c => if (chars.isLineBreakChar(c.toChar)) RecorderMacro.lineSep else c.toString()).mkString
    (sourceContent, lstart, lend)

  }

}


object RecorderMacro {

  lazy val lineSep:String = "-----"

  def apply(context: Context)(testName: context.Expr[String])
           (testFun: context.Expr[Unit])
           (suite: context.Expr[MyFunSuite]): context.Expr[Unit] = {

    new RecorderMacro[context.type](context).apply(testName)(testFun)(suite)
  }
}
