package recorder


import reflect.macros.Context
import org.scalatest.exceptions._
import reflect.internal.Chars

class RecorderMacro[C <: Context](val context: C) {
  import context.universe._

  def apply(testName: context.Expr[String])
           (testFun: context.Expr[Unit])
           (suite: context.Expr[MyFunSuite], anchorRecorder:context.Expr[AnchorRecorder]): context.Expr[Unit] = {

    val texts = getTexts(testFun.tree)

    reify(
      suite.splice.testPublic(testName.splice)({

        lazy val testSourceFile:Array[(String,Int)] =  {

          val content:Array[String] = context.literal(texts._1).splice.split(RecorderMacro.lineSep)

          MyFunSuite.sourceProcessor(content)
        }

        //lazy val fileTotalNumberLine = testSourceFile.size

        val testExpressionLineStart:Int = context.literal(texts._2).splice

        val testExpressionLineEnd:Int  = context.literal(texts._3).splice


        val anchorRecorderAlias = anchorRecorder.splice

        anchorRecorderAlias.reset()


        def anchorsToMessages = {
          "\n\n" + anchorRecorderAlias.records.map(_.toMessage).mkString("\n")
        }


        def ctx(errorLine:Int):String = {
          MyFunSuite.prettyShow(testSourceFile.drop(testExpressionLineStart - 1).take(testExpressionLineEnd - testExpressionLineStart + 2), errorLine).mkString("\n") +  anchorsToMessages
        }
        def errorCtx(errorLine:Int):String = {
          MyFunSuite.prettyShow(testSourceFile.drop(errorLine - 2 ).take(3), errorLine).mkString("\n")
        }

        def completeContext(errorLine:Option[Int]):String =  {
          errorLine.map(i => errorCtx(i) + "\n     ...\n" + ctx(i)).getOrElse("") + anchorsToMessages
        }

        val suitePackage = suite.splice.getClass.getPackage.toString

        try {
          testFun.splice
        } catch {
          case e: TestFailedException => {
            val mes = Option(e.getMessage).getOrElse("")



            val failedCtx = e.failedCodeLineNumber.map(ctx) //completeContext(e.failedCodeLineNumber)

            val location = e.failedCodeFileNameAndLineNumberString.map( suitePackage + java.io.File.separator + _ )
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
                throw new MyNotImplException(mes, Some("\n     ...\n" + errorCtx(notimpl.getLineNumber)  + "\n     ...\n" + ctx(secondLocation)), e, Some(location))
            }
          }
          case e: Throwable => {

            //val ctx = e.getStackTrace.take(7).mkString("\n") //context.literal(getTexts(testFun.tree)).splice
            //val firstStackTrace = e.getStackTrace()(0)

            val firstGoodStackTrace = e.getStackTrace.find(
              st => st.getClassName.contains(suite.splice.getClass.getName)
              )

            val location = firstGoodStackTrace.map( st => suitePackage + java.io.File.separator + st.getFileName + ":" + st.getLineNumber )
            val failContext = firstGoodStackTrace.map( st => ctx(st.getLineNumber)+"\n\n" )
            val myctx = failContext.getOrElse("") + e.getStackTrace.take(7).mkString("\n")
            //val myctx = e.getStackTrace.take(7).mkString("\n")
            //val location = suitePackage + java.io.File.separator + firstStackTrace.getFileName + ":" + firstStackTrace.getLineNumber
            val mes = e.toString + location.map("\n    at " + _) // Option(e.getMessage).getOrElse("")
            throw new MyException(mes, Some(myctx), e, location)

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

    val sourceFile:java.io.File = source.file.file.asInstanceOf[java.io.File]

    //val sourceContent = source.content.map(c => if (chars.isLineBreakChar(c.toChar)) RecorderMacro.lineSep else c.toString()).mkString

    val sourceContent:String =  MyFunSuite.fileToArray(sourceFile).mkString(RecorderMacro.lineSep)
    (sourceContent, lstart, lend)

  }

}


object RecorderMacro {

  lazy val lineSep:String = "-----"

  def apply(context: Context)(testName: context.Expr[String])
           (testFun: context.Expr[Unit])
           (suite: context.Expr[MyFunSuite], anchorRecorder: context.Expr[AnchorRecorder]): context.Expr[Unit] = {

    new RecorderMacro[context.type](context).apply(testName)(testFun)(suite, anchorRecorder)
  }


  def anchor[T: context.WeakTypeTag](context: Context)(a : context.Expr[T]):context.Expr[Unit] = {
    import context.universe._

    val aCode = context.literal(show(a.tree))

    val line = context.literal(a.tree.pos.line)

    val resultExp = reify {
       val result = a.splice
       ("" + result)
    }

    context.Expr[Unit](
      Apply(Select(Select(
        context.prefix.tree, newTermName("anchorRecorder")), newTermName("record")), List(aCode.tree, line.tree, resultExp.tree))

    )


  }
}
