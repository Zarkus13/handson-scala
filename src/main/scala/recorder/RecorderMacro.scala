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

    reify {
        val testExpressionLineStart:Int = context.literal(texts._2).splice

        val testExpressionLineEnd:Int  = context.literal(texts._3).splice

        val content:Array[String] = context.literal(texts._1).splice.split(RecorderMacro.lineSep)

        MyFunSuite.testBody(testName.splice, suite.splice, anchorRecorder.splice)(testFun.splice)(new TestContext(content, testExpressionLineStart, testExpressionLineEnd))
    }
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

    val resultExp = reify {("" + a.splice)}

    context.Expr[Unit](
      Apply(Select(Select(
        context.prefix.tree, newTermName("anchorRecorder")), newTermName("record")), List(aCode.tree, line.tree, resultExp.tree))

    )


  }
}
