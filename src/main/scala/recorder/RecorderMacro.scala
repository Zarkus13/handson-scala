package recorder


import reflect.macros.Context


import reflect.macros.Context

class RecorderMacro[C <: Context](val context: C) {
  import context.universe._

  def apply(testName: context.Expr[String])
           (testFun: context.Expr[Unit])
           (suite: context.Expr[MyFunSuite]): context.Expr[Unit] = {

    val expressions = recordExpressions(testFun.tree)




    reify(
      suite.splice.testPublic(testName.splice)({

        val listener = new TestRecorderListener()

        val recoderRuntime = new RecorderRuntime(listener)

        try {
          testFun.splice
        } catch {
          case e: Exception => {
            val mes = "\n" + Option(e.getMessage).getOrElse("") + "\n\n" +
              context.literal(getTexts(testFun.tree)).splice  +
              "\n\n"


            throw new Exception(mes, e)
          }
        }


      })
    )
  }


  def getTexts(recording:Tree):String = {
    val exps = splitExpressions(recording)
    exps.map(ex => getText(ex)).mkString("\n")

  }

  private[this] def declareRuntime: Tree = {
    val runtimeClass = context.mirror.staticClass(classOf[RecorderRuntime].getName)
    ValDef(
      Modifiers(),
      newTermName("$org_expecty_recorderRuntime"),
      TypeTree(runtimeClass.toType),
      Apply(
        Select(
          New(Ident(runtimeClass)),
          newTermName("<init>")),
        List(
          Select(
            context.prefix.tree,
            newTermName("listener")))))
  }


  private[this] def recordExpressions(recording: Tree): List[Tree] = {
    val exprs = splitExpressions(recording)
    exprs.flatMap { expr =>
      val text = getText(expr)
      val ast = showRaw(expr)
      try {
        List(resetValues, recordExpression(text, ast, expr))
      } catch {
        case e : Throwable => throw new RuntimeException(
          "Expecty: Error rewriting expression.\nText: " + text + "\nAST : " + ast, e)
      }
    }
  }



  private[this] def completeRecording: Tree =
    Apply(
      Select(
        Ident(newTermName("$org_expecty_recorderRuntime")),
        newTermName("completeRecording")),
      List())

  private[this] def resetValues: Tree =
    Apply(
      Select(
        Ident(newTermName("recoderRuntime")),
        newTermName("resetValues")),
      List())

  private[this] def recordExpression(text: String, ast: String, expr: Tree) = {
    val buggedExpr = recordAllValues(expr)
    log(expr, "Expression  : " + text.trim())
    log(expr, "Original AST: " + ast)
    log(expr, "Bugged AST  : " + showRaw(buggedExpr))
    log(expr, "")

    Apply(
      Select(
        Ident(newTermName("$org_expecty_recorderRuntime")),
        newTermName("recordExpression")),
      List(
        context.literal(text).tree,
        context.literal(ast).tree,
        buggedExpr))
  }

  private[this] def splitExpressions(recording: Tree): List[Tree] = recording match {
    case Block(xs, y) => xs.flatMap(splitExpressions) ::: splitExpressions(y)
    case _ => List(recording)
  }

  private[this] def recordAllValues(expr: Tree): Tree = expr match {
    case New(_) => expr // only record after ctor call
    case Literal(_) => expr // don't record
    // don't record value of implicit "this" added by compiler; couldn't find a better way to detect implicit "this" than via point
    case Select(x@This(_), y) if getPosition(expr).point == getPosition(x).point => expr
    case _ => recordValue(recordSubValues(expr), expr)
  }

  private[this] def recordSubValues(expr: Tree) : Tree = expr match {
    case Apply(x, ys) => Apply(recordAllValues(x), ys.map(recordAllValues(_)))
    case TypeApply(x, ys) => recordValue(TypeApply(recordSubValues(x), ys), expr)
    case Select(x, y) => Select(recordAllValues(x), y)
    case _ => expr
  }

  private[this] def recordValue(expr: Tree, origExpr: Tree): Tree =
    if (origExpr.tpe.typeSymbol.isType)
      Apply(
        Select(
          Ident(newTermName("$org_expecty_recorderRuntime")),
          newTermName("recordValue")),
        List(expr, Literal(Constant(getAnchor(origExpr)))))
    else expr

  private[this] def getText(expr: Tree): String = getPosition(expr) match {
    case p: scala.reflect.internal.util.RangePosition =>
      context.echo(expr.pos, "RangePosition found!"); p.lineContent.slice(p.start,p.end)
    case p: Position => p.lineContent

  }

  private[this] def getAnchor(expr: Tree): Int = expr match {
    case Apply(x, ys) => getAnchor(x) + 0
    case TypeApply(x, ys) => getAnchor(x) + 0
    case _ => {
      val pos = getPosition(expr)
      pos.point - pos.source.lineToOffset(pos.line - 1)
    }
  }

  private[this] def getPosition(expr: Tree) = expr.pos.asInstanceOf[scala.reflect.internal.util.Position]

  private[this] def log(expr: Tree, msg: String) {
    context.info(expr.pos, msg, force = false)
  }


}


object RecorderMacro {
  def apply(context: Context)(testName: context.Expr[String])
           (testFun: context.Expr[Unit])
           (suite: context.Expr[MyFunSuite]): context.Expr[Unit] = {

    new RecorderMacro[context.type](context).apply(testName)(testFun)(suite)
  }
}
