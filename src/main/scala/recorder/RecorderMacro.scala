package support.recorder


import reflect.macros.Context

object RecorderMacro {
  def apply(context: Context)
           (testName: context.Expr[String])
           (testFun: context.Expr[Unit])
           (suite: context.Expr[MyFunSuite]): context.Expr[Unit] = {


    import context.universe._

    reify(
       suite.splice.testPublic(testName.splice)({

         testFun.splice


       })
    )
  }
}