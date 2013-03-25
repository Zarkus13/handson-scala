package support

import org.scalatest.{Tag, FunSuite}
import org.scalatest.{Tracker, Stopper, Reporter, FunSuite}
import org.scalatest.matchers.{Matcher, ShouldMatchers}
import org.scalatest.events.{TestPending, TestFailed, TestIgnored, Event, InfoProvided}
import org.scalatest.exceptions.{TestPendingException}



import language.experimental.macros

import recorder.MyFunSuite
import recorder.RecorderMacro


trait HandsOnSuite extends MyFunSuite with ShouldMatchers {
  def __ : Matcher[Any] = {
    throw new TestPendingException
  }

  implicit val suite:MyFunSuite = this


  /*class RecorderWrapper(suite:HandsOnSuite) extends (Unit => Unit) {
    def apply(testFun: Unit):Unit = macro RecorderMacro.apply
  }  */

  def exercice(testName:String)(testFun: Unit)(implicit suite: MyFunSuite):Unit = macro RecorderMacro.apply



  /*override protected def test(testName: String, tags: Tag*)(testFun: => Unit):Unit


  = macro RecorderMacro.apply  */

  private class ReportToTheStopper(other: Reporter) extends Reporter {
    var failed = false
    def failure(event: Event, exception: Option[Throwable]) {
      failed = true
      event match {
        case e:TestFailed => 
          info("/!\\ /!\\ /!\\ /!\\ /!\\ /!\\ /!\\ /!\\ /!\\ /!\\ /!\\")
          info("               TEST FAILED                 ")
        case _ =>
          info("*******************************************")
          info("               TEST PENDING                ")
      }
      info("")
      info("")
      CustomStopper.testFailed(event).replace("\n","\n ").split("\n").foreach(info(_))
      info("")
      exception match {
        case Some(e) => {
          info("  => " + e.getMessage + " <= ")
          info("")
          e.getStackTrace.take(5).foreach( _e => info("     " + _e.toString))
        }
        case None =>
      }
      info("")
      info("")
      event match {
        case e:TestFailed => 
          info("/!\\ /!\\ /!\\ /!\\ /!\\ /!\\ /!\\ /!\\ /!\\ /!\\ /!\\")
        case _ =>
          info("*******************************************")
      }
    }

    def apply(event: Event) {
      event match {
        case e: TestFailed => 
          failure(e, e.throwable)
        case e: TestPending => 
          failure(event, None)
        case _ => other(event)
      }

    }
  }

  protected override def runTest(testName: String, reporter: Reporter, stopper: Stopper, configMap: Map[String, Any], tracker: Tracker) {
    if (!CustomStopper.oneTestFailed) {
      super.runTest(testName, new ReportToTheStopper(reporter), CustomStopper, configMap, tracker)
    }
  }


}



object HandsOnSuite {
  object partie1 extends Tag("support.partie1")
  object partie2 extends Tag("support.partie2")
}