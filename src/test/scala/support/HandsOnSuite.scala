package support

import org.scalatest.{Tag, FunSuite}
import org.scalatest.{Tracker, Stopper, Reporter, FunSuite}
import org.scalatest.matchers.{Matcher, ShouldMatchers}
import org.scalatest.events.{TestPending, TestFailed, TestIgnored, Event, InfoProvided}
import org.scalatest.exceptions.{TestPendingException}

trait HandsOnSuite extends FunSuite with ShouldMatchers {
  def __ : Matcher[Any] = {
    throw new TestPendingException
  }


  private class ReportToTheStopper(other: Reporter) extends Reporter {
    var failed = false
    def failure(event: CustomStopper.EventWrapper) {
      failed = true
      info("*****************************************")
      info("*****************************************")
      info("")
      info("")
      info("")
      info(CustomStopper.testFailed(event))
      info("")
      info("")
      info("")
      info("*****************************************")
      info("*****************************************")
    }

    def apply(event: Event) {
      event match {
        case e: TestIgnored => failure(event.asInstanceOf[CustomStopper.EventWrapper])
        case e: TestFailed => failure(event.asInstanceOf[CustomStopper.EventWrapper])
        case e: TestPending => failure(event.asInstanceOf[CustomStopper.EventWrapper])
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