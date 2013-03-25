package support

import org.scalatest.{Tag, FunSuite}
import org.scalatest.{Tracker, Stopper, Reporter, FunSuite}
import org.scalatest.matchers.{Matcher, ShouldMatchers}
import org.scalatest.events.{TestPending, TestFailed, TestIgnored, Event, InfoProvided}
import org.scalatest.exceptions.{TestPendingException}

import recorder.{MyTestPendingException, MyTestFailedException, MyException}

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

    //def headerFail = "/!\\ /!\\ /!\\ /!\\ /!\\ /!\\ /!\\ /!\\ /!\\ /!\\ /!\\\n                 TEST FAILED                 \n/!\\ /!\\ /!\\ /!\\ /!\\ /!\\ /!\\ /!\\ /!\\ /!\\ /!\\"
    //def footerFail = "/!\\ /!\\ /!\\ /!\\ /!\\ /!\\ /!\\ /!\\ /!\\ /!\\ /!\\"
    def headerFail =    "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n               TEST FAILED                 \n!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
    def footerFail =    "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
    def headerPending = "*******************************************\n               TEST PENDING                \n*******************************************"
    def footerPending = "*******************************************"

    def sendInfo(header: String, suite: Option[String], test: Option[String], location: Option[String], message: Option[String], context: Option[String], footer: String) {
      header.split("\n").foreach(info(_))
      suite.collect({ case s =>
        info( "Suite    : " + s.replace("\n","") )
      })
      test.collect({ case t =>
        info( "Test     : " + t.replace("\n","") )
      })
      location.collect({ case f =>
        info( "fichier  : " + f.replace("\n","") )
      })
      message.collect({ case m =>
        info("")
        m.split("\n").foreach( info(_) )
      })
      context.collect({ case c =>
        info("")
        c.split("\n").foreach( info(_) )
      })
      info("")
      footer.split("\n").foreach(info(_))
      CustomStopper.testFailed

/*
      failed = true

      (event, exception) match {
        case (_:TestFailed, Some(_:MyTestPendingException)) => printHeaderPendingMessage
        case (e:TestFailed, _) => printHeaderFailMessage
        case _ => printHeaderPendingMessage
      }
      info("")
      info("")
      CustomStopper.testFailed(event).split("\n").map("  " + _ ).foreach(info(_))
      info("")
      exception match {
        case Some(e: MyTestPendingException) => {
          Option(e.getMessage).getOrElse("").split("\n").map( "    " + _).foreach(info(_))
        }
        case Some(e) => {
          info("  => " + e.getMessage + " <= ")
          info("")
          e.getStackTrace.take(5).foreach( _e => info("     " + _e.toString))
        }
        case None =>
      }
      info("")
      info("")


      (event, exception) match {
        case (e:TestFailed, Some(_:MyTestPendingException)) => printFooterPendingMessage
        case (e:TestFailed, _) => printFooterFailMessage
        case _ => printFooterPendingMessage
      }*/
    }

    def apply(event: Event) {
      event match {
        case e: TestFailed => {
          e.throwable match {
            case Some(failure: MyTestFailedException) =>
              val message = Option(failure.getMessage)
              sendInfo(headerFail, Some(e.suiteName), Some(e.testName), failure.fileNameAndLineNumber, message, Some(failure.context), footerFail)
            case Some(pending: MyTestPendingException) =>
              sendInfo(headerPending, Some(e.suiteName), Some(e.testName), None, Some("Vous devez remplacer les __ par les valeurs correctes"), Some(pending.context), footerPending)
            case Some(failure: MyException) =>
              val context:String =  failure.getStackTrace.take(5).mkString("\n")
              sendInfo(headerFail, Some(e.suiteName), Some(e.testName), None, Option(failure.getMessage), Some(failure.context), footerFail)
            case Some(e) => println("something went wrong")
            case None =>
              sendInfo(headerFail, Some(e.suiteName), Some(e.testName), None, None, None, footerFail)
          }

        }
        case e: TestPending => sendInfo(headerPending, Some(e.suiteName), Some(e.testName), None, Some("pending"), None, footerPending)
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