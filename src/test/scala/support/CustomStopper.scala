package support

import scala.language.reflectiveCalls
import org.scalatest.events.Event
import org.scalatest.Stopper

object CustomStopper extends Stopper{
  var oneTestFailed = false
  override def apply() = oneTestFailed

  type EventWrapper = {
  	val suiteName: String
  	val testName: String
  }

  def testFailed (event: EventWrapper): String = {
    oneTestFailed = true
    meditationMessage(event)
  }

  private def meditationMessage(event: EventWrapper) = {
    "Please fix test \"%s\" of suite \"%s\"" format (event.testName, event.suiteName)
  }
}