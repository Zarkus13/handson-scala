import p1_1_premiers_pas._

import org.scalatest._
import support.CustomStopper

class HandsOnScala extends Suite {
  override def nestedSuites = List(
  	new e0_vars_vals,
  	new e1_classes,
  	new e2_case_classes
  )


  override def run(testName: Option[String], reporter: Reporter, stopper: Stopper, filter: Filter,
                   configMap: Map[String, Any], distributor: Option[Distributor], tracker: Tracker) {
  	if(!CustomStopper.oneTestFailed)
	  super.run(testName, reporter, CustomStopper, filter, configMap, distributor, tracker)
  }

}