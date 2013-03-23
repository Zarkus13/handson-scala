import p1_1_premiers_pas._
import p1_2_pas_suivant._
import p2_we_need_to_go_deeper._
import p3_cons_et_nil._
import p4_bonus_event_sourcing._

import org.scalatest._
import support.CustomStopper

class HandsOnScala extends Suite {
  override def nestedSuites = List(
  	new e0_vars_vals,
  	new e1_classes,
  	new e2_case_classes,
    new e3_boucle_for,
    new e4_listes,
    new e5_maps,
    new e6_sets,
    new e7_option,
    new e8_fonctions_de_plus_haut_niveau,
    new e0_une_mise_en_abime,
    new e1_un_peu_plus_generique,
    new e3_un_peu_plus_algebrique,
    new e4_on_a_besoin_de_la_covariance,
    new e0_list,
    new e1_bonus_stream,
    new testEventSourcing
  )


  override def run(testName: Option[String], reporter: Reporter, stopper: Stopper, filter: Filter,
                   configMap: Map[String, Any], distributor: Option[Distributor], tracker: Tracker) {
  	if(!CustomStopper.oneTestFailed)
	  super.run(testName, reporter, CustomStopper, filter, configMap, distributor, tracker)
  }
}
