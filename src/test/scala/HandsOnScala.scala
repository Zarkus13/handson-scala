import p1_1_premiers_pas._
import p1_2_pas_suivant._
import p2_we_need_to_go_deeper._
import p3_cons_et_nil._
import p4_bonus_event_sourcing._
import p5_type_classes._

import org.scalatest._
import support.CustomStopper

class HandsOn extends Suite {


  override def run(testName: Option[String], reporter: Reporter, stopper: Stopper, filter: Filter,
                   configMap: Map[String, Any], distributor: Option[Distributor], tracker: Tracker) {
  	if(!CustomStopper.oneTestFailed)
	  super.run(testName, reporter, CustomStopper, filter, configMap, distributor, tracker)
  }
}

class HandsOnScala extends HandsOn {
  override def nestedSuites = List(
    new partie_1_1,
    new partie_1_2,
    new partie_2,
    new partie_3,
    new partie_4,
    new partie_5
  )
}

class partie_1_1 extends HandsOn {
  override def nestedSuites = List(
    new e0_vars_vals,
    new e1_classes,
    new e2_case_classes,
    new e3_boucle_for
  )
}

class partie_1_2 extends HandsOn {
  override def nestedSuites = List(
    new e4_listes,
    new e5_maps,
    new e6_sets,
    new e7_option,
    new e8_fonctions_de_plus_haut_niveau,
    new e9_extracteurs_et_patterns
  )
}

class partie_2 extends HandsOn {
  override def nestedSuites = List(
    new e0_une_mise_en_abime,
    new e1_un_peu_plus_generique,
    new e3_un_peu_plus_algebrique,
    new e4_on_a_besoin_de_la_covariance
  )  
}

class partie_3 extends HandsOn {
  override def nestedSuites = List(
    new e0_list,
    new e1_bonus_stream
  )  
}

class partie_4 extends HandsOn {
  override def nestedSuites = List(
    new testEventSourcing
  )  
}

class partie_5 extends HandsOn {
  override def nestedSuites = List(
    new testJson,
    new client.testJsonClient
  )  
}