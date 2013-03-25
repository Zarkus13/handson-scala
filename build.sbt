scalaVersion := "2.10.0"


scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature")

traceLevel := -1

logLevel := Level.Info

// disable printing timing information, but still print [success]
showTiming := false

// disable printing a message indicating the success or failure of running a task
showSuccess := false

offline := true

retrieveManaged := true

libraryDependencies += "org.scala-lang" % "scala-reflect" % "2.10.0"

libraryDependencies += "org.scala-lang" % "scala-compiler" % "2.10.0"

libraryDependencies += "org.scalatest" %% "scalatest" % "1.9.1"


addCommandAlias("p1-1", "test-only partie_1_1")

addCommandAlias("p1-2", "test-only partie_1_2")

addCommandAlias("p2", "test-only partie_2")

addCommandAlias("p3", "test-only partie_3")

addCommandAlias("p4", "test-only partie_4")

addCommandAlias("go", "~ test-only HandsOnScala")