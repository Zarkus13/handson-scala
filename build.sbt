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

libraryDependencies += "org.scalatest" %% "scalatest" % "1.9.1" % "test"


addCommandAlias("p1-1", "test-only p1_1*")

addCommandAlias("p1-2", "test-only p1_2*")

addCommandAlias("p2", "test-only p2*")

addCommandAlias("p3", "test-only p3*")

addCommandAlias("go", "~ test-only HandsOnScala")