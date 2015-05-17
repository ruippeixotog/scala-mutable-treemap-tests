scalaVersion := "2.11.6"

libraryDependencies ++= Seq("org.scalacheck" %% "scalacheck" % "1.12.2" % "test")

javaOptions in run ++= Seq("-Xmx8g", "-Xms8g", "-Xmn400M")

testOptions in Test += Tests.Argument(TestFrameworks.ScalaCheck,
  "-verbosity", "2", "-minSuccessfulTests", "1000")
