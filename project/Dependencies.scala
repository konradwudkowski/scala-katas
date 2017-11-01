import sbt._

object Dependencies {
  lazy val cats       = "org.typelevel"  %% "cats-core"  % "0.9.0"
  lazy val scalaTest  = "org.scalatest"  %% "scalatest"  % "3.0.3"
  lazy val scalaCheck = "org.scalacheck" %% "scalacheck" % "1.13.4"

  lazy val compileDep = List(cats)
  lazy val testDep    = List(scalaTest, scalaCheck).map(_ % "test")
}
