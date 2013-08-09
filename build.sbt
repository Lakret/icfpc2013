name := "icfpc2013"

organization:= "kontur.ukko"

scalaVersion := "2.10.2"

resolvers ++= Seq(
	"Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/",
	"spray repo" at "http://repo.spray.io"
	)

libraryDependencies ++= Seq(
	"org.scalatest" % "scalatest_2.10" % "1.9.1" % "test",
	"io.spray" % "spray-client" % "1.2-M8",
	"io.spray" %%  "spray-json" % "1.2.5",
	"com.typesafe.akka" %% "akka-actor"   % "2.2.0-RC1",
	"com.typesafe" % "config" % "1.0.2"
	)

initialCommands := """
  import evaluator._
"""
