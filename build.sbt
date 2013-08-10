name := "icfpc2013"

organization:= "kontur.ukko"

scalaVersion := "2.10.2"

resolvers ++= Seq(
	"Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/",
	"spray repo" at "http://repo.spray.io"
	)

libraryDependencies ++= Seq(
	"org.scalatest" % "scalatest_2.10" % "1.9.1" % "test",
	"com.typesafe" % "config" % "1.0.2",
	"org.json4s" %% "json4s-jackson" % "3.2.5",
	"net.databinder.dispatch" %% "dispatch-core" % "0.11.0",
	"org.jboss.netty" % "netty" % "3.2.9.Final"
	)

scalacOptions ++= Seq("-unchecked")

initialCommands := """
  import evaluator._
"""
