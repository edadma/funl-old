import AssemblyKeys._

import LaikaKeys._


name := "FunL"

version := "0.1"

scalaVersion := "2.10.4"

scalacOptions ++= Seq( "-deprecation", "-feature", "-language:postfixOps", "-language:implicitConversions" )

organization := "org.funl-lang"

target := file( "/home/ed/target/" + moduleName.value )

resolvers += Resolver.sonatypeRepo( "snapshots" )

libraryDependencies += "org.scalatest" %% "scalatest" % "2.1.0" % "test"

libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.11.3" % "test"

libraryDependencies += ("org.scala-lang" % "jline" % scalaVersion.value).exclude("org.fusesource.jansi", "jansi")

//libraryDependencies += "org.scala-lang" %% "scala-pickling" % "0.8.0-SNAPSHOT"

//libraryDependencies += "org.scala-lang" % "scala-swing" % scalaVersion.value

//libraryDependencies += "org.ow2.asm" % "asm" % "4.2"

//libraryDependencies += "org.scaloid" %% "scaloid" % "3.2-8"

libraryDependencies ++= Seq(
//	"org.postgresql" % "postgresql" % "9.3-1101-jdbc41",		//9.2-1004-jdbc4"
//	"org.mongodb" % "casbah_2.9.3" % "2.7.0"
	)

libraryDependencies ++= Seq(
	"org.funl-lang" %% "lia" % "0.1",
	"org.funl-lang" %% "indentation-lexical" % "0.1",
	"org.funl-lang" %% "options" % "0.1"
	)

mainClass in (Compile, run) := Some( "funl.interp.ParserTestMain" )
//mainClass in (Compile, run) := Some( "funl.InterpreterMain" )

proguardSettings

ProguardKeys.options in Proguard ++= Seq( "-dontnote", "-dontwarn", "-ignorewarnings", "-optimizations !class/merging/*" )

ProguardKeys.options in Proguard += ProguardOptions.keepMain( "funl.InterpreterMain" )


assemblySettings

mainClass in assembly := Some("funl.InterpreterMain")

jarName in assembly := "funl.jar"


LaikaPlugin.defaults

templateDirectives in Laika += LaikaExtension.bootstrapToc