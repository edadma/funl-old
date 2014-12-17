import AssemblyKeys._

import LaikaKeys._


name := "FunL"

version := "0.17"

scalaVersion := "2.11.4"

scalacOptions ++= Seq( "-deprecation", "-feature", "-language:postfixOps", "-language:implicitConversions", "-language:existentials" )

incOptions := incOptions.value.withNameHashing( true )

organization := "org.funl-lang"

resolvers += "Hyperreal Repository" at "http://hyperreal.ca/maven2"

libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.0" % "test"

libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.11.5" % "test"

libraryDependencies += "jline" % "jline" % "2.11"

libraryDependencies += "com.h2database" % "h2" % "1.3.148"

//libraryDependencies += "org.scala-lang" %% "scala-pickling" % "0.8.0"

//libraryDependencies += "org.scala-lang" % "scala-swing" % scalaVersion.value

//libraryDependencies += "org.ow2.asm" % "asm" % "4.2"

//libraryDependencies += "org.scaloid" %% "scaloid" % "3.2-8"

libraryDependencies ++= Seq(
//	"org.postgresql" % "postgresql" % "9.3-1101-jdbc41",		//9.2-1004-jdbc4"
//	"org.mongodb" % "casbah_2.9.3" % "2.7.0"
	)

libraryDependencies ++= Seq(
	"org.funl-lang" %% "lia" % "0.14",
	"org.funl-lang" %% "indentation-lexical" % "0.2",
	"org.funl-lang" %% "json" % "0.3",
	"ca.hyperreal" %% "options" % "0.1"
	)

mainClass in (Compile, run) := Some( "funl.Main" )

proguardSettings

ProguardKeys.options in Proguard ++= Seq( "-dontnote", "-dontwarn", "-ignorewarnings", "-optimizations !class/merging/*" )

ProguardKeys.options in Proguard += ProguardOptions.keepMain( "funl.Main" )


assemblySettings

mainClass in assembly := Some( "funl.Main" )

jarName in assembly := "funl-dev.jar"


LaikaPlugin.defaults

templateDirectives in Laika += LaikaExtension.bootstrapToc


publishMavenStyle := true

publishTo := Some( Resolver.sftp( "private", "hyperreal.ca", "/var/www/hyperreal.ca/maven2" ) )

publishArtifact in Test := false

pomIncludeRepository := { _ => false }

licenses := Seq("LGPL" -> url("http://opensource.org/licenses/LGPL-3.0"))

homepage := Some(url("https://github.com/FunL/funl"))

pomExtra := (
  <scm>
    <url>git@github.com:FunL/funl.git</url>
    <connection>scm:git:git@github.com:FunL/funl.git</connection>
  </scm>
  <developers>
    <developer>
      <id>edadma</id>
      <name>Edward A. Maxedon, Sr.</name>
      <url>http://funl-lang.org</url>
    </developer>
  </developers>)
