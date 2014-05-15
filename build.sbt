import AssemblyKeys._

import LaikaKeys._


name := "FunL"

version := "0.4-SNAPSHOT"

scalaVersion := "2.11.0"

scalacOptions ++= Seq( "-deprecation", "-feature", "-language:postfixOps", "-language:implicitConversions" )

incOptions := incOptions.value.withNameHashing( true )

organization := "org.funl-lang"

resolvers += Resolver.sonatypeRepo( "snapshots" )

libraryDependencies += "org.scalatest" %% "scalatest" % "2.1.3" % "test"

libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.11.3" % "test"

libraryDependencies += "jline" % "jline" % "2.11"

//libraryDependencies += "org.scala-lang" %% "scala-pickling" % "0.8.0-SNAPSHOT"

//libraryDependencies += "org.scala-lang" % "scala-swing" % scalaVersion.value

//libraryDependencies += "org.ow2.asm" % "asm" % "4.2"

//libraryDependencies += "org.scaloid" %% "scaloid" % "3.2-8"

libraryDependencies ++= Seq(
//	"org.postgresql" % "postgresql" % "9.3-1101-jdbc41",		//9.2-1004-jdbc4"
//	"org.mongodb" % "casbah_2.9.3" % "2.7.0"
	)

libraryDependencies ++= Seq(
	"org.funl-lang" %% "lia" % "0.1-SNAPSHOT",
	"org.funl-lang" %% "indentation-lexical" % "0.1-SNAPSHOT"
	)

mainClass in (Compile, run) := Some( "funl.Main" )

proguardSettings

ProguardKeys.options in Proguard ++= Seq( "-dontnote", "-dontwarn", "-ignorewarnings", "-optimizations !class/merging/*" )

ProguardKeys.options in Proguard += ProguardOptions.keepMain( "funl.Main" )


assemblySettings

mainClass in assembly := Some( "funl.Main" )

jarName in assembly := "funl.jar"


LaikaPlugin.defaults

templateDirectives in Laika += LaikaExtension.bootstrapToc


publishMavenStyle := true

publishTo := {
  val nexus = "https://oss.sonatype.org/"
  if (isSnapshot.value)
    Some("snapshots" at nexus + "content/repositories/snapshots")
  else
    Some("releases"  at nexus + "service/local/staging/deploy/maven2")
}

publishArtifact in Test := false

pomIncludeRepository := { _ => false }

licenses := Seq("GPL" -> url("http://opensource.org/licenses/GPL-3.0"))

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