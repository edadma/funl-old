name := "FunL"

version := "0.17"

scalaVersion := "2.11.6"

scalacOptions ++= Seq( "-deprecation", "-feature", "-language:postfixOps", "-language:implicitConversions", "-language:existentials" )

incOptions := incOptions.value.withNameHashing( true )

organization := "org.funl-lang"

resolvers += "Hyperreal Repository" at "https://dl.bintray.com/edadma/maven"

libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.1" % "test"

libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.11.5" % "test"

libraryDependencies += "jline" % "jline" % "2.12.1"

libraryDependencies += "com.h2database" % "h2" % "1.4.187"

//libraryDependencies += "org.scala-lang" %% "scala-pickling" % "0.8.0"

//libraryDependencies += "org.scala-lang" % "scala-swing" % scalaVersion.value

//libraryDependencies += "org.ow2.asm" % "asm" % "4.2"

//libraryDependencies += "org.scaloid" %% "scaloid" % "3.2-8"

libraryDependencies ++= Seq(
//	"org.postgresql" % "postgresql" % "9.3-1101-jdbc41",		//9.2-1004-jdbc4"
//	"org.mongodb" % "casbah_2.9.3" % "2.7.0"
	)

libraryDependencies ++= Seq(
	"ca.hyperreal" %% "lia" % "0.15",
	"ca.hyperreal" %% "indentation-lexical" % "0.3",
	"ca.hyperreal" %% "json" % "0.3",
	"ca.hyperreal" %% "options" % "0.1"
	)

mainClass in (Compile, run) := Some( "funl.Main" )

proguardSettings

ProguardKeys.options in Proguard ++= Seq( "-dontnote", "-dontwarn", "-ignorewarnings", "-optimizations !class/merging/*" )

ProguardKeys.options in Proguard += ProguardOptions.keepMain( "funl.Main" )


import AssemblyKeys._

assemblySettings

mainClass in assembly := Some( "funl.Main" )

jarName in assembly := "funl-dev.jar"


seq(bintraySettings:_*)

publishMavenStyle := true

//publishTo := Some( Resolver.sftp( "private", "hyperreal.ca", "/var/www/hyperreal.ca/maven2" ) )

publishArtifact in Test := false

pomIncludeRepository := { _ => false }

licenses := Seq("MIT" -> url("http://opensource.org/licenses/MIT"))

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
