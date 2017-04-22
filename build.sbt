name := "FunL"

version := "0.20"

scalaVersion := "2.12.2"

crossScalaVersions := Seq( "2.11.11" )

scalacOptions ++= Seq( "-deprecation", "-feature", "-language:postfixOps", "-language:implicitConversions", "-language:existentials" )

incOptions := incOptions.value.withNameHashing( true )

organization := "xyz.hyperreal"

resolvers += "Hyperreal Repository" at "https://dl.bintray.com/edadma/maven"

libraryDependencies ++= Seq(
	"org.scalatest" %% "scalatest" % "3.0.0" % "test",
	"org.scalacheck" %% "scalacheck" % "1.13.4" % "test"
)

libraryDependencies += "jline" % "jline" % "2.14.3"

libraryDependencies += "com.h2database" % "h2" % "1.4.194"

libraryDependencies ++= Seq(
	"xyz.hyperreal" %% "lia" % "0.20",
	"xyz.hyperreal" %% "indentation-lexical" % "0.7",
	"xyz.hyperreal" %% "json" % "0.7",
	"xyz.hyperreal" %% "options" % "0.2"
	)

mainClass in (Compile, run) := Some( "funl.Main" )

isSnapshot := true

proguardSettings

ProguardKeys.options in Proguard ++= Seq( "-dontnote", "-dontwarn", "-ignorewarnings", "-optimizations !class/merging/*" )

ProguardKeys.options in Proguard += ProguardOptions.keepMain( "funl.Main" )


mainClass in assembly := Some( "funl.Main" )

assemblyJarName in assembly := "funl-" + version.value + ".jar"


publishMavenStyle := true

//publishTo := Some( Resolver.sftp( "private", "hyperreal.ca", "/var/www/hyperreal.ca/maven2" ) )

publishArtifact in Test := false

pomIncludeRepository := { _ => false }

licenses := Seq("MIT" -> url("http://opensource.org/licenses/MIT"))

homepage := Some(url("https://github.com/FunL/funl"))

pomExtra :=
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
  </developers>
