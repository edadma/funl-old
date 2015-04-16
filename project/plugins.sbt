addSbtPlugin("com.typesafe.sbt" % "sbt-proguard" % "0.2.2")

addSbtPlugin("com.eed3si9n" % "sbt-assembly" % "0.11.2")

addSbtPlugin("org.planet42" % "laika-sbt" % "0.5.0")

addSbtPlugin("com.typesafe.sbt" % "sbt-pgp" % "0.8.3")

resolvers += Resolver.url(
  "bintray-sbt-plugin-releases",
    url("http://dl.bintray.com/content/sbt/sbt-plugin-releases"))(
        Resolver.ivyStylePatterns)

addSbtPlugin( "me.lessis" % "bintray-sbt" % "0.2.1" )
