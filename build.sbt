
// must be at least 2.11 to use hmt_textmodel
scalaVersion := "2.12.3"

resolvers += Resolver.jcenterRepo
resolvers += "beta" at "http://beta.hpcc.uh.edu/nexus/content/repositories/releases"
resolvers += Resolver.bintrayRepo("neelsmith", "maven")
resolvers += Resolver.bintrayRepo("eumaeus", "maven")
libraryDependencies ++=   Seq(
  "org.scalatest" % "scalatest_2.12" % "3.0.1" % "test",
  "edu.holycross.shot" %% "seqcomp" % "1.0.0",
    "edu.holycross.shot.cite" %% "xcite" % "3.3.0",
    "edu.holycross.shot" %% "cex" % "6.2.1",
    "edu.holycross.shot" %% "citerelations" % "2.1.1",
    "edu.holycross.shot" %% "ohco2" % "10.8.1",
    "edu.holycross.shot" %% "citebinaryimage" % "1.1.2",
    "edu.holycross.shot" %% "scm" % "6.0.2",
    "edu.holycross.shot" %% "citeobj" % "7.0.0",
    "edu.holycross.shot" %% "greek" % "1.4.0",
    "edu.furman.classics" %% "poslib" % "0.1.0",
	 "io.spray" %%  "spray-json" % "1.3.3"
)
