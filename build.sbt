scalaVersion := "2.11.7"

scalacOptions := Seq("-unchecked", "-deprecation", "-encoding", "utf8", "-target:jvm-1.7", "-feature",
  "-Xlog-reflective-calls", "-Ywarn-adapted-args")

resolvers ++= Seq(
  "Sonatype Public" at "https://oss.sonatype.org/content/groups/public/",
  "bintray/non" at "http://dl.bintray.com/non/maven"
)

addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0-M5" cross CrossVersion.full)
addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.5.4")


libraryDependencies += "com.github.mpilquist" %% "simulacrum" % "0.4.0"
libraryDependencies += "org.typelevel" %% "discipline" % "0.4"
libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.5"
libraryDependencies += "org.spire-math" %% "cats" % "0.2.0"
