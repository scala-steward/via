val sharedSettings = Seq(
  scalacOptions ++= Seq(
    "-new-syntax",
    // "-no-indent",
    "-Wvalue-discard",
    "-Wunused:all",
    // "-Werror",
    "-deprecation",
    "-explain",
    "-explain-cyclic",
    "-rewrite",
    "-source:future"
  ),
  javacOptions ++= Seq("-source", "25", "-target", "25")
)

ThisBuild / name := "via"
ThisBuild / organization := "io.via"
ThisBuild / version := "0.0.1"
ThisBuild / scalaVersion := "3.8.1"

lazy val via =
  // select supported platforms
  crossProject(JSPlatform, JVMPlatform, NativePlatform)
    .crossType(CrossType.Full) // [Pure, Full, Dummy], default: CrossType.Full
    // .withoutSuffixFor(JVMPlatform)
    .in(file("via"))
    .settings(sharedSettings *)
    .jvmSettings(
      libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.19" % "test"
    )
    // configure Scala-Native settings
    .nativeSettings( /* ... */ ) // defined in sbt-scala-native
