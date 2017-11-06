name := """scalaz-vs-cats"""

version := "1.0.0"

scalaVersion in ThisBuild := "2.12.4"

/* Settings common to each sub project */
val common = Seq(
  scalacOptions ++= Seq(
    "-deprecation",
    "-language:higherKinds",
    "-Ypartial-unification"
  ),

  libraryDependencies ++= Seq(
    "org.typelevel" %% "cats-core"     % "1.0.0-RC1",
    "org.typelevel" %% "cats-effect"   % "0.5",
    "org.scalaz"    %% "scalaz-core"   % "7.3.0-M18",
    "org.scalaz"    %% "scalaz-effect" % "7.3.0-M18"
  )
)

/* The library itself */
lazy val lib = project.in(file(".")).settings(common)

/* Benchmarking suite.
 * Benchmarks can be executed by first switching to the `bench` project and then by running:
 *     jmh:run -t 1 -f 1 -wi 5 -i 5 .*Bench.*
 */
lazy val bench = project.in(file("bench")).settings(common).dependsOn(lib).enablePlugins(JmhPlugin)
