name := """scalaz-vs-cats"""

version := "1.0"

/* Settings common to each sub project */
val common = Seq(
  scalaVersion := "2.11.11",

  scalacOptions ++= Seq(
    "-language:higherKinds",
    "-Ypartial-unification"
  ),

  libraryDependencies ++= Seq(
    "org.typelevel" %% "cats-core"   % "1.0.0-MF",
    "org.scalaz"    %% "scalaz-core" % "7.3.0-M15"
  )
)

/* The library itself */
lazy val lib = project.in(file(".")).settings(common)

/* Benchmarking suite.
 * Benchmarks can be executed by first switching to the `bench` project and then by running:
 *     jmh:run -t 1 -f 1 -wi 10 -i 10 .*Bench.*
 */
lazy val bench = project.in(file("bench")).settings(common).dependsOn(lib).enablePlugins(JmhPlugin)
