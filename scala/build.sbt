name := """scalaz-vs-cats"""

version := "1.0.0"

scalaVersion in ThisBuild := "2.12.6"

val derivingVersion = "1.0.0-RC1"

/* Settings common to each sub project */
val common = Seq(
  scalacOptions ++= Seq(
    "-deprecation",
    "-language:higherKinds",
    "-Ypartial-unification",
    "-Ywarn-value-discard",
    "-Ywarn-unused-import",
    "-Ywarn-dead-code",
    "-Ywarn-numeric-widen",
    "-Ybackend-parallelism", "4"
  ),

  resolvers += Resolver.sonatypeRepo("snapshots"),

  addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.1" cross CrossVersion.full),
  addCompilerPlugin("com.fommil" %% "deriving-plugin" % derivingVersion),
  addCompilerPlugin("com.olegpy" %% "better-monadic-for" % "0.2.4"),

  libraryDependencies ++= Seq(
    "com.fommil"    %% "deriving-macro"  % derivingVersion,
    "com.fommil"    %% "scalaz-deriving" % derivingVersion,
    "org.typelevel" %% "cats-core"       % "1.1.0",
    "org.typelevel" %% "cats-effect"     % "1.0.0-RC2",
    "org.typelevel" %% "kittens"         % "1.1.0",
    "org.scalaz"    %% "scalaz-core"     % "7.2.25",
    "org.scalaz"    %% "scalaz-ioeffect" % "2.10.1"
  )
)

/* The library itself */
lazy val lib = project.in(file(".")).settings(common)

/* Benchmarking suite.
 * Benchmarks can be executed by first switching to the `bench` project and then by running:
      jmh:run -t 1 -f 1 -wi 5 -i 5 .*Bench.*
 */
lazy val bench = project.in(file("bench")).settings(common).dependsOn(lib).enablePlugins(JmhPlugin)
