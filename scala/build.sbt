name := """scalaz-vs-cats"""

version := "1.0.0"

scalaVersion in ThisBuild := "2.12.5"

/* Settings common to each sub project */
val common = Seq(
  scalacOptions ++= Seq(
    "-deprecation",
    "-language:higherKinds",
    "-Ypartial-unification",
    "-Ybackend-parallelism", "4"
  ),

  resolvers += Resolver.sonatypeRepo("snapshots"),

  addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.1" cross CrossVersion.full),

  libraryDependencies ++= Seq(
    "com.fommil"    %% "deriving-macro"  % "0.9.1-SNAPSHOT",
    "com.fommil"    %% "scalaz-deriving" % "0.9.1-SNAPSHOT",
    "org.typelevel" %% "cats-core"       % "1.0.0",
    "org.typelevel" %% "cats-effect"     % "0.7",
    "org.typelevel" %% "kittens"         % "1.0.0-RC1",
    "org.scalaz"    %% "scalaz-core"     % "7.2.18",
    "org.scalaz"    %% "scalaz-effect"   % "7.2.18"
  )
)

/* The library itself */
lazy val lib = project.in(file(".")).settings(common)

/* Benchmarking suite.
 * Benchmarks can be executed by first switching to the `bench` project and then by running:
      jmh:run -t 1 -f 1 -wi 5 -i 5 .*Bench.*
 */
lazy val bench = project.in(file("bench")).settings(common).dependsOn(lib).enablePlugins(JmhPlugin).settings(
  // bench/jmh:run -i 15 -wi 15 -f1 -t10 .*EqualBench.equalDiffScalaz*
  javaOptions in (Jmh, run) ++= scala.util.Properties.envOrNone("YOURKIT_AGENT").map { name =>
    val agent = file(name)
    require(agent.exists(), s"Yourkit agent specified ($agent) does not exist")
    Seq(s"-agentpath:${agent.getCanonicalPath}=quiet")
  }.getOrElse(Nil)
)
