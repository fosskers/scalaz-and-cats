val lib = project.in(file(".")).settings(
  addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.4")
)

/* Benchmarks can be executed by first switching to the `bench` project and then by running:
 *  bench/jmh:run -t 1 -f 1 -wi 5 -i 5 .*Bench.*
 */
val bench = project.dependsOn(lib).enablePlugins(JmhPlugin).settings(
  // bench/jmh:run -i 15 -wi 15 -f1 -t10 .*ClassScalaz*
  javaOptions in (Jmh, run) ++= scala.util.Properties.envOrNone("YOURKIT_AGENT").map { name =>
    val agent = file(name)
    require(agent.exists(), s"Yourkit agent specified ($agent) does not exist")
    Seq(s"-agentpath:${agent.getCanonicalPath}=quiet")
  }.getOrElse(Nil)
)
