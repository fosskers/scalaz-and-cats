import sbt._
import sbt.Keys._

object ProjectKeys {
  def KindProjector =
    addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.4")
}

object ProjectPlugin extends AutoPlugin {

  override def trigger = allRequirements

  val autoImport = ProjectKeys
  import autoImport._

  override def buildSettings = Seq(
    name := """scalaz-vs-cats""",
    version := "1.0.0",
    scalaVersion in ThisBuild := "2.12.4"
  )
  override def projectSettings = Seq(
    scalacOptions ++= Seq(
      "-deprecation",
      "-language:higherKinds",
      "-Ypartial-unification"
    ),
    resolvers += Resolver.sonatypeRepo("snapshots"),
    addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.1" cross CrossVersion.full),
    libraryDependencies ++= Seq(
      "com.fommil"    %% "deriving-macro"  % "0.9.1-SNAPSHOT",
      "com.fommil"    %% "scalaz-deriving" % "0.9.1-SNAPSHOT",
      "org.typelevel" %% "cats-core"       % "1.0.0-RC1",
      "org.typelevel" %% "cats-effect"     % "0.5",
      "org.typelevel" %% "kittens"         % "1.0.0-RC1",
      "org.scalaz"    %% "scalaz-core"     % "7.2.16",
      "org.scalaz"    %% "scalaz-effect"   % "7.2.16"
    )
  )
}

