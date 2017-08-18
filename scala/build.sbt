name := """scalaz-vs-cats"""

version := "1.0"

scalaVersion := "2.12.3"

scalacOptions ++= Seq(
  "-language:higherKinds",
  "-Ypartial-unification"
)

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-core"   % "1.0.0-MF",
  "org.scalaz"    %% "scalaz-core" % "7.3.0-M15",
  "com.azavea"    %% "scaliper"    % "0.5.0-SNAPSHOT" % "test"
)
