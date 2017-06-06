name := """scalaz-vs-cats"""

version := "1.0"

scalaVersion := "2.11.11"

scalacOptions ++= Seq(
  "-language:higherKinds",
  "-Ypartial-unification"
)

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats"        % "0.9.0",
  "org.scalaz"    %% "scalaz-core" % "7.3.0-M13",
  "com.azavea"    %% "scaliper"    % "0.5.0-SNAPSHOT" % "test"
)
