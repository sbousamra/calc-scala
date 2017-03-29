name := "calc-scala"
version := "0.0.1"
scalacOptions := Seq("-unchecked", "-feature", "-deprecation", "-encoding", "utf8", "-Xfatal-warnings", "-language:postfixOps")
scalaVersion := "2.11.8"


libraryDependencies ++= Seq(
  "org.tpolecat" %% "atto-core"  % "0.5.1",
  "org.tpolecat" %% "atto-compat-scalaz71" % "0.5.1",
  "org.tpolecat" %% "atto-compat-scalaz72" % "0.5.1",
  "org.tpolecat" %% "atto-compat-cats"     % "0.5.1"
)
