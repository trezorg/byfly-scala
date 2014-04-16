import AssemblyKeys._

name := "Byfly"

version := "0.0.2"

scalaVersion := "2.10.4"

crossScalaVersions := Seq("2.10.3", "2.10.4")

scalacOptions ++= Seq("-deprecation", "-feature")

libraryDependencies += "org.scalaj" %% "scalaj-http" % "0.3.9" withSources() excludeAll(
    ExclusionRule(organization = "junit")
)

libraryDependencies += "org.scalatest" %% "scalatest" % "1.9.1" % "test"

libraryDependencies += "org.jsoup" % "jsoup" % "1.7.2"

libraryDependencies += "org.rogach" %% "scallop" % "0.9.1"

libraryDependencies += "org.streum" %% "configrity-core" % "1.0.0"

libraryDependencies += "org.fusesource.jansi" % "jansi" % "1.11"

org.scalastyle.sbt.ScalastylePlugin.Settings

assemblySettings

jarName in assembly := "byfly.jar"

target in assembly := file("target/out")

test in assembly := {}

excludedJars in assembly <<= (fullClasspath in assembly) map { cp =>
  cp filter {_.data.getName == "scalatest_2.10-1.9.1.jar"}
}

copyToBin in deploy := true

linuxFile := "byfly"