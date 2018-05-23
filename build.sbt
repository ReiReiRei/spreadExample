name := "spread"

version := "0.1"

scalaVersion := "2.12.6"

libraryDependencies +="org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.0"
libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.5"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % "test"
libraryDependencies += "com.github.scopt" %% "scopt" % "3.7.0"
assemblyJarName in assembly := "spreasheet.jar"