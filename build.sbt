name := "expr-evaluator"

version := "1.0"

scalaVersion := "2.11.4"

// This adds the ScalaTest framework to the project:
libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.2.4" % "test"

// This adds the parser combinator library:
libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.3"