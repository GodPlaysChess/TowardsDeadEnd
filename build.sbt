name := "TowardsDeadEnd"

version := "1.0"

scalaVersion := "2.11.4"

addCommandAlias("generate-project",
  ";update-classifiers;update-sbt-classifiers;gen-idea sbt-classifiers")

libraryDependencies += "com.novocode" % "junit-interface" % "0.10" % "test"

libraryDependencies += "commons-codec" % "commons-codec" % "1.6"

libraryDependencies += "org.slf4j" % "slf4j-simple" % "1.6.4"

libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.2.1" % "test"

libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.1.1"

libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.12.2" % "test"
