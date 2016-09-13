name := "schema"

version := "1.1"

scalaVersion := "2.11.5"

organization := "sandbox"

libraryDependencies++=
  "org.scalaz" % "scalaz-core_2.11" % "7.2.0" ::
  "nz.ac.waikato.cms.weka" % "weka-dev" % "3.7.13" ::
  "com.chuusai" %% "shapeless" % "2.3.1"  ::
  "com.typesafe.play" %% "play-json" % "2.5.0" ::
   "org.scalatest" %% "scalatest" % "2.2.4" % "test" :: Nil