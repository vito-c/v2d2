name    := "v2d2"
version := "0.0.1"
fork    := true
cancelable in Global := true
// scalaVersion := "2.12.4"
scalaVersion := "2.11.7"
ensimeIgnoreScalaMismatch in ThisBuild := true
ensimeScalaVersion in ThisBuild := "2.11.7"
// ensimeServerVersion in ThisBuild := 3.0.0-SNAPSHOT

// enablePlugins(JavaAppPackaging)
// javaOptions += "-Dsmack.debugEnabled=true v2d2"
// val akkaVersion = "2.4.11" // "2.4.2-RC2"
// val akkaVersion = "2.5.4" // "2.4.2-RC2"
val sprayVersion = "1.3.3"

val akkaV       = "2.4.11" //"2.5.3"
val akkaHttpV   = "10.0.9"
val scalaTestV  = "3.0.1"
scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature")

// scalacOptions ++= Seq("-unchecked", "-deprecation")
// Need to import typesafe better
// "com.github.nscala-time" %% "nscala-time" % "2.16.0"
libraryDependencies ++= Seq(

  "com.typesafe.akka" %% "akka-actor" % akkaV,
  "com.typesafe.akka" %% "akka-contrib" % akkaV,
  "com.typesafe.akka" %% "akka-stream" % akkaV,
  "com.typesafe.akka" %% "akka-testkit" % akkaV,
  "com.typesafe.akka" %% "akka-http" % akkaHttpV,
  "com.typesafe.akka" %% "akka-http-spray-json" % akkaHttpV,
  "com.typesafe.akka" %% "akka-http-testkit" % akkaHttpV,
  "org.scalatest"     %% "scalatest" % scalaTestV % "test",
  "org.apache.commons" % "commons-lang3" % "3.6",
  "org.apache.commons" % "commons-text" % "1.1",

  "io.spray" %%  "spray-client" % sprayVersion,
  // "io.spray" %%  "spray-json" % sprayVersion,
  "com.github.nscala-time" %% "nscala-time" % "2.16.0",
  "org.scalactic" %% "scalactic" % "2.2.6",
  // "org.scalatest" %% "scalatest" % "2.2.6" % "test",
  // "com.typesafe.akka" %% "akka-http-spray-json" % akkaV,
  // // "com.typesafe.akka" %% "akka-http" % "10.0.9",
  // "com.typesafe.akka" %% "akka-http" % akkaV,
  // "com.typesafe.akka" %% "akka-actor" % akkaV, // "2.3.12",
  // "com.typesafe.akka" %% "akka-testkit" % akkaV,
  // "com.typesafe.akka" %% "akka-persistence" % akkaV,
  "org.slf4j" % "slf4j-log4j12" % "1.7.16",
  // "com.typesafe.scala-logging" %% "scala-logging" % "3.1.0",
  "com.typesafe.scala-logging" %% "scala-logging" % "3.7.2",
  "org.apache.commons" % "commons-lang3" % "3.4",
  "com.typesafe" % "config" % "1.3.0",
  // "com.typesafe.scala-logging" %% "scala-logging-slf4j" % "2.1.2",
  // "com.lihaoyi" %% "fastparse" % "0.3.7",
  "com.lihaoyi" %% "fastparse" % "0.4.4",
  // "com.lihaoyi" %% "pprint" % "0.4.3",
  "com.lihaoyi" %% "pprint" % "0.5.3",
  "com.github.seratch" %% "awscala" % "0.3.+",
  "org.jxmpp" % "jxmpp-jid" % "0.6.0",
  "org.jxmpp" % "jxmpp-stringprep-libidn" % "0.6.0"
)
libraryDependencies ++= Seq("smack-java7", "smack-tcp", "smack-extensions") map 
{ "org.igniterealtime.smack" % _ % "4.2.2" }

resolvers += "Glassfish" at "http://maven.glassfish.org/content/repositories/maven.hudson-labs.org"
resolvers += "scalaz-bintray" at "https://dl.bintray.com/scalaz/releases"
enablePlugins(DockerPlugin)
dockerfile in docker := {
  val jarFile: File = sbt.Keys.`package`.in(Compile, packageBin).value
  val classpath = (managedClasspath in Compile).value
  val mainclass = mainClass.in(Compile, packageBin).value.getOrElse(sys.error("Expected exactly one main class"))
  val jarTarget = s"/app/${jarFile.getName}"
  // Make a colon separated classpath with the JAR file
  val classpathString = classpath.files.map("/app/" + _.getName)
    .mkString(":") + ":" + jarTarget
  new Dockerfile {
    // Base image
    from("java")
    // Add all files on the classpath
    add(classpath.files, "/app/")
    // Add the JAR file
    add(jarFile, jarTarget)
    // On launch run Java with the classpath and the main class
    entryPoint("java", "-cp", classpathString, mainclass)
  }
}
