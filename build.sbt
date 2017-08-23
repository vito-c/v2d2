name    := "v2d2"
version := "0.0.1"
fork    := true
cancelable in Global := true
scalaVersion := "2.11.7"

// enablePlugins(JavaAppPackaging)
// javaOptions += "-Dsmack.debugEnabled=true v2d2"
val akkaVersion = "2.4.11" // "2.4.2-RC2"
val sprayVersion = "1.3.3"
val scalaTestVersion = "2.2.6"
scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature")

// scalacOptions ++= Seq("-unchecked", "-deprecation")
// Need to import typesafe better
// "com.github.nscala-time" %% "nscala-time" % "2.16.0"
libraryDependencies ++= Seq(
  "org.apache.commons" % "commons-lang3" % "3.6",
  "com.github.nscala-time" %% "nscala-time" % "2.16.0",
  "io.spray" %%  "spray-client" % sprayVersion,
  "io.spray" %%  "spray-json" % sprayVersion,
  "org.scalactic" %% "scalactic" % "2.2.6",
  "org.scalatest" %% "scalatest" % "2.2.6" % "test",
  "com.typesafe.akka" %% "akka-http-spray-json-experimental" % akkaVersion,
  // "com.typesafe.akka" %% "akka-http" % "10.0.9",
  "com.typesafe.akka" %% "akka-http-experimental" % akkaVersion,
  "com.typesafe.akka" %% "akka-actor" % akkaVersion, // "2.3.12",
  "com.typesafe.akka" %% "akka-testkit" % akkaVersion,
  "com.typesafe.akka" %% "akka-persistence" % akkaVersion,
  "org.slf4j" % "slf4j-log4j12" % "1.7.16",
  "com.typesafe.scala-logging" %% "scala-logging" % "3.1.0",
  "org.apache.commons" % "commons-lang3" % "3.4",
  "com.typesafe" % "config" % "1.3.0",
  // "com.typesafe.scala-logging" %% "scala-logging-slf4j" % "2.1.2",
  "com.lihaoyi" %% "fastparse" % "0.3.7",
  "com.github.seratch" %% "awscala" % "0.3.+"
)
libraryDependencies ++= Seq("smack-java7", "smack-tcp", "smack-extensions") map 
{ "org.igniterealtime.smack" % _ % "4.1.7" }


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
