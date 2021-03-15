name := "nonparregboot"

version := "0.1"

scalaVersion := "2.13.2"

scalacOptions ++= Seq("-deprecation", "-feature", "-language:postfixOps", "-language:implicitConversions",
  "-language:higherKinds", "-Ymacro-annotations")

addCompilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.1")

libraryDependencies += "org.typelevel" %% "cats-core" % "2.3.1"

libraryDependencies += "org.typelevel" %% "cats-macros" % "2.1.1"

libraryDependencies += "org.typelevel" %% "cats-kernel" % "2.3.1"

libraryDependencies += "org.typelevel" %% "cats-effect" % "2.3.1"

libraryDependencies += "io.laserdisc" %% "log-effect-fs2" % "0.14.1"

lazy val random = RootProject(uri("https://github.com/akopich/scala_pure_random.git"))

lazy val root = (project in file(".")).dependsOn(random)

libraryDependencies  ++= Seq(
  "org.scalanlp" %% "breeze" % "1.0",
  "org.scalanlp" %% "breeze-natives" % "1.0",
  "org.scalanlp" %% "breeze-viz" % "1.0"
)

addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.11.0" cross CrossVersion.full)

libraryDependencies += "org.scalactic" %% "scalactic" % "3.1.2"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.1.2" % "test"

libraryDependencies += "org.platanios" %% "tensorflow" % "0.5.10"

libraryDependencies ++= Seq(
  "org.slf4j" % "slf4j-api"       % "1.7.7",
  "org.slf4j" % "jcl-over-slf4j"  % "1.7.7"
).map(_.force())

libraryDependencies ~= { _.map(_.exclude("org.slf4j", "slf4j-jdk14")) }

libraryDependencies += "com.outr" %% "scribe" % "3.4.0"
