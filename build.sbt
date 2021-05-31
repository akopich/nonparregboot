name := "nonparregboot"

version := "0.1"

scalaVersion := "2.13.2"

scalacOptions ++= Seq("-deprecation", "-feature", "-language:postfixOps", "-language:implicitConversions",
  "-language:higherKinds", "-Ymacro-annotations")

addCompilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.1")

libraryDependencies += "org.typelevel" %% "cats-core" % "2.1.1"

libraryDependencies += "org.typelevel" %% "cats-macros" % "2.1.1"

libraryDependencies += "org.typelevel" %% "cats-kernel" % "2.1.1"

libraryDependencies += "org.typelevel" %% "cats-effect" % "2.1.3"

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
