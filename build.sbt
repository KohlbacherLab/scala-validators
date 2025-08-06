import scala.util.Properties.envOrElse


name := "validators"
ThisBuild / organization := "de.ekut.tbi"
ThisBuild / scalaVersion := "2.13.16"
ThisBuild / version      := envOrElse("VERSION","1.0.0")

val ownerRepo  = envOrElse("REPOSITORY","KohlbacherLab/scala-validators").split("/")
ThisBuild / githubOwner      := ownerRepo(0)
ThisBuild / githubRepository := ownerRepo(1)


//-----------------------------------------------------------------------------
// PROJECT
//-----------------------------------------------------------------------------

lazy val root = project.in(file("."))
  .settings(settings)
  .settings(
    libraryDependencies ++= Seq(
      "org.scalatest"  %% "scalatest"  % "3.2.18" % Test,
      "org.typelevel"  %% "cats-core"  % "2.10.0",
      "com.chuusai"    %% "shapeless"  % "2.3.13",
   )
 )

//-----------------------------------------------------------------------------
// SETTINGS
//-----------------------------------------------------------------------------

lazy val settings = commonSettings

lazy val compilerOptions = Seq(
  "-encoding", "utf8",
  "-unchecked",
  "-Xfatal-warnings",
  "-feature",
  "-language:higherKinds",
  "-language:postfixOps",
  "-deprecation"
)

lazy val commonSettings = Seq(
  scalacOptions ++= compilerOptions,
  resolvers ++= Seq(
    "Local Maven Repository" at "file://" + Path.userHome.absolutePath + "/.m2/repository",
    Resolver.sonatypeCentralSnapshots
  )
  
)

