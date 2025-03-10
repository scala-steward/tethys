lazy val scala212 = "2.12.20"
lazy val scala213 = "2.13.16"
lazy val scala3 = "3.3.5"

ThisBuild / scalaVersion := scala3

lazy val commonSettings = Seq(
  organization := "com.tethys-json",
  licenses := Seq(
    "Apache-2.0" -> url("https://www.apache.org/licenses/LICENSE-2.0")
  ),
  homepage := Some(url("https://github.com/tethys-json/tethys")),
  scmInfo := Some(
    ScmInfo(
      url("https://github.com/tethys-json/tethys"),
      "scm:git@github.com:tethys-json/tethys.git"
    )
  ),
  developers := List(
    Developer(
      id = "eld0727",
      name = "Aleksei Otts",
      email = "eld0727@gmail.com",
      url = url("https://github.com/eld0727")
    ),
    Developer(
      id = "REDNBLACK",
      name = "Boris Potepun",
      email = "boris.p@protonmail.com",
      url = url("https://github.com/REDNBLACK")
    ),
    Developer(
      id = "MrIrre",
      name = "Erlan Zhaygutov",
      email = "zhaigutov.erlan@gmail.com",
      url = url("https://github.com/MrIrre")
    )
  ),
  Test / publishArtifact := false
)

def crossScalaSettings = {
  def addDirsByScalaVersion(path: String): Def.Initialize[Seq[sbt.File]] =
    scalaVersion.zip(baseDirectory) { case (v, base) =>
      def extraDirs(versionSpecificFolder: String): Seq[sbt.File] =
        Seq(base / path / versionSpecificFolder)

      CrossVersion.partialVersion(v) match {
        case Some((2, y)) if y >= 13 =>
          extraDirs("scala-2.13+")
        case Some((3, _)) =>
          extraDirs("scala-3")
        case _ => Seq.empty
      }
    }

  Seq(
    crossScalaVersions := Seq(scala212, scala213, scala3),
    Compile / unmanagedSourceDirectories ++= addDirsByScalaVersion(
      "src/main"
    ).value,
    Test / unmanagedSourceDirectories ++= addDirsByScalaVersion(
      "src/test"
    ).value
  )
}

lazy val testSettings = Seq(
  libraryDependencies ++= Seq(
    "org.scalatest" %% "scalatest-flatspec" % "3.2.19" % Test,
    "org.scalatest" %% "scalatest-shouldmatchers" % "3.2.19" % Test
  )
)

lazy val tethys = project
  .in(file("."))
  .settings(
    publishTo := None,
    crossScalaVersions := Seq.empty,
    commonSettings
  )
  .aggregate(
    core,
    `macro-derivation`,
    `jackson-211`,
    `jackson-212`,
    `jackson-213`,
    `jackson-214`,
    `jackson-215`,
    json4s,
    circe,
    refined,
    enumeratum,
    cats
  )

lazy val modules = file("modules")

def addScalaReflect(scalaVersion: String): Seq[ModuleID] =
  CrossVersion.partialVersion(scalaVersion) match {
    case Some((2, y)) =>
      Seq("org.scala-lang" % "scala-reflect" % scalaVersion)
    case _ => Seq.empty
  }

lazy val core = project
  .in(modules / "core")
  .settings(crossScalaSettings)
  .settings(commonSettings)
  .settings(testSettings)
  .settings(
    name := "tethys-core",
    libraryDependencies ++= addScalaReflect(scalaVersion.value)
  )

lazy val cats = project
  .in(modules / "cats")
  .settings(crossScalaSettings)
  .settings(commonSettings)
  .settings(testSettings)
  .settings(
    name := "tethys-cats",
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-core" % "2.13.0"
    )
  )
  .dependsOn(core)

lazy val `macro-derivation` = project
  .in(modules / "macro-derivation")
  .settings(crossScalaSettings)
  .settings(commonSettings)
  .settings(testSettings)
  .settings(
    name := "tethys-derivation",
    libraryDependencies ++= addScalaReflect(scalaVersion.value)
  )
  .dependsOn(core)

lazy val jacksonSettings = Seq(
  Compile / unmanagedSourceDirectories += modules / "jackson-backend" / "src" / "main",
  Test / unmanagedSourceDirectories += modules / "jackson-backend" / "src" / "test",
  Test / unmanagedResourceDirectories += modules / "jackson-backend" / "src" / "test" / "resources"
)

lazy val `jackson-211` = project
  .in(modules / "jackson-211")
  .settings(crossScalaSettings)
  .settings(commonSettings)
  .settings(jacksonSettings)
  .settings(testSettings)
  .settings(
    name := "tethys-jackson211",
    libraryDependencies ++= Seq(
      "com.fasterxml.jackson.core" % "jackson-core" % "2.11.4"
    )
  )
  .dependsOn(core)

lazy val `jackson-212` = project
  .in(modules / "jackson-212")
  .settings(crossScalaSettings)
  .settings(commonSettings)
  .settings(jacksonSettings)
  .settings(testSettings)
  .settings(
    name := "tethys-jackson212",
    libraryDependencies ++= Seq(
      "com.fasterxml.jackson.core" % "jackson-core" % "2.12.7"
    )
  )
  .dependsOn(core)

lazy val `jackson-213` = project
  .in(modules / "jackson-213")
  .settings(crossScalaSettings)
  .settings(commonSettings)
  .settings(jacksonSettings)
  .settings(testSettings)
  .settings(
    name := "tethys-jackson213",
    libraryDependencies ++= Seq(
      "com.fasterxml.jackson.core" % "jackson-core" % "2.13.5"
    )
  )
  .dependsOn(core)

lazy val `jackson-214` = project
  .in(modules / "jackson-214")
  .settings(crossScalaSettings)
  .settings(commonSettings)
  .settings(jacksonSettings)
  .settings(testSettings)
  .settings(
    name := "tethys-jackson214",
    libraryDependencies ++= Seq(
      "com.fasterxml.jackson.core" % "jackson-core" % "2.14.3"
    )
  )
  .dependsOn(core)

lazy val `jackson-215` = project
  .in(modules / "jackson-215")
  .settings(crossScalaSettings)
  .settings(commonSettings)
  .settings(jacksonSettings)
  .settings(testSettings)
  .settings(
    name := "tethys-jackson215",
    libraryDependencies ++= Seq(
      "com.fasterxml.jackson.core" % "jackson-core" % "2.15.4"
    )
  )
  .dependsOn(core)

lazy val circe = project
  .in(modules / "circe")
  .settings(crossScalaSettings)
  .settings(commonSettings)
  .settings(testSettings)
  .settings(
    name := "tethys-circe",
    libraryDependencies ++= Seq(
      "io.circe" %% "circe-core" % "0.14.10"
    )
  )
  .dependsOn(core, `jackson-212` % Test)

lazy val json4s = project
  .in(modules / "json4s")
  .settings(crossScalaSettings)
  .settings(commonSettings)
  .settings(testSettings)
  .settings(
    name := "tethys-json4s",
    libraryDependencies ++= Seq(
      "org.json4s" %% "json4s-ast" % "4.0.7"
    )
  )
  .dependsOn(core)

lazy val enumeratum = project
  .in(modules / "enumeratum")
  .settings(crossScalaSettings)
  .settings(commonSettings)
  .settings(testSettings)
  .settings(
    name := "tethys-enumeratum",
    libraryDependencies ++= {
      CrossVersion.partialVersion(scalaVersion.value) match {
        case Some((2, y)) =>
          Seq("com.beachape" %% "enumeratum" % "1.7.5")
        case _ => Seq.empty
      }
    }
  )
  .dependsOn(core)

lazy val refined = project
  .in(modules / "refined")
  .settings(crossScalaSettings)
  .settings(commonSettings)
  .settings(testSettings)
  .settings(
    name := "tethys-refined",
    libraryDependencies ++= Seq(
      "eu.timepit" %% "refined" % "0.10.3"
    )
  )
  .dependsOn(core)

lazy val benchmarks = project
  .in(modules / "benchmarks")
  .settings(crossScalaSettings)
  .settings(commonSettings)
  .settings(
    publishTo := None,
    libraryDependencies ++= Seq(
      "io.spray" %% "spray-json" % "1.3.6",
      "org.json4s" %% "json4s-native" % "4.0.7",
      "org.json4s" %% "json4s-jackson" % "4.0.7",
      "io.circe" %% "circe-core" % "0.14.9",
      "io.circe" %% "circe-generic" % "0.14.9",
      "io.circe" %% "circe-jawn" % "0.14.9",
      "io.circe" %% "circe-jackson210" % "0.14.0",
      "dev.zio" %% "zio-json" % "0.7.1",
      "com.typesafe.play" %% "play-json" % "2.10.5",
      "org.knowm.xchart" % "xchart" % "3.8.2" exclude ("de.erichseifert.vectorgraphics2d", "VectorGraphics2D") withSources ()
    ),
    scalacOptions ++= {
      CrossVersion.partialVersion(scalaVersion.value) match {
        case Some((2, 13)) => Seq("-Ymacro-annotations")
        case _             => Seq.empty
      }
    }
  )
  .dependsOn(core, `macro-derivation`, `jackson-211`)
  .enablePlugins(JmhPlugin)
