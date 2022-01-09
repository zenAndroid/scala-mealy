val scala3Version = "3.1.0"
lazy val osName = System.getProperty("os.name") match {
  case n if n.startsWith("Linux")   => "linux"
  case n if n.startsWith("Mac")     => "mac"
  case n if n.startsWith("Windows") => "win"
  case _                            => throw new Exception("Unknown platform!")
}

// Add dependency on JavaFX libraries, OS dependent
lazy val javaFXModules = Seq("base", "controls", "fxml", "graphics", "media", "swing", "web")
lazy val root = project
  .in(file("."))
  .settings(
    name                                 := "hello-world",
    version                              := "0.1.0-SNAPSHOT",
    scalaVersion                         := scala3Version,
    libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % "test",
    libraryDependencies += "org.scalafx"  % "scalafx_3"       % "17.0.1-R26",
    libraryDependencies ++= javaFXModules.map(m => "org.openjfx" % s"javafx-$m" % "17" classifier osName),
    libraryDependencies += "org.scalameta" %% "munit" % "0.7.29" % Test

    // Determine OS version of JavaFX binaries

  )
