lazy val botDirectory = settingKey[File]("Where to publish the bot")
lazy val botName = settingKey[String]("The name of the bot")

lazy val publish = taskKey[Unit]("Publish the bot to the Scalatron server!")

lazy val scalatronBot = (project in file(".")).settings(
  name := "scalatron-bot",
  version := "1.0",
  scalaVersion := "2.11.8",
  libraryDependencies ++= List(
    "org.scalatest" %% "scalatest" % "3.0.0-M15" % "test",
    "org.scalacheck" %% "scalacheck" % "1.12.5" % "test"
  ),
  scalacOptions += "-feature",
  botDirectory := file("/workspace/scalatron/dist/bots"),
  botName := "ShivaTheDestroyer",
  publish := {
    (Keys.test in Test).value
    (Keys.compile in Compile).value
    val jarFile = (Keys.`package` in Compile).value

    val publishDir = new File(botDirectory.value, botName.value)

    IO.createDirectory(publishDir)
    IO.copyFile(jarFile, new File(publishDir, "ScalatronBot.jar"))

    println("Bot successfully published!")
  }
)
