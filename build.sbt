lazy val botDirectory = settingKey[File]("Where to publish the bot")
lazy val botName = settingKey[String]("The name of the bot")

//lazy val publish = taskKey[Unit]("Publish the bot to the Scalatron server!")

lazy val scalatronBot = (project in file(".")).settings(
  name := "scalatron-bot",
  version := "1.0",
  scalaVersion := "2.11.8",
  libraryDependencies ++= List(
    "org.spire-math" %% "spire" % "0.11.0",
    "org.scalatest" %% "scalatest" % "3.0.0-M15" % "test"
  ),
  scalacOptions += "-feature",
  botDirectory := file("/workspace/scalatron/bots"),
  botName := "MattBot"
)
