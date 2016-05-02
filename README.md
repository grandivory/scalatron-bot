# Scalatron Bot Framework

This project provides a basic framework to begin coding bots for
the Scalatron botwar simulator.

To get started, look in `com.grandivory.scalatron.bot.Bot`.
In that file, you'll find the `Bot` object, which holds the
method that gets called on each round of the botwar simulation.

Simply fill in the `performAction` method with the logic you want
to implement for your bot, and the project will handle the rest!

When you're ready to test your bot in the simulation, go to the
`build.sbt` and fill in the `botDirectory` and `botName`
parameters, then use the `publish` command to push your bot to
the Scalatron server. Best of luck!

## The rules of the game

TODO

## Bot I/O

There are 3 commands that a bot can receive in its `performAction`
call: `Welcome`, `React`, and `Goodbye`. `Welcome` and `Goodbye`
are each called exactly once. They should only be used to set up
initial state for your bot or to free up any persistent memory.

`React` is passed a series of parameters...TODO

