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

## The object of the game

The object of the game is to end up with the most energy at the
end of a round. Energy can gained by eating (moving into) "good"
plants or beasts or damaging enemies by exploding a slave bot.
Energy is lost by eating "bad" plants, getting bitten by "bad"
beasts, or being damaged in an exposion.

At the start of each round, a random playing field will be
generated, based on the number of bots playing. The field wraps
around itself, so moving out of the top will position a bot at
the bottom of the field. There will be random walls strewn about,
as well as a number of other game objects as follows:

####Beasts
**Snorgs** - Snorgs are enemy beasts. They will chase after
players and attempt to bite them. If a Snorg makes contact with
a bot, that bot loses 150 energy. After a Snorg bites 7 times, it
will disappear and respawn elsewhere.

**Fluppets** - Fluppets are good beasts. They will try to run
away from players. Eating a Fluppet will grant 200 energy.

####Plants
**Zugars** - Zugars are good plants. They do not move, and
eating one will grant 100 energy. Zugars will respawn elsewhere
when eaten.

**Toxifera** - Toxifera are bad plants. They do not move, and
eating one will take away 100 energy. Toxifera will respawn
elsewhere when eaten

####Walls
Walls do not move and cannot be moved over. Trying to move
through a wall will stun the bot for 4 rounds and cost 10
energy.

####Enemies
Every player has a main "master" bot and can also spawn mini or
"slave" bots. Trying to move into another player's master bot
will simply fail. Moving into a slave bot, however, will grant
150 energy and destroy the slave. Be careful, however, as slave
bots can explode, causing energy damage to everything around them
and granting the damage dealt as energy back to their master! If
two slave bots collide, both are destroyed.

#####Explosions
When a mini bot explodes, it can determine the explosion radius.
Exploding will use up all of the energy that a mini bot has. The
damage that it deals to other entities is calculated as follows:

energyPerArea = energy / PI*radius²
damage = (energyPerArea*200)*(1-(distance from center / radius))

The damage will then be capped based on how much energy the
target actually has (dealing 500 theoretical damage to a target
with only 100 energy will only deal 100 damage instead).


## Bot Commands

There are 3 commands that a bot can receive on its turn:
`Welcome`, `React`, and `Goodbye`. `Welcome` and `Goodbye`
are each called exactly once. They should only be used to set up
initial state for your bot or to free up any persistent memory.

`React` is called every time the bot can take an action. For main
bots, this is every other round, but slave bots can move every
round (meaning that slave bots are twice as fast as master bots).
When `React` is passed to a bot, it can choose from some
combination of the following actions:

###Game-affecting commands
**Move** - Tell the bot to move one square in any direction,
including diagonally

**Spawn** - Tell the bot to spawn a new slave bot next to it,
transferring some energy to the slave bot. Passing additional
parameters in the `slaveProperties` field will cause those
properties to be passed back to the slave when its `React`
command is given.

**Set** - Assign a new property to this bot or change the value
of an existing property, which will be passed back in the `React`
command.

**Explode** - For slave bots only, cause the bot to explode as
defined above, dealing damage to nearby entities (bots and
beasts). Note that explosions can only have a radius between
2 and 10 cells.

###Debugging / Fun commands
**Say** - Cause some text to appear on the game board, and stay
where it was created. Note that having too many messages can
slow down gameplay.

**Status** - Cause some text to appear and move with the bot

**MarkCell** - Change the color of a cell on the gameboard.

**DrawLine** - Draw a line across the gameboard.

**Log** - Cause some output to be shown in the Scalatron server
log.
