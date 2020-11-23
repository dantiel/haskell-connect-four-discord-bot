# Connect Four Discord Bot

This is a Discord bot written in Haskell that runs in a Docker container on a free Heroku worker dyno and allows the users play an ASCII connect four game against a KI by using chat commands.

## Installation

### Prerequisites

* [Heroku Toolbelt](https://devcenter.heroku.com/articles/heroku-cli)
* [Docker](https://www.docker.com/get-started) up and running
* [Haskell Tool Stack](https://docs.haskellstack.org/en/stable/install_and_upgrade/)

### Deployment

After [creating a discord bot and adding it to your server](https://github.com/reactiflux/discord-irc/wiki/Creating-a-discord-bot-&-getting-a-token) follow these steps:
 
1. Create a heroku app
2. Setup a postgres database
3. Supply database environment variables in `production.env` -- for reference see `example.env`.  `TOKEN` environment variable is 
4. Deploy by `> $ make deploy`. This will take a while at first, future deploys will run faster (see `Dockerfile`).
5. Make sure herokus dyno is properly configured `> $ heroku ps:scale web=0 worker=1`
6. Have fun!

## Bot Commands

* `!newgame` - start a new game
* `!move <X>` do your move where _`<X>`_ is a number [1-7]
* `!show` yield current state again

## TODO 

[X] win against KI
[ ] make difficulty selectable per user
[ ] add a multiplayer mode that allows playing against other users.
[ ] control by reactions than commands
[ ] play game in place by editing original message
[ ] replace `X`and `O` characters by colored symbols
[ ] cleaner code

