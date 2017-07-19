# networked-poker
A collection of programs to allow multiple clients to play poker together on one server, with a GUI for clients, and an AI to play against.

Currently working on the server. The other two haven't been started yet. 

## Installation

#### Install prerequistes
You need stack installed.

##### Debian based:
`sudo apt-get install haskell-stack`

##### Arch based:
`sudo pacman -S stack`

#### Clone the repository

`git clone https://github.com/ZedPea/networked-poker.git`

`cd networked-poker`

#### Setup stack and compile

`stack setup`

`stack install`

If you want to try the networked server, run instead

`stack install --flag server:debug`

It is currently non functional.

Then either add ~/.local/bin to your path and run the executable of your choice:

`ai`
`client`
`server`

Or, run

`stack exec ai`
`stack exec client`
`stack exec server`
