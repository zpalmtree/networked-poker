# networked-poker
A collection of programs to allow multiple clients to play poker together on one server, with a GUI for clients, and an AI to play against.

Currently working on the server. The other two haven't been started yet. Game is now runnable. Output is being added. No input is being taken from network yet.

Need to add output of what hand winners have.

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

Then either add ~/.local/bin to your path and run the executable of your choice:

`ai`
`client`
`server`

Or, run

`stack exec ai`
`stack exec client`
`stack exec server`
