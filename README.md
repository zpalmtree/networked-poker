# networked-poker
[![Build Status](https://travis-ci.org/ZedPea/networked-poker.svg?branch=gui)](https://travis-ci.org/ZedPea/networked-poker)

A collection of programs to allow multiple clients to play poker together on one server, with a GUI for clients, and an AI to play against.

Local server gameplay is pretty good, just started adding network support and initial network work on the client.

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

#### Compile and install

`stack install`

Then either add ~/.local/bin to your path and run the executable of your choice:

`ai`
`client`
`server`

Or, run

`stack exec ai`
`stack exec client`
`stack exec server`
