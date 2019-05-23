# A6 Physics Simulator.

## Main
Main method which follows the steps:

`0 initialize a universe`

`1 wait for user input`

`2 execute user input`

`3 GOTO 1`

### Functions for each variant of Input.t

These functions should not be exposed. They only need to be used by the pattern matching in step 2 of the main loop.

Function quit:
- exits the program

Function help:
- prints commands

Function Add:
- calls Simulator.add on the current universe

Function remove:
- calls Simulator.remove on the current universe

Function run:
- calls Simulator.run on current universe

Function print:
- prints the formatted current universe

## Input
Exception `Malformed` to be thrown when user inputs bad string. Could be variant for different types of bad strings.

Variant type `t` of
- `Quit`
- `Add of name, mass, position vector, velocity vector, (radius?)`
- `Remove of name`
- `Run of time`
- `Help`
- `Print`

Function read:
- Blocks until user inputs non-empty string
- Returns something of type `t` or `Malformed`

## Simulator
Type `vector of float, float, float`

Type `body of name, mass, position vector, velocity vector, (radius?)`

Type `universe of body list`

Function add:
- adds a body to a universe and returns a new universe

Function remove:
- removes a body from a universe and returns a new universe

Function step:
- take a universe and return that universe after a time of Δt
- Δt can be negative
- should not apear in the mli except for testing

Function display:
- takes a universe and displays it on the terminal
- returns unit

Function run:
- simulate a universe for a given period of time
- time can be negative
- repeatedly calls step and display
- returns new universe

Function format:
- prety print the information about a given universe
