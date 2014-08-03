## Description

I am currently learning Haskell so I decided to translate [my previous attempt at the bowling kata in C#](https://github.com/taylorjg/BowlingKata) from C# to Haskell.

## Links

* http://content.codersdojo.org/code-kata-catalogue/bowling-game/

## Screenshots

![Screenshot](https://raw.githubusercontent.com/taylorjg/Bowling_Haskell/master/Images/Screenshot.png "Screenshot")

## TODO

* Add unit tests using HUnit
* ~~Allow the user to choose from several pre-defined lists of rolls~~
* Allow the user to enter a their own list of rolls
* Display a formatted score card:
 * ~~on the console as an ASCII rendering~~
 * as an HTML page (generate an HTML page and open the default browser to display it)
* Currently, the applyRollToMove method is a bit ugly. Try to improve the structure:
 * maybe introduce different Frame types e.g. one for each frame state
 * add functions to assist with creating a new frame from an old frame
* ~~Create a separate module for the Frame type and the processRolls function~~
