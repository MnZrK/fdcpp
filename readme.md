# The development is in very very early development stage, it should compile but should not work ;)

## About

DC++ client written in F#. For now the UI is only console, intended to make HTML/JS ui, probably with Aurelia.

## How to work on this project

Note that the build is managed using FAKE, so .sln file is dummy. You are free to use
any text editor to work with the source base, I personally use Visual Studio Code
(just open main directory with Code) with a couple of F#-related plugins (Ionide).

## Build

    Run build.cmd

or for quick building (skipping Paket version check):

    Run buildQ.cmd

## Run

After building,

    Run build/FDCConsoleUI.exe

## Run Tests

After building,

    Run build.cmd RunTests
