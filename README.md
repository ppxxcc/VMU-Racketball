# VMU-Racketball
Racketball Game Demo for the VMU

About
=====
This is a simple game demo for the VMU which implements a single player pong-like experience, so, racketball.

![Demo gif](https://github.com/pxcla/VMU-Racketball/blob/master/demo.gif)

Build Instructions
==================
This project is built with the [Waterbear](https://github.com/wtetzner/waterbear) assembler by wtetzner.

To build the project, in the project root directory, run:

    waterbear assemble main.s -o racketball.vms

This will create an output ROM called `racketball.vms`

Special Thanks
==============

Falco Girgis - [gyrovorbis](https://github.com/gyrovorbis) - For creating ElysianVMU VMU emulator for testing.

Walter Tetzner - [wtetzner](https://github.com/wtetzner) - For creating the Waterbear assembler for building.

Marcus Comstedt - [homepage](https://mc.pp.se/dc/) - For providing all the information on the VMU architecture.
