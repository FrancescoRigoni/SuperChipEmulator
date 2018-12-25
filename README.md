Chip8/SuperChip/Chip48 platform emulator
====================================
An emulator for the Chip8/Chip48/SuperChip platform written in Scala.
This was my first emulator, and also my first real Scala project.

To build the emulator you need to make sure you have `scalac` and `sbt`, then run:
```
$ sbt compile
```

To run the emulator use the `startemu.sh` script
```
$ ./startemu.sh chip-8-roms/games/Space\ Invaders\ \[David\ Winter\].ch8
$ ./startemu.sh superchip8-roms/Spacefight\ 2091\ \[Carsten\ Soerensen\,\ 1992\].ch8
```

You can also import the project in IntelliJ.

![Space Fight](https://raw.githubusercontent.com/FrancescoRigoni/SuperChipEmulator/master/screenshots/space-fight.png "Space Fight")
![Super Astrododge](https://raw.githubusercontent.com/FrancescoRigoni/SuperChipEmulator/master/screenshots/super-astro-dodge.png "Super Astrododge")
![Super Astrododge](https://raw.githubusercontent.com/FrancescoRigoni/SuperChipEmulator/master/screenshots/super-astro-dodge-2.png "Super Astrododge")
![Single Dragon](https://raw.githubusercontent.com/FrancescoRigoni/SuperChipEmulator/master/screenshots/single-dragon.png "Single Dragon")
![Space Invaders](https://raw.githubusercontent.com/FrancescoRigoni/SuperChipEmulator/master/screenshots/space-invaders.png "Space Invaders")

Visit my website [The Code Butchery](https://thecodebutchery.com) for a tutorial on how to make your own emulator:
[Tutorial - Part 1](https://thecodebutchery.com/2018/11/22/write-a-chip8-retro-gaming-emulator-in-one-day-introduction/)
[Tutorial - Part 2](https://thecodebutchery.com/2018/11/25/write-a-chip8-retro-gaming-emulator-in-one-day-loading-the-first-rom/)
[Tutorial - Part 3](https://thecodebutchery.com/2018/12/03/write-a-chip8-retro-gaming-emulator-in-one-day-controller-and-display/)
[Tutorial - Part 4](https://thecodebutchery.com/2018/12/16/write-a-chip8-retro-gaming-emulator-in-one-day-the-cpu-and-its-done/)
