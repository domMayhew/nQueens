# nQueens.hs

## Quick Start
Compile nQueens.hs on your machine and run the executable with two optional arguments: an integer specifying the size of board to solve for, and "simple", to use a simpler but faster image template for each board. Both are optional, but to use "simple" you must specify size first.
E.g.,
```bash
./nQueens // Generates an image of the solutions to 8-queens
./nQueens 10 // Generates an image of the solutions to 10-queens
./nQueens 11 simple // Generates an image of the solutions to 11-queens using the simple image template.
```

## Dependencies and Acknowledgements
This module uses `Codec.Picture` from `JuicyPixels` to render PNGs.

I previously made an inefficient nQueens solver thrown together from shower thoughts. I discovered a more powerful algorithm through [the University of Helsinki's Haskell MOOC course](https://haskell.mooc.fi/) (exercise set 9b). This module is based on their algorithm.

## Artifacts
Although the solution images are artifacts, I have included them for ease of use, and because the larger files take a long time to produce.