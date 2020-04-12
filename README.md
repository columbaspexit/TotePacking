# TotePacking
How many copies of a book (or other rectangular-shaped object) will fit in a box?

TotePacking is a function that takes in the dimensions of an item & the dimensions of a tote/container meant to hold multiple copies of said item and outputs an *estimate* of the maximum number of copies the tote will hold.

The heuristic for TotePacking is based on how someone might try and pack a tote themselves, plus the fuzzy memory from a long-ago linear algebra class that rotations of an object can be represented as matrix transformations. Along with R's inherent facility with matrices and vectorized operations, it felt like half the battle of getting to a working function was already won. Well, maybe 1/64th of the battle.




