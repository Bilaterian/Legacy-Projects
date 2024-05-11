# Legacy Projects
A collection of refactored legacy code from Fortran, Ada and COBOL.

COMPUTER CALCULATION OF FIRE DANGER - firedanger.f95

Code for calculting the likelihood of a forest fire through observing various variables.
Original code was in .for and only contained one subroutine, danger.
refactored code split up the subroutines and improved the facilitation of I/O the make it more user friendly.
Code was updated to .f95.

A GAME OF HANGMAN CONVERTED FROM FORTRAN TO ADA. - hangman.adb

The Fortran code for hangman was re-engineered to Ada via translation.
The purpose of this exercise was to document the usefulness of Ada for dealing with strings.
Ada does not handle variable lengths of string very well and expects all memory allocated to strings to be used.

DATA STATISTICS - statnew.cob

An older version of a COBOL file was re-engineered to a newver version.
Program calculates statistical information for a series of numbers stored in a file.
"go to" statements were removed as they allowed the computer to jump through the code.
Made liberal use of the "compute" keyword as it made arithmetic far easier than typing out english-like sentences.
Increased functionality by prompting for filenames for input and output files, and adding a number of new statistical functions.

FLESCH READABILITY INDEX - flesch.95

Translated a PL/I program to Fortran.
The program calculates the readability of any english literature through an index derived by the number of words per sentence and the number of syllables per sentence.
Added functionality that prompts the user for a text file to grade.
Assured the code was modular through the use of subroutines.
Added the Flesch-Kincaid grade level to the output as an additional function.