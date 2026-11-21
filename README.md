# MMBASIC-Port-of-Adventure-game

Port of Classiv Adventure C code from GITHUB repository https://github.com/troglobit/adventure

Port was accomploished using Claude.ai

<hr/>

Prompt used:
rewrite the Classic Adventure program from C code to MMBasic suitable to run on a picocalc
source code is located at https://github.com/troglobit/adventure/tree/master/src

<hr/>
Output from Claude:
I’ve ported the game to MMBasic. I couldn’t run it on a real PicoCalc, so the first thing to do is run it on yours.

I tested it under MMB4L (MMBasic for Linux) against the original C game. In both, I gave identical random commands with the random numbers fixed. The two programs’ output matched word for word in about 1,000 sessions. That covered dwarves, the pirate, death and reincarnation, the puzzle objects, and the endgame (cave closing, the repository, blast bonus). To reach the endgame I shortened the countdown timers in both programs.

Files

picocalc-adventure.zip contains:

ADVENT.BAS, the program (about 68 KB).
ADVENT1.TXT to ADVENT4.TXT, the unmodified text files from the C project.

Put all five files in the same folder on the SD card. Then run RUN "advent.bas". If the files aren’t in the current directory, set DATADIR$ near the top of the program, for example "B:/advent/".

How it works
Text stays on the SD card. The 47 KB of descriptions is read on demand rather than stored in the program. On startup the program scans the four files once to find where each message starts.
Tables are generated from the C source. The cave map, vocabulary and starting state were extracted by script from advcave.h, advword.h and advent.c, so nothing was retyped.
Screen fitting. It detects the screen size and re-wraps the hard-wrapped text to the screen width. A [more] pause appears when a screen fills. Save and restore use ADVENT.SAV, via SUSPEND and RESTORE.
Differences from the C version

The C version has bugs that I did not copy:

Long messages broke. Messages numbered above 127 printed nothing or garbage, for example break food, read, and feed.
back was broken. The C version copied only the first of the travel entries and could crash on special exits.
log could crash.

My version gives the correct behaviour in each case.

I dropped the C debug flags and command-line options. The random numbers are now seeded from the timer instead of a fixed seed.

What to check on the device

MMB4L isn’t a PicoMite, so these are the likeliest problems:

Whether MM.INFO(FONTWIDTH) works for screen detection. If it doesn’t, the program defaults to 39×25.
Whether SEEK and LINE INPUT on RANDOM files behave the same as in my tests.
Whether DIM x$() LENGTH n is available on your firmware, which needs a recent PicoMite.

If the text files are altered (for example CRLF line endings), the program stops at startup with a clear message instead of printing garbled text.
