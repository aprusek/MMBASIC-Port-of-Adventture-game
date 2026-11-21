' ============================================================================
'  ADVENT.BAS  -  Colossal Cave Adventure for the ClockworkPi PicoCalc
'                 (MMBasic / PicoMite)
'
'  A port of the C version at https://github.com/troglobit/adventure
'  (Crowther/Woods Adventure 6.x via J. R. Jaeger's BDS C conversion).
'  The original is public domain (Unlicense); so is this port.
'
'  FILES NEEDED (copy together onto the SD card / flash):
'      ADVENT.BAS   this program
'      ADVENT1.TXT  long location descriptions
'      ADVENT2.TXT  short location descriptions
'      ADVENT3.TXT  object descriptions
'      ADVENT4.TXT  messages
'  The four .TXT files are the unmodified files from the C project.
'
'  RUN:   LOAD "advent.bas"  then  RUN        (or RUN "advent.bas")
'  If the files are not in the current directory, set DATADIR$ below,
'  e.g.  DATADIR$ = "B:/advent/"
'
'  Commands are 1 or 2 words (GET LAMP, UNLOCK GRATE, N, IN ...).
'  "SUSPEND" (or SAVE/PAUSE) saves the game to ADVENT.SAV and exits.
'  "RESTORE" (or LOAD) reloads it.  "?" lists the known motion/verb words.
' ============================================================================
OPTION EXPLICIT
OPTION DEFAULT INTEGER
OPTION BASE 0

' ---- configuration ---------------------------------------------------------
DIM DATADIR$ = ""                 ' folder holding ADVENT1-4.TXT ("" = current)
DIM SAVEFILE$ = "advent.sav"

' ---- sizes -----------------------------------------------------------------
CONST MAXOBJ = 100
CONST MAXLOC = 140
CONST DWARFMAX = 7
CONST MAXDIE = 3
CONST MAXTRS = 79
CONST NW = 306                  ' vocabulary entries
CONST NTR = 740                ' travel table entries

' ---- objects ---------------------------------------------------------------
CONST oKEYS = 1, oLAMP = 2, oGRATE = 3, oCAGE = 4, oROD = 5, oROD2 = 6
CONST oSTEPS = 7, oBIRD = 8, oDOOR = 9, oPILLOW = 10, oSNAKE = 11
CONST oFISSURE = 12, oTABLET = 13, oCLAM = 14, oOYSTER = 15, oMAGAZINE = 16
CONST oDWARF = 17, oKNIFE = 18, oFOOD = 19, oBOTTLE = 20, oWATER = 21
CONST oOIL = 22, oMIRROR = 23, oPLANT = 24, oPLANT2 = 25, oAXE = 28
CONST oDRAGON = 31, oCHASM = 32, oTROLL = 33, oTROLL2 = 34, oBEAR = 35
CONST oMESSAGE = 36, oVEND = 38, oBATTERIES = 39, oNUGGET = 50, oCOINS = 54
CONST oCHEST = 55, oEGGS = 56, oTRIDENT = 57, oVASE = 58, oEMERALD = 59
CONST oPYRAMID = 60, oPEARL = 61, oRUG = 62, oSPICES = 63, oCHAIN = 64

' ---- motions ---------------------------------------------------------------
CONST mNULLX = 21, mBACK = 8, mLOOK = 57, mCAVE = 67
CONST mENTRANCE = 64, mDEPRESSION = 63

' ---- action verbs ----------------------------------------------------------
CONST vTAKE = 1, vDROP = 2, vSAY = 3, vOPEN = 4, vNOTHING = 5, vLOCK = 6
CONST vON = 7, vOFF = 8, vWAVE = 9, vCALM = 10, vWALK = 11, vKILL = 12
CONST vPOUR = 13, vEAT = 14, vDRINK = 15, vRUB = 16, vTHROW = 17, vQUIT = 18
CONST vFIND = 19, vINVENTORY = 20, vFEED = 21, vFILL = 22, vBLAST = 23
CONST vSCORE = 24, vFOO = 25, vBRIEF = 26, vREAD = 27, vBREAK = 28
CONST vWAKE = 29, vSUSPEND = 30, vHOURS = 31, vLOG = 32, vLOAD = 33

' ---- location condition bits -----------------------------------------------
CONST cLIGHT = 1, cWATOIL = 2, cLIQUID = 4, cNOPIRAT = 8

' ---- game state ------------------------------------------------------------
DIM cond(MAXLOC + 1), place(MAXOBJ), fixed(MAXOBJ), visited(MAXLOC + 1)
DIM prop(MAXOBJ), actmsg(34)
DIM dloc(DWARFMAX), odloc(DWARFMAX), dseen(DWARFMAX)
DIM verb, obj, mot, obj1
DIM turns, cur, oldloc, oldloc2, newloc
DIM tally, tally2, limit, lmwarn, wzdark, closing, closed
DIM holding, detail, knfloc, clock1, clock2, panic
DIM dflag, daltloc, dkill, chloc, chloc2, bonus, numdie
DIM gaveup, foobar, saveflg
DIM w1$ LENGTH 20, w2$ LENGTH 20
DIM atype, aval, adj

' ---- tables ----------------------------------------------------------------
DIM vw$(NW + 1) LENGTH 15          ' vocabulary words (sorted)
DIM vc(NW + 1)                     ' vocabulary codes
DIM tr(NTR + 1)                    ' travel entries: dest*1e6 + verb*1e3 + cond
DIM trs(MAXLOC + 2)                ' first travel entry of each location
DIM idx(4, 205)                    ' file offsets of each text record

' ---- screen / output -------------------------------------------------------
DIM scrw = 39, scrh = 25, lcount = 0
DIM ob$

' ============================================================================
'  MAIN
' ============================================================================
SetupScreen
PRINT "Loading..."
OpenFiles
BuildIndexes
InitPlay
LoadVocab
LoadCave
CLS
lcount = 0
IF Yes(65, 1, 0) THEN
  limit = 1000
ELSE
  limit = 330
ENDIF
RANDOMIZE TIMER
saveflg = 0
DO WHILE saveflg = 0
  DoTurn
LOOP
SaveGame
QuitGame
END

' ============================================================================
'  SETUP
' ============================================================================
SUB SetupScreen
  ON ERROR SKIP 1
  scrw = MM.HRES \ MM.INFO(FONTWIDTH) - 1
  ON ERROR SKIP 1
  scrh = MM.VRES \ MM.INFO(FONTHEIGHT)
  IF scrw < 20 OR scrw > 120 THEN scrw = 39
  IF scrh < 8 OR scrh > 100 THEN scrh = 25

END SUB

SUB OpenFiles
  LOCAL f, fname$
  FOR f = 1 TO 4
    fname$ = DATADIR$ + "advent" + STR$(f) + ".txt"
    IF DIR$(fname$, FILE) = "" THEN
      PRINT "Sorry, I cannot find "; fname$
      END
    ENDIF
    OPEN fname$ FOR RANDOM AS #f
  NEXT f
END SUB

SUB QuitGame
  CLOSE #1, #2, #3, #4
  END
END SUB

SUB Bug(n)
  Say "Fatal error number " + STR$(n)
  QuitGame
END SUB

' Scan each text file once and remember where every "#" record starts.
' Offsets are worked out from the line lengths (files must have LF line
' endings, as shipped) so nothing depends on LOC()/EOF() behaviour.
SUB BuildIndexes
  LOCAL f, n, l$, fpos
  FOR f = 1 TO 4
    n = 0
    fpos = 1
    SEEK #f, 1
    DO WHILE fpos <= LOF(#f)
      LINE INPUT #f, l$
      fpos = fpos + LEN(l$) + 1
      IF LEFT$(l$, 1) = "#" THEN
        n = n + 1
        idx(f, n) = fpos
      ENDIF
    LOOP
  NEXT f
  ' self-check: the last message of advent4.txt must be found where expected
  FOR adj = 0 TO 2
    SELECT CASE adj
      CASE 0
        fpos = 0
      CASE 1
        fpos = -1
      CASE ELSE
        fpos = 1
    END SELECT
    SEEK #4, idx(4, 201) + fpos
    LINE INPUT #4, l$
    IF LEFT$(l$, 20) = "There's no point in " THEN
      adj = fpos
      EXIT SUB
    ENDIF
  NEXT adj
  PRINT "The ADVENT*.TXT files do not look right."
  PRINT "They must be the unmodified files with LF line endings."
  QuitGame
END SUB

' Read n values from DATA into a(s) ... a(s+n-1)
SUB LoadDat(a(), s, n)
  LOCAL i
  FOR i = 0 TO n - 1
    READ a(s + i)
  NEXT i
END SUB

SUB LoadVocab
  LOCAL i
  FOR i = 0 TO NW - 1
    READ vw$(i), vc(i)
  NEXT i
END SUB

' Travel table: each location is a list of entries ending in -1
SUB LoadCave
  LOCAL l, n, v
  n = 0
  FOR l = 1 TO MAXLOC
    trs(l) = n
    DO
      READ v
      IF v = -1 THEN EXIT DO
      tr(n) = v
      n = n + 1
    LOOP
  NEXT l
  trs(MAXLOC + 1) = n
END SUB

SUB InitPlay
  LOCAL i
  turns = 0
  LoadDat cond(), 1, 10
  LoadDat cond(), 13, 8
  LoadDat cond(), 21, 6
  LoadDat cond(), 31, 10
  LoadDat cond(), 42, 9
  LoadDat cond(), 51, 9
  LoadDat cond(), 79, 12
  LoadDat cond(), 95, 6
  LoadDat cond(), 113, 4
  LoadDat cond(), 122, 9
  LoadDat place(), 1, 10
  LoadDat place(), 11, 10
  LoadDat place(), 23, 7
  LoadDat place(), 31, 10
  LoadDat place(), 50, 5
  LoadDat place(), 56, 9
  LoadDat fixed(), 3, 7
  LoadDat fixed(), 11, 7
  LoadDat fixed(), 23, 8
  LoadDat fixed(), 31, 10
  LoadDat fixed(), 62, 2
  LoadDat actmsg(), 0, 11
  LoadDat actmsg(), 11, 10
  LoadDat actmsg(), 21, 11
  LoadDat dloc(), 0, 7
  LoadDat odloc(), 0, 7
  LoadDat dseen(), 0, 7
  FOR i = 50 TO MAXOBJ - 1
    prop(i) = -1
  NEXT i
  wzdark = 0 : closed = 0 : closing = 0 : holding = 0 : detail = 0
  limit = 100 : tally = 15 : tally2 = 0 : newloc = 1
  cur = 3 : oldloc = 3 : oldloc2 = 3
  knfloc = 0 : chloc = 114 : chloc2 = 140
  dkill = 0 : clock1 = 30 : clock2 = 50 : panic = 0
  bonus = 0 : numdie = 0 : daltloc = 18 : lmwarn = 0
  foobar = 0 : dflag = 0 : gaveup = 0 : saveflg = 0
END SUB

' ============================================================================
'  OUTPUT: word-wrapping, paging
' ============================================================================
SUB OutLine(s$)
  PRINT s$
  lcount = lcount + 1
  IF lcount >= scrh - 1 THEN
    PRINT "[more]";
    DO
    LOOP UNTIL INKEY$ <> ""
    PRINT CHR$(13); "      "; CHR$(13);
    lcount = 0
  ENDIF
END SUB

SUB FlushOut
  IF ob$ <> "" THEN
    OutLine ob$
    ob$ = ""
  ENDIF
END SUB

SUB PutWord(w$)
  IF ob$ = "" THEN
    ob$ = w$
  ELSEIF LEN(ob$) + 1 + LEN(w$) <= scrw THEN
    ob$ = ob$ + " " + w$
  ELSE
    OutLine ob$
    ob$ = w$
  ENDIF
END SUB

' Append the words of t$ to the output line, wrapping at the screen width
SUB AddWords(t$)
  LOCAL p, q
  p = 1
  DO WHILE p <= LEN(t$)
    q = INSTR(p, t$, " ")
    IF q = 0 THEN
      PutWord MID$(t$, p)
      p = LEN(t$) + 1
    ELSE
      IF q > p THEN PutWord MID$(t$, p, q - p)
      p = q + 1
    ENDIF
  LOOP
END SUB

' One physical line of a text file.  The source files are hard-wrapped at
' about 60 columns; re-flow them to the screen width.  A blank line, an
' indented line, or a line ending in a space is kept as a real line break.
SUB AddRaw(l$)
  IF l$ = "" THEN
    FlushOut
    OutLine ""
  ELSEIF LEFT$(l$, 1) = " " THEN
    FlushOut
    IF LEN(l$) <= scrw THEN
      OutLine l$
    ELSE
      AddWords l$
      FlushOut
    ENDIF
  ELSE
    AddWords l$
    IF RIGHT$(l$, 1) = " " THEN FlushOut
  ENDIF
END SUB

' Print a complete line of text
SUB Say(s$)
  FlushOut
  AddWords s$
  FlushOut
END SUB

' Print a line verbatim (no re-flow)
SUB Raw(s$)
  FlushOut
  OutLine s$
END SUB

FUNCTION ReadLine$()
  LOCAL a$
  FlushOut
  PRINT "> ";
  LINE INPUT a$
  lcount = 0
  ReadLine$ = a$
END FUNCTION

' ============================================================================
'  DATABASE  (database.c)
' ============================================================================

' Print the record that starts at offset 'off' of text file #f
SUB Speak(f, off)
  LOCAL l$, fpos
  fpos = off
  SEEK #f, off + adj
  DO WHILE fpos <= LOF(#f)
    LINE INPUT #f, l$
    fpos = fpos + LEN(l$) + 1
    IF LEFT$(l$, 1) = "#" THEN EXIT DO
    AddRaw l$
  LOOP
  FlushOut
END SUB

SUB Rspeak(m)
  IF m = 54 THEN
    Say "ok."
  ELSE
    Speak 4, idx(4, m)
  ENDIF
END SUB

SUB Desclg(l)
  Speak 1, idx(1, l)
END SUB

SUB Descsh(l)
  Speak 2, idx(2, l)
END SUB

' Print the message for 'item' in state 'state' from advent3.txt.
' The record looks like  /name\n/state0 text\n/state1 text\n/
SUB Pspeak(item, state)
  LOCAL l$, n, cnt, p, q, done, fpos
  IF item > 64 THEN
    Bug 40
    EXIT SUB
  ENDIF
  n = state + 2
  cnt = 0
  done = 0
  fpos = idx(3, item)
  SEEK #3, fpos + adj
  DO WHILE done = 0 AND fpos <= LOF(#3)
    LINE INPUT #3, l$
    fpos = fpos + LEN(l$) + 1
    IF LEFT$(l$, 1) = "#" THEN EXIT DO
    p = 1
    DO
      q = INSTR(p, l$, "/")
      IF cnt < n THEN
        IF q = 0 THEN EXIT DO
        cnt = cnt + 1
        p = q + 1
      ELSE
        IF q = 0 THEN
          AddRaw MID$(l$, p)
        ELSE
          IF q > p THEN AddRaw MID$(l$, p, q - p)
          done = 1
        ENDIF
        EXIT DO
      ENDIF
    LOOP
  LOOP
  FlushOut
END SUB

' Binary search of the first n table entries (0-based), as in database.c
FUNCTION BinSrch(w$, n)
  LOCAL lo, hi, mid
  lo = 0 : hi = n - 1
  BinSrch = -1
  DO WHILE lo <= hi
    mid = (lo + hi) \ 2
    IF w$ < vw$(mid) THEN
      hi = mid - 1
    ELSEIF w$ > vw$(mid) THEN
      lo = mid + 1
    ELSE
      BinSrch = mid
      EXIT FUNCTION
    ENDIF
  LOOP
END FUNCTION

' Look up a word.  Returns -1 if unknown.  Some words have two entries with
' different codes.  If minval = 0 return the lowest code, else the code
' (modulo 1000) of the first entry that is >= minval.
FUNCTION Vocab(w$, minval)
  LOCAL v1, v2, c1, c2
  Vocab = -1
  v1 = BinSrch(w$, NW)
  IF v1 < 0 THEN EXIT FUNCTION
  v2 = BinSrch(w$, NW - 1)
  IF v2 < 0 THEN v2 = v1
  c1 = vc(v1) : c2 = vc(v2)
  IF minval = 0 THEN
    IF c1 < c2 THEN
      Vocab = c1
    ELSE
      Vocab = c2
    ENDIF
  ELSEIF minval <= c1 THEN
    Vocab = c1 MOD 1000
  ELSEIF minval <= c2 THEN
    Vocab = c2 MOD 1000
  ENDIF
END FUNCTION

' ---- utility routines -------------------------------------------------------
FUNCTION RandN(n)
  RandN = INT(RND * n)
END FUNCTION

FUNCTION Pct(x)
  Pct = (RandN(100) < x)
END FUNCTION

FUNCTION Toting(i)
  Toting = (place(i) = -1)
END FUNCTION

FUNCTION Here(i)
  Here = (place(i) = cur OR place(i) = -1)
END FUNCTION

FUNCTION AtObj(i)
  AtObj = (place(i) = cur OR fixed(i) = cur)
END FUNCTION

FUNCTION Forced(l)
  Forced = (cond(l) = 2)
END FUNCTION

FUNCTION Dark()
  Dark = 0
  IF (cond(cur) AND cLIGHT) = 0 THEN
    IF prop(oLAMP) = 0 THEN
      Dark = 1
    ELSEIF Here(oLAMP) = 0 THEN
      Dark = 1
    ENDIF
  ENDIF
END FUNCTION

SUB CarryObj(o, w)
  IF o < MAXOBJ THEN
    IF place(o) <> -1 THEN
      place(o) = -1
      holding = holding + 1
    ENDIF
  ENDIF
END SUB

SUB DropObj(o, w)
  IF o < MAXOBJ THEN
    IF place(o) = -1 THEN holding = holding - 1
    place(o) = w
  ELSE
    fixed(o - MAXOBJ) = w
  ENDIF
END SUB

SUB MoveObj(o, w)
  LOCAL srcloc
  IF o < MAXOBJ THEN
    srcloc = place(o)
  ELSE
    srcloc = fixed(o - MAXOBJ)
  ENDIF
  IF srcloc > 0 AND srcloc <= 300 THEN CarryObj o, srcloc
  DropObj o, w
END SUB

SUB Dstroy(o)
  MoveObj o, 0
END SUB

' Move an object and return the value used to set the negated prop values
FUNCTION PutObj(o, w, pval)
  MoveObj o, w
  PutObj = -1 - pval
END FUNCTION

' Is a dwarf (not the pirate) in the room?
FUNCTION DCheck()
  LOCAL i
  DCheck = 0
  FOR i = 1 TO DWARFMAX - 2
    IF dloc(i) = cur THEN
      DCheck = i
      EXIT FUNCTION
    ENDIF
  NEXT i
END FUNCTION

' 0 -> WATER, 1 -> nothing, 2 -> OIL
FUNCTION Liq2(p)
  Liq2 = (1 - p) * oWATER + (p \ 2) * (oWATER + oOIL)
END FUNCTION

FUNCTION Liq()
  LOCAL i, j
  i = prop(oBOTTLE)
  j = -1 - i
  IF i > j THEN
    Liq = Liq2(i)
  ELSE
    Liq = Liq2(j)
  ENDIF
END FUNCTION

FUNCTION LiqLoc(l)
  IF (cond(l) AND cLIQUID) <> 0 THEN
    LiqLoc = Liq2(cond(l) AND cWATOIL)
  ELSE
    LiqLoc = Liq2(1)
  ENDIF
END FUNCTION

FUNCTION Yes(m1, m2, m3)
  LOCAL a$
  IF m1 <> 0 THEN Rspeak m1
  a$ = ReadLine$()
  IF LCASE$(LEFT$(a$, 1)) = "n" THEN
    IF m3 <> 0 THEN Rspeak m3
    Yes = 0
  ELSE
    IF m2 <> 0 THEN Rspeak m2
    Yes = 1
  ENDIF
END FUNCTION

' ============================================================================
'  ENGLISH  (english.c)
' ============================================================================
SUB NextTok(a$, p, w$)
  LOCAL q
  w$ = ""
  DO WHILE p <= LEN(a$)
    IF MID$(a$, p, 1) <> " " THEN EXIT DO
    p = p + 1
  LOOP
  IF p > LEN(a$) THEN EXIT SUB
  q = INSTR(p, a$, " ")
  IF q = 0 THEN q = LEN(a$) + 1
  w$ = MID$(a$, p, q - p)
  IF LEN(w$) > 19 THEN w$ = LEFT$(w$, 19)
  p = q
END SUB

SUB GetWords
  LOCAL a$, p, i, c$
  a$ = LCASE$(ReadLine$())
  FOR i = 1 TO LEN(a$)                ' tabs / CR -> space
    c$ = MID$(a$, i, 1)
    IF c$ = CHR$(9) OR c$ = CHR$(13) THEN a$ = LEFT$(a$, i - 1) + " " + MID$(a$, i + 1)
  NEXT i
  p = 1
  NextTok a$, p, w1$
  NextTok a$, p, w2$
END SUB

' Look up one word, setting atype/aval.  Returns 0 if unknown.
FUNCTION Analyze(w$)
  LOCAL wv, m
  wv = Vocab(w$, 0)
  IF wv = -1 THEN
    SELECT CASE RandN(3)
      CASE 0
        m = 60
      CASE 1
        m = 61
      CASE ELSE
        m = 13
    END SELECT
    Rspeak m
    Analyze = 0
  ELSE
    atype = wv \ 1000
    aval = wv MOD 1000
    Analyze = 1
  ENDIF
END FUNCTION

' Word-type of a known word without any side effects
FUNCTION WType(w$)
  LOCAL wv
  wv = Vocab(w$, 0)
  IF wv = -1 THEN
    WType = -1
  ELSE
    WType = wv \ 1000
  ENDIF
END FUNCTION

FUNCTION ProbJ$()
  IF WType(w1$) = 1 THEN
    ProbJ$ = w1$
  ELSE
    ProbJ$ = w2$
  ENDIF
END FUNCTION

' List the motion and verb words
SUB OutWords
  LOCAL i, j, per
  per = scrw \ 12
  IF per < 1 THEN per = 1
  j = 0
  FOR i = 0 TO NW - 1
    IF vc(i) < 1000 OR (vc(i) < 3000 AND vc(i) > 1999) THEN
      ob$ = ob$ + LEFT$(vw$(i) + SPACE$(12), 12)
      j = j + 1
      IF j = per THEN
        FlushOut
        j = 0
      ENDIF
    ENDIF
  NEXT i
  FlushOut
END SUB

' Analyze a two word sentence.  Returns 1 if verb/object/motion were set.
FUNCTION English()
  LOCAL t1, t2, v1, v2
  verb = 0 : obj = 0 : mot = 0
  t1 = -1 : v1 = -1 : t2 = -1 : v2 = -1
  English = 0
  GetWords
  IF w1$ = "" THEN EXIT FUNCTION
  IF Analyze(w1$) = 0 THEN EXIT FUNCTION
  t1 = atype : v1 = aval
  IF t1 = 2 AND v1 = vSAY THEN
    verb = vSAY
    obj = 1
    English = 1
    EXIT FUNCTION
  ENDIF
  IF w2$ <> "" THEN
    IF Analyze(w2$) = 0 THEN EXIT FUNCTION
    t2 = atype : v2 = aval
  ENDIF
  IF t1 = 3 AND t2 = 3 AND v1 = 51 AND v2 = 51 THEN
    OutWords
    EXIT FUNCTION
  ELSEIF t1 = 3 THEN
    Rspeak v1
    EXIT FUNCTION
  ELSEIF t2 = 3 THEN
    Rspeak v2
    EXIT FUNCTION
  ELSEIF t1 = 0 THEN
    IF t2 = 0 THEN
      Say "bad grammar..."
      EXIT FUNCTION
    ENDIF
    mot = v1
  ELSEIF t2 = 0 THEN
    mot = v2
  ELSEIF t1 = 1 THEN
    obj = v1
    IF t2 = 2 THEN verb = v2
    IF t2 = 1 THEN
      Say "bad grammar..."
      EXIT FUNCTION
    ENDIF
  ELSEIF t1 = 2 THEN
    verb = v1
    IF t2 = 1 THEN obj = v2
    IF t2 = 2 THEN
      Say "bad grammar..."
      EXIT FUNCTION
    ENDIF
  ELSE
    Bug 36
  ENDIF
  English = 1
END FUNCTION

' ============================================================================
'  TURN  (turn.c)
' ============================================================================
SUB DoTurn
  LOCAL i, ok, dummy
  ' if closing, he can't leave except via the main office
  IF newloc < 9 AND newloc <> 0 AND closing <> 0 THEN
    Rspeak 130
    newloc = cur
    IF panic = 0 THEN clock2 = 15
    panic = 1
  ENDIF
  ' see if a dwarf has seen him and has come from where he wants to go
  IF newloc <> cur AND Forced(cur) = 0 AND (cond(cur) AND cNOPIRAT) = 0 THEN
    FOR i = 1 TO DWARFMAX - 2
      IF odloc(i) = newloc AND dseen(i) <> 0 THEN
        newloc = cur
        Rspeak 2
        EXIT FOR
      ENDIF
    NEXT i
  ENDIF

  Dwarves                             ' & special dwarf (pirate who steals)

  IF cur <> newloc THEN
    turns = turns + 1
    cur = newloc
    IF cur = 0 THEN                   ' check for death
      Death
      EXIT SUB
    ENDIF
    IF Forced(cur) THEN               ' check for forced move
      Describe
      DoMove
      EXIT SUB
    ENDIF
    IF wzdark <> 0 THEN               ' check for wandering in dark
      IF Dark() <> 0 THEN
        IF Pct(35) THEN
          Rspeak 23
          oldloc2 = cur
          Death
          EXIT SUB
        ENDIF
      ENDIF
    ENDIF
    Describe
    IF Dark() = 0 THEN
      visited(cur) = visited(cur) + 1
      DescItem
    ENDIF
  ENDIF

  IF closed <> 0 THEN
    IF prop(oOYSTER) < 0 AND Toting(oOYSTER) THEN Pspeak oOYSTER, 1
    FOR i = 1 TO MAXOBJ - 1
      IF Toting(i) AND prop(i) < 0 THEN prop(i) = -1 - prop(i)
    NEXT i
  ENDIF

  wzdark = Dark()
  IF knfloc > 0 AND knfloc <> cur THEN knfloc = 0

  IF STimer() <> 0 THEN EXIT SUB      ' as the grains of sand slip by

  DO                                  ' retrieve player instructions
    ok = English()
  LOOP UNTIL ok <> 0

  IF mot <> 0 THEN                    ' execute player instructions
    DoMove
  ELSEIF obj <> 0 THEN
    DoObj
  ELSE
    ItVerb
  ENDIF
END SUB

SUB Describe
  IF Toting(oBEAR) THEN Rspeak 141
  IF Dark() <> 0 THEN
    Rspeak 16
  ELSEIF visited(cur) <> 0 THEN
    Descsh cur
  ELSE
    Desclg cur
  ENDIF
  IF cur = 33 THEN
    IF closing = 0 THEN
      IF Pct(25) THEN Rspeak 8
    ENDIF
  ENDIF
END SUB

SUB DescItem
  LOCAL i, state, skip
  FOR i = 1 TO MAXOBJ - 1
    IF AtObj(i) THEN
      skip = 0
      IF i = oSTEPS THEN
        IF Toting(oNUGGET) THEN skip = 1
      ENDIF
      IF skip = 0 THEN
        IF prop(i) < 0 THEN
          IF closed <> 0 THEN
            skip = 1
          ELSE
            prop(i) = 0
            IF i = oRUG OR i = oCHAIN THEN prop(i) = prop(i) + 1
            tally = tally - 1
          ENDIF
        ENDIF
      ENDIF
      IF skip = 0 THEN
        IF i = oSTEPS AND cur = fixed(oSTEPS) THEN
          state = 1
        ELSE
          state = prop(i)
        ENDIF
        Pspeak i, state
      ENDIF
    ENDIF
  NEXT i
  IF tally = tally2 AND tally <> 0 AND limit > 35 THEN limit = 35
END SUB

' Handle motion requests
SUB DoMove
  SELECT CASE mot
    CASE mNULLX
      ' nothing
    CASE mBACK
      GoBack
    CASE mLOOK
      IF detail < 3 THEN Rspeak 15
      detail = detail + 1
      wzdark = 0
      visited(cur) = 0
      newloc = cur
      cur = 0
    CASE mCAVE
      IF cur < 8 THEN
        Rspeak 57
      ELSE
        Rspeak 58
      ENDIF
    CASE ELSE
      oldloc2 = oldloc
      oldloc = cur
      DoTrav
  END SELECT
END SUB

' Return from whence we came.  Look for a motion that goes from here to the
' previous location, or to a forced location that leads to it.
SUB GoBack
  LOCAL kk, k2, want, d, c, t, f
  IF Forced(oldloc) THEN
    want = oldloc2
  ELSE
    want = oldloc
  ENDIF
  oldloc2 = oldloc
  oldloc = cur
  k2 = -1
  IF want = cur THEN
    Rspeak 91
    EXIT SUB
  ENDIF
  FOR kk = trs(cur) TO trs(cur + 1) - 1
    t = tr(kk)
    d = t \ 1000000
    c = t MOD 1000
    IF c = 0 THEN
      IF d = want THEN
        mot = (t \ 1000) MOD 1000
        DoTrav
        EXIT SUB
      ENDIF
      IF d >= 1 AND d <= 300 THEN
        IF Forced(d) THEN
          f = tr(trs(d))
          IF f \ 1000000 = want THEN k2 = kk
        ENDIF
      ENDIF
    ENDIF
  NEXT kk
  IF k2 >= 0 THEN
    mot = (tr(k2) \ 1000) MOD 1000
    DoTrav
  ELSE
    Rspeak 140
  ENDIF
END SUB

' Work out a new location given the current location and a motion
SUB DoTrav
  LOCAL mvflag, hitflag, rdest, rverb, rcond, robject, pctt, kk, last, t
  newloc = cur
  mvflag = 0 : hitflag = 0 : rdest = 0
  pctt = RandN(100)
  kk = trs(cur)
  last = trs(cur + 1) - 1
  DO WHILE kk <= last AND mvflag = 0
    t = tr(kk)
    rdest = t \ 1000000
    rverb = (t \ 1000) MOD 1000
    rcond = t MOD 1000
    robject = rcond MOD 100
    IF rverb <> 1 AND rverb <> mot AND hitflag = 0 THEN
      ' not this motion - keep looking
    ELSE
      hitflag = hitflag + 1
      SELECT CASE rcond \ 100
        CASE 0
          IF rcond = 0 THEN
            mvflag = mvflag + 1
          ELSEIF pctt < rcond THEN
            mvflag = mvflag + 1
          ENDIF
        CASE 1
          IF robject = 0 THEN
            mvflag = mvflag + 1
          ELSEIF Toting(robject) THEN
            mvflag = mvflag + 1
          ENDIF
        CASE 2
          IF Toting(robject) THEN
            mvflag = mvflag + 1
          ELSEIF AtObj(robject) THEN
            mvflag = mvflag + 1
          ENDIF
        CASE 3, 4, 5, 7
          IF prop(robject) <> (rcond \ 100) - 3 THEN mvflag = mvflag + 1
        CASE ELSE
          Bug 37
      END SELECT
    ENDIF
    kk = kk + 1
  LOOP
  IF mvflag = 0 THEN
    BadMove
  ELSEIF rdest > 500 THEN
    Rspeak rdest - 500
  ELSEIF rdest > 300 THEN
    SpcMove rdest
  ELSE
    newloc = rdest
  ENDIF
END SUB

' The player tried a poor move option
SUB BadMove
  LOCAL m
  m = 12
  IF mot >= 43 AND mot <= 50 THEN m = 9
  IF mot = 29 OR mot = 30 THEN m = 9
  IF mot = 7 OR mot = 36 OR mot = 37 THEN m = 10
  IF mot = 11 OR mot = 19 THEN m = 11
  IF verb = vFIND OR verb = vINVENTORY THEN m = 59
  IF mot = 62 OR mot = 65 THEN m = 42
  IF mot = 17 THEN m = 80
  Rspeak m
END SUB

' Very special movement
SUB SpcMove(rdest)
  SELECT CASE rdest - 300
    CASE 1                            ' plover movement via alcove
      IF holding = 0 THEN
        newloc = (99 + 100) - cur
      ELSEIF holding = 1 AND Toting(oEMERALD) THEN
        newloc = (99 + 100) - cur
      ELSE
        Rspeak 117
      ENDIF
    CASE 2                            ' trying to remove plover, bad route
      DropObj oEMERALD, cur
    CASE 3                            ' troll bridge
      IF prop(oTROLL) = 1 THEN
        Pspeak oTROLL, 1
        prop(oTROLL) = 0
        MoveObj oTROLL2, 0
        MoveObj oTROLL2 + MAXOBJ, 0
        MoveObj oTROLL, 117
        MoveObj oTROLL + MAXOBJ, 122
        newloc = cur
      ELSE
        IF cur = 117 THEN
          newloc = 122
        ELSE
          newloc = 117
        ENDIF
        IF prop(oTROLL) = 0 THEN prop(oTROLL) = prop(oTROLL) + 1
        IF Toting(oBEAR) = 0 THEN EXIT SUB
        Rspeak 162
        prop(oCHASM) = 1
        prop(oTROLL) = 2
        DropObj oBEAR, newloc
        fixed(oBEAR) = -1
        prop(oBEAR) = 3
        IF prop(oSPICES) < 0 THEN tally2 = tally2 + 1
        oldloc2 = newloc
        Death
      ENDIF
    CASE ELSE
      Bug 38
  END SELECT
END SUB

' Death via waking up the dwarves
SUB DwarfEnd
  Death
  NormEnd
END SUB

SUB NormEnd
  Score
  QuitGame
END SUB

SUB Score
  LOCAL t, i, k, s
  s = 0 : t = 0 : k = 0
  FOR i = 50 TO MAXTRS
    IF i = oCHEST THEN
      k = 14
    ELSEIF i > oCHEST THEN
      k = 16
    ELSE
      k = 12
    ENDIF
    IF prop(i) >= 0 THEN t = t + 2
    IF place(i) = 3 AND prop(i) = 0 THEN t = t + k - 2
  NEXT i
  s = t
  Raw LEFT$("Treasures:" + SPACE$(20), 20) + STR$(s)
  t = (MAXDIE - numdie) * 10
  IF t <> 0 THEN Raw LEFT$("Survival:" + SPACE$(20), 20) + STR$(t)
  s = s + t
  IF gaveup = 0 THEN s = s + 4
  IF dflag <> 0 THEN
    t = 25
  ELSE
    t = 0
  ENDIF
  IF t <> 0 THEN Raw LEFT$("Getting well in:" + SPACE$(20), 20) + STR$(t)
  s = s + t
  IF closing <> 0 THEN
    t = 25
  ELSE
    t = 0
  ENDIF
  IF t <> 0 THEN Raw LEFT$("Masters section:" + SPACE$(20), 20) + STR$(t)
  s = s + t
  IF closed <> 0 THEN
    IF bonus = 0 THEN
      t = 10
    ELSEIF bonus = 135 THEN
      t = 25
    ELSEIF bonus = 134 THEN
      t = 30
    ELSEIF bonus = 133 THEN
      t = 45
    ENDIF
    Raw LEFT$("Bonus:" + SPACE$(20), 20) + STR$(t)
    s = s + t
  ENDIF
  IF place(oMAGAZINE) = 108 THEN s = s + 1
  s = s + 2
  Raw LEFT$("Score:" + SPACE$(20), 20) + STR$(s)
END SUB

' The passing on of one of the player's incarnations
SUB Death
  LOCAL yea, i, j
  IF closing = 0 THEN
    yea = Yes(81 + numdie * 2, 82 + numdie * 2, 54)
    numdie = numdie + 1
    IF numdie >= MAXDIE THEN NormEnd
    IF yea = 0 THEN NormEnd
    place(oWATER) = 0
    place(oOIL) = 0
    IF Toting(oLAMP) THEN prop(oLAMP) = 0
    FOR j = 1 TO MAXOBJ - 1
      i = MAXOBJ - j
      IF Toting(i) THEN
        IF i = oLAMP THEN
          DropObj i, 1
        ELSE
          DropObj i, oldloc2
        ENDIF
      ENDIF
    NEXT j
    newloc = 3
    oldloc = cur
    EXIT SUB
  ENDIF
  ' closing -- no resurrection...
  Rspeak 131
  numdie = numdie + 1
  NormEnd
END SUB

' Process an object
SUB DoObj
  IF fixed(obj) = cur OR Here(obj) <> 0 THEN
    TrObj
  ELSEIF obj = oGRATE THEN            ' did he give grate as destination?
    IF cur = 1 OR cur = 4 OR cur = 7 THEN
      mot = mDEPRESSION
      DoMove
    ELSEIF cur > 9 AND cur < 15 THEN
      mot = mENTRANCE
      DoMove
    ENDIF
  ELSEIF DCheck() <> 0 AND dflag >= 2 THEN   ' is it a dwarf he is after?
    obj = oDWARF
    TrObj
  ELSEIF (Liq() = obj AND Here(oBOTTLE) <> 0) OR LiqLoc(cur) = obj THEN
    TrObj                             ' is he trying to get/use a liquid?
  ELSEIF obj = oPLANT AND AtObj(oPLANT2) <> 0 AND prop(oPLANT2) = 0 THEN
    obj = oPLANT2
    TrObj
  ELSEIF obj = oKNIFE AND knfloc = cur THEN   ' grab a knife?
    Rspeak 116
    knfloc = -1
  ELSEIF obj = oROD AND Here(oROD2) <> 0 THEN ' get at dynamite?
    obj = oROD2
    TrObj
  ELSE
    Say "I see no " + ProbJ$() + " here."
  ENDIF
END SUB

SUB TrObj
  IF verb <> 0 THEN
    TrVerb
  ELSE
    Say "What do you want to do with the " + ProbJ$() + "?"
  ENDIF
END SUB

' Dwarf stuff
SUB Dwarves
  LOCAL i, j, k, try, attack, stick, dtotal
  ' see if dwarves allowed here
  IF newloc = 0 THEN EXIT SUB
  IF Forced(newloc) <> 0 THEN EXIT SUB
  IF (cond(newloc) AND cNOPIRAT) <> 0 THEN EXIT SUB
  ' see if dwarves are active
  IF dflag = 0 THEN
    IF newloc > 15 THEN dflag = dflag + 1
    EXIT SUB
  ENDIF
  ' first close encounter (of 3rd kind): kill 0, 1 or 2
  IF dflag = 1 THEN
    IF newloc < 15 THEN EXIT SUB
    IF Pct(95) THEN EXIT SUB
    dflag = dflag + 1
    FOR i = 1 TO 2
      IF Pct(50) THEN dloc(RandN(5) + 1) = 0
    NEXT i
    FOR i = 1 TO DWARFMAX - 2
      IF dloc(i) = newloc THEN dloc(i) = daltloc
      odloc(i) = dloc(i)
    NEXT i
    Rspeak 3
    DropObj oAXE, newloc
    EXIT SUB
  ENDIF
  dtotal = 0 : attack = 0 : stick = 0
  FOR i = 1 TO DWARFMAX - 1
    IF dloc(i) <> 0 THEN
      ' move a dwarf at random
      FOR try = 1 TO 19
        j = RandN(106) + 15           ' allowed area
        IF j <> odloc(i) AND j <> dloc(i) THEN
          IF i <> DWARFMAX - 1 THEN EXIT FOR
          IF (cond(j) AND cNOPIRAT) <> cNOPIRAT THEN EXIT FOR
        ENDIF
      NEXT try
      IF j = 0 THEN j = odloc(i)
      odloc(i) = dloc(i)
      dloc(i) = j
      IF dseen(i) <> 0 AND newloc >= 15 THEN
        dseen(i) = 1
      ELSEIF dloc(i) = newloc OR odloc(i) = newloc THEN
        dseen(i) = 1
      ELSE
        dseen(i) = 0
      ENDIF
      IF dseen(i) <> 0 THEN
        dloc(i) = newloc
        IF i = 6 THEN
          DoPirate
        ELSE
          dtotal = dtotal + 1
          IF odloc(i) = dloc(i) THEN
            attack = attack + 1
            IF knfloc >= 0 THEN knfloc = newloc
            IF RandN(1000) < 95 * (dflag - 2) THEN stick = stick + 1
          ENDIF
        ENDIF
      ENDIF
    ENDIF
  NEXT i
  IF dtotal = 0 THEN EXIT SUB
  IF dtotal > 1 THEN
    Say "There are " + STR$(dtotal) + " threatening little dwarves in the room with you!"
  ELSE
    Rspeak 4
  ENDIF
  IF attack = 0 THEN EXIT SUB
  IF dflag = 2 THEN dflag = dflag + 1
  IF attack > 1 THEN
    Say STR$(attack) + " of them throw knives at you!!"
    k = 6
  ELSE
    Rspeak 5
    k = 52
  ENDIF
  IF stick <= 1 THEN
    Rspeak stick + k
    IF stick = 0 THEN EXIT SUB
  ELSE
    Say STR$(stick) + " of them get you !!!"
  ENDIF
  oldloc2 = newloc
  Death
END SUB

' Pirate stuff
SUB DoPirate
  LOCAL j, k, steal
  IF newloc = chloc THEN EXIT SUB
  IF prop(oCHEST) >= 0 THEN EXIT SUB
  k = 0 : steal = 0
  FOR j = 50 TO MAXTRS
    IF j <> oPYRAMID OR (newloc <> place(oPYRAMID) AND newloc <> place(oEMERALD)) THEN
      IF Toting(j) THEN
        steal = 1
        EXIT FOR
      ENDIF
      IF Here(j) THEN k = k + 1
    ENDIF
  NEXT j
  IF steal = 0 THEN
    IF tally = tally2 + 1 AND k = 0 AND place(oCHEST) = 0 AND Here(oLAMP) <> 0 AND prop(oLAMP) = 1 THEN
      Rspeak 186
      MoveObj oCHEST, chloc
      MoveObj oMESSAGE, chloc2
      dloc(6) = chloc
      odloc(6) = chloc
      dseen(6) = 0
      EXIT SUB
    ENDIF
    IF odloc(6) <> dloc(6) THEN
      IF Pct(20) THEN Rspeak 127
    ENDIF
    EXIT SUB
  ENDIF
  ' steal it
  Rspeak 128
  IF place(oMESSAGE) = 0 THEN MoveObj oCHEST, chloc
  MoveObj oMESSAGE, chloc2
  FOR j = 50 TO MAXTRS
    IF j = oPYRAMID AND (newloc = place(oPYRAMID) OR newloc = place(oEMERALD)) THEN
      ' leave the pyramid / emerald alone
    ELSE
      IF AtObj(j) <> 0 AND fixed(j) = 0 THEN CarryObj j, newloc
      IF Toting(j) THEN DropObj j, chloc
    ENDIF
  NEXT j
  dloc(6) = chloc
  odloc(6) = chloc
  dseen(6) = 0
END SUB

' Special time limit stuff.  Returns 1 when the cave has just closed.
FUNCTION STimer()
  LOCAL i
  STimer = 0
  IF foobar > 0 THEN
    foobar = -foobar
  ELSE
    foobar = 0
  ENDIF
  IF tally = 0 AND cur >= 15 AND cur <> 33 THEN clock1 = clock1 - 1
  IF clock1 = 0 THEN
    ' start closing the cave
    prop(oGRATE) = 0
    prop(oFISSURE) = 0
    FOR i = 1 TO DWARFMAX - 1
      dseen(i) = 0
    NEXT i
    MoveObj oTROLL, 0
    MoveObj oTROLL + MAXOBJ, 0
    MoveObj oTROLL2, 117
    MoveObj oTROLL2 + MAXOBJ, 122
    IF prop(oBEAR) <> 3 THEN Dstroy oBEAR
    prop(oCHAIN) = 0
    fixed(oCHAIN) = 0
    prop(oAXE) = 0
    fixed(oAXE) = 0
    Rspeak 129
    clock1 = -1
    closing = 1
    EXIT FUNCTION
  ENDIF
  IF clock1 < 0 THEN clock2 = clock2 - 1
  IF clock2 = 0 THEN
    ' set up storage room... and close the cave...
    prop(oBOTTLE) = PutObj(oBOTTLE, 115, 1)
    prop(oPLANT) = PutObj(oPLANT, 115, 0)
    prop(oOYSTER) = PutObj(oOYSTER, 115, 0)
    prop(oLAMP) = PutObj(oLAMP, 115, 0)
    prop(oROD) = PutObj(oROD, 115, 0)
    prop(oDWARF) = PutObj(oDWARF, 115, 0)
    cur = 115
    oldloc = 115
    newloc = 115
    i = PutObj(oGRATE, 116, 0)
    prop(oSNAKE) = PutObj(oSNAKE, 116, 1)
    prop(oBIRD) = PutObj(oBIRD, 116, 1)
    prop(oCAGE) = PutObj(oCAGE, 116, 0)
    prop(oROD2) = PutObj(oROD2, 116, 0)
    prop(oPILLOW) = PutObj(oPILLOW, 116, 0)
    prop(oMIRROR) = PutObj(oMIRROR, 115, 0)
    fixed(oMIRROR) = 116
    FOR i = 1 TO MAXOBJ - 1
      IF Toting(i) THEN Dstroy i
    NEXT i
    Rspeak 132
    closed = 1
    STimer = 1
    EXIT FUNCTION
  ENDIF
  IF prop(oLAMP) = 1 THEN limit = limit - 1
  IF limit <= 30 AND Here(oBATTERIES) <> 0 AND prop(oBATTERIES) = 0 AND Here(oLAMP) <> 0 THEN
    Rspeak 188
    prop(oBATTERIES) = 1
    IF Toting(oBATTERIES) THEN DropObj oBATTERIES, cur
    limit = limit + 2500
    lmwarn = 0
    EXIT FUNCTION
  ENDIF
  IF limit = 0 THEN
    limit = limit - 1
    prop(oLAMP) = 0
    IF Here(oLAMP) THEN Rspeak 184
    EXIT FUNCTION
  ENDIF
  IF limit < 0 AND cur <= 8 THEN
    Rspeak 185
    gaveup = 1
    NormEnd
  ENDIF
  IF limit <= 30 THEN
    IF lmwarn <> 0 THEN EXIT FUNCTION
    IF Here(oLAMP) = 0 THEN EXIT FUNCTION
    lmwarn = 1
    i = 187
    IF place(oBATTERIES) = 0 THEN i = 183
    IF prop(oBATTERIES) = 1 THEN i = 189
    Rspeak i
  ENDIF
END FUNCTION

' ============================================================================
'  INTRANSITIVE VERBS  (itverb.c)
' ============================================================================
SUB ItVerb
  SELECT CASE verb
    CASE vDROP, vSAY, vWAVE, vCALM, vRUB, vTHROW, vFIND, vFEED, vBREAK, vWAKE
      NeedObj
    CASE vTAKE
      IvTake
    CASE vOPEN, vLOCK
      IvOpen
    CASE vNOTHING
      Rspeak 54
    CASE vON, vOFF, vPOUR
      TrVerb
    CASE vWALK
      ActSpk verb
    CASE vKILL
      IvKill
    CASE vEAT
      IvEat
    CASE vDRINK
      IvDrink
    CASE vQUIT
      IvQuit
    CASE vFILL
      IvFill
    CASE vBLAST
      VbBlast
    CASE vSCORE
      Score
    CASE vFOO
      IvFoo
    CASE vSUSPEND
      saveflg = 1
    CASE vINVENTORY
      Inventory
    CASE vLOAD
      RestoreGame
    CASE ELSE
      Say "This intransitive not implemented yet"
  END SELECT
END SUB

' CARRY, TAKE etc.
SUB IvTake
  LOCAL anobj, item
  anobj = 0
  FOR item = 1 TO MAXOBJ - 1
    IF place(item) = cur THEN
      IF anobj <> 0 THEN
        NeedObj
        EXIT SUB
      ENDIF
      anobj = item
    ENDIF
  NEXT item
  IF anobj = 0 THEN
    NeedObj
    EXIT SUB
  ENDIF
  IF DCheck() <> 0 AND dflag >= 2 THEN
    NeedObj
    EXIT SUB
  ENDIF
  obj = anobj
  VbTake
END SUB

' OPEN, LOCK, UNLOCK
SUB IvOpen
  IF Here(oCLAM) THEN obj = oCLAM
  IF Here(oOYSTER) THEN obj = oOYSTER
  IF AtObj(oDOOR) THEN obj = oDOOR
  IF AtObj(oGRATE) THEN obj = oGRATE
  IF Here(oCHAIN) THEN
    IF obj <> 0 THEN
      NeedObj
      EXIT SUB
    ENDIF
    obj = oCHAIN
  ENDIF
  IF obj = 0 THEN
    Rspeak 28
    EXIT SUB
  ENDIF
  VbOpen
END SUB

' ATTACK, KILL etc
SUB IvKill
  obj1 = 0
  IF DCheck() <> 0 AND dflag >= 2 THEN obj = oDWARF
  IF Here(oSNAKE) THEN AddObj oSNAKE
  IF AtObj(oDRAGON) AND prop(oDRAGON) = 0 THEN AddObj oDRAGON
  IF AtObj(oTROLL) THEN AddObj oTROLL
  IF Here(oBEAR) AND prop(oBEAR) = 0 THEN AddObj oBEAR
  IF obj1 <> 0 THEN
    NeedObj
    EXIT SUB
  ENDIF
  IF obj <> 0 THEN
    VbKill
    EXIT SUB
  ENDIF
  IF Here(oBIRD) AND verb <> vTHROW THEN obj = oBIRD
  IF Here(oCLAM) OR Here(oOYSTER) THEN AddObj oCLAM
  IF obj1 <> 0 THEN
    NeedObj
    EXIT SUB
  ENDIF
  VbKill
END SUB

SUB IvEat
  IF Here(oFOOD) = 0 THEN
    NeedObj
  ELSE
    obj = oFOOD
    VbEat
  ENDIF
END SUB

SUB IvDrink
  IF LiqLoc(cur) <> oWATER AND (Liq() <> oWATER OR Here(oBOTTLE) = 0) THEN
    NeedObj
  ELSE
    obj = oWATER
    VbDrink
  ENDIF
END SUB

SUB IvQuit
  gaveup = Yes(22, 54, 54)
  IF gaveup <> 0 THEN NormEnd
END SUB

SUB IvFill
  IF Here(oBOTTLE) = 0 THEN
    NeedObj
  ELSE
    obj = oBOTTLE
    VbFill
  ENDIF
END SUB

' Handle fee fie foe foo...
SUB IvFoo
  LOCAL k, m
  k = Vocab(w1$, 3000)
  m = 42
  IF foobar <> 1 - k THEN
    IF foobar <> 0 THEN m = 151
    Rspeak m
    EXIT SUB
  ENDIF
  foobar = k
  IF k <> 4 THEN EXIT SUB
  foobar = 0
  IF place(oEGGS) = 92 THEN
    Rspeak m
    EXIT SUB
  ENDIF
  IF Toting(oEGGS) AND cur = 92 THEN
    Rspeak m
    EXIT SUB
  ENDIF
  IF place(oEGGS) = 0 AND place(oTROLL) = 0 AND prop(oTROLL) = 0 THEN prop(oTROLL) = 1
  IF Here(oEGGS) THEN
    k = 1
  ELSEIF cur = 92 THEN
    k = 0
  ELSE
    k = 2
  ENDIF
  MoveObj oEGGS, 92
  Pspeak oEGGS, k
END SUB

SUB Inventory
  LOCAL i, m
  m = 98
  FOR i = 1 TO 64
    IF i <> oBEAR AND Toting(i) THEN
      IF m <> 0 THEN Rspeak 99
      m = 0
      Pspeak i, -1
    ENDIF
  NEXT i
  IF Toting(oBEAR) THEN m = 141
  IF m <> 0 THEN Rspeak m
END SUB

' Ensure uniqueness as objects are searched out for an intransitive verb
SUB AddObj(o)
  IF obj1 <> 0 THEN EXIT SUB
  IF obj <> 0 THEN
    obj1 = -1
    EXIT SUB
  ENDIF
  obj = o
END SUB

' ============================================================================
'  TRANSITIVE VERBS  (verb.c)
' ============================================================================
SUB TrVerb
  SELECT CASE verb
    CASE vCALM, vWALK, vQUIT, vSCORE, vFOO, vBRIEF, vSUSPEND, vHOURS, vLOG
      ActSpk verb
    CASE vTAKE
      VbTake
    CASE vDROP
      VbDrop
    CASE vOPEN, vLOCK
      VbOpen
    CASE vSAY
      VbSay
    CASE vNOTHING
      Rspeak 54
    CASE vON
      VbOn
    CASE vOFF
      VbOff
    CASE vWAVE
      VbWave
    CASE vKILL
      VbKill
    CASE vPOUR
      VbPour
    CASE vEAT
      VbEat
    CASE vDRINK
      VbDrink
    CASE vRUB
      IF obj <> oLAMP THEN
        Rspeak 76
      ELSE
        ActSpk vRUB
      ENDIF
    CASE vTHROW
      VbThrow
    CASE vFEED
      VbFeed
    CASE vFIND, vINVENTORY
      VbFind
    CASE vFILL
      VbFill
    CASE vREAD
      VbRead
    CASE vBLAST
      VbBlast
    CASE vBREAK
      VbBreak
    CASE vWAKE
      VbWake
    CASE ELSE
      Say "This verb is not implemented yet."
  END SELECT
END SUB

' CARRY TAKE etc.
SUB VbTake
  LOCAL m, i
  IF Toting(obj) THEN
    ActSpk verb
    EXIT SUB
  ENDIF
  ' special case objects and fixed objects
  m = 25
  IF obj = oPLANT AND prop(oPLANT) <= 0 THEN m = 115
  IF obj = oBEAR AND prop(oBEAR) = 1 THEN m = 169
  IF obj = oCHAIN AND prop(oBEAR) <> 0 THEN m = 170
  IF fixed(obj) <> 0 THEN
    Rspeak m
    EXIT SUB
  ENDIF
  ' special case for liquids
  IF obj = oWATER OR obj = oOIL THEN
    IF Here(oBOTTLE) = 0 OR Liq() <> obj THEN
      obj = oBOTTLE
      IF Toting(oBOTTLE) AND prop(oBOTTLE) = 1 THEN
        VbFill
        EXIT SUB
      ENDIF
      IF prop(oBOTTLE) <> 1 THEN m = 105
      IF Toting(oBOTTLE) = 0 THEN m = 104
      Rspeak m
      EXIT SUB
    ENDIF
    obj = oBOTTLE
  ENDIF
  IF holding >= 7 THEN
    Rspeak 92
    EXIT SUB
  ENDIF
  ' special case for bird
  IF obj = oBIRD AND prop(oBIRD) = 0 THEN
    IF Toting(oROD) THEN
      Rspeak 26
      EXIT SUB
    ENDIF
    IF Toting(oCAGE) = 0 THEN
      Rspeak 27
      EXIT SUB
    ENDIF
    prop(oBIRD) = 1
  ENDIF
  IF (obj = oBIRD OR obj = oCAGE) AND prop(oBIRD) <> 0 THEN
    CarryObj oBIRD + oCAGE - obj, cur
  ENDIF
  CarryObj obj, cur
  ' handle liquid in bottle
  i = Liq()
  IF obj = oBOTTLE AND i <> 0 THEN place(i) = -1
  Rspeak 54
END SUB

' DROP etc.
SUB VbDrop
  LOCAL i
  ' check for dynamite
  IF Toting(oROD2) AND obj = oROD AND Toting(oROD) = 0 THEN obj = oROD2
  IF Toting(obj) = 0 THEN
    ActSpk verb
    EXIT SUB
  ENDIF
  IF obj = oBIRD AND Here(oSNAKE) <> 0 THEN         ' snake and bird
    Rspeak 30
    IF closed <> 0 THEN DwarfEnd
    Dstroy oSNAKE
    prop(oSNAKE) = -1
  ELSEIF obj = oCOINS AND Here(oVEND) <> 0 THEN     ' coins and vending machine
    Dstroy oCOINS
    DropObj oBATTERIES, cur
    Pspeak oBATTERIES, 0
    EXIT SUB
  ELSEIF obj = oBIRD AND AtObj(oDRAGON) <> 0 AND prop(oDRAGON) = 0 THEN   ' bird and dragon (ouch!!)
    Rspeak 154
    Dstroy oBIRD
    prop(oBIRD) = 0
    IF place(oSNAKE) <> 0 THEN tally2 = tally2 + 1
    EXIT SUB
  ENDIF
  IF obj = oBEAR AND AtObj(oTROLL) <> 0 THEN        ' bear and troll
    Rspeak 163
    MoveObj oTROLL, 0
    MoveObj oTROLL + MAXOBJ, 0
    MoveObj oTROLL2, 117
    MoveObj oTROLL2 + MAXOBJ, 122
    prop(oTROLL) = 2
  ELSEIF obj = oVASE THEN                           ' vase
    IF cur = 96 THEN
      Rspeak 54
    ELSE
      IF AtObj(oPILLOW) <> 0 THEN
        prop(oVASE) = 0
      ELSE
        prop(oVASE) = 2
      ENDIF
      Pspeak oVASE, prop(oVASE) + 1
      IF prop(oVASE) <> 0 THEN fixed(oVASE) = -1
    ENDIF
  ENDIF
  ' handle liquid and bottle
  i = Liq()
  IF i = obj THEN obj = oBOTTLE
  IF obj = oBOTTLE AND i <> 0 THEN place(i) = 0
  ' handle bird and cage
  IF obj = oCAGE AND prop(oBIRD) <> 0 THEN DropObj oBIRD, cur
  IF obj = oBIRD THEN prop(oBIRD) = 0
  DropObj obj, cur
END SUB

' LOCK, UNLOCK, OPEN, CLOSE etc.
SUB VbOpen
  LOCAL m, oyclam
  SELECT CASE obj
    CASE oCLAM, oOYSTER
      IF obj = oOYSTER THEN
        oyclam = 1
      ELSE
        oyclam = 0
      ENDIF
      IF verb = vLOCK THEN
        m = 61
      ELSEIF Toting(oTRIDENT) = 0 THEN
        m = 122 + oyclam
      ELSEIF Toting(obj) THEN
        m = 120 + oyclam
      ELSE
        m = 124 + oyclam
        Dstroy oCLAM
        DropObj oOYSTER, cur
        DropObj oPEARL, 105
      ENDIF
    CASE oDOOR
      IF prop(oDOOR) = 1 THEN
        m = 54
      ELSE
        m = 111
      ENDIF
    CASE oCAGE
      m = 32
    CASE oKEYS
      m = 55
    CASE oCHAIN
      IF Here(oKEYS) = 0 THEN
        m = 31
      ELSEIF verb = vLOCK THEN
        IF prop(oCHAIN) <> 0 THEN
          m = 34
        ELSEIF cur <> 130 THEN
          m = 173
        ELSE
          prop(oCHAIN) = 2
          IF Toting(oCHAIN) THEN DropObj oCHAIN, cur
          fixed(oCHAIN) = -1
          m = 172
        ENDIF
      ELSE
        IF prop(oBEAR) = 0 THEN
          m = 41
        ELSEIF prop(oCHAIN) = 0 THEN
          m = 37
        ELSE
          prop(oCHAIN) = 0
          fixed(oCHAIN) = 0
          IF prop(oBEAR) <> 3 THEN prop(oBEAR) = 2
          fixed(oBEAR) = 2 - prop(oBEAR)
          m = 171
        ENDIF
      ENDIF
    CASE oGRATE
      IF Here(oKEYS) = 0 THEN
        m = 31
      ELSEIF closing <> 0 THEN
        IF panic = 0 THEN
          clock2 = 15
          panic = panic + 1
        ENDIF
        m = 130
      ELSE
        m = 34 + prop(oGRATE)
        IF verb = vLOCK THEN
          prop(oGRATE) = 0
        ELSE
          prop(oGRATE) = 1
        ENDIF
        m = m + 2 * prop(oGRATE)
      ENDIF
    CASE ELSE
      m = 33
  END SELECT
  Rspeak m
END SUB

' SAY etc.
SUB VbSay
  Say "Okay."
  IF WType(w1$) = 2 AND Vocab(w1$, 0) MOD 1000 = vSAY THEN
    Say w2$
  ELSE
    Say w1$
  ENDIF
END SUB

' ON etc.
SUB VbOn
  IF Here(oLAMP) = 0 THEN
    ActSpk verb
  ELSEIF limit < 0 THEN
    Rspeak 184
  ELSE
    prop(oLAMP) = 1
    Rspeak 39
    IF wzdark <> 0 THEN
      wzdark = 0
      Describe
    ENDIF
  ENDIF
END SUB

' OFF etc.
SUB VbOff
  IF Here(oLAMP) = 0 THEN
    ActSpk verb
  ELSE
    prop(oLAMP) = 0
    Rspeak 40
  ENDIF
END SUB

' WAVE etc.
SUB VbWave
  IF Toting(obj) = 0 AND (obj <> oROD OR Toting(oROD2) = 0) THEN
    Rspeak 29
  ELSEIF obj <> oROD OR AtObj(oFISSURE) = 0 OR Toting(obj) = 0 OR closing <> 0 THEN
    ActSpk verb
  ELSE
    prop(oFISSURE) = 1 - prop(oFISSURE)
    Pspeak oFISSURE, 2 - prop(oFISSURE)
  ENDIF
END SUB

' ATTACK, KILL etc.
SUB VbKill
  LOCAL m, i
  SELECT CASE obj
    CASE oBIRD
      IF closed <> 0 THEN
        m = 137
      ELSE
        Dstroy oBIRD
        prop(oBIRD) = 0
        IF place(oSNAKE) = 19 THEN tally2 = tally2 + 1
        m = 45
      ENDIF
    CASE 0
      m = 44
    CASE oCLAM, oOYSTER
      m = 150
    CASE oSNAKE
      m = 46
    CASE oDWARF
      IF closed <> 0 THEN DwarfEnd
      m = 49
    CASE oTROLL
      m = 157
    CASE oBEAR
      m = 165 + (prop(oBEAR) + 1) \ 2
    CASE oDRAGON
      IF prop(oDRAGON) <> 0 THEN
        m = 167
      ELSE
        IF Yes(49, 0, 0) = 0 THEN EXIT SUB
        Pspeak oDRAGON, 1
        prop(oDRAGON) = 2
        prop(oRUG) = 0
        MoveObj oDRAGON + MAXOBJ, -1
        MoveObj oRUG + MAXOBJ, 0
        MoveObj oDRAGON, 120
        MoveObj oRUG, 120
        FOR i = 1 TO MAXOBJ - 1
          IF place(i) = 119 OR place(i) = 121 THEN MoveObj i, 120
        NEXT i
        newloc = 120
        EXIT SUB
      ENDIF
    CASE ELSE
      ActSpk verb
      EXIT SUB
  END SELECT
  Rspeak m
END SUB

' POUR
SUB VbPour
  IF obj = oBOTTLE OR obj = 0 THEN obj = Liq()
  IF obj = 0 THEN
    NeedObj
    EXIT SUB
  ENDIF
  IF Toting(obj) = 0 THEN
    ActSpk verb
    EXIT SUB
  ENDIF
  IF obj <> oOIL AND obj <> oWATER THEN
    Rspeak 78
    EXIT SUB
  ENDIF
  prop(oBOTTLE) = 1
  place(obj) = 0
  IF AtObj(oPLANT) <> 0 THEN
    IF obj <> oWATER THEN
      Rspeak 112
    ELSE
      Pspeak oPLANT, prop(oPLANT) + 1
      prop(oPLANT) = (prop(oPLANT) + 2) MOD 6
      prop(oPLANT2) = prop(oPLANT) \ 2
      Describe
    ENDIF
  ELSEIF AtObj(oDOOR) <> 0 THEN
    IF obj = oOIL THEN
      prop(oDOOR) = 1
    ELSE
      prop(oDOOR) = 0
    ENDIF
    Rspeak 113 + prop(oDOOR)
  ELSE
    Rspeak 77
  ENDIF
END SUB

' EAT
SUB VbEat
  LOCAL m
  SELECT CASE obj
    CASE oFOOD
      Dstroy oFOOD
      m = 72
    CASE oBIRD, oSNAKE, oCLAM, oOYSTER, oDWARF, oDRAGON, oTROLL, oBEAR
      m = 71
    CASE ELSE
      ActSpk verb
      EXIT SUB
  END SELECT
  Rspeak m
END SUB

' DRINK
SUB VbDrink
  IF obj <> oWATER THEN
    Rspeak 110
  ELSEIF Liq() <> oWATER OR Here(oBOTTLE) = 0 THEN
    ActSpk verb
  ELSE
    prop(oBOTTLE) = 1
    place(oWATER) = 0
    Rspeak 74
  ENDIF
END SUB

' THROW etc.
SUB VbThrow
  LOCAL m, i
  IF Toting(oROD2) AND obj = oROD AND Toting(oROD) = 0 THEN obj = oROD2
  IF Toting(obj) = 0 THEN
    ActSpk verb
    EXIT SUB
  ENDIF
  ' treasure to troll
  IF AtObj(oTROLL) <> 0 AND obj >= 50 AND obj < MAXOBJ THEN
    Rspeak 159
    DropObj obj, 0
    MoveObj oTROLL, 0
    MoveObj oTROLL + MAXOBJ, 0
    DropObj oTROLL2, 117
    DropObj oTROLL2 + MAXOBJ, 122
    EXIT SUB
  ENDIF
  ' feed the bears...
  IF obj = oFOOD AND Here(oBEAR) <> 0 THEN
    obj = oBEAR
    VbFeed
    EXIT SUB
  ENDIF
  ' if not axe, same as drop...
  IF obj <> oAXE THEN
    VbDrop
    EXIT SUB
  ENDIF
  ' AXE is THROWN
  i = DCheck()
  IF i <> 0 THEN                                      ' at a dwarf...
    m = 48
    IF Pct(33) THEN
      dseen(i) = 0
      dloc(i) = 0
      m = 47
      dkill = dkill + 1
      IF dkill = 1 THEN m = 149
    ENDIF
  ELSEIF AtObj(oDRAGON) <> 0 AND prop(oDRAGON) = 0 THEN   ' at a dragon...
    m = 152
  ELSEIF AtObj(oTROLL) <> 0 THEN                      ' at the troll...
    m = 158
  ELSEIF Here(oBEAR) <> 0 AND prop(oBEAR) = 0 THEN    ' at the bear...
    Rspeak 164
    DropObj oAXE, cur
    fixed(oAXE) = -1
    prop(oAXE) = 1
    EXIT SUB
  ELSE                                                ' otherwise it is an attack
    verb = vKILL
    obj = 0
    ItVerb
    EXIT SUB
  ENDIF
  ' handle the left over axe...
  Rspeak m
  DropObj oAXE, cur
  Describe
END SUB

' INVENTORY, FIND etc.
SUB VbFind
  LOCAL m
  IF Toting(obj) THEN
    m = 24
  ELSEIF closed <> 0 THEN
    m = 138
  ELSEIF DCheck() <> 0 AND dflag >= 2 AND obj = oDWARF THEN
    m = 94
  ELSEIF AtObj(obj) <> 0 THEN
    m = 94
  ELSEIF Liq() = obj AND Here(oBOTTLE) <> 0 THEN
    m = 94
  ELSEIF obj = LiqLoc(cur) THEN
    m = 94
  ELSE
    ActSpk verb
    EXIT SUB
  ENDIF
  Rspeak m
END SUB

' FILL
SUB VbFill
  LOCAL m, i
  SELECT CASE obj
    CASE oBOTTLE
      IF Liq() <> 0 THEN
        m = 105
      ELSEIF LiqLoc(cur) = 0 THEN
        m = 106
      ELSE
        prop(oBOTTLE) = cond(cur) AND cWATOIL
        i = Liq()
        IF Toting(oBOTTLE) THEN place(i) = -1
        IF i = oOIL THEN
          m = 108
        ELSE
          m = 107
        ENDIF
      ENDIF
    CASE oVASE
      IF LiqLoc(cur) = 0 THEN
        m = 144
      ELSEIF Toting(oVASE) = 0 THEN
        m = 29
      ELSE
        Rspeak 145
        VbDrop
        EXIT SUB
      ENDIF
    CASE ELSE
      m = 29
  END SELECT
  Rspeak m
END SUB

' FEED
SUB VbFeed
  LOCAL m
  SELECT CASE obj
    CASE oBIRD
      m = 100
    CASE oDWARF
      IF Here(oFOOD) = 0 THEN
        ActSpk verb
        EXIT SUB
      ENDIF
      dflag = dflag + 1
      m = 103
    CASE oBEAR
      IF Here(oFOOD) = 0 THEN
        IF prop(oBEAR) = 0 THEN
          m = 102
        ELSEIF prop(oBEAR) = 3 THEN
          m = 110
        ELSE
          ActSpk verb
          EXIT SUB
        ENDIF
      ELSE
        Dstroy oFOOD
        prop(oBEAR) = 1
        fixed(oAXE) = 0
        prop(oAXE) = 0
        m = 168
      ENDIF
    CASE oDRAGON
      IF prop(oDRAGON) <> 0 THEN
        m = 110
      ELSE
        m = 102
      ENDIF
    CASE oTROLL
      m = 182
    CASE oSNAKE
      IF closed <> 0 OR Here(oBIRD) = 0 THEN
        m = 102
      ELSE
        m = 101
        Dstroy oBIRD
        prop(oBIRD) = 0
        tally2 = tally2 + 1
      ENDIF
    CASE ELSE
      m = 14
  END SELECT
  Rspeak m
END SUB

' READ etc.
SUB VbRead
  LOCAL m, dummy
  m = 0
  IF Dark() <> 0 THEN
    Say "I see no " + ProbJ$() + " here."
    EXIT SUB
  ENDIF
  SELECT CASE obj
    CASE oMAGAZINE
      m = 190
    CASE oTABLET
      m = 196
    CASE oMESSAGE
      m = 191
    CASE oOYSTER
      IF Toting(oOYSTER) <> 0 AND closed <> 0 THEN
        dummy = Yes(192, 193, 54)
        EXIT SUB
      ENDIF
  END SELECT
  IF m <> 0 THEN
    Rspeak m
  ELSE
    ActSpk verb
  ENDIF
END SUB

' BLAST etc.
SUB VbBlast
  IF prop(oROD2) < 0 OR closed = 0 THEN
    ActSpk verb
  ELSE
    bonus = 133
    IF cur = 115 THEN bonus = 134
    IF Here(oROD2) THEN bonus = 135
    Rspeak bonus
    NormEnd
  ENDIF
END SUB

' BREAK etc.
SUB VbBreak
  LOCAL m
  IF obj = oMIRROR THEN
    m = 148
    IF closed <> 0 THEN
      Rspeak 197
      DwarfEnd
    ENDIF
  ELSEIF obj = oVASE AND prop(oVASE) = 0 THEN
    m = 198
    IF Toting(oVASE) THEN DropObj oVASE, cur
    prop(oVASE) = 2
    fixed(oVASE) = -1
  ELSE
    ActSpk verb
    EXIT SUB
  ENDIF
  Rspeak m
END SUB

' WAKE etc.
SUB VbWake
  IF obj <> oDWARF OR closed = 0 THEN
    ActSpk verb
  ELSE
    Rspeak 199
    DwarfEnd
  ENDIF
END SUB

' Speak the default verb message
SUB ActSpk(v)
  LOCAL m
  IF v < 1 OR v > 33 THEN Bug 39
  m = actmsg(v)
  IF m <> 0 THEN Rspeak m
END SUB

' No reasonable object found for the verb.  Used mostly by intransitive verbs.
SUB NeedObj
  IF WType(w1$) = 2 THEN
    Say w1$ + " what?"
  ELSE
    Say w2$ + " what?"
  ENDIF
END SUB

' ============================================================================
'  SAVE / RESTORE
' ============================================================================
SUB WrArr(a(), lo, hi)
  LOCAL i
  FOR i = lo TO hi
    PRINT #5, a(i)
  NEXT i
END SUB

SUB RdArr(a(), lo, hi)
  LOCAL i, v
  FOR i = lo TO hi
    INPUT #5, v
    a(i) = v
  NEXT i
END SUB

SUB SaveGame
  Say "Saving game to " + DATADIR$ + SAVEFILE$ + " ..."
  OPEN DATADIR$ + SAVEFILE$ FOR OUTPUT AS #5
  PRINT #5, turns
  PRINT #5, cur
  PRINT #5, oldloc
  PRINT #5, oldloc2
  PRINT #5, newloc
  WrArr cond(), 0, MAXLOC
  WrArr place(), 0, MAXOBJ - 1
  WrArr fixed(), 0, MAXOBJ - 1
  WrArr visited(), 0, MAXLOC
  WrArr prop(), 0, MAXOBJ - 1
  PRINT #5, tally
  PRINT #5, tally2
  PRINT #5, limit
  PRINT #5, lmwarn
  PRINT #5, wzdark
  PRINT #5, closing
  PRINT #5, closed
  PRINT #5, holding
  PRINT #5, detail
  PRINT #5, knfloc
  PRINT #5, clock1
  PRINT #5, clock2
  PRINT #5, panic
  WrArr dloc(), 0, DWARFMAX - 1
  PRINT #5, dflag
  WrArr dseen(), 0, DWARFMAX - 1
  WrArr odloc(), 0, DWARFMAX - 1
  PRINT #5, daltloc
  PRINT #5, dkill
  PRINT #5, chloc
  PRINT #5, chloc2
  PRINT #5, bonus
  PRINT #5, numdie
  PRINT #5, obj1
  PRINT #5, gaveup
  PRINT #5, foobar
  CLOSE #5
  Say "Game saved - see you later!"
END SUB

SUB RestoreGame
  IF DIR$(DATADIR$ + SAVEFILE$, FILE) = "" THEN
    Say "Sorry, cannot find any saved game (" + DATADIR$ + SAVEFILE$ + ")."
    EXIT SUB
  ENDIF
  OPEN DATADIR$ + SAVEFILE$ FOR INPUT AS #5
  INPUT #5, turns
  INPUT #5, cur
  INPUT #5, oldloc
  INPUT #5, oldloc2
  INPUT #5, newloc
  RdArr cond(), 0, MAXLOC
  RdArr place(), 0, MAXOBJ - 1
  RdArr fixed(), 0, MAXOBJ - 1
  RdArr visited(), 0, MAXLOC
  RdArr prop(), 0, MAXOBJ - 1
  INPUT #5, tally
  INPUT #5, tally2
  INPUT #5, limit
  INPUT #5, lmwarn
  INPUT #5, wzdark
  INPUT #5, closing
  INPUT #5, closed
  INPUT #5, holding
  INPUT #5, detail
  INPUT #5, knfloc
  INPUT #5, clock1
  INPUT #5, clock2
  INPUT #5, panic
  RdArr dloc(), 0, DWARFMAX - 1
  INPUT #5, dflag
  RdArr dseen(), 0, DWARFMAX - 1
  RdArr odloc(), 0, DWARFMAX - 1
  INPUT #5, daltloc
  INPUT #5, dkill
  INPUT #5, chloc
  INPUT #5, chloc2
  INPUT #5, bonus
  INPUT #5, numdie
  INPUT #5, obj1
  INPUT #5, gaveup
  INPUT #5, foobar
  CLOSE #5
  Say "Game restored."
  Describe
END SUB

' ============================================================================
'  DATA  (generated from the C tables: advent.c, advword.h, advcave.h)
' ============================================================================
' cond(1..10)
DATA 5,1,5,5,1,1,5,17,1,1
' cond(13..20)
DATA 32,0,0,2,0,0,64,2
' cond(21..26)
DATA 2,2,0,6,0,2
' cond(31..40)
DATA 2,2,0,0,0,0,0,4,0,2
' cond(42..50)
DATA 128,128,128,128,136,136,136,128,128
' cond(51..59)
DATA 128,128,136,128,136,0,8,0,2
' cond(79..90)
DATA 2,128,128,136,0,0,8,136,128,0,2,2
' cond(95..100)
DATA 4,0,0,0,0,1
' cond(113..116)
DATA 4,0,1,1
' cond(122..130)
DATA 8,8,8,8,8,8,8,8,8
' place(1..10)
DATA 3,3,8,10,11,0,14,13,94,96
' place(11..20)
DATA 19,17,101,103,0,106,0,0,3,3
' place(23..29)
DATA 109,25,23,111,35,0,97
' place(31..40)
DATA 119,117,117,0,130,0,126,140,0,96
' place(50..54)
DATA 18,27,28,29,30
' place(56..64)
DATA 92,95,97,100,101,0,119,127,130
' fixed(3..9)
DATA 9,0,0,0,15,0,-1
' fixed(11..17)
DATA -1,27,-1,0,0,0,-1
' fixed(23..30)
DATA -1,-1,67,-1,110,0,-1,-1
' fixed(31..40)
DATA 121,122,122,0,-1,-1,-1,-1,0,-1
' fixed(62..63)
DATA 121,-1
' actmsg(0..10)
DATA 0,24,29,0,33,0,33,38,38,42,14
' actmsg(11..20)
DATA 43,110,29,110,73,75,29,13,59,59
' actmsg(21..31)
DATA 174,109,67,13,147,155,195,146,110,13,13
' dloc(0..6)
DATA 0,19,27,33,44,64,114
' odloc(0..6)
DATA 0,0,0,0,0,0,0
' dseen(0..6)
DATA 0,0,0,0,0,0,0
' ---- vocabulary: word, code (original table order; sorted but for entry 0) ----
DATA "spelunker today",1016,"?",3051,"above",29,"abra",3050,"abracadabra",3050
DATA "across",42,"ascend",29,"attack",2012,"awkward",26,"axe",1028
DATA "back",8,"barren",40,"bars",1052,"batteries",1039,"battery",1039
DATA "beans",1024,"bear",1035,"bed",16,"bedquilt",70,"bird",1008
DATA "blast",2023,"blowup",2023,"bottle",1020,"box",1055,"break",2028
DATA "brief",2026,"broken",54,"building",12,"cage",1004,"calm",2010
DATA "canyon",25,"capture",2001,"carpet",1040,"carry",2001,"catch",2001
DATA "cave",67,"cavern",73,"chain",1064,"chant",2003,"chasm",1032
DATA "chest",1055,"clam",1014,"climb",56,"close",2006,"cobblestone",18
DATA "coins",1054,"continue",2011,"crack",33,"crap",3079,"crawl",17
DATA "cross",69,"d",30,"damn",3079,"damnit",3079,"dark",22
DATA "debris",51,"depression",63,"descend",30,"describe",57,"detonate",2023
DATA "devour",2014,"diamonds",1051,"dig",3066,"discard",2002,"disturb",2029
DATA "dome",35,"door",1009,"down",30,"downstream",4,"downward",30
DATA "dragon",1031,"drawing",1029,"drink",2015,"drop",2002,"dump",2002
DATA "dwarf",1017,"dwarves",1017,"e",43,"east",43,"eat",2014
DATA "egg",1056,"eggs",1056,"emerald",1059,"enter",3,"entrance",64
DATA "examine",57,"excavate",3066,"exit",11,"explore",2011,"extinguish",2008
DATA "fee",2025,"fee",3001,"feed",2021,"fie",2025,"fie",3002
DATA "fight",2012,"figure",1027,"fill",2022,"find",2019,"fissure",1012
DATA "floor",58,"foe",2025,"foe",3003,"follow",2011,"foo",2025
DATA "foo",3004,"food",1019,"forest",6,"fork",77,"forward",7
DATA "free",2002,"fuck",3079,"fum",2025,"fum",3005,"get",2001
DATA "geyser",1037,"giant",27,"go",2011,"gold",1050,"goto",2011
DATA "grate",1003,"gully",13,"h2o",1021,"hall",38,"headlamp",1002
DATA "help",3051,"hill",2,"hit",2012,"hocus",3050,"hole",52
DATA "hours",2031,"house",12,"i",2020,"ignite",2023,"in",19
DATA "info",3142,"information",3142,"inside",19,"inventory",2020,"inward",19
DATA "issue",1016,"jar",1020,"jewel",1053,"jewelry",1053,"jewels",1053
DATA "jump",39,"keep",2001,"key",1001,"keys",1001,"kill",2012
DATA "knife",1018,"knives",1018,"l",57,"lamp",1002,"lantern",1002
DATA "leave",11,"left",36,"light",2007,"lite",2007,"load",2033
DATA "lock",2006,"log",2032,"look",57,"lost",3068,"low",24
DATA "machine",1038,"magazine",1016,"main",76,"message",1036,"ming",1058
DATA "mirror",1023,"mist",3069,"moss",1040,"mumble",2003,"n",45
DATA "ne",47,"nest",1056,"north",45,"nothing",2005,"nowhere",21
DATA "nugget",1050,"null",21,"nw",50,"off",2008,"office",76
DATA "oil",1022,"on",2007,"onward",7,"open",2004,"opensesame",3050
DATA "oriental",72,"out",11,"outdoors",32,"outside",11,"over",41
DATA "oyster",1015,"passage",23,"pause",2030,"pearl",1061,"persian",1062
DATA "peruse",2027,"pillow",1010,"pirate",1030,"pit",31,"placate",2010
DATA "plant",1024,"plant",1025,"platinum",1060,"plover",71,"plugh",65
DATA "pocus",3050,"pottery",1058,"pour",2013,"proceed",2011,"pyramid",1060
DATA "quit",2018,"rations",1019,"read",2027,"release",2002,"reservoir",75
DATA "restore",2033,"retreat",8,"return",8,"right",37,"road",2
DATA "rock",15,"rod",1005,"rod",1006,"room",59,"rub",2016
DATA "rug",1062,"run",2011,"s",46,"save",2030,"say",2003
DATA "score",2024,"se",48,"secret",66,"sesame",3050,"shadow",1027
DATA "shake",2009,"shard",1058,"shatter",2028,"shazam",3050,"shell",74
DATA "shit",3079,"silver",1052,"sing",2003,"slab",61,"slit",60
DATA "smash",2028,"snake",1011,"south",46,"spelunker",1016,"spice",1063
DATA "spices",1063,"stairs",10,"stalactite",1026,"steal",2001,"steps",1007
DATA "steps",34,"stop",3139,"stream",14,"strike",2012,"surface",20
DATA "suspend",2030,"sw",49,"swim",3147,"swing",2009,"tablet",1013
DATA "take",2001,"tame",2010,"throw",2017,"toss",2017,"tote",2001
DATA "touch",57,"travel",2011,"treasure",1055,"tree",3064,"trees",3064
DATA "trident",1057,"troll",1033,"troll",1034,"tunnel",23,"turn",2011
DATA "u",29,"unlock",2004,"up",29,"upstream",4,"upward",29
DATA "utter",2003,"valley",9,"vase",1058,"velvet",1010,"vending",1038
DATA "view",28,"volcano",1037,"w",44,"wake",2029,"walk",2011
DATA "wall",53,"water",1021,"wave",2009,"west",44,"xyzzy",62
DATA "y2",55
' ---- travel table: dest*1000000 + verb*1000 + cond ; -1 ends a location ----
' loc 1
DATA 2002000,2044000,2029000,3003000,3012000,3019000,3043000,4005000,4013000,4014000,4046000,4030000,5006000,5045000
DATA 5043000,8063000,-1
' loc 2
DATA 1002000,1012000,1007000,1043000,1045000,1030000,5006000,5045000,5046000,-1
' loc 3
DATA 1003000,1011000,1032000,1044000,11062000,33065000,79005000,79014000,-1
' loc 4
DATA 1004000,1012000,1045000,5006000,5043000,5044000,5029000,7005000,7046000,7030000,8063000,-1
' loc 5
DATA 4009000,4043000,4030000,5006050,5007050,5045050,6006000,5044000,5046000,-1
' loc 6
DATA 1002000,1045000,4009000,4043000,4044000,4030000,5006000,5046000,-1
' loc 7
DATA 1012000,4004000,4045000,5006000,5043000,5044000,8005000,8015000,8016000,8046000,595060000,595014000,595030000,-1
' loc 8
DATA 5006000,5043000,5046000,5044000,1012000,7004000,7013000,7045000,9003303,9019303,9030303,593003000,-1
' loc 9
DATA 8011303,8029303,593011000,10017000,10018000,10019000,10044000,14031000,11051000,-1
' loc 10
DATA 9011000,9020000,9021000,9043000,11019000,11022000,11044000,11051000,14031000,-1
' loc 11
DATA 8063303,9064000,10017000,10018000,10023000,10024000,10043000,12025000,12019000,12029000,12044000,3062000,14031000,-1
' loc 12
DATA 8063303,9064000,11030000,11043000,11051000,13019000,13029000,13044000,14031000,-1
' loc 13
DATA 8063303,9064000,11051000,12025000,12043000,14023000,14031000,14044000,-1
' loc 14
DATA 8063303,9064000,11051000,13023000,13043000,20030150,20031150,20034150,15030000,16033000,16044000,-1
' loc 15
DATA 18036000,18046000,17007000,17038000,17044000,19010000,19030000,19045000,22029150,22031150,22034150,22035150,22023150,22043150
DATA 14029000,34055000,-1
' loc 16
DATA 14001000,-1
' loc 17
DATA 15038000,15043000,596039312,21007412,597041412,597042412,597044412,597069412,27041000,-1
' loc 18
DATA 15038000,15011000,15045000,-1
' loc 19
DATA 15010000,15029000,15043000,28045311,28036311,29046311,29037311,30044311,30007311,32045000,74049035,32049211,74066000,-1
' loc 20
DATA 1000,-1
' loc 21
DATA 1000,-1
' loc 22
DATA 15001000,-1
' loc 23
DATA 67043000,67042000,68044000,68061000,25030000,25031000,648052000,-1
' loc 24
DATA 67029000,67011000,-1
' loc 25
DATA 23029000,23011000,31056724,26056000,-1
' loc 26
DATA 88001000,-1
' loc 27
DATA 596039312,21007412,597041412,597042412,597043412,597069412,17041000,40045000,41044000,-1
' loc 28
DATA 19038000,19011000,19046000,33045000,33055000,36030000,36052000,-1
' loc 29
DATA 19038000,19011000,19045000,-1
' loc 30
DATA 19038000,19011000,19043000,62044000,62029000,-1
' loc 31
DATA 89001524,90001000,-1
' loc 32
DATA 19001000,-1
' loc 33
DATA 3065000,28046000,34043000,34053000,34054000,35044000,302071159,100071000,-1
' loc 34
DATA 33030000,33055000,15029000,-1
' loc 35
DATA 33043000,33055000,20039000,-1
' loc 36
DATA 37043000,37017000,28029000,28052000,39044000,65070000,-1
' loc 37
DATA 36044000,36017000,38030000,38031000,38056000,-1
' loc 38
DATA 37056000,37029000,37011000,595060000,595014000,595030000,595004000,595005000,-1
' loc 39
DATA 36043000,36023000,64030000,64052000,64058000,65070000,-1
' loc 40
DATA 41001000,-1
' loc 41
DATA 42046000,42029000,42023000,42056000,27043000,59045000,60044000,60017000,-1
' loc 42
DATA 41029000,42045000,43043000,45046000,80044000,-1
' loc 43
DATA 42044000,44046000,45043000,-1
' loc 44
DATA 43043000,48030000,50046000,82045000,-1
' loc 45
DATA 42044000,43045000,46043000,47046000,87029000,87030000,-1
' loc 46
DATA 45044000,45011000,-1
' loc 47
DATA 45043000,45011000,-1
' loc 48
DATA 44029000,44011000,-1
' loc 49
DATA 50043000,51044000,-1
' loc 50
DATA 44043000,49044000,51030000,52046000,-1
' loc 51
DATA 49044000,50029000,52043000,53046000,-1
' loc 52
DATA 50044000,51043000,52046000,53029000,55045000,86030000,-1
' loc 53
DATA 51044000,52045000,54046000,-1
' loc 54
DATA 53044000,53011000,-1
' loc 55
DATA 52044000,55045000,56030000,57043000,-1
' loc 56
DATA 55029000,55011000,-1
' loc 57
DATA 13030000,13056000,55044000,58046000,83045000,84043000,-1
' loc 58
DATA 57043000,57011000,-1
' loc 59
DATA 27001000,-1
' loc 60
DATA 41043000,41029000,41017000,61044000,62045000,62030000,62052000,-1
' loc 61
DATA 60043000,62045000,107046100,-1
' loc 62
DATA 60044000,63045000,30043000,61046000,-1
' loc 63
DATA 62046000,62011000,-1
' loc 64
DATA 39029000,39056000,39059000,65044000,65070000,103045000,103074000,106043000,-1
' loc 65
DATA 64043000,66044000,556046080,68061000,556029080,70029050,39029000,556045060,72045075,71045000,556030080,106030000,-1
' loc 66
DATA 65047000,67044000,556046080,77025000,96043000,556050050,97072000,-1
' loc 67
DATA 66043000,23044000,23042000,24030000,24031000,-1
' loc 68
DATA 23046000,69029000,69056000,65045000,-1
' loc 69
DATA 68030000,68061000,120046331,119046000,109045000,113075000,-1
' loc 70
DATA 71045000,65030000,65023000,111046000,-1
' loc 71
DATA 65048000,70046000,110045000,-1
' loc 72
DATA 65070000,118049000,73045000,97048000,97072000,-1
' loc 73
DATA 72046000,72017000,72011000,-1
' loc 74
DATA 19043000,120044331,121044000,75030000,-1
' loc 75
DATA 76046000,77045000,-1
' loc 76
DATA 75045000,-1
' loc 77
DATA 75043000,78044000,66045000,66017000,-1
' loc 78
DATA 77046000,-1
' loc 79
DATA 3001000,-1
' loc 80
DATA 42045000,80044000,80046000,81043000,-1
' loc 81
DATA 80044000,80011000,-1
' loc 82
DATA 44046000,44011000,-1
' loc 83
DATA 57046000,84043000,85044000,-1
' loc 84
DATA 57045000,83044000,114050000,-1
' loc 85
DATA 83043000,83011000,-1
' loc 86
DATA 52029000,52011000,-1
' loc 87
DATA 45029000,45030000,-1
' loc 88
DATA 25030000,25056000,25043000,20039000,92044000,92027000,-1
' loc 89
DATA 25001000,-1
' loc 90
DATA 23001000,-1
' loc 91
DATA 95045000,95073000,95023000,72030000,72056000,-1
' loc 92
DATA 88046000,93043000,94045000,-1
' loc 93
DATA 92046000,92027000,92011000,-1
' loc 94
DATA 92046000,92027000,92023000,95045309,95003309,95073309,611045000,-1
' loc 95
DATA 94046000,94011000,92027000,91044000,-1
' loc 96
DATA 66044000,66011000,-1
' loc 97
DATA 66048000,72044000,72017000,98029000,98045000,98073000,-1
' loc 98
DATA 97046000,97072000,99044000,-1
' loc 99
DATA 98050000,98073000,301043000,301023000,100043000,-1
' loc 100
DATA 301044000,301023000,301011000,99044000,302071159,33071000,101047000,101022000,-1
' loc 101
DATA 100046000,100071000,100011000,-1
' loc 102
DATA 103030000,103074000,103011000,-1
' loc 103
DATA 102029000,102038000,104030000,618046114,619046115,64046000,-1
' loc 104
DATA 103029000,103074000,105030000,-1
' loc 105
DATA 104029000,104011000,103074000,-1
' loc 106
DATA 64029000,65044000,108043000,-1
' loc 107
DATA 131046000,132049000,133047000,134048000,135029000,136050000,137043000,138044000,139045000,61030000,-1
' loc 108
DATA 556043095,556045095,556046095,556047095,556048095,556049095,556050095,556029095,556030095,106043000,626044000,-1
' loc 109
DATA 69046000,113045000,113075000,-1
' loc 110
DATA 71044000,20039000,-1
' loc 111
DATA 70045000,50030040,50039040,50056040,53030050,45030000,-1
' loc 112
DATA 131049000,132045000,133043000,134050000,135048000,136047000,137044000,138030000,139029000,140046000,-1
' loc 113
DATA 109046000,109011000,109109000,-1
' loc 114
DATA 84048000,-1
' loc 115
DATA 116049000,-1
' loc 116
DATA 115047000,593030000,-1
' loc 117
DATA 118049000,660041233,660042233,660069233,660047233,661041332,303041000,21039332,596039000,-1
' loc 118
DATA 72030000,117029000,-1
' loc 119
DATA 69045000,69011000,653043000,65307000,-1
' loc 120
DATA 69045000,74043000,-1
' loc 121
DATA 74043000,74011000,653045000,653007000,-1
' loc 122
DATA 123047000,660041233,660042233,660069233,660049233,303041000,596039000,124077000,126028000,129040000,-1
' loc 123
DATA 122044000,124043000,124077000,126028000,129040000,-1
' loc 124
DATA 123044000,125047000,125036000,128048000,128037000,128030000,126028000,129040000,-1
' loc 125
DATA 124046000,124077000,126045000,126028000,127043000,127017000,-1
' loc 126
DATA 125046000,125023000,125011000,124077000,610030000,610039000,-1
' loc 127
DATA 125044000,125011000,125017000,124077000,126028000,-1
' loc 128
DATA 124045000,124029000,124077000,129046000,129030000,129040000,126028000,-1
' loc 129
DATA 128044000,128029000,124077000,130043000,130019000,130040000,130003000,126028000,-1
' loc 130
DATA 129044000,124077000,126028000,-1
' loc 131
DATA 107044000,132048000,133050000,134049000,135047000,136029000,137030000,138045000,139046000,112043000,-1
' loc 132
DATA 107050000,131029000,133045000,134046000,135044000,136049000,137047000,138043000,139030000,112048000,-1
' loc 133
DATA 107029000,131030000,132044000,134047000,135049000,136043000,137045000,138050000,139048000,112046000,-1
' loc 134
DATA 107047000,131045000,132050000,133048000,135043000,136030000,137046000,138029000,139044000,112049000,-1
' loc 135
DATA 107045000,131048000,132030000,133046000,134043000,136044000,137049000,138047000,139050000,112029000,-1
' loc 136
DATA 107043000,131044000,132029000,133049000,134030000,135046000,137050000,138048000,139047000,112045000,-1
' loc 137
DATA 107048000,131047000,132046000,133030000,134029000,135050000,136045000,138049000,139043000,112044000,-1
' loc 138
DATA 107030000,131043000,132047000,133029000,134044000,135045000,136046000,137048000,139049000,112050000,-1
' loc 139
DATA 107049000,131050000,132043000,133044000,134045000,135030000,136048000,137029000,138046000,112047000,-1
' loc 140
DATA 112045000,112011000,-1
