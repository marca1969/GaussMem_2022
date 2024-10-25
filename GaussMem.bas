'   GaussMem - ZX Basic Edition
'   By Massimiliano Arca
'   June 2, 2023

' Routine for proportional PRINT'
#include "propPrint.bas"

' Subroutines and functions declaration
declare sub InitScreen
declare sub Frames
declare Function AskNumber(row as ubyte,col as ubyte,min as ubyte,max as ulong) as ulong

' Constants declarations'
const enter as UBYTE = 13
const delete as UBYTE = 12
const true as UBYTE = 1
const false as UBYTE = 0
const black as UBYTE = 0
const blue as UBYTE = 1
const red as UBYTE = 2
const white as UBYTE = 7

' Bug correction
asm
    ld iy,$5c3a
end asm

'Start of the program
' Variable definitions
dim esc as byte
dim NumberOfProcessors,TypeOfCalculation,TypeOfFunctions as integer
dim NumberOfFunctions,memory,mwords,mbytes,gbytes as ulong
DIM t$(6) as string
dim s(6,3) as ubyte 
DIM b$(3) as string
dim e$,m$,k$ as string
dim i%,j% as integer 
dim num as ubyte 
e$="                             "
' UDG definition
RESTORE
FOR i%=1 TO 4
    READ a$
    FOR j%=0 TO 7
        READ num
        POKE USR a$+j%,num
    NEXT j%
NEXT i%
DATA "a",3,15,28,24,56,56,24,28
DATA "b",192,224,16,0,124,16,40,212
DATA "c",15,3,0,209,170,171,170,169
DATA "d",168,196,0,154,85,213,21,213
' Arrays preparation
' t$ = type of calculation
FOR i%=1 TO 6
    READ t$(i%)
NEXT i%
DATA "SCF energies","SCF gradients","SCF frequencies"
DATA "MP2 energies","MP2 gradients","MP2 frequencies"
' s = Memory threshold
FOR i%=1 TO 6
    FOR j%=1 TO 3
        READ s(i%,j%)
    NEXT j%
NEXT i%
' b$ = type of basis functions (highest momentum)
FOR i%=1 TO 3
    READ b$(i%)
NEXT i%

' DATA Thresholds
DATA 4,4,9
DATA 4,5,16
DATA 4,9,27
DATA 4,5,10
DATA 4,6,16
DATA 6,10,28

' DATA Basis functions
DATA "d/f"
DATA "g"
DATA "h"
' End of Data Section

InitScreen
'FrameDrawing

' Main loop
do
    NumberOfProcessors=0
    NumberOfFunctions=0
    TypeOfCalculation=0
    TypeOfFunctions=0
    INK black
    FOR i%=5 TO 11
        PRINT AT i%,0;e$;"  ";AT i%*(i%<10)+9,1;"                              "
    NEXT i%

    ' Data input: sections 1–4
    ' Section 1
    propPrint (16,112,4,1,"Insert the number of shared processors")
    ' Ink blue (57) attributes within the frame
    for i%=0 to 27
        for j%=0 to 5
            poke 22978+i%+32*j%,57
        next j%
    next i%
    PRINT AT 4,1;"Processors     "
    NumberOfProcessors = AskNumber(4,14,1,64)
    
    ' Section 2
    propPrint (16,112,4,1,"Insert the number of shared processors")
    propPrint (16,112,4,1,"Insert the number of basis functions  ")
    PRINT AT 5,1;"Functions"
    NumberOfFunctions=AskNumber(5,14,3,1e5)
    
    ' Section 3
    PRINT AT 6,1;"Type of calc"
    propPrint (16,112,4,1,"Insert the number of basis functions  ")
    propPrint (16,112,6,0,"Insert:")
    FOR i%=1 TO 6
        propPrint (112,8*(i%+13),4,1,str(i%)+". "+t$(i%)+"          ")
    NEXT i%
     TypeOfCalculation=AskNumber(6,14,1,6)
    print at 6,14; t$(TypeOfCalculation)
    ink blue 
    FOR i%=14 TO 19
        print at i%,14;"             "
    NEXT i%
    ink black 
    ' Section 4
    PRINT AT 7,1;"Momentum"
    FOR i%=1 TO 3
        propPrint (112,8*(13+i%),4,1,str(i%)+". "+b$(i%)+" functions   ")
    NEXT i%
    TypeOfFunctions=AskNumber(7,14,1,3)
    PRINT AT 7,14;b$(TypeOfFunctions);" functions"
    FOR i%=14 TO 19
        PRINT AT i%,1;"                              "
    NEXT i%

    ' Memory calculation
    memory=s(TypeOfCalculation,TypeOfFunctions)
    memory=2*NumberOfProcessors*(memory+2*NumberOfFunctions^2/2^20)
    mwords=INT (0.5+memory)
    mbytes=INT (0.5+memory*8)
    gbytes=INT (0.5+((memory*8)/(2^10)))
    m$=STR$ mwords+"MW"
    IF mwords>1024 THEN LET m$=STR$ (mbytes)+"MB"
    IF mbytes>1024 THEN LET m$=STR$ (gbytes)+"GB"
    PRINT AT 9,1;"Memory estimate"
    INK red
    PRINT AT 9,17;mwords;" MW"
    IF mbytes>=1 THEN PRINT AT 10,17;mbytes;" MB"
    IF gbytes>=1 THEN PRINT AT 11,17;gbytes;" GB"
    propPrint (8,112,4,1,"Start your gjf/com Gaussian input file with:")
    ink blue
    PRINT AT 16,1;"%nproc=";NumberOfProcessors
    PRINT AT 17,1;"%mem="
    PRINT AT 17,6;m$
    ink black
    italic true
    PRINT AT 22,9; FLASH true; "Press "; FLASH false;"Q"; FLASH true;" to quit"
    print at 23,0; FLASH false;TAB 4; FLASH true;"Any other key to restart"
    italic false
    esc=false
    DO
        k$=inkey$
        IF k$="q" OR k$="Q" THEN 
            esc=true
        endif
    loop while k$=""
    pause true 
loop while esc=false
CLS
InitScreen
PRINT AT 16,2; INVERSE true; INK red;"Thank you for using GaussMem"
PRINT AT 17,2; bright false; INVERSE false; INK black;"  Massimiliano Arca  2022   "
PAUSE 15
END 

' Functions and subs'
Function AskNumber(row as ubyte,col as ubyte,min as ubyte,max as ulong) as ulong
    ' AskNumber
    ' row,col = text coordinates
    ' min = minimum value
    ' max = maximum value
    dim PressedKey as integer
    dim InputNumber as ulong
    dim condition as integer
    dim n$ as string
    italic true
    PRINT AT 22,0; INVERSE true;"Type number; ENTER to confirm   "
    n$="Allowed range: "+str(min)+" - "+str(max)
    do
        n$=n$+" "
    loop until len(n$)=31
    print at 23,0;n$ 
    italic false
    PRINT AT row,col; FLASH true; INVERSE true;" "
    InputNumber=-1
    condition=false
    n$=""
    DO
        PAUSE 0
        PressedKey=PEEK(23560)
        BEEP .01,-2
        IF (PressedKey>47 AND PressedKey<58) THEN n$=n$+CHR$(PressedKey) 
        IF n$<>"" then   
            InputNumber=VAL(n$)
        endif
        
        'Pressed DELETE
        IF PressedKey=delete THEN 
            IF LEN n$>0 THEN 
                n$=n$(TO LEN n$-2)
                IF n$="" THEN InputNumber=-1
            endif
        endif   
        'Pressed ENTER
        IF PressedKey=enter THEN 
            if (InputNumber>=min AND InputNumber<=max) THEN 
                condition=true
            else
                BORDER red
                BEEP .2,-10
                BORDER white
            endif
        endif
        PRINT AT row,col;n$; INVERSE true; FLASH true;" "; FLASH false; INVERSE false;" "
    LOOP until condition=true
    PRINT AT 22,0;"                                "
    PRINT AT 23,0;"                               "
    InputNumber=VAL(n$)
    PRINT AT row,col;InputNumber;" "
    return InputNumber
end Function

SUB Frames
    INK black
    PLOT 2,86
    DRAW 251,0
    PLOT 254,85
    DRAW 0,-58
    PLOT 253,26
    DRAW -251,0
    PLOT 1,27
    DRAW 0,58
    PLOT 4,84
    DRAW 247,0
    PLOT 252,83
    DRAW 0,-54
    PLOT 251,28
    DRAW -247,0
    PLOT 3,29
    DRAW 0,54
end sub
'End of FrameDrawing proc

sub InitScreen
    INK black
    PAPER white
    BORDER white
    CLS
    INVERSE true
    PRINT AT 0,8; BRIGHT 1;"GaussMem 2022"
    INVERSE false
    Frames
    propPrint (62,8,4,1,"ZX Spectrum Edition")
    PRINT AT 0,0;chr$ 144;CHR$ 145
    print at 1,0;CHR$ 146;CHR$ 147
end sub