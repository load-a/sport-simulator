IDENTIFICATION DIVISION.
PROGRAM-ID. Character-Builder.

ENVIRONMENT DIVISION.
INPUT-OUTPUT SECTION.
FILE-CONTROL.
  SELECT Character-Sheet ASSIGN TO "data/characters.dat"
  ORGANIZATION IS INDEXED
  ACCESS IS DYNAMIC
  RECORD KEY IS short-name
  FILE STATUS IS File-Status.

DATA DIVISION.
FILE SECTION.
FD Character-Sheet.
  COPY "copy/character-record.cpy".

WORKING-STORAGE SECTION.
  01 valid-index  PIC 9 VALUE 0.
    88 is-valid   VALUE 1.
    88 not-valid  VALUE 0.
  01 response     PIC X(20).
    88 yes        VALUE "Y".
    88 quit       VALUE "N".
    88 player     VALUE "PC".
    88 npc        VALUE "NPC".
  01 input-number PIC 999.
    88 valid-die VALUE 2, 4, 6, 8, 10, 12, 20.
  01 question     PIC X(40).

  01 File-Status PIC 99.
    88 end-of-file VALUE 10.

PROCEDURE DIVISION.
Main-Logic.

  PERFORM Create-Character UNTIL quit.

STOP RUN.

FORMATTING SECTION.
Case-Response.
  MOVE FUNCTION UPPER-CASE(response) TO response.

Trim-Response.
  MOVE FUNCTION TRIM(response) TO response.

USER-INTERFACE SECTION.
Ask.
  DISPLAY FUNCTION TRIM(question) ": " WITH NO ADVANCING
  ACCEPT response
  PERFORM Trim-Response.

Ask-Number.
  DISPLAY FUNCTION TRIM(question) ": " WITH NO ADVANCING
  ACCEPT input-number.

Confirm.
  DISPLAY FUNCTION TRIM(question) "? (Y/N): " WITH NO ADVANCING
  Accept response
  PERFORM Case-Response.

COPY "copy/character-preview.cpy".

CREATION SECTION.
Create-Character.
  PERFORM Create-Name

  IF is-valid
    PERFORM Basic-Details

    PERFORM Preview-Character
    MOVE "RECORD THIS CHARACTER" TO question
    PERFORM Confirm

    IF yes 
      PERFORM Save-Character
    END-IF
  END-IF

  MOVE "MAKE ANOTHER CHARACTER" TO question
  PERFORM Confirm.

Create-Name.
  MOVE "ENTER INDEX / SHORT NAME (10)" TO question.
  PERFORM Ask.
  PERFORM Validate-Name.

Basic-Details.
  MOVE "ENTER LONG NAME (21)" TO question
  PERFORM Ask
  MOVE response TO long-name

  MOVE "ENTER ORIGINAL TEAM (20)" TO question
  PERFORM Ask
  MOVE response TO team

  MOVE "ENTER POWER (##)" TO question
  PERFORM Ask-Number
  PERFORM Validate-Die
  Move input-number to Power

  MOVE "ENTER FOCUS (##)" TO question
  PERFORM Ask-Number
  PERFORM Validate-Die
  Move input-number to Focus

  MOVE "ENTER SPEED (##)" TO question
  PERFORM Ask-Number
  PERFORM Validate-Die
  Move input-number to speed

  MOVE "ENTER TYPE (PC | NPC)" TO question
  PERFORM Ask
  PERFORM Validate-Type.

  MOVE "FINE TUNE CHARACTER" TO question
  PERFORM Confirm.

  IF yes
    PERFORM Advanced-Details
  ELSE
    DISPLAY "USING DEFAULT VALUES"
    MOVE "bench" TO task
    MOVE 1.00 TO energy
    MOVE 0 TO experience
    MOVE 1 TO Level
  END-IF.

Advanced-Details.
  MOVE "ENTER TASK" TO question
  PERFORM Ask
  MOVE response TO task

  MOVE "ENTER ENERGY (#.##)" TO question
  PERFORM Ask-Number
  Move input-number to energy

  MOVE "ENTER EXPERIENCE (##)" TO question
  PERFORM Ask-Number
  Move input-number to experience

  MOVE "ENTER LEVEL (##)" TO question
  PERFORM Ask-Number
  Move input-number to level.

Save-Character.
  DISPLAY "ADDING NEW RECORD..."

  OPEN I-O Character-Sheet
    WRITE character-record INVALID KEY
      REWRITE character-record
    END-WRITE
  CLOSE Character-Sheet.

VALIDATION SECTION.
Validate-Name.
  OPEN I-O Character-Sheet
    PERFORM Case-Response.
    MOVE response TO short-name
    READ Character-Sheet KEY IS short-name
      INVALID KEY 
        SET is-valid TO TRUE
      NOT INVALID KEY 
        DISPLAY FUNCTION TRIM(short-name) " IS ALREADY IN RECORD"
        SET not-valid TO TRUE
    END-READ.
  CLOSE Character-Sheet.

Validate-Die.
  IF NOT valid-die
    DISPLAY "DEFAULTING TO 8"
    MOVE 8 to input-number
  END-IF.

Validate-Type.
  PERFORM Case-Response

  IF player
    SET character-type to "PLAYER"
  ELSE IF npc
    SET character-type to "NPC"
  ELSE
    DISPLAY "DEFAULTING CHARACTER TO TEST"
    SET character-type to "TEST"
  END-IF.
