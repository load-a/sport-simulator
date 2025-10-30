IDENTIFICATION DIVISION.
PROGRAM-ID. Sport-Simulator.

ENVIRONMENT DIVISION.
INPUT-OUTPUT SECTION.
FILE-CONTROL.
  SELECT CharacterSheet ASSIGN TO "characters.dat"
    ORGANIZATION IS LINE SEQUENTIAL
    FILE STATUS IS File-Status.

DATA DIVISION.
FILE SECTION.
FD CharacterSheet.
  01 character-record.
    02 long-name    PIC X(21).
    02 short-name   PIC X(10).
    02 task         PIC X(10).
    02 team         PIC X(20).
    02 level        PIC 99.
    02 experience   PIC 99.
    02 energy       PIC 9V99.
      88 exhausted  VALUE 00 THROUGH 24.
      88 tired      VALUE 25 THROUGH 50.
      88 fine       VALUE 51 THROUGH 100.
    02 power        PIC 99.
    02 focus        PIC 99.
    02 speed        PIC 99.

WORKING-STORAGE SECTION.
  01 File-Status PIC 99.
    88 end-of-file VALUE 10.

PROCEDURE DIVISION.
Main-logic.
  OPEN INPUT CharacterSheet
  PERFORM UNTIL end-of-file
    READ CharacterSheet
    AT END
      SET end-of-file TO TRUE
    NOT AT END
      DISPLAY long-name " (" FUNCTION TRIM(short-name) ")"
      DISPLAY "Team: " team
      DISPLAY "Task: " task
      DISPLAY "Level: " level " (" experience " / 100)"
      DISPLAY "Power: " power
      DISPLAY "Focus: " focus
      DISPLAY "Speed: " speed
      DISPLAY "Energy: " energy "%"
      DISPLAY SPACES
    END-READ
  END-PERFORM
  CLOSE CharacterSheet
STOP RUN.
