IDENTIFICATION DIVISION.
PROGRAM-ID. Sport-Simulator.

ENVIRONMENT DIVISION.
INPUT-OUTPUT SECTION.
FILE-CONTROL.
  SELECT CharacterSheet ASSIGN TO "data/characters.dat"
    ORGANIZATION IS INDEXED
    ACCESS MODE IS DYNAMIC
    RECORD KEY IS short-name
    FILE STATUS IS File-Status.

DATA DIVISION.
FILE SECTION.
FD CharacterSheet.
  COPY "copy/data/character-record.cpy".

WORKING-STORAGE SECTION.
  01 File-Status PIC 99.
    88 end-of-file VALUE 10.

PROCEDURE DIVISION.
Main-logic.
  OPEN INPUT CharacterSheet
  MOVE LOW-VALUE TO short-name
  START CharacterSheet KEY >= short-name
    INVALID KEY DISPLAY "NO RECORDS FOUND"
    NOT INVALID KEY
      PERFORM UNTIL end-of-file
        READ CharacterSheet NEXT RECORD
          AT END
            SET end-of-file TO TRUE
          NOT AT END
            PERFORM Preview-Character
        END-READ
      END-PERFORM
    END-START
  CLOSE CharacterSheet
STOP RUN.

USER-INTERFACE SECTION.
COPY "copy/procedure/character-preview.cpy".
