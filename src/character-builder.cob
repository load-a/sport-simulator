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
  COPY "copy/data/character-record.cpy".

WORKING-STORAGE SECTION.
  COPY "copy/data/input-data.cpy".

  01 Mode-Status PIC 9 VALUE ZERO.
    88 decide-mode      VALUE 0.
    88 create-mode      VALUE 1.
    88 edit-delete-mode VALUE 2.
    88 edit-mode        VALUE 3.
    88 delete-mode      VALUE 4.
    88 denied-mode        VALUE 9.

  01 Field-Table.
    02 field-buffer PIC X(10).
    02 Field-Entry      OCCURS 12 TIMES INDEXED BY F-IX.
      03 field-label    PIC X(10).
      03 feild-default  PIC X(10).
      03 field-code     PIC X(10).

  01 File-Status PIC 99.
    88 end-of-file VALUE 10.

PROCEDURE DIVISION.
  PERFORM Initialize-Table.

  Main-Logic.
    PERFORM Main-Loop UNTIL denied.
  STOP RUN.

  Main-Loop.
    PERFORM Decision-Loop UNTIL denied-mode
    SET decide-mode TO TRUE
    MOVE "CONTINUE WITH ANOTHER CHARACTER" TO question
    PERFORM Confirm.

  Decision-Loop.
    EVALUATE Mode-Status
    WHEN 1
      PERFORM Assign-All-Fields
      PERFORM Try-Save-Character
    WHEN 2
      MOVE "EDIT THIS CHARACTER" TO question
      PERFORM Confirm

      IF confirmed
        SET edit-mode TO TRUE
      ELSE
        PERFORM Try-Delete-Character
      END-IF
    WHEN 3
      PERFORM Preview-Character
      PERFORM Select-Field UNTIL denied
      PERFORM Try-Save-Character
    WHEN 4
      OPEN I-O Character-Sheet
        DELETE Character-Sheet
          INVALID KEY DISPLAY "IMPOSSIBLE ERROR: TRIED TO DELETE INVALID KEY."
        END-DELETE
      CLOSE Character-Sheet

      SET denied-mode TO TRUE
    WHEN OTHER
      PERFORM Lookup-Key
    END-EVALUATE.

CHARACTER-EDIT SECTION.
  Assign-All-Fields.
    PERFORM Reset-Index

    PERFORM UNTIL F-IX > 12
      PERFORM Assign-Field
      PERFORM Increment-Index
    END-PERFORM.

  Assign-Field.
    EVALUATE field-code(F-IX)
      WHEN "LONG-NAME"
        MOVE "ENTER LONG NAME (21)" TO question
        PERFORM Ask
        IF empty-input
          MOVE feild-default(F-IX) TO long-name
        ELSE
          MOVE response TO long-name
        END-IF
      WHEN "TASK"
        MOVE "ENTER ROLE (10)" TO question
        PERFORM Ask
        IF empty-input
          MOVE feild-default(F-IX) TO task
        ELSE
          PERFORM Normalize-Response
          MOVE response TO task
        END-IF
      WHEN "TEAM"
        MOVE "ENTER TEAM (20)" TO question
        PERFORM Ask
        IF empty-input
          MOVE feild-default(F-IX) TO team
        ELSE
          MOVE response TO team
        END-IF
      WHEN "LEVEL"
        MOVE "ENTER LEVEL (##)" TO question
        PERFORM Ask-Number
        IF invalid-number
          MOVE FUNCTION NUMVAL(feild-default(F-IX)) TO level
        ELSE
          MOVE input-number TO level
        END-IF
      WHEN "EXPERIENCE"
        MOVE "ENTER EXPERIENCE (##)" TO question
        PERFORM Ask-Number
        IF invalid-number
          MOVE FUNCTION NUMVAL(feild-default(F-IX)) TO experience
        ELSE
          MOVE input-number TO experience
        END-IF
      WHEN "ENERGY"
        MOVE "ENTER VIGOR (#.##)" TO question
        PERFORM Ask-Number
        IF invalid-number
          MOVE FUNCTION NUMVAL(feild-default(F-IX)) TO energy
        ELSE
          MOVE FUNCTION NUMVAL(response) TO energy
        END-IF
      WHEN "POWER"
        MOVE "ENTER POWER (##)" TO question
        PERFORM Ask-Number
        IF invalid-number
          MOVE FUNCTION NUMVAL(feild-default(F-IX)) TO power
        ELSE
          PERFORM Validate-Die
          MOVE input-number TO power
        END-IF
      WHEN "FOCUS"
        MOVE "ENTER FOCUS (##)" TO question
        PERFORM Ask-Number
        IF invalid-number
          MOVE FUNCTION NUMVAL(feild-default(F-IX)) TO focus
        ELSE
          PERFORM Validate-Die
          MOVE input-number TO focus
        END-IF
      WHEN "SPEED"
        MOVE "ENTER SPEED (##)" TO question
        PERFORM Ask-Number
        IF invalid-number
          MOVE FUNCTION NUMVAL(feild-default(F-IX)) TO speed
        ELSE
          PERFORM Validate-Die
          MOVE input-number TO speed
        END-IF
      WHEN "TYPE"
        MOVE "ENTER TYPE (PC | NPC | TEST)" TO question
        PERFORM Ask
        IF empty-input
          MOVE feild-default(F-IX) TO character-type
        ELSE
          MOVE response TO character-type
        END-IF
      WHEN "AGE"
        MOVE "ENTER AGE (##)" TO question
        PERFORM Ask-Number
        IF invalid-number
          MOVE FUNCTION NUMVAL(feild-default(F-IX)) TO age
        ELSE
          MOVE input-number TO age
        END-IF
      WHEN "INFO"
        MOVE "ENTER INFO (60)" TO question
        PERFORM Ask
        IF empty-input
          MOVE feild-default(F-IX) TO info
        ELSE
          MOVE response TO info
        END-IF
    END-EVALUATE.

  Try-Save-Character.
    PERFORM Preview-Character

    MOVE "RECORD THIS CHARACTER" TO question
    PERFORM Confirm

    IF confirmed 
      PERFORM Record-Character
    END-IF

    SET denied-mode TO TRUE.

  Record-Character.
    DISPLAY "WRITING RECORD..."

    OPEN I-O Character-Sheet
      WRITE character-record INVALID KEY
        REWRITE character-record
      END-WRITE
    CLOSE Character-Sheet.

  Try-Delete-Character.
    MOVE "DELETE CHARACTER" TO question
    PERFORM Confirm

    IF confirmed
      PERFORM Preview-Character

      MOVE "ARE YOU SURE YOU WANT TO DELETE THIS CHARACTER" TO question
      PERFORM Confirm

      IF confirmed
        SET delete-mode TO TRUE
      END-IF
    END-IF

    SET decide-mode TO TRUE.

  Select-Field.
    MOVE "ENTER A FIELD" TO question
    PERFORM Ask
    PERFORM Normalize-Response
    MOVE response TO field-buffer

    PERFORM Reset-Index
    SEARCH Field-Entry
      AT END 
        DISPLAY "INVALID FIELD: " field-buffer
      WHEN field-label(F-IX) = field-buffer
        PERFORM Assign-Field
    END-SEARCH.

  Lookup-Key.
    MOVE "ENTER KEY (SHORT-NAME) (10)" TO question.
    PERFORM Ask
    PERFORM Validate-Key.

VALIDATION SECTION.
  Validate-Die.
    IF NOT valid-die
      DISPLAY "INVALID DIE NUMBER. DEFAULTING TO 8"
      MOVE 8 to input-number
    END-IF.

  Validate-Key.
    IF empty-input OR invalid-text
      SET invalid-key TO TRUE
      DISPLAY "CANNOT USE EMPTY INDEX."
    ELSE
      SET valid-key TO TRUE
      PERFORM Determine-Mode
    END-IF.

  Determine-Mode.
    OPEN I-O Character-Sheet
      PERFORM Normalize-Response
      MOVE response TO short-name
      READ Character-Sheet KEY IS short-name
        INVALID KEY 
          SET create-mode TO TRUE
          DISPLAY "ENTERING CREATE-MODE..."
        NOT INVALID KEY 
          SET edit-delete-mode TO TRUE
          DISPLAY FUNCTION TRIM(short-name) " IS ALREADY IN RECORD"
      END-READ
    CLOSE Character-Sheet.

  Validate-Type.
    PERFORM Normalize-Response

    IF response = "PC"
      SET character-type to "PLAYER"
    ELSE IF response = "NPC"
      SET character-type to "NPC"
    ELSE
      DISPLAY "DEFAULTING CHARACTER TO TEST..."
      SET character-type to "TEST"
    END-IF.

TABLE-SECTION.
  Initialize-Table.
    MOVE "NAME"       TO field-label    (1)
    MOVE "NO NAME"    TO feild-default  (1)
    MOVE "LONG-NAME"  TO field-code     (1)

    MOVE "ROLE"       TO field-label    (2)
    MOVE "NONE"       TO feild-default  (2)
    MOVE "TASK"       TO field-code     (2)

    MOVE "TEAM"       TO field-label    (3)
    MOVE "NONE"       TO feild-default  (3)
    MOVE "TEAM"       TO field-code     (3)

    MOVE "LV"         TO field-label    (4)
    MOVE "01"         TO feild-default  (4)
    MOVE "LEVEL"      TO field-code     (4)

    MOVE "EXP"        TO field-label    (5)
    MOVE "00"         TO feild-default  (5)
    MOVE "EXPERIENCE" TO field-code     (5)

    MOVE "VIGOR"      TO field-label    (6)
    MOVE "1.00"       TO feild-default  (6)
    MOVE "ENERGY"     TO field-code     (6)

    MOVE "POWER"      TO field-label    (7)
    MOVE "08"         TO feild-default  (7)
    MOVE "POWER"      TO field-code     (7)

    MOVE "FOCUS"      TO field-label    (8)
    MOVE "08"         TO feild-default  (8)
    MOVE "FOCUS"      TO field-code     (8)

    MOVE "SPEED"      TO field-label    (9)
    MOVE "08"         TO feild-default  (9)
    MOVE "SPEED"      TO field-code     (9)

    MOVE "TYPE"       TO field-label    (10)
    MOVE "TEST"       TO feild-default  (10)
    MOVE "TYPE"       TO field-code     (10)

    MOVE "AGE"        TO field-label    (11)
    MOVE "25"         TO feild-default  (11)
    MOVE "AGE"        TO field-code     (11)

    MOVE "INFO"       TO field-label    (12)
    MOVE "NO INFO"    TO feild-default  (12)
    MOVE "INFO"       TO field-code     (12).

  Reset-Index.
    SET F-IX TO 1.

  Increment-Index.
    SET F-IX UP BY 1.

COPY "copy/procedure/input-section.cpy".
COPY "copy/procedure/character-preview.cpy".
