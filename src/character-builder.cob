IDENTIFICATION DIVISION.
PROGRAM-ID. Character-Builder.

ENVIRONMENT DIVISION.
INPUT-OUTPUT SECTION.
FILE-CONTROL.
  SELECT Character-Sheet ASSIGN TO "data/characters.dat"
  ORGANIZATION IS INDEXED
  ACCESS MODE IS DYNAMIC
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
    88 quit-mode        VALUE 9.

  01 Field-Table.
    02 field-buffer PIC X(10).
    02 Field-Entry      OCCURS 23 TIMES INDEXED BY F-IX.
      03 field-label    PIC X(10).
      03 feild-default  PIC X(15).
      03 field-code     PIC X(15).
    02 field-length     PIC 99 VALUE 23.

  01 File-Status PIC 99.
    88 end-of-file VALUE 10.

PROCEDURE DIVISION.
  PERFORM Initialize-Table.

  Main-Logic.
    PERFORM Main-Loop UNTIL denied.
  STOP RUN.

  Main-Loop.
    PERFORM Decision-Loop UNTIL quit-mode
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
      PERFORM Developer-View-Character
      PERFORM Select-Field UNTIL denied
      PERFORM Try-Save-Character
    WHEN 4
      OPEN I-O Character-Sheet
        DELETE Character-Sheet
          INVALID KEY DISPLAY "IMPOSSIBLE ERROR: TRIED TO DELETE INVALID KEY."
        END-DELETE
      CLOSE Character-Sheet

      SET quit-mode TO TRUE
    WHEN OTHER
      PERFORM Lookup-Key
    END-EVALUATE.

CHARACTER-EDIT SECTION.
  Assign-All-Fields.
    PERFORM Reset-Index

    PERFORM UNTIL F-IX > field-length
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
      WHEN "AGE"
        MOVE "ENTER AGE (##)" TO question
        PERFORM Ask-Number
        IF invalid-number
          MOVE FUNCTION NUMVAL(feild-default(F-IX)) TO age
        ELSE
          MOVE input-number TO age
        END-IF
      WHEN "GENDER"
        MOVE "ENTER GENDER (10)" TO question
        PERFORM Ask
        IF empty-input
          MOVE feild-default(F-IX) TO gender
        ELSE
          PERFORM Normalize-Response
          MOVE response TO gender
        END-IF
      WHEN "RACE"
        MOVE "ENTER RACE (20)" TO question
        PERFORM Ask
        IF empty-input
          MOVE feild-default(F-IX) TO race
        ELSE
          PERFORM Normalize-Response
          MOVE response TO race
        END-IF
      WHEN "DESCRIPTION"
        MOVE "ENTER INFO (80)" TO question
        PERFORM Ask
        IF empty-input
          MOVE feild-default(F-IX) TO description
        ELSE
          MOVE response TO description
        END-IF
      WHEN "ORIGINAL-TEAM"
        MOVE "ENTER TEAM (20)" TO question
        PERFORM Ask
        IF empty-input
          MOVE feild-default(F-IX) TO original-team
        ELSE
          PERFORM Normalize-Response
          MOVE response TO original-team
        END-IF
      WHEN "SALARY-NEED"
        MOVE "ENTER NEED (##)" TO question
        PERFORM Ask-Number
        IF invalid-number
          MOVE FUNCTION NUMVAL(feild-default(F-IX)) TO salary-need
        ELSE
          MOVE input-number TO salary-need
        END-IF
      WHEN "SALARY-WANT"
        MOVE "ENTER WANT (##)" TO question
        PERFORM Ask-Number
        IF invalid-number
          MOVE FUNCTION NUMVAL(feild-default(F-IX)) TO salary-want
        ELSE
          MOVE input-number TO salary-want
        END-IF
      WHEN "PER-DIEM"
        MOVE "ENTER PAY (##)" TO question
        PERFORM Ask-Number
        IF invalid-number
          MOVE FUNCTION NUMVAL(feild-default(F-IX)) TO per-diem
        ELSE
          MOVE input-number TO per-diem
        END-IF
      WHEN "JOB"
        MOVE "ENTER JOB (10)" TO question
        PERFORM Ask
        IF empty-input
          MOVE feild-default(F-IX) TO job
        ELSE
          PERFORM Normalize-Response
          MOVE response TO job
        END-IF
      WHEN "SKILL"
        MOVE "ENTER SKILL (10)" TO question
        PERFORM Ask
        IF empty-input
          MOVE feild-default(F-IX) TO skill
        ELSE
          PERFORM Normalize-Response
          MOVE response TO skill
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
      WHEN "POWER-STAT"
        MOVE "ENTER POWER (##)" TO question
        PERFORM Ask-Number
        IF invalid-number
          MOVE FUNCTION NUMVAL(feild-default(F-IX)) TO power-stat
        ELSE
          PERFORM Validate-Die
          MOVE input-number TO power-stat
        END-IF
      WHEN "POWER-BONUS"
        MOVE "ENTER POWER BONUS (#)" TO question
        PERFORM Ask-Number
        IF invalid-number
          MOVE FUNCTION NUMVAL(feild-default(F-IX)) TO power-bonus
        ELSE
          MOVE input-number TO power-bonus
        END-IF
      WHEN "FOCUS-STAT"
        MOVE "ENTER FOCUS (##)" TO question
        PERFORM Ask-Number
        IF invalid-number
          MOVE FUNCTION NUMVAL(feild-default(F-IX)) TO focus-stat
        ELSE
          PERFORM Validate-Die
          MOVE input-number TO focus-stat
        END-IF
      WHEN "FOCUS-BONUS"
        MOVE "ENTER FOCUS BONUS (#)" TO question
        PERFORM Ask-Number
        IF invalid-number
          MOVE FUNCTION NUMVAL(feild-default(F-IX)) TO focus-bonus
        ELSE
          MOVE input-number TO focus-bonus
        END-IF
      WHEN "SPEED-STAT"
        MOVE "ENTER SPEED (##)" TO question
        PERFORM Ask-Number
        IF invalid-number
          MOVE FUNCTION NUMVAL(feild-default(F-IX)) TO speed-stat
        ELSE
          PERFORM Validate-Die
          MOVE input-number TO speed-stat
        END-IF
      WHEN "SPEED-BONUS"
        MOVE "ENTER SPEED BONUS (#)" TO question
        PERFORM Ask-Number
        IF invalid-number
          MOVE FUNCTION NUMVAL(feild-default(F-IX)) TO speed-bonus
        ELSE
          MOVE input-number TO speed-bonus
        END-IF
      WHEN "BODY"
        MOVE "ENTER BODY (###)" TO question
        PERFORM Ask-Number
        IF invalid-number
          MOVE FUNCTION NUMVAL(feild-default(F-IX)) TO body
        ELSE
          MOVE input-number TO body
        END-IF
      WHEN "MIND"
        MOVE "ENTER MIND (###)" TO question
        PERFORM Ask-Number
        IF invalid-number
          MOVE FUNCTION NUMVAL(feild-default(F-IX)) TO mind
        ELSE
          MOVE input-number TO mind
        END-IF
      WHEN "SPIRIT"
        MOVE "ENTER SPIRIT (###)" TO question
        PERFORM Ask-Number
        IF invalid-number
          MOVE FUNCTION NUMVAL(feild-default(F-IX)) TO spirit
        ELSE
          MOVE input-number TO spirit
        END-IF
      WHEN "CHARACTER-TYPE"
        MOVE "ENTER TYPE (PC | NPC | TEST)" TO question
        PERFORM Ask
        IF empty-input
          MOVE feild-default(F-IX) TO character-type
        ELSE
          MOVE response TO character-type
        END-IF
    END-EVALUATE.

  Try-Save-Character.
    PERFORM Developer-View-Character

    MOVE "RECORD THIS CHARACTER" TO question
    PERFORM Confirm

    IF confirmed 
      PERFORM Record-Character
    END-IF

    SET quit-mode TO TRUE.

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
      PERFORM Developer-View-Character

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
    MOVE "NAME"           TO field-label    (1)
    MOVE "NO NAME"        TO feild-default  (1)
    MOVE "LONG-NAME"      TO field-code     (1)

    MOVE "AGE"            TO field-label    (2)
    MOVE "30"             TO feild-default  (2)
    MOVE "AGE"            TO field-code     (2)

    MOVE "SEX"            TO field-label    (3)
    MOVE "NONE"           TO feild-default  (3)
    MOVE "GENDER"         TO field-code     (3)

    MOVE "RACE"           TO field-label    (4)
    MOVE "EOSIAN"         TO feild-default  (4)
    MOVE "RACE"           TO field-code     (4)

    MOVE "INFO"           TO field-label    (5)
    MOVE "NO DESCRIPTION" TO feild-default  (5)
    MOVE "DESCRIPTION"    TO field-code     (5)

    MOVE "TEAM"           TO field-label    (6)
    MOVE "NO TEAM"        TO feild-default  (6)
    MOVE "ORIGINAL-TEAM"  TO field-code     (6)

    MOVE "NEED"           TO field-label    (7)
    MOVE "30"             TO feild-default  (7)
    MOVE "SALARY-NEED"    TO field-code     (7)

    MOVE "WANT"           TO field-label    (8)
    MOVE "60"             TO feild-default  (8)
    MOVE "SALARY-WANT"    TO field-code     (8)

    MOVE "PAY"            TO field-label    (9)
    MOVE "45"             TO feild-default  (9)
    MOVE "PER-DIEM"       TO field-code     (9)

    MOVE "JOB"            TO field-label    (10)
    MOVE "REST"           TO feild-default  (10)
    MOVE "JOB"            TO field-code     (10)

    MOVE "SKILL"          TO field-label    (11)
    MOVE "NONE"           TO feild-default  (11)
    MOVE "SKILL"          TO field-code     (11)

    MOVE "LV"             TO field-label    (12)
    MOVE "1"              TO feild-default  (12)
    MOVE "LEVEL"          TO field-code     (12)

    MOVE "EXP"            TO field-label    (13)
    MOVE "0"              TO feild-default  (13)
    MOVE "EXPERIENCE"     TO field-code     (13)

    MOVE "POWER"          TO field-label    (14)
    MOVE "8"              TO feild-default  (14)
    MOVE "POWER-STAT"     TO field-code     (14)

    MOVE "FOCUS"          TO field-label    (15)
    MOVE "8"              TO feild-default  (15)
    MOVE "FOCUS-STAT"     TO field-code     (15)

    MOVE "SPEED"          TO field-label    (16)
    MOVE "8"              TO feild-default  (16)
    MOVE "SPEED-STAT"     TO field-code     (16)

    MOVE "POWER+"         TO field-label    (17)
    MOVE "0"              TO feild-default  (17)
    MOVE "POWER-BONUS"    TO field-code     (17)

    MOVE "FOCUS+"         TO field-label    (18)
    MOVE "0"              TO feild-default  (18)
    MOVE "FOCUS-BONUS"    TO field-code     (18)

    MOVE "SPEED+"         TO field-label    (19)
    MOVE "0"              TO feild-default  (19)
    MOVE "SPEED-BONUS"    TO field-code     (19)

    MOVE "BODY"           TO field-label    (20)
    MOVE "100"            TO feild-default  (20)
    MOVE "BODY"           TO field-code     (20)

    MOVE "MIND"           TO field-label    (21)
    MOVE "100"            TO feild-default  (21)
    MOVE "MIND"           TO field-code     (21)

    MOVE "SPIRIT"         TO field-label    (22)
    MOVE "100"            TO feild-default  (22)
    MOVE "SPIRIT"         TO field-code     (22)

    MOVE "TYPE"           TO field-label    (23)
    MOVE "NO NAME"        TO feild-default  (23)
    MOVE "CHARACTER-TYPE" TO field-code     (23).

  Reset-Index.
    SET F-IX TO 1.

  Increment-Index.
    SET F-IX UP BY 1.

  Table-help.
    PERFORM Reset-Index

    PERFORM UNTIL F-IX > field-length
      DISPLAY field-label(F-IX) "->" field-code(F-IX) "(" feild-default(F-IX) ")"
      PERFORM Increment-Index
    END-PERFORM.

COPY "copy/procedure/input-section.cpy".
COPY "copy/procedure/character-preview.cpy".
