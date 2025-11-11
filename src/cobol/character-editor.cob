IDENTIFICATION DIVISION.
PROGRAM-ID. Character-Editor.

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
  COPY "src/copy/file-description/character-record.cpy".

WORKING-STORAGE SECTION.
  COPY "src/copy/working-storage/user-interface-data.cpy".

  01 Mode-Status PIC 9 VALUE ZERO.
    88 decide-mode VALUE 0.
    88 create-mode VALUE 1.
    88 edit-mode   VALUE 2.
    88 delete-mode VALUE 3.
    88 list-mode   VALUE 4.
    88 quit-mode   VALUE 9.

  01 Character-Status PIC 9 VALUE ZEROS.
    88 invalid-character  VALUE 0.
    88 new-character      VALUE 1.
    88 existing-character VALUE 2.

  01 Field-Table.
    02 field-buffer     PIC X(10).
    02 Field-Entry      OCCURS 23 TIMES INDEXED BY F-IX.
      03 field-label    PIC X(10).
      03 feild-default  PIC X(15).
      03 field-code     PIC X(15).
    02 field-length     PIC 99 VALUE 23.

  01 File-Status    PIC 99.
    88 end-of-file  VALUE 10.

PROCEDURE DIVISION.
  PERFORM Initialize-Table.

  Main-Logic.
    PERFORM Main-Loop UNTIL ui-denied.
  STOP RUN.

  Main-Loop.
    PERFORM UNTIL quit-mode
      PERFORM Get-Mode
      PERFORM Execute-Mode
    END-PERFORM.

  Get-Mode.
    MOVE "[C]REATE, [E]DIT, [L]IST, [D]ELETE or [Q]UIT" TO ui-prompt
    PERFORM UI-Ask
    PERFORM UI-Normalize-Answer

    EVALUATE ui-answer(1:1)
      WHEN "C"
        SET create-mode TO TRUE
      WHEN "E"
        SET edit-mode TO TRUE
      WHEN "L"
        SET list-mode TO TRUE
      WHEN "D"
        SET delete-mode TO TRUE
      WHEN "Q"
        SET quit-mode TO TRUE
      WHEN OTHER
        DISPLAY "INVALID CHOICE: " ui-answer
    END-EVALUATE.

  Execute-Mode.
    EVALUATE Mode-Status
    WHEN 1
      PERFORM Try-Crate-Character UNTIL ui-denied
    WHEN 2
      PERFORM Try-Edit-Character UNTIL ui-denied
    WHEN 3
      PERFORM Try-Delete-Character UNTIL ui-denied
    WHEN 4
      PERFORM List-Characters
    WHEN 9
      DISPLAY "EXITING..."
      EXIT PARAGRAPH
    WHEN OTHER
      DISPLAY "INVALID MODE: " Mode-Status
    END-EVALUATE

    SET decide-mode TO TRUE.

  Try-Crate-Character.
    PERFORM Lookup-Key

    IF new-character
      DISPLAY "CREATING NEW CHARACTER..."
      PERFORM Assign-All-Fields
      PERFORM Try-Record-Character
    ELSE IF existing-character
      DISPLAY "CANNOT CREATE EXISTING CHARACTER."
      PERFORM UI-Clear-Data
    ELSE
      DISPLAY "CANNOT CREATE INVALID CHARACTER: " ui-answer
    END-IF

    MOVE "CREATE ANOTHER CHARACTER" TO ui-prompt
    PERFORM UI-Confirm

    IF ui-denied
      SET decide-mode TO TRUE
    END-IF.

  Try-Edit-Character.
    PERFORM Lookup-Key

    IF new-character
      DISPLAY "CANNOT EDIT NEW CHARACTER."
      PERFORM UI-Clear-Data
    ELSE IF existing-character
      DISPLAY "EDITING CHARACTER..."
      PERFORM Developer-View-Character
      PERFORM Select-Field UNTIL ui-denied
      PERFORM Try-Record-Character
    ELSE
      DISPLAY "CANNOT EDIT INVALID CHARACTER: " ui-answer
    END-IF

    MOVE "EDIT ANOTHER CHARACTER" TO ui-prompt
    PERFORM UI-Confirm

    IF ui-denied
      SET decide-mode TO TRUE
    END-IF.

  Try-Delete-Character.
    PERFORM Lookup-Key

    IF new-character
      DISPLAY "CANNOT DELETE NEW CHARACTER."
      PERFORM UI-Clear-Data
    ELSE IF existing-character
      PERFORM Developer-View-Character
      MOVE "ARE YOU SURE YOU WANT TO DELETE THIS CHARACTER" TO ui-prompt
      PERFORM UI-Confirm

      IF ui-confirmed
        OPEN I-O Character-Sheet
          DELETE Character-Sheet
            INVALID KEY DISPLAY "INVARIANT VIOLATION: TRIED TO DELETE INVALID KEY."
          END-DELETE
        CLOSE Character-Sheet
      END-IF
    ELSE
      DISPLAY "CANNOT DELETE INVALID CHARACTER: " ui-answer
    END-IF

    MOVE "DELETE ANOTHER CHARACTER" TO ui-prompt
    PERFORM UI-Confirm

    IF ui-denied
      SET decide-mode TO TRUE
    END-IF.

  List-Characters.
      OPEN INPUT Character-Sheet
      MOVE LOW-VALUE TO short-name
      START Character-Sheet KEY >= short-name
        INVALID KEY DISPLAY "NO RECORDS FOUND"
        NOT INVALID KEY
          PERFORM UNTIL end-of-file
            READ Character-Sheet NEXT RECORD
              AT END
                SET end-of-file TO TRUE
              NOT AT END
                PERFORM Developer-View-Character
            END-READ
          END-PERFORM
        END-START
      CLOSE Character-Sheet.

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
        MOVE "ENTER LONG NAME (21)" TO ui-prompt
        PERFORM UI-Ask
        IF ui-empty-answer
          MOVE feild-default(F-IX) TO long-name
        ELSE
          MOVE ui-answer TO long-name
        END-IF
      WHEN "AGE"
        MOVE "ENTER AGE (##)" TO ui-prompt
        PERFORM UI-Ask-Number
        IF ui-invalid-number
          MOVE FUNCTION NUMVAL(feild-default(F-IX)) TO age
        ELSE
          MOVE ui-number TO age
        END-IF
      WHEN "GENDER"
        MOVE "ENTER GENDER (10)" TO ui-prompt
        PERFORM UI-Ask
        IF ui-empty-answer
          MOVE feild-default(F-IX) TO gender
        ELSE
          PERFORM UI-Normalize-Answer
          MOVE ui-answer TO gender
        END-IF
      WHEN "RACE"
        MOVE "ENTER RACE (20)" TO ui-prompt
        PERFORM UI-Ask
        IF ui-empty-answer
          MOVE feild-default(F-IX) TO race
        ELSE
          PERFORM UI-Normalize-Answer
          MOVE ui-answer TO race
        END-IF
      WHEN "DESCRIPTION"
        MOVE "ENTER INFO (80)" TO ui-prompt
        PERFORM UI-Ask
        IF ui-empty-answer
          MOVE feild-default(F-IX) TO description
        ELSE
          MOVE ui-answer TO description
        END-IF
      WHEN "ORIGINAL-TEAM"
        MOVE "ENTER TEAM (20)" TO ui-prompt
        PERFORM UI-Ask
        IF ui-empty-answer
          MOVE feild-default(F-IX) TO original-team
        ELSE
          PERFORM UI-Normalize-Answer
          MOVE ui-answer TO original-team
        END-IF
      WHEN "SALARY-NEED"
        MOVE "ENTER NEED (##)" TO ui-prompt
        PERFORM UI-Ask-Number
        IF ui-invalid-number
          MOVE FUNCTION NUMVAL(feild-default(F-IX)) TO salary-need
        ELSE
          MOVE ui-number TO salary-need
        END-IF
      WHEN "SALARY-WANT"
        MOVE "ENTER WANT (##)" TO ui-prompt
        PERFORM UI-Ask-Number
        IF ui-invalid-number
          MOVE FUNCTION NUMVAL(feild-default(F-IX)) TO salary-want
        ELSE
          MOVE ui-number TO salary-want
        END-IF
      WHEN "PER-DIEM"
        MOVE "ENTER PAY (##)" TO ui-prompt
        PERFORM UI-Ask-Number
        IF ui-invalid-number
          MOVE FUNCTION NUMVAL(feild-default(F-IX)) TO per-diem
        ELSE
          MOVE ui-number TO per-diem
        END-IF
      WHEN "JOB"
        MOVE "ENTER JOB (10)" TO ui-prompt
        PERFORM UI-Ask
        IF ui-empty-answer
          MOVE feild-default(F-IX) TO job
        ELSE
          PERFORM UI-Normalize-Answer
          MOVE ui-answer TO job
        END-IF
      WHEN "SKILL"
        MOVE "ENTER SKILL (10)" TO ui-prompt
        PERFORM UI-Ask
        IF ui-empty-answer
          MOVE feild-default(F-IX) TO skill
        ELSE
          PERFORM UI-Normalize-Answer
          MOVE ui-answer TO skill
        END-IF
      WHEN "LEVEL"
        MOVE "ENTER LEVEL (##)" TO ui-prompt
        PERFORM UI-Ask-Number
        IF ui-invalid-number
          MOVE FUNCTION NUMVAL(feild-default(F-IX)) TO level
        ELSE
          MOVE ui-number TO level
        END-IF
      WHEN "EXPERIENCE"
        MOVE "ENTER EXPERIENCE (##)" TO ui-prompt
        PERFORM UI-Ask-Number
        IF ui-invalid-number
          MOVE FUNCTION NUMVAL(feild-default(F-IX)) TO experience
        ELSE
          MOVE ui-number TO experience
        END-IF
      WHEN "POWER-STAT"
        MOVE "ENTER POWER (##)" TO ui-prompt
        PERFORM UI-Ask-Number
        IF ui-invalid-number
          MOVE FUNCTION NUMVAL(feild-default(F-IX)) TO power-stat
        ELSE
          PERFORM Validate-Die
          MOVE ui-number TO power-stat
        END-IF
      WHEN "POWER-BONUS"
        MOVE "ENTER POWER BONUS (#)" TO ui-prompt
        PERFORM UI-Ask-Number
        IF ui-invalid-number
          MOVE FUNCTION NUMVAL(feild-default(F-IX)) TO power-bonus
        ELSE
          MOVE ui-number TO power-bonus
        END-IF
      WHEN "FOCUS-STAT"
        MOVE "ENTER FOCUS (##)" TO ui-prompt
        PERFORM UI-Ask-Number
        IF ui-invalid-number
          MOVE FUNCTION NUMVAL(feild-default(F-IX)) TO focus-stat
        ELSE
          PERFORM Validate-Die
          MOVE ui-number TO focus-stat
        END-IF
      WHEN "FOCUS-BONUS"
        MOVE "ENTER FOCUS BONUS (#)" TO ui-prompt
        PERFORM UI-Ask-Number
        IF ui-invalid-number
          MOVE FUNCTION NUMVAL(feild-default(F-IX)) TO focus-bonus
        ELSE
          MOVE ui-number TO focus-bonus
        END-IF
      WHEN "SPEED-STAT"
        MOVE "ENTER SPEED (##)" TO ui-prompt
        PERFORM UI-Ask-Number
        IF ui-invalid-number
          MOVE FUNCTION NUMVAL(feild-default(F-IX)) TO speed-stat
        ELSE
          PERFORM Validate-Die
          MOVE ui-number TO speed-stat
        END-IF
      WHEN "SPEED-BONUS"
        MOVE "ENTER SPEED BONUS (#)" TO ui-prompt
        PERFORM UI-Ask-Number
        IF ui-invalid-number
          MOVE FUNCTION NUMVAL(feild-default(F-IX)) TO speed-bonus
        ELSE
          MOVE ui-number TO speed-bonus
        END-IF
      WHEN "BODY"
        MOVE "ENTER BODY (###)" TO ui-prompt
        PERFORM UI-Ask-Number
        IF ui-invalid-number
          MOVE FUNCTION NUMVAL(feild-default(F-IX)) TO body
        ELSE
          MOVE ui-number TO body
        END-IF
      WHEN "MIND"
        MOVE "ENTER MIND (###)" TO ui-prompt
        PERFORM UI-Ask-Number
        IF ui-invalid-number
          MOVE FUNCTION NUMVAL(feild-default(F-IX)) TO mind
        ELSE
          MOVE ui-number TO mind
        END-IF
      WHEN "SPIRIT"
        MOVE "ENTER SPIRIT (###)" TO ui-prompt
        PERFORM UI-Ask-Number
        IF ui-invalid-number
          MOVE FUNCTION NUMVAL(feild-default(F-IX)) TO spirit
        ELSE
          MOVE ui-number TO spirit
        END-IF
      WHEN "CHARACTER-TYPE"
        MOVE "ENTER TYPE (PC | NPC | TEST)" TO ui-prompt
        PERFORM UI-Ask
        IF ui-empty-answer
          MOVE feild-default(F-IX) TO character-type
        ELSE
          MOVE ui-answer TO character-type
        END-IF
    END-EVALUATE.

  Try-Record-Character.
    PERFORM Developer-View-Character

    MOVE "RECORD THIS CHARACTER" TO ui-prompt
    PERFORM UI-Confirm

    IF ui-confirmed 
      PERFORM Record-Character
    END-IF

    SET quit-mode TO TRUE.

  Record-Character.
    DISPLAY "WRITING CHARACTER RECORD..."

    OPEN I-O Character-Sheet
      WRITE character-record INVALID KEY
        REWRITE character-record
      END-WRITE
    CLOSE Character-Sheet.

  Select-Field.
    MOVE "ENTER A FIELD" TO ui-prompt
    PERFORM UI-Ask
    PERFORM UI-Normalize-Answer
    MOVE ui-answer TO field-buffer

    PERFORM Reset-Index
    SEARCH Field-Entry
      AT END 
        DISPLAY "INVALID FIELD: " field-buffer
      WHEN field-label(F-IX) = field-buffer
        PERFORM Assign-Field
    END-SEARCH.

  Lookup-Key.
    MOVE "ENTER KEY (SHORT-NAME) (10)" TO ui-prompt.
    PERFORM UI-Ask
    PERFORM UI-Normalize-Answer
    PERFORM Validate-Key
    PERFORM Validate-Character-Status.

VALIDATION SECTION.
  Validate-Die.
    IF NOT valid-die
      DISPLAY "INVALID DIE NUMBER. DEFAULTING TO 8"
      MOVE 8 to ui-number
    END-IF.

  Validate-Key.
    IF ui-empty-answer OR ui-invalid-text
      SET ui-invalid-key TO TRUE
    ELSE
      SET ui-valid-key TO TRUE
    END-IF.

  Validate-Character-Status.
    IF ui-invalid-key
      SET invalid-character TO TRUE
      EXIT PARAGRAPH
    END-IF

    OPEN I-O Character-Sheet
      MOVE ui-answer TO short-name
      READ Character-Sheet KEY IS short-name
        INVALID KEY 
          DISPLAY FUNCTION TRIM(short-name) " IS NOT IN RECORD."
          SET new-character TO TRUE
        NOT INVALID KEY 
          DISPLAY FUNCTION TRIM(short-name) " IS ALREADY IN RECORD."
          SET existing-character TO TRUE
      END-READ
    CLOSE Character-Sheet.

  Validate-Type.
    PERFORM UI-Normalize-Answer

    IF ui-answer = "PC"
      SET character-type to "PLAYER"
    ELSE IF ui-answer = "NPC"
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

COPY "src/copy/procedure/user-interface.cpy".
COPY "src/copy/procedure/character-preview.cpy".

*> Build: `cobc -x -o build/character-editor src/cobol/character-editor.cob`
